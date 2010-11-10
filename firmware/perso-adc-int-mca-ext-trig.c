/** \file firmware/perso-adc-int-mca-ext-trig.c
 * \brief Personality: MCA with internal ADC and external trigger
 *
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 * \defgroup perso_adc_int_mca_ext_trig Personality: MCA with internal ADC and external trigger
 * \ingroup firmware_personality_groups
 *
 * Internal ADC code.
 *
 * @{
 */


#include <stdlib.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>


/** Histogram element size */
#define ELEMENT_SIZE_IN_BYTES 3


#include "global.h"
#include "main.h"
#include "perso-adc-int-global.h"
#include "packet-comm.h"
#include "table-element.h"
#include "data-table.h"

#include "timer1-measurement.h"


/** Number of elements in the histogram table */
#define MAX_COUNTER (1<<ADC_RESOLUTION)


/** Histogram table
 *
 * ATmega644P has 4Kbyte RAM.  When using 10bit ADC resolution,
 * MAX_COUNTER==1024 and 24bit values will still fit (3K table).
 *
 * For the definition of sizeof_table, see adc-int-histogram.c.
 *
 * \see data_table
 */
volatile table_element_t table[MAX_COUNTER] asm("data_table");


/** See * \see data_table */
data_table_info_t data_table_info = {
  /** Actual size of #data_table in bytes */
  sizeof(table),
  /** Type of value table we send */
  VALUE_TABLE_TYPE_HISTOGRAM,
  /** Table element size */
  ELEMENT_SIZE_IN_BYTES,
};


/** See * \see data_table */
PERSONALITY("adc-int-mca",
            2,0,
            1,
            sizeof(table),
            ELEMENT_SIZE_IN_BYTES);


/** Initialize peripherals
 *
 * Configure peak hold capacitor reset pin.
 */
void personality_io_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void personality_io_init(void)
{
    /* configure pin 20 as an output                               */
    DDRD |= (_BV(DDD6));
    /* set pin 20 to ground                                        */
    PORTD &= ~_BV(PD6);
}


/** AD conversion complete interrupt entry point
 *
 * This function is called when an A/D conversion has completed.
 * Update histogram
 * Discharge peak hold capacitor
 */
ISR(ADC_vect)
{
  /* pull pin to discharge peak hold capacitor                    */
  /** \todo worst case calculation: runtime & R7010 */
  PORTD |= _BV(PD6);

  /* Read analog value */
  uint16_t result = ADCW;

  /* We are confident that the range of values the ADC gives us
   * is within the specced 10bit range of 0..1023. */

  /* cut off 2, 1 or 0 LSB */
  const uint16_t index = result >> (10-ADC_RESOLUTION);

  /* For 24bit values, the source code looks a little more complicated
   * than just table[index]++ (even though the generated machine
   * instructions are not).  Anyway, we needed to move the increment
   * into a properly defined _inc function.
   */
  volatile table_element_t *element = &(table[index]);
  table_element_inc(element);

  /* set pin to GND and release peak hold capacitor   */
  PORTD &=~ _BV(PD6);

  /* If a hardware event on int0 pin occurs an interrupt flag in EIFR is set.
   * Since int0 is only configured but not enabled ISR(INT0_vect){} is
   * not executed and therefore this flag is not reset automatically.
   * To reset this flag the bit at position INTF0 must be set.
   */
  EIFR |= _BV(INTF0);
}


/** Setup of INT0
 *
 * INT0 via pin 16 is configured but not enabled
 * Trigger on falling edge
 * Enable pull up resistor on Pin 16 (20-50kOhm)
 */
inline static
void adc_int_trigger_src_conf(void)
{

    /* Configure INT0 pin 16 as input */
    /* Reset Int0 pin 16 bit DDRD in port D Data direction register */
    DDRD &= ~(_BV(DDD2));
    /* Port D data register: Enable pull up on pin 16, 20-50kOhm */
    PORTD |= _BV(PD2);

    /* Disable interrupt "INT0" (clear interrupt enable bit in
     * external interrupt mask register) otherwise an interrupt may
     * occur during level and edge configuration (EICRA)  */
    EIMSK &= ~(_BV(INT0));
    /* Level and edges on the external pin that activates INT0
     * is configured now (interrupt sense control bits in external
     * interrupt control register A). Disable everything.  */
    EICRA &= ~(_BV(ISC01) | _BV(ISC00));
    /* Now enable interrupt on falling edge.
     * [ 10 = interrupt on rising edge
     *   11 = interrupt on falling edge ] */
    EICRA |=  _BV(ISC01);
    /* Clear interrupt flag by writing a locical one to INTFn in the
     * external interrupt flag register.  The flag is set when a
     * interrupt occurs. if the I-flag in the sreg is set and the
     * corresponding flag in the EIFR the program counter jumps to the
     * vector table  */
    EIFR |= _BV(INTF0);
    /* reenable interrupt INT0 (External interrupt mask
     * register). we do not want to jump to the ISR in case of an interrupt
     * so we do not set this bit  */
    // EIMSK |= (_BV(INT0));

}


/** ADC initialisation and configuration
 *
 * ADC configured as auto trigger
 * Trigger source INT0
 * Use external analog reference AREF at PIN 32
 * AD input channel on Pin 40 ADC0
 */
inline static
void adc_int_init(void)
{
  uint16_t result;

  /* channel number: PIN 40 ADC0 -> ADMUX=0 */
  ADMUX = 0;

  /* select voltage reference: external AREF Pin 32 as reference */
  ADMUX &= ~(_BV(REFS1) | _BV(REFS0));

  /* clear ADC Control and Status Register A
   * enable ADC & configure IO-Pins to ADC (ADC ENable) */
  ADCSRA = _BV(ADEN);

  /* ADC prescaler selection (ADC Prescaler Select Bits) */
  /* bits ADPS0 .. ADPS2 */
  ADCSRA |= ((((ADC_PRESCALER >> 2) & 0x1)*_BV(ADPS2)) |
             (((ADC_PRESCALER >> 1) & 0x1)*_BV(ADPS1)) |
              ((ADC_PRESCALER & 0x01)*_BV(ADPS0)));

  /* dummy read out (first conversion takes some time) */
  /* software triggered AD-Conversion */
  ADCSRA |= _BV(ADSC);

  /* wait until conversion is complete */
  loop_until_bit_is_clear(ADCSRA, ADSC);

  /* Either we should discharge the peak/hold cap here to make sure
   * that the first proper ADCW read will measure a good value, or we
   * just discount the first value as being irrelevant for the whole
   * histogram.
   */

  /* clear returned AD value, other next conversion value is not ovrtaken */
  result = ADCW;

  /* Enable AD conversion complete interrupt if I-Flag in sreg is set
   * (-> ADC interrupt enable) */
  ADCSRA |= _BV(ADIE);

   /* Configure ADC trigger source:
    * Select external trigger "interrupt request 0"
    * Interrupt on rising edge                         */
  ADCSRB |= _BV(ADTS1);
  ADCSRB &= ~(_BV(ADTS0) | _BV(ADTS2));

  /* ADC auto trigger enable: ADC will be started by trigger signal */
  ADCSRA |= _BV(ADATE);
}


/** ADC subsystem and trigger setup */
inline static
void adc_init(void)
{
  /** configure INT0 pin 16 */
  adc_int_trigger_src_conf();

  /** configure AREF at pin 32 and single shot auto trigger over int0
   * at pin 40 ADC0 */
  adc_int_init();
}


void personality_start_measurement_sram(void)
{
  const void *voidp = &pparam_sram.params[0];
  const uint16_t *timer1_value = voidp;
  adc_init();
  timer1_init(*timer1_value);
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
