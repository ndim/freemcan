/** \file firmware/adc-ext-mca.c
 * \brief External ADC based MCA
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
 * \defgroup adc_ext_mca External ADC based MCA
 * \ingroup firmware
 *
 * External ADC code.
 *
 * @{
 */


#include <avr/io.h>
#include <avr/interrupt.h>

#include "global.h"
#include "histogram.h"
#include "adc-ext-histogram.h"

/* following two includes need F_CPU */
#include "ad7813.h"
#include <util/delay.h>


/** Initialize peripherals
 *
 * Configure peak hold capacitor reset pin.
 */
void io_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void io_init(void)
{
    /* configure pin as an output                               */
    SHRST_IO_DDR |= (_BV(SHRST_IO_CTRL_BIT));
    /* set pin to ground                                        */
    SHRST_IO_PORT &= ~_BV(SHRST_IO_CTRL_BIT);
}


/** Interrupt 0 service proceeding PMT pulse with external ADC
 *
 * Softtrigger of the AD-converter
 * Get data
 * Update of table
 * Reset of sample & hold capacitor
 *
 * Timing of the following code which depends on F_CPU is crucial
 * since all hardware functionality is emulated by software!
 *
 * Measurements done with F_CPU = 18,432 MHz and five NOPs (AD7813_DELAY)
 * 9-Bit ADC resoluteion and 24 bit table:
 * Falling edge INT0 til falling edge CONVST         : 1700ns
 * Pulse width CONVST is on GND                      : 700ns
 * Rising edge BUSY til rising edge CONVST           : 700ns
 * Pulse width BUSY is on VDD                        : 1700ns
 * 1st data read out - READ is on GND                : 700ns
 * READ is on VDD                                    : 400ns
 * snd data read out - READ is on GND (second pulse) : 700ns

 * Dead time (complete runtime) should be approx. 10,8us if
 * preamp is populated with 27pF || 120kOhm
 */
//__attribute__((optimize(3))) ISR(INT0_vect){
ISR(INT0_vect){
   /* Wait til preamplifier, sample & hold circuit and ADC input are
    * settled down and stable before starting a software trigger on
    * the ADC. The interrupt execution response is five clock cycles minimum.
    * After five clock cycles the program vector address for the
    * actual interrupt handling routine is executed. During these five
    * clock cycle period, the program counter (three bytes) is pushed
    * onto the stack. The vector is a jump to the interrupt routine,
    * and takes three clock cycles. Some working registers are pushed
    * onto the stack.
    *
    * Force falling edge on CONVST pin to soft trigger an ADC conversion
    * (track and hold)
    */
   AD7813_IO_CONVST_PORT &= ~_BV(AD7813_IO_CONVST_CTRL_BIT);
   /* 1.) Wait for at least AD7813_T_CONVST_FALLING_EDGE_TO_BUSY_RISING_EDGE_DELAY
    * (t3) but less than (AD7813_T_CONVERSION [t1] -
    * AD7813_T_CONVST_FALLING_EDGE_TO_BUSY_RISING_EDGE_DELAY [t3])
    * to have a stable busy signal from AD7813
    * 2.) Before rising the CONVST signal back to VDD (reset) you have
    * to wait for AD7813_T_CONVST_PULSEWIDTH (t2) to provide a minimum
    * pulse width to GND on the CONVST PIN. The CONVST must be reset
    * to VDD to operate AD7813 in the fast MODE 1 but this must happen
    * during BUSY is high
    * Condition 1.) is fullfilled if 30ns < debounce time < 2270ns
    * Condition 2.) is fullfilled if 20ns < debounce time
    */
   AD7813_DELAY
   AD7813_DELAY
   /* Reset CONVST to VDD during BUSY is high to operate ADC in MODE 1 */
   AD7813_IO_CONVST_PORT |= (_BV(AD7813_IO_CONVST_CTRL_BIT));
   /* Poll BUSY signal til AD conversion is completed (falling edge on
    * BUSY signal) */
   loop_until_bit_is_clear(AD7813_IO_BUSY_PIN, AD7813_IO_BUSY_CTRL_BIT);

   /* Read 1byte result from ADC: force READ to GND */
   AD7813_IO_RD_PORT &= ~(_BV(AD7813_IO_RD_CTRL_BIT));
   /* Wait for at least AD7813_T_DATA_ACCESS_TIME_AFTR_RD_LOW (t6) til
    * data on latch is valid */
   AD7813_DELAY
   /* Read higher byte (8 bit) from ADC latch */
   register uint8_t result1 = AD7813_IO_DATA_PIN;
   /* Wait til atmega port is stable */
   AD7813_DELAY
   /* Force READ pin to VDD (reset) */
   AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

   /* 9 or 10 bit: Serial read out of lower two bits. Not so fast but
    * higher resolution. Wait for at least either
    * AD7813_T_BUS_RELINQUISH_TIME_AFTR_RD_HIGH (t7) or
    * AD7813_T_MIN_BETWEEN_MSB_AND_LSB_READS (t8) */
   /* #if (ADC_RESOLUTION > 8) */
   AD7813_DELAY
   /* Force READ to GND to access lower 2 Bits by serial data read out */
   AD7813_IO_RD_PORT &= ~(_BV(AD7813_IO_RD_CTRL_BIT));
   AD7813_DELAY
   /* Read lower byte (8 bit) from ADC latch */
   register uint8_t result0 = AD7813_IO_DATA_PIN;
   /* Wait til ATmega latch is stable */
   AD7813_DELAY
   /* Force READ to VDD to reset the signal */
   AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

   const uint16_t index = (((((uint16_t)(result1)) << 2) | (result0 >> 6)) >> (10-ADC_RESOLUTION));

   /* 8 bit or less (delay must be adjusted separately)
   #else
     const uint16_t index = ((uint16_t)(result1) >> (8-ADC_RESOLUTION));
   #endif */

   /* The ADC needs a pause time of
    * AD7813_T_RISING_EDGE_OF_CS_OR_RD_TO_FALLING_EDGE_OF_CONVST_DELAY
    * now (t9) which is far below the rest of this ISR so we do not
    * care about it */

   /* For 24bit values, the source code looks a little more complicated
    * than just table[index]++ (even though the generated machine
    * instructions are not).  Anyway, we needed to move the increment
    * into a properly defined _inc function.
    */
   volatile histogram_element_t *element = &(table[index]);
   histogram_element_inc(element);

   /* Wait til preamp falls below INT0 trigger threshold
    * with 27pF||120k */
   _delay_us(AD7813_TO_SHRST_T_DELAY);

   /* Reset SH-CAP - 2,2us pulse width, start approx 9us after INT0 */
   SHRST_IO_PORT |= _BV(SHRST_IO_CTRL_BIT);
   _delay_us(2);
   /* Set pin to GND and release peak hold capacitor   */
   SHRST_IO_PORT &=~ _BV(SHRST_IO_CTRL_BIT);

   /* If there are any pending int0 interrupts clear them since they cannot
    * be valid (e.g. second PMT pulse occured during execution of ISR) */
   EIFR |= _BV(INTF0);
}


/** Setup of INT0
 *
 * INT0 via pin 16 is configured but not enabled
 * Trigger on falling edge
 * Enable pull up resistor on Pin 16 (20-50kOhm)
 */
inline static
void trigger_src_conf(void)
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
     * register). jump to the ISR in case of an interrupt  */
    EIMSK |= (_BV(INT0));

}


/** Initialize AD7813 peripherals
 */
inline static
void AD7813_init(void)
{
    /* ADC7813 data bus */

    /* Configure complete port group as input */
    AD7813_IO_DATA_DDR &= ~AD7813_IO_DATA_CTRL_VALUE;
    /* Disable all internal pull ups on this port, Isource=Isink=200uA */
    AD7813_IO_DATA_PORT &= ~AD7813_IO_DATA_CTRL_VALUE;

    /* Configure ADC control pins. Inputs first of all to avoid
     * short circuits */

    /* Configure BUSY as input */
    AD7813_IO_BUSY_DDR &= ~(_BV(AD7813_IO_BUSY_CTRL_BIT));
    /* Disable pull up resistor at input */
    AD7813_IO_BUSY_PORT &= ~_BV(AD7813_IO_BUSY_CTRL_BIT);

    /* Configure READ as output */
    AD7813_IO_RD_DDR |= (_BV(AD7813_IO_RD_CTRL_BIT));
    /* Set READ to VDD */
    AD7813_IO_RD_PORT |= (_BV(AD7813_IO_RD_CTRL_BIT));

    /* Configure CONVST as output */
    AD7813_IO_CONVST_DDR |= (_BV(AD7813_IO_CONVST_CTRL_BIT));
    /* Set CONVST to VDD */
    AD7813_IO_CONVST_PORT |= (_BV(AD7813_IO_CONVST_CTRL_BIT));

    /* An internal INT CONVST signal pulse is started after CONVST
     * is switched to VDD. After the ADC is powered up and INT
     * CONVST goes down the ADC is engaged and ready for track/hold
     * edge on CONVST. The power up takes a time of
     * AD7813_T_PWR_UP_AFTR_RISING_EDGE_ON_CONVST. So actually we
     * need to wait at this point for that time but it is small and
     * so we assume that all following code consumes more time  */
}


/** ADC subsystem and trigger setup */
void adc_init(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init7")));
void adc_init(void)
{
  AD7813_init();
  trigger_src_conf();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
