/** \file firmware/timer1-adc-trigger.c
 * \brief Timer hardware directly triggering ADC
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
 * \defgroup timer1_adc_trigger Timer hardware directly triggering ADC
 * \ingroup firmware_personality_groups
 *
 * Timer hardware directly triggering ADC
 *
 * @{
 */


/** Make sure we use the sub 1 second timer resolution */
#define TIMER1_SUB_1SEC

#include "global.h"
#include "main.h"
#include "data-table.h"
#include "perso-adc-int-global.h"
#include "timer1-adc-trigger.h"
#include "timer1-constants.h"
#include "packet-comm.h"
#include "wdt-softreset.h"


/** timer counter
 *
 * Initialized once by main() with value received from host
 * controller. Never touched by main() again after starting the timer
 * interrupt.
 *
 * Timer interrupt handler has exclusive access to read/writes
 * timer1_count to decrement, once the timer ISR has been enabled.
 */
volatile uint16_t timer1_count;


/** timer counter */
volatile uint16_t timer1_count;


/** Original timer count received in the command.
 *
 * Used later for determining how much time has elapsed yet. Written
 * once only, when the command has been received.
 */
volatile uint16_t orig_timer1_count;


/** FIXME
 *
 * Is sent by hostware. Number of dropped analog samples (downsampling of
 * analog signal sampled with timer1 time base)
 */
volatile uint16_t orig_skip_samples;
volatile uint16_t skip_samples;


/** ADC trigger source
 *
 *   0 free running mode
 *   2 external INT0
 *   5 timer1 compare match B
 *   ...
 */
#define ADC_TRIGGER_SOURCE 5


/** Timer1 compare output mode for channel A for non-PWM mode:
 *
 * Toggle LED pin 19 on ATmega644 DIP40 on compare match A.
 *
 *   0  OCnA disconnected
 *   1  toggle OCnA on compare match
 *   2  clear OCnA on compare match
 *   3  set OCnA on compare match
 */
#define TIMER1_COMA_MODE 1


/** Timer1 compare output mode for channel B for non-PWM mode:
 *
 * Toggle pin 18 on ATmega644 DIP40 on compare match B. Conflicts with
 * Pollin board usage for switch 3!
 */
#define TIMER1_COMB_MODE 1


/** Set up our IO pins */
void timer1_adc_trigger_io_init(void)
  __attribute__((naked))
  __attribute__((section(".init5")));
void timer1_adc_trigger_io_init(void)
{
  /* Toggled pin on port PD4 on compare match B. This is ATmega644
   * DIP40 pin 18.  Conflicts with Pollin board usage for switch 3!
   */
  DDRD |= (_BV(DDD4));

  /* Configure "measurement in progress LED"                      */
  /* configure ATmega644 pin 19 as an output */
  DDRD |= (_BV(DDD5));
}


/** Configure 16 bit timer to trigger an ISR every 0.1 second
 *
 * Configure "measurement in progress toggle LED-signal"
 */
void timer1_init(void)
{
  /* Derive sample rate (time base) as a multiple of the base
     compare match value for 0.1sec. Write to output compare
     reg. A                                                       */
  OCR1A = (TIMER1_COMPARE_MATCH_VAL);

  /* The ADC can only be triggered via compare register B.
     Set the trigger point (compare match B) to 50% of
     compare match A                                              */
  OCR1B = (TIMER1_COMPARE_MATCH_VAL >> 1);

  /* Configure and start timer */
  TCCR1A =
    BITF(TIMER1_COMA_MODE, COM1A, 0) |
    BITF(TIMER1_COMA_MODE, COM1A, 1) |
    BITF(TIMER1_COMB_MODE, COM1B, 0) |
    BITF(TIMER1_COMB_MODE, COM1B, 1) |
    BITF(TIMER1_WGM_MODE,  WGM1, 0) |
    BITF(TIMER1_WGM_MODE,  WGM1, 1);
  TCCR1B =
    BITF(TIMER1_PRESCALER,  CS1, 0) |
    BITF(TIMER1_PRESCALER,  CS1, 1) |
    BITF(TIMER1_PRESCALER,  CS1, 2) |
    BITF(TIMER1_WGM_MODE,  WGM1, 2) |
    BITF(TIMER1_WGM_MODE,  WGM1, 3);

  /* we do not need to jump to any ISRs since we do everything
     inside the ADC callback function                             */

  /* output compare match B interrupt enable                      */
  //TIMSK1 |= BIT(OCIE1B);

  /* output compare match A interrupt enable                      */
  //TIMSK1 |= _BV(OCIE1A);
}


/** \todo Should give out a reasonable value */
uint16_t get_duration(void)
{
  return 0;
}


/** ADC initialisation and configuration
 *
 * ADC configured as auto trigger
 * Trigger source compare register B
 * Use external analog reference AREF at PIN 32
 * AD input channel on Pin 40 ADC0
 */
inline static
void adc_init(void)
{
  uint16_t result;

  ADMUX =
    /* select voltage reference: 0: external AREF Pin 32 as reference */
    BITF(0, REFS, 1) |
    BITF(0, REFS, 0) |
    /* ADC input channel number: PIN 40 ADC0 -> ADMUX=0 */
    BITF(0, MUX, 4) |
    BITF(0, MUX, 3) |
    BITF(0, MUX, 2) |
    BITF(0, MUX, 1) |
    BITF(0, MUX, 0) |
    0;

  /* ADC Control and Status Register A */
  ADCSRA =
    /* enable ADC & configure IO-Pins to ADC (ADC ENable) */
    _BV(ADEN) |
    BITF(ADC_PRESCALER, ADPS, 2) |
    BITF(ADC_PRESCALER, ADPS, 1) |
    BITF(ADC_PRESCALER, ADPS, 0) |
    0;

  /* dummy read out (first conversion takes some time) */
  /* software triggered AD-Conversion */
  ADCSRA |= _BV(ADSC);

  /* wait until conversion is complete */
  loop_until_bit_is_clear(ADCSRA, ADSC);

  /* clear returned AD value, other next conversion value is not ovrtaken */
  result = ADCW;

  /* Enable AD conversion complete interrupt if I-Flag in sreg is set
   * (-> ADC interrupt enable) */
  ADCSRA |= _BV(ADIE);

  /* Configure ADC trigger source:
   * Select external trigger trigger ADC on Compare Match B of Timer1 */
  ADCSRB =
    BITF(ADC_TRIGGER_SOURCE, ADTS, 2) |
    BITF(ADC_TRIGGER_SOURCE, ADTS, 1) |
    BITF(ADC_TRIGGER_SOURCE, ADTS, 0) |
    0;

  /* ADC auto trigger enable: ADC will be started by trigger signal */
  ADCSRA |= _BV(ADATE);
}


/** \bug Handle two uint16_t values from parameters: measurement
 *       duration and skip_samples.
 */
void personality_start_measurement_sram(void)
{
  size_t ofs = 0;

  if (personality_info.param_data_size_timer_count == 2) {
    const void *timer1_count_vp = &pparam_sram.params[ofs];
    const uint16_t *timer1_count_p = timer1_count_vp;
    orig_timer1_count = *timer1_count_p;
    timer1_count = *timer1_count_p;

    /** Safeguard: We cannot handle 0 or 1 count measurements.
     *
     * Enable this if you do something with get_duration() above.
     *
    if (orig_timer1_count <= 1) {
      send_text_P(PSTR("Unsupported timer value <= 1"));
      wdt_soft_reset();
    }
    */

    ofs += 2;
  }

  if (personality_info.param_data_size_skip_samples == 2) {
    const void *skip_samples_vp = &pparam_sram.params[ofs];
    const uint16_t *skip_samples_p = skip_samples_vp;
    orig_skip_samples = *skip_samples_p;
    skip_samples = *skip_samples_p;
  }

  adc_init();
  timer1_init();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
