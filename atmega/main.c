/*
avrdude -v -p m644p -P /dev/ttyS1 -c ponyser -u -U lfuse:w:0xd7:m -U hfuse:w:0x99:m -U efuse:w:0xfc:m
avrdude -v -p m644p -P /dev/ttyS1 -c ponyser -U flash:w:data.hex -U eeprom:w:dataeep.hex
*/

/*------------------------------------------------------------------------------
 * Includes
 *------------------------------------------------------------------------------
 */

#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdlib.h>
#include <stdint.h>

#include "registers.h"


/*------------------------------------------------------------------------------
 * Defines
 *------------------------------------------------------------------------------
 */

#ifndef F_CPU
#warning "F_CPU not defined in makefile, set xtal to 18432000 Hz"
#define F_CPU 18432000UL                      //xtal frequency hz
//#define F_CPU 1000000UL                     //werksauslieferung 8mhz/8
#endif

#define BAUDRATE 9600UL

#define F_ADC_CLK_SRC 200000UL                //hz
#define ADC_DIVISION_FACTOR F_CPU/F_ADC_CLK_SRC


#define MAX_COUNTER 256
#define BIT(NO) (1<<(NO))


/*------------------------------------------------------------------------------
 * Variables  (static, not visible in other modules)
 *------------------------------------------------------------------------------
 */


uint32_t table[MAX_COUNTER];

/** maximum timer count
 *
 * Written by main() with value received from controller.
 * Read by timer interrupt handler.
 */
volatile uint16_t max_timer_count = 0;

/** maximum timer reached
 *
 * Will be set to 1 when max_timer_count is exceeded, is 0 otherwise.
 * Written by timer interrupt handler.
 * Read by run_measurements() main loop.
 */
volatile uint8_t max_timer_flag = 0;


/*------------------------------------------------------------------------------
 * Local prototypes (not visible in other modules)
 *------------------------------------------------------------------------------
 */



/*
ISR(INT0_vect) {
}*/



/** AD conversion complete interrupt entry point
 *
 * Jumped to if the AD conversion is completed
 * ADC-Callback function calls (adc_get(), histogram_update() ...)
 */
ISR(ADC_vect) {
  uint16_t result;
  uint8_t index;

  //result = ADCL;
  //result += (ADCH<<8);
  result = ADCW;

  /* 500khz adc clk -> 3,5 LSB accuracy */
  index = (uint8_t)(result>>2);

  table[index]++;

  max_timer_count++;

  /* Der logische wechsel am int0 pin hat ein interruptflag im EIFR
   * gesetzt.  schreibe logische eins ins INTFn und lösche das flag.
   * Normalerweise übernimmt das die ISR(INT0_vect){} automatisch aber
   * da wir den int0 für das auslösen des adcs nur konfigurieren aber
   * nicht freigeben müssen wir das flag manuell löschen
   *
   * antwort aus dem forum: Du musst den External INT nur
   * konfigurieren, aber nicht unbedingt freigeben. Nur das Interrupt
   * Flag muss irgendwo manuell wieder zurückgesetzt werden. Am
   * Sinnvollsten in der ISR(ADC_vect).
   */
  EIFR |= BIT(INTF0);
}


/** Setup of INT0
 *
 * INT0 Pin 16 is configured but not enabled
 * On rising edge
 * pull up resistor 20-50kOhm
 */
inline static
void trigger_src_conf(void)
{
    /* MCUCR bit PUD (4): alle pullups können global auseschaltet
     * werden (schreibe 1) */
    /* port D Data direction register */
    /* bit 2 in DDRD == 0 -> Int0 (pin 16) ist eingang, ausgang
     * sonst. lösche bit 2 */
    DDRD &= ~(BIT(DDD2));
    /* port d data register: pull up einschalten, 20-50kOhm */
    PORTD |= BIT(PD2);

    /* Disable interrupt pin "INT0" (clear interrupt enable bit in
     * External interrupt mask register) otherwise an interrupt may
     * occur if EICRA is changed */
    EIMSK &= ~(BIT(INT0));
    /* int on rising or falling edge or level triggered (External
     * interrupt control register A) */
    EICRA &= ~(BIT(ISC01) | BIT(ISC00));
    /* 11 = interrupt on rising edge (setze bit 0 und 1 auf 1) */
    EICRA |=  (BIT(ISC01) | BIT(ISC00));
    /* Clear interrupt flag by writing a locical one to INTFn in the
     * external interrupt flag register.  The flag is set when a
     * interrupt occurs. if the I-flag in the sreg is set and the
     * corresponding flag in the EIFR the program counter jumps to the
     * vector table*/
    EIFR |= BIT(INTF0);
    /* reenable interrupt INT0 (External interrupt mask
     * register). jump to the ISR in case of an interrupt */
    //EIMSK |= (BIT(INT0));

}


/** ADC initialisation and configuration
 *
 * ADC configured as auto trigger
 * Trigger source INT0
 * AREF at PIN 32
 * Channel Pin 40 ADC0
 */
inline static
void adc_init(void)
{
  uint16_t result;

  /*channel number: PIN 40 ADC0 -> ADMUX=0*/
  ADMUX = 0;

  /*select voltage reference: external AREF Pin 32 as reference*/
  ADMUX &= ~(BIT(REFS1) | BIT(REFS0));

  /*clear ADC Control and Status Register A
    enable ADC & configure IO-Pins to ADC (ADC ENable)*/
  ADCSRA = BIT(ADEN);

  /*ADC prescaler selection (ADC Prescaler Select Bits)*/
  /*bits ADPS0 .. ADPS2*/
  uint8_t adc_ps;
  adc_ps = ADC_DIVISION_FACTOR;
  ADCSRA |= ((((adc_ps>>2) & 0x1)*BIT(ADPS2)) | (((adc_ps>>1) & 0x1)*BIT(ADPS1)) | ((adc_ps & 0x01)*BIT(ADPS0)));

  /* dummy read out (first conversion takes some time) */
  /* software trigger ADC */
  ADCSRA |= BIT(ADSC);
  while ( ADCSRA & BIT(ADSC) ) {
     ;                              // conversion complete
  }

  /*clear returned AD value, other next conversion value is not ovrtaken*/
  result = ADCW;

  /* enable ad conversion complete interrupt if I-Flag in sreg is set
   * (-> ADC interrupt enable) */
  ADCSRA |= BIT(ADIE);

  /*falls der adc nicht per software im ISR ausgelöst werden soll*/

  /* Configure trigger source:
   *
   * Wenn man ADTS1 bit in ADCSRB setzt -> (auslöser external
   * interrupt request 0) s.259 und s.242.
   *
   * positive edge on trigger signal */
  ADCSRB |= BIT(ADTS1);
  ADCSRB &= ~(BIT(ADTS0) | BIT(ADTS2));

  /*ADC auto trigger enable -> adc triggered by trigger signal*/
  ADCSRA |= BIT(ADATE);
}

inline static
void run_measurement(void)
{
    sei(); /* enable interrupts I-Flag bit 7  in SREQ (status register)*/
    while (max_timer_count< 4) {
    }
    cli(); /* disable interrupts */

    ADCSRA &= ~BIT(ADATE);  //autotrigger off
}

/** USART0 initialisation to 8 databits no parity
 *
 */
inline static
void uart0_init(void)
{
  uint16_t baud_value;

  /*baud setting valid only  for asynchrounous normal mode*/
  baud_value=(F_CPU / (16L * BAUDRATE)) - 1;

  UBRR0H=(uint8_t)(baud_value>>8);
  UBRR0L=(uint8_t)baud_value;

  /*Asynchron (no clk is used); 8 databit no parity (8N1 frame format)*/
  UCSR0C = (BIT(UCSZ01) | BIT(UCSZ00));

  /*tx enable*/
  UCSR0B = BIT(TXEN0);
}

inline static
void uart_putc(unsigned char c)
{
    /* poll til output buffer is empty */
    while (!(UCSR0A & BIT(UDRE0)))
    {
    }

    /* put the char */
    UDR0 = c;
}

inline static
void uart_puts (char *s)
{
    while (*s)
    {   /* til *s != '\0' (not final string character) */
        uart_putc(*s);
        s++;
    }
}


/** counter overrun ISR */
#if 0
/*ISR(TCC0_OVF_vect) {
  static uint16_t timer_count = 0;
  timer_count++;
  if (timer_count > max_timer_count) {
    max_timer_flag = 1;
  }
}*/
#endif


/** send table[] to controller via serial port */
inline static
void send_histogram(void)
{
  /* TODO: Send header. Should say how many values of what size. */
  /* TODO: Send all table[] data via serial port */
}


void main(void) __attribute__((noreturn));
void main(void)
{

    /* Initialize global variables */
    max_timer_count = 0;
    max_timer_flag  = 0;
    for (int i=0; i<MAX_COUNTER; i++) {
      table[i]=0;
    }


    /*configure USART0 for 8N1*/
    uart0_init();

    /*configure INT0 pin 16 on rising edge*/
    trigger_src_conf();

    /* configure AREF at pin 32 and single shot auto trigger over int0
     * at pin 40 ADC0 */
    adc_init();

    /*configure unused pins*/

    /*4 measurements*/
    run_measurement();

    char s[7];
    int16_t h = -12345;

    itoa( h, s, 10 ); // 10 fuer radix -> Dezimalsystem
    uart_puts( s );

    for (int i=0; i<MAX_COUNTER; i++) {
      utoa( table[i], s, 10 );
      uart_puts( s );
    }
    //send_histogram();

    while (1) {
      ;
   }

}
