/** \file firmware/ad7813.h
 * \brief Defines for external ADC (AD7813) peripherals and timing
 *
 * \author Copyright (C) 2010 samplemaker
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
 * @{
 */

#ifndef AD7813_H
#define AD7813_H

#include <avr/io.h>

/* AD7813 input/output-peripherals */

/** AD7813 input control "READ" for data acquisistion:
    PORT configuration */
#define AD7813_IO_RD_PORT PORTB
/** AD7813 input control "READ" for data acquisistion:
    DDR configuration */
#define AD7813_IO_RD_DDR DDRB
/** AD7813 input control "READ" for data acquisistion:
    Pseudonym configuration */
#define AD7813_IO_RD_CTRL_BIT PB0          //replacement for PORTB0 and DDB0

/** AD7813 input control "CONVERSIONSTART" for power up and conversion
    start: PORT configuration */
#define AD7813_IO_CONVST_PORT PORTB
/** AD7813 input control "CONVERSIONSTART" for power up and conversion
    start: DDR configuration */
#define AD7813_IO_CONVST_DDR DDRB
/** AD7813 input control "CONVERSIONSTART" for power up and conversion
    start: Pseudonym configuration */
#define AD7813_IO_CONVST_CTRL_BIT PB1      //replacement for PORTB1 and DDB1

/** AD7813 output control "BUSYSIGNAL" for conversion pending:
    PORT configuration */
#define AD7813_IO_BUSY_PORT PORTB
/** AD7813 output control "BUSYSIGNAL" for conversion pending:
    DDR configuration */
#define AD7813_IO_BUSY_DDR DDRB
/** AD7813 output control "BUSYSIGNAL" for conversion pending:
    PIN configuration */
#define AD7813_IO_BUSY_PIN PINB
/** AD7813 output control "BUSYSIGNAL" for conversion pending:
    Pseudonym configuration */
#define AD7813_IO_BUSY_CTRL_BIT PB2        //replacement for PORTB2, DDB2 and PINB2

/** AD7813 databus output: PORT configuration */
#define AD7813_IO_DATA_PORT PORTA
/** AD7813 databus output: DDR configuration */
#define AD7813_IO_DATA_DDR DDRA
/** AD7813 databus output: PIN configuration */
#define AD7813_IO_DATA_PIN PINA
/** AD7813 databus output: IO - BUS configuration as pseudonym */
#define AD7813_IO_DATA_CTRL_VALUE (_BV(PA0) | _BV(PA1) | _BV(PA2) | _BV(PA3) | _BV(PA4) | _BV(PA5) | _BV(PA6) | _BV(PA7))

/* AD7813 timings in [ns] */
// #define AD7813_T_PWR_UP_AFTR_RISING_EDGE_ON_CONVST 1500                      //tPOWER-UP
// #define AD7813_T_CONVERSION 2300                                             //t1
// #define AD7813_T_CONVST_PULSEWIDTH 20                                        //t2
// #define AD7813_T_CONVST_FALLING_EDGE_TO_BUSY_RISING_EDGE_DELAY 30            //t3
// #define AD7813_T_CS_TO_RD_SETUP_TIME 0                                       //t4
// #define AD7813_T_CS_HOLD_AFTR_RD_HIGH 0                                      //t5
// #define AD7813_T_DATA_ACCESS_TIME_AFTR_RD_LOW 10                             //t6
// #define AD7813_T_BUS_RELINQUISH_TIME_AFTR_RD_HIGH 10                         //t7
// #define AD7813_T_MIN_BETWEEN_MSB_AND_LSB_READS 10                            //t8
// #define AD7813_T_RISING_EDGE_OF_CS_OR_RD_TO_FALLING_EDGE_OF_CONVST_DELAY 50  //t9

/* ISR0 delays in dependancy of core frequency (feature is experimental !) */
#if (F_CPU > 15000000)
  #define AD7813_DELAY asm (" nop \n nop \n nop \n nop \n nop \n " :: );
  #define AD7813_TO_SHRST_T_DELAY 1
#else
  #define AD7813_DELAY asm (" nop \n nop \n nop \n" :: );
  #define AD7813_TO_SHRST_T_DELAY 0
#endif

#endif /* !AD7813_H */
