/** \file firmware/perso-adc-int-global.h
 * \brief Global adjustments for internal ADC related stuff
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
 * \defgroup global_constants_adc_int Global Constants And Definitions for internal ADC
 * \ingroup firmware_personality_groups
 * @{
 */

#ifndef PERSO_ADC_INT_GLOBAL_H
#define PERSO_ADC_INT_GLOBAL_H


/** ADC prescaler selection for ADC clock source frequency
 *
 *  0: ADC clock divider=2
 *  1: ADC clock divider=2
 *  2: ADC clock divider=4
 *  3: ADC clock divider=8
 *  4: ADC clock divider=16
 *  5: ADC clock divider=32
 *  6: ADC clock divider=64
 *  7: ADC clock divider=128
 *
 *  Select a prescaler in order to set a resonable ADC clock frequency.
 *  For ATMEGA644P the nominal frequency range lies between 50 - 200kHz.
 *
 *  ADC clock division factor = F_CPU [Hz]/ADC clock source frequency [Hz]
 *  E.g. 16000kHz/64 = 250khz
 */
#define ADC_PRESCALER (6)


/** ADC resolution in bit
 *
 *  Put in here resonable value:
 *  E.g. 8 bit resolution at 500 kHz.
 *  (ATMEGA644P has 3,5LSB accuracy at 1Mhz; 4V)
 */
#define ADC_RESOLUTION (10)


/** @} */


#endif /* !PERSO_ADC_INT_GLOBAL_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
