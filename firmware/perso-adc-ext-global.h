/** \file firmware/perso-adc-ext-global.h
 * \brief Global adjustments for external ADC related stuff
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
 * \defgroup global_constants_adc_ext Global Constants And Definitions for external ADC
 * \ingroup firmware_personality_groups
 * @{
 */

#ifndef PERSO_ADC_EXT_GLOBAL_H
#define PERSO_ADC_EXT_GLOBAL_H


#include <stdint.h>


/** Sample & hold capacitor control: PORT configuration */
#define SHRST_IO_PORT PORTD


/** Sample & hold capacitor control: DDR configuration */
#define SHRST_IO_DDR DDRD


/** Sample & hold capacitor control: Pseudonym configuration */
#define SHRST_IO_CTRL_BIT PD6


/** ADC resolution in bit
 *
 *  Put in a reasonable value (e.g. 10 bit ADC -> 9 bit)
 */
#define ADC_RESOLUTION (9)


/** @} */


#endif /* !PERSO_ADC_EXT_GLOBAL_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
