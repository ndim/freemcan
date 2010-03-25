/** \file hostware/serial-setup.c
 * \brief Serial port access code
 *
 * \author Copyright (C) 2001 by Craig Hollabaugh
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
 * \defgroup serial_setup Serial Port Setup
 * \ingroup hostware_generic
 * @{
 */

#include <sys/ioctl.h>
#include <fcntl.h>
#include <termios.h>
#include <stdlib.h>
#include <stdio.h>

/** Configure serial port
 *
 *  No parity 8 characters (8N1)
 *  Read from device via blocking mode
 *  Arguments: file descriptor for device and baudrate
 */

void serial_setup(const int fd, const long TERM_SPEED)
{
  struct termios tio;

  /* CS8:         8 data bits
   * CREAD:       allow input to be received / enable receiver
   * CLOCAL:      modem control signal to open port (ignore CD setting)
   */
  tio.c_cflag = TERM_SPEED | CS8 | CREAD | CLOCAL;

  /* PARENB:      no parity bit
   * CSTOPB:      use two stop bits after each transmitted character
   * CSIZE:
   * HUPCL:       if defice is closed reset DTR and RTS (hang up modem)
   * CRTSCTS:     hardware flow control (request to send RTS and clear to send CTS)
   */
  tio.c_cflag &= ~PARENB;
  tio.c_cflag &= ~CSTOPB;
  tio.c_cflag &= ~CSIZE;
  tio.c_cflag &= ~HUPCL;

  /* set input flag noncanonical, no processing */
  tio.c_lflag = 0;

  /* ignore parity errors */
  tio.c_iflag = IGNPAR;

  /* set output flag noncanonical, no processing */
  tio.c_oflag = 0;

  /* no time delay */
  tio.c_cc[VTIME] = 0;
  /* no char delay */
  tio.c_cc[VMIN]  = 0;

  /* flush the buffer */
  tcflush(fd, TCIFLUSH);
  /* set the attributes */
  tcsetattr(fd, TCSANOW, &tio);

  /* If the port operates in raw data mode, each read(2) system call will
   * return the number of characters that are actually available in the serial
   * input buffers. If no characters are available, the call will block
   * (wait) until characters come in, an interval timer expires, or an
   * error occurs. The read function can be made to return immediately
   * by doing fcntl(fd, F_SETFL, FNDELAY); The FNDELAY option causes the read
   * function to return 0 if no characters are available on the port.
   * To restore normal (blocking) behavior, call fcntl() without the
   * FNDELAY option.
   */
  fcntl(fd, F_SETFL, 0);
}

/** Returns appropriate baud definition from termios.h
*/


long serial_get_baud(const long whichBaud)
{
  switch (whichBaud) {
    case  50:      return  B50;
    case  75:      return  B75;
    case  110:     return  B110;
    case  134:     return  B134;
    case  150:     return  B150;
    case  200:     return  B200;
    case  300:     return  B300;
    case  600:     return  B600;
    case  1200:    return  B1200;
    case  1800:    return  B1800;
    case  2400:    return  B2400;
    case  4800:    return  B4800;
    case  9600:    return  B9600;
    case  19200:   return  B19200;
    case  38400:   return  B38400;
    case  57600:   return  B57600;
    case  115200:  return  B115200;
    case  230400:  return  B230400;
    case  460800:  return  B460800;
    case  500000:  return  B500000;
    case  576000:  return  B576000;
    case  921600:  return  B921600;
    case  1000000: return  B1000000;
    case  1152000: return  B1152000;
    case  1500000: return  B1500000;
    case  2000000: return  B2000000;
    case  2500000: return  B2500000;
    case  3000000: return  B3000000;
    case  3500000: return  B3500000;
    case  4000000: return  B4000000;
    /* No "default:" label to force the compiler to complain about
     * unhandled cases. We still handle other cases, but after the
     * switch() statement. */
  }
  fprintf(stderr,
	  "Baud rate %d is not supported"
	  "\n", whichBaud);
  exit(1);
}


int serial_open(const char *device_name)
{
  /*
   * O_NDELAY - tells port to operate and ignore the DCD line
   * O_NOCTTY - do not make this our controlling tty, ever
   */
  return open(device_name, O_RDWR | O_NDELAY | O_NOCTTY);
}


/** @} */
