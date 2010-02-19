/** \file serial-setup.c
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
 */

#include <sys/ioctl.h>
#include <fcntl.h>
#include <termios.h>
#include <stdlib.h>


/** Set up serial port parameters */
void serial_setup(const int fd, const long baud)
{
  struct termios tio;

 /* Like the character size you must manually set the parity enable
  * and parity type bits.  UNIX serial drivers support even, odd, and
  * no parity bit generation. Space parity can be simulated with
  * clever coding.

    * No parity (8N1):
      options.c_cflag &= ~PARENB
      options.c_cflag &= ~CSTOPB
      options.c_cflag &= ~CSIZE;
      options.c_cflag |= CS8;

    * Even parity (7E1):
      options.c_cflag |= PARENB
      options.c_cflag &= ~PARODD
      options.c_cflag &= ~CSTOPB
      options.c_cflag &= ~CSIZE;
      options.c_cflag |= CS7;

    * Odd parity (7O1):
      options.c_cflag |= PARENB
      options.c_cflag |= PARODD
      options.c_cflag &= ~CSTOPB
      options.c_cflag &= ~CSIZE;
      options.c_cflag |= CS7;

    * Space parity is setup the same as no parity (7S1):
      options.c_cflag &= ~PARENB
      options.c_cflag &= ~CSTOPB
      options.c_cflag &= ~CSIZE;
      options.c_cflag |= CS8;
 */

  /* we are not concerned about preserving the old serial port configuration
   * CS8, 8 data bits
   * CREAD, receiver enabled
   * CLOCAL, don't change the port's owner
   */

  tio.c_cflag = baud | CS8 | CREAD | CLOCAL;

  /* No parity (8N1): */
  tio.c_cflag &= ~PARENB;
  tio.c_cflag &= ~CSTOPB;
  tio.c_cflag &= ~CSIZE;


  /* Some versions of UNIX support hardware flow control using the CTS
   * (Clear To Send) and RTS (Request To Send) signal lines. If the
   * CNEW_RTSCTS or CRTSCTS constants are defined on your system then
   * hardware flow control is probably supported. Do the following to
   * enable hardware flow control:
   *
   * options.c_cflag |= CNEW_RTSCTS;    // Also called CRTSCTS
   *
   * Similarly, to disable hardware flow control:
   *
   * options.c_cflag &= ~CNEW_RTSCTS;
   */

  tio.c_cflag &= ~HUPCL; /* clear the HUPCL bit, close doesn't change DTR */

  /* Choosing Raw Input */
  /* Canonical input is line-oriented. Input characters are put into a
   * buffer which can be edited interactively by the user until a CR
   * (carriage return) or LF (line feed) character is received.  Raw
   * input is unprocessed. Input characters are passed through exactly as
   * they are received, when they are received. Generally you'll deselect
   * the ICANON, ECHO, ECHOE, and ISIG options when using raw input:
   *
   * options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
   */

  tio.c_lflag = 0;       /* set input flag noncanonical, no processing */

  tio.c_iflag = IGNPAR;  /* ignore parity errors */

  tio.c_oflag = 0;       /* set output flag noncanonical, no processing */

  tio.c_cc[VTIME] = 0;   /* no time delay */
  tio.c_cc[VMIN]  = 0;   /* no char delay */

  tcflush(fd, TCIFLUSH); /* flush the buffer */
  tcsetattr(fd, TCSANOW, &tio); /* set the attributes */

  /* Set up for no delay, ie nonblocking reads will occur.
     When we read, we'll get what's in the input buffer or nothing */

  /* Reading data from a port is a little trickier. When you operate the
   * port in raw data mode, each read(2) system call will return the
   * number of characters that are actually available in the serial
   * input buffers. If no characters are available, the call will block
   * (wait) until characters come in, an interval timer expires, or an
   * error occurs. The read function can be made to return immediately
   * by doing the following:
   *
   *  fcntl(fd, F_SETFL, FNDELAY);
   *
   * The FNDELAY option causes the read function to return 0 if no
   * characters are available on the port. To restore normal (blocking)
   * behavior, call fcntl() without the FNDELAY option:
   *
   *  fcntl(fd, F_SETFL, 0);
   */


  /* fcntl(fd, F_SETFL, FNDELAY); */
  fcntl(fd, F_SETFL, 0);
}

