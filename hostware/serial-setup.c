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

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "serial-setup.h"


/** Element type for list auf baud rates and constants #baud_table. */
typedef struct {
  const long baudrate;
  const long baudconst;
} baud_t;


/** Shortcut macro for defining #baud_table */
#define B(N) {N, B##N}


/** Table of baud rates and corresponding constants.
 *
 * Might be extended by accessor functions to e.g. expose the values
 * in a GUI.
 *
 * In case you happen upon a system where Bnnnn is undefined, please
 * wrap that line as follows and submit a patch:
 *
 * \code
 *     #ifdef Bnnnn
 *       B(nnnn),
 *     #endif
 * \endcode
 */
baud_t baud_table[] = {
  B(50),
  B(75),
  B(110),
  B(134),
  B(150),
  B(200),
  B(300),
  B(600),
  B(1200),
  B(2400),
  B(4800),
  B(9600),
  B(19200),
  B(38400),
  B(57600),
  B(115200),
  B(230400),
  B(460800),
  B(500000),
  B(576000),
  B(921600),
  B(1000000),
  B(1152000),
  B(1500000),
  B(2000000),
  B(2500000),
  B(3000000),
  B(3500000),
  B(4000000),
  {0, 0}
};


/* documented in serial-setup.h */
void serial_setup(const int fd,
                  const long baudrate,
                  const int bits_per_byte,
                  const serial_parity_t parity,
                  const int stop_bits)
{
  struct termios tio;
  /* Initialize memory to zero, as tcgetattr(2) does not write all
   * bytes in tio, and we need to do byte-by-byte comparison later and
   * need something reproducible for that. */
  memset(&tio, '\0', sizeof(tio));
  tcgetattr(fd, &tio);

  const long baudconst = serial_get_baudconst(baudrate);

  /* CREAD:       allow input to be received / enable receiver
   * CLOCAL:      modem control signal to open port (ignore CD setting)
   */
  tio.c_cflag = CREAD | CLOCAL;

  /* set baud rate */
  cfsetispeed(&tio, baudconst);
  cfsetospeed(&tio, baudconst);

  /* PARENB:      no parity bit
   * CSTOPB:      use two stop bits after each transmitted character
   * CSIZE:
   * HUPCL:       if defice is closed reset DTR and RTS (hang up modem)
   * CRTSCTS:     hardware flow control (request to send RTS and clear to send CTS)
   */
  tio.c_cflag &= ~HUPCL;

  /* Set parity (N,E,O) */
  bool parity_set = false;
  switch (parity) {
  case PARITY_NONE:
    tio.c_cflag &= ~PARENB;
    parity_set = true;
    break;
#ifdef PARITY_SUPPORT_NOT_IMPLEMENTED_YET
  We do not have proper parity support yet. Need a decision on whether we will ignore
  bytes with wrong parity or forward them while marked or whatever.
  case PARITY_EVEN:
    tio.c_cflag |=  PARENB;
    tio.c_cflag &= ~PARODD;
    parity_set = true;
    break;
  case PARITY_ODD:
    tio.c_cflag |=  PARENB;
    tio.c_cflag |=  PARODD;
    parity_set = true;
    break;
#endif
  }
  if (!parity_set) {
    fprintf(stderr, "Invalid parity value %d\n", parity);
    abort();
  }

  /* set stop bits */
  switch (stop_bits) {
  case 1: tio.c_cflag &= ~CSTOPB; break;
  case 2: tio.c_cflag |=  CSTOPB; break;
  default:
    fprintf(stderr, "Invalid stop_bits value: %d\n", stop_bits);
    abort();
    break;
  }

  /* set bits per byte */
  switch (bits_per_byte) {
  case 8:
    tio.c_cflag &= ~CSIZE; /* Mask the character size bits */
    tio.c_cflag |= CS8;    /* Select 8 data bits */
    break;
  case 7:
    tio.c_cflag &= ~CSIZE; /* Mask the character size bits */
    tio.c_cflag |= CS7;    /* Select 7 data bits */
    break;
  default:
    fprintf(stderr, "Invalid bits_per_byte value %d\n", bits_per_byte);
    abort();
    break;
  }

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
  const int ret_tcflush = tcflush(fd, TCIFLUSH);
  if (ret_tcflush < 0) {
    perror("tcflush");
    abort();
  }

  /* Set the attributes. The tcsetattr(2) return value has no
   * meaningful information, so we need to first set the new
   * attributes (tio), then get the current attributes (tio2) and
   * finally we can check whether tio and tio2 match. */
  tcsetattr(fd, TCSANOW, &tio);
  if (1) {
    struct termios tio2;
    /* Initialize to zero like tio above to give us reproducible results. */
    memset(&tio2, '\0', sizeof(tio2));
    tcgetattr(fd, &tio2);
    if (0 != memcmp(&tio, &tio2, sizeof(tio))) {
      fprintf(stderr, "Error setting struct termios (tcsetattr)\n");
      /* fmlog_data(&tio, sizeof(tio));
       * fmlog_data(&tio2, sizeof(tio2)); */
      abort();
    }
  }

  /* If the port operates in raw data mode, each read(2) system call will
   * return the number of characters that are actually available in the serial
   * input buffers. If no characters are available, the call will block
   * (wait) until characters come in, an interval timer expires, or an
   * error occurs. The read function can be made to return immediately
   * by doing fcntl(fd, F_SETFL, FNDELAY); The FNDELAY option causes the read
   * function to return 0 if no characters are available on the port.
   * To restore normal (blocking) behavior, call fcntl() without the
   * FNDELAY option. Blocking behaviour is fine if you do IO multiplexing
   * on the file descriptor with select(2), poll(2) or epoll(2).
   */
  fcntl(fd, F_SETFL, 0);
}


/* documented in serial-setup.h */
long serial_get_baudconst(const long baudrate)
{
  for (unsigned int i=0; baud_table[i].baudrate != 0; i++) {
    if (baud_table[i].baudrate == baudrate) {
      return baud_table[i].baudconst;
    }
  }
  fprintf(stderr, "Baud rate %ld is not supported\n", baudrate);
  exit(1);
}


/* documented in serial-setup.h */
long serial_get_baudrate(const long baudconst)
{
  for (unsigned int i=0; baud_table[i].baudrate != 0; i++) {
    if (baud_table[i].baudconst == baudconst) {
      return baud_table[i].baudconst;
    }
  }
  fprintf(stderr, "Baud const %ld=0%lo is not supported\n",
          baudconst, baudconst);
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


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
