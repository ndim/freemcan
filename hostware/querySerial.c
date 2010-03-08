/** \file hostware/querySerial.c
 * \brief Serial port access code template for further use in freemcan.
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

#include <stdio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <termios.h>
#include <stdlib.h>
#include <unistd.h>

#include "serial-setup.h"


int main(int argc, char *argv[])
{
  int fd, result;
  long baud;
  char buffer[256];

  if (argc != 3)
  {
    printf("Usage: querySerial device portspeed \n");
    exit( 1 );
  }

  baud = serial_string_to_baud(argv[2]);

  /* open the serial port device file
   * O_NDELAY - tells port to operate and ignore the DCD line
   * O_NOCTTY - this process is not to become the controlling
   *            process for the port. The driver will not send
   *            this process signals due to keyboard aborts, etc.
   */
  if ((fd = serial_open(argv[1])) < 0)
  {
    printf("Couldn't open %s\n",argv[1]);
    exit(1);
  }

  serial_setup(fd, baud);

  /* write the users command out the serial port */
  /*
  result = write(fd, argv[4], strlen(argv[4]));
  if (result < 0)
  {
    fputs("write failed\n", stderr);
    close(fd);
    exit(1);
  }
  */

  /* wait for awhile, based on the user's timeout value in mS */
  /* usleep(atoi(argv[3]) * 1000); */

  while(1){
    /* read the input buffer and print it */
    result = read(fd,buffer,255);
    buffer[result] = '\0'; /* zero terminate so printf works */
    printf("%s\n",buffer);
  }

  /* close the device file */
  close(fd);
}
