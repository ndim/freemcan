#include <stdio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <termios.h>
#include <stdlib.h>
#include <unistd.h>

#include "serial-setup.h"

/* These are the hash definitions */
#define USERBAUD1200 '1'+'2'
#define USERBAUD2400 '2'+'4'
#define USERBAUD9600 '9'+'6'
#define USERBAUD1920 '1'+'9'
#define USERBAUD3840 '3'+'8'


int main(int argc, char *argv[])
{
  int fd, whichBaud, result;
  long baud;
  char buffer[256];

  if (argc != 3)
  {
    printf("Usage: querySerial device portspeed \n");
    exit( 1 );
  }

  /* compute which baud rate the user wants using a simple adding
   * hash function
   */
  whichBaud = argv[2][0] + argv[2][1];

  switch (whichBaud) {
    case USERBAUD1200:
      baud = B1200;
      break;
    case USERBAUD2400:
      baud = B2400;
      break;
    case USERBAUD9600:
      baud = B9600;
      break;
    case USERBAUD1920:
      baud = B19200;
      break;
    case USERBAUD3840:
      baud = B38400;
      break;
    default:
      printf("Baud rate %s is not supported, "
	     "use 1200, 2400, 9600, 19200 or 38400.\n", argv[2]);
      exit(1);
      break;
  }

  /* open the serial port device file
   * O_NDELAY - tells port to operate and ignore the DCD line
   * O_NOCTTY - this process is not to become the controlling
   *            process for the port. The driver will not send
   *            this process signals due to keyboard aborts, etc.
   */
  if ((fd = open(argv[1],O_RDWR | O_NDELAY | O_NOCTTY)) < 0)
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
