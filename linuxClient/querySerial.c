#include <stdio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <termios.h>
#include <stdlib.h>

/* These are the hash definitions */
#define USERBAUD1200 '1'+'2'
#define USERBAUD2400 '2'+'4'
#define USERBAUD9600 '9'+'6'
#define USERBAUD1920 '1'+'9'
#define USERBAUD3840 '3'+'8'

struct termios tio;

int main(int argc, char *argv[])
{
  int fd, status, whichBaud, result;
  long baud;
  char buffer[255];

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
      printf("Baud rate %s is not supported, ");
      printf("use 1200, 2400, 9600, 19200 or 38400.\n", argv[2]);
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

 /*Like the character size you must manually set the parity enable and parity type bits.
 UNIX serial drivers support even, odd, and no parity bit generation. Space parity can be
 simulated with clever coding.

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

// No parity (8N1):
  tio.c_cflag &= ~PARENB;
  tio.c_cflag &= ~CSTOPB;
  tio.c_cflag &= ~CSIZE;


/*Some versions of UNIX support hardware flow control using the CTS (Clear To Send) and RTS
(Request To Send) signal lines. If the  CNEW_RTSCTS or CRTSCTS constants are defined on your
system then hardware flow control is probably supported. Do the following to enable hardware flow control:
    options.c_cflag |= CNEW_RTSCTS;    // Also called CRTSCTS
Similarly, to disable hardware flow control:
    options.c_cflag &= ~CNEW_RTSCTS;*/


  tio.c_cflag &= ~HUPCL; /* clear the HUPCL bit, close doesn't change DTR */

/*Choosing Raw Input*/
/*Canonical input is line-oriented. Input characters are put into a buffer which can be edited interactively by the user until a CR (carriage return) or LF (line feed) character is received.
Raw input is unprocessed. Input characters are passed through exactly as they are received, when they are received. Generally you'll deselect the ICANON, ECHO, ECHOE, and ISIG options when using raw input:
    options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);*/

  tio.c_lflag = 0;       /* set input flag noncanonical, no processing */

  tio.c_iflag = IGNPAR;  /* ignore parity errors */

  tio.c_oflag = 0;       /* set output flag noncanonical, no processing */

  tio.c_cc[VTIME] = 0;   /* no time delay */
  tio.c_cc[VMIN]  = 0;   /* no char delay */

  tcflush(fd, TCIFLUSH); /* flush the buffer */
  tcsetattr(fd, TCSANOW, &tio); /* set the attributes */

/* Set up for no delay, ie nonblocking reads will occur.
   When we read, we'll get what's in the input buffer or nothing */

/*Reading data from a port is a little trickier. When you operate the port in raw data mode, each read(2) system
call will return the number of characters that are actually available in the serial input buffers. If no characters
are available, the call will block (wait) until characters come in, an interval timer expires, or an error occurs. The
read function can be made to return immediately by doing the following:

    fcntl(fd, F_SETFL, FNDELAY);

The FNDELAY option causes the read function to return 0 if no characters are available on the port. To restore
 normal (blocking) behavior, call fcntl() without the FNDELAY option:

    fcntl(fd, F_SETFL, 0);
*/


  //fcntl(fd, F_SETFL, FNDELAY);
    fcntl(fd, F_SETFL, 0);

/* write the users command out the serial port */
 /* result = write(fd, argv[4], strlen(argv[4]));
  if (result < 0)
  {
    fputs("write failed\n", stderr);
    close(fd);
    exit(1);
  }*/

/* wait for awhile, based on the user's timeout value in mS*/
  //usleep(atoi(argv[3]) * 1000);

while(1){
/* read the input buffer and print it */
  result = read(fd,buffer,255);
  buffer[result] = 0; // zero terminate so printf works
  printf("%s\n",buffer);
}

/* close the device file */
  close(fd);
}
