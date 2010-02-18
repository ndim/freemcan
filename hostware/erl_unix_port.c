#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include <sys/select.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>


#define DEBUG(...)				\
  do {						\
    fprintf(stderr, __VA_ARGS__);		\
  } while (0)


static void copy_data(const int in_fd, const int out_fd,
		      const int data_size)
{
  char buf[data_size];
  const ssize_t read_chars = read(in_fd, buf, sizeof(buf));
  assert(data_size == read_chars);
  write(out_fd, buf, sizeof(buf));
}


static int read_size(const int in_fd)
{
  int bytes_to_read;
  int r = ioctl(in_fd, FIONREAD, &bytes_to_read);
  if (r < 0) {
    DEBUG("cannot determine number of characters to read from stdin");
    abort();
  }
  assert(r > 0);
  return bytes_to_read;
}


static void main_loop(const char *unix_name)
{
  const int unix = open(unix_name, O_NOCTTY|O_RDWR);
  assert(unix>0);
  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(STDIN_FILENO, &in_fdset);
    FD_SET(unix, &in_fdset);
    const int max_fd = unix;
    int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
	perror("select");
	abort();
      }
    } else if (0 == n) { /* timeout */
      DEBUG("select timeout\n");
      abort();
    } else { /* n>0 */
      if (FD_ISSET(STDIN_FILENO, &in_fdset)) {
	copy_data(STDIN_FILENO, unix, read_size(STDIN_FILENO));
      }
      if (FD_ISSET(unix, &in_fdset)) {
	copy_data(unix, STDIN_FILENO, read_size(unix));
      }
    }
  }
}


int main(int argc, char *argv[])
{
  DEBUG("%s\n", __FILE__);
  for (int i=0; i<argc; i++) {
    DEBUG("argv[%d] = %s\n", i, argv[i]);
  }
  assert(argc == 2);
  assert(argv[1] != NULL);
  const char *unix_name = argv[1];
  main_loop(unix_name);
  return 0;
}
