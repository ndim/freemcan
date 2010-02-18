#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <errno.h>


#define DEBUG(...)				\
  do {						\
    fprintf(stderr, __VA_ARGS__);		\
  } while (0)


static int read_size(const int in_fd)
{
  int bytes_to_read;
  int r = ioctl(in_fd, FIONREAD, &bytes_to_read);
  if (r < 0) {
    DEBUG("cannot determine number of characters to read from stdin");
    abort();
  }
  return bytes_to_read;
}


#define MAX_DEVICE_FDS 1
static int device_fds[MAX_DEVICE_FDS];


char printable(const char ch)
{
  if ((32 <= ch) && (ch < 127)) {
    return ch;
  } else {
    return '.';
  }
}


void hexdump(const char *buf, const size_t size)
{
  const uint8_t *b = (const uint8_t *)buf;
  for (size_t y=0; y<size; y+=16) {
    DEBUG("%04x ", y);
    for (int x=0; x<16; x++) {
      if (y*16+x<size) {
	DEBUG(" %02x", b[y*16+x]);
      } else {
	DEBUG("   ");
      }
    }
    DEBUG("  ");
    for (size_t x=0; x<16; x++) {
      if (y*16+x<size) {
	DEBUG("%c", printable(b[y*16+x]));
      } else {
	DEBUG(" ");
      }
    }
    DEBUG("\n");
  }
}


int dev_open(const char *device_name)
{
  /* FIXME: Run stat(2) on device_name, then change open()-like code
   *        according to file type (device file, unix domain socket).
   */
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (S_ISCHR(sb.st_mode)) {
    DEBUG("%s: character device\n", device_name);
    return open(device_name, O_NOCTTY|O_RDWR);
  } else if (S_ISSOCK(sb.st_mode)) {
    DEBUG("%s: socket\n", device_name);
    const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    assert(strlen(device_name) < sizeof(addr.sun_path));
    strcpy(addr.sun_path, device_name);
    const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
    if (connect_ret < 0) {
      perror("connect");
      abort();
    }
    for (int i=0; i<MAX_DEVICE_FDS; i++) {
      if (device_fds[i] == 0) {
	device_fds[i] = sock;
	return sock;
      }
    }
    DEBUG("Out of device_fds space\n");
    abort();
  } else {
    DEBUG("unknown?\n");
    abort();
  }
}


void dev_close(const int device_fd)
{
  for (int i=0; i<MAX_DEVICE_FDS; i++) {
    if (device_fds[i] == device_fd) {
      device_fds[i] = 0;
      close(device_fd);
      return;
    }
  }
}


int dev_select_set_in(fd_set *in_fdset, int maxfd)
{
  int ret = maxfd;
  for (int i=0; i<MAX_DEVICE_FDS; i++) {
    const int fd = device_fds[i];
    if (fd != 0) {
      FD_SET(fd, in_fdset);
      if (fd > ret) {
	ret = fd;
      }
    }
  }
  return ret;
}


void dev_select_do_io(fd_set *in_fdset)
{
  for (int i=0; i<MAX_DEVICE_FDS; i++) {
    const int device_fd = device_fds[i];
    if (device_fd == 0) {
      continue;
    }
    if (FD_ISSET(device_fd, in_fdset)) {
      const int bytes_to_read = read_size(device_fd);
      if (bytes_to_read == 0) {
	DEBUG("Closing down FD %d\n", device_fd);
	dev_close(device_fd);
	continue;
      }
      assert(bytes_to_read > 0);
      char buf[bytes_to_read+1];
      const ssize_t read_bytes = read(device_fd, buf, bytes_to_read);
      assert(read_bytes == bytes_to_read);
      buf[bytes_to_read] = '\0';
      DEBUG("Received %d bytes from fd %d: %s\n", read_bytes, device_fd, buf);
      hexdump(buf, read_bytes);
    }
  }
}


int ui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


void ui_select_do_io(fd_set *in_fdset)
{
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    const int device_fd = device_fds[0];
    const int bytes_to_read = read_size(device_fd);
    if (bytes_to_read == 0) {
      DEBUG("EOF from stdin, exiting.\n");
      exit(0);
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(STDIN_FILENO, buf, sizeof(buf));
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    DEBUG("Received %d bytes from fd %d: %s\n", read_bytes, STDIN_FILENO, buf);
    hexdump(buf, read_bytes);
    DEBUG("Copying data to fd %d\n", device_fd);
    write(device_fd, buf, sizeof(buf));
  }
}


/* Next up: char-by-char input */

int main(int argc, char *argv[])
{
  assert(argc == 2);
  assert(argv[1] != NULL);
  assert(isatty(STDIN_FILENO));
  assert(isatty(STDOUT_FILENO));
  const char *device_name = argv[1];
  DEBUG("freemcan-main: device=%s\n", device_name);
  const int device_fd = dev_open(device_name);
  assert(device_fd > 0);
  DEBUG("device_fd = %d\n", device_fd);
  // write(device_fd, "Yeah!", 5);
  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(device_fd, &in_fdset);
    int max_fd = -1;
    max_fd = ui_select_set_in(&in_fdset, max_fd);
    max_fd = dev_select_set_in(&in_fdset, max_fd);
    assert(max_fd >= 0);
    const int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
	perror("select");
	abort();
      }
    } else if (0 == n) { /* timeout */
      DEBUG("select timeout\n");
      abort();
    } else { /* n>0 */
      dev_select_do_io(&in_fdset);
      ui_select_do_io(&in_fdset);
    }
  }
  return 0;
}
