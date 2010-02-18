#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

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
    printf("character device\n");
    return open(device_name, O_NOCTTY|O_RDWR);
  } else if (S_ISSOCK(sb.st_mode)) {
    printf("socket\n");
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
    printf("Out of device_fds space\n");
    abort();
  } else {
    printf("unknown?\n");
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
    if (FD_ISSET(device_fd, in_fdset)) {
      const int bytes_to_read = read_size(device_fd);
      if (bytes_to_read == 0) {
	DEBUG("Closing down FD %d\n", device_fd);
	dev_close(device_fd);
	continue;
      }
      assert(bytes_to_read > 0);
      char buf[bytes_to_read];
      const ssize_t read_bytes = read(device_fd, buf, sizeof(buf));
      assert(read_bytes == bytes_to_read);
      printf("Received %d bytes from %d: %s\n", read_bytes, device_fd, buf);
    }
  }
}


int main(int argc, char *argv[])
{
  assert(argc == 2);
  assert(argv[1] != NULL);
  const char *device_name = argv[1];
  printf("freemcan-main: device=%s\n", device_name);
  const int device_fd = dev_open(device_name);
  assert(device_fd > 0);
  printf("device_fd = %d\n", device_fd);
  write(device_fd, "Yeah!", 5);
  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(device_fd, &in_fdset);
    int max_fd = -1;
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
    }
  }
  return 0;
}
