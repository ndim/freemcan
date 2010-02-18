#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include <sys/select.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>


#define READ_FILENO 3
#define WRITE_FILENO 4


#define DEBUG(...)				\
  do {						\
    fprintf(stderr, __VA_ARGS__);		\
  } while (0)


static void do_copy_data(const int in_fd, const int out_fd,
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
  return bytes_to_read;
}


static void copy_data(const int in_fd, const int out_fd)
{
  const int bytes_to_read = read_size(in_fd);
  if (bytes_to_read > 0) {
    do_copy_data(in_fd, out_fd, bytes_to_read);
  }
}


static void main_loop(const char *unix_name)
{
  const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  assert(sock>0);
  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  assert(strlen(unix_name) < sizeof(addr.sun_path));
  strcpy(addr.sun_path, unix_name);
  const int bind_ret = bind(sock, (const struct sockaddr *)&addr, sizeof(addr));
  if (bind_ret < 0) {
    perror("bind");
    abort();
  }

  const int listen_ret = listen(sock, 0);
  if (listen_ret < 0) {
    perror("listen");
    abort();
  }
  DEBUG("Waiting...\n");

  const int connfd = accept(sock, NULL, 0);
  if (connfd < 0) {
    perror("accept");
    abort();
  }
  DEBUG("Connected\n");

  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(READ_FILENO, &in_fdset);
    FD_SET(connfd, &in_fdset);
    const int max_fd = connfd;
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
      if (FD_ISSET(READ_FILENO, &in_fdset)) {
	copy_data(READ_FILENO, connfd);
      }
      if (FD_ISSET(connfd, &in_fdset)) {
	copy_data(connfd, WRITE_FILENO);
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
