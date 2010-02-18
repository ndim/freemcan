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


int open_device(const char *device_name)
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
    return sock;
  } else {
    printf("unknown?\n");
    abort();
  }
}


int main(int argc, char *argv[])
{
  assert(argc == 2);
  assert(argv[1] != NULL);
  const char *device_name = argv[1];
  printf("freemcan-main: device=%s\n", device_name);
  const int device_fd = open_device(device_name);
  assert(device_fd > 0);
  printf("device_fd = %d\n", device_fd);
  return 0;
}
