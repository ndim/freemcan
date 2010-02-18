#include <assert.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


int main(int argc, char *argv[])
{
  assert(argc == 2);
  assert(argv[1] != NULL);
  const char *device_name = argv[1];
  printf("freemcan-main: device=%s\n", device_name);
  int device_fd = open(device_name, O_NOCTTY|O_RDWR);
  assert(device_fd > 0);
  printf("device_fd = %d\n", device_fd);
  return 0;
}
