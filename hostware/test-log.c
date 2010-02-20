#include "freemcan-log.h"

int main()
{
  char buf[50] = "DataData DataData Data Moo Meh Feh Gna Erks.";
  fmlog("Test");
  fmlog_data(buf, sizeof(buf));
  return 0;
}
