#include <assert.h>
#include <errno.h>
#include <unistd.h>

#include "freemcan-device.h"
#include "freemcan-device-select.h"
#include "freemcan-log.h"
#include "freemcan-signals.h"
#include "freemcan-tui.h"

/**
 * \defgroup freemcan_tui_select TUI handling for select(2) based main loop
 * \ingroup hostware_tui
 * \ingroup mainloop_select
 */


/** Set up select() data structure with (ncurses based) text UI
 * \ingroup freemcan_tui_select
 */
int tui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


/** Do TUI's IO stuff if necessary (from select loop)
 * \ingroup freemcan_tui_select
 */
void tui_select_do_io(fd_set *in_fdset)
{
  /* user interface do_io */
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    tui_do_io();
  }
}


/************************************************************************/
/** \defgroup mainloop_select Main loop based on select(2)
 * \ingroup hostware
 * @{
 */
/************************************************************************/


/** TUI's main program with select(2) based main loop */
int main(int argc, char *argv[])
{
  const char *device_name = main_init(argc, argv);

  /** device init */
  device_select_init(device_name);

  /** initialize output module */
  tui_init();

  /** main loop */
  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);

    int max_fd = -1;
    max_fd = tui_select_set_in(&in_fdset, max_fd);
    max_fd = device_select_set_in(&in_fdset, max_fd);
    assert(max_fd >= 0);

    const int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
	fmlog_error("select(2)");
	abort();
      }
    } else if (0 == n) { /* timeout */
      fmlog("select(2) timeout");
      abort();
    } else { /* n>0 */
      device_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }

    if (sigint || sigterm || quit_flag) {
      break;
    }

  } /* main loop */

  /* clean up */
  device_select_fini();
  tui_fini();

  /* implicitly call atexit_func */
  exit(EXIT_SUCCESS);
}

/** @} */
