#include <assert.h>
#include <errno.h>
#include <unistd.h>

#include <poll.h>

#include "freemcan-device.h"
#include "freemcan-log.h"
#include "freemcan-signals.h"
#include "freemcan-tui.h"

/**
 * \defgroup freemcan_tui_poll TUI handling for poll(2) based main loop
 * \ingroup hostware_tui
 * \ingroup mainloop_poll
 * @{
 */


static
void tui_poll_handler(struct pollfd *pfd)
{
  if (pfd->revents & POLLIN) {
    tui_do_io();
  }
}


void tui_poll_setup(struct pollfd *pollfds, poll_handler_t *pollhandlers,
		    nfds_t *index, const nfds_t limit)
{
  assert(index);
  assert(*index < limit);
  pollfds[*index].fd = STDIN_FILENO;
  pollfds[*index].events = POLLIN;
  pollfds[*index].revents = 0;
  pollhandlers[*index] = tui_poll_handler;
  (*index)++;
}

/** @} */

/************************************************************************/
/** \defgroup mainloop_poll Main loop based on poll(2)
 * \ingroup hostware
 * @{
 */
/************************************************************************/

/** TUI's main program with poll(2) based main loop
 *
 * \todo To be implemented.
 */
int main(int argc, char *argv[])
{
  const char *device_name = main_init(argc, argv);
  /** device init */
  dev_init(device_name);

  /** initialize output module */
  tui_init();

#define MAX_POLLFDS (2)
  struct pollfd pollfds[MAX_POLLFDS];
  poll_handler_t pollhandlers[MAX_POLLFDS];
  nfds_t index = 0;
  dev_poll_setup(pollfds, pollhandlers, &index, MAX_POLLFDS);
  tui_poll_setup(pollfds, pollhandlers, &index, MAX_POLLFDS);
  assert(index > 0);

  /** main loop */
  while (1) {

    const int n = poll(pollfds, index, -1);
    if (n<0) {
      if (errno != EINTR) { /** \todo Is it correct to ignore EINTR with poll(2)? */
	fmlog_error("poll(2)");
	abort();
      }
    } else if (n == 0) { /* timeout */
      fmlog("poll(2) timeout");
      abort();
    } else { /* n>0 */
      for (nfds_t i=0; i<index; i++) {
	if (pollhandlers[i] && pollfds[i].revents) {
	  pollhandlers[i](&(pollfds[i]));
	}
      }
    }

    if (sigint || sigterm || quit_flag) {
      break;
    }
  } /* main loop */

  /* clean up */
  dev_fini();
  tui_fini();

  /* implicitly call atexit_func */
  exit(EXIT_SUCCESS);
}

/** @} */

