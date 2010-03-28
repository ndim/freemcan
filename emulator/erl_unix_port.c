/** \file emulator/erl_unix_port.c
 * \brief Specialized Erlang port to interface to AF_UNIX sockets
 *
 * \bug Implement FSM status -> state change.
 *
 * \bug Should perhaps be replaced by kernel-2.13.4/examples/uds_dist
 *
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Hans Ulrich Niedermann.
 */

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


/** Read from Erlang on this file descriptor */
#define READ_FILENO 3

/** Write to Erlang on this file descriptor */
#define WRITE_FILENO 4


/** Print debug message on stderr (FD 2) */
#define DEBUG(...)                              \
    do {                                        \
        fprintf(stderr, "EUP: " __VA_ARGS__);   \
    } while (0)


/** We need a max() function a few times in preparation for select(2) */
static inline
int max(const int a, const int b)
{
    if (a>b) return a;
    else return b;
}


/** Copy a given number of bytes from one FD to another */
static void do_copy_data(const int in_fd, const int out_fd,
                         const int data_size)
{
    DEBUG("Copying %d bytes from fd %d to fd %d\n", data_size, in_fd, out_fd);
    char buf[data_size];
    const ssize_t read_chars = read(in_fd, buf, sizeof(buf));
    assert(data_size == read_chars);
    if (out_fd < 0) {
        DEBUG("Dropping the data\n");
    } else {
        write(out_fd, buf, sizeof(buf));
    }
}


/** Size of data to be read from a file descriptor without blocking */
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


/************************************************************************
 * Erlang port communications
 ************************************************************************/


/** Connection file descriptor
 *
 * Is maintained at value -1 when there is no connection, and some FD
 * >= 0 when there is a connection on the UNIX domain socket.
 *
 * Forward declaration, it does not really belong at this place, but
 * we need to define it here.
 */
static int connfd = -1;


/** Erlang port select(2) setup */
static int port_select_set_in(fd_set *in_fdset, int max_in)
{
    FD_SET(READ_FILENO, in_fdset);
    return max(max_in, READ_FILENO);
}


/** Erlang port select(2) IO execution */
static void port_select_do_io(fd_set *in_fdset)
{
    if (FD_ISSET(READ_FILENO, in_fdset)) {
        DEBUG("Data from READ_FILENO\n");
        const int bytes_to_read = read_size(READ_FILENO);
        if (bytes_to_read > 0) {
            do_copy_data(READ_FILENO, connfd, bytes_to_read);
        } else {
            /* connection to Erlang close, abort this */
            DEBUG("No data?\n");
            abort();
        }
    }
}


/************************************************************************
 * connection sockets
 ************************************************************************/


/** Init connection socket logic with file descriptor */
static void conn_init(const int fd)
{
    DEBUG("Connected: fd=%d\n", fd);
    assert(connfd == -1);
    connfd = fd;
}


/** Close down connection socket logic */
static void conn_fini()
{
    close(connfd);
    connfd = -1;
}


/** Connection logic select(2) setup */
static int conn_select_set_in(fd_set *in_fdset, int max_in)
{
    if (connfd == -1) {
        return max_in;
    }
    FD_SET(connfd, in_fdset);
    return max(connfd, max_in);
}


/** Connection logic select(2) IO execution */
static void conn_select_do_io(fd_set *in_fdset)
{
    if (connfd == -1) {
        return;
    }
    if (FD_ISSET(connfd, in_fdset)) {
        const int bytes_to_read = read_size(connfd);
        if (bytes_to_read > 0) {
            DEBUG("Data from connfd\n");
            do_copy_data(connfd, WRITE_FILENO, bytes_to_read);
        } else {
            /* connection closed */
            DEBUG("Closed connection from connfd\n");
            conn_fini();
        }
    }
}


/************************************************************************
 * listen sockets
 ************************************************************************/


static int listen_sock = -1;


static void listen_init(const char *unix_name)
{
    listen_sock = socket(AF_UNIX, SOCK_STREAM, 0);
    assert(listen_sock>0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    assert(strlen(unix_name) < sizeof(addr.sun_path));
    strcpy(addr.sun_path, unix_name);
    const int bind_ret =
        bind(listen_sock, (const struct sockaddr *)&addr, sizeof(addr));
    if (bind_ret < 0) {
        perror("bind");
        abort();
    }
    const int listen_ret = listen(listen_sock, 0);
    if (listen_ret < 0) {
        perror("listen");
        abort();
    }
}


static int listen_select_set_in(fd_set *in_fdset, int max_in)
{
    FD_SET(listen_sock, in_fdset);
    return max(listen_sock, max_in);
}


static void listen_select_do_io(fd_set *in_fdset)
{
    if (FD_ISSET(listen_sock, in_fdset)) {
        const int connfd = accept(listen_sock, NULL, NULL);
        if (connfd < 0) {
            if (errno == EINTR) {
                return;
            }  else {
                perror("accept");
                abort();
            }
        }
        conn_init(connfd);
    }
}


/************************************************************************
 *
 ************************************************************************/


static void main_loop(const char *unix_name)
{
    listen_init(unix_name);

    /** \todo Simulate a FreeMCA device that boots whenever someone
     * connects to the UNIX socket.
     */

    /** \todo Make the Erlang code updatable at run-time. */

    while (1) {
        fd_set in_fdset;
        FD_ZERO(&in_fdset);
        int max_fd = -1;
        max_fd = port_select_set_in(&in_fdset, max_fd);
        max_fd = conn_select_set_in(&in_fdset, max_fd);
        max_fd = listen_select_set_in(&in_fdset, max_fd);
        assert(max_fd >= 1);
        DEBUG("Waiting for... uhm... stuff to happen.\n");
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
            port_select_do_io(&in_fdset);
            conn_select_do_io(&in_fdset);
            listen_select_do_io(&in_fdset);
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


/*
 * Local Variables:
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
