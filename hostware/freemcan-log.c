/** \file hostware/freemcan-log.c
 * \brief Logging mechanism implementation
 *
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 * \defgroup freemcan_log FreeMCAn logging system
 * \ingroup hostware_generic
 *
 * The FreeMCAn logging system. It provides a way to log status
 * messages, error messages (including errno codes) in a way that can
 * be used with different user interfaces.
 *
 * @{
 */


#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "freemcan-log.h"


/** Default fmlog handler writing to stderr
 *
 * \todo Use writev(2)?
 */
static void default_fmlog_handler(void *data __attribute__ (( unused )),
                                  const char *message,
                                  const size_t length)
{
  ssize_t ret = write(STDERR_FILENO, (void *) message, length);
  assert(ret >= 0);
  ssize_t ret2 = write(STDERR_FILENO, "\n", 1);
  assert(ret2 >= 0);
}


/** Pointer to log handler function */
static fmlog_handler_t fmlog_handler = default_fmlog_handler;


/** Pointer to log handler function's data */
static void *fmlog_handler_data = NULL;


void fmlog_reset_handler(void)
{
  fmlog_handler = default_fmlog_handler;
  fmlog_handler_data = NULL;
}


void fmlog_set_handler(fmlog_handler_t the_fmlog_handler,  void *the_data)
{
  fmlog_handler = the_fmlog_handler;
  fmlog_handler_data = the_data;
}


void fmlog(const char *format, ...)
{
  va_list ap;
  if (fmlog_handler) {
    /** \bug Use va_copy? */
    va_start(ap, format);
    static char buf[4096];
    int r = vsnprintf(buf, sizeof(buf), format, ap);
    assert((r >= 0) && (((unsigned int)r)<sizeof(buf)));
    va_end(ap);

    fmlog_handler(fmlog_handler_data, buf, r);
  }
}


void fmlog_error(const char *format, ...)
{
  const int errno_copy = errno;
  va_list ap;
  if (fmlog_handler) {
    /** \bug Use va_copy? */
    va_start(ap, format);
    static char buf[4096];
    int r = vsnprintf(buf, sizeof(buf), format, ap);
    assert((r >= 0) && (((unsigned int)r)<sizeof(buf)));
    va_end(ap);

    const char *errmsg = strerror(errno_copy);
    const size_t errlen = strlen(errmsg);

    /* buf = buf + ": " + strerror(errno_copy); */
    char *p;
    for (p=buf; *p!='\0'; p++) /* look for \0 */ ;
    assert(buf+sizeof(buf) >= p+2+errlen+1);
    *p++ = ':';
    *p++ = ' ';
    for (const char *q = errmsg; *q != '\0'; q++, p++) {
      *p = *q;
    }
    *p = '\0';
    ssize_t to_write = p-buf;

    fmlog_handler(fmlog_handler_data, buf, to_write);
  }
}


/** Print debug string */
#define DEBUG(...)                              \
  do {                                          \
    fprintf(stderr, __VA_ARGS__);               \
  } while (0)


/** Return a printable character */
static char printable(const char ch)
{
  if ((32 <= ch) && (ch < 127)) {
    return ch;
  } else {
    return '.';
  }
}


/* Print hexdump of data block */
void fmlog_data(const char *prefix, const void *data, const size_t size)
{
  const uint8_t *b = (const uint8_t *)data;
  for (size_t y=0; y<size; y+=16) {
    char buf[80];
    ssize_t idx = 0;
    idx += sprintf(&(buf[idx]), "%04zx ", y);
    for (size_t x=0; x<16; x++) {
      const size_t i = x+y;
      if (i<size) {
        idx += sprintf(&(buf[idx]), " %02x", b[i]);
      } else {
        idx += sprintf(&(buf[idx]), "   ");
      }
    }
    idx += sprintf(&buf[idx], "  ");
    for (size_t x=0; x<16; x++) {
      const size_t i = x+y;
      if (i<size) {
        idx += sprintf(&buf[idx], "%c", printable(b[i]));
      } else {
        idx += sprintf(&buf[idx], " ");
      }
    }
    fmlog("%s%s", prefix, buf);
  }
}


void fmlog_data32(const char *prefix, const void *data, const size_t size)
{
  const uint8_t *b = (const uint8_t *)data;
  for (size_t y=0; y<size; y+=16) {
    char buf[80];
    ssize_t idx = 0;
    idx += sprintf(&(buf[idx]), "%04zx ", y);
    for (size_t x=0; x<16; x+=4) {
      const size_t i = x+y;
      if (i<size) {
        const uint32_t value = *((const uint32_t *)&b[i]);
        idx += sprintf(&(buf[idx]), " %08x", value);
      } else {
        idx += sprintf(&(buf[idx]), " " "    " "    ");
      }
    }
    fmlog("%s%s", prefix, buf);
  }
}


void fmlog_data16(const char *prefix, const void *data, const size_t size)
{
  const uint8_t *b = (const uint8_t *)data;
  for (size_t y=0; y<size; y+=16) {
    char buf[80];
    ssize_t idx = 0;
    idx += sprintf(&(buf[idx]), "%04zx ", y);
    for (size_t x=0; x<16; x+=2) {
      const size_t i = x+y;
      if (i<size) {
        const uint16_t value = *((const uint16_t *)&b[i]);
        idx += sprintf(&(buf[idx]), " %04x", value);
      } else {
        idx += sprintf(&(buf[idx]), " " "    ");
      }
    }
    fmlog("%s%s", prefix, buf);
  }
}


void fmlog_data24(const char *prefix, const void *data, const size_t size)
{
  const uint8_t *b = (const uint8_t *)data;
  for (size_t y=0; y<size; y+=24) {
    char buf[80];
    ssize_t idx = 0;
    idx += sprintf(&(buf[idx]), "%04zx ", y);
    for (size_t x=0; x<24; x+=3) {
      const size_t i = x+y;
      if (i<size) {
        const uint32_t value =
          (((uint32_t)b[i])   <<  0) +
          (((uint32_t)b[i+1]) <<  8) +
          (((uint32_t)b[i+2]) << 16);
        idx += sprintf(&(buf[idx]), " %06x", value);
      } else {
        idx += sprintf(&(buf[idx]), " " "    " "  ");
      }
    }
    fmlog("%s%s", prefix, buf);
  }
}


/* documented in freemcan-log.h */
void fmlog_value_table(const char *prefix, const uint32_t *elements, const size_t count)
{
  /* Find largest element in array */
  uint32_t max = 0;
  for (size_t i=0; i<count; i++) {
    if (elements[i] > max) {
      max = elements[i];
    }
  }

  /* Lookup table for places per number, and numbers per line */
  unsigned int places, perline;
  if      (max >= 1000000000) { places = 10; perline =  4; }
  else if (max >=  100000000) { places =  9; perline =  4; }
  else if (max >=   10000000) { places =  8; perline =  8; }
  else if (max >=    1000000) { places =  7; perline =  8; }
  else if (max >=     100000) { places =  6; perline =  8; }
  else if (max >=      10000) { places =  5; perline =  8; }
  else if (max >=       1000) { places =  4; perline =  8; }
  else if (max >=        100) { places =  3; perline = 16; }
  else if (max >=         10) { places =  2; perline = 16; }
  else                        { places =  1; perline = 32; }

  /* prepare format string for numbers */
  char fmt[10] = "";
  sprintf(fmt, " %%%dd", places);

  for (size_t y=0; y<count; y+=perline) {
    char line[80] = "";
    ssize_t idx = sprintf(&(line[0]), "%4zu:", y);
    const int xmax = (y+perline<=count) ? (perline) : (count%perline);
    for (int x=0; x<xmax; x++) {
      if ((x&7) == 0) {
        idx += sprintf(&(line[idx]), " ");
      }
      idx += sprintf(&(line[idx]), fmt, elements[y+x]);
    }
    fmlog("%s%s", prefix, line);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
