/** \file hostware/test-log.c
 * \brief Test the code from freemcan-log.c
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
 */

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#include "freemcan-log.h"


static void test_fmlog_hist(void)
{
  static const size_t numbers[] =
    { 0, 1, 2,
      14, 15, 16, 17, 18,
      30, 31, 32, 33 };

  uint32_t *testarray = calloc(128, 4);
  assert(testarray);
  testarray[0] = 99;
  for (unsigned int i=0; i<(sizeof(numbers)/sizeof(numbers[0])); i++) {
    fmlog("tess_fmlog_hist: %u elements", numbers[i]);
    fmlog_value_table("PREFIX ", testarray, numbers[i]);
  }
  fmlog("tess_fmlog_hist: Done.");
  free(testarray);
}


int main()
{
  char buf[50] = "DataData DataData Data Moo Meh Feh Gna Erks.";
  fmlog("Test");
  fmlog_data("PFX ", buf, sizeof(buf));
  test_fmlog_hist();
  return 0;
}


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
