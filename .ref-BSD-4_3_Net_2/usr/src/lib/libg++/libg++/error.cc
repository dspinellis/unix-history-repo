/* 
Copyright (C) 1990 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifdef __GNUG__
#pragma implementation
#endif
#include <stdio.h>
#include <builtin.h>

void default_one_arg_error_handler(const char* msg)
{
  fputs("Error: ", stderr);
  fputs(msg, stderr);
  fputs("\n", stderr);
  abort();
}


void default_two_arg_error_handler(const char* kind, const char* msg)
{
  fputs(kind, stderr);
  fputs(" Error: ", stderr);
  fputs(msg, stderr);
  fputs("\n", stderr);
  abort();
}

two_arg_error_handler_t lib_error_handler = default_two_arg_error_handler;

two_arg_error_handler_t set_lib_error_handler(two_arg_error_handler_t f)
{
  two_arg_error_handler_t old = lib_error_handler;
  lib_error_handler = f;
  return old;
}

