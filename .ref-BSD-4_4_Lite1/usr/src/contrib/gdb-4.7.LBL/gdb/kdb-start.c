/* Main loop for the standalone kernel debugger, for GDB, the GNU Debugger.
   Copyright 1989, 1991 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"

static char *args[] = {"kdb", "kdb-symbols", 0};

static char *environment[] = {0};

char **environ;

start ()
{
#ifdef NAMES_HAVE_UNDERSCORE
  INIT_STACK (_kdb_stack_beg, _kdb_stack_end);
#else /* not NAMES_HAVE_UNDERSCORE */
  INIT_STACK (kdb_stack_beg, kdb_stack_end);
#endif /* not NAMES_HAVE_UNDERSCORE */

  environ = environment;

  main (2, args, environment);
}
