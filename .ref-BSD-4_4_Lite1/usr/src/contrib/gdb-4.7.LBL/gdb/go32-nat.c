/* Native dependent code for dos running GO32 for GDB, the GNU debugger.
   Copyright 1992 Free Software Foundation, Inc.

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

#include <stdio.h>

static void uerror(char *s)
{
  fprintf(stderr, "Fatal: %s!\n");
  exit(1);
}

fetch_core_registers(){uerror("attempt to call fetch_core_registers()");}

fetch_inferior_registers(){uerror("attempt to call fetch_inferior_registers()");}
store_inferior_registers(){uerror("attempt to call store_inferior_registers()");}

child_resume(){uerror("attempt to call child_resume()");}
child_xfer_memory(){uerror("attempt to call child_xfer_memory()");}

