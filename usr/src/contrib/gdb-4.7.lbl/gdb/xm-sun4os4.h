/* Macro definitions for running gdb on a Sun 4 running sunos 4.
   Copyright (C) 1989, Free Software Foundation, Inc.

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

#include "xm-sparc.h"
#define FPU

/* Large alloca's fail because the attempt to increase the stack limit in
   main() fails because shared libraries are allocated just below the initial
   stack limit.  The SunOS kernel will not allow the stack to grow into
   the area occupied by the shared libraries.  Sun knows about this bug
   but has no obvious fix for it.  */
#define BROKEN_LARGE_ALLOCA

/* SunOS 4.x has memory mapped files.  */

#define HAVE_MMAP

/* If you expect to use the mmalloc package to obtain mapped symbol files,
   for now you have to specify some parameters that determine how gdb places
   the mappings in it's address space.  See the comments in map_to_address()
   for details.  This is expected to only be a short term solution.  Yes it
   is a kludge.
   FIXME:  Make this more automatic. */

#define MMAP_BASE_ADDRESS	0xE0000000	/* First mapping here */
#define MMAP_INCREMENT		0x01000000	/* Increment to next mapping */

/* /usr/include/malloc.h defines these w/o prototypes (and uses
   char * instead of void *).  */
#define MALLOC_INCOMPATIBLE
extern char* malloc PARAMS (());
extern char* realloc PARAMS (());
extern int free PARAMS (());

/* SunOS 4.x uses nonstandard "char *" as type of third argument to ptrace() */

#define PTRACE_ARG3_TYPE char*
