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

/*
 * Include limits to override definitions in defs.h.
 */
#include <machine/limits.h>
#include "xm-sparc.h"

/* Get rid of any system-imposed stack limit if possible.  */

#ifdef notdef
#define SET_STACK_LIMIT_HUGE
#endif

/* Enable use of alternate code for Sun's format of core dump file.  */

#define NEW_SUN_CORE

/* Do implement the attach and detach commands.  */

#define ATTACH_DETACH

/* Override copies of {fetch,store}_inferior_registers in infptrace.c.  */

#define FETCH_INFERIOR_REGISTERS

/* Before storing, we need to read all the registers.  */

#define CHILD_PREPARE_TO_STORE() read_register_bytes (0, NULL, REGISTER_BYTES)

/* It does have a wait structure, and it might help things out . . . */

#define HAVE_WAIT_STRUCT

#define FPU

/* Large alloca's fail because the attempt to increase the stack limit in
   main() fails because shared libraries are allocated just below the initial
   stack limit.  The SunOS kernel will not allow the stack to grow into
   the area occupied by the shared libraries.  Sun knows about this bug
   but has no obvious fix for it.  */
/* XXX is this true for BSD */
#define BROKEN_LARGE_ALLOCA

#define PSIGNAL_IN_SIGNAL_H

#define PTRACE_ARG3_TYPE char*
