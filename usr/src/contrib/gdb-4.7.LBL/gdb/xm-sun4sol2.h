/* Macro definitions for running gdb on a Sun 4 running Solaris 2.
   Copyright 1989, 1992 Free Software Foundation, Inc.

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

/* Most of what we know is generic to SPARC hosts.  */

#include "xm-sparc.h"

/* Pick up more stuff from the generic SVR4 host include file. */

#include "xm-sysv4.h"

/* SVR4's can't seem to agree on what to call the type that contains the
   general registers.  Kludge around it with a #define.  */

#define	gregset_t	prgregset_t
#define	fpregset_t	prfpregset_t

/* The native Sun compiler complains about using volatile
   to indicate functions that never return.  So shut it up by simply
   defining away "NORETURN", which is normally defined to "volatile". */

#ifndef __GNUC__
# define NORETURN /**/
#endif

/* Large alloca's fail because the attempt to increase the stack limit in
   main() fails because shared libraries are allocated just below the initial
   stack limit.  The SunOS kernel will not allow the stack to grow into
   the area occupied by the shared libraries.  Sun knows about this bug
   but has no obvious fix for it.  */
#define BROKEN_LARGE_ALLOCA

/* If you expect to use the mmalloc package to obtain mapped symbol files,
   for now you have to specify some parameters that determine how gdb places
   the mappings in it's address space.  See the comments in map_to_address()
   for details.  This is expected to only be a short term solution.  Yes it
   is a kludge.
   FIXME:  Make this more automatic. */

#define MMAP_BASE_ADDRESS	0xE0000000	/* First mapping here */
#define MMAP_INCREMENT		0x01000000	/* Increment to next mapping */

/* These are not currently used in SVR4 (but should be, FIXME!).  */
#undef	DO_DEFERRED_STORES
#undef	CLEAR_DEFERRED_STORES

/* May be needed, may be not?  From Pace Willisson's port.  FIXME.  */
#define NEED_POSIX_SETPGID

/* Solaris PSRVADDR support does not seem to include a place for nPC.  */
#define PRSVADDR_BROKEN
