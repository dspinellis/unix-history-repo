/* Emulation of getpagesize() for systems that need it.
   Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*

NAME

	getpagesize -- return the number of bytes in page of memory

SYNOPSIS

	int getpagesize (void)

DESCRIPTION

	Returns the number of bytes in a page of memory.  This is the
	granularity of many of the system memory management routines.
	No guarantee is made as to whether or not it is the same as the
	basic memory management hardware page size.

BUGS

	Is intended as a reasonable replacement for systems where this
	is not provided as a system call.  The value of 4096 may or may
	not be correct for the systems where it is returned as the default
	value.

*/


#include <sys/types.h>
#include <sys/param.h>

#ifdef HAVE_SYSCONF
#include <unistd.h>
#define GNU_OUR_PAGESIZE sysconf(_SC_PAGESIZE)
#else
#ifdef	PAGESIZE
#define	GNU_OUR_PAGESIZE PAGESIZE
#else	/* no PAGESIZE */
#ifdef	EXEC_PAGESIZE
#define	GNU_OUR_PAGESIZE EXEC_PAGESIZE
#else	/* no EXEC_PAGESIZE */
#ifdef	NBPG
#define	GNU_OUR_PAGESIZE (NBPG * CLSIZE)
#ifndef	CLSIZE
#define	CLSIZE 1
#endif	/* CLSIZE */
#else	/* no NBPG */
#ifdef	NBPC
#define	GNU_OUR_PAGESIZE NBPC
#else	/* no NBPC */
#define	GNU_OUR_PAGESIZE 4096	/* Just punt and use reasonable value */
#endif /* NBPC */
#endif /* NBPG */
#endif /* EXEC_PAGESIZE */
#endif /* PAGESIZE */
#endif /* HAVE_SYSCONF */

int
getpagesize ()
{
  return (GNU_OUR_PAGESIZE);
}

