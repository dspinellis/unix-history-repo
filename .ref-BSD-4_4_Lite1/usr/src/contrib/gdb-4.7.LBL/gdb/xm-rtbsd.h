/* Definitions to host GDB on an IBM RT/PC running BSD Unix. 
   Copyright 1986, 1987, 1989, 1991, 1992  Free Software Foundation, Inc.
   Contributed by David Wood @ New York University (wood@lab.ultra.nyu.edu).

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

/* Get rid of any system-imposed stack limit if possible.  */
#define SET_STACK_LIMIT_HUGE

/* This machine is most significant byte first */
#define HOST_BYTE_ORDER BIG_ENDIAN

/* This OS has the wait structure */
#define HAVE_WAIT_STRUCT

#ifdef HOSTING_ONLY
/*
 * This next two defines are to get GDB up and running as a host to
 * do remote debugging.  I know there is a gdb for the RT, but there wasn't
 * an xconfig/rt* file.  
 */
#define KERNEL_U_ADDR_BSD  /* This may be correct, but hasn't been tested */
#define REGISTER_U_ADDR(a,b,c)	   \
	(printf("GDB can not debug IBM RT/PC BSD executables (yet)\n"),\
	quit(),0)
#else
# include	"GDB for the RT is not included in the distribution" 
#endif

