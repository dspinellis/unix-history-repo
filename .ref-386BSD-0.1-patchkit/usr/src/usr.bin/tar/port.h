/* Portability declarations.
   Copyright (C) 1988 Free Software Foundation

This file is part of GNU Tar.

GNU Tar is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Tar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Tar; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/*
 * Portability declarations for tar.
 *
 * @(#)port.h 1.3	87/11/11	by John Gilmore, 1986
 */

/*
 * Everybody does wait() differently.  There seem to be no definitions
 * for this in V7 (e.g. you are supposed to shift and mask things out
 * using constant shifts and masks.)  So fuck 'em all -- my own non
 * standard but portable macros.  Don't change to a "union wait"
 * based approach -- the ordering of the elements of the struct 
 * depends on the byte-sex of the machine.  Foo!
 */
#define	TERM_SIGNAL(status)	((status) & 0x7F)
#define TERM_COREDUMP(status)	(((status) & 0x80) != 0)
#define TERM_VALUE(status)	((status) >> 8)

#ifdef	MSDOS
/* missing things from sys/stat.h */
#define	S_ISUID		0
#define	S_ISGID		0
#define	S_ISVTX		0

/* device stuff */
#define	makedev(ma, mi)		((ma << 8) | mi)
#define	major(dev)		(dev)
#define	minor(dev)		(dev)
#endif	/* MSDOS */
