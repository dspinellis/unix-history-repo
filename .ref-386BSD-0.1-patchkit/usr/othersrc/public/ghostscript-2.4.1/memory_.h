/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* memory_.h */
/* Generic substitute for Unix memory.h */

/****** Note: the System V bcmp routine only returns zero or non-zero, ******/
/****** unlike memcmp which returns -1, 0, or 1. ******/

#ifdef __TURBOC__
/* The Turbo C implementation of memset swaps the arguments and calls */
/* the non-standard routine setmem.  We may as well do it in advance. */
#  undef memset				/* just in case */
#  include <mem.h>
#  define memset(dest,chr,cnt) setmem(dest,cnt,chr)
#else
#  ifdef VMS
	extern char *memcpy(), *memset();
	extern int memcmp();
#  else
#    if defined(BSD4_2) || defined(UTEK)
	extern bcopy(), bcmp(), bzero();
#       define memcpy(dest,src,len) bcopy(src,dest,len)
#       define memcmp(b1,b2,len) bcmp(b1,b2,len)
	/* Define our own version of memset */
#	if defined(__STDC__) && !defined(UTEK)
	static void memset(void *dest, register char ch, unsigned len)
#	else
	static void memset(dest, ch, len)
	  char *dest; register char ch; unsigned len;
#	endif				/* (!)__STDC__, ... */
	   {	if ( ch == 0 )
			bzero(dest, len);
		else if ( len > 0 )
		   {	register char *p = (char *)dest;
			register unsigned count = len;
			do { *p++ = ch; } while ( --count );
		   }
	   }
#    else				/* !BSD4_2 */
#      if defined(_POSIX_SOURCE) || defined(_HPUX_SOURCE) || defined(__WATCOMC__)
#        include <string.h>
#      else
#        include <memory.h>
#      endif				/* !_POSIX_SOURCE, ... */
#    endif				/* !BSD4_2, ... */
#  endif				/* !VMS */
#endif					/* !__MSDOS__ */

/* memswab is not a standard routine.  It swaps bytes in 16-bit words. */
/* We thought we would come out ahead by using the Watcom C `swab' */
/* library routine, but it's slower than the C routine in gsmisc.c! */
/* Note that the arguments are declared char *, */
/* but the implementation may assume they are int-aligned. */
#ifdef __PROTOTYPES__
extern void memswab(const char *src, char *dest, int count);
#else
extern void memswab();
#endif

/* memflip8x8 transposes an 8 x 8 block of bits. */
/* line_size is the raster of the input data; */
/* dist is the distance between output bytes. */
/* Dot matrix printers need this.  The C code is in gsmisc.c. */
#ifdef __PROTOTYPES__
extern void memflip8x8(const byte *inp, int line_size, byte *outp, int dist);
#else
extern void memflip8x8();
#endif
