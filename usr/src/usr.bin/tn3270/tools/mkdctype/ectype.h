/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ectype.h	4.2 (Berkeley) %G%
 */

#define	INCLUDED_ECTYPE

#define	E_UPPER	0x01
#define	E_LOWER	0x02
#define	E_DIGIT	0x04
#define	E_SPACE	0x08
#define	E_PUNCT	0x10
#define	E_PRINT 0x20

#define	Eisalpha(c)	(ectype[(c)&0xff]&(E_UPPER|E_LOWER))
#define	Eisupper(c)	(ectype[(c)&0xff]&E_UPPER)
#define	Eislower(c)	(ectype[(c)&0xff]&E_LOWER)
#define	Eisdigit(c)	(ectype[(c)&0xff]&E_DIGIT)
#define	Eisalnum(c)	(ectype[(c)&0xff]&(E_UPPER|E_LOWER|E_DIGIT))
#define	Eisspace(c)	(ectype[(c)&0xff]&E_SPACE)	/* blank or null */
#define	Eispunct(c)	(ectype[(c)&0xff]&E_PUNCT)
#define	Eisprint(c)	(ectype[(c)&0xff]&E_PRINT)
