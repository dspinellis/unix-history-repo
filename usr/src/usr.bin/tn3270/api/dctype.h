/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dctype.h	3.3 (Berkeley) %G%
 */

#define	INCLUDED_ECTYPE

#define	D_UPPER	0x01
#define	D_LOWER	0x02
#define	D_DIGIT	0x04
#define	D_SPACE	0x08
#define	D_PUNCT	0x10
#define	D_PRINT 0x20

#define	Disalpha(c)	(dctype[(c)]&(D_UPPER|D_LOWER))
#define	Disupper(c)	(dctype[(c)]&D_UPPER)
#define	Dislower(c)	(dctype[(c)]&D_LOWER)
#define	Disdigit(c)	(dctype[(c)]&D_DIGIT)
#define	Disalnum(c)	(dctype[(c)]&(D_UPPER|D_LOWER|D_DIGIT))
#define	Disspace(c)	(dctype[(c)]&D_SPACE)	/* blank or null */
#define	Dispunct(c)	(dctype[(c)]&D_PUNCT)
#define	Disprint(c)	(dctype[(c)]&D_PRINT)

extern unsigned char dctype[192];
