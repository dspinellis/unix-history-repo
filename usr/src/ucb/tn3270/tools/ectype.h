/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)ectype.h	3.2 (Berkeley) 3/28/88
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
