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
 *	@(#)ascebc.h	4.1 (Berkeley) %G%
 */

#define	INCLUDED_ASCEBC

/*
 * ascii/ebcdic translation information
 */

#define	NASCII	128		/* number of ascii characters */
#define	NASCEBC	  4		/* number of ascii to ebcdic tables */

#define AE_NO	 -1		/* no translation - user has already done it */
#define	AE_PR	  0		/* ascii to ebcdic "print" translation */
#define	AE_IN	  1		/* ascii to ebcdic "input" translation */
#define	AE_SP	  2		/* ascii to ebcdic special translation */
#define AE_TX	  3		/* ascii to ebcdic pure text translation */

#define	NEBC	256		/* number of ebcdic characters */
#define	NEBCASC	  1		/* number of ebcdic to ascii tables */

extern unsigned char
	ascebc[NASCEBC][NASCII],
	ebcasc[NEBCASC][NEBC];
