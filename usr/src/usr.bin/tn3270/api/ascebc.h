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
 *	@(#)ascebc.h	1.2 (Berkeley) %G%
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
