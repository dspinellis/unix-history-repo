/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
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
