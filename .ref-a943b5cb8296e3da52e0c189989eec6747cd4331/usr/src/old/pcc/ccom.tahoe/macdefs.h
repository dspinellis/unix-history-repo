/*	macdefs.h	1.3	86/02/04	*/

#ifndef _MACDEFS_
#define _MACDEFS_

#define makecc(val,i)	lastcon = (lastcon<<8)|((val<<24)>>24);		

#define ARGINIT		SZINT
#define AUTOINIT	(13*SZINT)

/*
 * Storage space requirements.
 */
#define SZCHAR		8
#define SZINT		32
#define SZFLOAT		32
#define SZDOUBLE	64
#define SZLONG		32
#define SZSHORT		16
#define SZPOINT		32

/*
 * Alignment constraints
 */
#define ALCHAR		8
#define ALINT		32
#define ALFLOAT		32
#define ALDOUBLE	32
#define ALLONG		32
#define ALSHORT		16
#define ALPOINT		32
#define ALSTRUCT	8
#define ALSTACK		32	

#define ACONFMT	"$0x%lx"	/* format for printing address constants */
#define CONFMT	"%ld"		/* format for printing constants */
#define LABFMT	"L%d"		/* format for printing labels */

typedef	long	CONSZ;		/* size in which constants are converted */
typedef	long	OFFSZ;		/* size in which offsets are kept */

#define CCTRANS(x) x		/* character set macro */

/*
 * Register cookie for stack pointer.
 */
#define STKREG	13		/* stack pointer */

/*
 * Maximum and minimum register variables
 */
#define MINRVAR	6		/* use R6 thru ... */
#define MAXRVAR	12		/* ... R12 */

#define BACKAUTO		/* stack grows negatively for automatics */
#define BACKTEMP		/* stack grows negatively for temporaries */
/*#define FIELDOPS	/* no hardware field support */
/*#define RTOLBYTES	/* bytes are number from left to right */
#define ENUMSIZE(high,low) INT	/* enums are always stored in full int */

#define ADDROREG
#define FIXDEF(p)	outstab(p)
#define FIXARG(p)	fixarg(p)
#ifndef ncopy
#define	ncopy(q, p)	((q)->in = (p)->in)
#endif

#define	PRTDCON			/* use machine-specific prtdcon routine */
extern	prtdcon();
#endif
