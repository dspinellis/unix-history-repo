/*	manifest.h	4.1	85/03/19	*/

#ifndef _MANIFEST_
#define	_MANIFEST_

#include <stdio.h>
#include "pcclocal.h"
#include "config.h"

#define DSIZE	(MAXOP+1)	/* DSIZE is the size of the dope array */

#define NOLAB	(-1)		/* no label with constant */

/*
 * Node types
 */
#define LTYPE	02		/* leaf */
#define UTYPE	04		/* unary */
#define BITYPE	010		/* binary */

/*
 * Bogus type values
 */
#define TNULL	PTR		/* pointer to UNDEF */
#define TVOID	FTN		/* function returning UNDEF (for void) */

/*
 * Type packing constants
 */
#define TMASK	060		/* mask for 1st component of compound type */
#define TMASK1	0300		/* mask for 2nd component of compound type */
#define TMASK2	0360		/* mask for 3rd component of compound type */
#define BTMASK	017		/* basic type mask */
#define BTSHIFT	4		/* basic type shift */
#define TSHIFT	2		/* shift count to get next type component */

/*
 * Type manipulation macros
 */
#define MODTYPE(x,y)	x = ((x)&(~BTMASK))|(y)	/* set basic type of x to y */
#define BTYPE(x)	((x)&BTMASK)		/* basic type of x */
#define ISUNSIGNED(x)	((x)<=ULONG&&(x)>=UCHAR)
#define UNSIGNABLE(x)	((x)<=LONG&&(x)>=CHAR)
#define ENUNSIGN(x)	((x)+(UNSIGNED-INT))
#define DEUNSIGN(x)	((x)+(INT-UNSIGNED))
#define ISPTR(x)	(((x)&TMASK)==PTR)
#define ISFTN(x)	(((x)&TMASK)==FTN)	/* is x a function type */
#define ISARY(x)	(((x)&TMASK)==ARY)	/* is x an array type */
#define INCREF(x)	((((x)&~BTMASK)<<TSHIFT)|PTR|((x)&BTMASK))
#define DECREF(x)	((((x)>>TSHIFT)&~BTMASK)|( (x)&BTMASK))
/* advance x to a multiple of y */
#define SETOFF(x,y)	if ((x)%(y) != 0) (x) = (((x)/(y) + 1) * (y))
/* can y bits be added to x without overflowing z */
#define NOFIT(x,y,z)	(((x)%(z) + (y)) > (z))

/*
 * Pack and unpack field descriptors (size and offset)
 */
#define PKFIELD(s,o)	(((o)<<6)| (s))
#define UPKFSZ(v)	((v) &077)
#define UPKFOFF(v)	((v)>>6)

/*
 * Operator information
 */
#define TYFLG	016
#define ASGFLG	01
#define LOGFLG	020

#define SIMPFLG	040
#define COMMFLG	0100
#define DIVFLG	0200
#define FLOFLG	0400
#define LTYFLG	01000
#define CALLFLG	02000
#define MULFLG	04000
#define SHFFLG	010000
#define ASGOPFLG 020000

#define SPFLG	040000

#define optype(o)	(dope[o]&TYFLG)
#define asgop(o)	(dope[o]&ASGFLG)
#define logop(o)	(dope[o]&LOGFLG)
#define callop(o)	(dope[o]&CALLFLG)

/*
 * External declarations, typedefs and the like
 */
#ifdef FLEXNAMES
char	*hash();
char	*savestr();
char	*tstr();
extern	int tstrused;
extern	char *tstrbuf[];
extern	char **curtstr;
#define	freetstr()	curtstr = tstrbuf, tstrused = 0
#endif

extern	int nerrors;		/* number of errors seen so far */
extern	int dope[];		/* a vector containing operator information */
extern	char *opst[];		/* a vector containing names for ops */

typedef	union ndu NODE;
typedef	unsigned int TWORD;
#define NIL	(NODE *)0

#ifndef ONEPASS
#ifndef EXPR
#define EXPR '.'
#endif
#ifndef BBEG
#define BBEG '['
#endif
#ifndef BEND
#define BEND ']'
#endif
#else
#include "onepass.h"
#endif
#endif
