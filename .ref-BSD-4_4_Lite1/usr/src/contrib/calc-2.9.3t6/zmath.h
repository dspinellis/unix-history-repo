/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Data structure declarations for extended precision integer arithmetic.
 * The assumption made is that a long is 32 bits and shorts are 16 bits,
 * and longs must be addressible on word boundaries.
 */

#ifndef	ZMATH_H
#define	ZMATH_H

#include <stdio.h>
#include "alloc.h"
#include "endian.h"
#include "longbits.h"

#include "have_stdlib.h"
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif


#ifndef ALLOCTEST
# if defined(CALC_MALLOC)
#  define freeh(p) (((void *)p == (void *)_zeroval_) ||			\
		    ((void *)p == (void *)_oneval_) || free((void *)p))
# else
#  define freeh(p) { if (((void *)p != (void *)_zeroval_) &&		\
			 ((void *)p != (void *)_oneval_)) free((void *)p); }
# endif
#endif


typedef	int FLAG;			/* small value (e.g. comparison) */
typedef int BOOL;			/* TRUE or FALSE value */
typedef unsigned long HASH;		/* hash value */


#if !defined(TRUE)
#define	TRUE	((BOOL) 1)			/* booleans */
#endif
#if !defined(FALSE)
#define	FALSE	((BOOL) 0)
#endif


/*
 * NOTE: FULL must be twice the storage size of a HALF
 *	 LEN storage size must be <= FULL storage size
 */

#if LONG_BITS == 64			/* for 64-bit machines */
typedef unsigned int HALF;		/* unit of number storage */
typedef int SHALF;			/* signed HALF */
typedef unsigned long FULL;		/* double unit of number storage */
typedef long LEN;			/* unit of length storage */

#define BASE	((FULL) 4294967296)	/* base for calculations (2^32) */
#define BASE1	((FULL) (BASE - 1))	/* one less than base */
#define BASEB	32			/* number of bits in base */
#define	BASEDIG	10			/* number of digits in base */
#define	MAXHALF	((FULL) 0x7fffffff)	/* largest positive half value */
#define	MAXFULL	((FULL) 0x7fffffffffffffff) /* largest positive full value */
#define	TOPHALF	((FULL) 0x80000000)	/* highest bit in half value */
#define	TOPFULL	((FULL) 0x8000000000000000)	/* highest bit in full value */
#define MAXLEN	((LEN)	0x7fffffffffffffff)	/* longest value allowed */

#else					/* for 32-bit machines */
typedef unsigned short HALF;		/* unit of number storage */
typedef short SHALF;			/* signed HALF */
typedef unsigned long FULL;		/* double unit of number storage */
typedef long LEN;			/* unit of length storage */

#define BASE	((FULL) 65536)		/* base for calculations (2^16) */
#define BASE1	((FULL) (BASE - 1))	/* one less than base */
#define BASEB	16			/* number of bits in base */
#define	BASEDIG	5			/* number of digits in base */
#define	MAXHALF	((FULL) 0x7fff)		/* largest positive half value */
#define	MAXFULL	((FULL) 0x7fffffff)	/* largest positive full value */
#define	TOPHALF	((FULL) 0x8000)		/* highest bit in half value */
#define	TOPFULL	((FULL) 0x80000000)	/* highest bit in full value */
#define MAXLEN	((LEN)	0x7fffffff)	/* longest value allowed */
#endif

#define	MAXREDC	5			/* number of entries in REDC cache */
#define	SQ_ALG2	20			/* size for alternative squaring */
#define	MUL_ALG2 20			/* size for alternative multiply */
#define	POW_ALG2 40			/* size for using REDC for powers */
#define	REDC_ALG2 50			/* size for using alternative REDC */


typedef union {
	FULL	ivalue;
	struct {
		HALF Svalue1;
		HALF Svalue2;
	} sis;
} SIUNION;


#if !defined(BYTE_ORDER)
#include <machine/endian.h>
#endif

#if !defined(LITTLE_ENDIAN)
#define LITTLE_ENDIAN	1234	/* Least Significant Byte first */
#endif
#if !defined(BIG_ENDIAN)
#define BIG_ENDIAN	4321	/* Most Significant Byte first */
#endif
/* PDP_ENDIAN - LSB in word, MSW in long is not supported */

#if BYTE_ORDER == LITTLE_ENDIAN
# define silow	sis.Svalue1	/* low order half of full value */
# define sihigh	sis.Svalue2	/* high order half of full value */
#else
# if BYTE_ORDER == BIG_ENDIAN
#  define silow	sis.Svalue2	/* low order half of full value */
#  define sihigh sis.Svalue1	/* high order half of full value */
# else
   :@</*/>@:    BYTE_ORDER must be BIG_ENDIAN or LITTLE_ENDIAN    :@</*/>@:
# endif
#endif


typedef struct {
	HALF	*v;		/* pointer to array of values */
	LEN	len;		/* number of values in array */
	BOOL	sign;		/* sign, nonzero is negative */
} ZVALUE;



/*
 * Function prototypes for integer math routines.
 */
#if defined(__STDC__)
#define MATH_PROTO(a) a
#else
#define MATH_PROTO(a) ()
#endif

extern HALF * alloc MATH_PROTO((LEN len));
#ifdef	ALLOCTEST
extern void freeh MATH_PROTO((HALF *));
#endif


/*
 * Input, output, and conversion routines.
 */
extern void zcopy MATH_PROTO((ZVALUE z, ZVALUE *res));
extern void itoz MATH_PROTO((long i, ZVALUE *res));
extern void atoz MATH_PROTO((char *s, ZVALUE *res));
extern long ztoi MATH_PROTO((ZVALUE z));
extern void zprintval MATH_PROTO((ZVALUE z, long decimals, long width));
extern void zprintx MATH_PROTO((ZVALUE z, long width));
extern void zprintb MATH_PROTO((ZVALUE z, long width));
extern void zprinto MATH_PROTO((ZVALUE z, long width));


/*
 * Basic numeric routines.
 */
extern void zmuli MATH_PROTO((ZVALUE z, long n, ZVALUE *res));
extern long zdivi MATH_PROTO((ZVALUE z, long n, ZVALUE *res));
extern long zmodi MATH_PROTO((ZVALUE z, long n));
extern void zadd MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zsub MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zmul MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zdiv MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res, ZVALUE *rem));
extern void zquo MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *rem));
extern BOOL zdivides MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern void zor MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zand MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zxor MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zshift MATH_PROTO((ZVALUE z, long n, ZVALUE *res));
extern void zsquare MATH_PROTO((ZVALUE z, ZVALUE *res));
extern long zlowbit MATH_PROTO((ZVALUE z));
extern long zhighbit MATH_PROTO((ZVALUE z));
extern void zbitvalue MATH_PROTO((long n, ZVALUE *res));
extern BOOL zisset MATH_PROTO((ZVALUE z, long n));
extern BOOL zisonebit MATH_PROTO((ZVALUE z));
extern BOOL zisallbits MATH_PROTO((ZVALUE z));
extern FLAG ztest MATH_PROTO((ZVALUE z));
extern FLAG zrel MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern BOOL zcmp MATH_PROTO((ZVALUE z1, ZVALUE z2));


/*
 * More complicated numeric functions.
 */
extern long iigcd MATH_PROTO((long i1, long i2));
extern void zgcd MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zlcm MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zreduce MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *z1res, ZVALUE *z2res));
extern void zfact MATH_PROTO((ZVALUE z, ZVALUE *dest));
extern void zpfact MATH_PROTO((ZVALUE z, ZVALUE *dest));
extern void zlcmfact MATH_PROTO((ZVALUE z, ZVALUE *dest));
extern void zperm MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zcomb MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern BOOL zprimetest MATH_PROTO((ZVALUE z, long count));
extern FLAG zjacobi MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern void zfib MATH_PROTO((ZVALUE z, ZVALUE *res));
extern void zpowi MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void ztenpow MATH_PROTO((long power, ZVALUE *res));
extern void zpowermod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
extern BOOL zmodinv MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern BOOL zrelprime MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern long zlog MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern long zlog10 MATH_PROTO((ZVALUE z));
extern long zdivcount MATH_PROTO((ZVALUE z1, ZVALUE z2));
extern long zfacrem MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *rem));
extern void zgcdrem MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern long zlowfactor MATH_PROTO((ZVALUE z, long count));
extern long zdigits MATH_PROTO((ZVALUE z1));
extern FLAG zdigit MATH_PROTO((ZVALUE z1, long n));
extern BOOL zsqrt MATH_PROTO((ZVALUE z1, ZVALUE *dest));
extern void zroot MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *dest));
extern BOOL zissquare MATH_PROTO((ZVALUE z));
extern HASH zhash MATH_PROTO((ZVALUE z));

#if 0
extern void zapprox MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res1, ZVALUE *res2));
#endif


#if 0
extern void zmulmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
extern void zsquaremod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zsubmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
#endif
extern void zminmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern BOOL zcmpmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE z3));


/*
 * These functions are for internal use only.
 */
extern void ztrim MATH_PROTO((ZVALUE *z));
extern void zshiftr MATH_PROTO((ZVALUE z, long n));
extern void zshiftl MATH_PROTO((ZVALUE z, long n));
extern HALF *zalloctemp MATH_PROTO((LEN len));
extern void initmasks MATH_PROTO((void));


/*
 * Modulo arithmetic definitions.
 * Structure holding state of REDC initialization.
 * Multiple instances of this structure can be used allowing
 * calculations with more than one modulus at the same time.
 * Len of zero means the structure is not initialized.
 */
typedef	struct {
	LEN len;		/* number of words in binary modulus */
	ZVALUE mod;		/* modulus REDC is computing with */
	ZVALUE inv;		/* inverse of modulus in binary modulus */
	ZVALUE one;		/* REDC format for the number 1 */
} REDC;

extern REDC *zredcalloc MATH_PROTO((ZVALUE z1));
extern void zredcfree MATH_PROTO((REDC *rp));
extern void zredcencode MATH_PROTO((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcdecode MATH_PROTO((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcmul MATH_PROTO((REDC *rp, ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zredcsquare MATH_PROTO((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcpower MATH_PROTO((REDC *rp, ZVALUE z1, ZVALUE z2, ZVALUE *res));


/*
 * macro expansions to speed this thing up
 */
#define ziseven(z)	(!(*(z).v & 01))
#define zisodd(z)	(*(z).v & 01)
#define ziszero(z)	((*(z).v == 0) && ((z).len == 1))
#define zisneg(z)	((z).sign)
#define zispos(z)	(((z).sign == 0) && (*(z).v || ((z).len > 1)))
#define zisunit(z)	((*(z).v == 1) && ((z).len == 1))
#define zisone(z)	((*(z).v == 1) && ((z).len == 1) && !(z).sign)
#define zisnegone(z)	((*(z).v == 1) && ((z).len == 1) && (z).sign)
#define zistwo(z)	((*(z).v == 2) && ((z).len == 1) && !(z).sign)
#define zisleone(z)	((*(z).v <= 1) && ((z).len == 1))
#define zistiny(z)	((z).len == 1)
#define zissmall(z)	(((z).len < 2) || (((z).len == 2) && (((SHALF)(z).v[1]) >= 0)))
#define zisbig(z)	(((z).len > 2) || (((z).len == 2) && (((SHALF)(z).v[1]) < 0)))

#define z1tol(z)	((long)((z).v[0]))
#define z2tol(z)	(((long)((z).v[0])) + \
				(((long)((z).v[1] & MAXHALF)) << BASEB))
#define	zclearval(z)	memset((z).v, 0, (z).len * sizeof(HALF))
#define	zcopyval(z1,z2)	memcpy((z2).v, (z1).v, (z1).len * sizeof(HALF))
#define zquicktrim(z)	{if (((z).len > 1) && ((z).v[(z).len-1] == 0)) \
				(z).len--;}
#define	zfree(z)	freeh((z).v)


/*
 * Output modes for numeric displays.
 */
#define MODE_DEFAULT	0
#define MODE_FRAC	1
#define MODE_INT	2
#define MODE_REAL	3
#define MODE_EXP	4
#define MODE_HEX	5
#define MODE_OCTAL	6
#define MODE_BINARY	7
#define MODE_MAX	7

#define MODE_INITIAL	MODE_REAL


/*
 * Output routines for either FILE handles or strings.
 */
extern void math_chr MATH_PROTO((int ch));
extern void math_str MATH_PROTO((char *str));
extern void math_fill MATH_PROTO((char *str, long width));
extern void math_flush MATH_PROTO((void));
extern void math_divertio MATH_PROTO((void));
extern void math_cleardiversions MATH_PROTO((void));
extern void math_setfp MATH_PROTO((FILE *fp));
extern char *math_getdivertedio MATH_PROTO((void));
extern int math_setmode MATH_PROTO((int mode));
extern long math_setdigits MATH_PROTO((long digits));


#ifdef VARARGS
extern void math_fmt();
#else
extern void math_fmt MATH_PROTO((char *, ...));
#endif


/*
 * The error routine.
 */
#ifdef VARARGS
extern void math_error();
#else
extern void math_error MATH_PROTO((char *, ...));
#endif


/*
 * constants used often by the arithmetic routines
 */
extern HALF _zeroval_[], _oneval_[], _twoval_[], _tenval_[];
extern ZVALUE _zero_, _one_, _ten_;

extern BOOL _math_abort_;	/* nonzero to abort calculations */
extern ZVALUE _tenpowers_[2 * BASEB];	/* table of 10^2^n */
extern int _outmode_;		/* current output mode */
extern LEN _mul2_;		/* size of number to use multiply algorithm 2 */
extern LEN _sq2_;		/* size of number to use square algorithm 2 */
extern LEN _pow2_;		/* size of modulus to use REDC for powers */
extern LEN _redc2_;		/* size of modulus to use REDC algorithm 2 */
extern HALF *bitmask;		/* bit rotation, norm 0 */

#endif

/* END CODE */
