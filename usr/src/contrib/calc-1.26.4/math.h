/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Data structure declarations for extended precision arithmetic.
 * The assumption made is that a long is 32 bits and shorts are 16 bits,
 * and longs must be addressible on word boundaries.
 */

#include "alloc.h"

#include "have_stdlib.h"
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif


#ifndef	NULL
#define	NULL	0
#endif

/*#define ALLOCTEST 1*/

#ifndef ALLOCTEST
# if defined(CALC_MALLOC)
#  define freeh(p) ((p == _zeroval_) || (p == _oneval_) || free(p))
# else
#  define freeh(p) { if ((p != _zeroval_) && (p != _oneval_)) free((void *)p); }
# endif
#endif

typedef	short FLAG;			/* small value (e.g. comparison) */
typedef unsigned short BOOL;		/* TRUE or FALSE value */

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
typedef unsigned short HALF;		/* unit of number storage */
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
#define proto(a) a
#else
#define proto(a) ()
#endif

extern HALF * alloc proto((LEN len));
#ifdef	ALLOCTEST
extern void freeh proto((HALF *));
#endif


extern long iigcd proto((long i1, long i2));
extern void itoz proto((long i, ZVALUE * res));
extern long ztoi proto((ZVALUE z));
extern void zcopy proto((ZVALUE z, ZVALUE * res));
extern void zadd proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zsub proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zmul proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zsquare proto((ZVALUE z, ZVALUE * res));
extern void zreduce proto((ZVALUE z1, ZVALUE z2,
	ZVALUE * z1res, ZVALUE * z2res));
extern void zdiv proto((ZVALUE z1, ZVALUE z2,
	ZVALUE * res, ZVALUE * rem));
extern void zquo proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zmod proto((ZVALUE z1, ZVALUE z2, ZVALUE * rem));
extern BOOL zdivides proto((ZVALUE z1, ZVALUE z2));
extern void zor proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zand proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zxor proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zshift proto((ZVALUE z, long n, ZVALUE * res));
extern long zlowbit proto((ZVALUE z));
extern long zhighbit proto((ZVALUE z));
extern BOOL zisset proto((ZVALUE z, long n));
extern BOOL zisonebit proto((ZVALUE z));
extern BOOL zisallbits proto((ZVALUE z));
extern void zbitvalue proto((long n, ZVALUE * res));
extern FLAG ztest proto((ZVALUE z));
extern FLAG zrel proto((ZVALUE z1, ZVALUE z2));
extern BOOL zcmp proto((ZVALUE z1, ZVALUE z2));
extern void trim proto((ZVALUE * z));
extern void shiftr proto((ZVALUE z, long n));
extern void shiftl proto((ZVALUE z, long n));
extern void zfact proto((ZVALUE z, ZVALUE * dest));
extern void zpfact proto((ZVALUE z, ZVALUE * dest));
extern void zlcmfact proto((ZVALUE z, ZVALUE * dest));
extern void zperm proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zcomb proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern BOOL zprimetest proto((ZVALUE z, long count));
extern FLAG zjacobi proto((ZVALUE z1, ZVALUE z2));
extern void zfib proto((ZVALUE z, ZVALUE * res));
extern void zpowi proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void ztenpow proto((long power, ZVALUE * res));
extern void zpowermod proto((ZVALUE z1, ZVALUE z2,
	ZVALUE z3, ZVALUE * res));
extern BOOL zmodinv proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zgcd proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern void zlcm proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern BOOL zrelprime proto((ZVALUE z1, ZVALUE z2));
extern long zlog proto((ZVALUE z1, ZVALUE z2));
extern long zlog10 proto((ZVALUE z));
extern long zdivcount proto((ZVALUE z1, ZVALUE z2));
extern long zfacrem proto((ZVALUE z1, ZVALUE z2, ZVALUE * rem));
extern void zgcdrem proto((ZVALUE z1, ZVALUE z2, ZVALUE * res));
extern long zlowfactor proto((ZVALUE z, long count));
extern long zdigits proto((ZVALUE z1));
extern FLAG zdigit proto((ZVALUE z1, long n));
extern BOOL zsqrt proto((ZVALUE z1, ZVALUE * dest));
extern void zroot proto((ZVALUE z1, ZVALUE z2, ZVALUE * dest));
extern BOOL zissquare proto((ZVALUE z));
extern void zmuli proto((ZVALUE z, long n, ZVALUE *res));
extern long zmodi proto((ZVALUE z, long n));
extern long zdivi proto((ZVALUE z, long n, ZVALUE * res));
extern HALF *zalloctemp proto((LEN len));

#if 0
extern void zapprox proto((ZVALUE z1, ZVALUE z2, ZVALUE* res1, ZVALUE* res2));
#endif


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

#if 0
extern void zmulmod proto((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
extern void zsquaremod proto((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zsubmod proto((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
#endif
extern void zminmod proto((ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern BOOL zcmpmod proto((ZVALUE z1, ZVALUE z2, ZVALUE z3));
extern REDC *zredcalloc proto((ZVALUE z1));
extern void zredcfree proto((REDC *rp));
extern void zredcencode proto((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcdecode proto((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcmul proto((REDC *rp, ZVALUE z1, ZVALUE z2, ZVALUE *res));
extern void zredcsquare proto((REDC *rp, ZVALUE z1, ZVALUE *res));
extern void zredcpower proto((REDC *rp, ZVALUE z1, ZVALUE z2, ZVALUE *res));


/*
 * Rational arithmetic definitions.
 */
typedef struct {
	ZVALUE num, den;
	long links;
} NUMBER;

extern NUMBER *qadd(), *qsub(), *qmul(), *qdiv(), *qquo(), *qmod(), *qcomb();
extern NUMBER *qsquare(), *qgcd(), *qlcm(), *qmin(), *qmax(), *qand(), *qor();
extern NUMBER *qxor(), *qpowermod(), *qpowi(), *qpower(), *qneg(), *qsign();
extern NUMBER *qfact(), *qpfact(), *qsqrt(), *qshift(), *qminv();
extern NUMBER *qint(), *qfrac(), *qnum(), *qden(), *qinv(), *qabs(), *qroot();
extern NUMBER *qfacrem(), *qcopy(), *atoq(), *itoq(), *iitoq();
extern NUMBER *qperm(), *qgcdrem(), *qtrunc(), *qround(), *qalloc();
extern NUMBER *qlowfactor(), *qfib(), *qcfappr(), *qcos(), *qsin(), *qexp();
extern NUMBER *qscale(), *qln(), *qbtrunc(), *qbround(), *qisqrt();
extern NUMBER *qtan(), *qacos(), *qasin(), *qatan(), *qatan2(), *qjacobi();
extern NUMBER *qinc(), *qdec(), *qhypot(), *qcosh(), *qsinh(), *qtanh();
extern NUMBER *qacosh(), *qasinh(), *qatanh(), *qlegtoleg(), *qiroot();
extern NUMBER *qpi(), *qbappr(), *qdivi(), *qlcmfact(), *qminmod();
extern NUMBER *qredcin(), *qredcout(), *qredcmul(), *qredcsquare();
extern NUMBER *qredcpower();
extern BOOL qcmp(), qcmpi(), qprimetest(), qissquare();
extern BOOL qisset(), qcmpmod(), qquomod();
extern FLAG qrel(), qreli(), qnear(), qdigit();
extern long qtoi(), qprecision(), qplaces(), qdigits();
extern long qilog2(), qilog10(), qparse();
extern void qfreenum();
extern void qprintnum();
extern void setepsilon();

#if 0
extern NUMBER *qbitvalue(), *qmuli(), *qmulmod(), *qsquaremod();
extern NUMBER *qaddmod(), *qsubmod(), *qreadval(), *qnegmod();
extern BOOL qbittest();
extern FLAG qtest();
#endif

#ifdef CODE
extern NUMBER *qaddi();
#endif


/*
 * Complex arithmetic definitions.
 */
typedef struct {
	NUMBER *real;		/* real part of number */
	NUMBER *imag;		/* imaginary part of number */
	long links;		/* link count */
} COMPLEX;

extern COMPLEX *cadd(), *csub(), *cmul(), *cdiv(), *csquare();
extern COMPLEX *cneg(), *cinv();
extern COMPLEX *comalloc(), *caddq(), *csubq(), *cmulq(), *cdivq();
extern COMPLEX *cpowi(), *csqrt(), *cscale(), *cshift(), *cround();
extern COMPLEX *cbround(), *cint(), *cfrac(), *croot(), *cexp(), *cln();
extern COMPLEX *ccos(), *csin(), *cpolar(), *cpower(), *cmodq(), *cquoq();
extern void comfree(), comprint();
extern BOOL ccmp();
extern void cprintfr();

#if 0
extern COMPLEX *cconj(), *creal(), *cimag(), *qqtoc();
#endif


/*
 * macro expansions to speed this thing up
 */
#define iseven(z)	(!(*(z).v & 01))
#define isodd(z)	(*(z).v & 01)
#define iszero(z)	((*(z).v == 0) && ((z).len == 1))
#define isneg(z)	((z).sign)
#define ispos(z)	(((z).sign == 0) && (*(z).v || ((z).len > 1)))
#define isunit(z)	((*(z).v == 1) && ((z).len == 1))
#define isone(z)	((*(z).v == 1) && ((z).len == 1) && !(z).sign)
#define isnegone(z)	((*(z).v == 1) && ((z).len == 1) && (z).sign)
#define istwo(z)	((*(z).v == 2) && ((z).len == 1) && !(z).sign)
#define isleone(z)	((*(z).v <= 1) && ((z).len == 1))
#define istiny(z)	((z).len == 1)
#define issmall(z)	(((z).len < 2) || (((z).len == 2) && (((short)(z).v[1]) >= 0)))
#define isbig(z)	(((z).len > 2) || (((z).len == 2) && (((short)(z).v[1]) < 0)))
#define z1tol(z)	((long)((z).v[0]))
#define z2tol(z)	(((long)((z).v[0])) + \
				(((long)((z).v[1] & MAXHALF)) << BASEB))

#define qiszero(q)	(iszero((q)->num))
#define qisneg(q)	(isneg((q)->num))
#define qispos(q)	(ispos((q)->num))
#define qisint(q)	(isunit((q)->den))
#define qisfrac(q)	(!isunit((q)->den))
#define qisunit(q)	(isunit((q)->num) && isunit((q)->den))
#define qisone(q)	(isone((q)->num) && isunit((q)->den))
#define qisnegone(q)	(isnegone((q)->num) && isunit((q)->den))
#define qistwo(q)	(istwo((q)->num) && isunit((q)->den))
#define qiseven(q)	(isunit((q)->den) && iseven((q)->num))
#define qisodd(q)	(isunit((q)->den) && isodd((q)->num))
#define qistwopower(q)	(isunit((q)->den) && zistwopower((q)->num))
#define qhighbit(q)	(zhighbit((q)->num))
#define qlowbit(q)	(zlowbit((q)->num))
#define qdivides(q1, q2)	(zdivides((q1)->num, (q2)->num))
#define qdivcount(q1, q2)	(zdivcount((q1)->num, (q2)->num))
#define qilog(q1, q2)	(zlog((q1)->num, (q2)->num))
#define qlink(q)	((q)->links++, (q))

#define qfree(q)	{if (--((q)->links) <= 0) qfreenum(q);}
#define quicktrim(z)	{if (((z).len > 1) && ((z).v[(z).len-1] == 0)) (z).len--;}

#define cisreal(c)	(qiszero((c)->imag))
#define cisimag(c)	(qiszero((c)->real) && !cisreal(c))
#define ciszero(c)	(cisreal(c) && qiszero((c)->real))
#define cisone(c)	(cisreal(c) && qisone((c)->real))
#define cisnegone(c)	(cisreal(c) && qisnegone((c)->real))
#define cisrunit(c)	(cisreal(c) && qisunit((c)->real))
#define cisiunit(c)	(qiszero((c)->real) && qisunit((c)->imag))
#define cistwo(c)	(cisreal(c) && qistwo((c)->real))
#define cisint(c)	(qisint((c)->real) && qisint((c)->imag))
#define ciseven(c)	(qiseven((c)->real) && qiseven((c)->imag))
#define cisodd(c)	(qisodd((c)->real) || qisodd((c)->imag))
#define clink(c)	((c)->links++, (c))

#define	clearval(z)	memset((z).v, 0, (z).len * sizeof(HALF))
#define	copyval(z1, z2)	memcpy((z2).v, (z1).v, (z1).len * sizeof(HALF))


/*
 * Flags for qparse calls
 */
#define QPF_SLASH	0x1	/* allow slash for fractional number */
#define QPF_IMAG	0x2	/* allow trailing 'i' for imaginary number */


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
extern void math_chr(), math_str(), math_flush();
extern void divertio(), cleardiversions(), setfp();
extern char *getdivertedio();
extern void set_mode();		/* set output mode for numeric output */
extern void setdigits();	/* set # of digits for float or exp output */

#ifdef VARARGS
extern void math_fmt();
#else
# ifdef __STDC__
extern void math_fmt(char *, ...);
# else
extern void math_fmt();
# endif
#endif

/*
 * Print a formatted string containing arbitrary numbers, similar to printf.
 */
#ifdef VARARGS
extern void qprintf();
#else
# ifdef __STDC__
extern void qprintf(char *, ...);
# else
extern void qprintf();
# endif
#endif

/*
 * constants used often by the arithmetic routines
 */
extern HALF _zeroval_[], _oneval_[], _twoval_[], _tenval_[];
extern ZVALUE _zero_, _one_, _ten_;
extern NUMBER _qzero_, _qone_, _qnegone_, _qonehalf_;
extern COMPLEX _czero_, _cone_;

#if 0
extern NUMBER _conei_;
#endif

extern BOOL _sinisneg_;		/* whether sin(x) < 0 (set by cos(x)) */
extern BOOL _math_abort_;	/* nonzero to abort calculations */
extern long _epsilonprec_;	/* binary precision of epsilon */
extern NUMBER *_epsilon_;	/* default error for real functions */
extern ZVALUE _tenpowers_[32];	/* table of 10^2^n */
extern long _outdigits_;	/* current output digits for float or exp */
extern int _outmode_;		/* current output mode */
extern LEN _mul2_;		/* size of number to use multiply algorithm 2 */
extern LEN _sq2_;		/* size of number to use square algorithm 2 */
extern LEN _pow2_;		/* size of modulus to use REDC for powers */
extern LEN _redc2_;		/* size of modulus to use REDC algorithm 2 */
extern HALF *bitmask;		/* bit rotation, norm 0 */

#if 0
extern char *_mallocptr_;	/* pointer for malloc calls */
#endif

/*
 * misc function declarations - most to keep lint happy
 */
extern void initmasks();	/* init the bitmask rotation arrays */

#ifdef VARARGS
void error();
#else
# ifdef __STDC__
void error(char *, ...);
# else
void error();
# endif
#endif


/* END CODE */
