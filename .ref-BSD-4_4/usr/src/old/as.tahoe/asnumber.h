/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)asnumber.h 4.3 2/14/82
 */

union Ib_int{		/* byte */
	u_char	Ib_uchar[1];
	char	Ichar;
};
union Iw_int{		/* word */
	u_char	Iw_uchar[2];
	u_short	Iw_ushort[1];
	short	Iw_short;
};
union Il_int{		/* long word */
	u_char	Il_uchar[4];
	u_short	Il_ushort[2];
	u_int	Il_ulong[1];
	int	Il_long;
};

union Iq_int{		/* quad word */
	u_char	Iq_uchar[8];
	u_short	Iq_ushort[4];
	u_int	Iq_ulong[2];
};


union Ff_float{
	u_char	Ff_uchar[4];
	u_short	Ff_ushort[2];
	u_int	Ff_ulong[1];
	float	Ff_value;
};

union Fd_float{
	u_char	Fd_uchar[8];
	u_short	Fd_ushort[4];
	u_int	Fd_ulong[2];
	double	Fd_value;
};


struct	as_number{
	union {
		union Ib_int	numIb_int;
		union Iw_int	numIw_int;
		union Il_int	numIl_int;
		union Iq_int	numIq_int;
		union Ff_float	numFf_float;
		union Fd_float	numFd_float;
	}num_num;
	char	num_tag;		/* the key field: TYPB..TYPUNPACKED */
	char	num_sign;		/* the sign */
	short	num_exponent;		/* the unexcessed exp */
};
typedef	struct as_number	Bignum;

extern	Bignum	Znumber;		/* one all zero'ed out */

#define	num_uchar	num_num.numIq_int.Iq_uchar
#define	num_uint	num_num.numIq_int.Iq_ulong
#define	num_ulong	num_num.numIq_int.Iq_ulong
#define	num_ushort	num_num.numIq_int.Iq_ushort
/*
 *	The following definitions must all be consistent.
 *	They define the granularity of working on longs and quad 
 *	words. Currently, the granularity is as large as it can be: 32 bits
 *	in a chunk.
 */
#define	CH_N		2		/* number of pieces */
#define	CH_BITS		32		/* number of bits per piece */
#define	CH_FIELD(x)	((x).num_num.numIq_int.Iq_ulong)
typedef	u_int		*chptr;		/* basic data type */
#define SIGNBIT		0x80000000

#define	HOC		(CH_N - 1)	/* high order chunk */
#if 0
#define	MAXINT_1	((unsigned)(1<<(CH_BITS - 1)))
#define	MAXINT_10	((unsigned)((MAXINT_1/(unsigned)10)))
#define	MAXINT_5	((unsigned)((MAXINT_1/(unsigned)5)))
#else not 0
/*
 *	These values were computed using dc, so are exact.
 *	Only MAXINT_10 and MAXINT_5 are used in the programs.
 */
#define	MAXINT_1	2147483648
#define	MAXINT_10	214748364
#define	MAXINT_5	429496729
#endif not 0

Bignum as_atoi();		/* converts string to integer */
Bignum as_atof();		/* converts string to float */

/*
 *	Definitions for overflows.
 */
typedef	u_int	Ovf;

#define	OVF_ADDV	(1<<0)	/* integer: adding two vectors overflowed */
#define	OVF_LSHIFT	(1<<1)	/* integer: left shifting a vector lost bits */
#define	OVF_POSOVF	(1<<2)	/* integer: positive number overflowed */
#define	OVF_MAXINT	(1<<3)	/* integer: the number was the maxint + 1*/
#define	OVF_F		(1<<4)	/* float: F overflow */
#define	OVF_D		(1<<5)	/* float: D overflow */
#define	OVF_OVERFLOW	(1<<9)	/* overflow in conversion */
#define	OVF_UNDERFLOW	(1<<10)	/* underflow in conversion */

Ovf	posovf();
Ovf	numclear();
Ovf	numshift();
Ovf	numaddv();
Ovf	numaddd();
Ovf	num1comp();
Ovf	numnegate();
/*
 *	Bit manipulations
 */
#define	ONES(n)	((1 << (n)) - 1)
/*
 *	Assertions
 */
#if 1
#define	assert(x, str) if (!(x)) panic("%s%s\n", "x", str)
#else
#define assert(x, str)
#endif
