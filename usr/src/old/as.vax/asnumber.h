/*
 *	Copyright (c) 1982 Regents of the University of California
 *	@(#)asnumber.h 4.1 %G%
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

union Io_int{		/* octal word */
	u_char	Io_uchar[16];
	u_short	Io_ushort[8];
	u_int	Io_ulong[4];
	union	Iq_int	Io_quad[2];
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

union Fg_float{
	u_char	Fg_uchar[8];
	u_short	Fg_ushort[4];
	u_int	Fg_ulong[2];
};

union Fh_float{
	u_char	Fh_uchar[16];
	u_short	Fh_ushort[8];
	u_int	Fh_ulong[4];
};

struct	as_number{
	union {
		union Ib_int	numIb_int;
		union Iw_int	numIw_int;
		union Il_int	numIl_int;
		union Iq_int	numIq_int;
		union Io_int	numIo_int;
		union Ff_float	numFf_float;
		union Fd_float	numFd_float;
		union Fg_float	numFg_float;
		union Fh_float	numFh_float;
	}num_num;
	char	num_tag;		/* the key field: TYPB..TYPUNPACKED */
	char	num_sign;		/* when unpacked, the sign */
	short	num_exponent;		/* when unpacked, the unexcessed exp */
};
typedef	struct as_number	Bignum;

extern	Bignum	Znumber;		/* one all zero'ed out */

#define	num_uchar	num_num.numIq_int.Iq_uchar
#define	num_uint	num_num.numIq_int.Iq_ulong
#define	num_ulong	num_num.numIq_int.Iq_ulong
#define	num_ushort	num_num.numIq_int.Iq_ushort
/*
 *	The following definitions must all be consistent.
 *	They define the granularity of working on longs, quad and octal
 *	words. Currently, the granularity is as large as it can be: 32 bits
 *	in a chunk.
 */
#define	CH_N		4		/* number of pieces */
#define	CH_BITS		32		/* number of bits per piece */
#define	CH_FIELD(x)	((x).num_num.numIo_int.Io_ulong)
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
Bignum bigatof();		/* converts string to float */
Bignum floatconvert();	/* converts amongst float #s */
Bignum intconvert();		/* converts amongst float #s */
Bignum bignumconvert();	/* converts amongst float #s */
Bignum bignumpack();		/* converts UNPACKED bignum to bignum */
Bignum bignumunpack();	/* converts bignum to UNPACKED bignum */

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
#define	OVF_G		(1<<6)	/* float: G overflow */
#define	OVF_H		(1<<7)	/* float: H overflow */
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
 *	Definitions to unpack big numbers numbers into
 *	a 128 bit fraction and 16 bit excess-free exponent,
 *	and an 8 copy bits for the sign.
 *
 *	The fraction is represented as a normalized binary number,
 *	128 bits long, with the binary point between bits 127 and the
 *	hypothetical 128'th bit.  This hypothetical 128'th bit
 *	is always assumed to be one.
 */
/*
 *	A map entry is NOTAKE if the corresponding byte is
 *	not to be taken
 *
 *	The maps are for going from packed to unpacked format (b_up)
 *	and from unpacked to packed format (b_p)
 *	for the mantissa (b_upmmap) and for the exponent(b_upemap)
 *
 *	byte #i in the packed number goes to byte #b_upmmap[i] in the unpacked
 */
#define	NOTAKE	-1
struct ty_bigdesc{
	char	b_upmmap[16];	/* byte x of float goes to up_mmap[x] in mant */
	char	b_pmmap[16];	/* inverse of upmmap */
	char	b_upemap[2];	/* byte x of float goes to up_emap[x] in exp */
	char	b_pemap[2];	/* inverse of upemap */
	char	b_mlshift;	/* left shift quantity to justify to left */
	char	b_ershift;	/* right shift quantity to r justify exponent */
	short	b_msigbits;	/* # sig bits in mantissa */
	char	b_esigbits;	/* # sig bits in exponent */
	short	b_eexcess;	/* exponent excess */
};
extern struct ty_bigdesc ty_bigdesc[];
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
