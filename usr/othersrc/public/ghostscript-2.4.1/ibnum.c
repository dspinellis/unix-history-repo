/* Copyright (C) 1990, 1991 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* ibnum.c */
/* Level 2 encoded number reading utilities for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "stream.h"
#include "bnum.h"
#include "btoken.h"

/* ------ Encoded number reading ------ */

/* Set up to read from an encoded number array/string. */
int
sread_num_array(stream *s, ref *op)
{	switch ( r_type(op) )
	   {
	case t_string:
	   {	/* Check that this is a legitimate encoded number array */
		byte *bp = op->value.bytes;
		short count;
		int nshift;
		if ( r_size(op) < 4 || (bt_type)bp[0] != bt_num_array ||
		    !num_is_valid(bp[1])
		    )
			return e_typecheck;
		sread_string(s, bp + 2, r_size(op) - 2);
		s->num_format = bp[1];
		sgetshort(s, &count);
		nshift = ((bp[1] & 0x70) == 0x20 ? 1 : 2);
		if ( count != (r_size(op) - 4) >> nshift )
			return e_typecheck;
	   }	break;
	case t_array:
		sread_string(s, (byte *)op->value.refs, r_size(op) * sizeof(ref));
		s->num_format = num_array;
		break;
	default:
		return e_typecheck;
	   }
	return 0;
}

/* Get the number of elements in an encoded number stream. */
uint
scount_num_stream(stream *s)
{	long avlong;
	savailable(s, &avlong);
	switch ( s->num_format & 0x170 )
	   {
	case num_int16:
		return (uint)(avlong >> 1);
	case num_array:
		return (uint)(avlong / sizeof(ref));
	default:			/* num_int32, num_float */
		return (uint)(avlong >> 2);
	   }
}

/* Read an encoded number from a stream according to its num_format. */
/* Put the value in np->value.{intval,realval}.  Return t_int, t_real, */
/* t_null if end of stream, or e_syntaxerror or e_typecheck. */
static double binary_scale[32] = {
#define expn2(n) (0.5 / (1L << (n-1)))
	1.0, expn2(1), expn2(2), expn2(3),
	expn2(4), expn2(5), expn2(6), expn2(7),
	expn2(8), expn2(9), expn2(10), expn2(11),
	expn2(12), expn2(13), expn2(14), expn2(15),
	expn2(16), expn2(17), expn2(18), expn2(19),
	expn2(20), expn2(21), expn2(22), expn2(23),
	expn2(24), expn2(25), expn2(26), expn2(27),
	expn2(28), expn2(29), expn2(30), expn2(31)
#undef expn2
};
int
sget_encoded_number(stream *s, ref *np)
{	int format = s->num_format;
	short snum;
	long lnum;
	int code, type;
	switch ( format & 0x170 )
	   {
	case num_int32: case num_int32 + 16:
		if ( (format & 31) == 0 )
			type = t_integer,
			code = sgetlong(s, &np->value.intval);
		else
		   {	type = t_real;
			code = sgetlong(s, &lnum);
			if ( !code )
				np->value.realval =
				  (double)lnum * binary_scale[format & 31];
		   }
		break;
	case num_int16:
		code = sgetshort(s, &snum);
		if ( (format & 15) == 0 )
		   {	type = t_integer;
			np->value.intval = snum;
		   }
		else
		   {	type = t_real;
			if ( !code )
				np->value.realval =
				  (double)snum * binary_scale[format & 15];
		   }
		break;
	case num_float:
		type = t_real;
		code = sgetfloat(s, &np->value.realval);
		break;
	case num_array:
	   {	uint count = sgets(s, (byte *)np, sizeof(ref));
		code = (count == 0 ? 1 : count == sizeof(ref) ? 0 :
			e_syntaxerror);
		if ( !code )
		 switch ( r_type(np) )
		   {
		case t_integer: return t_integer;
		case t_real: return t_real;
		default: return e_typecheck;
		   }
	   }	break;
	default:
		return e_syntaxerror;	/* invalid num_format?? */
	   }
	switch ( code )
	   {
	case 0: return type;
	case 1: return t_null;		/* end of stream */
	default: return code;
	   }
}

/* ------ Get/put number ------ */

/* Get/put encoded numbers on a stream according to num_format. */
/* 1 means end of stream, 0 means not end, <0 means error. */

/* Get/put a short. */
int
sgetshort(register stream *s, short *p)
{	int a = sgetc(s), b;
	if ( a < 0 ) return 1;
	b = sgetc(s);
	if ( b < 0 ) return e_syntaxerror;
	*p = (short)(s_is_lsb(s) ? (b << 8) + a : (a << 8) + b);
	return 0;
}
void
sputshort(register stream *s, short num)
{	byte a = num & 0xff;
	byte b = (byte)(num >> 8);
	if ( s_is_msb(s) )
	   {	byte t = a; a = b; b = t;
	   }
	sputc(s, a);
	sputc(s, b);
}

/* Get/put a long. */
int
sgetlong(register stream *s, long *p)
{	int a = sgetc(s), b, c, d;
	if ( a < 0 ) return 1;
	b = sgetc(s);
	c = sgetc(s);
	d = sgetc(s);
	if ( (b | c | d) < 0 ) return e_syntaxerror;
	*p =  (long)(s_is_lsb(s) ?
		     ((long)d << 24) + ((long)c << 16) + (b << 8) + a :
		     ((long)a << 24) + ((long)b << 16) + (c << 8) + d);
	return 0;
}
void
sputlong(register stream *s, long num)
{	byte a = num & 0xff;
	byte b = (byte)(num >> 8);
	byte c = (byte)(num >> 16);
	byte d = (byte)(num >> 24);
	if ( s_is_msb(s) )
	   {	byte t = a; a = d; d = t;
		t = b; b = c; c = t;
	   }
	sputc(s, a);
	sputc(s, b);
	sputc(s, c);
	sputc(s, d);
}

/* Get/put a float.  We don't handle non-IEEE native representations yet. */
int
sgetfloat(register stream *s, float *p)
{	if ( s->num_format == num_float_native )
	   {	uint len = sgets(s, (byte *)p, sizeof(float));
		return (len == sizeof(float) ? 0 :
			len == 0 && seofp(s) ? 1 :
			e_syntaxerror);
	   }
	else
	   {	/* Hack: we know floats and longs are the same size. */
		return sgetlong(s, (long *)p);
	   }
}
void
sputfloat(register stream *s, floatp num)
{	float f = num;			/* coerce from double */
	if ( s->num_format == num_float_native )
		sputs(s, (byte *)&f, sizeof(float));
	else
	   {	/* Hack: we know floats and longs are the same size. */
		sputlong(s, *(long *)&f);
	   }
}
