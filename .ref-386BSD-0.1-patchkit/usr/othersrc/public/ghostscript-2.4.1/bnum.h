/* Copyright (C) 1990 Aladdin Enterprises.  All rights reserved.
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

/* bnum.h */
/* Interface to Level 2 number readers */
/* Requires stream.h */

/* Homogenous number array formats. */
/* The default for numbers is big-endian. */
#define num_int32 0			/* [0..31] */
#define num_int16 32			/* [32..47] */
#define num_float 48
#define num_float_IEEE num_float
#define num_float_native (num_float + 1)
#define num_msb 0
#define num_lsb 128
#define num_is_lsb(format) ((format) >= num_lsb)
#define num_is_valid(format) (((format) & 127) <= 49)
/* Special "format" for reading from an array */
#define num_array 256

/* Test the byte ordering of numbers on a stream */
#define s_is_lsb(s) num_is_lsb(s->num_format)
#define s_is_msb(s) !s_is_lsb(s)

/* Read from an array or encoded number array */
extern	int	sread_num_array(P2(stream *, ref *));
extern	uint	scount_num_stream(P1(stream *));
extern	int	sget_encoded_number(P2(stream *, ref *));

/* Get/put a number with appropriate byte swapping */
extern	int	sgetshort(P2(stream *, short *));
extern	void	sputshort(P2(stream *, short));
extern	int	sgetlong(P2(stream *, long *));
extern	void	sputlong(P2(stream *, long));
extern	int	sgetfloat(P2(stream *, float *));
extern	void	sputfloat(P2(stream *, floatp));
