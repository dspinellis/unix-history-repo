/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.3 (Berkeley) %G%
 *
 * from: $Header: types.h,v 1.4 92/06/17 06:10:30 torek Exp $ (LBL)
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

typedef struct _physadr {
	short r[1];
} *physadr;

typedef struct label_t {
	int val[2];
} label_t;

typedef	u_long	vm_offset_t;
typedef	u_long	vm_size_t;

#ifdef	__GNUC__
typedef	char			s1byte_t;	/* Basic data types. */
typedef	unsigned char		u1byte_t;
typedef	short			s2byte_t;
typedef	unsigned short		u2byte_t;
typedef	long			s4byte_t;
typedef	unsigned long		u4byte_t;
typedef	long long		s8byte_t;
typedef	unsigned long long	u8byte_t;
typedef	float			f4byte_t;
typedef	double			f8byte_t;
#endif

#endif	/* _MACHTYPES_H_ */
