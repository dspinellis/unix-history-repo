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
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.5 (Berkeley) %G%
 *
 * from: $Header: types.h,v 1.5 92/11/26 02:00:07 torek Exp $ (LBL)
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

typedef	char			   int8;	/* Basic integral types. */
typedef	unsigned char		 u_int8;
typedef	short			  int16;
typedef	unsigned short		u_int16;
typedef	int			  int32;
typedef	unsigned int		u_int32;
typedef	long long		  int64;
typedef	unsigned long long	u_int64;

#endif	/* _MACHTYPES_H_ */
