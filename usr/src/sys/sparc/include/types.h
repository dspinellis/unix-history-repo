/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.1 (Berkeley) %G%
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

#endif	/* _MACHTYPES_H_ */
