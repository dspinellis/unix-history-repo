/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.9 (Berkeley) %G%
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

typedef struct _physadr {
	short r[1];
} *physadr;

typedef struct label_t {		/* consistent with HP-UX */
	int val[15];
} label_t;

typedef	u_long	vm_offset_t;
typedef	u_long	vm_size_t;

#endif	/* _MACHTYPES_H_ */
