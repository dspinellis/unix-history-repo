/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.5 (Berkeley) %G%
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

typedef struct _physadr {
	int r[1];
} *physadr;

typedef struct label_t {
	int val[14];
} label_t;

#endif	/* _MACHTYPES_H_ */
