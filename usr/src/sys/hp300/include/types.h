/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.11 (Berkeley) %G%
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

typedef struct _physadr {
	short r[1];
} *physadr;

typedef struct label_t {			/* Consistent with HP-UX. */
	int val[15];
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
