/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.8 (Berkeley) %G%
 */

#ifndef	_MACHTYPES_H_
#define	_MACHTYPES_H_

typedef struct _physadr {
	int r[1];
} *physadr;

typedef struct label_t {
	int val[14];
} label_t;

/*
 * Basic integral types.  Omit the typedef if
 * not possible for a machine/compiler combination.
 */
typedef	signed char		   int8;
typedef	unsigned char		 u_int8;
typedef	short			  int16;
typedef	unsigned short		u_int16;
typedef	int			  int32;
typedef	unsigned int		u_int32;
typedef	long long		  int64;
typedef	unsigned long long	u_int64;

#endif	/* _MACHTYPES_H_ */
