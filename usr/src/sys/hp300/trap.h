/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: trap.h 1.6 88/11/25$
 *
 *	@(#)trap.h	7.2 (Berkeley) 6/22/90
 */

/*
 * Trap type values
 */

#define	T_BUSERR	0
#define	T_ADDRERR	1
#define	T_ILLINST	2
#define	T_ZERODIV	3
#define	T_CHKINST	4
#define	T_TRAPVINST	5
#define	T_PRIVINST	6
#define	T_TRACE		7
#define	T_MMUFLT	8
#define	T_SSIR		9
#define	T_FMTERR	10
#define T_FPERR		11
#define T_COPERR	12
#define T_ASTFLT	13
#define T_TRAP15	15
