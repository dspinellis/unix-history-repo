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
 * from: Utah $Hdr: reg.h 1.5 89/04/11$
 *
 *	@(#)reg.h	7.1 (Berkeley) 5/8/90
 */

/*
 * Location of the users' stored
 * registers relative to D0.
 * Usage is u.u_ar0[XX].
 */
#define	D0	(0)
#define	D1	(1)
#define	D2	(2)
#define	D3	(3)
#define	D4	(4)
#define	D5	(5)
#define	D6	(6)
#define	D7	(7)
#define	A0	(8)
#define	A1	(9)
#define	A2	(10)
#define	A3	(11)
#define	A4	(12)
#define	A5	(13)
#define	A6	(14)
#define	A7	(15)

#define	SP	A7
#define	PC	(17)
#define	PS	(16)

#ifdef IPCREG
#define	NIPCREG 17
int ipcreg[NIPCREG] =
	{D0,D1,D2,D3,D4,D5,D6,D7,A0,A1,A2,A3,A4,A5,A6,A7,PC};
#endif

#ifdef KERNEL
/*
 * Due to a mental lapse somewhere down the line, wait returns its values
 * in strange registers.  Kludge it up here so we don't have to in the
 * machine-independent code.
 */
#define	R0	D1
#define	R1	A0
#endif
