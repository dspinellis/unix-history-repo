/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)reg.h	5.2 (Berkeley) %G%
 */

/*
 * Location of the users' stored
 * registers within appropriate frame of 'trap' and 'syscall', relative to
 * base of stack frame.
 * Normal usage is u.u_ar0[XX] in kernel.
 */

/* When referenced during a trap/exception, registers are at these offsets */

#define	tES	(0)
#define	tDS	(1)
#define	tEDI	(2)
#define	tESI	(3)
#define	tEBP	(4)

#define	tEBX	(6)
#define	tEDX	(7)
#define	tECX	(8)
#define	tEAX	(9)

#define	tEIP	(12)
#define	tCS	(13)
#define	tEFLAGS	(14)
#define	tESP	(15)
#define	tSS	(16)

/* During a system call, registers are at these offsets instead of above. */

#define	sEDI	(0)
#define	sESI	(1)
#define	sEBP	(2)

#define	sEBX	(4)
#define	sEDX	(5)
#define	sECX	(6)
#define	sEAX	(7)
#define	sEFLAGS	(8)
#define	sEIP	(9)
#define	sCS	(10)
#define	sESP	(11)
#define	sSS	(12)

#define	SP	sESP
#define	PS	sEFLAGS
#define	R0	sEDX
#define	R1	sECX
/*
 * Registers accessible to ptrace(2) syscall for debugger
 */
#ifdef IPCREG
#define	NIPCREG 14
int ipcreg[NIPCREG] =
  { tES,tDS,tEDI,tESI,tEBP,tEBX,tEDX,tECX,tEAX,tEIP,tCS,tEFLAGS,tESP,tSS };
#endif
