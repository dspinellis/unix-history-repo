/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	5.2 (Berkeley) %G%
 */

#include <sys/syscall.h>
#include <machine/machAsmDefs.h>

/* vax/tahoe compat */
#define	ret
#define	r0	v0
#define	r1	v1

#define	SYSCALL(x)	LEAF(x); li v0,SYS_/**/x; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
#define	PSEUDO(x,y)	LEAF(x); li v0,SYS_/**/y; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
