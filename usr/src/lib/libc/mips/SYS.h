/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)SYS.h	8.1 (Berkeley) %G%
 */

#include <sys/syscall.h>
#include <machine/machAsmDefs.h>

#ifdef __STDC__
#define	RSYSCALL(x)	LEAF(x); li v0,SYS_ ## x; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
#define	PSEUDO(x,y)	LEAF(x); li v0,SYS_ ## y; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
#else
#define	RSYSCALL(x)	LEAF(x); li v0,SYS_/**/x; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
#define	PSEUDO(x,y)	LEAF(x); li v0,SYS_/**/y; syscall; bne a3,zero,err; \
			j ra; err: j _cerror; END(x);
#endif
