/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdbparam.h	7.8 (Berkeley) %G%
 */

#define DBNAME "kdb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"

#define	MAXINT	0x7fffffff
#define	MAXSTOR (KERNBASE - ctob(UPAGES))

#define	ENTRYMASK	1			/* check for entry masks */
#define	ishiddenreg(p)	((p) <= &kdbreglist[8])

#define BPT	0x30
#define KCALL	0xcf
#define CASEL	0xfc
#define TBIT	0x10

#define	KDB_IPL		0xf	/* highest priority software interrupt */
#define	setsoftkdb()	mtpr(SIRR, KDB_IPL)

#define	clrsstep()	(kdbpcb.pcb_psl &= ~TBIT)
#define	setsstep()	(kdbpcb.pcb_psl |= TBIT)

#define	SETBP(ins)	((BPT<<24) | ((ins) & 0xffffff))

#define	getprevpc(fp)	kdbget((off_t)(fp)-8, DSP)	/* pc of caller */
#define	getprevframe(fp) (kdbget((off_t)(fp), DSP)&~3)	/* fp of caller */
#define	getnargs(fp)	(((kdbget((off_t)(fp)-4, DSP)&0xffff)-4)/4)
#define	nextarg(ap)	((ap) + 4)		/* next argument in list */
#define	NOFRAME		0			/* fp at top of call stack */

#define	issignalpc(pc)	((unsigned)MAXSTOR < (pc) && (pc) < (unsigned)KERNBASE)
#define	getsignalpc(fp)	kdbget((off_t)(fp)+44, DSP)/* pc of caller before signal */

#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	(((a) >> 16) & 0xffff)
#define	itol(a,b)	((long)(((a) << 16) | ((b) & 0xffff)))
#define	byte(a)		(((a) >> 24) & 0xff)
#define	btol(a)		((long)((a) << 24))

/* check for address wrap around */
#define	addrwrap(oaddr,newaddr) \
	(((oaddr)^(newaddr)) >> 24)
/*
 * INSTACK tells whether its argument is a stack address.
 * INUDOT tells whether its argument is in the (extended) u. area.
 * These are used for consistency checking and dont have to be exact.
 *
 * INKERNEL tells whether its argument is a kernel space address.
 * KVTOPH trims a kernel virtal address back to its offset
 * in the kernel address space.
 */
#define	INSTACK(x)	(((int)(x)&0xf0000000) == 0xb0000000)
#define	INUDOT(x)	(((int)(x)&0xf0000000) == 0xb0000000)
#define	INKERNEL(x)	(((int)(x)&0xf0000000) == 0xc0000000)

#define	KERNOFF		(KERNBASE + 0x800)	/* start of kernel's text */
/* #define	KVTOPH(x)	((x)&~ 0xc0000000) sometimes */
