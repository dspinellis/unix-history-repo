/*
 *	@(#)kdbparam.h	7.2 (Berkeley) 5/7/88
 */

#define DBNAME "kdb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"

#define	KDB_IPL		0xf	/* highest priority software interrupt */
#define	setsoftkdb()	mtpr(SIRR, KDB_IPL)

#define	MAXINT	0x7fffffff
#define	MAXSTOR (KERNBASE - ctob(UPAGES))

#define	ENTRYMASK	1			/* check for entry masks */
#define	ishiddenreg(p)	((p) <= &reglist[8])

#define BPT	03
#define TBIT	020

#define	clrsstep()	(pcb.pcb_psl &= ~TBIT)
#define	setsstep()	(pcb.pcb_psl |= TBIT)

#define	SETBP(ins)	(BPT | ((ins) & ~0xff))

#define	getprevpc(fp)	get((fp)+16, DSP)	/* pc of caller */
#define	getprevframe(fp) (get((fp)+12, DSP)&~2)	/* fp of caller */
#define	getnargs(fp)	(get((fp)+6, DSP)&0xffff)
#define	nextarg(ap)	((ap) + 4)		/* next argument in list */
#define	NOFRAME		0			/* fp at top of call stack */

#define	issignalpc(pc)	(MAXSTOR < (pc) && (pc) < MAXSTOR+ctob(UPAGES))
#define	getsignalpc(fp)	get((fp)+92, DSP)	/* pc of caller before signal */

/* long to ints and back (puns) */
union {
	int	I[2];
	long	L;
} itolws;

#define leng(a)		itol(0,a)
#define shorten(a)	((short)(a))
#define itol(a,b)	(itolws.I[0]=(b), itolws.I[1]=(a), itolws.L)
#define	byte(a)		((a) & 0xff)
#define	btol(a)		((a))

/* check for address wrap around */
#define	addrwrap(oaddr,newaddr) (((oaddr)^(newaddr)) >> 24)
/*
 * INSTACK tells whether its argument is a stack address.
 * INUDOT tells whether its argument is in the (extended) u. area.
 * These are used for consistency checking and dont have to be exact.
 *
 * INKERNEL tells whether its argument is a kernel space address.
 * KVTOPH trims a kernel virtal address back to its offset
 * in the kernel address space.
 */
#define	INSTACK(x)	(((x)&0xf0000000) == 0x70000000)
#define	INUDOT(x)	(((x)&0xf0000000) == 0x70000000)
#define	INKERNEL(x)	(((x)&0xf0000000) == 0x80000000)

#define	KVTOPH(x)	((x)&~ 0x80000000)
#define	KERNOFF		0x80000000
