/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)Locore.c	6.4 (Berkeley) %G%
 */

#include "dz.h"
#include "mba.h"
#include "uba.h"

#include "pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "vm.h"
#include "tty.h"
#include "proc.h"
#include "buf.h"
#include "msgbuf.h"
#include "mbuf.h"
#include "protosw.h"
#include "domain.h"

#include "nexus.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

struct	scb scb;
int	(*UNIvec[128])();
#if NUBA > 1
int	(*UNI1vec[128])();
#endif
struct	rpb rpb;
int	intstack[3*128];

int	masterpaddr;		/* p_addr of current process on master cpu */

struct	user u;

doadump() { dumpsys(); }

Xmba3int() { }
Xmba2int() { }
Xmba1int() { }
Xmba0int() { }

lowinit()
{
	extern int dumpmag;
	extern struct domain unixdomain;
#ifdef PUP
	extern struct domain pupdomain;
#endif
#ifdef INET
	extern struct domain inetdomain;
#endif
#include "imp.h"
#if NIMP > 0
	extern struct domain impdomain;
#endif

	/* cpp messes these up for lint so put them here */
	unixdomain.dom_next = domains;
	domains = &unixdomain;
#ifdef PUP
	pupdomain.dom_next = domains;
	domains = &pupdomain;
#endif
#ifdef INET
	inetdomain.dom_next = domains;
	domains = &inetdomain;
#endif
#if NIMP > 0
	impdomain.dom_next = domains;
	domains = &impdomain;
#endif
	dumpmag = 0;			/* used only by savecore */

	/*
	 * Pseudo-uses of globals.
	 */
	lowinit();
	intstack[0] = intstack[1];
	rpb = rpb;
	scb = scb;
	maxmem = physmem = freemem = 0;
	u = u;
	fixctlrmask();
	main(0);
	Xustray();

	/*
	 * Routines called from interrupt vectors.
	 */
	panic("Machine check");
	printf("Write timeout");
	(*UNIvec[0])();
#if NUBA > 1
	(*UNI1vec[0])();
#endif
	ubaerror(0, (struct uba_hd *)0, 0, 0, (struct uba_regs *)0);
	cnrint(0);
	cnxint(0);
	consdin();
	consdout();
#if NDZ > 0
	dzdma();
#endif
#if NMBA > 0
	mbintr(0);
#endif
	hardclock((caddr_t)0, 0);
	softclock((caddr_t)0, 0);
	trap(0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, (unsigned)0, 0, 0);
	ipintr();
	rawintr();

	if (vmemall((struct pte *)0, 0, (struct proc *)0, 0))
		return;		/* use value */
	machinecheck((caddr_t)0);
	memerr();
	boothowto = 0;
}

consdin() { }
consdout() { }
#if NDZ > 0
dzdma() { dzxint((struct tty *)0); }
#endif

int	catcher[256];
int	cold = 1;

Xustray() { }

struct	pte Sysmap[6*NPTEPG];
char	Sysbase[6*NPTEPG*NBPG];
int	umbabeg;
struct	pte Nexmap[16][16];
struct	nexus nexus[MAXNNEXUS];
struct	pte UMEMmap[NUBA][512];
char	umem[NUBA][512*NBPG];
int	umbaend;
struct	pte Usrptmap[USRPTSIZE];
struct	pte usrpt[USRPTSIZE*NPTEPG];
struct	pte Forkmap[UPAGES];
struct	user forkutl;
struct	pte Xswapmap[UPAGES];
struct	user xswaputl;
struct	pte Xswap2map[UPAGES];
struct	user xswap2utl;
struct	pte Swapmap[UPAGES];
struct	user swaputl;
struct	pte Pushmap[UPAGES];
struct	user pushutl;
struct	pte Vfmap[UPAGES];
struct	user vfutl;
struct	pte CMAP1;
char	CADDR1[NBPG];
struct	pte CMAP2;
char	CADDR2[NBPG];
struct	pte mmap[1];
char	vmmap[NBPG];
struct	pte Mbmap[NMBCLUSTERS/CLSIZE];
struct	mbuf mbutl[NMBCLUSTERS*CLBYTES/sizeof (struct mbuf)];
struct	pte msgbufmap[CLSIZE];
struct	msgbuf msgbuf;
struct	pte camap[32];
int	cabase;
#ifdef unneeded
char	caspace[32*NBPG];
#endif
int	calimit;

/*ARGSUSED*/
badaddr(addr, len) caddr_t addr; int len; { return (0); }

/*ARGSUSED*/
copyin(udaddr, kaddr, n) caddr_t udaddr, kaddr; unsigned n; { return (0); }

/*ARGSUSED*/
copyout(kaddr, udaddr, n) caddr_t kaddr, udaddr; unsigned n; { return (0); }

/*ARGSUSED*/
setjmp(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
longjmp(lp) label_t *lp; { /*NOTREACHED*/ }

/*ARGSUSED*/
setrq(p) struct proc *p; { }

/*ARGSUSED*/
remrq(p) struct proc *p; { }

swtch() { if (whichqs) whichqs = 0; if (masterpaddr) masterpaddr = 0; }

/*ARGSUSED*/
resume(pcbpf) unsigned pcbpf; { }

/*ARGSUSED*/
fubyte(base) caddr_t base; { return (0); }

/*ARGSUSED*/
subyte(base, i) caddr_t base; { return (0); }

/*ARGSUSED*/
suibyte(base, i) caddr_t base; { return (0); }

/*ARGSUSED*/
fuword(base) caddr_t base; { return (0); }

/*ARGSUSED*/
fuiword(base) caddr_t base; { return (0); }

/*ARGSUSED*/
suword(base, i) caddr_t base; { return (0); }

/*ARGSUSED*/
suiword(base, i) caddr_t base; { return (0); }

/*ARGSUSED*/
copyseg(udaddr, pf) caddr_t udaddr; unsigned pf; {
    CMAP1 = CMAP1; CADDR1[0] = CADDR1[0];
}

/*ARGSUSED*/
clearseg(pf) unsigned pf; { CMAP2 = CMAP2; CADDR2[0] = CADDR2[0]; }

/*ARGSUSED*/
useracc(udaddr, bcnt, rw) caddr_t udaddr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
kernacc(addr, bcnt, rw) caddr_t addr; unsigned bcnt; { return (0); }

/*
 * Routines handled by asm.sed script.
 */

/*VARARGS1*/
/*ARGSUSED*/
mtpr(reg, value) int reg, value; { }

/*ARGSUSED*/
mfpr(reg) int reg; { return (0); }


spl0() { }
spl4() { return (0); }
spl5() { return (0); }
spl6() { return (0); }
spl7() { return (0); }

/*ARGSUSED*/
splx(s) int s; { }

/*ARGSUSED*/
bcopy(from, to, count) caddr_t from, to; unsigned count; { ; }

/*ARGSUSED*/
bzero(base, count) caddr_t base; unsigned count; { ; }

/*ARGSUSED*/
bcmp(s1, s2, count) caddr_t s1, s2; unsigned count; { return (0); }

/*ARGSUSED*/
scanc(size, cp, table, mask)
unsigned size; caddr_t cp, table; int mask; { return (0); }

/*ARGSUSED*/
ffs(i) { return (0); }

ntohs(s) u_short s; { return ((int)s); }

htons(s) u_short s; { return ((int)s); }

/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.2 BSD UNIX ....";
char	etext;
