/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)Locore.c	7.5 (Berkeley) %G%
 */

#include "dz.h"
#include "mba.h"
#include "uba.h"

#include "pte.h"

#include "param.h"
#include "systm.h"
#include "user.h"
#include "vm.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "buf.h"
#include "msgbuf.h"
#include "mbuf.h"
#include "protosw.h"
#include "domain.h"
#include "map.h"
#include "dkbad.h"

#include "scb.h"
#include "nexus.h"
#include "ioa.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

struct	scb scb[1];
int	(*UNIvec[NUBA][128])();		/* unibus vec for ubas */
int	(*eUNIvec)();			/* end of unibus vec */
struct	rpb rpb;
int	dumpflag;
int	intstack[3*NBPG];
int	masterpaddr;		/* p_addr of current process on master cpu */
struct	user u;
int	icode[8];
int	szicode = sizeof (icode);
/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.3 BSD UNIX ....";
int	etext;

doadump() { dumpsys(); }

#if NMBA > 0
Xmba3int() { }
Xmba2int() { }
Xmba1int() { }
Xmba0int() { }
#endif

lowinit()
{
#if !defined(GPROF)
	caddr_t cp;
#endif
	extern int dumpmag;
	extern int rthashsize;
	extern int arptab_size;
	extern int dk_ndrive;
	extern struct domain unixdomain;
#ifdef INET
	extern struct domain inetdomain;
#endif
#include "imp.h"
#if NIMP > 0
	extern struct domain impdomain;
#endif
#ifdef NS
	extern struct domain nsdomain;
#endif

	/* cpp messes these up for lint so put them here */
	unixdomain.dom_next = domains;
	domains = &unixdomain;
#ifdef INET
	inetdomain.dom_next = domains;
	domains = &inetdomain;
#endif
#if NIMP > 0
	impdomain.dom_next = domains;
	domains = &impdomain;
#endif
#ifdef NS
	nsdomain.dom_next = domains;
	domains = &nsdomain;
#endif
	dumpmag = 0;			/* used only by savecore */
	rthashsize = rthashsize;	/* used by netstat, etc. */
	arptab_size = arptab_size;	/* used by arp command */
	dk_ndrive = dk_ndrive;		/* used by vmstat, iostat, etc. */

	/*
	 * Pseudo-uses of globals.
	 */
	lowinit();
	intstack[0] = intstack[1];
	rpb = rpb;
	scb[0] = scb[0];
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
	(*UNIvec[0][0])();
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
#if VAX8200			/* XXX wrong conditional */
	bi_buserr(0);
#endif
#if VAX8200
	rxcdintr();
	rx50intr();
#endif
	hardclock((caddr_t)0, 0);
	softclock((caddr_t)0, 0);
	trap(0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, (unsigned)0, 0, 0);
	rawintr();
#ifdef INET
	ipintr();
#endif
#ifdef NS
	nsintr();
#endif
	machinecheck((caddr_t)0);
	memerr();

	/*
	 * Miscellaneous routines called from configurable
	 * drivers.
	 */
	ubapurge((struct uba_ctlr *)0);
	ubattydma(0);
	(void) ubamem(0, 0, 16, 1);
	(void) isbad((struct dkbad *)0, 0, 0, 0);
	disksort((struct buf *)0, (struct buf *)0);
	(void) uwritec((struct uio *)0);
	(void) todr();
	if (vmemall((struct pte *)0, 0, (struct proc *)0, 0))
		return;		/* use value */
	boothowto = 0;
	dumpflag = 0; dumpflag = dumpflag;
#ifdef KADB
	bootesym = 0; bootesym = bootesym;
#endif
#if !defined(GPROF)
	cp = (caddr_t)&etext;
	cp = cp;
#endif
}

consdin() { }
consdout() { }
#if NDZ > 0
dzdma() { dzxint((struct tty *)0); }
#endif

quad	catcher[128];
int	cold = 1;

Xustray() { }

struct	pte Sysmap[6*NPTEPG];
#ifdef KADB
char	Sysbase[6*NPTEPG*NBPG];
#endif
int	umbabeg;
struct	pte Nexmap[16][16];
struct	nexus nexus[MAXNNEXUS];
#if VAX8600
struct	pte Ioamap[MAXNIOA][IOAMAPSIZ/NBPG];
#endif
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
struct	pte kmempt[200], ekmempt[1];
#if VAX8200
struct	pte RX50map[1];
struct	pte Ka820map[1];
#endif
#if VAX630
struct	pte Ka630map[1];
#endif
char	kmembase[100*NBPG];
#if VAX8200 || VAX630
struct	pte Clockmap[1];
#endif
#ifdef NFS
struct	pte Nfsiomap[MAXPHYS/NBPG+1];
char	nfsiobuf[MAXPHYS+NBPG];
#endif

/*ARGSUSED*/
badaddr(addr, len) caddr_t addr; int len; { return (0); }

/*ARGSUSED*/
ovbcopy(from, to, len) caddr_t from, to; unsigned len; { }
copyinstr(udaddr, kaddr, maxlength, lencopied)
    caddr_t udaddr, kaddr; u_int maxlength, *lencopied;
{ *kaddr = *udaddr; *lencopied = maxlength; return (0); }
copyoutstr(kaddr, udaddr, maxlength, lencopied)
    caddr_t kaddr, udaddr; u_int maxlength, *lencopied;
{ *udaddr = *kaddr; *lencopied = maxlength; return (0); }
copystr(kfaddr, kdaddr, maxlength, lencopied)
    caddr_t kfaddr, kdaddr; u_int maxlength, *lencopied;
{ *kdaddr = *kfaddr; *lencopied = maxlength; return (0); }

/*ARGSUSED*/
copyin(udaddr, kaddr, n) caddr_t udaddr, kaddr; u_int n; { return (0); }
/*ARGSUSED*/
copyout(kaddr, udaddr, n) caddr_t kaddr, udaddr; u_int n; { return (0); }

/*ARGSUSED*/
longjmp(lp) label_t *lp; { /*NOTREACHED*/ }

/*ARGSUSED*/
savectx(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
setrq(p) struct proc *p; { }

/*ARGSUSED*/
remrq(p) struct proc *p; { }

swtch() { if (whichqs) whichqs = 0; if (masterpaddr) masterpaddr = 0; }

/*ARGSUSED*/
resume(pcbpf) unsigned pcbpf; { }

/*ARGSUSED*/
fubyte(base) caddr_t base; { return (0); }
#ifdef notdef
/*ARGSUSED*/
fuibyte(base) caddr_t base; { return (0); }
#endif
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
copyseg(udaddr, pf)
    caddr_t udaddr; unsigned pf;
{ CMAP1 = CMAP1; CADDR1[0] = CADDR1[0]; }

/*ARGSUSED*/
clearseg(pf) unsigned pf; { CMAP2 = CMAP2; CADDR2[0] = CADDR2[0]; }

/*ARGSUSED*/
useracc(udaddr, bcnt, rw) caddr_t udaddr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
kernacc(addr, bcnt, rw) caddr_t addr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
addupc(pc, prof, counts) int pc; struct uprof *prof; int counts; { }

/*
 * Routines expanded by inline.
 */
spl0() { }
splsoftclock() { return (0); }
splnet() { return (0); }
spl4() { return (0); }
spl5() { return (0); }
splbio() { return (0); }
spltty() { return (0); }
#ifdef notdef
spl6() { return (0); }		/* not currently used */
#endif
splclock() { return (0); }
spl7() { return (0); }
splhigh() { return (0); }

/*ARGSUSED*/
splx(s) int s; { }

/*ARGSUSED*/
bcopy(from, to, len) caddr_t from, to; unsigned len; { }
/*ARGSUSED*/
bzero(base, count) caddr_t base; unsigned count; { }
/*ARGSUSED*/
blkclr(base, count) caddr_t base; unsigned count; { }

/*ARGSUSED*/
/*VARARGS1*/
mtpr(reg, v) int reg; { }
/*ARGSUSED*/
mfpr(reg) int reg; { return (0); }

/*ARGSUSED*/
setjmp(lp) label_t *lp; { return (0); }

#ifndef VAX630
/*ARGSUSED*/
scanc(size, cp, table, mask)
    unsigned size; char *cp, table[]; int mask;
{ return (0); }
#endif

/*ARGSUSED*/
skpc(mask, size, cp) int mask; int size; char *cp; { return (0); }

#ifdef notdef
/*ARGSUSED*/
locc(mask, size, cp) int mask; char *cp; unsigned size; { return (0); }
#endif

/*ARGSUSED*/
_insque(p, q) caddr_t p, q; { }
/*ARGSUSED*/
_remque(p) caddr_t p; { }

/*ARGSUSED*/
ffs(v) long v; { return (0); }

#ifdef notdef
imin(a, b) int a, b; { return (a < b ? a : b); }
imax(a, b) int a, b; { return (a > b ? a : b); }
unsigned min(a, b) u_int a, b; { return (a < b ? a : b); }
unsigned max(a, b) u_int a, b; { return (a > b ? a : b); }
#endif
u_short ntohs(s) u_short s; { return ((u_short)s); }
u_short htons(s) u_short s; { return ((u_short)s); }
u_long ntohl(l) u_long l; { return ((u_long)l); }
u_long htonl(l) u_long l; { return ((u_long)l); }
