/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)Locore.c	1.6 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
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

#include "cpu.h"
#include "mtpr.h"
#include "trap.h"
#include "psl.h"
#include "pte.h"
#include "scb.h"
#include "cp.h"
#include "mem.h"

#include "../tahoemath/fp.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

struct	scb scb;
struct	rpb rpb;
int	dumpflag;
int	intstack[3*NBPG];
int	masterpaddr;		/* p_addr of current process on master cpu */
struct	user u;
int	icode[8];
int	szicode = sizeof (icode);
quad	catcher[SCB_LASTIV];
/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.3 BSD UNIX ....";
int	etext;

doadump() { dumpsys(); }

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
	scb = scb;
	maxmem = physmem = freemem = 0;
	u = u;
	fixctlrmask();
	main(0);

	/*
	 * Routines called from interrupt vectors.
	 */
	buserror((caddr_t)0);
	panic("Machine check");
	printf("Write timeout");
	rawintr();
#ifdef INET
	ipintr();
#endif
#ifdef NS
	nsintr();
#endif
	cnrint(0);
	cnxint(0);
	hardclock((caddr_t)0, 0);
	softclock((caddr_t)0, 0);
	fpemulate(0, 0, 0, 0, 0, 0, 0, 0, 0);
	trap(0, 0, 0, 0, 0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, 0, 0, 0, 0, (unsigned)0, 0, 0);

	if (vmemall((struct pte *)0, 0, (struct proc *)0, 0))
		return;		/* use value */
	boothowto = 0;
	if (rmget((struct map *)0, 0, 0) == 0)
		return;
	dumpflag = 0; dumpflag = dumpflag;
#if !defined(GPROF)
	cp = (caddr_t)&etext;
	cp = cp;
#endif
}

struct	pte Sysmap[6*NPTEPG];
caddr_t	Sysbase;
struct	pte VMEMmap[1];
caddr_t	vmem, vmembeg, vmemend;
struct	pte VMEMmap1[1];
caddr_t	vmem1;
struct	pte VBmap[1];
caddr_t	vbbase, vbend;
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
struct	pte CMAP1[1], CMAP2[1];
caddr_t	CADDR1, CADDR2;
struct	pte mmap[1];
char	vmmap[1];
struct	pte msgbufmap[3*NBPG];
struct	msgbuf msgbuf;
struct	pte kmempt[100], ekmempt[1];
char	kmembase[100*NBPG];
struct	pte Mbmap[NMBCLUSTERS/CLSIZE];
struct	mbuf mbutl[NMBCLUSTERS*CLBYTES/sizeof (struct mbuf)];

/*ARGSUSED*/
badaddr(addr, len) caddr_t addr; int len; { return (0); }
/*ARGSUSED*/
ovbcopy(from, to, len) caddr_t from, to; unsigned len; { }
copyinstr(udaddr, kaddr, maxlength, lencopied)
    caddr_t udaddr, kaddr; u_int maxlength, *lencopied;
{ *kaddr = *udaddr; *lencopied = maxlength; return (0); }
copyoutstr(kaddr, udaddr, maxlength, lencopied)
    caddr_t kaddr, udaddr; u_int maxlength, *lencopied;
{ *kaddr = *udaddr; *lencopied = maxlength; return (0); }
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
/*ARGSUSED*/
subyte(base, i) caddr_t base; { return (0); }
/*ARGSUSED*/
fuword(base) caddr_t base; { return (0); }
/*ARGSUSED*/
suword(base, i) caddr_t base; { return (0); }

/*ARGSUSED*/
copyseg(udaddr, pf)
    caddr_t udaddr; unsigned pf;
{ CMAP1[0] = CMAP1[0]; CADDR1[0] = CADDR1[0]; }

/*ARGSUSED*/
clearseg(pf) unsigned pf; { CMAP2[0] = CMAP2[0]; CADDR2[0] = CADDR2[0]; }

/*ARGSUSED*/
useracc(udaddr, bcnt, rw) caddr_t udaddr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
kernacc(addr, bcnt, rw) caddr_t addr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
addupc(pc, prof, counts) int pc; struct uprof *prof; int counts; { }

/*ARGSUSED*/
scanc(size, cp, table, mask)
    unsigned size; u_char *cp, table[]; u_char mask;
{ return (0); }

/*ARGSUSED*/
skpc(mask, size, cp) int mask; int size; char *cp; { return (0); }

#ifdef notdef
/*ARGSUSED*/
locc(mask, size, cp) int mask; int size; char *cp; { return (0); }
#endif

/*
 * Routines expanded by inline.
 */
#ifdef notdef
fuibyte(base) caddr_t base; { return (fubyte(base)); }
#endif
fuiword(base) caddr_t base; { return (fuword(base)); }
suibyte(base, i) caddr_t base; { return (subyte(base, i)); }
suiword(base, i) caddr_t base; { return (suword(base, i)); }

/*ARGSUSED*/
setjmp(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
_insque(p, q) caddr_t p, q; { }
/*ARGSUSED*/
_remque(p) caddr_t p; { }

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
_movow(dst, v) u_short *dst, v; { }
/*ARGSUSED*/
_movob(dst, v) u_char *dst, v; { }

/*ARGSUSED*/
ffs(v) long v; { return (0); }

imin(a, b) int a, b; { return (a < b ? a : b); }
imax(a, b) int a, b; { return (a > b ? a : b); }
#ifdef notdef
unsigned min(a, b) u_int a, b; { return (a < b ? a : b); }
unsigned max(a, b) u_int a, b; { return (a > b ? a : b); }
#endif
