/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)Locore.c	7.1 (Berkeley) 5/8/90
 */

#include "pte.h"
#include "cpu.h"

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

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

int	machineid;
int	mmutype;
int	ectype;
struct	user u;
int	icode[8];
int	szicode = sizeof (icode);
u_int	lowram;
u_char	ssir;
int	Usrptsize;

/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.3 BSD UNIX ....";
int	etext;

doadump() { dumpsys(); doboot(); }

lowinit()
{
#if !defined(GPROF)
	caddr_t cp;
#endif
	struct frame frame;
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
	machineid = machineid;
	mmutype = mmutype;
	ectype = ectype;
	lowram = lowram;
	ssir = ssir;
	maxmem = physmem = freemem = 0;
	u = u;
	Usrptsize = Usrptsize;
	main(0);

	/*
	 * Routines called from interrupt vectors.
	 */
	panic("Machine check");
	printf("Write timeout");
	hilint();
	if (dmaintr())
		return;
	intrhand(0);
	regdump((int *)0, 0);
	hardclock((caddr_t)0, 0);
	nmihand(frame);
	softclock((caddr_t)0, 0);
	trap(0, (unsigned)0, (unsigned)0, frame);
	syscall(0, frame);
	straytrap(0);

	/*
	 * Miscellaneous routines called from configurable
	 * drivers.
	 */
	disksort((struct buf *)0, (struct buf *)0);
	if (vmemall((struct pte *)0, 0, (struct proc *)0, 0))
		return;		/* use value */
	boothowto = 0;
/*	dumpflag = 0; dumpflag = dumpflag; */
#if !defined(GPROF)
	cp = (caddr_t)&etext;
	cp = cp;
#endif
}

int	cold = 1;

struct	pte Sysmap[SYSPTSIZE];
struct	pte Usrptmap[USRPTSIZE];
int	Usrptsize;
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
char	mbutl[NMBCLUSTERS][MCLBYTES];
struct	pte msgbufmap[CLSIZE];
struct	msgbuf msgbuf;
struct	pte kmempt[200], ekmempt[1];
int	IObase;
struct	pte Usriomap[USRIOSIZE];
char	usrio[USRIOSIZE*NBPG];
struct	ste Sysseg[NPTEPG];
struct	pte Grfmap[1024];
char	grfregs[1024*NBPG];

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

swtch() { if (whichqs) whichqs = 0; }

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

TBIA() { }
/*ARGSUSED*/
TBIS(addr) caddr_t addr; { }
TBIAS() { }
TBIAU() { }
ICIA() { }
DCIA() { }
DCIS() { }
DCIU() { }
PCIA() { }
ecacheon() { }
ecacheoff() { }

getsfc() { return (0); }
getdfc() { return (0); }

/*ARGSUSED*/
loadustp(ustp) int ustp; { }
/*ARGSUSED*/
flushustp(ustp) int ustp; { }

/*ARGSUSED*/
ploadw(addr) caddr_t addr; { }

/*ARGSUSED*/
addupc(pc, prof, counts) int pc; struct uprof *prof; int counts; { }

spl0() { }
splsoftclock() { return (0); }
splnet() { return (0); }
spl1() { return (0); }
spl2() { return (0); }
spl3() { return (0); }
spl4() { return (0); }
splimp() { return (0); }
splbio() { return (0); }
spltty() { return (0); }
spl5() { return (0); }
splclock() { return (0); }
spl6() { return (0); }
splhigh() { return (0); }
spl7() { return (0); }

/*ARGSUSED*/
splx(s) int s; { }

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
bcmp(str1, str2, count) caddr_t str1, str2; unsigned count; { return (0); }

/*ARGSUSED*/
strlen(str) caddr_t str; { return (0); }

/*ARGSUSED*/
setjmp(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
qsetjmp(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
scanc(size, cp, table, mask)
    unsigned size; u_char *cp, table[]; int mask;
{ return (0); }

/*ARGSUSED*/
skpc(mask, size, cp) int mask; int size; char *cp; { return (0); }

/*ARGSUSED*/
locc(mask, size, cp) int mask; char *cp; unsigned size; { return (0); }

/*ARGSUSED*/
ffs(v) long v; { return (0); }

#ifdef FPCOPROC
/*ARGSUSED*/
m68881_save(fpframep) struct fpframe *fpframep; { }
/*ARGSUSED*/
m68881_restore(fpframep) struct fpframe *fpframep; { }
#endif

doboot() { /*NOTREACHED*/ }
