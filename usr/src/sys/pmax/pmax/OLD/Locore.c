/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Locore.c	7.2 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/user.h>
#include <sys/vm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>
#include <sys/domain.h>
#include <sys/map.h>

#include <machine/pte.h>

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

int	dumpflag;
int	intstack[3*NBPG];
int	icode[8];
int	szicode = sizeof (icode);
char	MachUTLBMiss[10], MachUTLBMissEnd[1];
char	MachException[10], MachException[1];

/*
 * Variables declared for savecore, or
 * implicitly, such as by config or the loader.
 */
char	version[] = "4.3 BSD UNIX ....";
int	etext;

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
	consdin();
	consdout();
	hardclock((caddr_t)0, 0);
	softclock((caddr_t)0, 0);
	trap((unsigned)0, (unsigned)0, (unsigned)0, (unsigned)0);
	memerr();

	/*
	 * Miscellaneous routines called from configurable
	 * drivers.
	 */
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

struct	pte Sysmap[6*NPTEPG];
#ifdef KADB
char	Sysbase[6*NPTEPG*NBPG];
#endif
struct	pte Usrptmap[USRPTSIZE];
struct	pte usrpt[USRPTSIZE*NPTEPG];
struct	pte Usriomap[USRIOSIZE];
struct	pte usrio[USRIOSIZE*NPTEPG];
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
struct	pte Mbmap[NMBCLUSTERS/CLSIZE];
struct	mbuf mbutl[NMBCLUSTERS*CLBYTES/sizeof (struct mbuf)];
struct	pte kmempt[200];
char	kmembase[100*NBPG];

/*ARGSUSED*/
badaddr(addr, len) char *addr; int len; { return (0); }

/*ARGSUSED*/
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
bzero(to, n) caddr_t to; u_int n; { }
/*ARGSUSED*/
bcmp(from, to, n) caddr_t from, to; u_int n; { }
/*ARGSUSED*/
bcopy(from, to, n) caddr_t from, to; u_int n; { }

/*ARGSUSED*/
CopyToBuffer(src, dst, length)
	u_short *src, *dst; int length;
{ *dst = *src; }
/*ARGSUSED*/
CopyFromBuffer(src, dst, length)
	u_short *src; char *dst; int length;
{ *dst = *src; }

/*ARGSUSED*/
savectx(lp) label_t *lp; { return (0); }

/*ARGSUSED*/
setrq(p) struct proc *p; { }

/*ARGSUSED*/
remrq(p) struct proc *p; { }

swtch() { if (whichqs) whichqs = 0; }

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
copyseg(udaddr, pf) caddr_t udaddr; unsigned pf; { }

/*ARGSUSED*/
clearseg(pf) unsigned pf; { }

/*ARGSUSED*/
addupc(pc, prof, ticks) unsigned pc; struct uprof *prof; int ticks; { }

void MachEnableIntr() { }
void setsoftnet() { }
void clearsoftnet() { }
void setsoftclock() { }
void clearsoftclock() { }
spl0() { return (0); }
splsoftclock() { return (0); }
splnet() { return (0); }
splimp() { return (0); } /* XXX */
splbio() { return (0); }
spltty() { return (0); }
splclock() { return (0); }
splhigh() { return (0); }

/*ARGSUSED*/
splx(s) int s; { }

#ifdef notdef
/*ARGSUSED*/
scanc(size, cp, table, mask)
    unsigned size; char *cp, table[]; int mask;
{ return (0); }

/*ARGSUSED*/
skpc(mask, size, cp) int mask; int size; char *cp; { return (0); }

/*ARGSUSED*/
locc(mask, size, cp) int mask; char *cp; unsigned size; { return (0); }
#endif

/*ARGSUSED*/
_insque(p, q) caddr_t p, q; { }
/*ARGSUSED*/
_remque(p) caddr_t p; { }

/*ARGSUSED*/
ffs(v) long v; { return (0); }

/*ARGSUSED*/
strlen(str) char *str; { return (0); }

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

void MachKernGenException() { }
void MachUserGenException() { }
void MachTLBModException() { }
void MachTLBMissException() { }
void MachEmptyWriteBuffer() { }
/*ARGSUSED*/
void MachTLBWriteIndexed(index, highEntry, lowEntry)
	int index, highEntry, lowEntry; { }
/*ARGSUSED*/
void MachSetPID(pid) int pid; { }
/*ARGSUSED*/
void newptes(pte, v, size) struct pte *pte; u_int v; int size; { }
void MachTLBFlush() { }
/*ARGSUSED*/
void MachTLBFlushPID(pid) int pid; { }
/*ARGSUSED*/
void MachTLBFlushAddr(virt) caddr_t virt; { }
/*ARGSUSED*/
void MachSwitchFPState(from, to) struct proc *from; struct user *to; { }
/*ARGSUSED*/
void MachGetCurFPState(p) struct proc *p; { }
/*ARGSUSED*/
void MachFPInterrupt(p) struct proc *p; { }
/*ARGSUSED*/
void MachFPInterrupt(statusReg, causeReg, pc)
	unsigned statusReg, causeReg, pc; { }
void MachConfigCache() { }
void MachFlushCache() { }
/*ARGSUSED*/
void MachFlushICache(vaddr, len) caddr_t vaddr, int len; { }
