/*	Locore.c	4.5	%G%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/tty.h"
#include "../h/cmap.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/uba.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */
struct	cmap *cmap;
struct	cmap *ecmap;

struct	scb scb;
int	(*UNIvec[128])();
struct	rpb rpb;
int	intstack[3*128];

struct	user u;

doadump() { dumpsys(); }

Xmba3int() { }
Xmba2int() { }
Xmba1int() { }
Xmba0int() { }

#if VAX780
Xuba3int() { }
Xuba2int() { }
Xuba1int() { }
Xuba0int() { }
#endif

lowinit()
{

	/*
	 * Pseudo-uses of globals.
	 */
	lowinit();
	intstack[0] = intstack[1];
	scb = scb;
	maxmem = physmem = freemem = 0;
	u = u;
	fixctlrmask();
	main(0);

	/*
	 * Routines called from interrupt vectors.
	 */
	panic("Machine check");
	printf("Write timeout");
	(*UNIvec[0])();
	ubaerror(0, (struct uba_hd *)0, 0, 0, (struct uba_regs *)0);
	cnrint(0);
	cnxint(0);
	hardclock((caddr_t)0, 0);
	trap(0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, (unsigned)0, 0, 0);
}

consdin() { }
consdout() { }
dzdma() { dzxint((struct tty *)0); }

int	catcher[256];
int	cold = 1;

Xustray() { }

struct	pte Sysmap[6*NPTEPG];
char	Sysbase[6*NPTEPG*NBPG];
int	umbabeg;
struct	pte Nexmap[16][16];
char	nexus[16][16*NBPG];
struct	pte UMEMmap[4][16];
char	umem[4][16*NBPG];
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
char	CADDR1;
struct	pte CMAP2;
char	CADDR2;
struct	pte mmap[1];
char	vmmap[NBPG];
struct	pte bufmap[NBUF];
char	buffers[NBUF][BSIZE];
struct	pte msgbufmap[CLSIZE];
char	msgbuf[CLSIZE*NBPG];
struct	pte camap[32];
int	cabase;
char	caspace[32*NBPG];
int	calimit;

/*ARGSUSED*/
badaddr(addr, len) caddr_t addr; int len; { return (0); }

/*ARGSUSED*/
addupc(pc, prof, n) caddr_t pc; struct uprof *prof; { }

/*ARGSUSED*/
copyin(udaddr, kaddr, n) caddr_t udaddr, kaddr; unsigned n; { return (0); }

/*ARGSUSED*/
copyout(kaddr, udaddr, n) caddr_t kaddr, udaddr; unsigned n; { return (0); }

/*ARGSUSED*/
setjmp(lp) label_t lp; { return (0); }

/*ARGSUSED*/
longjmp(lp) label_t lp; { /*NOTREACHED*/ }

/*ARGSUSED*/
setrq(p) struct proc *p; { }

/*ARGSUSED*/
remrq(p) struct proc *p; { }

swtch() { }

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
copyseg(udaddr, pf) caddr_t udaddr; unsigned pf; { }

/*ARGSUSED*/
clearseg(pf) unsigned pf; { }

/*ARGSUSED*/
useracc(udaddr, bcnt, rw) caddr_t udaddr; unsigned bcnt; { return (0); }

/*ARGSUSED*/
kernacc(addr, bcnt, rw) caddr_t addr; unsigned bcnt; { return (0); }

/*VARARGS1*/
/*ARGSUSED*/
mtpr(reg, value) int reg, value; { }

/*ARGSUSED*/
mfpr(reg) int reg; { return (0); }


spl0() { return (0); }
spl1() { return (0); }
spl4() { return (0); }
spl5() { return (0); }
spl6() { return (0); }
spl7() { return (0); }

/*ARGSUSED*/
splx(s) int s; { }

/*ARGSUSED*/
bcopy(to, from, count) caddr_t to, from; unsigned count; { (* int *)mcrmap = 0; }
