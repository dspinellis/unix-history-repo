/*	Locore.c	3.2	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/tty.h"
#include "../h/cmap.h"

/*
 * Pseudo file for lint to show what is used/defined in locore.s.
 */

int	printsw;
int	coresw;
struct	cmap *cmap;
struct	cmap *ecmap;

lowinit()
{
	extern	int (*UNIvec[BSIZE/NBPW])();

	/*
	 * Pseudo-uses of globals.
	 */
	lowinit();
	intstack[0] = intstack[1];
	Sysmap[0] = Sysmap[1];
	maxmem = physmem = freemem = 0;
	/* should reference _u */
	main(0);

	/*
	 * Routines called from interrupt vectors.
	 */
	dump();
	hpintr(0, 0);
	htintr(0, 0);
	(*UNIvec[0])();
	printf("error!");
	cnrint(0);
	cnxint(0);
	clock((caddr_t)0, 0);
	if (runrun)
		;
	trap(0, 0, (unsigned)0, 0, 0);
	syscall(0, 0, (unsigned)0, 0, 0);
}

dzdma()
{

	dzxint((struct tty *)0);
}

/*ARGSUSED*/
addupc(pc, prof, n)
	caddr_t pc;
	struct uprof *prof;
{

}

/*ARGSUSED*/
fubyte(base)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
fuibyte(base)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
subyte(base, i)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
suibyte(base, i)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
fuword(base)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
fuiword(base)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
suword(base, i)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
suiword(base, i)
	caddr_t base;
{

	return (0);
}

/*ARGSUSED*/
copyin(udaddr, kaddr, n)
	caddr_t udaddr, kaddr;
	unsigned n;
{

	return (0);
}

/*ARGSUSED*/
copyout(kaddr, udaddr, n)
	caddr_t kaddr, udaddr;
	unsigned n;
{

	return (0);
}

int	whichqs;
struct {
	struct proc *q_forw;
	struct proc *q_rev;
} qs[32];

swtch()
{

	whichqs = 0;
}

/*ARGSUSED*/
resume(pcbpf)
	unsigned pcbpf;
{

	/*NOTREACHED*/
}

/*ARGSUSED*/
copyseg(udaddr, pf)
	caddr_t udaddr;
	unsigned pf;
{

}

/*ARGSUSED*/
clearseg(pf)
	unsigned pf;
{

}

/*ARGSUSED*/
useracc(udaddr, bcnt, rw)
	caddr_t udaddr;
	unsigned bcnt;
{

	return (0);
}

/*ARGSUSED*/
kernacc(addr, bcnt, rw)
	caddr_t addr;
	unsigned bcnt;
{

	return (0);
}

/*ARGSUSED*/
udiv(i, j)
	int i, j;
{

	return (0);
}

#ifdef	UNNEEDED
/*ARGSUSED*/
urem(i, j)
	unsigned i, j;
{

	return (0);
}
#endif

/*VARARGS1*/
/*ARGSUSED*/
mtpr(reg, value)
	int reg, value;
{

}

/*ARGSUSED*/
mfpr(reg)
	int reg;
{

	return (0);
}

struct	user u;
struct	user swaputl;
struct	user forkutl;
struct	user xswaputl;
struct	user xswap2utl;
struct	user pushutl;
struct	user vfutl;
struct	user pushutl;

struct	pte usrpt[USRPTSIZE*NPTEPG];

struct	pte Sysmap[6*NPTEPG];
struct	pte Swapmap[UPAGES];
struct	pte Forkmap[UPAGES];
struct	pte Xswapmap[UPAGES];
struct	pte Xswap2map[UPAGES];
struct	pte Pushmap[UPAGES];
struct	pte Vfmap[UPAGES];

struct	pte mmap[1];
struct	pte mcrmap[1];
char	vmmap[NBPG];
int	mcr[3];

struct	pte Usrptmap[USRPTSIZE];

char	buffers[NBUF][BSIZE];
