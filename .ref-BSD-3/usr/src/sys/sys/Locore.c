/*	Locore.c	2.1	1/5/80	*/

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
	Umap[0] = Umap[1];
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
	trap((caddr_t)0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 0, 15, 0);
}

int	waitloc;
int	ewaitloc;

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
copyin(uaddr, kaddr, n)
	caddr_t uaddr, kaddr;
	unsigned n;
{

	return (0);
}

/*ARGSUSED*/
copyout(kaddr, uaddr, n)
	caddr_t kaddr, uaddr;
	unsigned n;
{

	return (0);
}

idle()
{

}

/*ARGSUSED*/
save(svb)
	label_t svb;
{

	return (0);
}

/*ARGSUSED*/
resume(uaddr, svb)
	short uaddr[UPAGES];
	label_t svb;
{

	/*NOTREACHED*/
}

spl1()
{

	return (0);
}

spl4()
{

	return (0);
}

spl5()
{

	return (0);
}

spl6()
{

	return (0);
}

spl7()
{

	return (0);
}

spl0()
{

	return (0);
}

/*ARGSUSED*/
splx(s)
{

}

/*ARGSUSED*/
copyseg(uaddr, pf)
	caddr_t uaddr;
	unsigned pf;
{

}

/*ARGSUSED*/
clearseg(pf)
	unsigned pf;
{

}

/*ARGSUSED*/
useracc(uaddr, bcnt, rw)
	caddr_t uaddr;
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

struct	user u;
struct	user swaputl;
struct	user forkutl;
struct	user xswaputl;
struct	user xswap2utl;
struct	user pushutl;
struct	user vfutl;
struct	user pushutl;

struct	pte usrpt[USRPTSIZE*NPTEPG];

struct	pte Sysmap[4*NPTEPG];
struct	pte Umap[UPAGES];
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
