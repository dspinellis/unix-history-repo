/*	machine.h	4.1	81/05/14	*/

#ifdef NEWVM
#include <machine/param.h>
#else
#include <sys/vm.h>
#include <machine/machparam.h>
#endif
#include <machine/vmparam.h>

#define	PAGSIZ		(NBPG*CLSIZE)

#define DBNAME "adb\n"
#define LPRMODE "%X"
#define OFFMODE "+%X"
#define	TXTRNDSIZ	PAGSIZ

#define	MAXINT	0x7fffffff
#define	MAXSTOR USRSTACK
#define	MAXFILE 0xffffffff
#define	LOWRAM	0xfffffdce

/*
 * INSTACK tells whether its argument is a stack address.
 * INUDOT tells whether its argument is in the (extended) u. area.
 * These are used for consistency checking and don't have to be exact.
 *
 * INKERNEL tells whether its argument is a kernel space address.
 * KVTOPH trims a kernel virtal address back to its offset
 * in the kernel address space.
 */
#define	INSTACK(x)	(((x)&0xf0000000) == 0xf0000000)
#define	INUDOT(x)	((x) >= kernudot && (x) < kernudot + ctob(UPAGES))
#define	INKERNEL(x) \
	((x) > KERNOFF && (x) < KERNOFF + ctob(slr))

#define	KVTOPH(x)	(((x) - KERNOFF) + (kmem ? lowram : 0))
#define	KERNOFF		(unsigned)KERNBASE

unsigned	lowram;
int		kernudot;
int		kmem;

#define mc68010		1	/* XXX */
#define mc68020		1	/* XXX */
