/*	machine.h	1.2	87/06/25	*/

#include <sys/vm.h>

#define	PAGSIZ		(NBPG*CLSIZE)

#define DBNAME "adb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"
#define	TXTRNDSIZ	PAGSIZ

#define	MAXINT	0x7fffffff
#define	MAXSTOR (KERNBASE - ctob(UPAGES))
#define	MAXFILE 0xffffffff

/*
 * INSTACK tells whether its argument is a stack address.
 * INUDOT tells whether its argument is in the (extended) u. area.
 * These are used for consistency checking and dont have to be exact.
 *
 * INKERNEL tells whether its argument is a kernel space address.
 * KVTOPH trims a kernel virtal address back to its offset
 * in the kernel address space.
 */
#define	INSTACK(x)	(((int)(x)&0xf0000000) == 0xb0000000)
#define	INUDOT(x)	(((int)(x)&0xf0000000) == 0xb0000000)
#define	INKERNEL(x)	(((int)(x)&0xf0000000) == 0xc0000000)

#define	KERNOFF		(KERNBASE + 0x800)	/* start of kernel's text */
#define	KVTOPH(x)	((x)&~ 0xc0000000)
