/*	kdbparam.h	7.1	86/11/20	*/

#include <sys/vm.h>

#define DBNAME "kdb\n"
#define LPRMODE "%R"
#define OFFMODE "+%R"

#define	MAXINT	0x7fffffff
#define	MAXSTOR (KERNBASE - ctob(UPAGES))

#define BPT	0x30
#define KCALL	0xcf
#define CASEL	0xfc
#define TBIT	0x10

#define	SETBP(ins)	((BPT<<24) | ((ins) & 0xffffff))

#define ALIGN	-4

#define leng(a)		((long)((unsigned)(a)))
#define shorten(a)	(((a) >> 16) & 0xffff)
#define	itol(a,b)	(((a) << 16) | ((b) & 0xffff))
#define	byte(a)		(((a) >> 24) & 0xff)

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

#define	KERNBASE	0xc0000000
#define	KERNOFF		(KERNBASE + 0x800)	/* start of kernel's text */
#define	KVTOPH(x)	((x)&~ 0xc0000000)
