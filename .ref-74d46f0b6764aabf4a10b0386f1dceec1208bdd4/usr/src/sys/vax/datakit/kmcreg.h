/*	@(#)kmc.h	1.1	*/
#define	KSTEP	1
#define	KMS	2
#define	KCSR	3
#define	KSTOP	4
#define	KMCLR	5
#define	KRUN	6
#define	KLU	7
#define	KWRCR	8
#define	KRESET	9

struct kmcntl {
	int	kmd;
	short	*kcsr;
	int	kval;
};

#include <sys/ioctl.h>

#define	KCSETA	_IOW('k', 1, struct kmcntl)

#define	lobyte(X)	(((unsigned char *)&X)[0])
#define	hibyte(X)	(((unsigned char *)&X)[1])
#define	loword(X)	(((ushort *)&X)[0])
#define	hiword(X)	(((ushort *)&X)[1])
