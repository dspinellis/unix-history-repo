/*	subr_xxx.c	4.21	82/12/17	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/fs.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/uio.h"

/*
 * Routine placed in illegal entries in the bdevsw and cdevsw tables.
 */
nodev()
{

	return (ENODEV);
}

/*
 * Null routine; placed in insignificant entries
 * in the bdevsw and cdevsw tables.
 */
nulldev()
{

	return (0);
}

imin(a, b)
{

	return (a < b ? a : b);
}

imax(a, b)
{

	return (a > b ? a : b);
}

unsigned
min(a, b)
	u_int a, b;
{

	return (a < b ? a : b);
}

unsigned
max(a, b)
	u_int a, b;
{

	return (a > b ? a : b);
}

extern	cabase, calimit;
extern	struct pte camap[];

caddr_t	cacur = (caddr_t)&cabase;
caddr_t	camax = (caddr_t)&cabase;
int	cax = 0;
/*
 * This is a kernel-mode storage allocator.
 * It is very primitive, currently, in that
 * there is no way to give space back.
 * It serves, for the time being, the needs of
 * auto-configuration code and the like which
 * need to allocate some stuff at boot time.
 */
caddr_t
calloc(size)
	int size;
{
	register caddr_t res;
	register int i;

	if (cacur+size >= (caddr_t)&calimit)
		panic("calloc");
	while (cacur+size > camax) {
		(void) vmemall(&camap[cax], CLSIZE, &proc[0], CSYS);
		vmaccess(&camap[cax], camax, CLSIZE);
		for (i = 0; i < CLSIZE; i++)
			clearseg(camap[cax++].pg_pfnum);
		camax += NBPG * CLSIZE;
	}
	res = cacur;
	cacur += size;
	return (res);
}

#ifndef vax
ffs(mask)
	register long mask;
{
	register int i;

	for(i=1; i<NSIG; i++) {
		if (mask & 1)
			return (i);
		mask >>= 1;
	}
	return (0);
}

bcmp(s1, s2, len)
	register char *s1, *s2;
	register int len;
{

	while (len--)
		if (*s1++ != *s2++)
			return (1);
	return (0);
}

strlen(s1)
	register char *s1;
{
	register int len;

	for (len = 0; *s1++ != '\0'; len++)
		/* void */;
	return (len);
}
#endif

/*
 * Pass back c to the user.
 */
passuc(c, uio)
	register c;
	struct uio *uio;
{
	register struct iovec *iov = uio->uio_iov;

	switch (uio->uio_segflg) {

	case 0:
		if (subyte(iov->iov_base, c) < 0)
			goto fault;
		break;

	case 1:
		*iov->iov_base = c;
		break;

	case 2:
		if (suibyte(iov->iov_base, c) < 0)
			goto fault;
		break;
	}
	iov->iov_base++;
	iov->iov_len--;
	uio->uio_resid--;
	uio->uio_offset++;
	if (iov->iov_len <= 0) {
		uio->uio_iov++;
		uio->uio_iovcnt--;
	}
	return (0);
fault:
	return (EFAULT);
}
