/*	subr_xxx.c	4.16	82/08/14	*/

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
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/uio.h"

/*
 * Pass back  c  to the user at his location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * on the last character of the user's read.
 * u_base is in the user address space unless u_segflg is set.
 */
passc(c)
register c;
{
	register id;

	if ((id = u.u_segflg) == 1)
		*u.u_base = c;
	else
		if (id?suibyte(u.u_base, c):subyte(u.u_base, c) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return (u.u_count == 0? -1: 0);
}

#include "ct.h"
#if NCT > 0
/*
 * Pick up and return the next character from the user's
 * write call at location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * when u_count is exhausted.  u_base is in the user's
 * address space unless u_segflg is set.
 */
cpass()
{
	register c, id;

	if (u.u_count == 0)
		return (-1);
	if ((id = u.u_segflg) == 1)
		c = *u.u_base;
	else
		if ((c = id==0?fubyte(u.u_base):fuibyte(u.u_base)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return (c&0377);
}
#endif

/*
 * Routine which sets a user error; placed in
 * illegal entries in the bdevsw and cdevsw tables.
 */
nodev()
{

	u.u_error = ENODEV;
}

/*
 * Null routine; placed in insignificant entries
 * in the bdevsw and cdevsw tables.
 */
nulldev()
{

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
	unsigned int a, b;
{

	return (a < b ? a : b);
}

unsigned
max(a, b)
	unsigned int a, b;
{

	return (a > b ? a : b);
}

struct proc *
pfind(pid)
	int pid;
{
	register struct proc *p;

	for (p = &proc[pidhash[PIDHASH(pid)]]; p != &proc[0]; p = &proc[p->p_idhash])
		if (p->p_pid == pid)
			return (p);
	return ((struct proc *)0);
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

	while (--len)
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

copyuout(from, len, uio)
	caddr_t from;
	int len;
	register struct uio *uio;
{
	register struct iovec *iov = uio->uio_iov;
	int error = 0;
	int count;

	while (uio->uio_iovcnt > 0) {
		count = iov->iov_len;
		if (count > len)
			count = len;
		if (copyout(from, iov->iov_base, count)) {
			error = EFAULT;
			break;
		}
		iov->iov_base += len;
		from += count;
		uio->uio_resid -= count;
		iov->iov_len -= len;
		iov++;
		uio->uio_iovcnt--;
		if (iov->iov_len)
			break;
	}
	return (error);
}

/*
 * Pass back c to the user.
 */
passuc(c, uio)
	register c;
	struct uio *uio;
{
	register struct iovec *iov = uio->uio_iov;
	register id;

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
