/*	mem.c	4.5	82/10/13	*/

/*
 * Memory special file
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/uio.h"

#include "../vax/mtpr.h"

mmread(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (mmrw(dev, uio, UIO_READ));
}

mmwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	return (mmrw(dev, uio, UIO_WRITE));
}

mmrw(dev, uio, rw)
	dev_t dev;
	struct uio *uio;
	enum uio_rw rw;
{
	register int o;
	register unsigned c, v;
	register struct iovec *iov;
	int error = 0;

	while (uio->uio_resid > 0 && error == 0) {
		iov = uio->uio_iov;
		if (iov->iov_len == 0) {
			uio->uio_iov++;
			uio->uio_iovcnt--;
			if (uio->uio_iovcnt < 0)
				panic("mmrw");
			continue;
		}
		switch (minor(dev)) {

/* minor device 0 is physical memory */
		case 0:
			v = btop(uio->uio_offset);
			if (v >= physmem)
				goto fault;
			*(int *)mmap = v | (PG_V | PG_KR);
			mtpr(TBIS, vmmap);
			o = (int)uio->uio_offset & PGOFSET;
			c = min((unsigned)(NBPG - o), iov->iov_len);
			c = min(c, (unsigned)(NBPG - ((int)iov->iov_base&PGOFSET)));
			error = uiomove((caddr_t)&vmmap[o], c, rw, uio);
			continue;

/* minor device 1 is kernel memory */
		case 1:
			if ((caddr_t)uio->uio_offset < (caddr_t)&umbabeg &&
			    (caddr_t)uio->uio_offset + uio->uio_resid >= (caddr_t)&umbabeg)
				goto fault;
			if ((caddr_t)uio->uio_offset >= (caddr_t)&umbabeg &&
			    (caddr_t)uio->uio_offset < (caddr_t)&umbaend)
				goto fault;
			c = iov->iov_len;
			if (!kernacc((caddr_t)uio->uio_offset, c, rw == UIO_READ ? B_READ : B_WRITE))
				goto fault;
			error = uiomove((caddr_t)uio->uio_offset, c, rw, uio);
			continue;

/* minor device 2 is EOF/RATHOLE */
		case 2:
			if (rw == UIO_READ)
				return;
			c = iov->iov_len;
			break;

/* minor device 3 is unibus memory (addressed by shorts) */
		case 3:
			c = iov->iov_len;
			if (!kernacc((caddr_t)uio->uio_offset, c, rw == UIO_READ ? B_READ : B_WRITE))
				goto fault;
			if (!useracc(iov->iov_base, c, rw == UIO_READ ? B_WRITE : B_READ))
				goto fault;
			error = UNIcpy((caddr_t)uio->uio_offset, iov->iov_base,
			    c, rw);
			break;
		}
		if (error)
			break;
		iov->iov_base += c;
		iov->iov_len -= c;
		uio->uio_offset += c;
		uio->uio_resid -= c;
	}
	return (error);
fault:
	return (EFAULT);
}

/*
 * UNIBUS Address Space <--> User Space transfer
 */
UNIcpy(uniadd, usradd, cnt, rw)
	caddr_t uniadd, usradd;
	int cnt;
	enum uio_rw rw;
{
	register short *from, *to;
	register int i;
 
	if (rw == UIO_READ) {
		from = (short *)uniadd;
		to = (short *)usradd;
	} else {
		from = (short *)usradd;
		to = (short *)uniadd;
	}
	for (i = (cnt>>1); i > 0; i--)
		*to++ = *from++;
	return (0);
}
