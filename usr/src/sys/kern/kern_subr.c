/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_subr.c	6.4 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "uio.h"

uiomove(cp, n, rw, uio)
	register caddr_t cp;
	register int n;
	enum uio_rw rw;
	register struct uio *uio;
{
	register struct iovec *iov;
	u_int cnt;
	int error = 0;

	while (n > 0 && uio->uio_resid) {
		iov = uio->uio_iov;
		cnt = iov->iov_len;
		if (cnt == 0) {
			uio->uio_iov++;
			uio->uio_iovcnt--;
			continue;
		}
		if (cnt > n)
			cnt = n;
		switch (uio->uio_segflg) {

		case UIO_USERSPACE:
		case UIO_USERISPACE:
			if (rw == UIO_READ)
				error = copyout(cp, iov->iov_base, cnt);
			else
				error = copyin(iov->iov_base, cp, cnt);
			if (error)
				return (error);
			break;

		case UIO_SYSSPACE:
			if (rw == UIO_READ)
				bcopy((caddr_t)cp, iov->iov_base, cnt);
			else
				bcopy(iov->iov_base, (caddr_t)cp, cnt);
			break;
		}
		iov->iov_base += cnt;
		iov->iov_len -= cnt;
		uio->uio_resid -= cnt;
		uio->uio_offset += cnt;
		cp += cnt;
		n -= cnt;
	}
	return (error);
}

/*
 * Give next character to user as result of read.
 */
ureadc(c, uio)
	register int c;
	register struct uio *uio;
{
	register struct iovec *iov;

again:
	if (uio->uio_iovcnt == 0)
		panic("ureadc");
	iov = uio->uio_iov;
	if (iov->iov_len <= 0 || uio->uio_resid <= 0) {
		uio->uio_iovcnt--;
		uio->uio_iov++;
		goto again;
	}
	switch (uio->uio_segflg) {

	case UIO_USERSPACE:
		if (subyte(iov->iov_base, c) < 0)
			return (EFAULT);
		break;

	case UIO_SYSSPACE:
		*iov->iov_base = c;
		break;

	case UIO_USERISPACE:
		if (suibyte(iov->iov_base, c) < 0)
			return (EFAULT);
		break;
	}
	iov->iov_base++;
	iov->iov_len--;
	uio->uio_resid--;
	uio->uio_offset++;
	return (0);
}

#ifdef notdef
/*
 * Get next character written in by user from uio.
 */
uwritec(uio)
	struct uio *uio;
{
	register struct iovec *iov;
	register int c;

again:
	if (uio->uio_iovcnt <= 0 || uio->uio_resid <= 0)
		panic("uwritec");
	iov = uio->uio_iov;
	if (iov->iov_len == 0) {
		uio->uio_iovcnt--;
		uio->uio_iov++;
		goto again;
	}
	switch (uio->uio_segflg) {

	case UIO_USERSPACE:
		c = fubyte(iov->iov_base);
		break;

	case UIO_SYSSPACE:
		c = *iov->iov_base & 0377;
		break;

	case UIO_USERISPACE:
		c = fuibyte(iov->iov_base);
		break;
	}
	if (c < 0)
		return (-1);
	iov->iov_base++;
	iov->iov_len--;
	uio->uio_resid--;
	uio->uio_offset++;
	return (c & 0377);
}
#endif
