/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)rx50.c	7.5 (Berkeley) 12/16/90
 */

#if VAX8200

/*
 * Routines to handle the console RX50.
 */

#include "sys/param.h"
#include "sys/time.h"
#include "sys/kernel.h"
#include "sys/vmmac.h"
#include "sys/buf.h"
#include "sys/errno.h"
#include "sys/uio.h"

#include "../include/cpu.h"
#include "rx50reg.h"

struct	rx50device rx50device;

#define	rx50unit(dev)	minor(dev)

struct rx50state {
	short	rs_flags;	/* see below */
	short	rs_drive;	/* current drive number */
	u_int	rs_blkno;	/* current block number */
} rx50state;

/* flags */
#define	RS_0OPEN	0x01	/* drive 0 open -- must be first */
#define	RS_1OPEN	0x02	/* drive 1 open -- must be second */
#define	RS_BUSY		0x04	/* operation in progress */
#define	RS_WANT		0x08	/* wakeup when done */
#define	RS_DONE		0x20	/* I/O operation done */
#define	RS_ERROR	0x40	/* error bit set at interrupt */

/*
 * Open a console RX50.
 */
/*ARGSUSED*/
rx50open(dev, flags)
	dev_t dev;
	int flags;
{
	int unit;

	/* only on 8200 (yet) */
	if (cpu != VAX_8200 || (unit = rx50unit(dev)) >= 2)
		return (ENXIO);

	/* enforce exclusive access */
	if (rx50state.rs_flags & (1 << unit))
		return (EBUSY);
	rx50state.rs_flags |= 1 << unit;
	return (0);
}

/*
 * Close a console RX50.
 */
/*ARGSUSED*/
rx50close(dev, flags)
	dev_t dev;
	int flags;
{

	rx50state.rs_flags &= ~(1 << dev);	/* atomic */
}

/*
 * Perform a read (uio->uio_rw==UIO_READ) or write (uio->uio_rw==UIO_WRITE).
 */
rx50rw(dev, uio, flags)
	dev_t dev;
	register struct uio *uio;
	int flags;
{
	register struct rx50device *rxaddr;
	register struct rx50state *rs;
	register char *cp;
	register int error, i, t;
	char secbuf[512];
	static char driveselect[2] = { RXCMD_DRIVE0, RXCMD_DRIVE1 };

	/* enforce whole-sector I/O */
	if ((uio->uio_offset & 511) || (uio->uio_resid & 511))
		return (EINVAL);

	rs = &rx50state;

	/* lock out others */
	i = spl4();
	while (rs->rs_flags & RS_BUSY) {
		rs->rs_flags |= RS_WANT;
		sleep((caddr_t) &rx50state, PZERO - 1);
	}
	rs->rs_flags |= RS_BUSY;
	rs->rs_drive = rx50unit(dev);
	splx(i);

	rxaddr = &rx50device;
	error = 0;

	while (uio->uio_resid) {
		rs->rs_blkno = uio->uio_offset >> 9;
		if (rs->rs_blkno >= RX50MAXSEC) {
			if (rs->rs_blkno > RX50MAXSEC)
				error = EINVAL;
			else if (uio->uio_rw == UIO_WRITE)
				error = ENOSPC;
			/* else ``eof'' */
			break;
		}
		rs->rs_flags &= ~(RS_ERROR | RS_DONE);
		if (uio->uio_rw == UIO_WRITE) {
			/* copy the data to the RX50 silo */
			error = uiomove(secbuf, 512, uio);
			if (error)
				break;
			i = rxaddr->rxrda;
			for (cp = secbuf, i = 512; --i >= 0;)
				rxaddr->rxfdb = *cp++;
			i = RXCMD_WRITE;
		} else
			i = RXCMD_READ;
		rxaddr->rxcmd = i | driveselect[rs->rs_drive];
		i = rs->rs_blkno - ((t = rs->rs_blkno / RX50SEC) * RX50SEC);
		rxaddr->rxtrk = t == 79 ? 0 : t + 1;
#ifdef notdef
		rxaddr->rxsec = "\1\3\5\7\11\1\3\5\7"[(2*t + i) % 5] + (i > 4);
#else
		rxaddr->rxsec = RX50SKEW(i, t);
#endif
		i = rxaddr->rxgo;	/* start it up */
		i = spl4();
		while ((rs->rs_flags & RS_DONE) == 0)
			sleep((caddr_t) &rs->rs_blkno, PRIBIO);
		splx(i);
		if (rs->rs_flags & RS_ERROR) {
			error = EIO;
			break;
		}
		if (uio->uio_rw == UIO_READ) {
			/* copy the data out of the silo */
			i = rxaddr->rxrda;
			for (cp = secbuf, i = 512; --i >= 0;)
				*cp++ = rxaddr->rxedb;
			error = uiomove(secbuf, 512, uio);
			if (error)
				break;
		}
	}

	/* let others in */
	rs->rs_flags &= ~RS_BUSY;
	if (rs->rs_flags & RS_WANT)
		wakeup((caddr_t) rs);

	return (error);
}

rx50intr()
{
	register struct rx50device *rxaddr = &rx50device;
	register struct rx50state *rs = &rx50state;
	int i;

#ifdef lint
	i = 0; i = i;
#endif

	/* ignore spurious interrupts */
	if ((rxaddr->rxcmd & RXCMD_DONE) == 0)
		return;
	if ((rs->rs_flags & RS_BUSY) == 0) {
		printf("stray rx50 interrupt ignored\n");
		return;
	}
	if (rxaddr->rxcmd & RXCMD_ERROR) {
		printf(
	"csa%d: hard error sn%d: cmd=%x trk=%x sec=%x csc=%x ict=%x ext=%x\n",
			rs->rs_drive + 1, rs->rs_blkno,
			rxaddr->rxcmd, rxaddr->rxtrk, rxaddr->rxsec,
			rxaddr->rxcsc, rxaddr->rxict, rxaddr->rxext);
		rxaddr->rxcmd = RXCMD_RESET;
		i = rxaddr->rxgo;
		rs->rs_flags |= RS_ERROR;
	}
	rs->rs_flags |= RS_DONE;
	wakeup((caddr_t) &rs->rs_blkno);
}
#endif
