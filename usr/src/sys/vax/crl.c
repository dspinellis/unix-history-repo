/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)crl.c	7.3 (Berkeley) 2/17/90
 */
/*
 * TO DO (tef  7/18/85):
 *	1) change printf's to log() instead???
 */

#if VAX8600
#include "param.h"
#include "systm.h"
#include "conf.h"
#include "user.h"
#include "buf.h"

#include "cons.h"
#include "cpu.h"
#include "crl.h"
#include "mtpr.h"

struct {
	short	crl_state;		/* open and busy flags */
	short	crl_active;		/* driver state flag */
	struct	buf *crl_buf;		/* buffer we're using */
	ushort *crl_xaddr;		/* transfer address */
	short	crl_errcnt;
} crltab;

struct {
	int	crl_cs;		/* saved controller status */
	int	crl_ds;		/* saved drive status */
} crlstat;

/*ARGSUSED*/
crlopen(dev, flag)
	dev_t dev;
	int flag;
{
	struct buf *geteblk();

	if (cpu != VAX_8600)
		return (ENXIO);
	if (crltab.crl_state != CRL_IDLE)
		return (EALREADY);
	crltab.crl_state = CRL_OPEN;
	crltab.crl_buf = geteblk(512);
	return (0);
}

/*ARGSUSED*/
crlclose(dev, flag)
	dev_t dev;
	int flag;
{

	brelse(crltab.crl_buf);
	crltab.crl_state = CRL_IDLE;
}

/*ARGSUSED*/
crlrw(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct buf *bp;
	register int i;
	register int s;
	int error;

	if (uio->uio_resid == 0) 
		return (0);
	s = spl4();
	while (crltab.crl_state & CRL_BUSY)
		sleep((caddr_t)&crltab, PRIBIO);
	crltab.crl_state |= CRL_BUSY;
	splx(s);

	bp = crltab.crl_buf;
	error = 0;
	while ((i = imin(CRLBYSEC, uio->uio_resid)) > 0) {
		bp->b_blkno = uio->uio_offset>>9;
		if (bp->b_blkno >= MAXSEC || (uio->uio_offset & 0x1FF) != 0) {
			error = EIO;
			break;
		}
		if (uio->uio_rw == UIO_WRITE) {
			error = uiomove(bp->b_un.b_addr, i, uio);
			if (error)
				break;
		}
		bp->b_flags = uio->uio_rw == UIO_WRITE ? B_WRITE : B_READ;
		s = spl4(); 
		crlstart();
		while ((bp->b_flags & B_DONE) == 0)
			sleep((caddr_t)bp, PRIBIO);	
		splx(s);
		if (bp->b_flags & B_ERROR) {
			error = EIO;
			break;
		}
		if (uio->uio_rw == UIO_READ) {
			error = uiomove(bp->b_un.b_addr, i, uio);
			if (error)
				break;
		}
	}
	crltab.crl_state &= ~CRL_BUSY;
	wakeup((caddr_t)&crltab);
	return (error);
}

crlstart()
{
	register struct buf *bp;

	bp = crltab.crl_buf;
	crltab.crl_errcnt = 0;
	crltab.crl_xaddr = (ushort *) bp->b_un.b_addr;
	bp->b_resid = 0;

	if ((mfpr(STXCS) & STXCS_RDY) == 0)
		/* not ready to receive order */
		return;
	if ((bp->b_flags&(B_READ|B_WRITE)) == B_READ) {
		crltab.crl_active = CRL_F_READ;
		mtpr(STXCS, bp->b_blkno<<8 | STXCS_IE | CRL_F_READ);
	} else {
		crltab.crl_active = CRL_F_WRITE;
		mtpr(STXCS, bp->b_blkno<<8 | STXCS_IE | CRL_F_WRITE);
	}
#ifdef lint
	crlintr();
#endif
}

crlintr()
{
	register struct buf *bp;
	int i;

	bp = crltab.crl_buf;
	i = mfpr(STXCS);
	switch ((i>>24) & 0xFF) {

	case CRL_S_XCMPLT:
		switch (crltab.crl_active) {

		case CRL_F_RETSTS:
			crlstat.crl_ds = mfpr(STXDB);
			printf("crlcs=0x%b, crlds=0x%b\n", crlstat.crl_cs,
				CRLCS_BITS, crlstat.crl_ds, CRLDS_BITS); 
			break;

		case CRL_F_READ:
		case CRL_F_WRITE:
			bp->b_flags |= B_DONE;
		}
		crltab.crl_active = 0;
		wakeup((caddr_t)bp);
		break;

	case CRL_S_XCONT:
		switch (crltab.crl_active) {

		case CRL_F_WRITE:
			mtpr(STXDB, *crltab.crl_xaddr++);
			mtpr(STXCS, bp->b_blkno<<8 | STXCS_IE | CRL_F_WRITE);
			break;

		case CRL_F_READ:
			*crltab.crl_xaddr++ = mfpr(STXDB);
			mtpr(STXCS, bp->b_blkno<<8 | STXCS_IE | CRL_F_READ);
		}
		break;

	case CRL_S_ABORT:
		crltab.crl_active = CRL_F_RETSTS;
		mtpr(STXCS, STXCS_IE | CRL_F_RETSTS);
		bp->b_flags |= B_DONE|B_ERROR;
		break;

	case CRL_S_RETSTS:
		crlstat.crl_cs = mfpr(STXDB);
		mtpr(STXCS, STXCS_IE | CRL_S_RETSTS);
		break;

	case CRL_S_HNDSHK:
		printf("crl: hndshk error\n");	/* dump out some status too? */
		crltab.crl_active = 0;
		bp->b_flags |= B_DONE|B_ERROR;
		wakeup((caddr_t)bp);
		break;

	case CRL_S_HWERR:
		printf("crl: hard error sn%d\n", bp->b_blkno);
		crltab.crl_active = CRL_F_ABORT;
		mtpr(STXCS, STXCS_IE | CRL_F_ABORT);
		break;
	}
}
#endif
