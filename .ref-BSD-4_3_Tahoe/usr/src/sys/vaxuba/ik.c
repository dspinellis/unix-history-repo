/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ik.c	7.1 (Berkeley) 4/25/88
 */

#include "ik.h"
#if NIK > 0
/*
 * Ikonas Frame Buffer Interface -- Bill Reeves.
 */
#include "../machine/pte.h"

#include "param.h"
#include "dir.h"
#include "user.h"
#include "buf.h"
#include "systm.h"
#include "map.h"
#include "uio.h"
#include "ioctl.h"

#include "ubareg.h"
#include "ubavar.h"
#include "ikreg.h"

#define IKBUSY 01
#define IKDMAPRI (PZERO-1)
#define IKWAITPRI (PZERO+1)

int	ikprobe(), ikattach(), ikintr();
struct	uba_device *ikdinfo[NIK];
u_short	ikstd[] = { 0772460, 0000000, 0 };
struct	uba_driver ikdriver =
	{ ikprobe, 0, ikattach, 0, ikstd, "ik", ikdinfo, 0, 0 };

struct ik_softc {
	char	ik_open;
	short	ik_uid;
	short	ik_state;
	int	ik_ubinfo;
	int	ik_count;
	struct	buf *ik_bp;
	int	ik_bufp;
	int	ik_icnt;
} ik_softc[NIK];

int	ikstrategy();
u_int	ikminphys();
struct	buf rikbuf[NIK];

#define IKUNIT(dev) (minor(dev))

ikprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* value-result */
	register struct ikdevice *ikaddr = (struct ikdevice *) reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	ikintr(0);
#endif
	ikaddr->ik_istat = 0;
	ikaddr->ik_xaddr = 0;
	ikaddr->ik_yaddr = 0;
	ikaddr->ik_ustat = IK_IENABLE | IK_GO;
	DELAY(10000);
	ikaddr->ik_ustat = 0;
	return (sizeof (struct ikdevice));
}

/*ARGSUSED*/
ikattach(ui)
	struct uba_device *ui;
{

}

ikopen(dev)
	dev_t dev;
{
	register struct ik_softc *ikp;
	register struct uba_device *ui;

	if (IKUNIT(dev) >= NIK || (ikp = &ik_softc[minor(dev)])->ik_open ||
	    (ui = ikdinfo[IKUNIT(dev)]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	ikp->ik_open = 1;
	ikp->ik_icnt = 0;
	ikp->ik_state = 0;
	ikp->ik_uid = u.u_uid;
	maptouser(ui->ui_addr);
	return (0);
}

ikclose(dev)
	dev_t dev;
{

	ik_softc[minor(dev)].ik_open = 0;
	ik_softc[minor(dev)].ik_state = 0;
	unmaptouser(ikdinfo[IKUNIT(dev)]->ui_addr);
}

ikread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = IKUNIT(dev);

	if (unit >= NIK)
		return (ENXIO);
	return (physio(ikstrategy, &rikbuf[unit], dev, B_READ, ikminphys, uio));
}

ikwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = IKUNIT(dev);

	if (unit >= NIK)
		return (ENXIO);
	return (physio(ikstrategy, &rikbuf[unit], dev, B_WRITE, ikminphys, uio));
}

u_int
ikminphys(bp)
	register struct buf *bp;
{

	if (bp->b_bcount > 65536)	/* may be able to do twice as much */
		bp->b_bcount = 65536;
}

ikstrategy(bp)
	register struct buf *bp;
{
	register struct ik_softc *ikp = &ik_softc[IKUNIT(bp->b_dev)];
	register struct uba_device *ui;

	if (IKUNIT(bp->b_dev) >= NIK || (ui = ikdinfo[IKUNIT(bp->b_dev)]) == 0
				|| ui->ui_alive == 0)
		goto bad;
	(void) spl5();
	while (ikp->ik_state & IKBUSY)
		sleep((caddr_t)ikp, IKDMAPRI+1);
	ikp->ik_state |= IKBUSY;
	ikp->ik_bp = bp;
	ikp->ik_ubinfo = ubasetup(ui->ui_ubanum, bp, UBA_NEEDBDP);
	ikp->ik_bufp = ikp->ik_ubinfo & 0x3ffff;
	ikp->ik_count = -(bp->b_bcount>>1);	/* its a word count */
	ikstart(ui);
	while (ikp->ik_state&IKBUSY)
		sleep((caddr_t)ikp, IKDMAPRI);
	ikp->ik_count = 0;
	ikp->ik_bufp = 0;
	(void) spl0();
	ubarelse(ui->ui_ubanum, &ikp->ik_ubinfo);
	ikp->ik_bp = 0;
	iodone(bp);
	wakeup((caddr_t)ikp);
	return;
bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

ikstart(ui)
	register struct uba_device *ui;
{
	register int istat;
	register struct ikdevice *ikaddr = (struct ikdevice *) ui->ui_addr;
	register struct ik_softc *ikp = &ik_softc[IKUNIT(ui->ui_unit)];

	istat = ikaddr->ik_istat|DMAENABLE;
	ikaddr->ik_istat = istat;
	ikaddr->ik_wc =  ikp->ik_count;
	ikaddr->ik_ubaddr = ikp->ik_bufp;
	ikaddr->ik_ustat = IK_GO|IK_IENABLE|((ikp->ik_bufp>>12)&060);
}

/*ARGSUSED*/
ikioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	register caddr_t data;
	int flag;
{
	register struct uba_device *ui = ikdinfo[IKUNIT(dev)];
	register struct ik_softc *ikp;

	switch (cmd) {

	case IKIOGETADDR:
		*(caddr_t *)data = ui->ui_addr;
		break;

	case IKIOWAITINT:
		ikp = &ik_softc[IKUNIT(dev)];
		ikp->ik_state |= IKBUSY;
		while (ikp->ik_state&IKBUSY)
			sleep((caddr_t)ikp, IKWAITPRI);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

/*ARGSUSED*/
ikintr(dev)
	dev_t dev;
{
	register struct ikdevice *ikaddr =
			(struct ikdevice *) ikdinfo[IKUNIT(dev)]->ui_addr;
	register struct ik_softc *ikp = &ik_softc[IKUNIT(dev)];

	ikp->ik_icnt++;
	if (ikp->ik_state&IKBUSY) {
		ikaddr->ik_ustat = 0;
		ikp->ik_state &= ~IKBUSY;
		wakeup((caddr_t)ikp);
	}
}

ikreset(uban)
	int uban;
{
	register int i;
	register struct uba_device *ui;
	register struct ik_softc *ikp = ik_softc;

	for (i = 0; i < NIK; i++, ikp++) {
		if ((ui = ikdinfo[i]) == 0 || ui->ui_alive == 0 ||
		    ui->ui_ubanum != uban || ikp->ik_open == 0)
			continue;
		printf(" ik%d", i);
		if ((ikp->ik_state&IKBUSY) == 0)
			continue;
		ikp->ik_ubinfo =
		    ubasetup(ui->ui_ubanum, ikp->ik_bp, UBA_NEEDBDP);
		ikp->ik_count = -(ikp->ik_bp->b_bcount/2);
		ikstart(ui);
	}
}
#endif
