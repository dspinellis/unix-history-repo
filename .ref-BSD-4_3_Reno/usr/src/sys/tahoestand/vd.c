/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vd.c	7.16 (Berkeley) 6/30/90
 */

/*
 * Stand alone driver for the VDDC/SMDE controller 
 */
#include "machine/mtpr.h"

#include "sys/param.h"
#include "sys/time.h"
#include "sys/buf.h"
#include "sys/disklabel.h"
#include "saio.h"

#include "tahoevba/vdreg.h"
#include "tahoevba/vbaparam.h"

#define	COMPAT_42	1

#define NVD		4		/* controllers */
#define	NDRIVE		8		/* drives per controller */

#define	VDADDR(ctlr)	((struct vddevice *)vdaddrs[ctlr])
long	vdaddrs[NVD] = { 0xffff2000, 0xffff2100, 0xffff2200, 0xffff2300 };

u_char	vdinit[NVD];			/* controller initialized */
u_char	vdtype[NVD];			/* controller type */
u_char	dkconfigured[NVD][NDRIVE];	/* unit configured */
u_char	dkflags[NVD][NDRIVE];		/* unit flags */

static	struct disklabel dklabel[NVD][NDRIVE];	/* pack label */
static	struct mdcb mdcb;
static	struct dcb dcb;
static	char lbuf[DEV_BSIZE];

vdopen(io)
	register struct iob *io;
{
	register int ctlr = io->i_ctlr;
	register struct dkinfo *dk;
	register struct disklabel *lp, *dlp;
	int error;

	if ((u_int)io->i_adapt)
		return (EADAPT);
	if ((u_int)ctlr >= NVD)
		return (ECTLR);
	if (!vdinit[ctlr] && (error = vdreset_ctlr(ctlr, io->i_unit)))
		return (error);
	lp = &dklabel[io->i_ctlr][io->i_unit];
	if (!dkconfigured[io->i_ctlr][io->i_unit]) {
		struct iob tio;

		/*
		 * Read in the pack label.
		 */
		lp->d_secsize = 1024;
		lp->d_nsectors = 72;
		lp->d_ntracks = 24;
		lp->d_ncylinders = 711;
		lp->d_secpercyl = 72*24;
		if (!vdreset_drive(io))
			return (ENXIO);
		tio = *io;
		tio.i_bn = LABELSECTOR;
		tio.i_ma = lbuf;
		tio.i_cc = DEV_BSIZE;
		tio.i_flgs |= F_RDDATA;
		if (vdstrategy(&tio, READ) != DEV_BSIZE)
			return (ERDLAB);
		dlp = (struct disklabel *)(lbuf + LABELOFFSET);
		if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC)
#ifdef COMPAT_42
		{
			printf("dk%d: unlabeled\n", io->i_unit);
			if (error = vdmaptype(io))
				return (error);
		}
#else
			return (EUNLAB);
#endif
		else {
			*lp = *dlp;
			if (!vdreset_drive(io))
				return (ENXIO);
		}
		dkconfigured[io->i_ctlr][io->i_unit] = 1;
	}
	if (io->i_part < 0 || io->i_part >= lp->d_npartitions ||
	    lp->d_partitions[io->i_part].p_size == 0)
		return (EPART);
	io->i_boff =
	    (lp->d_partitions[io->i_part].p_offset * lp->d_secsize) / DEV_BSIZE;
	return (0);
}

/*
 * Reset and initialize the controller.
 */
vdreset_ctlr(ctlr, unit)
	register int ctlr, unit;
{
	register int i;
	register struct vddevice *vdaddr = VDADDR(ctlr);

	if (badaddr(vdaddr, 2)) {
		printf("vd%d: %x: invalid csr\n", ctlr, vdaddr);
		return (ENXIO);
	}
	/* probe further to find what kind of controller it is */
	vdaddr->vdreset = 0xffffffff;
	DELAY(1000000);
	if (vdaddr->vdreset != 0xffffffff) {
		vdtype[ctlr] = VDTYPE_VDDC;
		DELAY(1000000);
	} else {
		vdtype[ctlr] = VDTYPE_SMDE;
		vdaddr->vdrstclr = 0;
		DELAY(3000000);
		vdaddr->vdcsr =  0;
		vdaddr->vdtcf_mdcb = AM_ENPDA;
		vdaddr->vdtcf_dcb = AM_ENPDA;
		vdaddr->vdtcf_trail = AM_ENPDA;
		vdaddr->vdtcf_data = AM_ENPDA;
		vdaddr->vdccf = CCF_SEN | CCF_DIU | CCF_STS |
		    XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	if (!vdcmd(ctlr, 0, VDOP_INIT, 10) ||
	    !vdcmd(ctlr, 0, VDOP_DIAG, 10)) {
		vderror(unit, dcb.opcode == VDOP_INIT ? "init" : "diag", &dcb);
		return (EIO);
	}
	vdinit[ctlr] = 1;
	for (i = NDRIVE - 1; i >= 0; i--)
		dkconfigured[ctlr][i] = 0;
	return (0);
}

/*
 * Reset and configure a drive's parameters.
 */
vdreset_drive(io)
	register struct iob *io;
{
	register int ctlr = io->i_ctlr, slave = io->i_unit;
	register struct disklabel *lp = &dklabel[io->i_ctlr][io->i_unit];
	register struct vddevice *vdaddr = VDADDR(ctlr);
	int pass = 0, type = vdtype[ctlr], error;
	int devflags = dkflags[ctlr][slave];		/* starts with 0 */

again:
	dcb.opcode = VDOP_CONFIG;		/* command */
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta = 0;
	dcb.devselect = slave | devflags;
	dcb.trail.rstrail.ncyl = lp->d_ncylinders;
	dcb.trail.rstrail.nsurfaces = lp->d_ntracks;
	if (type == VDTYPE_SMDE) {
		dcb.trailcnt = sizeof (struct treset) / sizeof (long);
		dcb.trail.rstrail.nsectors = lp->d_nsectors;
		dcb.trail.rstrail.slip_sec = lp->d_trackskew;
		dcb.trail.rstrail.recovery = VDRF_NORMAL;
	} else
		dcb.trailcnt = 2;	/* XXX */
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(vdaddr, (u_long)&mdcb, type);
	if (!vdpoll(vdaddr, &dcb, 10, type)) {
		if (pass++ != 0) {
			printf(" during drive configuration.\n");
			return (0);
		}
		VDRESET(vdaddr, type);
		if (error = vdreset_ctlr(ctlr, io->i_unit))
			return (error);
		goto again;
	}
	if ((dcb.operrsta & VDERR_HARD) == 0) {		/* success */
		dkflags[ctlr][slave] = devflags;
		return (1);
	}
	if (devflags == 0) {
		devflags = VD_ESDI;
		goto again;
	}
	if (type == VDTYPE_SMDE && (vdaddr->vdstatus[slave] & STA_US) == 0) {
		printf("dk%d: nonexistent drive\n", io->i_unit);
		return (0);
	}
	if ((dcb.operrsta & (DCBS_OCYL|DCBS_NRDY)) == 0) {
		vderror(io->i_unit, "config", &dcb);
		return (0);
	}
	devflags = 0;
	if (pass++)			/* give up */
		return (0);
	/*
	 * Try to spin up drive with remote command.
	 */
	if (!vdcmd(ctlr, 0, VDOP_START, 62)) {
		vderror(io->i_unit, "start", &dcb);
		return (0);
	}
	DELAY(62000000);
	goto again;
}

vdcmd(ctlr, unit, cmd, time)
	register int ctlr;
	int unit, cmd, time;
{
	register struct vddevice *vdaddr = VDADDR(ctlr);

	dcb.opcode = cmd;
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = unit | dkflags[ctlr][unit];
	dcb.trailcnt = 0;
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	VDGO(vdaddr, (u_long)&mdcb, vdtype[ctlr]);
	if (!vdpoll(vdaddr, &dcb, time, vdtype[ctlr]))
		_stop(" during initialization operation.\n");
	return ((dcb.operrsta & VDERR_HARD) == 0);
}

vdstrategy(io, cmd)
	register struct iob *io;
	int cmd;
{
	register struct disklabel *lp;
	int ctlr, cn, tn, sn, slave, retries = 0;
	daddr_t bn;
	struct vddevice *vdaddr;

	if (io->i_cc == 0 || io->i_cc > 65535) {
		printf("dk%d: invalid transfer size %d\n", io->i_unit,
		    io->i_cc);
		io->i_error = EIO;
		return (-1);
	}
	lp = &dklabel[io->i_ctlr][io->i_unit];
	bn = io->i_bn * (DEV_BSIZE / lp->d_secsize);
	cn = bn / lp->d_secpercyl;
	sn = bn % lp->d_secpercyl;
	tn = sn / lp->d_nsectors;
	sn = sn % lp->d_nsectors;

top:
	dcb.opcode = (cmd == READ ? VDOP_RD : VDOP_WD);
	dcb.intflg = DCBINT_NONE;
	dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	ctlr = io->i_ctlr;
	slave = io->i_unit;
	dcb.devselect = slave | dkflags[ctlr][slave];
	dcb.trailcnt = sizeof (struct trrw) / sizeof (int);
	dcb.trail.rwtrail.memadr = (u_long)io->i_ma; 
	dcb.trail.rwtrail.wcount = (io->i_cc + 1) / sizeof (short);
	dcb.trail.rwtrail.disk.cylinder = cn;
	dcb.trail.rwtrail.disk.track = tn;
	dcb.trail.rwtrail.disk.sector = sn;
	mdcb.mdcb_head = &dcb;
	mdcb.mdcb_status = 0;
	vdaddr = VDADDR(ctlr);
	VDGO(vdaddr, (u_long)&mdcb, vdtype[ctlr]);
	if (!vdpoll(vdaddr, &dcb, 60, vdtype[ctlr]))
		_stop(" during i/o operation.\n");
	if (dcb.operrsta & VDERR_HARD) {
		if (retries++ == 0 && vdreset_ctlr(ctlr, io->i_unit) == 0 &&
		    vdreset_drive(io))
			goto top;
		vderror(io->i_unit, cmd == READ ? "read" : "write", &dcb);
		io->i_error = EIO;
		return (-1);
	}
	mtpr(PADC, 0);
	return (io->i_cc);
}

vderror(unit, cmd, dcb)
	int unit;
	char *cmd;
	struct dcb *dcb;
{

	printf("dk%d: %s error; status %b", unit, cmd,
	    dcb->operrsta, VDERRBITS);
	if (dcb->err_code)
		printf(", code %x", dcb->err_code);
	printf("\n");
}

/*
 * Poll controller until operation
 * completes or timeout expires.
 */
vdpoll(vdaddr, dcb, t, type)
	register struct vddevice *vdaddr;
	register struct dcb *dcb;
	register int t, type;
{

	t *= 1000;
	for (;;) {
		uncache(&dcb->operrsta);
		if (dcb->operrsta & (DCBS_DONE|DCBS_ABORT))
			break;
		if (--t <= 0) {
			printf("vd: controller timeout");
			VDABORT(vdaddr, type);
			DELAY(30000);
			uncache(&dcb->operrsta);
			return (0);
		}
		DELAY(1000);
	}
	if (type == VDTYPE_SMDE) {
		for (;;) {
			uncache(&vdaddr->vdcsr);
			if ((vdaddr->vdcsr & CS_GO) == 0)
				break;
			DELAY(50);
		}
		DELAY(300);
		uncache(&dcb->err_code);
	}
	DELAY(200);
	uncache(&dcb->operrsta);
	return (1);
}

#ifdef COMPAT_42
struct	dkcompat {
	int	nsectors;		/* sectors per track */
	int	ntracks;		/* tracks per cylinder */
	int	ncylinders;		/* cylinders per drive */
	int	secsize;		/* sector size */
#define	NPART	2
	int	poff[NPART];		/* [a+b] for bootstrapping */
} dkcompat[] = {
	{ 64, 20,  842,  512, 0, 61440 },	/* 2361a eagle */
	{ 48, 24,  711,  512, 0, 61056 },	/* xsd */
	{ 44, 20,  842,  512, 0, 52800 },	/* eagle */
	{ 64, 10,  823,  512, 0, 38400 },	/* fuji 360 */
	{ 32, 24,  711,  512, 0, 40704 },	/* xfd */
	{ 32, 19,  823,  512, 0, 40128 },	/* smd */
	{ 32, 10,  823,  512, 0, 19200 },	/* fsd */
	{ 18, 15, 1224, 1024, 0, 21600 },	/* mxd */
};
#define	NDKCOMPAT	(sizeof (dkcompat) / sizeof (dkcompat[0]))

/*
 * Identify and configure drive from above table
 * by trying to read the last sector until a description
 * is found for which we're successful.
 */
vdmaptype(io)
	struct iob *io;
{
	register struct disklabel *lp = &dklabel[io->i_ctlr][io->i_unit];
	register struct dkcompat *dp;
	int i, ctlr, slave, type;
	struct vddevice *vdaddr;

	ctlr = io->i_ctlr;
	slave = io->i_unit;
	vdaddr = VDADDR(ctlr);
	type = vdtype[ctlr];
	for (dp = dkcompat; dp < &dkcompat[NDKCOMPAT]; dp++) {
		if (type == VDTYPE_VDDC && dp->nsectors != 32)
			continue;
		lp->d_nsectors = dp->nsectors;
		lp->d_ntracks = dp->ntracks;
		lp->d_ncylinders = dp->ncylinders;
		lp->d_secsize = dp->secsize;
		if (!vdreset_drive(io))		/* set drive parameters */
			return (EIO);
		dcb.opcode = VDOP_RD;
		dcb.intflg = DCBINT_NONE;
		dcb.nxtdcb = (struct dcb *)0;	/* end of chain */
		dcb.devselect = slave | dkflags[ctlr][slave];
		dcb.operrsta = 0;
		dcb.trailcnt = sizeof (struct trrw) / sizeof (long);
		dcb.trail.rwtrail.memadr = (u_long)lbuf; 
		dcb.trail.rwtrail.wcount = lp->d_secsize / sizeof (short);
		dcb.trail.rwtrail.disk.cylinder = dp->ncylinders - 2;
		dcb.trail.rwtrail.disk.track = dp->ntracks - 1;
		dcb.trail.rwtrail.disk.sector = dp->nsectors - 1;
		mdcb.mdcb_head = &dcb;
		mdcb.mdcb_status = 0;
		VDGO(vdaddr, (u_long)&mdcb, type);
		if (!vdpoll(vdaddr, &dcb, 60, type))
			_stop(" during i/o operation.\n");
		if (dcb.operrsta & VDERR_HARD)
			continue;
		/* simulate necessary parts of disk label */
		lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
		lp->d_secperunit = lp->d_secpercyl * lp->d_ncylinders;
		lp->d_npartitions = NPART;
		for (i = 0; i < NPART; i++) {
			lp->d_partitions[i].p_offset = dp->poff[i];
			lp->d_partitions[i].p_size =
			    lp->d_secperunit - dp->poff[i];
		}
		return (0);
	}
	printf("dk%d: unknown drive type\n", io->i_unit);
	return (ENXIO);
}
#endif
