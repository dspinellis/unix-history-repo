/*	vd.c	1.11	86/11/03	*/

#include "dk.h"
#if NVD > 0
/*
 * VDDC - Versabus SMD/SMDE driver.
 */
#ifdef VDDCPERF
#define	DOSCOPE
#endif

#include "param.h"
#include "buf.h"
#include "cmap.h"
#include "conf.h"
#include "dir.h"
#include "dkstat.h"
#include "map.h"
#include "systm.h"
#include "user.h"
#include "vmmac.h"
#include "proc.h"
#include "uio.h"

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#define	VDGENDATA
#include "../tahoevba/vdreg.h"
#undef VDGENDATA
#include "../tahoevba/scope.h"

#define	VDMAXIO		(MAXBPTE*NBPG)
#define	DUMPSIZE	64	/* controller limit */

#define VDUNIT(x)	(minor(x) >> 3)
#define FILSYS(x)	(minor(x) & 0x07)
#define PHYS(x)		(vtoph((struct proc *)0, (unsigned)(x)))

#define CTLR_ERROR	1
#define DRIVE_ERROR	2
#define HARD_DATA_ERROR	3
#define SOFT_DATA_ERROR	4
#define	WRITE_PROTECT	5

#define b_cylin	b_resid
#define b_daddr	b_error

struct	vba_ctlr *vdminfo[NVD];
struct  vba_device *vddinfo[NDK];
int	vdprobe(), vdslave(), vdattach(), vddgo();
struct	vba_driver vddriver =
    { vdprobe, vdslave, vdattach, vddgo, vddcaddr, "dk",
      vddinfo, "vd", vdminfo };

/*
 * Per-drive state.
 */
typedef struct {
	struct	buf raw_q_element;
	short	sec_per_blk;
	short	sec_per_cyl;
	char	status;
	struct	buf xfer_queue;
	int	drive_type;
	fs_tab	info;
} unit_tab;

/*
 * Per-controller state.
 */
typedef struct {
	char	ctlr_type;	/* controller type */
	struct	pte *map;	/* i/o page map */
	caddr_t	utl;		/* mapped i/o space */
	u_int	cur_slave:8;	/* last active unit number */
	u_int	int_expected:1;	/* expect an interrupt */
	u_int	ctlr_started:1;	/* start command was issued */
	u_int	overlap_seeks:1;/* should overlap seeks */
	u_int	initdone:1;	/* controller initialization completed */
	u_int	off_cylinder:16;/* off cylinder bit map */
	u_int	unit_type[16];	/* slave types */
	u_int	cur_cyl[16];	/* cylinder last selected */
	long	cur_trk[16];	/* track last selected */
	fmt_mdcb ctlr_mdcb;	/* controller mdcb */
	fmt_dcb	ctlr_dcb;	/* r/w dcb */
	fmt_dcb	seek_dcb[4];	/* dcbs for overlapped seeks */
	caddr_t	rawbuf;		/* buffer for raw+swap i/o */
} ctlr_tab;

ctlr_tab vdctlr_info[NVD];
unit_tab vdunit_info[NDK];

/*
 * See if the controller is really there; if so, initialize it.
 */
vdprobe(reg, vm)
	caddr_t reg;
	struct vba_ctlr *vm;
{
	register br, cvec;		/* must be r12, r11 */
	register cdr *addr = (cdr *)reg;
	register ctlr_tab *ci;
	int i;

	if (badaddr((caddr_t)reg, 2))
		return (0);
	ci = &vdctlr_info[vm->um_ctlr];
	addr->cdr_reset = 0xffffffff;
	DELAY(1000000);
	if (addr->cdr_reset != (unsigned)0xffffffff) {
		ci->ctlr_type = SMDCTLR;
		ci->overlap_seeks = 0;
		DELAY(1000000);
	} else {
		ci->overlap_seeks = 1;
		ci->ctlr_type = SMD_ECTLR;
		addr->cdr_reserved = 0x0;
		DELAY(3000000);
		addr->cdr_csr = 0;
		addr->mdcb_tcf = AM_ENPDA;
		addr->dcb_tcf = AM_ENPDA;
		addr->trail_tcf = AM_ENPDA;
		addr->data_tcf = AM_ENPDA;
		addr->cdr_ccf = CCF_SEN | CCF_DER | CCF_STS |
		    XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	/*
	 * Allocate page tables and i/o buffer.
	 */
	vbmapalloc(btoc(VDMAXIO)+1, &ci->map, &ci->utl);
	ci->rawbuf = calloc(VDMAXIO);
	/*
	 * Initialize all the drives to be of an unknown type.
	 */
	for (i = 0; i < 15; i++)
		ci->unit_type[i] = UNKNOWN;
	br = 0x17, cvec = 0xe0 + vm->um_ctlr;	/* XXX */
	return (sizeof (*addr));
}

/*
 * See if a drive is really there
 * Try to reset/configure the drive, then test its status.
 */
vdslave(vi, addr)
	register struct vba_device *vi;
	register cdr *addr;
{
	register ctlr_tab *ci = &vdctlr_info[vi->ui_ctlr];
	register unit_tab *ui = &vdunit_info[vi->ui_unit];
	register fmt_mdcb *mdcb = &ci->ctlr_mdcb;
	register fmt_dcb *dcb = &ci->ctlr_dcb;
	register int type;

	if (!ci->initdone) {
		printf("vd%d: %s controller\n", vi->ui_ctlr,
		    ci->ctlr_type == SMDCTLR ? "smd" : "smde");
		if (vdnotrailer(addr, vi->ui_ctlr, vi->ui_slave, INIT, 10) &
		    HRDERR) {
			printf("vd%d: init error\n", vi->ui_ctlr);
			return (0);
		}
		if (vdnotrailer(addr, vi->ui_ctlr, vi->ui_slave, DIAG, 10) &
		    HRDERR) {
			printf("vd%d: diagnostic error\n", vi->ui_ctlr);
			return (0);
		}
		ci->initdone = 1;
	}
	/*
	 * Seek on all drive types starting from the largest one.
	 * a successful seek to the last sector/cylinder/track verifies 
	 * the drive type connected to this port. 
	 */
	for (type = 0; type < nvddrv; type++) {	
		/* XXX */
		if (ci->ctlr_type == SMDCTLR && vdst[type].nsec != 32)
			continue;
		/* XXX */
		if (!vdconfigure_drive(addr, vi->ui_ctlr, vi->ui_slave, type,0))
			return (0);
		dcb->opcode = (short)RD;
		dcb->intflg = NOINT;
		dcb->nxtdcb = (fmt_dcb *)0;	/* end of chain */
		dcb->operrsta = 0;
		dcb->devselect = (char)(vi->ui_slave);
		dcb->trailcnt = (char)(sizeof (trrw) / sizeof (long));
		dcb->trail.rwtrail.memadr = (char *)PHYS(ci->rawbuf); 
		dcb->trail.rwtrail.wcount = vdst[type].secsize/sizeof(short);
		dcb->trail.rwtrail.disk.cylinder = vdst[type].ncyl - 2;
		dcb->trail.rwtrail.disk.track = vdst[type].ntrak - 1;
		dcb->trail.rwtrail.disk.sector = vdst[type].nsec - 1;
		mdcb->firstdcb = (fmt_dcb *)(PHYS(dcb));
		mdcb->vddcstat = 0;
		VDDC_ATTENTION(addr, (fmt_mdcb *)(PHYS(mdcb)), ci->ctlr_type);
		if (!vdpoll(ci, addr, 60))
			printf(" during probe\n");
		if ((dcb->operrsta&HRDERR) == 0)
			break;
	}
	if (type >= nvddrv) {
		/*
		 * If reached here, a drive which is not defined in the 
		 * 'vdst' tables is connected. Cannot set its type.
		 */
		printf("dk%d: unknown drive type\n", vi->ui_unit);
		return (0);
	}
	ui->drive_type = type;
	ui->info = vdst[type];
	ui->sec_per_blk = DEV_BSIZE / ui->info.secsize;
	vi->ui_type = type;
 	vi->ui_dk = 1;
	return (1);
}

vdconfigure_drive(addr, ctlr, slave, type, pass)
	register cdr *addr;
	int ctlr, slave, type, pass;
{
	register ctlr_tab *ci = &vdctlr_info[ctlr];

	ci->ctlr_dcb.opcode = RSTCFG;		/* command */
	ci->ctlr_dcb.intflg = NOINT;
	ci->ctlr_dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	ci->ctlr_dcb.operrsta = 0;
	ci->ctlr_dcb.devselect = (char)slave;
	ci->ctlr_dcb.trail.rstrail.ncyl = vdst[type].ncyl;
	ci->ctlr_dcb.trail.rstrail.nsurfaces = vdst[type].ntrak;
	if (ci->ctlr_type == SMD_ECTLR) {
		ci->ctlr_dcb.trailcnt = (char)5;
		ci->ctlr_dcb.trail.rstrail.nsectors = vdst[type].nsec;
		ci->ctlr_dcb.trail.rstrail.slip_sec = vdst[type].nslip;
		ci->ctlr_dcb.trail.rstrail.recovery = 0x18f;
	} else
		ci->ctlr_dcb.trailcnt = (char)2;
	ci->ctlr_mdcb.firstdcb = (fmt_dcb *)(PHYS(&ci->ctlr_dcb));
	ci->ctlr_mdcb.vddcstat = 0;
	VDDC_ATTENTION(addr, (fmt_mdcb *)(PHYS(&ci->ctlr_mdcb)), ci->ctlr_type);
	if (!vdpoll(ci, addr, 5)) {
		printf(" during config\n");
		return (0);
	}
	if (ci->ctlr_dcb.operrsta & HRDERR) {
		if ((ci->ctlr_dcb.operrsta & (NOTCYLERR|DRVNRDY)) == 0)
			printf("vd%d: drive %d: config error\n", ctlr, slave);
		else if (pass == 0) {
			vdstart_drive(addr, ctlr, slave);
			return (vdconfigure_drive(addr, ctlr, slave, type, 1));
		} else if (pass == 2)
			return (vdconfigure_drive(addr, ctlr, slave, type, 3));
		return (0);
	}
	return (1);
}

vdstart_drive(addr, ctlr, slave)
	cdr *addr;
	register int ctlr, slave;
{
	int error = 0;

	printf("vd%d: starting drive %d, wait...", ctlr, slave);
	if (vdctlr_info[ctlr].ctlr_started) {
#ifdef notdef
printf("DELAY(5500000)...");
		DELAY(5500000);
#endif
		goto done;
	}
	vdctlr_info[ctlr].ctlr_started = 1;
	error = vdnotrailer(addr, ctlr, 0, VDSTART, (slave*6)+62) & HRDERR;
	if (!error) {
#ifdef notdef
		DELAY((slave * 5500000) + 62000000);
#endif
	}
done:
	printf("\n");
	return (error == 0);
}

vdnotrailer(addr, ctlr, unit, function, time)
	register cdr *addr;
	int ctlr, unit, function, time;
{
	register ctlr_tab *ci = &vdctlr_info[ctlr];
	fmt_mdcb *mdcb = &ci->ctlr_mdcb;
	fmt_dcb *dcb = &ci->ctlr_dcb;

	dcb->opcode = function;		/* command */
	dcb->intflg = NOINT;
	dcb->nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb->operrsta = 0;
	dcb->devselect = (char)unit;
	dcb->trailcnt = (char)0;
	mdcb->firstdcb = (fmt_dcb *)(PHYS(dcb));
	mdcb->vddcstat = 0;
	VDDC_ATTENTION(addr, (fmt_mdcb *)(PHYS(mdcb)), ci->ctlr_type);
	if (!vdpoll(ci, addr, time)) {
		printf(" during init\n");
		return (DCBCMP|ANYERR|HRDERR|OPABRT);
	}
	return (dcb->operrsta);
}

vdattach(vi)
	register struct vba_device *vi;
{
	register unit_tab *ui = &vdunit_info[vi->ui_unit];
	register ctlr_tab *ci = &vdctlr_info[vi->ui_ctlr];
	register struct buf *cq = &vi->ui_mi->um_tab;
	register struct buf *uq = cq->b_forw;
	register struct buf *start_queue = uq;
	register fs_tab	*fs = &ui->info;

	ui->info = vdst[vi->ui_type];
	ui->sec_per_blk = DEV_BSIZE / ui->info.secsize;
	ui->sec_per_cyl = ui->info.nsec * ui->info.ntrak;
	ui->xfer_queue.b_dev = vi->ui_slave;
	ci->unit_type[vi->ui_slave] = vi->ui_type;
	/* load unit into controller's active unit list */
	if (uq == NULL) {
		cq->b_forw = &ui->xfer_queue;
		ui->xfer_queue.b_forw = &ui->xfer_queue;
		ui->xfer_queue.b_back = &ui->xfer_queue;
	} else {
		while (uq->b_forw != start_queue)
			uq = uq->b_forw;
		ui->xfer_queue.b_forw = start_queue;
		ui->xfer_queue.b_back = uq;
		uq->b_forw = &ui->xfer_queue;
		start_queue->b_back = &ui->xfer_queue;
	}
	printf("dk%d: %s <ntrak %d, ncyl %d, nsec %d>\n",
	    vi->ui_unit, fs->type_name,
	    ui->info.ntrak, ui->info.ncyl, ui->info.nsec);
	/*
	 * (60 / rpm) / (number of sectors per track * (bytes per sector / 2))
	 */
	dk_mspw[vi->ui_unit] = 120.0 / (fs->rpm * fs->nsec * fs->secsize);
}

/*ARGSUSED*/
vddgo(um)
	struct vba_ctlr *um;
{

}

vdstrategy(bp)
	register struct buf *bp;
{
	register int unit = VDUNIT(bp->b_dev);
	register struct vba_device *vi = vddinfo[unit];
	register par_tab *par;
	register unit_tab *ui;
	register fs_tab *fs;
	register int blks, bn, s;

	if (bp->b_bcount == 0 || vi == 0 || vi->ui_alive == 0)
		goto bad;
	ui = &vdunit_info[unit];
	fs = &ui->info;
	par = &fs->partition[FILSYS(bp->b_dev)];
	blks = (bp->b_bcount + DEV_BSIZE-1) >> DEV_BSHIFT;
	if (bp->b_blkno + blks >= par->par_len) {
		blks = par->par_len - bp->b_blkno;
		if (blks <= 0)
			goto bad;
		bp->b_bcount = blks * DEV_BSIZE;
	}
	bn = bp->b_blkno + par->par_start;
	bn *= ui->sec_per_blk;
	bp->b_daddr = (bn / fs->nsec) % fs->ntrak;
	bp->b_cylin = bn / ui->sec_per_cyl;
	vbasetup(bp, ui->info.secsize);
	s = spl7();
	if (ui->xfer_queue.av_forw == NULL) {
		register ctlr_tab *ci = &vdctlr_info[vi->ui_ctlr];
		int slave = vi->ui_slave;

		if (bp->b_cylin != ci->cur_cyl[slave] ||
		    bp->b_daddr != ci->cur_trk[slave])
			ci->off_cylinder |= 1 << slave;
	}
	bp->b_daddr |= (bn % fs->nsec) << 8;
	disksort(&ui->xfer_queue, bp);
	if (!vddinfo[unit]->ui_mi->um_tab.b_active++) {
		splx(s);
		vdstart(vddinfo[unit]->ui_mi);
	} else
		splx(s);
	return;
bad:	
	bp->b_flags |= B_ERROR, bp->b_error = ENXIO;
	bp->b_resid = bp->b_bcount;
	iodone(bp);
}

/*
 * Start up a transfer on a drive.
 */
vdstart(ci)
	register struct vba_ctlr *ci;
{
	register struct buf *cq = &ci->um_tab;
	register struct buf *uq = cq->b_forw;

	/* search for next ready unit */
	cq->b_forw = cq->b_forw->b_forw;
	uq = cq->b_forw;
	do {
		if (uq->av_forw != NULL) {
			cq->b_forw = uq;
			vdexecute(ci, uq);
			return;
		}
		uq = uq->b_forw;
	} while (uq != cq->b_forw);
}

/*
 * Initiate seeks for all drives off-cylinder.
 */
vdload_seeks(ci, uq)
	register ctlr_tab *ci;
	register struct buf *uq;
{
	register int unit, slave, nseeks;
	register fmt_dcb *dcb;
	register struct buf *bp;
	register struct buf *start_queue = uq;

	nseeks = 0;
	do {
		bp = uq->av_forw;
		if (bp != NULL) {
			unit = VDUNIT(bp->b_dev);
			slave = vddinfo[unit]->ui_slave;
			if (ci->off_cylinder & (1 << slave)) {
				ci->off_cylinder &= ~(1 << slave);
				if (ci->cur_cyl[slave] != bp->b_cylin) {
					ci->cur_cyl[slave] = bp->b_cylin;
					dk_seek[unit]++;
				}
				ci->cur_trk[slave] = bp->b_daddr&0xff;
				dcb = &ci->seek_dcb[nseeks++];
				dcb->opcode = SEEK;
				dcb->intflg = NOINT | INT_PBA;
				dcb->operrsta = 0;
				dcb->devselect = (char)slave;
				dcb->trailcnt = (char)1;
				dcb->trail.sktrail.skaddr.cylinder =
				    bp->b_cylin;
				dcb->trail.sktrail.skaddr.track =
				    bp->b_daddr & 0xff;
				dcb->trail.sktrail.skaddr.sector = 0;
			}
		}
		uq = uq->b_forw;
	} while (uq != start_queue && nseeks < 4);
	return (nseeks);
}

extern	vd_int_timeout();
/*
 * Execute the next command on the unit queue uq.
 */
vdexecute(controller_info, uq)
	register struct vba_ctlr *controller_info;
	register struct buf *uq;
{
	register struct	buf *bp = uq->av_forw;
	register int ctlr = controller_info->um_ctlr;
	register ctlr_tab *ci = &vdctlr_info[ctlr];
	register int unit = VDUNIT(bp->b_dev);
	register int slave = vddinfo[unit]->ui_slave;
	register fmt_mdcb *mdcb = &ci->ctlr_mdcb; 
	register fmt_dcb *dcb = &ci->ctlr_dcb; 
	
	/*
	 * If there are overlapped seeks to perform, shuffle
	 * them to the front of the queue and get them started
	 * before any data transfers (to get some parallelism).
	 */
	if ((ci->off_cylinder & ~(1<<slave)) && ci->overlap_seeks) {
		register int i, nseeks;
		
		/* setup seek requests in seek-q */
		nseeks = vdload_seeks(ci, uq);
		/* place at the front of the master q */
		mdcb->firstdcb = (fmt_dcb *)PHYS(&ci->seek_dcb[0]);
		/* shuffle any remaining seeks up in the seek-q */
		for (i = 1; i < nseeks; i++)
			ci->seek_dcb[i-1].nxtdcb = 
			    (fmt_dcb *)PHYS(&ci->seek_dcb[i]);
		ci->seek_dcb[nseeks-1].nxtdcb = (fmt_dcb *)PHYS(dcb);
	} else {
		if (bp->b_cylin != ci->cur_cyl[slave]) {
			ci->cur_cyl[slave] = bp->b_cylin;
			dk_seek[unit]++;
		}
		ci->cur_trk[slave] = bp->b_daddr & 0xff;
		ci->off_cylinder = 0;
		mdcb->firstdcb = (fmt_dcb *)(PHYS(dcb));
	}
	dcb->opcode = (bp->b_flags & B_READ) ? RD : WD;
	dcb->intflg = INTDONE;
	dcb->nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb->operrsta = 0;
	dcb->devselect = (char)slave;
	dcb->trailcnt = (char)(sizeof (trrw) / sizeof (long));
	dcb->trail.rwtrail.memadr = (char *)
	    vbastart(bp, ci->rawbuf, (long *)ci->map, ci->utl);
	dcb->trail.rwtrail.wcount = (short)((bp->b_bcount+1) / sizeof (short));
	dcb->trail.rwtrail.disk.cylinder = bp->b_cylin;
	dcb->trail.rwtrail.disk.track = bp->b_daddr & 0xff;
	dcb->trail.rwtrail.disk.sector = bp->b_daddr >> 8;
	mdcb->vddcstat = 0;
   	dk_wds[unit] += bp->b_bcount / 32;
	ci->int_expected = 1;
	timeout(vd_int_timeout, (caddr_t)ctlr, 20*60);
  	dk_busy |= 1 << unit;
	scope_out(1);
	VDDC_ATTENTION((cdr *)(vdminfo[ctlr]->um_addr),
	    (fmt_mdcb *)(PHYS(mdcb)), ci->ctlr_type);
}

/*
 * Watch for lost interrupts.
 */
vd_int_timeout(ctlr)
	register int ctlr;
{
	register ctlr_tab *ci = &vdctlr_info[ctlr];
	register fmt_dcb *dcb = &ci->ctlr_dcb;

	uncache(&dcb->operrsta);
	printf("vd%d: lost interrupt, status %b", ctlr, dcb->operrsta, ERRBITS);
	if (ci->ctlr_type == SMD_ECTLR) {
		uncache(&dcb->err_code);
		printf(", error code %x", dcb->err_code);
	}
	printf("\n");
	if ((dcb->operrsta&DCBCMP) == 0) { 
		VDDC_ABORT((cdr *)(vdminfo[ctlr]->um_addr), ci->ctlr_type);
		dcb->operrsta |= DCBUSC | DCBABT | ANYERR | HRDERR | CTLRERR;
	}
	vdintr(ctlr);
}

/*
 * Handle a disk interrupt.
 */
vdintr(ctlr)
	register int ctlr;
{
	register ctlr_tab *ci;
	register struct buf *cq, *uq, *bp;
	register int slave, unit;
	register fmt_mdcb  *mdcb;
	register fmt_dcb *dcb;
	int code, s;

	untimeout(vd_int_timeout, (caddr_t)ctlr);
	scope_out(2);
	ci = &vdctlr_info[ctlr];
	if (!ci->int_expected) {
		printf("vd%d: stray interrupt\n", ctlr);
		return;
	}
	/*
	 * Take first request off controller's queue.
	 */
	cq = &vdminfo[ctlr]->um_tab;
	uq = cq->b_forw;
	bp = uq->av_forw;
	unit = VDUNIT(bp->b_dev);
	dk_busy &= ~(1 << unit);
	dk_xfer[unit]++;
	ci->int_expected = 0;
	/* find associated control blocks */
	mdcb = &ci->ctlr_mdcb, uncache(&mdcb->intdcb);
	dcb = &ci->ctlr_dcb, uncache(&dcb->operrsta);
	if (ci->ctlr_type == SMD_ECTLR)
		uncache(&dcb->err_code);
	slave = uq->b_dev;
	switch (code = vddecode_error(dcb)) {

	case CTLR_ERROR:
	case DRIVE_ERROR:
		if (cq->b_errcnt >= 2)
			vdhard_error(ci, bp, dcb);
		if (code == CTLR_ERROR)
			vdreset_ctlr((cdr *)vdminfo[ctlr]->um_addr, ctlr);
		else
			reset_drive((cdr *)vdminfo[ctlr]->um_addr, ctlr,
			    slave, 2);
		if (cq->b_errcnt++ < 2) {	/* retry error */
			cq->b_forw = uq->b_back;
			vdstart(vdminfo[ctlr]);
			return;
		}
		bp->b_resid = bp->b_bcount;
		break;

	case HARD_DATA_ERROR:
	case WRITE_PROTECT:
		vdhard_error(ci, bp, dcb);
		bp->b_resid = 0;
		break;

	case SOFT_DATA_ERROR:
		vdsoft_error(ci, bp, dcb);
		/* fall thru... */

	default:			/* operation completed */
		bp->b_error = 0;
		bp->b_resid = 0;
		break;
	}
	vbadone(bp, ci->rawbuf, (long *)ci->map, ci->utl);
	/*
	 * Take next request on this unit q, or, if none,
	 * the next request on the next active unit q.
	 */
	s = spl7();
	uq->av_forw = bp->av_forw;
	if (uq->av_back != bp) {
		register struct buf *next;

		unit = VDUNIT(uq->av_forw->b_dev);
		slave = vddinfo[unit]->ui_slave;
		next = uq->av_forw;
		if (next->b_cylin != ci->cur_cyl[slave] ||
		    (next->b_daddr & 0xff) != ci->cur_trk[slave])
			ci->off_cylinder |= 1 << slave;
	} else
		uq->av_back = NULL;
	splx(s);
	/* reset controller state */
	cq->b_errcnt = 0;
	cq->b_active--;
	scope_out(3);
	if (bp->b_flags & B_ERROR)
		bp->b_error = EIO;
	iodone(bp);
	vdstart(vdminfo[ctlr]);
}

/*
 * Convert controller status to internal operation/error code.
 */
vddecode_error(dcb)
	register fmt_dcb *dcb;
{

	if (dcb->operrsta & HRDERR) {
		if (dcb->operrsta & WPTERR)
			return (WRITE_PROTECT);
		if (dcb->operrsta & (HCRCERR | HCMPERR | UCDATERR |
		    DSEEKERR | NOTCYLERR |DRVNRDY | INVDADR))
			return (DRIVE_ERROR);
		if (dcb->operrsta & (CTLRERR | OPABRT | INVCMD | DNEMEM))
			return (CTLR_ERROR);
		return (HARD_DATA_ERROR);
	}
	if (dcb->operrsta & SFTERR)
		return (SOFT_DATA_ERROR);
	return (0);
}

/*
 * Report a hard error.
 */
vdhard_error(ci, bp, dcb)
	ctlr_tab *ci; 
	register struct buf *bp;
	register fmt_dcb *dcb;
{
	unit_tab *ui = &vdunit_info[VDUNIT(bp->b_dev)];

	bp->b_flags |= B_ERROR;
	harderr(bp, ui->info.type_name);
	if (dcb->operrsta & WPTERR)
		printf("write protected");
	else {
		printf("status %b", dcb->operrsta, ERRBITS);
		if (ci->ctlr_type == SMD_ECTLR)
			printf(" ecode %x", dcb->err_code);
	}
	printf("\n");
}

/*
 * Report a soft error.
 */
vdsoft_error(ci, bp, dcb)
	ctlr_tab *ci; 
	register struct buf *bp;
	register fmt_dcb *dcb;
{
	unit_tab *ui = &vdunit_info[VDUNIT(bp->b_dev)];

	printf("dk%d%c: soft error sn%d status %b", minor(bp->b_dev) >> 3,
	    'a'+(minor(bp->b_dev)&07), bp->b_blkno, dcb->operrsta, ERRBITS);
	if (ci->ctlr_type == SMD_ECTLR)
		printf(" ecode %x", dcb->err_code);
	printf("\n");
}

/*ARGSUSED*/
vdopen(dev, flag)
	dev_t dev;
	int flag;
{
	register unit = VDUNIT(dev);
	register struct vba_device *vi = vddinfo[unit];

	if (vi == 0 || vi->ui_alive == 0 || vi->ui_type >= nvddrv)
		return (ENXIO);
	if (vdunit_info[unit].info.partition[FILSYS(dev)].par_len == 0)
		return (ENXIO);
	return (0);
}

vdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = VDUNIT(dev);
	register unit_tab *ui = &vdunit_info[unit];

	if (unit >= NDK)
		return (ENXIO);
	return (physio(vdstrategy, &ui->raw_q_element, dev, B_READ,
	    minphys, uio));
}

vdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = VDUNIT(dev);
	register unit_tab *ui = &vdunit_info[unit];

	if (unit >= NDK)
		return (ENXIO);
	return (physio(vdstrategy, &ui->raw_q_element, dev, B_WRITE,
	    minphys, uio));
}

/*
 * Crash dump.
 */
vddump(dev)
	dev_t dev;
{
	register int unit = VDUNIT(dev);
	register unit_tab *ui = &vdunit_info[unit];
	register fs_tab *fs = &ui->info;
	register int ctlr = vddinfo[unit]->ui_ctlr;
	register struct vba_ctlr *vba_vdctlr_info = vdminfo[ctlr];
	register int filsys = FILSYS(dev);
	register cdr *addr = (cdr *)(vba_vdctlr_info->um_addr);
	register int cur_blk, blkcount, blocks;
	caddr_t memaddr;

	vdreset_ctlr(addr, ctlr);
	blkcount = maxfree - 2;		/* In 1k byte pages */
	if (dumplo + blkcount > fs->partition[filsys].par_len) {
		blkcount = fs->partition[filsys].par_len - dumplo;
		printf("vd%d: Dump truncated to %dMB\n", unit, blkcount/1024);
	}
	cur_blk = fs->partition[filsys].par_start + dumplo;
	memaddr = 0;
	while (blkcount > 0) {
		blocks = MIN(blkcount, DUMPSIZE);
		if (!vdwrite_block(addr, ctlr, unit, memaddr, cur_blk, blocks))
			return (EIO);
		blkcount -= blocks;
		memaddr += blocks * NBPG;
		cur_blk += blocks;
	}
	return (0);
}

/*
 * Write a block to disk during a crash dump.
 */
vdwrite_block(caddr, ctlr, unit, addr, block, blocks)
	register cdr *caddr;
	register int ctlr, unit;
	register caddr_t addr;
	register int block, blocks;
{
	register ctlr_tab *ci = &vdctlr_info[ctlr];
	register fmt_mdcb *mdcb = &ci->ctlr_mdcb;
	register fmt_dcb *dcb = &ci->ctlr_dcb;
	register unit_tab *ui = &vdunit_info[unit];
	register fs_tab	 *fs = &ui->info;

	block *= (int)ui->sec_per_blk;
	blocks *= (int)ui->sec_per_blk;
	mdcb->firstdcb = (fmt_dcb *)(PHYS(dcb));
	dcb->intflg = NOINT;
	dcb->opcode = WD;
	dcb->operrsta = 0;
	dcb->devselect = (char)(vddinfo[unit])->ui_slave;
	dcb->trailcnt = (char)(sizeof (trrw) / sizeof (long));
	dcb->trail.rwtrail.memadr = addr;
	dcb->trail.rwtrail.wcount = (short)
	    ((blocks * fs->secsize)/ sizeof (short));
	dcb->trail.rwtrail.disk.cylinder = (short)(block / ui->sec_per_cyl);
	dcb->trail.rwtrail.disk.track = (char)((block / fs->nsec) % fs->ntrak);
	dcb->trail.rwtrail.disk.sector = (char)(block % fs->nsec);
	VDDC_ATTENTION(caddr, (fmt_mdcb *)(PHYS(mdcb)), ci->ctlr_type);
	if (!vdpoll(ci, caddr, 5)) {
		printf(" during dump\n");
		return (0);
	}
	if (dcb->operrsta & HRDERR) {
		printf("dk%d: hard error, status=%b\n", unit,
		    dcb->operrsta, ERRBITS);
		return (0);
	}
	return (1);
}

vdsize(dev)
	dev_t dev;
{
	struct vba_device *vi = vddinfo[VDUNIT(dev)];

	if (vi == 0 || vi->ui_alive == 0 || vi->ui_type >= nvddrv)
		return (-1);
	return (vdunit_info[VDUNIT(dev)].info.partition[FILSYS(dev)].par_len);
}

/*
 * Perform a controller reset.
 */
vdreset_ctlr(addr, ctlr)
	register cdr *addr;
	register int ctlr;
{
	register struct buf *cq = &vdminfo[ctlr]->um_tab;
	register struct buf *uq = cq->b_forw;
	register ctlr_tab *ci = &vdctlr_info[ctlr];
	
	VDDC_RESET(addr, ci->ctlr_type);
	ci->ctlr_started = 0;
	if (ci->ctlr_type == SMD_ECTLR) {
		addr->cdr_csr = 0;
		addr->mdcb_tcf = AM_ENPDA;
		addr->dcb_tcf = AM_ENPDA;
		addr->trail_tcf = AM_ENPDA;
		addr->data_tcf = AM_ENPDA;
		addr->cdr_ccf = CCF_STS | XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE | CCF_EDE | CCF_ECE | CCF_ERR;
	}
	if (vdnotrailer(addr, ctlr, 0, INIT, 10) & HRDERR) {
		printf("failed to init\n");
		return (0);
	}
	if (vdnotrailer(addr, ctlr, 0, DIAG, 10) & HRDERR) {
		printf("diagnostic error\n");
		return (0);
	}
	/*  reset all units attached to controller */
	uq = cq->b_forw;
	do {
		reset_drive(addr, ctlr, uq->b_dev, 0);
		uq = uq->b_forw;
	} while (uq != cq->b_forw);
	return (1);
}

/*
 * Perform a reset on a drive.
 */
reset_drive(addr, ctlr, slave, start)
	register cdr *addr;
	register int ctlr, slave, start;
{
	register int type = vdctlr_info[ctlr].unit_type[slave];

	if (type == UNKNOWN)
		return;
	if (!vdconfigure_drive(addr, ctlr, slave, type, start))
		printf("vd%d: drive %d: couldn't reset\n", ctlr, slave);
}

/*
 * Poll controller until operation completes
 * or timeout expires.
 */
vdpoll(ci, addr, t)
	register ctlr_tab *ci;
	register cdr *addr;
	register int t;
{
	register fmt_dcb *dcb = &ci->ctlr_dcb;

	t *= 1000;
	uncache(&dcb->operrsta);
	while ((dcb->operrsta&(DCBCMP|DCBABT)) == 0) {
		DELAY(1000);
		uncache(&dcb->operrsta);
		if (--t <= 0) {
			printf("vd%d: controller timeout", ci-vdctlr_info);
			VDDC_ABORT(addr, ci->ctlr_type);
			DELAY(30000);
			uncache(&dcb->operrsta);
			return (0);
		}
	}
	if (ci->ctlr_type == SMD_ECTLR) {
		uncache(&addr->cdr_csr);
		while (addr->cdr_csr&CS_GO) {
			DELAY(50);
			uncache(&addr->cdr_csr);
		}
		DELAY(300);
	}
	DELAY(200);
	uncache(&dcb->operrsta);
	return (1);
}

#ifdef notdef
/*
 * Dump the mdcb and DCB for diagnostic purposes.
 */
vdprintdcb(lp)
	register long *lp;
{
	register int i, dcb, tc;

	for (dcb = 0; lp; lp = (long *)(*lp), dcb++) {
		lp = (long *)((long)lp | 0xc0000000);
		printf("\nDump of dcb%d@%x:", dcb, lp);
		for (i = 0, tc = lp[3] & 0xff; i < tc+7; i++)
			printf(" %lx", lp[i]);
		printf("\n");
	}
	DELAY(1750000);
}
#endif
#endif
