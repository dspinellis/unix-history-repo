/*	uda.c	4.5	82/05/27	*/

#include "ra.h"
#if NUDA > 0
/*
 * UDA50/RAxx disk device driver
 *
 * Restrictions:
 *	Unit numbers must be less than 8.
 *
 * TO DO:
 *	write dump code
 *	test on 750
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/dk.h"
#include "../h/cpu.h"
#include "../h/cmap.h"

int udadebug;
#define	printd	if(udadebug&1)printf

int udaerror = 0;	/* set to cause hex dump of error log packets */

/*
 * Parameters for the communications area
 */

#define	NRSPL2	3		/* log2 number of response packets */
#define	NCMDL2	3		/* log2 number of command packets */
#define	NRSP	(1<<NRSPL2)
#define	NCMD	(1<<NCMDL2)

#include "../h/udareg.h"
#include "../h/mscp.h"

struct uda_softc {
	short	sc_state;	/* state of controller */
	short	sc_mapped;	/* Unibus map allocated for uda struct? */
	int	sc_ubainfo;	/* Unibus mapping info */
	struct uda *sc_uda;	/* Unibus address of uda struct */
	int	sc_ivec;	/* interrupt vector address */
	short	sc_credits;	/* transfer credits */
	short	sc_lastcmd;	/* pointer into command ring */
	short	sc_lastrsp;	/* pointer into response ring */
} uda_softc[NUDA];

/*
 * Controller states
 */
#define	S_IDLE	0		/* hasn't been initialized */
#define	S_STEP1	1		/* doing step 1 init */
#define	S_STEP2	2		/* doing step 2 init */
#define	S_STEP3	3		/* doing step 3 init */
#define	S_SCHAR	4		/* doing "set controller characteristics" */
#define	S_RUN	5		/* running */

struct uda {
	struct udaca	uda_ca;		/* communications area */
	struct mscp	uda_rsp[NRSP];	/* response packets */
	struct mscp	uda_cmd[NCMD];	/* command packets */
} uda[NUDA];

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
struct size {
	daddr_t	nblocks;
	daddr_t	blkoff;
} ra_sizes[8] ={
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	15884,	340670,		/* D=blk 340670 thru 356553 */
	55936,	356554,		/* E=blk 356554 thru 412489 */
	-1,	412490,		/* F=blk 412490 thru end */
	82080,	49324,		/* G=blk 49324 thru 131403 */
	-1,	131404,		/* H=blk 131404 thru end */
};
/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */

daddr_t	radsize[NRA];			/* disk size, from ONLINE end packet */

int	udprobe(), udslave(), udattach(), udintr();
struct	mscp *udgetcp();
struct	uba_ctlr *udminfo[NUDA];
struct	uba_device *uddinfo[NRA];
struct	uba_device *udip[NUDA][8];	/* 8 == max number of drives */

u_short	udstd[] = { 0772150, 0 };
struct	uba_driver udadriver =
 { udprobe, udslave, udattach, 0, udstd, "ra", uddinfo, "uda", udminfo, 0 };
struct	buf rudbuf[NRA];
struct	buf udutab[NRA];
struct	buf udwtab[NUDA];		/* I/O wait queue, per controller */

#define	b_qsize		b_resid		/* queue size per drive, in udutab */
#define	b_ubinfo	b_resid		/* Unibus mapping info, per buffer */

udprobe(reg, ctlr)
	caddr_t reg;
	int ctlr;
{
	register int br, cvec;
	register struct uda_softc *sc = &uda_softc[ctlr];

#ifdef lint
	br = 0; cvec = br; br = cvec; reg = reg;
	udread(0); udwrite(0); udreset(0); udintr(0);
#endif
	/* SHOULD CHECK THAT IT REALLY IS A UDA */
	br = 0x15;
	cvec = sc->sc_ivec = (uba_hd[numuba].uh_lastiv -= 4);
	return(1);
}

udslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	/*
	 * TOO HARD TO FIND OUT IF DISK IS THERE UNTIL
	 * INITIALIZED.  WE'LL FIND OUT WHEN WE FIRST
	 * TRY TO ACCESS IT.
	 */
#ifdef lint
	ui = ui; reg = reg;
#endif
	return(1);
}

udattach(ui)
	register struct uba_device *ui;
{

	if (ui->ui_dk > 0)
		dk_mspw[ui->ui_dk] = 1.0 / (60 * 31 * 256);	/* approx */
	ui->ui_flags = 0;
	udip[ui->ui_ctlr][ui->ui_slave] = ui;
	radsize[ui->ui_unit] = (daddr_t)0xffffff;	/* max possible size */
}

/*
 * Open a UDA.  Initialize the device and
 * set the unit online.
 */
udopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct uba_device *ui;
	register struct uda_softc *sc;
	int s;

#ifdef lint
	flag = flag;
#endif
	unit = minor(dev) >> 3;
	if (unit >= NRA || (ui = uddinfo[unit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	sc = &uda_softc[ui->ui_ctlr];
	s = spl5();
	if (sc->sc_state != S_RUN) {
		if (sc->sc_state == S_IDLE)
			udinit(ui->ui_ctlr);
		/* wait for initialization to complete */
		sleep((caddr_t)ui->ui_mi, 0);
		if (sc->sc_state != S_RUN) {
			u.u_error = EIO;
			return;
		}
	}
	splx(s);
	/* SHOULD PROBABLY FORCE AN ONLINE ATTEMPT
	   TO SEE IF DISK IS REALLY THERE */
}

/*
 * Initialize a UDA.  Set up UBA mapping registers,
 * initialize data structures, and start hardware
 * initialization sequence.
 */
udinit(d)
	int d;
{
	register struct uda_softc *sc;
	register struct uda *ud;
	struct udadevice *udaddr;
	struct uba_ctlr *um;

	sc = &uda_softc[d];
	um = udminfo[d];
	um->um_tab.b_active++;
	ud = &uda[d];
	udaddr = (struct udadevice *)um->um_addr;
	if (sc->sc_mapped == 0) {
		/*
		 * Map the communications area and command
		 * and response packets into Unibus address
		 * space.
		 */
		sc->sc_ubainfo = uballoc(um->um_ubanum, (caddr_t)ud,
		    sizeof (struct uda), 0);
		sc->sc_uda = (struct uda *)(sc->sc_ubainfo & 0x3ffff);
		sc->sc_mapped = 1;
	}

	/*
	 * Start the hardware initialization sequence.
	 */
	udaddr->udaip = 0;		/* start initialization */
	while ((udaddr->udasa & UDA_STEP1) == 0)
		;
	udaddr->udasa = UDA_ERR|(NCMDL2<<11)|(NRSPL2<<8)|UDA_IE|(sc->sc_ivec/4);
	/*
	 * Initialization continues in interrupt routine.
	 */
	sc->sc_state = S_STEP1;
	sc->sc_credits = 0;
}

udstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	register struct buf *dp;
	register int unit;
	int xunit = minor(bp->b_dev) & 07;
	daddr_t sz, maxsz;
	int s;

	sz = (bp->b_bcount+511) >> 9;
	unit = dkunit(bp);
	if (unit >= NRA)
		goto bad;
	ui = uddinfo[unit];
	um = ui->ui_mi;
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	if ((maxsz = ra_sizes[xunit].nblocks) < 0)
		maxsz = radsize[unit] - ra_sizes[xunit].blkoff;
	if (bp->b_blkno < 0 || bp->b_blkno+sz > maxsz ||
	    ra_sizes[xunit].blkoff >= radsize[unit])
		goto bad;
	s = spl5();
	/*
	 * Link the buffer onto the drive queue
	 */
	dp = &udutab[ui->ui_unit];
	if (dp->b_actf == 0)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	bp->av_forw = 0;
	/*
	 * Link the drive onto the controller queue
	 */
	if (dp->b_active == 0) {
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
		dp->b_active = 1;
	}
	if (um->um_tab.b_active == 0) {
#if defined(VAX750)
		if (cpu == VAX_750) {
			if (um->um_ubinfo != 0)
				printf("uda: ubinfo %x\n",um->um_ubinfo);
			else
				um->um_ubinfo =
				uballoc(um->um_ubanum, (caddr_t)0, 0,
					UBA_NEEDBDP);
		}
#endif
		(void) udstart(um);
	}
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

udstart(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct mscp *mp;
	register struct uda_softc *sc;
	register struct uba_device *ui;
	struct udadevice *udaddr;
	int i;

	sc = &uda_softc[um->um_ctlr];
	
loop:
	if ((dp = um->um_tab.b_actf) == NULL) {
		/*
		 * Release uneeded UBA resources and return
		 */
		um->um_tab.b_active = 0;
#if defined(VAX750)
		if (cpu == VAX_750) {
			if (um->um_ubinfo == 0)
				printf("uda: um_ubinfo == 0\n");
			else
				ubarelse(um->um_ubanum, &um->um_ubinfo);
		}
#endif
		return (0);
	}
	if ((bp = dp->b_actf) == NULL) {
		/*
		 * No more requests for this drive, remove
		 * from controller queue and look at next drive.
		 * We know we're at the head of the controller queue.
		 */
		dp->b_active = 0;
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	um->um_tab.b_active++;
	udaddr = (struct udadevice *)um->um_addr;
	if ((udaddr->udasa&UDA_ERR) || sc->sc_state != S_RUN) {
		harderr(bp, "ra");
		printf("udasa %o, state %d\n", udaddr->udasa&0xffff, sc->sc_state);
		udinit(um->um_ctlr);
		/* SHOULD REQUEUE OUTSTANDING REQUESTS, LIKE UDRESET */
		return (0);
	}
	ui = uddinfo[dkunit(bp)];
	/*
	 * If no credits, can't issue any commands
	 * until some outstanding commands complete.
	 */
	if (sc->sc_credits < 2)
		return (0);
	if ((mp = udgetcp(um)) == NULL)
		return (0);
	sc->sc_credits--;	/* committed to issuing a command */
	if (ui->ui_flags == 0) {	/* not online */
		mp->mscp_opcode = M_OP_ONLIN;
		mp->mscp_unit = ui->ui_slave;
		dp->b_active = 2;
		um->um_tab.b_actf = dp->b_forw;	/* remove from controller q */
		printd("uda: bring unit %d online\n", ui->ui_slave);
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		i = udaddr->udaip;
		goto loop;
	}
	switch (cpu) {
	case VAX_780:
		i = UBA_NEEDBDP|UBA_CANTWAIT;
		break;

	case VAX_750:
		i = um->um_ubinfo|UBA_HAVEBDP|UBA_CANTWAIT;
		break;

	case VAX_730:
		i = UBA_CANTWAIT;
		break;
	}
	if ((i = ubasetup(um->um_ubanum, bp, i)) == 0) {
		mp->mscp_opcode = M_OP_GTUNT;
		mp->mscp_unit = ui->ui_slave;
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		i = udaddr->udaip;	/* initiate polling */
		return(1);		/* wait for interrupt */
	}
	mp->mscp_cmdref = (long)bp;	/* pointer to get back */
	mp->mscp_opcode = bp->b_flags&B_READ ? M_OP_READ : M_OP_WRITE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_lbn = bp->b_blkno + ra_sizes[minor(bp->b_dev)&7].blkoff;
	mp->mscp_bytecnt = bp->b_bcount;
	mp->mscp_buffer = (i & 0x3ffff) | (((i>>28)&0xf)<<24);
#if defined(VAX750)
	if (cpu == VAX_750)
		i &= 0xfffffff;		/* mask off bdp */
#endif
	bp->b_ubinfo = i;		/* save mapping info */
	*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
	i = udaddr->udaip;		/* initiate polling */
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
		dp->b_qsize++;
		dk_xfer[ui->ui_dk]++;
		dk_wds[ui->ui_dk] += bp->b_bcount>>6;
	}

	/*
	 * Move drive to the end of the controller queue
	 */
	if (dp->b_forw != NULL) {
		um->um_tab.b_actf = dp->b_forw;
		um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
		dp->b_forw = NULL;
	}
	/*
	 * Move buffer to I/O wait queue
	 */
	dp->b_actf = bp->av_forw;
	dp = &udwtab[um->um_ctlr];
	bp->av_forw = dp;
	bp->av_back = dp->av_back;
	dp->av_back->av_forw = bp;
	dp->av_back = bp;
	goto loop;
}

/*
 * UDA interrupt routine.
 */
udintr(d)
	int d;
{
	register struct uba_ctlr *um = udminfo[d];
	register struct udadevice *udaddr = (struct udadevice *)um->um_addr;
	struct buf *bp;
	register int i;
	register struct uda_softc *sc = &uda_softc[d];
	register struct uda *ud = &uda[d];
	struct uda *uud;
	struct mscp *mp;

	printd("udintr: state %d, udasa %o\n", sc->sc_state, udaddr->udasa);
	switch (sc->sc_state) {
	case S_IDLE:
		printf("uda%d: random interrupt ignored\n", d);
		return;

	case S_STEP1:
#define	STEP1MASK	0174377
#define	STEP1GOOD	(UDA_STEP2|UDA_IE|(NCMDL2<<3)|NRSPL2)
		if ((udaddr->udasa&STEP1MASK) != STEP1GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udaddr->udasa = ((int)&sc->sc_uda->uda_ca.ca_ringbase)|
		    (cpu == VAX_780 ? UDA_PI : 0);
		sc->sc_state = S_STEP2;
		return;

	case S_STEP2:
#define	STEP2MASK	0174377
#define	STEP2GOOD	(UDA_STEP3|UDA_IE|(sc->sc_ivec/4))
		if ((udaddr->udasa&STEP2MASK) != STEP2GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udaddr->udasa = ((int)&sc->sc_uda->uda_ca.ca_ringbase)>>16;
		sc->sc_state = S_STEP3;
		return;

	case S_STEP3:
#define	STEP3MASK	0174000
#define	STEP3GOOD	UDA_STEP4
		if ((udaddr->udasa&STEP3MASK) != STEP3GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udaddr->udasa = UDA_GO;
		sc->sc_state = S_SCHAR;

		/*
		 * Initialize the data structures.
		 */
		uud = sc->sc_uda;
		for (i = 0; i < NRSP; i++) {
			ud->uda_ca.ca_rspdsc[i] = UDA_OWN|UDA_INT|
				(long)&uud->uda_rsp[i].mscp_cmdref;
			ud->uda_rsp[i].mscp_dscptr = &ud->uda_ca.ca_rspdsc[i];
			ud->uda_rsp[i].mscp_header.uda_msglen = sizeof (struct mscp);
		}
		for (i = 0; i < NCMD; i++) {
			ud->uda_ca.ca_cmddsc[i] = UDA_INT|
				(long)&uud->uda_cmd[i].mscp_cmdref;
			ud->uda_cmd[i].mscp_dscptr = &ud->uda_ca.ca_cmddsc[i];
			ud->uda_cmd[i].mscp_header.uda_msglen = sizeof (struct mscp);
		}
		bp = &udwtab[d];
		bp->av_forw = bp->av_back = bp;
		sc->sc_lastcmd = 0;
		sc->sc_lastrsp = 0;
		if ((mp = udgetcp(um)) == NULL) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		mp->mscp_opcode = M_OP_STCON;
		mp->mscp_cntflgs = M_CF_ATTN|M_CF_MISC|M_CF_THIS;
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		i = udaddr->udaip;	/* initiate polling */
		return;

	case S_SCHAR:
	case S_RUN:
		break;

	default:
		printf("uda%d: interrupt in unknown state %d ignored\n",
			d, sc->sc_state);
		return;
	}

	if (udaddr->udasa&UDA_ERR) {
		printf("uda%d: fatal error (%o)\n", d, udaddr->udasa&0xffff);
		udaddr->udaip = 0;
		wakeup((caddr_t)um);
	}

	/*
	 * Check for a buffer purge request.
	 */
	if (ud->uda_ca.ca_bdp) {
		/*
		 * THIS IS A KLUDGE.
		 * Maybe we should change the entire
		 * UBA interface structure.
		 */
		int s = spl7();

		i = um->um_ubinfo;
		printd("uda: purge bdp %d\n", ud->uda_ca.ca_bdp);
		um->um_ubinfo = ud->uda_ca.ca_bdp<<28;
		ubapurge(um);
		um->um_ubinfo = i;
		(void) splx(s);
		ud->uda_ca.ca_bdp = 0;
		udaddr->udasa = 0;	/* signal purge complete */
	}

	/*
	 * Check for response ring transition.
	 */
	if (ud->uda_ca.ca_rspint) {
		ud->uda_ca.ca_rspint = 0;
		for (i = sc->sc_lastrsp;; i++) {
			i %= NRSP;
			if (ud->uda_ca.ca_rspdsc[i]&UDA_OWN)
				break;
			udrsp(um, ud, sc, i);
			ud->uda_ca.ca_rspdsc[i] |= UDA_OWN;
		}
		sc->sc_lastrsp = i;
	}

	/*
	 * Check for command ring transition.
	 */
	if (ud->uda_ca.ca_cmdint) {
		printd("uda: command ring transition\n");
		ud->uda_ca.ca_cmdint = 0;
	}
	(void) udstart(um);
}

/*
 * Process a response packet
 */
udrsp(um, ud, sc, i)
	register struct uba_ctlr *um;
	register struct uda *ud;
	register struct uda_softc *sc;
	int i;
{
	register struct mscp *mp;
	struct uba_device *ui;
	struct buf *dp, *bp;
	int st;

	mp = &ud->uda_rsp[i];
	mp->mscp_header.uda_msglen = sizeof (struct mscp);
	sc->sc_credits += mp->mscp_header.uda_credits & 0xf;
	if ((mp->mscp_header.uda_credits & 0xf0) > 0x10)
		return;
	/*
	 * If it's an error log message (datagram),
	 * pass it on for more extensive processing.
	 */
	if ((mp->mscp_header.uda_credits & 0xf0) == 0x10) {
		uderror(um, (struct mslg *)mp);
		return;
	}
	if (mp->mscp_unit >= 8)
		return;
	if ((ui = udip[um->um_ctlr][mp->mscp_unit]) == 0)
		return;
	st = mp->mscp_status&M_ST_MASK;
	switch (mp->mscp_opcode) {
	case M_OP_STCON|M_OP_END:
		if (st == M_ST_SUCC)
			sc->sc_state = S_RUN;
		else
			sc->sc_state = S_IDLE;
		um->um_tab.b_active = 0;
		wakeup((caddr_t)um);
		break;

	case M_OP_ONLIN|M_OP_END:
		/*
		 * Link the drive onto the controller queue
		 */
		dp = &udutab[ui->ui_unit];
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
		if (st == M_ST_SUCC) {
			ui->ui_flags = 1;	/* mark it online */
			radsize[ui->ui_unit] = (daddr_t)mp->mscp_untsize;
			printd("uda: unit %d online\n", mp->mscp_unit);
		} else {
			harderr(dp->b_actf, "ra");
			printf("OFFLINE\n");
			while (bp = dp->b_actf) {
				dp->b_actf = bp->av_forw;
				bp->b_flags |= B_ERROR;
				iodone(bp);
			}
		}
		dp->b_active = 1;
		break;

	case M_OP_AVATN:
		printd("uda: unit %d attention\n", mp->mscp_unit);
		ui->ui_flags = 0;	/* it went offline and we didn't notice */
		break;

	case M_OP_READ|M_OP_END:
	case M_OP_WRITE|M_OP_END:
		bp = (struct buf *)mp->mscp_cmdref;
		ubarelse(um->um_ubanum, (int *)&bp->b_ubinfo);
		/*
		 * Unlink buffer from I/O wait queue.
		 */
		bp->av_back->av_forw = bp->av_forw;
		bp->av_forw->av_back = bp->av_back;
		dp = &udutab[ui->ui_unit];
		if (ui->ui_dk >= 0)
			if (--dp->b_qsize == 0)
				dk_busy &= ~(1<<ui->ui_dk);
		if (st == M_ST_OFFLN || st == M_ST_AVLBL) {
			ui->ui_flags = 0;	/* mark unit offline */
			/*
			 * Link the buffer onto the front of the drive queue
			 */
			if ((bp->av_forw = dp->b_actf) == 0)
				dp->b_actl = bp;
			dp->b_actf = bp;
			/*
			 * Link the drive onto the controller queue
			 */
			if (dp->b_active == 0) {
				dp->b_forw = NULL;
				if (um->um_tab.b_actf == NULL)
					um->um_tab.b_actf = dp;
				else
					um->um_tab.b_actl->b_forw = dp;
				um->um_tab.b_actl = dp;
				dp->b_active = 1;
			}
			return;
		}
		if (st != M_ST_SUCC) {
			harderr(bp, "ra");
			printf("status %o\n", mp->mscp_status);
			bp->b_flags |= B_ERROR;
		}
		bp->b_resid = bp->b_bcount - mp->mscp_bytecnt;
		iodone(bp);
		break;

	case M_OP_GTUNT|M_OP_END:
		break;

	default:
		printf("uda: unknown packet\n");
	}
}


/*
 * Process an error log message
 *
 * For now, just log the error on the console.
 * Only minimal decoding is done, only "useful"
 * information is printed.  Eventually should
 * send message to an error logger.
 */
uderror(um, mp)
	register struct uba_ctlr *um;
	register struct mslg *mp;
{
	printf("uda%d: %s error, ", um->um_ctlr,
		mp->mslg_flags&M_LF_SUCC ? "soft" : "hard");
	switch (mp->mslg_format) {
	case M_FM_CNTERR:
		printf("controller error, event 0%o\n", mp->mslg_event);
		break;

	case M_FM_BUSADDR:
		printf("host memory access error, event 0%o, addr 0%o\n",
			mp->mslg_event, *((long *)&mp->mslg_busaddr[0]));
		break;

	case M_FM_DISKTRN:
		printf("disk transfer error, unit %d\n", mp->mslg_unit);
		break;

	case M_FM_SDI:
		printf("SDI error, unit %d, event 0%o\n", mp->mslg_unit,
			mp->mslg_event);
		break;

	case M_FM_SMLDSK:
		printf("small disk error, unit %d, event 0%o, cyl %d\n",
			mp->mslg_unit, mp->mslg_event, mp->mslg_sdecyl);
		break;

	default:
		printf("unknown error, unit %d, format 0%o, event 0%o\n",
			mp->mslg_unit, mp->mslg_format, mp->mslg_event);
	}

	if (udaerror) {
		register long *p = (long *)mp;
		register int i;

		for (i = 0; i < mp->mslg_header.uda_msglen; i += sizeof(*p))
			printf("%x ", *p++);
		printf("\n");
	}
}


/*
 * Find an unused command packet
 */
struct mscp *
udgetcp(um)
	struct uba_ctlr *um;
{
	register struct mscp *mp;
	register struct udaca *cp;
	register struct uda_softc *sc;
	register int i;

	cp = &uda[um->um_ctlr].uda_ca;
	sc = &uda_softc[um->um_ctlr];
	i = sc->sc_lastcmd;
	if ((cp->ca_cmddsc[i] & (UDA_OWN|UDA_INT)) == UDA_INT) {
		cp->ca_cmddsc[i] &= ~UDA_INT;
		mp = &uda[um->um_ctlr].uda_cmd[i];
		mp->mscp_unit = mp->mscp_modifier = 0;
		mp->mscp_opcode = mp->mscp_flags = 0;
		mp->mscp_bytecnt = mp->mscp_buffer = 0;
		mp->mscp_errlgfl = mp->mscp_copyspd = 0;
		sc->sc_lastcmd = (i + 1) % NCMD;
		return(mp);
	}
	return(NULL);
}

udread(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRA)
		u.u_error = ENXIO;
	else
		physio(udstrategy, &rudbuf[unit], dev, B_READ, minphys);
}

udwrite(dev)
	dev_t dev;
{
	register int unit = minor(dev) >> 3;

	if (unit >= NRA)
		u.u_error = ENXIO;
	else
		physio(udstrategy, &rudbuf[unit], dev, B_WRITE, minphys);
}

udreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register struct buf *bp, *dp;
	register int unit;
	struct buf *nbp;
	int d;

	for (d = 0; d < NUDA; d++) {
		if ((um = udminfo[d]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" uda%d", d);
		um->um_tab.b_active = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		uda_softc[d].sc_state = S_IDLE;
		for (unit = 0; unit < NRA; unit++) {
			if ((ui = uddinfo[unit]) == 0)
				continue;
			if (ui->ui_alive == 0 || ui->ui_mi != um)
				continue;
			udutab[unit].b_active = 0;
			udutab[unit].b_qsize = 0;
		}
		for (bp = udwtab[d].av_forw; bp != &udwtab[d]; bp = nbp) {
			nbp = bp->av_forw;
			ubarelse(uban, (int *)&bp->b_ubinfo);
			/*
			 * Link the buffer onto the drive queue
			 */
			dp = &udutab[dkunit(bp)];
			if (dp->b_actf == 0)
				dp->b_actf = bp;
			else
				dp->b_actl->av_forw = bp;
			dp->b_actl = bp;
			bp->av_forw = 0;
			/*
			 * Link the drive onto the controller queue
			 */
			if (dp->b_active == 0) {
				dp->b_forw = NULL;
				if (um->um_tab.b_actf == NULL)
					um->um_tab.b_actf = dp;
				else
					um->um_tab.b_actl->b_forw = dp;
				um->um_tab.b_actl = dp;
				dp->b_active = 1;
			}
		}
		udinit(d);
	}
}

uddump()
{
	return(ENXIO);
}
