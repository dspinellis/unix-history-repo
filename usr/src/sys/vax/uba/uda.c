/*
 *	@(#)uda.c	7.6 (Berkeley) %G%
 */

/************************************************************************
 *									*
 *			Copyright (c) 1983 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *			All rights reserved.				*
 *									*
 ************************************************************************/
/* 
 * uda.c - UDA50A Driver
 *
 * decvax!rich
 */

#define	COMPAT_42
#define	DEBUG
#define	UDADEVNUM	(9)		/* entry in bdevsw */
#include "ra.h"
#if NUDA > 0
/*
 * UDA50/RAxx disk device driver
 *
 * Restrictions:
 *      Unit numbers must be less than 8.
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "file.h"
#include "ioctl.h"
#include "user.h"
#include "map.h"
#include "vm.h"
#include "dkstat.h"
#include "cmap.h"
#include "uio.h"
#include "disklabel.h"
#include "syslog.h"
#include "stat.h"

#include "../vax/cpu.h"
#include "ubareg.h"
#include "ubavar.h"
#include "../vax/mtpr.h"

#define TENSEC	(1000)
 
#define NRSPL2  3               /* log2 number of response packets */
#define NCMDL2  3               /* log2 number of command packets */
#define NRSP    (1<<NRSPL2)
#define NCMD    (1<<NCMDL2)
#define	UDABURST	4	/* default for DMA burst size */

#include "../vaxuba/udareg.h"
#include "../vax/mscp.h"


struct uda_softc {
	short   sc_state;       /* state of controller */
	short   sc_mapped;      /* Unibus map allocated for uda struct? */
	int     sc_ubainfo;     /* Unibus mapping info */
	struct uda *sc_uda;     /* Unibus address of uda struct */
	int     sc_ivec;        /* interrupt vector address */
	short   sc_credits;     /* transfer credits */
	short   sc_lastcmd;     /* pointer into command ring */
	short   sc_lastrsp;     /* pointer into response ring */
} uda_softc[NUDA];
struct uda {
	struct udaca    uda_ca;         /* communications area */
	struct mscp     uda_rsp[NRSP];  /* response packets */
	struct mscp     uda_cmd[NCMD];  /* command packets */
} uda[NUDA];

#define udunit(dev)	(minor(dev) >> 3)
#define udpart(dev)	(minor(dev) & 07)
#define udminor(unit, part)	(((unit) << 3) | (part))

struct	ra_info {
	daddr_t		radsize;	/* Max user size form online pkt */
	unsigned	ratype;		/* Drive type int field  */
	unsigned	rastatus;	/* Command status from */
					/* last onlin or GTUNT */
	int		rastate;   	/* open/closed state */
	u_long		openpart;	/* partitions open */
	u_long		bopenpart;	/* block partitions open */
	u_long		copenpart;	/* characters partitions open */
} ra_info[NRA];

struct  uba_ctlr *udminfo[NUDA];
struct  uba_device *uddinfo[NRA];
struct  uba_device *udip[NUDA][8];      /* 8 == max number of drives */
struct  disklabel udlabel[NRA];
struct  buf rudbuf[NRA];
struct  buf udutab[NRA];
struct  buf udwtab[NUDA];               /* I/O wait queue, per controller */


int     udamicro[NUDA];         /* to store microcode level */
int     udaburst[NUDA] = { 0 };	/* DMA burst size, 0 is default */


/*
 * Controller states
 */
#define S_IDLE  0               /* hasn't been initialized */
#define S_STEP1 1               /* doing step 1 init */
#define S_STEP2 2               /* doing step 2 init */
#define S_STEP3 3               /* doing step 3 init */
#define S_SCHAR 4               /* doing "set controller characteristics" */
#define S_RUN   5               /* running */

/*
 * Software state, per drive
 */
#define	CLOSED		0
#define	WANTOPEN	1
#define	RDLABEL		2
#define	OPEN		3
#define	OPENRAW		4

int     udaerror = 0;                   /* causes hex dump of packets */
int     udadebug = 0;
int	uda_cp_wait = 0;		/* Something to wait on for command */
					/* packets and or credits. */
int	wakeup();
extern	int	hz;			/* Should find the right include */
#ifdef	DEBUG
#define printd  if (udadebug) printf
#define	printd10	if(udadebug >= 10) printf
#endif 
#define mprintf printf			/* temporary JG hack until Rich fixes*/

int     udprobe(), udslave(), udattach(), udintr(), udstrategy();
struct  mscp *udgetcp();

u_short udstd[] = { 0772150, 0772550, 0777550, 0 };
struct  uba_driver udadriver =
 { udprobe, udslave, udattach, 0, udstd, "ra", uddinfo, "uda", udminfo, 0 };

#define b_qsize         b_resid         /* queue size per drive, in udutab */
#define b_ubinfo        b_resid         /* Unibus mapping info, per buffer */

udprobe(reg, ctlr)
	caddr_t reg;
	int ctlr;
{
	register int br, cvec;
	register struct uda_softc *sc = &uda_softc[ctlr];
	struct udadevice *udaddr;

	int	cur_time;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	udreset(0); udintr(0);
#endif
	udaddr = (struct udadevice *) reg;

	sc->sc_ivec = (uba_hd[numuba].uh_lastiv -= 4);
#if VAX630
	if (cpu == VAX_630) {
		br = 0x15;
		cvec = sc->sc_ivec;
 		return(sizeof (struct udadevice));
	}
#endif
	udaddr->udaip = 0;              /* start initialization */

	cur_time = mfpr(TODR);			/* Time of day */
	while(cur_time + TENSEC > mfpr(TODR)){	/* wait for at most 10 secs */
		if((udaddr->udasa & UDA_STEP1) != 0)
			break;
	}
	if(cur_time + TENSEC <= mfpr(TODR))
		return(0);		/* Not a uda or it won't init as it  */
					/* should within ten seconds.  */
	udaddr->udasa=UDA_ERR|(NCMDL2<<11)|(NRSPL2<<8)|UDA_IE|(sc->sc_ivec/4);
	while((udaddr->udasa&UDA_STEP2)==0)
		DELAY(1000);		/* intr should have */
					/*   have happened by now */
	
	return(sizeof (struct udadevice));
}

/* ARGSUSED */
udslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	register struct uba_ctlr *um = udminfo[ui->ui_ctlr];
	register struct uda_softc *sc = &uda_softc[ui->ui_ctlr];
	struct udadevice *udaddr;
	struct	mscp	*mp;
	int	i;			/* Something to write into to start */
					/* the uda polling */


	udaddr = (struct udadevice *)um->um_addr;
	if(sc->sc_state != S_RUN){
		if(!udinit(ui->ui_ctlr))
			return(0);
	}
	/* Here we will wait for the controller */
	/* to come into the run state or go idle.  If we go idle we are in */
	/* touble and I don't yet know what to do so I will punt */
	while(sc->sc_state != S_RUN && sc->sc_state != S_IDLE);	/* spin */
	if(sc->sc_state == S_IDLE){	/* The Uda failed to initialize */
		printf("UDA failed to init\n");
		return(0);
	}
	/* The controller is up so let see if the drive is there! */
	if(0 == (mp = udgetcp(um))){	/* ditto */
		printf("UDA can't get command packet\n");
		return(0);
	}
	mp->mscp_opcode = M_OP_GTUNT;	/* This should give us the drive type*/
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_cmdref = (long) ui->ui_slave;
#ifdef	DEBUG
	printd("uda%d Get unit status slave %d\n",ui->ui_ctlr,ui->ui_slave);
#endif	
	ra_info[ui->ui_unit].rastatus = 0;	/* set to zero */
	udip[ui->ui_ctlr][ui->ui_slave] = ui;
	*((long *) mp->mscp_dscptr ) |= UDA_OWN | UDA_INT;/* maybe we should poll*/
	i = udaddr->udaip;
#ifdef lint
	i = i;
#endif
	while(!ra_info[ui->ui_unit].rastatus);  /* Wait for some status */
	udip[ui->ui_ctlr][ui->ui_slave] = 0;
	if(!ra_info[ui->ui_unit].ratype)	/* packet from a GTUNT */
		return(0);		/* Failed No such drive */
	else
		return(1);		/* Got it and it is there */
}

udattach(ui)
	register struct uba_device *ui;
{
	register struct uba_ctlr *um = ui->ui_mi ;
	struct udadevice *udaddr = (struct udadevice *) um->um_addr;
	register struct	mscp	*mp;
	register unit = ui->ui_unit;
	int	i;			/* Something to write into to start */
					/* the uda polling */
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = 1.0 / (60 * 31 * 256);     /* approx */
	ui->ui_flags = 0;
	udip[ui->ui_ctlr][ui->ui_slave] = ui;
	/* check to see if the drive is a available if it is bring it online */
	/* if not then just return.  open will try an online later */
	if(ra_info[unit].rastatus != M_ST_AVLBL)
		return;			/* status was set by a GTUNT */
	if(0 == (mp = udgetcp(um))){	/* ditto */
		printf("UDA can't get command packet\n");
		return;
	}
	mp->mscp_opcode = M_OP_ONLIN;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_cmdref = (long) ui->ui_slave;
#ifdef	DEBUG
	printd("uda%d ONLIN slave %d\n",ui->ui_ctlr,ui->ui_slave);
#endif	
	*((long *) mp->mscp_dscptr ) |= UDA_OWN | UDA_INT;
	i = udaddr->udaip;
#ifdef	lint
	i = i;
#endif
	for (i = 1000; ui->ui_flags == 0 && ra_info[unit].ratype != 0; ) {
		if (--i == 0)
			break;
		DELAY(1000);
	}
	/*
	 * Try to read pack label.
	 */
	if (rainit(ui, 0) == 0) {
		printf("ra%d: %s\n", unit, udlabel[unit].d_typename);
#ifdef notyet
		addswap(makedev(UDADEVNUM, udminor(unit, 0)), &udlabel[unit]);
#endif
	} else
		printf("ra%d: offline\n", unit);
}

/*
 * Open a UDA.  Initialize the device and
 * set the unit online.
 */
udopen(dev, flag, fmt)
	dev_t dev;
	int flag, fmt;
{
	int unit;
	register struct uba_device *ui;
	register struct uda_softc *sc;
	register struct disklabel *lp;
	register struct partition *pp;
	register struct ra_info *ra;
	int s, i, part, mask, error;
	daddr_t start, end;

	unit = udunit(dev);
	part = udpart(dev);
	mask = 1 << part;
	ra = &ra_info[unit];
	if (unit >= NRA || (ui = uddinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	sc = &uda_softc[ui->ui_ctlr];
	lp = &udlabel[unit];
	s = spl5();
	if (sc->sc_state != S_RUN) {
		if (sc->sc_state == S_IDLE)
			if(!udinit(ui->ui_ctlr)){
				printf("uda: Controller failed to init\n");
				(void) splx(s);
				return(ENXIO);
			}
		/* wait for initialization to complete */
		timeout(wakeup,(caddr_t)ui->ui_mi,11*hz);	/* to be sure*/
		sleep((caddr_t)ui->ui_mi, 0);
		if (sc->sc_state != S_RUN)
		{
			(void) splx(s); /* added by Rich */
			return (EIO);
		}
	}
	while (ra->rastate != OPEN && ra->rastate != OPENRAW &&
	    ra->rastate != CLOSED)
		sleep((caddr_t)ra, PZERO+1);
	splx(s);
	if (ui->ui_flags == 0 ||
	    (ra->rastate != OPEN && ra->rastate != OPENRAW))
		if (error = rainit(ui, flag))
			return (error);

	if (part >= lp->d_npartitions)
		return (ENXIO);
	/*
	 * Warn if a partion is opened
	 * that overlaps another partition which is open
	 * unless one is the "raw" partition (whole disk).
	 */
#define	RAWPART		2		/* 'c' partition */	/* XXX */
	if ((ra->openpart & mask) == 0 &&
	    part != RAWPART) {
		pp = &lp->d_partitions[part];
		start = pp->p_offset;
		end = pp->p_offset + pp->p_size;
		for (pp = lp->d_partitions;
		     pp < &lp->d_partitions[lp->d_npartitions]; pp++) {
			if (pp->p_offset + pp->p_size <= start ||
			    pp->p_offset >= end)
				continue;
			if (pp - lp->d_partitions == RAWPART)
				continue;
			if (ra->openpart &
			    (1 << (pp - lp->d_partitions)))
				log(LOG_WARNING,
				    "ra%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a',
				    pp - lp->d_partitions + 'a');
		}
	}
	switch (fmt) {
	case S_IFCHR:
		ra->copenpart |= mask;
		break;
	case S_IFBLK:
		ra->bopenpart |= mask;
		break;
	}
	ra->openpart |= mask;
	return (0);
}

/* ARGSUSED */
udclose(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register int unit = udunit(dev);
	register struct uda_softc *sc;
	struct uba_ctlr *um;
	register struct ra_info *ra = &ra_info[unit];
	int s, mask = (1 << udpart(dev));

	um = udminfo[unit];
	sc = &uda_softc[um->um_ctlr];
	switch (fmt) {
	case S_IFCHR:
		ra->copenpart &= ~mask;
		break;
	case S_IFBLK:
		ra->bopenpart &= ~mask;
		break;
	}
	if (((ra->copenpart | ra->bopenpart) & mask) == 0)
		ra->openpart &= ~mask;
	/*
	 * Should wait for I/O to complete on this partition
	 * even if others are open, but wait for work on blkflush().
	 */
	if (ra->openpart == 0) {
		s = spl5();
		while (udutab[unit].b_actf)
			sleep((caddr_t)&udutab[unit], PZERO - 1);
		splx(s);
		ra->rastate = CLOSED;
	}
	return (0);
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

	if (udaburst[d] == 0)
		udaburst[d] = UDABURST;
 	udaddr->udaip = 0;              /* start initialization */

	while((udaddr->udasa & UDA_STEP1) == 0){
		if(udaddr->udasa & UDA_ERR)
			return(0);	/* CHECK */
	}
	udaddr->udasa=UDA_ERR|(NCMDL2<<11)|(NRSPL2<<8)|UDA_IE|(sc->sc_ivec/4);
	/*
	 * Initialization continues in interrupt routine.
	 */
	sc->sc_state = S_STEP1;
	sc->sc_credits = 0;
	return(1);
}

/*
 * Initialize a drive:
 * bring on line and read in pack label.
 */
rainit(ui, flags)
	register struct uba_device *ui;
{
	register struct mscp *mp;
	register struct disklabel *lp;
	register struct uda_softc *sc;
	register unit = ui->ui_unit;
	register struct ra_info *ra = &ra_info[unit];
	struct udadevice *udaddr;
	char *msg, *readdisklabel();
	int s, i;
	extern int cold;

	lp = &udlabel[unit];
	sc = &uda_softc[ui->ui_ctlr];

	if (ui->ui_flags == 0) {
		/* check to see if the device is really there. */
		/* this code was taken from Fred Canters 11 driver */
		udaddr = (struct udadevice *) ui->ui_mi->um_addr;

		ra->rastate = WANTOPEN;
		s = spl5();
		while(0 ==(mp = udgetcp(ui->ui_mi))){
			uda_cp_wait++;
			sleep((caddr_t)&uda_cp_wait,PSWP+1);
			uda_cp_wait--;
		}
		mp->mscp_opcode = M_OP_ONLIN;
		mp->mscp_unit = ui->ui_slave;
			/* need to sleep on something */
		mp->mscp_cmdref = (long)ra;
#ifdef	DEBUG
		printd("uda: bring unit %d online\n",unit);
#endif	
		*((long *) mp->mscp_dscptr ) |= UDA_OWN | UDA_INT ;
		i = udaddr->udaip;
#ifdef	lint
		i = i;
#endif
			/* make sure we wake up */
		if (cold) {
			(void) splx(s);
			for (i = 10*1000; ra->rastate == WANTOPEN && --i; )
				DELAY(1000);
		} else {
			timeout(wakeup, (caddr_t)ra, 10 * hz);
			sleep((caddr_t)ra, PSWP+1);
			/*wakeup in udrsp() */
			(void) splx(s);
		}
		if (ra->rastate != OPENRAW) {
			ra->rastate = CLOSED;
			return (EIO);
		}
	}

	lp->d_secsize = DEV_BSIZE;
	lp->d_secperunit = ra->radsize;

	if (flags & O_NDELAY)
		return (0);
	ra->rastate = RDLABEL;
	/*
	 * Set up default sizes until we've read the label,
	 * or longer if there isn't one there.
	 * Set secpercyl, as readdisklabel wants to compute b_cylin
	 * (although we don't need it).
	 */
	lp->d_secpercyl = 1;
	lp->d_npartitions = 1;
	lp->d_partitions[0].p_size = lp->d_secperunit;
	lp->d_partitions[0].p_offset = 0;
	/*
	 * Read pack label.
	 */
	if (msg = readdisklabel(udminor(unit, 0), udstrategy, lp)) {
		log(LOG_ERR, "ra%d: %s\n", unit, msg);
#ifdef COMPAT_42
		if (udmaptype(unit, lp))
			ra->rastate = OPEN;
		else
			ra->rastate = OPENRAW;
#else
		ra->rastate = OPENRAW;
#endif
	} else
		ra->rastate = OPEN;
	wakeup((caddr_t)ra);
	return (0);
}

udstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	register struct buf *dp;
	register struct disklabel *lp;
	register int unit;
	struct uda_softc *sc;
	int xunit = udpart(bp->b_dev);
	daddr_t sz, maxsz;
	int s;

	unit = udunit(bp->b_dev);
	if (unit >= NRA) {
		bp->b_error = ENXIO;
		goto bad;
	}
	ui = uddinfo[unit];
	lp = &udlabel[unit];
	sc = &uda_softc[ui->ui_ctlr];
	um = ui->ui_mi;
	if (ui == 0 || ui->ui_alive == 0 || ra_info[unit].rastate == CLOSED) {
		bp->b_error = ENXIO;
		goto bad;
	}
	if (ra_info[unit].rastate < OPEN)
		goto q;
	if ((ra_info[unit].openpart & (1 << xunit)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}
	maxsz = lp->d_partitions[xunit].p_size; 
	sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	if (bp->b_blkno < 0 || bp->b_blkno + sz > maxsz) {
		if (bp->b_blkno == maxsz) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		sz = maxsz - bp->b_blkno;
		if (sz <= 0) {
			bp->b_error = EINVAL;
			goto bad;
		}
		bp->b_bcount = sz << DEV_BSHIFT;
	}
q:
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
		if (cpu == VAX_750
		    && udwtab[um->um_ctlr].av_forw == &udwtab[um->um_ctlr]) {
			if (um->um_ubinfo != 0) {
				printd("udastrat: ubinfo 0x%x\n",um->um_ubinfo);
			} else
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
done:
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
	struct disklabel *lp;
	struct udadevice *udaddr;
	struct uda *ud = &uda[um->um_ctlr];
	daddr_t sz;
	int i;

	sc = &uda_softc[um->um_ctlr];
	
loop:
	if ((dp = um->um_tab.b_actf) == NULL) {

		um->um_tab.b_active = 0;
		/* Check for response ring transitions lost in the
		 * Race condition
		 */
		for (i = sc->sc_lastrsp;; i++) {
			i %= NRSP;
			if (ud->uda_ca.ca_rspdsc[i]&UDA_OWN)
				break;
			udrsp(um, ud, sc, i);
			ud->uda_ca.ca_rspdsc[i] |= UDA_OWN;
		}
		sc->sc_lastrsp = i;
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
		if (ra_info[dp - udutab].openpart == 0)
			wakeup((caddr_t)dp);
		goto loop;		/* Need to check for loop */
	}
	um->um_tab.b_active++;
	udaddr = (struct udadevice *)um->um_addr;
	if ((udaddr->udasa&UDA_ERR) || sc->sc_state != S_RUN) {
		harderr(bp, "ra");
		mprintf("Uda%d udasa %o, state %d\n",um->um_ctlr , udaddr->udasa&0xffff, sc->sc_state);
		(void)udinit(um->um_ctlr);
		/* SHOULD REQUEUE OUTSTANDING REQUESTS, LIKE UDRESET */
		return (0);
	}
	ui = uddinfo[udunit(bp->b_dev)];
	lp = &udlabel[ui->ui_unit];
	if (ui->ui_flags == 0) {        /* not online */
		if ((mp = udgetcp(um)) == NULL){
			return (0);
		}
		mp->mscp_opcode = M_OP_ONLIN;
		mp->mscp_unit = ui->ui_slave;
		dp->b_active = 2;
		um->um_tab.b_actf = dp->b_forw; /* remove from controller q */
#ifdef	DEBUG
		printd("uda: bring unit %d online\n", ui->ui_slave);
#endif		
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		if (udaddr->udasa&UDA_ERR)
			printf("Uda (%d) Error (%x)\n",um->um_ctlr , udaddr->udasa&0xffff);
		i = udaddr->udaip;
		goto loop;
	}
	switch (cpu) {
	case VAX_8600:
	case VAX_780:
		i = UBA_NEEDBDP|UBA_CANTWAIT;
		break;

	case VAX_750:
		i = um->um_ubinfo|UBA_HAVEBDP|UBA_CANTWAIT;
		break;

	case VAX_730:
	case VAX_630:
		i = UBA_CANTWAIT;
		break;
	}
	if ((i = ubasetup(um->um_ubanum, bp, i)) == 0)
		return(1);
	if ((mp = udgetcp(um)) == NULL) {
#if defined(VAX750)
		if (cpu == VAX_750)
			i &= 0xfffffff;         /* mask off bdp */
#endif
		ubarelse(um->um_ubanum,&i);
		return(0);
	}
	mp->mscp_cmdref = (long)bp;     /* pointer to get back */
	mp->mscp_opcode = bp->b_flags&B_READ ? M_OP_READ : M_OP_WRITE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_buffer = (i & 0x3ffff) | (((i>>28)&0xf)<<24);
#if defined(VAX750)
	if (cpu == VAX_750)
		i &= 0xfffffff;         /* mask off bdp */
#endif
	bp->b_ubinfo = i;               /* save mapping info */
	i = udpart(bp->b_dev);
	mp->mscp_lbn = bp->b_blkno +
	    lp->d_partitions[i].p_offset;
	sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	if (bp->b_blkno + sz > lp->d_partitions[i].p_size)
		mp->mscp_bytecnt = (lp->d_partitions[i].p_size - bp->b_blkno) >>
		    DEV_BSHIFT;
	else
		mp->mscp_bytecnt = bp->b_bcount;
	*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
	if (udaddr->udasa&UDA_ERR) 
		printf("Uda(%d) udasa (%x)\n",um->um_ctlr , udaddr->udasa&0xffff);
	i = udaddr->udaip;              /* initiate polling */
	dp->b_qsize++;
	if (ui->ui_dk >= 0) {
		dk_busy |= 1<<ui->ui_dk;
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
	register d;
{
	struct uba_ctlr *um = udminfo[d];
	register struct udadevice *udaddr = (struct udadevice *)um->um_addr;
	struct buf *bp;
	register int i;
	register struct uda_softc *sc = &uda_softc[d];
	register struct uda *ud = &uda[d];
	struct uda *uud;
	register struct mscp *mp;

#ifdef	DEBUG
	printd10("udintr: state %d, udasa %o\n", sc->sc_state, udaddr->udasa);
#endif	
#ifdef VAX630
	(void) spl5();
#endif
	switch (sc->sc_state) {
	case S_IDLE:
		printf("uda%d: random interrupt ignored\n", d);
		return;

	case S_STEP1:
#define STEP1MASK       0174377
#define STEP1GOOD       (UDA_STEP2|UDA_IE|(NCMDL2<<3)|NRSPL2)
		if ((udaddr->udasa&STEP1MASK) != STEP1GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udaddr->udasa = ((int)&sc->sc_uda->uda_ca.ca_ringbase)|
		    ((cpu == VAX_780) || (cpu == VAX_8600) ? UDA_PI : 0);
		sc->sc_state = S_STEP2;
		return;

	case S_STEP2:
#define STEP2MASK       0174377
#define STEP2GOOD       (UDA_STEP3|UDA_IE|(sc->sc_ivec/4))
		if ((udaddr->udasa&STEP2MASK) != STEP2GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udaddr->udasa = ((int)&sc->sc_uda->uda_ca.ca_ringbase)>>16;
		sc->sc_state = S_STEP3;
		return;

	case S_STEP3:
#define STEP3MASK       0174000
#define STEP3GOOD       UDA_STEP4
		if ((udaddr->udasa&STEP3MASK) != STEP3GOOD) {
			sc->sc_state = S_IDLE;
			wakeup((caddr_t)um);
			return;
		}
		udamicro[d] = udaddr->udasa;
		log(LOG_INFO, "uda%d: version %d model %d\n", d,
		    udamicro[d] & 0xf, (udamicro[d] >> 4) & 0xf);
		/*
		 * Requesting the error status (|= 2)
		 * may hang older controllers.
		 */
		i = UDA_GO | (udaerror? 2 : 0);
		if (udaburst[d])
			i |= (udaburst[d] - 1) << 2;
		udaddr->udasa = i;
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
			ud->uda_rsp[i].mscp_header.uda_msglen = mscp_msglen;
		}
		for (i = 0; i < NCMD; i++) {
			ud->uda_ca.ca_cmddsc[i] = UDA_INT|
				(long)&uud->uda_cmd[i].mscp_cmdref;
			ud->uda_cmd[i].mscp_dscptr = &ud->uda_ca.ca_cmddsc[i];
			ud->uda_cmd[i].mscp_header.uda_msglen = mscp_msglen;
		}
		bp = &udwtab[d];
		bp->av_forw = bp->av_back = bp;
		sc->sc_lastcmd = 1;
		sc->sc_lastrsp = 0;
		mp = &uda[um->um_ctlr].uda_cmd[0];
		mp->mscp_unit = mp->mscp_modifier = 0;
		mp->mscp_flags = 0;
		mp->mscp_bytecnt = mp->mscp_buffer = 0;
		mp->mscp_errlgfl = mp->mscp_copyspd = 0;
		mp->mscp_opcode = M_OP_STCON;
		mp->mscp_cntflgs = M_CF_ATTN|M_CF_MISC|M_CF_THIS;
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		i = udaddr->udaip;      /* initiate polling */
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
		printf("uda(%d): fatal error (%o)\n", d, udaddr->udasa&0xffff);
		udaddr->udaip = 0;
		wakeup((caddr_t)um);
	}

	/*
	 * Check for a buffer purge request.
	 */
	if (ud->uda_ca.ca_bdp) {
#ifdef	DEBUG
		printd("uda: purge bdp %d\n", ud->uda_ca.ca_bdp);
#endif		
		UBAPURGE(um->um_hd->uh_uba, ud->uda_ca.ca_bdp);
		ud->uda_ca.ca_bdp = 0;
		udaddr->udasa = 0;      /* signal purge complete */
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
#ifdef	DEBUG
		printd("uda: command ring transition\n");
#endif		
		ud->uda_ca.ca_cmdint = 0;
	}
	if(uda_cp_wait)
		wakeup((caddr_t)&uda_cp_wait);
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
	register struct uba_device *ui;
	register int unit;
	struct buf *dp, *bp, nullbp;
	int st;

	mp = &ud->uda_rsp[i];
	mp->mscp_header.uda_msglen = mscp_msglen;
	sc->sc_credits += mp->mscp_header.uda_credits & 0xf;  /* just 4 bits?*/
	if ((mp->mscp_header.uda_credits & 0xf0) > 0x10)	/* Check */
		return;
#ifdef	DEBUG
	printd10("udarsp, opcode 0x%x status 0x%x\n",mp->mscp_opcode,mp->mscp_status);
#endif	
	/*
	 * If it's an error log message (datagram),
	 * pass it on for more extensive processing.
	 */
	if ((mp->mscp_header.uda_credits & 0xf0) == 0x10) {	/* check */
		uderror(um, (struct mslg *)mp);
		return;
	}
	st = mp->mscp_status&M_ST_MASK;
	/* The controller interrupts as drive 0 */
	/* this means that you must check for controller interrupts */
	/* before you check to see if there is a drive 0 */
	if((M_OP_STCON|M_OP_END) == mp->mscp_opcode){
		if (st == M_ST_SUCC)
			sc->sc_state = S_RUN;
		else
			sc->sc_state = S_IDLE;
		um->um_tab.b_active = 0;
		wakeup((caddr_t)um);
		return;
	}
	if (mp->mscp_unit >= 8)
		return;
	if ((ui = udip[um->um_ctlr][mp->mscp_unit]) == 0)
		return;
	unit = ui->ui_unit;
	switch (mp->mscp_opcode) {

	case M_OP_ONLIN|M_OP_END:
		ra_info[unit].rastatus = st;
		ra_info[unit].ratype =  mp->mscp_mediaid;
		dp = &udutab[unit];
		if (st == M_ST_SUCC) {
			/*
			 * Link the drive onto the controller queue
			 */
			dp->b_forw = NULL;
			if (um->um_tab.b_actf == NULL)
				um->um_tab.b_actf = dp;
			else
				um->um_tab.b_actl->b_forw = dp;
			um->um_tab.b_actl = dp;
			ui->ui_flags = 1;       /* mark it online */
			ra_info[unit].rastate = OPENRAW;
			ra_info[unit].radsize=(daddr_t)mp->mscp_untsize;
#ifdef	DEBUG
			printd("uda: unit %d online\n", mp->mscp_unit);
#endif			
#define F_to_C(x,i)     ( ((x)->mscp_mediaid) >> (i*5+7) & 0x1f ? ( ( (((x)->mscp_mediaid) >>( i*5 + 7)) & 0x1f) + 'A' - 1): ' ')
		/* this mess decodes the Media type identifier */
#ifdef	DEBUG
			printd("uda: unit %d online %x %c%c %c%c%c%d\n"
				,mp->mscp_unit, mp->mscp_mediaid
				,F_to_C(mp,4),F_to_C(mp,3),F_to_C(mp,2)
				,F_to_C(mp,1),F_to_C(mp,0)
				,mp->mscp_mediaid & 0x7f);
#endif				
			dp->b_active = 1;
		} else {
			if(dp->b_actf){
				harderr(dp->b_actf,"ra");
			} else {
				nullbp.b_blkno = 0;
				nullbp.b_dev = makedev(UDADEVNUM,unit);
				harderr(&nullbp, "ra");
			}
			printf("OFFLINE\n");
			while (bp = dp->b_actf) {
				dp->b_actf = bp->av_forw;
				bp->b_flags |= B_ERROR;
				iodone(bp);
			}
			ra_info[unit].rastate = CLOSED;
		}
		if(mp->mscp_cmdref!=NULL){/* Seems to get lost sometimes */
			wakeup((caddr_t)mp->mscp_cmdref);
		}
		break;

/*
 * The AVAILABLE ATTENTION messages occurs when the
 * unit becomes available after spinup,
 * marking the unit offline will force an online command
 * prior to using the unit.
 */
	case M_OP_AVATN:
#ifdef	DEBUG
		printd("uda: unit %d attention\n", mp->mscp_unit);
#endif		
		ui->ui_flags = 0;       /* it went offline and we didn't notice */
		ra_info[unit].ratype =  mp->mscp_mediaid;
		break;

	case M_OP_END:
/*
 * An endcode without an opcode (0200) is an invalid command.
 * The mscp specification states that this would be a protocol
 * type error, such as illegal opcodes. The mscp spec. also
 * states that parameter error type of invalid commands should
 * return the normal end message for the command. This does not appear
 * to be the case. An invalid logical block number returned an endcode
 * of 0200 instead of the 0241 (read) that was expected.
 */
	
		printf("endcd=%o, stat=%o\n", mp->mscp_opcode, mp->mscp_status);
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
#if defined(VAX750)
		if (cpu == VAX_750 && um->um_tab.b_active == 0
		    && udwtab[um->um_ctlr].av_forw == &udwtab[um->um_ctlr]) {
			if (um->um_ubinfo == 0)
				printf("udintr: um_ubinfo == 0\n");
			else
				ubarelse(um->um_ubanum, &um->um_ubinfo);
		}
#endif
		dp = &udutab[unit];
		dp->b_qsize--;
		if (ui->ui_dk >= 0)
			if (dp->b_qsize == 0)
				dk_busy &= ~(1<<ui->ui_dk);
		if (st == M_ST_OFFLN || st == M_ST_AVLBL) {
			ui->ui_flags = 0;       /* mark unit offline */
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
#if defined(VAX750)
			if (cpu == VAX750 && um->um_ubinfo == 0)
				um->um_ubinfo =
				   uballoc(um->um_ubanum, (caddr_t)0, 0,
					UBA_NEEDBDP);
#endif
			return;
		}
		if (st != M_ST_SUCC) {
			harderr(bp, "ra");
#ifdef	DEBUG
			printd("status %o\n", mp->mscp_status);
#endif
			bp->b_flags |= B_ERROR;
		}
		bp->b_resid = bp->b_bcount - mp->mscp_bytecnt;
		iodone(bp);
		break;

	case M_OP_GTUNT|M_OP_END:
#ifdef	DEBUG
		printd("GTUNT end packet status = 0x%x media id 0x%x\n"
			,st,mp->mscp_mediaid);
#endif		
		ra_info[unit].rastatus = st;
		ra_info[unit].ratype = mp->mscp_mediaid;
		break;

	default:
		printf("uda: unknown packet\n");
		uderror(um, (struct mslg *)mp);
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
	register	i;


	if(!(mp->mslg_flags & (M_LF_SUCC | M_LF_CONT)))
		printf("uda%d: hard error\n");

	mprintf("uda%d: %s error, ", um->um_ctlr,
		mp->mslg_flags & ( M_LF_SUCC | M_LF_CONT ) ? "soft" : "hard");
	switch (mp->mslg_format) {
	case M_FM_CNTERR:
		mprintf("controller error, event 0%o\n", mp->mslg_event);
		break;

	case M_FM_BUSADDR:
		mprintf("host memory access error, event 0%o, addr 0%o\n",
			mp->mslg_event, mp->mslg_busaddr);
		break;

	case M_FM_DISKTRN:
		mprintf("disk transfer error, unit %d, grp 0x%x, hdr 0x%x, event 0%o\n",
			mp->mslg_unit, mp->mslg_group, mp->mslg_hdr,
mp->mslg_event);
		break;

	case M_FM_SDI:
		mprintf("SDI error, unit %d, event 0%o, hdr 0x%x\n",
			mp->mslg_unit, mp->mslg_event, mp->mslg_hdr);
		for(i = 0; i < 12;i++)
			mprintf("\t0x%x",mp->mslg_sdistat[i] & 0xff);
		mprintf("\n");
		break;

	case M_FM_SMLDSK:
		mprintf("small disk error, unit %d, event 0%o, cyl %d\n",
			mp->mslg_unit, mp->mslg_event, mp->mslg_sdecyl);
		break;

	default:
		mprintf("unknown error, unit %d, format 0%o, event 0%o\n",
			mp->mslg_unit, mp->mslg_format, mp->mslg_event);
	}

	if (udaerror) {
		register long *p = (long *)mp;

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
	int	s;

	s = spl5();
	cp = &uda[um->um_ctlr].uda_ca;
	sc = &uda_softc[um->um_ctlr];
	/*
	 * If no credits, can't issue any commands
	 * until some outstanding commands complete.
	 */
	i = sc->sc_lastcmd;
	if(((cp->ca_cmddsc[i]&(UDA_OWN|UDA_INT))==UDA_INT)&&
	    (sc->sc_credits >= 2)) {
		sc->sc_credits--;       /* committed to issuing a command */
		cp->ca_cmddsc[i] &= ~UDA_INT;
		mp = &uda[um->um_ctlr].uda_cmd[i];
		mp->mscp_unit = mp->mscp_modifier = 0;
		mp->mscp_opcode = mp->mscp_flags = 0;
		mp->mscp_bytecnt = mp->mscp_buffer = 0;
		mp->mscp_errlgfl = mp->mscp_copyspd = 0;
		sc->sc_lastcmd = (i + 1) % NCMD;
		(void) splx(s);
		return(mp);
	}
	(void) splx(s);
	return(NULL);
}

udread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = udunit(dev);

	if (unit >= NRA)
		return (ENXIO);
	return (physio(udstrategy, &rudbuf[unit], dev, B_READ, minphys, uio));
}

udwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = udunit(dev);

	if (unit >= NRA)
		return (ENXIO);
	return (physio(udstrategy, &rudbuf[unit], dev, B_WRITE, minphys, uio));
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
		uda_softc[d].sc_mapped = 0;	/* Rich */
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
			bp->b_ubinfo = 0;
			/*
			 * Link the buffer onto the drive queue
			 */
			dp = &udutab[udunit(bp->b_dev)];
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
		(void)udinit(d);
	}
}

#define DBSIZE 32

#define ca_Rspdsc       ca_rspdsc[0]
#define ca_Cmddsc       ca_rspdsc[1]
#define uda_Rsp         uda_rsp[0]
#define uda_Cmd         uda_cmd[0]

struct  uda     udad[NUDA];

uddump(dev)
	dev_t dev;
{
	struct udadevice *udaddr;
	struct uda *ud_ubaddr;
	char *start;
	int num, blk, unit;
	int maxsz;
	int blkoff;
	register struct uba_regs *uba;
	register struct uba_device *ui;
	register struct uda *udp;
	register struct pte *io;
	register int i;
	struct disklabel *lp;
	unit = udunit(dev);
	if (unit >= NRA)
		return (ENXIO);
#define phys(cast, addr) ((cast)((int)addr & 0x7fffffff))
	ui = phys(struct uba_device *, uddinfo[unit]);
	if (ui->ui_alive == 0)
		return (ENXIO);
	uba = phys(struct uba_hd *, ui->ui_hd)->uh_physuba;
	ubainit(uba);
	udaddr = (struct udadevice *)ui->ui_physaddr;
	DELAY(2000000);
	udp = phys(struct uda *, &udad[ui->ui_ctlr]);
	lp = &udlabel[unit];

	num = btoc(sizeof(struct uda)) + 1;
	io = &uba->uba_map[NUBMREG-num];
	for(i = 0; i<num; i++)
		*(int *)io++ = UBAMR_MRV|(btop(udp)+i);
	ud_ubaddr = (struct uda *)(((int)udp & PGOFSET)|((NUBMREG-num)<<9));

	udaddr->udaip = 0;
	while ((udaddr->udasa & UDA_STEP1) == 0)
		if(udaddr->udasa & UDA_ERR) return(EFAULT);
	udaddr->udasa = UDA_ERR;
	while ((udaddr->udasa & UDA_STEP2) == 0)
		if(udaddr->udasa & UDA_ERR) return(EFAULT);
	udaddr->udasa = (short)&ud_ubaddr->uda_ca.ca_ringbase;
	while ((udaddr->udasa & UDA_STEP3) == 0)
		if(udaddr->udasa & UDA_ERR) return(EFAULT);
	udaddr->udasa = (short)(((int)&ud_ubaddr->uda_ca.ca_ringbase) >> 16);
	while ((udaddr->udasa & UDA_STEP4) == 0)
		if(udaddr->udasa & UDA_ERR) return(EFAULT);
	udaddr->udasa = UDA_GO;
	udp->uda_ca.ca_Rspdsc = (long)&ud_ubaddr->uda_Rsp.mscp_cmdref;
	udp->uda_ca.ca_Cmddsc = (long)&ud_ubaddr->uda_Cmd.mscp_cmdref;
	udp->uda_Cmd.mscp_cntflgs = 0;
	udp->uda_Cmd.mscp_version = 0;
	if (udcmd(M_OP_STCON, udp, udaddr) == 0) {
		return(EFAULT);
	}
	udp->uda_Cmd.mscp_unit = ui->ui_slave;
	if (udcmd(M_OP_ONLIN, udp, udaddr) == 0) {
		return(EFAULT);
	}

	num = maxfree;
	start = 0;
	blkoff = lp->d_partitions[udpart(dev)].p_offset;
	maxsz = lp->d_partitions[udpart(dev)].p_size;
	if (dumplo < 0)
		return (EINVAL);
	if (dumplo + num >= maxsz)
		num = maxsz - dumplo;
	blkoff += dumplo;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		io = uba->uba_map;
		for (i = 0; i < blk; i++)
			*(int *)io++ = (btop(start)+i) | UBAMR_MRV;
		*(int *)io = 0;
		udp->uda_Cmd.mscp_lbn = btop(start) + blkoff;
		udp->uda_Cmd.mscp_unit = ui->ui_slave;
		udp->uda_Cmd.mscp_bytecnt = blk*NBPG;
		udp->uda_Cmd.mscp_buffer = 0;
		if (udcmd(M_OP_WRITE, udp, udaddr) == 0) {
			return(EIO);
		}
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}


udcmd(op, udp, udaddr)
	int op;
	register struct uda *udp;
	struct udadevice *udaddr;
{
	int i;

	udp->uda_Cmd.mscp_opcode = op;
	udp->uda_Rsp.mscp_header.uda_msglen = mscp_msglen;
	udp->uda_Cmd.mscp_header.uda_msglen = mscp_msglen;
	udp->uda_ca.ca_Rspdsc |= UDA_OWN|UDA_INT;
	udp->uda_ca.ca_Cmddsc |= UDA_OWN|UDA_INT;
	if (udaddr->udasa&UDA_ERR)
		printf("Udaerror udasa (%x)\n", udaddr->udasa&0xffff);
	i = udaddr->udaip;
#ifdef	lint
	i = i;
#endif
	for (;;) {
		if (udp->uda_ca.ca_cmdint)
			udp->uda_ca.ca_cmdint = 0;
		if (udp->uda_ca.ca_rspint)
			break;
	}
	udp->uda_ca.ca_rspint = 0;
	if (udp->uda_Rsp.mscp_opcode != (op|M_OP_END) ||
	    (udp->uda_Rsp.mscp_status&M_ST_MASK) != M_ST_SUCC) {
		printf("error: com %d opc 0x%x stat 0x%x\ndump ",
			op,
			udp->uda_Rsp.mscp_opcode,
			udp->uda_Rsp.mscp_status);
		return(0);
	}
	return(1);
}

udioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	int unit = udunit(dev);
	register struct disklabel *lp;
	int error = 0;

	lp = &udlabel[unit];

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[udpart(dev)];
		break;

	case DIOCSDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			*lp = *(struct disklabel *)data;
		break;

	case DIOCWDINFO:
		if ((flag & FWRITE) == 0) {
			error = EBADF;
			break;
		}
		{
		struct buf *bp;
		struct disklabel *dlp;
#ifdef notdef
		daddr_t alt, end;
#endif

		*lp = *(struct disklabel *)data;
		bp = geteblk(lp->d_secsize);
		bp->b_dev = makedev(major(dev), udminor(udunit(dev), 0));
		bp->b_bcount = lp->d_secsize;
		bp->b_blkno = LABELSECTOR;
		bp->b_flags = B_READ;
		dlp = (struct disklabel *)(bp->b_un.b_addr + LABELOFFSET);
		udstrategy(bp);
		biowait(bp);
		if (bp->b_flags & B_ERROR) {
			error = u.u_error;		/* XXX */
			u.u_error = 0;
			goto bad;
		}
		*dlp = *lp;
#ifdef notdef
		alt = lp->d_ncylinders * lp->d_secpercyl - lp->d_ntracks + 1;
		end = alt + 8;
		for (;;) {
			bp->b_flags = B_WRITE;
			udstrategy(bp);
			biowait(bp);
			if (bp->b_flags & B_ERROR) {
				error = u.u_error;	/* XXX */
				u.u_error = 0;
			}
			if (bp->b_blkno >= end)
				break;
			bp->b_blkno = alt;
			alt += 2;
		}
#endif
bad:
		brelse(bp);
		}
		break;

	default:
		error = ENOTTY;
		break;
	}
	return (0);
}

udsize(dev)
	dev_t dev;
{
	register int unit = udunit(dev);
	register struct uba_device *ui;

	if (unit >= NRA || (ui = uddinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_flags == 0 || ra_info[unit].rastate != OPEN)
		return (-1);
	return ((int)udlabel[unit].d_partitions[udpart(dev)].p_size);
}

#ifdef COMPAT_42
struct size {
	daddr_t nblocks;
	daddr_t blkoff;
}  ra25_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	10032,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=blk 340670 thru 356553 */
	0,	0,		/* E=blk 356554 thru 412489 */
	0,	0,		/* F=blk 412490 thru end */
	-1,	25916,		/* G=blk 49324 thru 131403 */
	0,	0,		/* H=blk 131404 thru end */
}, rd52_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	9766,	15884,		/* B=blk 15884 thru 25649 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=unused */
	0,	0,		/* E=unused */
	0,	0,		/* F=unused */
	-1,	25650,		/* G=blk 25650 thru end */
	0,	0,		/* H=unused */
}, rd53_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=unused */
	33440,	0,		/* E=blk 0 thru 33439 */
	-1,	33440,		/* F=blk 33440 thru end */
	-1,	49324,		/* G=blk 49324 thru end */
	-1,	15884,		/* H=blk 15884 thru end */
}, ra60_sizes[8] = {
	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	400176,	0,		/* C=sectors 0 thru 400175 */
	82080,	49324,		/* 4.2 G => D=sectors 49324 thru 131403 */
	268772,	131404,		/* 4.2 H => E=sectors 131404 thru 400175 */
	350852,	49324,		/* F=sectors 49324 thru 400175 */
	157570,	242606,		/* UCB G => G=sectors 242606 thru 400175 */
	193282,	49324,		/* UCB H => H=sectors 49324 thru 242605 */
}, ra80_sizes[8] = {
	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	242606,	0,		/* C=sectors 0 thru 242605 */
	0,	0,		/* D=unused */
	193282,	49324,		/* UCB H => E=sectors 49324 thru 242605 */
	82080,	49324,		/* 4.2 G => F=sectors 49324 thru 131403 */
	192696,	49910,		/* G=sectors 49910 thru 242605 */
	111202,	131404,		/* 4.2 H => H=sectors 131404 thru 242605 */
}, ra81_sizes[8] ={
/*
 * These are the new standard partition sizes for ra81's.
 * An RA_COMPAT system is compiled with D, E, and F corresponding
 * to the 4.2 partitions for G, H, and F respectively.
 */
#ifndef	UCBRA
	15884,	0,		/* A=sectors 0 thru 15883 */
	66880,	16422,		/* B=sectors 16422 thru 83301 */
	891072,	0,		/* C=sectors 0 thru 891071 */
#ifdef RA_COMPAT
	82080,	49324,		/* 4.2 G => D=sectors 49324 thru 131403 */
	759668,	131404,		/* 4.2 H => E=sectors 131404 thru 891071 */
	478582,	412490,		/* 4.2 F => F=sectors 412490 thru 891071 */
#else
	15884,	375564,		/* D=sectors 375564 thru 391447 */
	307200,	391986,		/* E=sectors 391986 thru 699185 */
	191352,	699720,		/* F=sectors 699720 thru 891071 */
#endif RA_COMPAT
	515508,	375564,		/* G=sectors 375564 thru 891071 */
	291346,	83538,		/* H=sectors 83538 thru 374883 */

/*
 * These partitions correspond to the sizes used by sites at Berkeley,
 * and by those sites that have received copies of the Berkeley driver
 * with deltas 6.2 or greater (11/15/83).
 */
#else UCBRA

	15884,	0,		/* A=sectors 0 thru 15883 */
	33440,	15884,		/* B=sectors 15884 thru 49323 */
	891072,	0,		/* C=sectors 0 thru 891071 */
	15884,	242606,		/* D=sectors 242606 thru 258489 */
	307200,	258490,		/* E=sectors 258490 thru 565689 */
	325382,	565690,		/* F=sectors 565690 thru 891071 */
	648466,	242606,		/* G=sectors 242606 thru 891071 */
	193282,	49324,		/* H=sectors 49324 thru 242605 */

#endif UCBRA
};

udmaptype(unit, lp)
	register unit;
	register struct disklabel *lp;
{
	register struct size *rasizes;
	register struct partition *pp;
	register type;

	lp->d_secperunit = ra_info[unit].radsize;
	type = ra_info[unit].ratype & 0x7f;
	lp->d_typename[0] = 'r';
	lp->d_typename[1] = 'a';
	lp->d_typename[2] = '0' + type/10;
	lp->d_typename[3] = '0' + type%10;
	switch (type) {
	case    25:
		rasizes = ra25_sizes;
		lp->d_nsectors = 42;
		lp->d_ntracks = 4;
		lp->d_ncylinders = 302;
		break;
	case    52:
		lp->d_typename[1] = 'd';
		rasizes = rd52_sizes;
		lp->d_nsectors = 18;
		lp->d_ntracks = 7;
		lp->d_ncylinders = 480;
		break;
	case    53:
		rasizes = rd53_sizes;
		lp->d_typename[1] = 'd';
		lp->d_nsectors = 18;
		lp->d_ntracks = 8;
		lp->d_ncylinders = 963;
		break;
	case    60:
		rasizes = ra60_sizes;
		lp->d_nsectors = 42;
		lp->d_ntracks = 4;
		lp->d_ncylinders = 2382;
		break;
	case    80:
		rasizes = ra80_sizes;
		lp->d_nsectors = 31;
		lp->d_ntracks = 14;
		lp->d_ncylinders = 559;
		break;
	case    81:
		rasizes = ra81_sizes;
		lp->d_nsectors = 51;
		lp->d_ntracks = 14;
		lp->d_ncylinders = 1248;
		break;
	default:
		printf("Don't have a partition table for an ra%d\n", type);
		lp->d_npartitions = 1;
		lp->d_partitions[0].p_offset = 0;
		lp->d_partitions[0].p_size = lp->d_secperunit;
		return (0);
	}
	lp->d_secsize = 512;
	lp->d_npartitions = 8;
	lp->d_secpercyl = lp->d_nsectors * lp->d_ntracks;
	for (pp = lp->d_partitions; pp < &lp->d_partitions[8];
	    pp++, rasizes++) {
		pp->p_offset = rasizes->blkoff;
		if ((pp->p_size = rasizes->nblocks) == (u_long)-1)
			pp->p_size = ra_info[unit].radsize - rasizes->blkoff;
	}
	return (1);
}
#endif COMPAT_42
#endif
