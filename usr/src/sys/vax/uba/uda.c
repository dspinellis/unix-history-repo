/*
 *	@(#)uda.c	6.7 (Berkeley) %G%
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
 * Date:        Jan  30 1984
 *
 * This thing has been beaten beyound belief.  It still has two main features.
 * 1) When this device is on the same unibus as another DMA device
 * like a versatec or a rk07. the Udstrat routine complains that it still
 * has a buffered data path that it shouldn't.  I don't know why.
 *
 * decvax!rich.
 *
 */

#define	DEBUG
#define	UDADEVNUM	(9)		/* entry in bdevsw */
#include "ra.h"
#if NUDA > 0
/*
 * UDA50/RAxx disk device driver
 *
 * Restrictions:
 *      Unit numbers must be less than 8.
 *      Partitions A and B must be the same size on all RA drives.
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "map.h"
#include "vm.h"
#include "dk.h"
#include "cmap.h"
#include "uio.h"

#include "../vax/cpu.h"
#include "ubareg.h"
#include "ubavar.h"
#include "../vax/mtpr.h"

#define TENSEC	(1000)
 
#define NRSPL2  3               /* log2 number of response packets */
#define NCMDL2  3               /* log2 number of command packets */
#define NRSP    (1<<NRSPL2)
#define NCMD    (1<<NCMDL2)

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

/* THIS SHOULD BE READ OFF THE PACK, PER DRIVE */
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
}, ra60_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	15884,	242606,		/* D=blk 242606 thru 258489 */
	-1,	258490,		/* E=blk 258490 thru end */
	0,	0,		/* F=unused */
	-1,	242606,		/* G=blk 242606 thru end */
	193282,	49324,		/* H=blk 49324 thru 242605 */
}, ra80_sizes[8] = {
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	0,	0,		/* D=unused */
	0,	0,		/* E=unused */
	0,	0,		/* F=unused */
	0,	0,		/* G=unused */
	193282,	49324,		/* H=blk 49324 thru 242605 */
}, ra81_sizes[8] ={
	15884,	0,		/* A=blk 0 thru 15883 */
	33440,	15884,		/* B=blk 15884 thru 49323 */
	-1,	0,		/* C=blk 0 thru end */
	15884,	242606,		/* D=blk 242606 thru 258489 */
	307200,	258490,		/* E=blk 258490 thru 565689 */
	-1,	565690,		/* F=blk 565690 thru end */
	-1,	242606,		/* G=blk 242606 thru end */
	193282,	49324,		/* H=blk 49324 thru 242605 */
};

struct	ra_info {
	struct  size    *ra_sizes;	/* Partion tables for drive */
	daddr_t		radsize;	/* Max user size form online pkt */
	unsigned	ratype;		/* Drive type int field  */
	unsigned	rastatus;	/* Command status from */
					/* last onlin or GTUNT */
} ra_info[NRA];


/* END OF STUFF WHICH SHOULD BE READ IN PER DISK */
struct  uba_ctlr *udminfo[NUDA];
struct  uba_device *uddinfo[NRA];
struct  uba_device *udip[NUDA][8];      /* 8 == max number of drives */
struct  buf rudbuf[NRA];
struct  buf udutab[NRA];
struct  buf udwtab[NUDA];               /* I/O wait queue, per controller */


int     nNRA = NRA;
int     nNUDA = NUDA;
int     udamicro[NUDA];         /* to store microcode level */


/*
 * Controller states
 */
#define S_IDLE  0               /* hasn't been initialized */
#define S_STEP1 1               /* doing step 1 init */
#define S_STEP2 2               /* doing step 2 init */
#define S_STEP3 3               /* doing step 3 init */
#define S_SCHAR 4               /* doing "set controller characteristics" */
#define S_RUN   5               /* running */


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

int     udprobe(), udslave(), udattach(), udintr();
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


#ifdef lint
	ui = ui; reg = reg; i = i;
#endif
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
	struct	mscp	*mp;
	int	i;			/* Something to write into to start */
					/* the uda polling */
#ifdef	lint
	i = i;
#endif
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = 1.0 / (60 * 31 * 256);     /* approx */
	ui->ui_flags = 0;
	udip[ui->ui_ctlr][ui->ui_slave] = ui;
	/* check to see if the drive is a available if it is bring it online */
	/* if not then just return.  open will try an online later */
	if(ra_info[ui->ui_unit].rastatus != M_ST_AVLBL)
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
	while(ui->ui_flags == 0 && ra_info[ui->ui_unit].ratype != 0);
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
	register struct mscp *mp;
	register struct uba_ctlr *um;
	struct udadevice *udaddr;
	int s,i;
	extern quota;
	
#ifdef lint
	flag = flag; i = i;
#endif
	unit = minor(dev) >> 3;
	if (unit >= nNRA || (ui = uddinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	sc = &uda_softc[ui->ui_ctlr];
	s = spl5();
	if (sc->sc_state != S_RUN) {
		if (sc->sc_state == S_IDLE)
			if(!udinit(ui->ui_ctlr)){
				printf("uda: Controller failed to init\n");
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
	/* check to see if the device is really there. */
	/* this code was taken from Fred Canters 11 driver */
	um = ui->ui_mi;
	udaddr = (struct udadevice *) um->um_addr;
	(void) splx(s);
	if(ui->ui_flags == 0){
		s = spl5();
		while(0 ==(mp = udgetcp(um))){
			uda_cp_wait++;
			sleep(&uda_cp_wait,PSWP+1);
			uda_cp_wait--;
		}
		mp->mscp_opcode = M_OP_ONLIN;
		mp->mscp_unit = ui->ui_slave;
		mp->mscp_cmdref = (long) & ra_info[ui->ui_unit].ratype;
			/* need to sleep on something */
#ifdef	DEBUG
		printd("uda: bring unit %d online\n",ui->ui_unit);
#endif	
		*((long *) mp->mscp_dscptr ) |= UDA_OWN | UDA_INT ;
		i = udaddr->udaip;
		timeout(wakeup,(caddr_t) mp->mscp_cmdref,10 * hz);
			/* make sure we wake up */
		sleep((caddr_t) mp->mscp_cmdref,PSWP+1); /*wakeup in udrsp() */
		(void) splx(s);
	}
	if(ui->ui_flags == 0){
		return(ENXIO);  /* Didn't go online */
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

udstrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	register struct buf *dp;
	register int unit;
	register struct size    *rasizes;
	int xunit = minor(bp->b_dev) & 07;
	daddr_t sz, maxsz;
	int s;

	sz = (bp->b_bcount+511) >> 9;
	unit = dkunit(bp);
	if (unit >= nNRA)
		goto bad;
	rasizes = ra_info[unit].ra_sizes;
	ui = uddinfo[unit];
	um = ui->ui_mi;
	if (ui == 0 || ui->ui_alive == 0)
		goto bad;
	if ((maxsz = rasizes[xunit].nblocks) < 0)
		maxsz = ra_info[unit].radsize - rasizes[xunit].blkoff;
	if (bp->b_blkno < 0 || bp->b_blkno+sz > maxsz ||
	    rasizes[xunit].blkoff >= ra_info[unit].radsize)
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
	struct  size    *rasizes;
	struct udadevice *udaddr;
	struct  uda     *ud = &uda[um->um_ctlr];
	int i;

	sc = &uda_softc[um->um_ctlr];
	
loop:
	if ((dp = um->um_tab.b_actf) == NULL) {
		/*
		 * Release uneeded UBA resources and return
		 */
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
		goto loop;		/* Need to check for loop */
	}
	um->um_tab.b_active++;
	udaddr = (struct udadevice *)um->um_addr;
	if ((udaddr->udasa&UDA_ERR) || sc->sc_state != S_RUN) {
		harderr(bp, "ra");
		mprintf("Uda%d udasa %o, state %d\n",um->um_ctlr , udaddr->udasa&0xffff, sc->sc_state);
		udinit(um->um_ctlr);
		/* SHOULD REQUEUE OUTSTANDING REQUESTS, LIKE UDRESET */
		return (0);
	}
	ui = uddinfo[dkunit(bp)];
	rasizes = ra_info[ui->ui_unit].ra_sizes;
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
		if(dp->b_qsize != 0){
			return(0); /* When a command completes and */
				   /* frees a bdp udstart will be called */
		}
		if ((mp = udgetcp(um)) == NULL){
			return (0);
		}
		mp->mscp_opcode = M_OP_GTUNT;
		mp->mscp_unit = ui->ui_slave;
		*((long *)mp->mscp_dscptr) |= UDA_OWN|UDA_INT;
		if (udaddr->udasa&UDA_ERR)
			printf("Uda(%d) udasa (%x)\n",um->um_ctlr, udaddr->udasa&0xffff);
		i = udaddr->udaip;      /* initiate polling */
		return(1);              /* wait for interrupt */
	}
	if ((mp = udgetcp(um)) == NULL) {
		ubarelse(um->um_ubanum,&i);
		return(0);
	}
	mp->mscp_cmdref = (long)bp;     /* pointer to get back */
	mp->mscp_opcode = bp->b_flags&B_READ ? M_OP_READ : M_OP_WRITE;
	mp->mscp_unit = ui->ui_slave;
	mp->mscp_lbn = bp->b_blkno + rasizes[minor(bp->b_dev)&7].blkoff;
	mp->mscp_bytecnt = bp->b_bcount;
	mp->mscp_buffer = (i & 0x3ffff) | (((i>>28)&0xf)<<24);
#if defined(VAX750)
	if (cpu == VAX_750)
		i &= 0xfffffff;         /* mask off bdp */
#endif
	bp->b_ubinfo = i;               /* save mapping info */
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

#ifdef	DEBUG
	printd10("udintr: state %d, udasa %o\n", sc->sc_state, udaddr->udasa);
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
		    (cpu == VAX_780 ? UDA_PI : 0);
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
#ifdef	DEBUG
		printd("Uda%d Version %d model %d\n",d,udamicro[d]&0xF,
			(udamicro[d]>>4) & 0xF);
		/*
		 * Requesting the error status (|= 2)
		 * may hang older controllers.
		 */
		udaddr->udasa = UDA_GO | (udaerror? 2 : 0);
#endif
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
		/*
		 * THIS IS A KLUDGE.
		 * Maybe we should change the entire
		 * UBA interface structure.
		 */
		int s = spl6();		/* was spl7 but I don't like turning */
					/* off machine checks */
		i = um->um_ubinfo;
#ifdef	DEBUG
		printd("uda: purge bdp %d\n", ud->uda_ca.ca_bdp);
#endif		
		um->um_ubinfo = ud->uda_ca.ca_bdp<<28;
		ubapurge(um);
		um->um_ubinfo = i;
		(void) splx(s);
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
		wakeup(&uda_cp_wait);
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
	struct buf *dp, *bp,nullbp;
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
	switch (mp->mscp_opcode) {

	case M_OP_ONLIN|M_OP_END:
		ra_info[ui->ui_unit].rastatus = st;
		ra_info[ui->ui_unit].ratype =  mp->mscp_mediaid;
		dp = &udutab[ui->ui_unit];
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
			ra_info[ui->ui_unit].radsize=(daddr_t)mp->mscp_untsize;
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
			switch(mp->mscp_mediaid & 0x7f){
			case    25:
				ra_info[ui->ui_unit].ra_sizes = ra25_sizes;
				break;
			case    60:
				ra_info[ui->ui_unit].ra_sizes = ra60_sizes;
				break;
			case    80:
				ra_info[ui->ui_unit].ra_sizes = ra80_sizes;
				break;
			case    81:
				ra_info[ui->ui_unit].ra_sizes = ra81_sizes;
				break;
			default:
				ui->ui_flags = 0;       /* mark it offline */
				ra_info[ui->ui_unit].ratype = 0;
				printf("Don't have a parition table for ");
				printf("a %c%c %c%c%c%d\n"
				,F_to_C(mp,4),F_to_C(mp,3),F_to_C(mp,2)
				,F_to_C(mp,1),F_to_C(mp,0)
				,mp->mscp_mediaid & 0x7f);
				while (bp = dp->b_actf) {
					dp->b_actf = bp->av_forw;
					bp->b_flags |= B_ERROR;
					iodone(bp);
				}
			}
			dp->b_active = 1;
		} else {
			if(dp->b_actf){
				harderr(dp->b_actf,"ra");
			} else {
				nullbp.b_blkno = 0;
				nullbp.b_dev = makedev(UDADEVNUM,ui->ui_unit);
				harderr(&nullbp, "ra");
			}
			printf("OFFLINE\n");
			while (bp = dp->b_actf) {
				dp->b_actf = bp->av_forw;
				bp->b_flags |= B_ERROR;
				iodone(bp);
			}
		}
		if(mp->mscp_cmdref!=NULL){/* Seems to get lost sometimes */
			wakeup((caddr_t *) mp->mscp_cmdref);
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
		ra_info[ui->ui_unit].ratype =  mp->mscp_mediaid;
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
		if (cpu == VAX_750
		    && udwtab[um->um_ctlr].av_forw == &udwtab[um->um_ctlr]) {
			if (um->um_ubinfo == 0)
				printf("udintr: um_ubinfo == 0\n");
			else
				ubarelse(um->um_ubanum, &um->um_ubinfo);
		}
#endif
		dp = &udutab[ui->ui_unit];
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
		ra_info[ui->ui_unit].rastatus = st;
		ra_info[ui->ui_unit].ratype =  mp->mscp_mediaid;
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
	register int unit = minor(dev) >> 3;

	if (unit >= nNRA)
		return (ENXIO);
	return (physio(udstrategy, &rudbuf[unit], dev, B_READ, minphys, uio));
}

udwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = minor(dev) >> 3;

	if (unit >= nNRA)
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
		for (unit = 0; unit < nNRA; unit++) {
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
	struct  size    *rasizes;
	unit = minor(dev) >> 3;
	if (unit >= nNRA)
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
	rasizes = ra_info[ui->ui_unit].ra_sizes;
	maxsz = rasizes[minor(dev)&07].nblocks;
	blkoff = rasizes[minor(dev)&07].blkoff;
	if(maxsz < 0)
		maxsz = ra_info[unit].radsize-blkoff;
	if (dumplo < 0 || dumplo + num >= maxsz)
		return (EINVAL);
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

#ifdef	lint
	i = i;
#endif

	udp->uda_Cmd.mscp_opcode = op;
	udp->uda_Rsp.mscp_header.uda_msglen = mscp_msglen;
	udp->uda_Cmd.mscp_header.uda_msglen = mscp_msglen;
	udp->uda_ca.ca_Rspdsc |= UDA_OWN|UDA_INT;
	udp->uda_ca.ca_Cmddsc |= UDA_OWN|UDA_INT;
	if (udaddr->udasa&UDA_ERR)
		printf("Udaerror udasa (%x)\n", udaddr->udasa&0xffff);
	i = udaddr->udaip;
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

udsize(dev)
	dev_t dev;
{
	int unit = minor(dev) >> 3;
	struct uba_device *ui;
	struct	size	*rasizes;

	if (unit >= nNRA || (ui = uddinfo[unit]) == 0 || ui->ui_alive == 0
		 || ui->ui_flags == 0)
		return (-1);
	rasizes = ra_info[ui->ui_unit].ra_sizes;
	return (rasizes[minor(dev) & 07].nblocks);
}

#endif
