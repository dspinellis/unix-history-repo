/*	vd.c	1.1	85/07/21	*/

#include "fsd.h"
#if NVD > 0
/*
**	VDDC Driver - Versabus to SMD direct interface version.
**		Written for TAHOE vmunix, CCI-WDC 9/1/83.		
*/

#include "../h/param.h"
#include "../h/buf.h"
#include "../h/cmap.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/dk.h"
#include "../h/map.h"
#include "../machine/mtpr.h"
#include "../machine/pte.h"
#include "../h/systm.h"
#include "../vba/vbavar.h"
#include "../h/user.h"
#include "../h/vmmac.h"
#include "../h/proc.h"
#include "../h/uio.h"
#include "../vba/vddc.h"

int	vddebug = 1;	/* if = 1, error messages are printed on the console */
int	vdintflg = 0;	/* if = 1, interrupts are handled by the driver, 
			 * otherwise they are just ignored. (during setup) */

static struct size FSD[] = {
    9600,       0,	/* minor 0/ 8/16/24 = fsd0a - fsd3a - cyl   0 -  59*/
   12000,    9600,	/* minor 1/ 9/17/25 = fsd0b - fsd3b - cyl  60 - 134*/
  108480,   21600,	/* minor 2/10/18/26 = fsd0c - fsd3c - cyl 135 - 812*/
    1600,  130080,	/* minor 3/11/19/27 = fsd0d - fsd3d - cyl 813 - 822*/
  130080,       0,	/* minor 4/12/20/28 = fsd0e - fsd3e - cyl   0 - 812*/
  131680,       0,	/* minor 5/13/21/29 = fsd0f - fsd3f - cyl   0 - 822*/
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
       0,	0,	/* Non existent minor device */
};

static struct size	SMD[]= {
    20064,	0, 	/* minor 32/40/48/56 = smd0a - smd3a cyl 0- 65 */
    13680,  20064, 	/* minor 33/41/49/57 = smd0b - smd3b cyl 66- 110 */
   214928,  33744, 	/* minor 34/42/50/58 = smd0c - smd3c cyl 111-817 */
     1520, 248672, 	/* minor 35/43/51/59 = smd0d - smd3d cyl 818-822 */
   248672,	0, 	/* minor 36/44/52/60 = smd0e - smd3e cyl 0-817 */
   250192,	0, 	/* minor 37/45/53/61 = smd0f - smd3f cyl 0-822 */
	0,	0, 	/* minor 38/46/54/62 = smd0g - smd3g */
	0,	0, 	/* minor 39/47/55/63 = smd0h - smd3h */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
};

static struct size XFSD[] = {
    20352,	0, 	/* minor 64/72/80/88 = xfsd0a - xfsd3a cyl 0- 52 */
    20352,  20352, 	/* minor 65/73/81/89 = xfsd0b - xfsd3b cyl 53- 105 */
   230400,  40704, 	/* minor 66/74/82/90 = xfsd0c - xfsd3c cyl 106-705 */
     1920, 271104, 	/* minor 67/75/83/91 = xfsd0d - xfsd3d cyl 706-710 */
   271104,	0, 	/* minor 68/76/84/92 = xfsd0e - xfsd3e cyl 0-705 */
   273024,	0, 	/* minor 69/77/85/93 = xfsd0f - xfsd3f cyl 0-710 */
	0,	0, 	/* minor 70/78/86/94 = xfsd0g - xfsd3g */
	0,	0, 	/* minor 71/79/87/95 = xfsd0h - xfsd3h */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
	0,	0,	/* Non existent minor device */
};

/*
/*
/* Layout of minor number assignments for the VDDC devices.
/*
/* 	  1
/*	  5		            3 2   0
/*	 +---------------------------+-----+
/*	 |     		 Unit number | FLS |
/*	 +---------------------------+-----+
/*			      	   |    |_____ File system # ( 0-7 )
/*			      	   |__________ Unit # in the system
/*
/********************************************************/

#define VDUNIT(x)	(minor(x) >> 3)
#define FLSYS(x)	(minor(x) & 0x07)
#define PHYS(x)		( vtoph( 0, (int) (x) ) )


/* Drive types should be in order of drive capacity for auto-configuration */
/* e.g: smallest capacity = drive type 0, highest capacity = type NXPDRV-1 */

struct vdst {
	short nsect;
	short ntrak;
	short nspc;
	short ncyl;
	struct size *sizes;
	short dtype;		/* type as in byte 5 (drive) of iopb */
	char	*name;		/* drive name for autoconf */
} vdst[] = {

16,	10,	16*10,	823,	FSD,	0,	"fsd",
16,	19,	16*19,	823,	SMD,	1, 	"smd",
16,	24,	16*24,  711,   XFSD,	2,	"xfd"
};


struct	vba_ctlr *vdminfo[NVD];
struct  vba_device *vddinfo[NFSD];

/*
**	Internal Functions
*/
int	vdopen();
int	vdclose();
int	vdprobe();		/* See if VDDC is really there */
int	vd_setup();		/* Called from vdprobe */
int	vdslave();		/* See if drive is really there */
int	vdattach();
int	vddgo();
int	vdstrategy();		/* VDDC strategy routine */
int	vdstart();		/* Top level interface to device queue */
int	vdintr();		/* Top Level ISR */
int	vdread();		/* raw i/o read routine */
int	vdwrite();		/* raw i/o write routine */
int	vddump();		/* dump routine */
int	vdsize();		/* sizes for swapconfig */
int	dskrst();		/* reset a drive after hard error */

long	vdstd[] = {
		0x0f2000 };

struct	vba_driver vddriver = {
	vdprobe, vdslave, vdattach, vddgo, vdstd, 
	"smd/fsd", vddinfo, "vd", vdminfo
};

struct	buf	vdutab[NFSD];
struct 	buf	rvdbuf[NFSD];
char	vdbuf[NVD][MAXBPTE * NBPG];	/* internal buffer for raw/swap i/o */
long	vdbufused[NVD];
extern char	vd0utl[],vd1utl[],vd2utl[],vd3utl[];

/*
**	Disk Address
*/
struct	dskadr	{
	char	track;		/* all 8 bits */
	char	sector;		/* low order 5 bits */
	short	cylinder;	/* low order 12 bits */
};

/*
**	DCB Trailer Formats
**********************************/

/*
**	Read / Write Trailer
*/
struct	trrw	{
	char	*memadr;			/* memory address */
	long	wcount;				/* 16 bit word count */
	struct	dskadr	disk;			/* disk address */
};

/*
**	Format Trailer
*/
struct	trfmt	{
	char	*addr;			/* data buffer to be filled on sector*/
	long	nsectors;		/* # of sectors to be formatted */
	struct	dskadr	disk;
	struct	dskadr  hdr;
};

/*
**	Reset / Configure Trailer
*/
struct	treset	{
	long	ncyl;		/* # cylinders */
	long	nsurfaces;	/* # surfaces */
};				/* # of sectors is defined by VDDC */
				/* to be 32/track of 512 data bytes each */

/*
**	Seek Trailer
*/
struct	trseek	{
	struct	dskadr	disk;
};

/*
**	DCB Format
*/
struct	fmt_dcb	{
	struct	fmt_dcb	*nxtdcb;	/* next dcb in chain or End of Chain */
	short	intflg;			/* interrupt settings and flags */
	short	opcode;			/* DCB Command code etc... */
	long	operrsta;		/* Error & Status info */
	short	fill;			/* not used */
	char	devselect;		/* Drive selection */
	char	trailcnt;		/* Trailer Word Count */
	long	err_memadr;		/* Error memory address */
	short	fill2;
	short	err_wcount;		/* Error word count */
	short	err_track;		/* Error track/sector */
	short	err_cyl;		/* Error cylinder adr */
	union	{
		struct	trrw	rwtrail;	/* read/write trailer */
		struct	trfmt	fmtrail;	/* format trailer */
		struct	treset	resetrail;	/* reset/configure trailer */
		struct	trseek	seektrail;	/* seek trailer */
	} trail;
};

/*
**	MDCB Format
*/
struct	fmt_mdcb	{
	struct	fmt_dcb	*firstdcb;	/* first dcb in chain */
	struct	fmt_dcb	*procdcb;	/* dcb being processed */
	struct	fmt_dcb	*intdcb;	/* dcb causing interrupt */
	long	vddcstat;		/* VDDC status */
}mdcbx[NVD];

/*
**	DCB
*/

struct	fmt_dcb		dcbx[NVD];

int vdtimeout;	
#define	POLLTILLDONE(x, name) { \
	vdtimeout = 1000*(x); \
	uncache((char *)&dcb->operrsta); \
	while (! (dcb->operrsta & DCBCMP)) { \
		DELAY(1000); \
		vdtimeout--; \
		uncache((char *)&dcb->operrsta); \
		if (vdtimeout <=0) { \
			printf("vd: timeout on %s\n", name);\
			return(0); \
		} \
	} \
}

/*
**	See if the controller is really there. 
**	if TRUE - initialize the controller.
*/
vdprobe(cntrl_vaddr)
caddr_t cntrl_vaddr;
{
	if ( badaddr(cntrl_vaddr,2) ) return(0); /* no controller */
	else 
		if (vd_setup(cntrl_vaddr))	/* initialize the controller */
			return(1);
		else return(0);		/* initialization error */	
}

vd_setup(cntrl_vaddr)
caddr_t cntrl_vaddr;
{
	register struct fmt_dcb *dcb = &dcbx[0];
	register struct fmt_mdcb *mdcb = &mdcbx[0];
	int j;

	VDDC_RESET(cntrl_vaddr);		/* Reset the controller */
		/* Burn some time ...... needed after accessing reset port */
	for (j=0; j<20; j++)
		DELAY(1000);

	/* setup & issue INIT to initialize VDDC */

	dcb->opcode = INIT;
	dcb->nxtdcb = (struct fmt_dcb *)0;
	dcb->intflg = NOINT;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	dcb->operrsta  = 0;
	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) );	/* do it */
	POLLTILLDONE(1,"INIT");		/* poll till done */
	if (dcb->operrsta & HRDERR) {
		if (vddebug)
			printf("vd: init error, err=%b\n",
			    dcb->operrsta, ERRBITS);
		return(0);
	}
	/* setup & issue DIAGNOSE */

	dcb->opcode = DIAG;
	dcb->nxtdcb = (struct fmt_dcb *)0;
	dcb->intflg = NOINT;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	dcb->operrsta  = 0;
	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
	POLLTILLDONE(1,"DIAG")		/* poll till done */
	if (dcb->operrsta & HRDERR) {
		if (vddebug)
			printf("vd: diagnose error, err=%b\n",
			    dcb->operrsta, ERRBITS);
		return(0);
	}
	/* Start drives command */
#ifdef notdef
	dcb->opcode = VDSTART;
	dcb->nxtdcb = (struct fmt_dcb *)0;
	dcb->intflg = NOINT;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	dcb->operrsta  = 0;
	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
	POLLTILLDONE(20,"VDSTART")		/* poll till done */
	if (dcb->operrsta & HRDERR) {
		if (vddebug)
			printf("vd: start error, err=%b\n",
			    dcb->operrsta, ERRBITS);
		return(0);
	}
#endif
	return(1);
   }

/*
 * See if a drive is really there
 * Try to Reset/Configure the drive, then test its status.
*/
vdslave(ui,cntrl_vaddr)
register struct vba_device *ui;
register caddr_t cntrl_vaddr;
{
	register struct fmt_dcb	*dcb = &dcbx[0];
	register struct fmt_mdcb *mdcb = &mdcbx[ui->ui_ctlr];
	register struct vdst *st;
	int dsktype;

	/*
	**  check drive status - see if drive exists. 
	*/
	dcb->opcode = VDSTATUS;
	dcb->intflg = NOINT;
	dcb->operrsta  = 0;
	dcb->devselect = (char)ui->ui_slave;
	dcb->trailcnt = (char)0;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	mdcb->vddcstat = 0;
	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb))	/* do it */
	POLLTILLDONE(5,"VDSTATUS")
#ifdef notdef
	if (dcb->operrsta & HRDERR) {
		if (vddebug) 
		  printf("vd%d: status error, err=%b\n", ui->ui_unit,
		      dcb->operrsta, ERRBITS);
		return(0);
	}
#endif
	uncache((char *)&mdcb->vddcstat);
	if (mdcb->vddcstat & DRVNRDY) return(0); /* not ready-> non existent */

	/*
	 * drive is alive, now get its type!
	 * Seek on all drive types starting from the largest one.
	 * a sucessful seek to the last sector/cylinder/track verifies 
	 * the drive type connected to this port. 
	 */
	for (dsktype = NVDDRV-1; dsktype >= 0; dsktype--) {	
		st = &vdst[dsktype];
		dcb->opcode = RSTCFG;		/* configure drive command */
		dcb->intflg = NOINT;
		dcb->operrsta  = 0;
		dcb->trail.resetrail.ncyl = st->ncyl;
		dcb->trail.resetrail.nsurfaces = st->ntrak;
		dcb->devselect = (char)ui->ui_slave;
		dcb->trailcnt = (char)2;
		mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
		VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
		POLLTILLDONE(3,"RSTCFG")
		if (dcb->operrsta & HRDERR) {
			if (vddebug) 
				printf("vd%d: reset error, err=%b\n", 
				    ui->ui_unit, dcb->operrsta, ERRBITS);
			return(0);
		}
		mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
		dcb->intflg = NOINT;
		dcb->opcode =  RD;
		dcb->operrsta = 0;
		dcb->devselect = (char)ui->ui_slave;
		dcb->trailcnt = (char)3;
		dcb->trail.rwtrail.memadr = (char *)PHYS(vdbuf);
		dcb->trail.rwtrail.wcount = 256;
		dcb->trail.rwtrail.disk.cylinder = st->ncyl -4;
		dcb->trail.rwtrail.disk.track = st->ntrak -1;
		dcb->trail.rwtrail.disk.sector = 0;
		VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
		POLLTILLDONE(5,"RD")
if (vddebug) 
	printf("vd%d: cyl %d, trk %d, sec %d, operrsta err=%b\n", 
	   ui->ui_unit, 
	   dcb->trail.rwtrail.disk.cylinder,
	   dcb->trail.rwtrail.disk.track,
	   dcb->trail.rwtrail.disk.sector,
	   dcb->operrsta, ERRBITS);
		if ( (dcb->operrsta & HRDERR) == 0) 
		/* found the drive type! */
			break;
	}
	if (dsktype < 0) {
		/* If reached here, a drive which is not defined in the 
		 * 'vdst' tables is connected. Cannot set it's type.
		 */
		printf("vd%d: unrecognized drive type\n", ui->ui_unit);
		return(0);
	}
	ui->ui_type = dsktype;
	vddriver.ud_dname = st->name;
	return(1);
}

vdattach(ui)
struct vba_device *ui;
{
	if (ui->ui_dk >= 0)
		dk_mspw[ui->ui_dk] = .0000020345;	/* BAD VALUE */
}

vddgo(um)
struct vba_ctlr *um;
{
}

vdstrategy(bp)
register struct buf *bp;
{
	register struct vba_device *ui;
	register struct vba_ctlr *um;
	register int unit;
	register struct buf *dp;
	register struct size *sizep;
	int index, blocks, s;

	vdintflg = 1;		/* enable interrupts handling by the driver */
	blocks = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	unit = VDUNIT(bp->b_dev);
	ui = vddinfo[unit];
	if (ui == 0 || ui->ui_alive == 0) goto bad1;
	index = FLSYS(bp->b_dev); /* get file system index */
	sizep = vdst[ui->ui_type].sizes;
	if (bp->b_blkno < 0 ||
	 (dkblock(bp)+blocks > sizep[index].nblocks))	/* disk overflow */
		goto bad1;
	s = spl8();
	dp = &vdutab[ui->ui_unit];
	bp->b_resid = bp->b_blkno + sizep[index].block0; 
					/* block # plays same role as
					   cylinder # for disksort, as long
					   as increasing blocks correspond to
					   increasing cylinders on disk */

	buf_setup (bp, SECTSIZ);

	disksort(dp, bp);
	if (dp->b_active == 0) {	/* unit is on controller queue */
		/* put the device on the controller queue */
		dp->b_forw = NULL;		/* end of queue indicator */
		um = ui->ui_mi;		/* get controller structure !! */
		if (um->um_tab.b_actf == NULL)	/* controller queue is empty */
			um->um_tab.b_actf = dp; 
		else
			um->um_tab.b_actl->b_forw = dp; /* add into queue */
		um->um_tab.b_actl = dp;		/* update queue tail */
		dp->b_active ++;
	}
	bp = &ui->ui_mi->um_tab;	/* controller structure addr */
	if (bp->b_actf && 		/* cntrl queue not empty */
		bp->b_active == 0)	/* controller not active */
		(void) vdstart(ui->ui_mi);/* go start I/O */
	splx(s);
	return;

bad1:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}


/*
 * Start up a transfer on a drive.
 */
vdstart(um)
register struct vba_ctlr *um;
{
	register struct buf *bp, *dp;
	register struct fmt_dcb *dcb = &dcbx[um->um_ctlr];
	register struct fmt_mdcb *mdcb;
	register struct size *sizep;	/* Pointer to one of the tables */
	register struct vdst *st;
	register int index ;		/* Index in the relevant table */
	register int phadr;		/* Buffer's physical address */
	register caddr_t cntrl_vaddr = um->um_addr;
	int	sblock, unit;
	int ct;

loop:
	/*
	 * Pull a request off the controller queue
	 */
	if ((dp = um->um_tab.b_actf) == NULL)
		return ;
	if ((bp = dp->b_actf) == NULL) {
		dp->b_active = 0;	/* device removed from ctlr queue */
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	/*
		 * Mark controller busy, and
		 * prepare a command packet for the controller.
		 */
	um->um_tab.b_active++;
	unit = VDUNIT(bp->b_dev);
	st = &vdst[vddinfo[unit]->ui_type];
	mdcb = &mdcbx[vddinfo[unit]->ui_ctlr];
	index = FLSYS(bp->b_dev);
	sizep = st->sizes;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	dcb->intflg = INTDUN;		/* interrupt on completion */
	dcb->opcode = (bp->b_flags & B_READ) ? RD : WD;
	dcb->operrsta = 0;
	dcb->devselect = (char)(vddinfo[unit])->ui_slave;
	dcb->trailcnt = (char)3;
	ct = vddinfo[unit]->ui_ctlr;

	switch (ct) {
		case 0:
			phadr = get_ioadr(bp, vdbuf[0], VD0map, (caddr_t)vd0utl);
			break;
		case 1:
			phadr = get_ioadr(bp, vdbuf[1], VD1map, (caddr_t)vd1utl);
			break;
		case 2:
			phadr = get_ioadr(bp, vdbuf[2], VD2map, (caddr_t)vd2utl);
			break;
		case 3:
			phadr = get_ioadr(bp, vdbuf[3], VD3map, (caddr_t)vd3utl);
			break;
	}
/*
	phadr = get_ioadr(bp, vdbuf, IOmap, (caddr_t)ioutl);
*/

	if (vddinfo[unit]->ui_dk >= 0) {
		int dku = vddinfo[unit]->ui_dk;
		dk_busy |= 1<<dku;
		dk_xfer[dku]++;
		dk_wds[dku] += bp->b_bcount>>6;
	}
	dcb->trail.rwtrail.memadr = (char *)phadr;
	dcb->trail.rwtrail.wcount = (bp->b_bcount + 1) / 2;
	sblock = sizep[index].block0 + bp->b_blkno;
	dcb->trail.rwtrail.disk.cylinder = (short)(sblock / st->nspc);
	dcb->trail.rwtrail.disk.track = (char)((sblock % st->nspc) / st->nsect);
	dcb->trail.rwtrail.disk.sector = (char)(sblock*2 % (st->nsect*2));

#ifdef VDDCPERF
	scope_out(1);
#endif

	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb))	/* do it */
}


/*
 * Handle a disk interrupt.
 */
vdintr(vdnum)
register vdnum;
{
	register struct buf *bp, *dp;
	register struct vba_ctlr *um = vdminfo[vdnum];	
	register struct fmt_dcb *dcb = &dcbx[vdnum];
	register struct fmt_mdcb *mdcb = &mdcbx[vdnum];
	register struct vdst *st;
	int unit;
	struct vba_device *ui;

#ifdef VDDCPERF
	scope_out(2);
#endif
	if (intenable == 0 || vdintflg == 0) 	/* ignore all interrupts */
		return;
	if (um->um_tab.b_active == NULL) return;/* unexpected interrupt */
	uncache((char *)&mdcb->intdcb);
	uncache((char *)&dcb->operrsta);
	if ( mdcb->intdcb != (struct fmt_dcb *)PHYS(dcb)) {	/* dcb causing interrupt */
		printf("vd%d: bad dcb=%x (phys=%x)\n",
		    vdnum, mdcb->intdcb, PHYS(dcb));
		return;
	}
	if (! (dcb->operrsta & DCBCMP))	{ /* unexpected interrupt */
		printf("vd%d: unexpected interrupt, err=%b\n", vdnum,
		    dcb->operrsta, ERRBITS);
		return;
	}
	dp = um->um_tab.b_actf;		/* device queue head in ctlr queue */
	bp = dp->b_actf;		/* first buffer in device queue */
	unit = VDUNIT(bp->b_dev);
	ui = vddinfo[unit];
	if (ui->ui_dk >= 0)
		dk_busy &= ~(1 << ui->ui_dk);
	if (dcb->operrsta & (HRDERR|SFTERR)) {
		st = &vdst[ui->ui_type];
		if (dcb->operrsta & HRDERR) {
			harderr(bp, &st->name[7]);
			printf("status=%b\n", dcb->operrsta, ERRBITS);
			dskrst(bp);
			bp->b_flags |= B_ERROR;
		} else
#define	SECTOR(x)	((x)*2)
			printf("%s%d: soft error sn%d status=%b\n", &st->name[7], unit,
			   SECTOR(bp->b_blkno + st->sizes[FLSYS(bp->b_dev)].block0),
			   dcb->operrsta, ERRBITS);
	}
	switch (vdnum) {
		case 0:
			end_transfer(bp, vdbuf[0], VD0map, (caddr_t)vd0utl);
			break;
		case 1:
			end_transfer(bp, vdbuf[1], VD1map, (caddr_t)vd1utl);
			break;
		case 2:
			end_transfer(bp, vdbuf[2], VD2map, (caddr_t)vd2utl);
			break;
		case 3:
			end_transfer(bp, vdbuf[3], VD3map, (caddr_t)vd3utl);
			break;
	}

	um->um_tab.b_active = 0;
	um->um_tab.b_errcnt = 0;
	if (dp->b_forw != NULL) {		/* more than 1 unit on queue */
		um->um_tab.b_actf = dp->b_forw;	/* next device on ctlr queue */
		dp->b_forw = um->um_tab.b_actl->b_forw;	/* be last in queue */
		um->um_tab.b_actl->b_forw = dp;	/* last points now to dp */
		um->um_tab.b_actl = dp;		/* pointer in ctlr structure */
	}
	dp->b_errcnt = 0;
	dp->b_actf = bp->av_forw;		/* remove first from queue */
	bp->b_resid = 0;	/* All data read here */

#ifdef VDDCPERF
	scope_out(3);
#endif

	iodone(bp);
	vdstart(um);		/* start requests for next device on queue */
}


vdread(dev, uio)
dev_t dev;
struct uio *uio;
{
	register int unit = VDUNIT(dev);
	register int error;
	register int ct;
	register int s;

	if (unit >= NFSD)
		error = ENXIO;
	else {
		ct = vddinfo[unit]->ui_ctlr;
		s = spl8();
		while (vdbufused[ct]) sleep (&vdbufused[ct],PRIBIO+1);
		vdbufused[ct] = 1;
		splx(s);
		error = physio(vdstrategy, &rvdbuf[unit], dev, B_READ, minphys, uio);
		vdbufused[ct] = 0;
		wakeup (&vdbufused[ct]);
	}
	return error;
}

vdwrite(dev, uio)
dev_t dev;
struct uio *uio;
{
	register int unit = VDUNIT(dev);
	register int error;
	register int ct;
	register int s;

	if (unit >= NFSD)
		error = ENXIO;
	else {
		ct = vddinfo[unit]->ui_ctlr;
		s = spl8();
		while (vdbufused[ct]) sleep (&vdbufused[ct],PRIBIO+1);
		vdbufused[ct] = 1;
		splx(s);
		error = physio(vdstrategy, &rvdbuf[unit], dev, B_WRITE, minphys, uio);
		vdbufused[ct] = 0;
		wakeup (&vdbufused[ct]);
	}
	return error;
}

#define	DUMPSIZE	32	/* Up to 32k at a time - controller limit */

vddump(dev)
dev_t	dev;
/*
 * Dump the main memory to the given device.
 */
{
	register struct vba_ctlr *um;
	register struct fmt_dcb *dcb = &dcbx[0];
	register struct fmt_mdcb *mdcb = &mdcbx[0];
	register struct vdst *st;
	register int unit;
	register caddr_t cntrl_vaddr ;
	register struct size *sizep;	/* Pointer to one of the tables */
	int index,sblock,blkcount,thiscount;
	int	memaddr;

	unit = VDUNIT(dev);
	um = (vddinfo[unit])->ui_mi;
	st = &vdst[(vddinfo[unit])->ui_type];
	dcb = &dcbx[um->um_ctlr];
	cntrl_vaddr = um->um_addr;
	memaddr = 0x0;
	index = FLSYS(dev);
	sizep = st->sizes;
	blkcount = maxfree - 2;		/* In 1k byte pages */
	if (dumplo + blkcount > sizep[index].nblocks) return(EINVAL);
	sblock = sizep[index].block0 + dumplo;
	while (blkcount > 0) {
		thiscount = MIN (blkcount, DUMPSIZE);
		mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
		dcb->intflg = NOINT;
		dcb->opcode = WD;
		dcb->operrsta = 0;
		dcb->devselect = (char)(vddinfo[unit])->ui_slave;
		dcb->trailcnt = (char)3;
		dcb->trail.rwtrail.memadr = (char *)memaddr;
		dcb->trail.rwtrail.wcount = thiscount*512;
		dcb->trail.rwtrail.disk.cylinder= (short)(sblock/st->nspc);
		dcb->trail.rwtrail.disk.track = (char)((sblock % st->nspc) 
			/ st->nsect);
		dcb->trail.rwtrail.disk.sector = (char)(sblock*2 % (st->nsect*2));
		VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
		POLLTILLDONE(5,"WD");
		if (dcb->operrsta & HRDERR) {
			if (vddebug)
				printf("vd%d: i/o error, err=%b\n", unit,
				    dcb->operrsta, ERRBITS);
			return(EIO);
		};
		blkcount -= thiscount;
		memaddr += thiscount*NBPG;
		sblock += thiscount;
	}
	return(0);
}

vdopen(dev, flag)
register dev_t dev;
int flag;
{
	register struct vba_device *ui;
	register unit = VDUNIT(dev);

	ui = vddinfo[unit];
	if (ui == 0 || ui->ui_alive == 0 || ui->ui_type >= NVDDRV)
		return ENXIO;
	return 0;
}

vdsize(dev)
register dev_t dev;
{
	register struct vba_device *ui;
	register unit = VDUNIT(dev);

	ui = vddinfo[unit];
	if (ui == 0 || ui->ui_alive == 0 || ui->ui_type >= NVDDRV)
		return -1;
	return vdst[ui->ui_type].sizes[FLSYS(dev)].nblocks;
}

/* reset a drive after a hard error */
dskrst(bp)
	register struct buf *bp;
{
	register struct vdst *st;
	register struct fmt_dcb *dcb;
	register struct fmt_mdcb *mdcb;
	register struct vba_device *ui;
	register caddr_t cntrl_vaddr ;
	int unit;

	unit = VDUNIT(bp->b_dev);
	ui = vddinfo[unit];
	mdcb = &mdcbx[ui->ui_ctlr];
	dcb = &dcbx[ui->ui_ctlr];
	cntrl_vaddr = (ui->ui_mi)->um_addr;
	st = &vdst[vddinfo[unit]->ui_type];
	dcb->opcode = RSTCFG;		/* configure drive command */
	dcb->intflg = NOINT;
	dcb->operrsta  = 0;
	dcb->trail.resetrail.ncyl = st->ncyl;
	dcb->trail.resetrail.nsurfaces = st->ntrak;
	dcb->devselect = (char)ui->ui_slave;
	dcb->trailcnt = (char)2;
	mdcb->firstdcb = (struct fmt_dcb *)PHYS(dcb);
	VDDC_ATTENTION(cntrl_vaddr,PHYS(mdcb) )	/* do it */
	POLLTILLDONE(3,"reset")
	if (dcb->operrsta & HRDERR) {
		if (vddebug) {
			harderr(bp, &st->name[7]);
			printf("reset failed, err=%b\n", dcb->operrsta,ERRBITS);
		}
	}
}
#endif
