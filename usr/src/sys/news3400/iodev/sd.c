/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: sd.c,v 4.300 91/06/27 20:42:56 root Rel41 $ SONY
 *
 *	@(#)sd.c	7.4 (Berkeley) %G%
 */
#define	dkblock(bp)	bp->b_blkno

/*
 * Copyright (c) 1987-1991 by SONY Corporation.
 */

#include "sd.h"
#if NSD > 0

#include <machine/fix_machine_type.h>

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/dkstat.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/reboot.h>
#include <sys/ioctl.h>
#include <sys/systm.h>
#include <sys/mtio.h>
#include <sys/stat.h>
#include <sys/disklabel.h>
#include <vm/vm.h>
#include <sys/syslog.h>

#include <ufs/ffs/fs.h>

# include <machine/cpu.h>

#ifdef IPC_MRX
# include "../iop/iopvar.h"
# include "../ipc/newsipc.h"
#endif

#ifdef CPU_SINGLE
# include <news3400/hbdev/hbvar.h>
# include <news3400/iodev/ioptohb.h>
#endif

#include <news3400/iodev/scsireg.h>
#include <news3400/iodev/scu.h>
#include <news3400/iodev/dkio.h>
#include <news3400/iodev/sdreg.h>
/* #ifdef DISKINFO KU:XXX */
#include <news3400/iodev/diskinfo.h>
/* #endif /* DISKINFO */

#define	sce_sdecode	sce_hdecode

#define	dev2unit(x)	((minor(x) & ~0x80) >> 3)
#define	dev2part(x)	(minor(x) & 0x7)

/* /sys/sys/file.h */
#define	FREAD		00001		/* descriptor read/receive'able */
#define	FWRITE		00002		/* descriptor write/send'able */

#define	PART_A		0
#define	PART_B		1
#define	PART_C		2
#define	PART_D		3
#define	PART_E		4
#define	PART_F		5
#define	PART_G		6
#define	PART_H		7

#define	MAXPROBERETRY	100
#define	NRETRY		10
#define	MAXHRDERR	100
#define	MAXRETRYCNT	16

#define	SDBSIZE1K	(DEV_BSIZE * 2)
#define	MAXSDPHYS	((NSCMAP - 1) * NBPG)

#define	D100MSEC	100000

#if OD_STOPTIME < 1
# define	OD_STOPTIME	5
#endif /* OD_STOPTIME < 1 */

#define FORMAT_MODE_CORRUPTED	0x31
#define	ONLY_ONE	1

/************** PARTITIONS *************************************/

#define	PART_UNUSED	(0)
#define	PART_SPEC	(-1)
#define	PART_CALCF	(-2)
#define	PART_CALCG	(-3)

struct defpart {
	int range_min;
	int range_max;
	int partsize[PNUM];
};

struct defpart defpart_std[] = {
	{
		0,		/* range_min */
		20,		/* range_max */

		  PART_SPEC,		/* A: */
		PART_UNUSED,		/* B: */
		  PART_SPEC,		/* C: */
		PART_UNUSED,		/* D: */
		PART_UNUSED,		/* E: */
		PART_UNUSED,		/* F: */
		PART_UNUSED,		/* G: */
		PART_UNUSED,		/* H: */
	},
	{
		20,		/* range_min */
		61,		/* range_max */

		      15884,		/* A: */
		      10032,		/* B: */
		  PART_SPEC,		/* C: */
		      15884,		/* D: */
		PART_UNUSED,		/* E: */
		 PART_CALCF,		/* F: */
		 PART_CALCG,		/* G: */
		PART_UNUSED,		/* H: */
	},
	{
		61,		/* range_min */
		206,		/* range_max */

		      15884,		/* A: */
		      33440,		/* B: */
		  PART_SPEC,		/* C: */
		      15884,		/* D: */
		      55936,		/* E: */
		 PART_CALCF,		/* F: */
		 PART_CALCG,		/* G: */
		PART_UNUSED,		/* H: */
	},
	{
		206,		/* range_min */
		356,		/* range_max */

		      15884,		/* A: */
		      33440,		/* B: */
		  PART_SPEC,		/* C: */
		      15884,		/* D: */
		      55936,		/* E: */
		 PART_CALCF,		/* F: */
		 PART_CALCG,		/* G: */
		     291346,		/* H: */
	},
	{
		356,		/* range_min */
		99999999,	/* range_max */

		      15884,		/* A: */
		      66880,		/* B: */
		  PART_SPEC,		/* C: */
		      15884,		/* D: */
		     307200,		/* E: */
		 PART_CALCF,		/* F: */
		 PART_CALCG,		/* G: */
		     291346,		/* H: */
	},
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
};

/************* ADDITIONAL SENSE ERROR CODES *************************/

struct msg_list {
	int	ml_code;		/* message code */
	int	ml_msglvl;		/* message level */
	char	*ml_msgstr;		/* message string */
};

#define	sdskeylist	skeylist

struct msg_list ecodelist_mo[] = {
	{ 0x80, 0, NULL },
	{ 0x81, 0, NULL },
	{ 0x82, 0, NULL },
	{ 0x83, 0, NULL },

	{ -1,   0, NULL }
};

/************** Ref. sd_var.c ********************************/

extern struct iop/**/_ctlr *sdminfo[];
extern struct iop/**/_device *sddinfo[];
extern struct iop/**/_device *sdip[][MAXSLAVE];

extern struct buf rsdbuf[];		/* buffer for raw I/O */
extern struct buf csdbuf[];		/* buffer for controll */
extern struct buf sdutab[];		/* per drive buffers */

extern struct sdc_softc sdc_softc[];
extern struct sdd_softc sdd_softc[];
extern u_char sd_b_openf[][PNUM];
extern u_char sd_c_openf[][PNUM];

extern struct scsi kernscsi[];
extern struct sdst sdstdrv[];
extern struct disklabel sdlabel[];
extern struct size sdsizedrv[][PNUM];

extern u_char sdc_rsense[][RSEN_CNT];

extern struct sync_param sd_sync_param[];

extern int nsd;
extern int nsdc;

/************** Ref. sddefs.c *********************************/

extern struct sddevinfo sddevinfo[];

/**************************************************************/

extern struct msg_list skeylist[];
extern struct msg_list ecodelist[];

extern int	boothowto;
extern int	rsense_msg_disp;	/* RSENSE-message display flag */
extern int	mo_disp_format;		/* MO format mode display flag */

int sd_ignore_error;

static int re_init_done;

static u_char sdwork[2340];		/* buffer for error recovery */
static u_char sdtmp[DEV_BSIZE];		/* buffer for temporary */
#ifdef mips
volatile static int sdtmp_stat = 0;	/* status of sdtmp */
#else
static int sdtmp_stat = 0;		/* status of sdtmp */
#endif

char	pname[] = "abcdefgh";

struct scsi *get_scsi();
struct sc_map *get_sc_map();
struct sc_inq *get_sc_inq();

int	sdprobe(), sdslave(), sdattach(), sddgo(), sdintr();
int	sdwstart, sdwatch(), sdstop();	/* Have started guardian */
void	sdexec();

static sd_check(), sd_tstdrv(), sd_other_pages(), sd_err_rcv(), sd_synctr_on();
static disklabel2sdst(), sdst2disklabel(), sd_scu_exec();

#ifdef CPU_SINGLE
struct hb_driver sdcdriver =
    {sdprobe, sdslave, sdattach, sddgo, sdintr, "sd", sddinfo, "sdc", sdminfo};
#else
struct iop_driver sdcdriver =
    {sdprobe, sdslave, sdattach, sddgo, "sd", sddinfo, "sdc", sdminfo};
#endif

/*ARGSUSED*/
sdprobe(im)
	struct iop/**/_ctlr *im;
{
	static int sdd_init = 0;
	register struct sc_inq *sci;
	register int ctlr;
	register int fw;
	int i;

	if (sdd_init == 0) {
		sdd_init++;
		for (i = 0; i < nsd; i++)
			sdd_softc[i].sdd_start = -2;
	}

	sci = get_sc_inq(im->im_intr);
	ctlr = im->im_ctlr;
	/*
	 * Check device type
	 *	0x00: Direct access device.
	 *	0x01: Sequential access device.
	 *	0x04: Write-once read-multiple device.
	 *	0x05: Read-only Direct-access device.
	 *	0x7f: Specified device is nonexistent.
	 */
	fw = sdc_softc[ctlr].sdc_firmware & ~SDCFW_DEVMASK;

	switch (sci->sci_devtype) {

	case 0x00:
		/*
		 * Assumed that the device is HD.
		 *	Later, distinguish MO from HD.
		 */
		sdc_softc[ctlr].sdc_firmware = fw | SDCFW_HD;
		break;

	default:
		/*
		 * device type mis-match
		 */
		return (0);
	}

	/*
	 * Set interrupt handler routine
	 */
	if (set_inthandler(im, sdintr) == 0)
		return (0);

	return (1);
}


/*ARGSUSED*/
sdslave(ii, reg, intr)
	register struct iop/**/_device *ii;
	caddr_t reg;
	int intr;
{
	register struct scsi *sc;

	sc = get_scsi(intr);
	sdip[ii->ii_ctlr][ii->ii_slave] = ii;
	ii->ii_intr = intr;

	/*
	 * check what the device is.
	 */
	if ((ii->ii_type = sd_check(ii, sc)) < 0)
		goto bad_slave;

	/*
	 * set up ERROR RECOVERY PARAMETERS
	 */
	if (sd_err_rcv(ii, sc) < 0)
		goto bad_slave;

	/*
	 * set up OTHER PARAMETERS
	 */
	if (sd_other_pages(ii, sc) < 0)
		goto bad_slave;

	/*
	 * set up Synchronous Transfer
	 */
	sd_synctr_on(ii, sc);

	return (1);

bad_slave:
	/*
	 * no such slave
	 */
	ii->ii_intr = -1;
	return (0);
}

identity_check(sci, capacity, unit)
	register struct sc_inq *sci;
	int capacity;
	int unit;
{
	register struct sddevinfo *sdi;
	register u_char *id_name;
	register int index;
	register int i;
	int id_pass;

	id_name = sci->sci_vendid;
	while (*id_name == ' ')
		id_name++;

	index = UNKNOWN_DISK;
	id_pass = 0;
	for (sdi = sddevinfo; sdi->id_len >= 0; sdi++) {
		/*
		 * check vendor & product ID
		 */
		if (strncmp(id_name, sdi->id_name, sdi->id_len) != 0)
			continue;
		id_pass = sdi - sddevinfo;

		/*
		 * check revision
		 */
		if (strncmp(sdi->revs, sci->sci_revision, 4) == 0)
			index = id_pass;
		else {
			for (i = 0; i < 4; i++) {
				if (*(sdi->revs + i) == '?')
					continue;
				if (*(sdi->revs + i) != sci->sci_revision[i])
					break;
			}
			if (i < 4)
				continue;
		}

		/*
		 * check capacity
		 */
		if (capacity == -1)
			break;
		if (sdi->capacity == -1) {
			printf("sd%d: capacity=0x%x(%d)\n",
				unit, capacity, capacity);
			break;
		}
		if (capacity == sdi->capacity)
			break;
	}
	if (index == 0)
		index = id_pass;

	return (index);
}

search_index(type)
	register int type;
{
	register struct sddevinfo *sdi;
	register int i;
	int index;

	index = UNKNOWN_DISK;
	i = 0;
	for (sdi = sddevinfo; sdi->id_len > 0; sdi++) {
		if (sdi->type == type) {
			index = i;
			break;
		}
		i++;
	}
	return (index);
}

static
sd_check(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sc_inq *sci;
	register struct sc_rcap *scr;
	register int intr;
	register int slave;
	register int unit;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;
	struct sc_extnd *sce;
	int retrycnt;
	int index;
	int media_in;

	intr = ii->ii_intr;
	slave = ii->ii_slave;
	unit = ii->ii_unit;
	sdc = &sdc_softc[ii->ii_ctlr];
	sdd = &sdd_softc[unit];
	scr = (struct sc_rcap *)sc->sc_param;
	sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];

	/*
	 * check if the logical unit is ready.
	 *	(by TEST UNIT READY command)
	 */
	media_in = sd_tstdrv(ii, sc);
	if (media_in < 0)
		return (-3);

	/*
	 * Get controller and drive information.
	 *	(by INQUIRY command)
	 */
	retrycnt = 0;
	sci = get_sc_inq(intr);
loop_inq:
	if (retrycnt++ > MAXPROBERETRY)
		return (-1);

	scop_inquiry(intr, sc, slave, SCSI_INTDIS, sizeof(struct sc_inq), sci);
	sc->sc_tstatus &= TGSTMASK;

	if (sc->sc_istatus != INST_EP || sc->sc_tstatus != TGST_GOOD) {

		bzero((caddr_t)sce, RSEN_CNT);
		scop_rsense(intr, sc, slave, SCSI_INTDIS, RSEN_CNT,
				(caddr_t)sce);
		sc->sc_tstatus &= TGSTMASK;
		if (sc->sc_istatus != INST_EP || sc->sc_tstatus != TGST_GOOD)
			return (-1);
		if (sce->sce_extend != 0x70)
			goto loop_inq;

		switch (sce->sce_sdecode) {

		case 0x04:	/* Drive Not Ready */
		case 0x28:	/* Medium Changed */
		case 0x29:	/* Power On or Reset or Bus Device Reset */
		case 0x2a:	/* Mode Select Parameter Changed */
			break;

		default:
			return (-1);
		}
		DELAY(D100MSEC);		/* wait 100 ms. */
		goto loop_inq;
	}

	index = identity_check(sci, -1, unit);

	switch (sddevinfo[index].type) {

	case SMO_S501:
	case SMO_S501_ISO:
	case SMO_S501_ISO2:
		sdc->sdc_firmware =
			SDCFW_MO | (sdc->sdc_firmware & ~SDCFW_DEVMASK);
		break;

	defaults:
		break;
	}

	if (sci->sci_qual & 0x80) {
		/*
		 * removable medium device
		 */
		sdc->sdc_firmware |= SDCFW_RMB;
		if ((media_in == 0) || ((sdc->sdc_firmware & SDCFW_MO) == 0))
			return (index);
	}

	/****************/
	/* HD & MO only */
	/****************/
	/*
	 * Get drive capacity
	 *	(by READ CAPACITY command)
	 */
	retrycnt = 0;
loop_rcap:
	if (retrycnt++ > MAXPROBERETRY)
		return (-4);

	scop_rcap(intr, sc, slave, SCSI_INTDIS, 8, (caddr_t)0);
	sc->sc_tstatus &= TGSTMASK;
	if (sc->sc_istatus != INST_EP)
		return (-5);
	if (sc->sc_tstatus == TGST_CC) {
		bzero((caddr_t)sce, RSEN_CNT);
		scop_rsense(intr, sc, slave, SCSI_INTDIS, RSEN_CNT,
				(caddr_t)sce);
		sc->sc_tstatus &= TGSTMASK;
		if (sc->sc_istatus != INST_EP || sc->sc_tstatus != TGST_GOOD)
			return (-6);
		if (sderrordisp((caddr_t)sce, ii) == FORMAT_MODE_CORRUPTED) {
			scr->scr_nblock = 0;
			scr->scr_blocklen = DEV_BSIZE;
			sdd->sdd_flags |= SDDF_NONFMT;
		} else {
			DELAY(D100MSEC);		/* wait 100 ms. */
			goto loop_rcap;
		}
	}
	else if (sc->sc_tstatus != TGST_GOOD) {
		DELAY(D100MSEC);		/* wait 100 ms. */
		goto loop_rcap;
	}

	sdd->sdd_nsect = scr->scr_nblock + 1;
	sdd->sdd_sectsize = scr->scr_blocklen;

	index = identity_check(sci, scr->scr_nblock +1, unit);

	return (index);
}

static
sd_tstdrv(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sc_extnd *sce;
	register int intr;
	register int slave;
	register int retrycnt;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;

	sdc = &sdc_softc[ii->ii_ctlr];
	sdd = &sdd_softc[ii->ii_unit];

	intr = ii->ii_intr;
	slave = ii->ii_slave;
	sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];
	retrycnt = 0;
loop_tst:
	if (retrycnt++ > MAXPROBERETRY)
		return (-1);

	scop_tst(intr, sc, slave, SCSI_INTDIS);
	sc->sc_tstatus &= TGSTMASK;
	if (sc->sc_istatus != INST_EP) {
		DELAY(D100MSEC);		/* wait 100 ms. */
		goto loop_tst;
	}

	switch (sc->sc_tstatus) {

	case TGST_CC:
		/* Get error code */
		bzero((caddr_t)sce, RSEN_CNT);
		scop_rsense(intr, sc, slave, SCSI_INTDIS, RSEN_CNT,
				(caddr_t)sce);
		sc->sc_tstatus &= TGSTMASK;
		if (sc->sc_istatus != INST_EP || sc->sc_tstatus != TGST_GOOD) {
			DELAY(D100MSEC);	/* wait 100 ms. */
			goto loop_tst;
		}

		if (sce->sce_extend != 0x70)
			goto loop_tst;

		switch (sce->sce_skey) {

		case 0x0:		/* No Sense */
		case 0x4:		/* Hardware error */
		case 0x6:		/* Unit attention */
			goto loop_tst;

		case 0x2:		/* Not ready */
			switch (sce->sce_sdecode) {

			case 0x04:	/* Not ready */
				/*
				 * Drive not ready... so start..
				 */
				scop_stst(intr, sc, slave, SCSI_INTDIS, SDSS_START);
				DELAY(D100MSEC * 10);	/* wait 1 sec. */
				goto loop_tst;

			case 0x0a:	/* No Disk *//*MO*/
			default:
				DELAY(D100MSEC);
				goto loop_tst;
			}
			break;

		case 0x03:
			if (sce->sce_sdecode == FORMAT_MODE_CORRUPTED)
				return (1);	/* ignore error */
			/* fall through */

		default:
			return (-2);
		}
		break;
			
	case TGST_BUSY:
		goto loop_tst;

	case TGST_GOOD:
		break;

	default:
		return (-3);
	}

	return (1);
}

#ifdef NEWSOS4
static
sd_setup_cmds(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sddevinfo *sdi;
	register struct sc_extnd *sce;
	struct sc_ureq **p;
	struct sc_ureq *scu;
	int error;
	extern struct sc_ureq scu_rsense;

	if ((p = sddevinfo[ii->ii_type].setup_cmds) == NULL)
		return (0);

	/*
	 * Do setup commands
	 */
	while (scu = *p) {
		bcopy((caddr_t)scu, sdtmp, sizeof(struct sc_ureq));
		scu = (struct sc_ureq *)sdtmp;
		scu->scu_cdb[1] |= (ii->ii_slave & 0x07) << 5;
		error = sd_scu_exec((ii->ii_unit << 3), scu, sc);
		if (error != 0)
			return (-1);
		if ((scu->scu_istatus != INST_EP)
		    || (scu->scu_tstatus != TGST_GOOD)) {
			bcopy((caddr_t)&scu_rsense, sdtmp, sizeof(struct sc_ureq));
			scu = (struct sc_ureq *)sdtmp;
			scu->scu_cdb[1] |= (ii->ii_slave & 0x07) << 5;
			sce = (scu->scu_addr == NULL) ?
				(struct sc_extnd *)scu->scu_param :
				(struct sc_extnd *)scu->scu_addr;
			if (sd_scu_exec((ii->ii_unit << 3), scu, sc) == 0) {
				/* UNIT ATTENTION */
				/* retry same command */
				if (sce->sce_skey == 0x06)
					continue;
			}
		}
		p++;
	}
	return (1);
}
#endif /* NEWSOS4 */

static
sd_other_pages(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sddevinfo *sdi;
	char **p;
	char *page;
	int length;
	int retrycnt;
	int len;

	sdi = &sddevinfo[ii->ii_type];
	if ((p = sdi->other_pages) == NULL)
		return (0);

	/*
	 * set other parameters
	 */
	while (page = *p++) {
		retrycnt = 0;
loop_other_pages:
		bzero((caddr_t)sdtmp, 4);
		length = *(page + 1) + 2;
		bcopy(page, &sdtmp[4], length);
		if (retrycnt++ > MAXPROBERETRY)
			return (-1);

		scop_mselect(ii->ii_intr, sc, ii->ii_slave, SCSI_INTDIS,
				(SDM_PF<<24) + length +4, (caddr_t)sdtmp);
		sc->sc_tstatus &= TGSTMASK;
		if ((sc->sc_istatus != INST_EP)
				|| (sc->sc_tstatus != TGST_GOOD)) {
			struct sc_extnd *sce;

			sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];
			scop_rsense(ii->ii_intr, sc, ii->ii_slave, SCSI_INTDIS,
						RSEN_CNT, (caddr_t)sce);
			switch (sce->sce_skey) {

			case 0x00:
			case 0x02:
			case 0x04:
			case 0x06:
				DELAY(D100MSEC);   /* 100 ms. */
				goto loop_other_pages;

			default:
				return (-1);
			}
		}
	}

	if (sdi->firm_flags & FIRM_CACHE_ON)
		sdc_softc[ii->ii_ctlr].sdc_firmware |= SDCFW_CACHE;
	else
		sdc_softc[ii->ii_ctlr].sdc_firmware &= ~SDCFW_CACHE;

	return (1);
}

static
sd_err_rcv(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sdc_softc *sdc;
	register int intr;
	register int slave;
	register int len;
	struct sc_extnd *sce;
	struct sdd_softc *sdd;
	struct sddevinfo *sdi;
	int retrycnt;
	char *erp_page;

	intr = ii->ii_intr;
	slave = ii->ii_slave;
	sdc = &sdc_softc[ii->ii_ctlr];
	sdd = &sdd_softc[ii->ii_unit];
	sdi = &sddevinfo[ii->ii_type];
	sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];

	/*
	 * set Default DISK sector size
	 */
	if (sdd->sdd_sectsize == 0)
		sdd->sdd_sectsize = DEV_BSIZE;


	if (sdi->ERP_page == NULL) {
		/*
		 * use default error recovery parameters
		 */
		sdc->sdc_firmware |= SDCFW_DEFMODE;
		return (0);
	}

	if (sdi->firm_flags & FIRM_AWRE)
		sdc->sdc_firmware |= SDCFW_AWRE;
	if (sdi->firm_flags & FIRM_ARRE)
		sdc->sdc_firmware |= SDCFW_ARRE;
	/*
	 * set ERROR RECOVERY PARAMETERS
	 */
loop_err_rcv:
	bzero((caddr_t)sdtmp, 4);
	erp_page = sdi->ERP_page;
	len = *(erp_page + 1) + 2;
	bcopy(erp_page, &sdtmp[4], len);

	scop_mselect(intr, sc, slave, SCSI_INTDIS,
			(SDM_PF<<24) + len +4, (caddr_t)sdtmp);
	sc->sc_tstatus &= TGSTMASK;
	if (sc->sc_istatus != INST_EP || sc->sc_tstatus != TGST_GOOD) {
		if (sc->sc_tstatus == TGST_CC) {
			bzero((caddr_t)sce, RSEN_CNT);
			scop_rsense(intr, sc, slave, SCSI_INTDIS, RSEN_CNT,
					(caddr_t)sce);
			if (sce->sce_sdecode == 0x2a) {
				/* mode select parameter changed */
				goto ercv_done;
			} else if (sce->sce_skey == 0x6) {
				/* unit attention */
				goto loop_err_rcv;
			}
		}
		/*
		 * use default ERROR RECOVERY mode
		 */
		sdc->sdc_firmware |= SDCFW_DEFMODE;
		sdc->sdc_firmware &= ~(SDCFW_AWRE|SDCFW_ARRE);
	}

ercv_done:

	return (1);
}

static
sd_synctr_on(ii, sc)
	register struct iop/**/_device *ii;
	register struct scsi *sc;
{
	register struct sddevinfo *sdi;
	register struct sync_param *syncp;

	sdi = &sddevinfo[ii->ii_type];

	if (sdi->firm_flags & FIRM_SYNCTR) {
		scinit(sc, ii->ii_slave, DEV_BSIZE); 
		sc->sc_opcode = SCOP_TST;
		sc->sc_message = MSG_EXTND;	/* extended message */
		sc->sc_param[0] = MSG_EXTND;
		sc->sc_param[1] = 0x03;
		sc->sc_param[2] = 0x01;		/* synchronous transfer */
		sc->sc_param[3] = sdi->tr_period;	/* transfer period */
		sc->sc_param[4] = sdi->tr_offset;	/* REQ offset */

		if (sdc_softc[ii->ii_ctlr].sdc_firmware & SDCFW_CACHE)
			sc->sc_tstatus |= TS_CONTR_ON;	/* contiguous TR ON */
		else
			sc->sc_tstatus |= TS_CONTR_OFF;	/* contiguous TR OFF */

#ifdef news1800
		if (scsi_berr_bug() != 0) {
			sc->sc_tstatus &= ~TS_CONTR_ON;
			sc->sc_tstatus |= TS_CONTR_OFF;
		}
#endif

		if (sc->sc_tstatus & TS_CONTR_OFF)
			sdc_softc[ii->ii_ctlr].sdc_firmware &= ~SDCFW_CONTR;
		else
			sdc_softc[ii->ii_ctlr].sdc_firmware |= SDCFW_CONTR;

		sc_go(ii->ii_intr, sc, SCSI_INTDIS);

		syncp = &sd_sync_param[ii->ii_unit];
		syncp->tr_period = sc->sc_param[3];
		syncp->tr_offset = sc->sc_param[4];
		if (sc->sc_param[4])
			sdd_softc[ii->ii_unit].sdd_flags |= SDDF_SYNCTR;
	}
}


sdattach(ii)
	register struct iop/**/_device *ii;
{
	register int unit;
	register int i;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;
	int dummy;

	sdc = &sdc_softc[ii->ii_ctlr];
	sdc->sdc_timeo = 60;			/* timeout 60 sec */

	unit = ii->ii_unit;
	sdd = &sdd_softc[unit];
	sdd->sdd_stoptime = OD_STOPTIME;	/* REMOVABLE MEDIA */
	sdd->sdd_start = -2;

	sdmaptype(ii);

	if (sdwstart == 0) {
		sdwstart++;
		timeout(sdwatch, (caddr_t)0, hz);
		timeout(sdstop, (caddr_t)0, hz);
	}

#ifdef NEWSOS4
	/*
	 * Do setup commands
	 */
	if (sd_setup_cmds(ii, get_scsi(ii->ii_intr)) < 0)
		printf("sd%d: setup failure\n", ii->ii_unit);
#endif

	/*
	 * initialize open flag
	 */
	for (i = 0; i < PNUM; i++) {
		sd_b_openf[unit][i] = 0;
		sd_c_openf[unit][i] = 0;
	}

	if (re_init_done > 0)
		return;

	if (sdc->sdc_firmware & SDCFW_HD) {
		/*
		 * If device is Hard Disk,
		 *	then get partition information.
		 */
		sdrpartinfo(ii);
		dummy = DEV_BSIZE * sdstdrv[unit].rps * sdstdrv[unit].nsect;
	} else
		dummy = DEV_BSIZE * 40 * 31;

	if (ii->ii_dk >= 0 && dummy)
		dk_wpms[ii->ii_dk] = dummy / (2 * 1000000);
}

sdmaptype(ii)
	register struct iop/**/_device *ii;
{
	printf("sd%d: %s\n", ii->ii_unit, sddevinfo[ii->ii_type].call_name);
}

int sd_b_major = -1;

sd_b_open(dev, flag)
	dev_t dev;
	int flag;
{
	sd_b_major = major(dev);
	return (_sdopen(dev, flag, S_IFBLK));
}

int sd_c_major = -1;

sd_c_open(dev, flag)
	dev_t dev;
	int flag;
{
	sd_c_major = major(dev);
	return (_sdopen(dev, flag, S_IFCHR));
}

_sdopen(dev, flag, fmt)
	register dev_t dev;
	int flag;
	int fmt;
{
	register struct iop/**/_device *ii;
	register struct sdd_softc *sdd;
	register struct sdc_softc *sdc;
	register struct sddevinfo *sdi;
	register int unit;
	register int i;
	u_char *sdopfp;
	u_char	old_sdopf;
	int media_changed;
	int s;
	int stat;
	struct scsi uscsi;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0 || ii->ii_alive == 0)
		return (ENXIO);

	sdd = &sdd_softc[unit];
	sdc = &sdc_softc[ii->ii_ctlr];
	sdi = &sddevinfo[ii->ii_type];

	if (sdd->sdd_flags & SDDF_XUSE)
		return (EBUSY);

	/*
	 * LOCK while sdstop() running.
	 */
	s = splclock();
	while (sdc->sdc_state & SDCS_SCUNLOCK) {
		sdc->sdc_state |= SDCS_OPEN_WAIT;
		sleep((caddr_t)sdc, PRIBIO);
	}
	splx(s);

	/*
	 * LOCK sdtmp buffer
	 */
	s = splclock();
	while (sdtmp_stat & B_BUSY) {
		sdtmp_stat |= B_WANTED;
		sleep((caddr_t)sdtmp, PRIBIO);
	}
	sdtmp_stat |= B_BUSY;
	splx(s);
	sdd->sdd_flags |= SDDF_GETTMP;

	if ((fmt & S_IFMT) == S_IFBLK)
		sdopfp = &sd_b_openf[unit][dev2part(dev)];
	else
		sdopfp = &sd_c_openf[unit][dev2part(dev)];
	old_sdopf = *sdopfp;
	if (old_sdopf <= 1)
		*sdopfp += 1;		/* 1: 1st open (ONLY_ONE) */
					/* 2: already opened */
	stat = 0;
	media_changed = 0;

	/*
	 * From here on until pre_open_done is only for removable devices
	 */
	if ((sdc->sdc_firmware & SDCFW_RMB) == 0)
		goto pre_open_done;

	if ((minor(dev) & 0x80) || (dev == rootdev))
		sdd->sdd_stoptime = 0x7fffffff;		/*XXX*/

	/*
	 * Start Unit
	 */
	s = splclock();	/* inhibit clock interrupt */
	i = sdd->sdd_start;
	sdd->sdd_start = sdd->sdd_stoptime;
	splx(s);
	if (i <= 0) {
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_opcode = SCOP_STST;
		uscsi.sc_count = SDSS_START;

		if (sdcmd(dev, &uscsi)) {
			sdd->sdd_start = i;
			if ((flag & FWRITE) == 0)
				goto sdopen_setup;
			stat = EIO;
			goto pre_open_done;
		}
	}

	/*
	 * prevent medium removal
	 */
	scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
	uscsi.sc_opcode = SCOP_MEDRMV;
	uscsi.sc_count = SDRMV_PREV;
	if (sdcmd(dev, &uscsi)) {
		stat = EIO;
		goto pre_open_done;
	}
	sdd->sdd_flags |= SDDF_INHRMV;

sdopen_setup:
	if ((sdd->sdd_flags & SDDF_SAMEDSK) == SDDF_DSKCHGD) {
		sdd->sdd_flags |= SDDF_SAMEDSK;
		media_changed = 1;

		/*
		 * From here on until mo_check_done is only for MO device
		 */
		if ((sdc->sdc_firmware & SDCFW_MO) == 0)
			goto mo_check_done;

		/*
		 * Mode Sense
		 */
		bzero(sdtmp, 36);
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = 36;
		uscsi.sc_opcode = SCOP_MSENSE;
		uscsi.sc_lad = (SDM_PF << 16)|((SDM_PC_CUR|SDM_PCODE_ALL) << 8);
		uscsi.sc_count = 36;

		if (sdcmd(dev, &uscsi) == 0) {
			/*
			 * check Write Protect mode
			 */
			if (sdtmp[2] & 0x80)
				sdd->sdd_flags |= SDDF_WPROTECT;
			else
				sdd->sdd_flags &= ~SDDF_WPROTECT;
			/*
			 * check Format Mode
			 */
			if (sdtmp[26] == 2) {
				ii->ii_type = search_index(SMO_S501);
				if (mo_disp_format)
					printf("sd%d: format mode 2 (original format)\n", unit);
			} else if (sdtmp[26] == 3) {
				int spare;

				spare = *(short *)&sdtmp[32];
				if (spare == 2048)
					ii->ii_type =
						search_index(SMO_S501_ISO2);
				else
					ii->ii_type =
						search_index(SMO_S501_ISO);
				if (mo_disp_format)
					printf("sd%d: format mode 3 (ISO format) spare=%d\n", unit, spare);
			} else {
				sdd->sdd_flags |= SDDF_NONFMT;
				if (mo_disp_format)
					printf("sd%d: Non format\n", unit);
			}
			sdi = &sddevinfo[ii->ii_type];
		}

		/*
		 * Mode Select
		 *	Error Recovery Parameters set
		 */
		i = *(sdi->ERP_page +1) + 2;	/* page length */
		bzero(sdtmp, i + 4);
		bcopy(sdi->ERP_page, (caddr_t)&sdtmp[4], i);
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = i + 4;
		uscsi.sc_opcode = SCOP_MSELECT;
		uscsi.sc_lad = (SDM_PF << 16);
		uscsi.sc_count = i + 4;

		(void) sdcmd(dev, &uscsi);

		/*
		 * Read Grown Defect list
		 */
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = 4;
		uscsi.sc_opcode = SCOP_RDL;
		uscsi.sc_cdb.un_type1.t1_ladhi = (SDDL_GLIST|SDDL_PHYSFMT) << 8;
		uscsi.sc_cdb.un_type1.t1_p3 = 4;

		(void) sdcmd(dev, &uscsi);
		i = *(short *)&sdtmp[2] / 8;
		if (i > (1024*9/10))
			printf("sd%d: WARNING: DEFECT SPARE LOCATION < 10%\n",
				unit);
mo_check_done:
		/*
		 * Read Capacity
		 */
		bzero((caddr_t)sdtmp, 8);
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = 8;
		uscsi.sc_opcode = SCOP_RCAP;

		(void) sdcmd(dev, &uscsi);
		sdd->sdd_nsect = *(int *)&sdtmp[0] + 1;
		sdd->sdd_sectsize = *(int *)&sdtmp[4];

		if ((sdd->sdd_sectsize != DEV_BSIZE)
				&& (sdd->sdd_sectsize != SDBSIZE1K))
			sdd->sdd_sectsize = DEV_BSIZE;
	}

	if ((sdd->sdd_flags & SDDF_WPROTECT) && (flag & FWRITE))
		stat = EROFS;

pre_open_done:

	if (stat == 0) {
		if ((isalone(unit) == ONLY_ONE) || media_changed) {
			/*
			 * read partition information from sector zero.
			 */
			sdrpartinfo(ii);
			if (ii->ii_dk >= 0) {
				dk_wpms[ii->ii_dk] =
					sdd->sdd_sectsize * sdstdrv[unit].rps
					* sdstdrv[unit].nsect / (2 * 1000000);
			}
		}
	} else {
		/*
		 * open error
		 */
		*sdopfp = old_sdopf;
		if ((sdd->sdd_flags & SDDF_INHRMV) && (isalone(unit) == 0)) {
			sdd->sdd_flags &= ~SDDF_INHRMV;
			scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
			uscsi.sc_opcode = SCOP_MEDRMV;
			uscsi.sc_count = SDRMV_ALLOW;
			(void) sdcmd(dev, &uscsi);
		}
	}

	/*
	 * UNLOCK open
	 */
	s = splclock();
	sdd->sdd_flags &= ~SDDF_GETTMP;
	if (sdtmp_stat & B_WANTED)
		wakeup((caddr_t)sdtmp);
	sdtmp_stat &= ~(B_BUSY|B_WANTED);
	splx(s);
	return (stat);
}

int sd_access_check_on;		/* Common flags for sd_access_check() */

sd_access_check(bp)
	register struct buf *bp;
{
	register struct iop/**/_device *ii;
	register struct sdd_softc *sdd;
	int unit;
	int check_part;
	int limit;
	int over;
	register struct size *sizes;
	register int lba;	/* logical block address */
	register int sz;
	register int i;

	check_part = 0;
	unit = dev2unit(bp->b_dev);
	ii = sddinfo[unit];
	sdd = &sdd_softc[unit];
	sizes = sdstdrv[unit].sizes;

	lba = sizes[dev2part(bp->b_dev)].sd_blkoff + dkblock(bp);
	sz = howmany(bp->b_bcount, sdd->sdd_sectsize);

	/*
	 * When block device is used,
	 *	inhibit raw device write operation.
	 */
	if ((major(bp->b_dev) == sd_c_major)			/* RAW I/O */
	    && ((bp->b_flags & B_READ) == 0)			/* WRITE */
	    && ((ii->ii_flags & SD_F_ENW) == 0)			/* INHIBIT */
	    && ((sd_access_check_on & SD_F_ENW) == 0)) {

		for (i = 0; i < PNUM; i++) {
			if (sd_b_openf[unit][i] == 0)
				continue;
			/*
			 *   |----|========|---|======|-------|
			 * 1 |---+++--------------------------| CUT OFF
			 * 2 |---++++++++++++-----------------| CUT OFF
			 * 3 |---++++++++++++++++-------------| CUT OFF
			 * 4 |---++++++++++++++++++++++++-----| CUT OFF
			 * 5 |-------+++----------------------| ERROR
			 * 6 |------------+++-----------------| ERROR
			 * 7 |------------+++++++-------------| ERROR
			 * 8 |------------+++++++++++++++-----| ERROR
			 */
			if ((lba < (sizes[i].sd_blkoff + sizes[i].sd_nblocks))
			    && ((lba + sz) > sizes[i].sd_blkoff))
				check_part |= (1 << i);
		}
	}

	if (check_part) {
		limit = 0x7fffffff;	/* XXX */
		for (i = 0; i < PNUM; i++) {
			if ((check_part & (1 << i)) == 0)
				continue;

			if (lba >= sizes[i].sd_blkoff) {
				bp->b_flags |= B_ERROR;
				bp->b_error = EIO;
				bp->b_resid = bp->b_bcount;

				printf("sd%d%c: RAW DEVICE WRITE PROTECTED: ",
					unit, pname[dev2part(bp->b_dev)]);
				printf("sn = 0x%x(%d), off = 0x%x(%d)\n",
					dkblock(bp)*DEV_BSIZE/sdd->sdd_sectsize,
					dkblock(bp)*DEV_BSIZE/sdd->sdd_sectsize,
					sizes[dev2part(bp->b_dev)].sd_blkoff,
					sizes[dev2part(bp->b_dev)].sd_blkoff);

				return (-1);
			}

			if (sizes[i].sd_blkoff < limit)
				limit = sizes[i].sd_blkoff;
		}
	} else {
		limit = sizes[dev2part(bp->b_dev)].sd_blkoff
			+ sizes[dev2part(bp->b_dev)].sd_nblocks;
	}

	if ((over = (lba + sz) - limit) > 0) {
		/*
		 * Logical Block Address is outside the valid area.
		 */
		if (((ii->ii_flags & SD_F_EOLBA) != 0)
			|| ((sd_access_check_on & SD_F_EOLBA) != 0)) {
			/*
			 * error if outside LBA
			 */
			return(-1);
		}
		bp->b_resid = bp->b_bcount - (sz - over) * sdd->sdd_sectsize;
		if (bp->b_resid >= bp->b_bcount) {
			bp->b_resid = bp->b_bcount;
			return(-1);
		}
	}

	return (0);
}

sd_b_close(dev, flag)
	dev_t dev;
	int flag;
{
	return (_sdclose(dev, flag, S_IFBLK));
}

sd_c_close(dev, flag)
	dev_t dev;
	int flag;
{
	return (_sdclose(dev, flag, S_IFCHR));
}

_sdclose(dev, flag, fmt)
	register dev_t dev;
	int flag;
	int fmt;
{
	register struct iop/**/_device *ii;
	register struct sdd_softc *sdd;
	register int unit;
	struct sdc_softc *sdc;
	struct scsi uscsi;
	struct sc_extnd *sce;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0 || ii->ii_alive == 0)
		return (ENXIO);

	sdd = &sdd_softc[unit];
	sdc = &sdc_softc[ii->ii_ctlr];
	sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];

	/*
	 * still remain jobs
	 *	sleep about 10ms -> 1sec
	 */
	while (ii->ii_mi->im_tab.b_actf != NULL)
		sleep((caddr_t)&lbolt, PRIBIO);

	if ((fmt & S_IFMT) == S_IFBLK)
		sd_b_openf[unit][dev2part(dev)] = 0;
	else
		sd_c_openf[unit][dev2part(dev)] = 0;
	sdd->sdd_flags &= ~SDDF_XUSE;

	if ((sdc->sdc_firmware & SDCFW_RMB) && (isalone(unit) == 0)) {
		sdd->sdd_flags &= ~SDDF_INHRMV;
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_opcode = SCOP_MEDRMV;
		uscsi.sc_count = SDRMV_ALLOW;
		(void) sdcmd(dev, &uscsi);
	}
	return (0);
}

sdcmd(dev, usc)
	dev_t dev;
	struct scsi *usc;
{
	register struct buf *bp;
	register struct scsi *ksc;
	register u_char *point;
	register int unit;
	int error;
	int s;
	int cnt;

	if (usc == 0)
		return (ENXIO);

	error = 0;
	unit = dev2unit(dev);
	bp = &csdbuf[unit];

	/*
	 * LOCK csdbuf
	 */
	s = splclock();
	while (bp->b_flags & B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bzero((caddr_t)bp, sizeof(struct buf));
	bp->b_flags = B_BUSY|B_READ;
	splx(s);

	ksc = &kernscsi[unit];
	bcopy((caddr_t)usc, (caddr_t)ksc, sizeof(struct scsi));
	/*
	 * setup command buffer
	 */
	bp->b_dev = dev;
	bp->b_proc = curproc;
	cnt = ksc->sc_ctrnscnt;
	bp->b_bcount = cnt;

	point = ksc->sc_cpoint;
	bp->b_un.b_addr = (caddr_t)point;
	if (cnt > 0) {
		if (point == NULL) {
			ksc->sc_cpoint = point = get_scsi(unit)->sc_param;
			if (cnt > 20) {
				error = EFAULT;
				goto done;
			}
		}
		if (point < (u_char *)KERNBASE) {
			if (useracc(point, cnt, B_WRITE) == NULL) {
				error = EFAULT;
				goto done;
			}
			curproc->p_flag |= SPHYSIO;
			vslock(point, cnt);
			bp->b_flags |= B_PHYS;
		}
#ifndef mips
		else {
			if (kernacc(point, cnt, B_WRITE) == NULL) {
				error = EFAULT;
				goto done;
			}
		}
#endif
		ksc->sc_tstatus = TS_MAPPED_PIO;	/* XXX */
	}

	/*
	 * call strategy entry, and wait command done.
	 */
	sdstrategy(bp);
	iowait(bp);

	if ((cnt > 0) && (point < (u_char *)KERNBASE)) {
		vsunlock(point, cnt, B_READ);
		curproc->p_flag &= ~SPHYSIO;
	}
	if ((bp->b_flags & B_ERROR) == 0)
		error = 0;
	else {
		if (bp->b_error)
			error = bp->b_error;
		else
			error = EIO;
	}
	bcopy((caddr_t)ksc, (caddr_t)usc, sizeof(struct scsi));

done:
	/*
	 * UNLOCK csdbuf
	 */
	s = splclock();
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags = 0;
	splx(s);
	return (error);
}

/*
 * read partition information from sector zero.
 */
sdrpartinfo(ii)
	register struct iop/**/_device *ii;
{
	register struct disklabel *dlp;
	register struct sdst *hsp;
	register struct sdst *st;
	register int unit;
	register int i;
	struct firstsector *fsp;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;
	struct sddevinfo *sdi;
#ifdef DISKINFO
	struct diskinfo *dip;
#endif
	struct scsi uscsi;
	int s;

	sdi = &sddevinfo[ii->ii_type];
	unit = ii->ii_unit;

	sdd = &sdd_softc[unit];
	sdc = &sdc_softc[ii->ii_ctlr];

	if ((sdd->sdd_flags & (SDDF_NONFMT|SDDF_FMTDONE)) == 0) {
		register struct sc_rcap *scr = (struct sc_rcap *)sdtmp;

		sdd->sdd_flags |= SDDF_FMTDONE;

		bzero((caddr_t)sdtmp, 8);
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = (u_char *)scr;
		uscsi.sc_ctrnscnt = 8;
		uscsi.sc_opcode = SCOP_RCAP;
		(void) sdcmd(unit << 3, &uscsi);

		sdd->sdd_nsect = scr->scr_nblock + 1;
		sdd->sdd_sectsize = scr->scr_blocklen;
		if (sdd->sdd_sectsize == 0)
			sdd->sdd_sectsize = SDBSIZE1K;
	}

	bzero(sdtmp, DEV_BSIZE);

	if ((sdd->sdd_flags & SDDF_NONFMT) == 0) {
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = DEV_BSIZE;
		uscsi.sc_opcode = SCOP_READ;
		uscsi.sc_lad = 0;
		uscsi.sc_count = 1;

		(void) sdcmd(unit << 3, &uscsi);
		sdd->sdd_flags &= ~SDDF_SKIPCHECK;
	}

	fsp = (struct firstsector *)sdtmp;
	dlp = (struct disklabel *)(sdtmp + LABELOFFSET);
#ifdef DISKINFO
	dip = &fsp->diskinfo;
#endif

	s = splclock();
	hsp = &sdstdrv[unit];
	bzero((caddr_t)hsp, sizeof (struct sdst));
	bzero(&sdlabel[unit], sizeof (struct disklabel));

	if ((dlp->d_magic == DISKMAGIC)
	    && ((ii->ii_flags & SD_F_IGNLABEL) == 0)) {
		sdlabel[unit] = *dlp;
		disklabel2sdst(unit, dlp, hsp);
#ifdef DISKINFO
	} else if ((dip->di_magic == DISKINFO_MAGIC)
		    && ((ii->ii_flags & SD_F_IGNLABEL) == 0)) {
		diskinfo2sdst(unit, dip, hsp);
		diskinfo2disklabel(unit, dip, &sdlabel[unit]);
#endif
	} else {
		if ((ii->ii_type == UNKNOWN_DISK)
				|| (sdi->sdstp->sizes == calc_disk_sizes)) {
			/*
			 * If device is UNKNOWN PARTITION SIZE,
			 *	calculate default partition from capacity.
			 */
			st = sdi->sdstp;
			hsp->nsect = st->nsect;		/* # sectors/track */
			hsp->ntrak = st->ntrak;		/* # tracks/cylinder */
			hsp->nspc = st->nspc;		/* # sectors/cylinder */
			hsp->ncyl = st->ncyl;		/* # cylinders */
			hsp->rps = st->rps;		/* # revolutions/sec */
			hsp->sizes = sdsizedrv[unit];	/* partition table */

			sd_calcpart(ii, hsp->sizes,
					sdd->sdd_nsect, sdd->sdd_sectsize);
			sdst2disklabel(unit, hsp, &sdlabel[unit]);
		} else {
			/*
			 * If device is support disk,
			 *	copy default partition from size table.
			 */
			st = sdi->sdstp;

			hsp->nsect = st->nsect;	/* # sectors/track */
			hsp->ntrak = st->ntrak;	/* # tracks/cylinder */
			hsp->nspc = st->nspc;	/* # sectors/cylinder */
			hsp->ncyl = st->ncyl;	/* # cylinders */
			hsp->rps = st->rps;	/* # revolutions / second */
			hsp->sizes = sdsizedrv[unit];	/* partition table */

			for (i = 0; i < PNUM; i++) {
			    hsp->sizes[i].sd_nblocks = st->sizes[i].sd_nblocks;
			    hsp->sizes[i].sd_blkoff = st->sizes[i].sd_blkoff;
			}
			sdst2disklabel(unit, hsp, &sdlabel[unit]);
		}
	}

	/* BEGIN XXX*/
	if (hsp->rps == 0) {
		/*
		 * If device is support disk,
		 *	copy default partition from size table.
		 */
		st = sdi->sdstp;

		hsp->nsect = st->nsect;		/* # sectors/track */
		hsp->ntrak = st->ntrak;		/* # tracks/cylinder */
		hsp->nspc = st->nspc;		/* # sectors/cylinder */
		hsp->ncyl = st->ncyl;		/* # cylinders */
		hsp->rps = st->rps;		/* # revolutions / second */
		sdst2disklabel(unit, hsp, &sdlabel[unit]);
	}
	/* END XXX*/
	(void)splx(s);
}

static char Warn_Part[] = "sd%d: PARTITION TABLE CHANGED\n";
static char Pr_Part_Fmt[] = "sd%d%c: nblk=%d, off=%d\n";

#define stsz(N) st->sizes[(N)].sd_nblocks
#define stof(N) st->sizes[(N)].sd_blkoff
#define dlsz(N) dlp->d_partitions[(N)].p_size
#define dlof(N) dlp->d_partitions[(N)].p_offset
#define disz(N) dip->di_part[(N)].dp_nblocks
#define diof(N) dip->di_part[(N)].dp_blkoff

#ifndef BBSIZE
#define BBSIZE 8192
#endif

static
check_sdst(unit, st)
	int unit;
	struct sdst *st;
{
	if (st->nsect == 0) {
		st->nsect = 1;
		printf("sd%d: nsect SHOULD BE != 0, 1 assumed\n", unit);
	}

	if (st->rps == 0) {
		st->rps = 60;
		printf("sd%d: rps SHOULD BE != 0, 60 assumed\n", unit);
	}
}

static
disklabel2sdst(unit, dlp, st)
	int unit;
	register struct disklabel *dlp;
	register struct sdst *st;
{
	register int i;
	int msg_header_printed;

	msg_header_printed = 0;

	st->nsect = dlp->d_nsectors;	/* # sectors/track */
	st->ntrak = dlp->d_ntracks;	/* # tracks/cylinder */
	st->nspc = dlp->d_secpercyl;	/* # sectors/cylinder */
	st->ncyl = dlp->d_ncylinders;	/* # cylinders */
	st->rps = dlp->d_rpm / 60;	/* # revolutions / second */
	st->sizes = sdsizedrv[unit];	/* partition table */

	check_sdst(unit, st);

	for (i = 0; i < PNUM; i++) {
		if (msg_header_printed == 0) {
			if (((stsz(i) != 0) || (stof(i) != 0))
			    && ((stsz(i) != dlsz(i)) || (stof(i) != dlof(i)))) {
				msg_header_printed = 1;
			}
		}
	}

	for (i = 0; i < PNUM; i++) {
		stsz(i) = dlsz(i);
		stof(i) = dlof(i);
	}

	if (msg_header_printed) {
		printf(Warn_Part, unit);
		for (i = 0; i < PNUM; i++)
			printf(Pr_Part_Fmt, unit, pname[i], stsz(i), stof(i));
	}
}

#ifdef DISKINFO
static
diskinfo2sdst(unit, dip, st)
	int unit;
	register struct diskinfo *dip;
	register struct sdst *st;
{
	register int i;
	int msg_header_printed;

	msg_header_printed = 0;

	st->nsect = dip->di_dkst.dks_nsect;	/* # sectors/track */
	st->ntrak = dip->di_dkst.dks_ntrak;	/* # tracks/cylinder */
	st->nspc = dip->di_dkst.dks_nsect * dip->di_dkst.dks_ntrak;
						/* # sectors/cylinder */
	st->ncyl = dip->di_dkst.dks_ncyl;	/* # cylinders */
	st->rps = dip->di_dkst.dks_rps;		/* # revolutions / second */
	st->sizes = sdsizedrv[unit];		/* partition table */

	check_sdst(unit, st);

	for (i = 0; i < PNUM; i++) {
		if (msg_header_printed == 0) {
			if (((stsz(i) != 0) || (stof(i) != 0))
			    && ((stsz(i) != disz(i)) || (stof(i) != diof(i)))) {
				msg_header_printed = 1;
			}
		}
	}

	for (i = 0; i < PNUM; i++) {
		stsz(i) = disz(i);
		stof(i) = diof(i);
	}

	if (msg_header_printed) {
		printf(Warn_Part, unit);
		for (i = 0; i < PNUM; i++)
			printf(Pr_Part_Fmt, unit, pname[i], stsz(i), stof(i));
	}
}

static
diskinfo2disklabel(unit, dip, dlp)
	int unit;
	register struct diskinfo *dip;
	register struct disklabel *dlp;
{
	register int i;

	dlp->d_type = DTYPE_SCSI;			/* drive type */
	dlp->d_secsize = sdd_softc[unit].sdd_sectsize;	/* # of bytes per sector */
	dlp->d_nsectors = dip->di_dkst.dks_nsect;	/* # sectors/track */
	dlp->d_ntracks = dip->di_dkst.dks_ntrak;	/* # tracks/cylinder */
	dlp->d_ncylinders = dip->di_dkst.dks_ncyl;	/* # cylinders */
	dlp->d_secpercyl = dip->di_dkst.dks_nsect * dip->di_dkst.dks_ntrak;
							/* # sectors/cylinder */
	dlp->d_rpm = dip->di_dkst.dks_rps * 60;		/* # revolutions / second */
	dlp->d_bbsize = BBSIZE;	/*XXX*/	/* size of boot area at sn0, bytes */
	dlp->d_sbsize = SBSIZE;	/*XXX*/	/* max size of fs superblock, bytes */

	for (i = 0; i < PNUM; i++) {
		dlsz(i) = disz(i);
		dlof(i) = diof(i);
	}
}
#endif /* DISKINFO */

/* #ifdef DISKINFO KU:XXX */
static
disklabel2diskinfo(unit, dlp, dip)
	int unit;
	register struct disklabel *dlp;
	register struct diskinfo *dip;
{
	register int i;

	dip->di_magic = DISKINFO_MAGIC;
	dip->di_dkst.dks_nsect = dlp->d_nsectors;	/* # sectors/track */
	dip->di_dkst.dks_ntrak = dlp->d_ntracks;	/* # tracks/cylinder */
	dip->di_dkst.dks_ncyl = dlp->d_ncylinders;	/* # cylinders */
	dip->di_dkst.dks_rps = dlp->d_rpm / 60;		/* # revolutions/second */

	for (i = 0; i < PNUM; i++) {
		disz(i) = dlsz(i);
		diof(i) = dlof(i);
	}
}
/* #endif /* DISKINFO */

static
sdst2disklabel(unit, st, dlp)
	int unit;	/*XXX*/
	register struct sdst *st;
	register struct disklabel *dlp;
{
	register int i;

	dlp->d_type = DTYPE_SCSI;			/* drive type */
	dlp->d_secsize = sdd_softc[unit].sdd_sectsize;	/* # of bytes per sector */
	dlp->d_nsectors = st->nsect;	/* # sectors/track */
	dlp->d_ntracks = st->ntrak;	/* # tracks/cylinder */
	dlp->d_ncylinders = st->ncyl;	/* # cylinders */
	dlp->d_secpercyl = st->nspc;	/* # sectors/cylinder */
	dlp->d_rpm = st->rps * 60;	/* # revolutions / minute */
	dlp->d_bbsize = BBSIZE;	/*XXX*/	/* size of boot area at sn0, bytes */
	dlp->d_sbsize = SBSIZE;	/*XXX*/	/* max size of fs superblock, bytes */

	for (i = 0; i < PNUM; i++) {
		dlsz(i) = stsz(i);
		dlof(i) = stof(i);
	}
}

#undef stsz
#undef stof
#undef dlsz
#undef dlof
#undef disz
#undef diof

sd_calcpart(ii, disk_sizes, nsect, sectsize)
	register struct iop/**/_device *ii;
	register struct size disk_sizes[];
	int nsect;
	int sectsize;
{
	register struct defpart *dp;
	register int size_mb;
	register int i;
	int psize;

	size_mb = nsect * sectsize / (1024 * 1024);

	for (dp = defpart_std; dp->range_max; dp++)
		if ((dp->range_min <= size_mb) && (size_mb < dp->range_max))
			break;

	/* PASS1 */
	for (i = 0; i < PNUM; i++) {
		psize = dp->partsize[i];

		switch (psize) {

		case PART_UNUSED:
			disk_sizes[i].sd_nblocks = 0;
			break;

		case PART_SPEC:
			disk_sizes[i].sd_nblocks = nsect * sectsize / DEV_BSIZE;
			break;

		case PART_CALCF:
		case PART_CALCG:
			break;

		default:
			disk_sizes[i].sd_nblocks = psize;
			break;
		}
	}

	/* PASS2 */
	for (i = 0; i < PNUM; i++) {
		psize = dp->partsize[i];

		switch (psize) {

		case PART_UNUSED:
		case PART_SPEC:
			break;

		case PART_CALCF:
			disk_sizes[i].sd_nblocks =
				disk_sizes[PART_C].sd_nblocks - 
				(disk_sizes[PART_A].sd_nblocks +
				 disk_sizes[PART_B].sd_nblocks +
				 disk_sizes[PART_D].sd_nblocks +
				 disk_sizes[PART_E].sd_nblocks +
				 disk_sizes[PART_H].sd_nblocks);
			break;

		case PART_CALCG:
			disk_sizes[i].sd_nblocks =
				disk_sizes[PART_C].sd_nblocks - 
				(disk_sizes[PART_A].sd_nblocks +
				 disk_sizes[PART_B].sd_nblocks +
				 disk_sizes[PART_H].sd_nblocks);
			break;

		default:
			break;
		}
	}

	/* OFFSET */
	disk_sizes[PART_A].sd_blkoff = 0;
	disk_sizes[PART_B].sd_blkoff = disk_sizes[PART_A].sd_nblocks;
	disk_sizes[PART_C].sd_blkoff = 0;
	disk_sizes[PART_D].sd_blkoff = disk_sizes[PART_A].sd_nblocks
					+ disk_sizes[PART_B].sd_nblocks
					+ disk_sizes[PART_H].sd_nblocks;
	disk_sizes[PART_E].sd_blkoff = disk_sizes[PART_D].sd_blkoff
					+ disk_sizes[PART_D].sd_nblocks;
	disk_sizes[PART_F].sd_blkoff = disk_sizes[PART_E].sd_blkoff
					+ disk_sizes[PART_E].sd_nblocks;
	disk_sizes[PART_G].sd_blkoff = disk_sizes[PART_D].sd_blkoff;

	if (disk_sizes[PART_H].sd_nblocks == 0)
		disk_sizes[PART_H].sd_blkoff = 0;
	else {
		disk_sizes[PART_H].sd_blkoff =
			disk_sizes[PART_A].sd_nblocks +
			disk_sizes[PART_B].sd_nblocks;
	}

	for (i = 0; i < PNUM; i++)
		if (disk_sizes[i].sd_nblocks == 0)
			disk_sizes[i].sd_blkoff = 0;
}

int sd_str_pr = 0;

sdstrategy(bp)
	register struct buf *bp;
{
	register struct iop/**/_device *ii;
	register struct sdst *st;
	register struct buf *dp;
	register int unit;
	register int ssize;
	struct sdd_softc *sdd;
	struct sdc_softc *sdc;
	long bn;
	int xunit;
	int s;

	xunit = dev2part(bp->b_dev);
	unit = dev2unit(bp->b_dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0 || ii->ii_alive == 0)
		goto bad;

	if (bp != &csdbuf[unit]) {
		/*
		 * READ / WRITE command
		 */
		sdd = &sdd_softc[unit];
		if (sdd->sdd_flags & SDDF_NONFMT)
			goto bad;
		sdc = &sdc_softc[ii->ii_ctlr];
		ssize = sdd->sdd_sectsize;
		if ((ssize != DEV_BSIZE)
			&& ((((dkblock(bp) * DEV_BSIZE) % ssize) != 0)
				|| (((bp->b_flags & B_READ) == 0) &&
				    ((bp->b_bcount % ssize) != 0)))) {
			goto bad;
		}

		st = &sdstdrv[unit];
		bn = dkblock(bp);
		bp->b_resid = 0;
		if ((bn < 0) || (bn >= st->sizes[xunit].sd_nblocks))
			goto bad2;
		if (sd_access_check(bp) < 0)
			goto bad2;

#ifdef notdef /* KU: XXX */
		bp->b_cylin = (bn + st->sizes[xunit].sd_blkoff) / st->nspc;
	} else {
		bp->b_cylin = 0;
#endif
	}

	s = splsc();
	dp = &sdutab[ii->ii_unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		sdustart(ii);
		bp = &ii->ii_mi->im_tab;
		if (bp->b_actf && bp->b_active == 0)
			sdstart(ii->ii_mi);
	}
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
	goto done;
bad2:
	bp->b_resid = bp->b_bcount;
done:
	iodone(bp);
}

/*
 * Unit start routine.
 */
sdustart(ii)
	register struct iop/**/_device *ii;
{
	register struct iop/**/_ctlr *im;
	register struct buf *dp;

	if (ii == NULL)
		return;
	im = ii->ii_mi;
	dk_busy &= ~(1 << ii->ii_dk);
	dp = &sdutab[ii->ii_unit];
	if (dp->b_actf == NULL)
		return;
	/*
	 * If the controller is active, just remember
	 * that this device would like to be positioned ...
	 * if we tried to position now we would confuse the SD.
	 */
	if (im->im_tab.b_active) {
		sdc_softc[im->im_ctlr].sdc_softas |= (1 << ii->ii_slave);
		return;
	}
	/*
	 * If we have already positioned this drive,
	 * then just put it on the ready queue.
	 */
	if (dp->b_active == 0)
		dp->b_active = 1;
	/*
	 * Device is ready to go
	 * put it on the ready queue for the controller
	 * (unless its already there.)
	 */
	if (dp->b_active != 2) {
		im->im_tab.b_actf = dp;
		dp->b_active = 2;
	}
}

/*
 * Start up a transfer on a drive.
 */
sdstart(im)
	register struct iop/**/_ctlr *im;
{
	register struct buf *bp;
	register struct buf *dp;
	register struct sdc_softc *sdc;

loop:
	/*
	 * Pull a request off the controller queue.
	 */
	if ((dp = im->im_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL)
		return;
	/*
	 * Mark controller busy, and
	 * determine destination of this request.
	 */
	im->im_tab.b_active++;

	sdexec(bp);
}

void
sdexec(bp)
	register struct buf *bp;
{
	register struct iop/**/_device *ii;
	register struct buf_stat *bs;
	register struct scsi *sc;
	register int ssize;
	register int unit;
	register int intr;
	register int bn;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;
	struct sdst *st;
	int sz;
	int over;
	struct sc_map *map;

	unit = dev2unit(bp->b_dev);
	ii = sddinfo[unit];
	intr = ii->ii_intr;
	sdd = &sdd_softc[unit];
	sdc = &sdc_softc[ii->ii_ctlr];

	sc = get_scsi(intr);

	if (bp == &csdbuf[unit]) {	/* do sdcmd() */
		bcopy((caddr_t)&kernscsi[unit],
			(caddr_t)sc, sizeof(struct scsi));
		if (bp->b_un.b_addr == NULL) {
			map = 0;
		} else {
			map = get_sc_map(intr);
			sc->sc_map = (struct sc_map *)ipc_phys(map);
		}
	} else {			/* R/W */
		ssize = sdd->sdd_sectsize;

		st = &sdstdrv[unit];
		bn = dkblock(bp);
		if (sdd->sdd_lastblk / st->nspc != bn / st->nspc)
			dk_seek[ii->ii_dk]++;

		st = &sdstdrv[unit];
		bn = dkblock(bp);

		/*
		 * Setup for the transfer, and get in the IOP queue.
		 */
		scinit(sc, ii->ii_slave, ssize); 
		sc->sc_ctrnscnt = bp->b_bcount - bp->b_resid;
		map = get_sc_map(intr);
		sc->sc_map = (struct sc_map *)ipc_phys(map);

		/* cdb */
		sc->sc_cdb.un_type1.t1_opcode =
				(bp->b_flags & B_READ) ?	SCOP_EREAD :
				(ii->ii_flags & SD_F_WRTVRFY) ?	SCOP_WRTVRFY :
								SCOP_EWRITE;
#ifdef mips
		{
		int v;

		v = (bn + st->sizes[dev2part(bp->b_dev)].sd_blkoff)
					* DEV_BSIZE / sdd->sdd_sectsize;
		sc->sc_ladhi = v >> 16;
		sc->sc_ladlo = v;

		v = (sc->sc_ctrnscnt + ssize - 1) / ssize;
		sc->sc_cdb.un_type1.t1_p2 = v >> 8;
		sc->sc_cdb.un_type1.t1_p3 = v;
		}
#else
		*(u_int *)(&sc->sc_cdb.un_type1.t1_ladhi) =
				(bn + st->sizes[dev2part(bp->b_dev)].sd_blkoff)
					* DEV_BSIZE / sdd->sdd_sectsize;
		*(u_short *)(&sc->sc_cdb.un_type1.t1_p2) =
				    (sc->sc_ctrnscnt + ssize -1) / ssize;
#endif
		if ((sdd->sdd_flags & SDDF_ERASEOFF)
				&& ((bp->b_flags & B_READ) == 0)) {
			sc->sc_ctrl = 0x40;
		}
	}

	sdc->sdc_firmware |= SDCFW_BUSY;
	iop/**/go(ii, map);
}

/*
 * Now all ready to go.
 */
sddgo(im)
	register struct iop/**/_ctlr *im;
{
	register int intr;

	im->im_tab.b_active = 2;
	intr = im->im_intr;

	sc_go(intr, get_scsi(intr), SCSI_INTEN);
}

/*
 * copyin(), copyout() can't use in the interrupt routine.
 *	because user process is changed.
 */
/*
 * Handle a disk interrupt.
 *	d: controller number
 */
sdintr(d)
	int d;
{
	register struct iop/**/_ctlr *im;
	register struct sdc_softc *sdc;
	register struct sdd_softc *sdd;
	register struct scsi *sc;
	register int intr;
	register int unit;
	register int slave;
	register int as;
	struct iop/**/_device *ii;
	struct sddevinfo *sdi;
	struct sc_extnd *sce;
	struct sdst *st;
	struct buf *bp;
	struct buf *dp;
	char *erp_page;
	int code;
	int len;
	int tstatus;
	int delay_start();
	int delay_medrmv();
	int wait_re_init_done();

	im = sdminfo[d];
	sdc = &sdc_softc[im->im_ctlr];
	intr = im->im_intr;
	as = sdc->sdc_softas;

	sdc->sdc_wticks = 0;
	sc = get_scsi(intr);
	/*
	 * If SDCS_IOCTL bit is set, then don't check error.
	 */
	if (sdc->sdc_state & SDCS_IOCTL) {
		sdc->sdc_state &= ~SDCS_IOCTL;
		sdd = &sdd_softc[(sdip[d][sc->sc_identify & IDT_DRMASK])->ii_unit];

		if (sdc->sdc_state & SDCS_SCUNLOCK) {
			int s;

			sdc->sdc_state &= ~SDCS_SCUNLOCK;
			s = splclock();
			if (sdc->sdc_state & SDCS_OPEN_WAIT) {
				sdc->sdc_state &= ~SDCS_OPEN_WAIT;
				wakeup((caddr_t)sdc);
			}
			splx(s);
			/*
			 * UNLOCK SCSI access
			 */
			sdc->sdc_firmware &= ~SDCFW_BUSY;
		}
		return;
	}

	im->im_tab.b_active = 1;
	/*
	 * Get device and block structures, and a pointer
	 * to the iop_device for the drive.
	 */
	dp = im->im_tab.b_actf;
	bp = dp->b_actf;
	unit = dev2unit(bp->b_dev);

	ii = sddinfo[unit];
	slave = ii->ii_slave;
	st = &sdstdrv[unit];
	dk_busy &= ~(1 << ii->ii_dk);
	sdd = &sdd_softc[unit];
	sdi = &sddevinfo[ii->ii_type];
	sce = (struct sc_extnd *)&sdc_rsense[ii->ii_ctlr][0];

	/*
	 * Check error on the drive.
	 */
	tstatus = sc->sc_tstatus & TGSTMASK;
	if (sc->sc_istatus != INST_EP) {
		/*
		 * initiator status is bad.
		 *	check & retry !!
		 */
		if ((sc->sc_istatus&(INST_EP|INST_PRE)) == (INST_EP|INST_PRE)) {
			/* detect parity error or abnormal terminate */
			if ((sc->sc_istatus & INST_LB) == 0)
				printf("sd%d: SCSI bus parity error\n", unit);
			sdc->sdc_countcc--;
			goto sdintr_exec;
		}
		if ((sc->sc_istatus & INST_EP) == 0) {
			if (sc->sc_istatus & (INST_WAIT | INST_IP | INST_WR)) {
				if (++sdc->sdc_retrycnt < NRETRY) {
					im->im_tab.b_active = 2;
					/*
					 * Konomama return sitemo,
					 * lost interrupt ni narudake deha
					 * naidarou ka ?
					 * Isso error ni sitahou ga
					 * ii nodeha naidarou ka ?
					 */
					return;
				}
			}
			printf("SCSI%d: abnormal termination\n", intr);
			printf("ISTAT = 0x%x, TSTAT = 0x%x\n",
					sc->sc_istatus, sc->sc_tstatus);
			if (++sdc->sdc_nhrderr >= MAXHRDERR) {
				printf("SCSI%d: too many hard errors\n", intr);
				sdc->sdc_nhrderr = 0;
				goto sdintr_error;
			}
			screset(intr);
			goto sdintr_exec;
		}
		if ((sc->sc_istatus & (INST_TO|INST_HE)) != 0) {
			if (sc->sc_istatus & INST_HE) {
				/*
				 * SCSI bus reset is occured.
				 *	to be continue --> hdreset()
				 */
				re_init_done = 0;
				timeout(wait_re_init_done, bp, 10*hz);
				return;
			}
			if (++sdc->sdc_nhrderr >= MAXHRDERR) {
				printf("SCSI%d: too many hard errors (ISTAT=0x%x)\n",
					intr, sc->sc_istatus);
				sdc->sdc_nhrderr = 0;
				goto sdintr_error;
			}
			if (++sdc->sdc_retrycnt >= NRETRY) {
				printf("SCSI%d: too many initiator errors (ISTAT=0x%x)\n",
					intr, sc->sc_istatus);
				goto sdintr_error;
			}
			DELAY(D100MSEC * 10);
			goto sdintr_exec;
		}
	}

	if (sdd->sdd_flags & SDDF_SKIPCHECK)
		goto sdintr_done;

check_target_status:
	/*
	 * check target status
	 */
	switch (sdc->sdc_state) {

	/********************************/
	/*				*/
	/*	NORMAL OPERATION	*/
	/*				*/
	/********************************/
	case SDCS_NORMAL:
		switch (tstatus) {

		case TGST_GOOD:
			break;

		case TGST_CC:
			sdc->sdc_state |= SDCS_RSENSE;
sdintr_rsense:
			im->im_tab.b_active = 2;
			bzero((caddr_t)sce, RSEN_CNT);
			scop_rsense(intr, sc, slave,
					SCSI_INTEN, RSEN_CNT, (caddr_t)sce);
			return;

		case TGST_BUSY:
			if (++sdc->sdc_retrycnt > MAXRETRYCNT) {
				goto sdintr_error;
			}
			timeout(sdexec, (caddr_t)bp, hz);
			return;

		default:
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		break;

	/****************************************/
	/*					*/
	/*	REQUEST SENSE analysis		*/
	/*					*/
	/****************************************/
	case SDCS_RSENSE:
	case SDCS_PREVRMB|SDCS_RSENSE:
	case SDCS_ECC|SDCS_RASREAD|SDCS_RSENSE:
	case SDCS_ECC|SDCS_RASWRITE|SDCS_RSENSE:
	case SDCS_ECCOFF|SDCS_RSENSE:
	case SDCS_ECCOFF|SDCS_RASBLK|SDCS_RSENSE:
	case SDCS_ECCOFF|SDCS_RASBLK|SDCS_LOSTDATA|SDCS_RSENSE:
	case SDCS_ECC|SDCS_RASBLK|SDCS_RSENSE:
	case SDCS_ECC|SDCS_RASBLK|SDCS_LOSTDATA|SDCS_RSENSE:
		if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		/*
		 * error message print out
		 */
		code = sderrordisp(sce, ii);


		if ((sdc->sdc_state == (SDCS_ECC|SDCS_RASBLK|SDCS_RSENSE)) ||
		    (sdc->sdc_state == (SDCS_ECC|SDCS_RASBLK|SDCS_LOSTDATA|SDCS_RSENSE))) {
			printf("sd%d: cannot reassign block %d\n",
						unit, sdd->sdd_badsect);
			goto sdintr_error;
		}
		if (sdc->sdc_state == (SDCS_PREVRMB|SDCS_RSENSE)) {
			if (sce->sce_skey == 0x2) {
				/*
				 * Not ready
				 */
				sdc->sdc_state = SDCS_PREVRMB;
				timeout(delay_medrmv, (caddr_t)ii, hz);
				return;
			}
		}

		/*			*/
		/* RSENSE error handler	*/
		/*			*/
		switch (code) {

		/********************************/
		/*	continue		*/
		/********************************/
		/* NO SENSE */
		case 0x00:	/* No Additional Sense Information */
		/* RECOVERED ERROR */
		case 0x38:	/* Recovered with Auto-Reallocation */
			sdc->sdc_state &= ~SDCS_RSENSE;
			goto check_target_status;

		/********************************/
		/*	continue or error	*/
		/********************************/
		/* ILLEGAL REQUEST */

		case 0x21:	/* illegal Logical Block Address */
			if (&st->sizes[dev2part(bp->b_dev)] == NULL)
				goto sdintr_error;
			if (bp->b_bcount > 0) {
				bp->b_resid = bp->b_bcount
				    - (sdd->sdd_badsect * sdd->sdd_sectsize
				    - (st->sizes[dev2part(bp->b_dev)].sd_blkoff
				    + dkblock(bp)) * DEV_BSIZE);
			}
			if (bp->b_resid >= bp->b_bcount || bp->b_resid <= 0) {
				/*
				 * all I/O failure
				 */
				bp->b_resid = bp->b_bcount;
				goto sdintr_error;
			}
			/* Ignore error */
			break;

		/* MEDIUM ERROR */
		case 0x31:	/* Medium Format Corrupted */
			sdd->sdd_flags |= SDDF_NONFMT;
			/* Ignore error */
			break;

		/********************************/
		/*	more retry		*/
		/********************************/
		/* MEDIUM or RECOVERED ERROR */
		case 0x10:	/* ID CRC Error */
		case 0x12:	/* No Address Mark found in ID field */
		case 0x13:	/* No Address Mark found in Data field */
		case 0x14:	/* No recode found */
		/* H/W or MEDIUM or RECOVERED ERROR */
		case 0x15:	/* Seek Positioning Error */
			if (sd_ignore_error) {
				sdc->sdc_state = SDCS_NORMAL;
				goto check_target_status;
			}
			/* fall through */

		/* H/W ERROR */
		case 0x01:	/* No Index/Address Mark Found signal */
		case 0x02:	/* No Seek Complete */
		case 0x06:	/* No Track Zero found */
		/* H/W ERROR or RECOVERED ERROR */
		case 0x03:	/* Write Fault */
		case 0x08:	/* Logical Unit Communication Failure */
		case 0x09:	/* Track Following Error */
		case 0x0b:	/* Load/Unload Failure */
		case 0x0c:	/* Spindle Failure */
		case 0x0d:	/* Focus Failure */
		case 0x0e:	/* Tracking Failure */
		case 0x0f:	/* Drive Initialization Failure */
			sdc->sdc_state = SDCS_ECC|SDCS_ECC_HOLD|SDCS_REZERO;

			scinit(sc, slave, sdd->sdd_sectsize); 
			/* sc_cdb */
			sc->sc_opcode = SCOP_REZERO;

			sddgo(im);
			return;

		/********************************/
		/*	re-allocate & retry	*/
		/********************************/
		/* MEDIUM or RECOVERED ERROR */
		case 0x11:	/* Unrecovered Read Error */
			if (sdc->sdc_state & SDCS_RASREAD) {
sdintr_lostdata:
				sdc->sdc_state =
					SDCS_ECC|SDCS_RASBLK|SDCS_LOSTDATA;
				im->im_tab.b_active = 2;
				scop_rasblk(intr, sc, slave,
					SCSI_INTEN, sdd->sdd_badsect);
				sdd->sdd_flags &= ~SDDF_VBADSECT;
				return;
			}
			/* fall through */

		/* RECOVERED ERROR */
		case 0x17:	/* Recovered read data with retries */
		case 0x18:	/* Recovered read data with ECC */
			/*
			 * set ECC ON & more retry
			 */
			if (sdc->sdc_firmware & SDCFW_DEFMODE)
				goto sdintr_ecc;

			if (sdc->sdc_state & SDCS_RASREAD)
				goto sdintr_rasblk;

			sdc->sdc_state = SDCS_ECC;
			goto sdintr_msel_set;

		/********************************/
		/*	unit start & retry	*/
		/********************************/
		/* NOT READY */
		case 0x04:	/* Drive Not Ready */
			if (sdc->sdc_state & SDCS_ECC)
				sdc->sdc_state = SDCS_ECCOFF|SDCS_RETRY;
			else
				sdc->sdc_state = SDCS_RETRY;
			goto sdintr_stst;

		/********************************/
		/*	retry			*/
		/********************************/
		/* UNIT ATTENTION */
		case 0x28:	/* Medium Changed */
			sdd->sdd_flags &= ~(SDDF_NONFMT
					    |SDDF_SAMEDSK
					    |SDDF_REQ_EJECT
					    |SDDF_XUSE
					    |SDDF_INHRMV
					    |SDDF_ERASEOFF);
			/* fall through */

		case 0x29: /* Power On or Reset or Bus Device Reset */
			if (sdc->sdc_firmware & SDCFW_RMB) {
				/***************************/
				/* medium removable device */
				/***************************/
				sdc->sdc_state = SDCS_PREVRMB;
				im->im_tab.b_active = 2;
				scop_medrmv(intr, sc, slave,
					    SCSI_INTEN, SDRMV_PREV);
				return;
			}

		case 0x2a:	/* Mode Select Parameter Changed */
		case 0x47:	/* SCSI interface bus parity error */
			if (sdc->sdc_state & SDCS_ECC) {
				sdc->sdc_state = SDCS_RETRY;
				goto sdintr_msel_reset;
			}
			sdc->sdc_state = SDCS_NORMAL;
			goto sdintr_exec;

		/********************************/
		/*	set error flag		*/
		/********************************/
		case 0x40:	/* RAM failure */
		case 0x41:	/* Data Path diagnostic failure */
		case 0x42:	/* Power On diagnostic failure */

		case 0xb2:	/* Caddy load/eject failed */
		case 0xb4:	/* Focus servo failure */
		case 0xb5:	/* Spindle servo failure */
		case 0xb6:	/* Caddy load mechanism failed */
			goto sdintr_error;

/*MO*/		case 0x80:	/* Limit Laser Life */
/*MO*/		case 0x81:	/* Focus Coil Over-current Failure */
/*MO*/		case 0x82:	/* Tracking Coil Over-current Failure */
/*MO*/		case 0x83:	/* Temperature Alarm */
/*CD*/	/*	case 0x80: */	/* Prevent bit is set */
/*CD*/	/*	case 0x81: */	/* Logical unit is reserved */
/*CD*/	/*	case 0x82: */	/* End of usr area encountered */
/*CD*/	/*	case 0x83: */	/* Overlapped commands attempted */
			goto sdintr_error;

		default:
			/*
			 *	error detect, but what shall we do ?
			 */
	/*	case 0x05: */	/* Drive Not Selected */
	/*	case 0x07: */	/* Multiple Drives Selected */
	/*	case 0x0a: */	/* No disk */
	/*	case 0x1a: */	/* Parameter overrun */
	/*	case 0x1b: */	/* Synchronous transfer error */
	/*	case 0x1d: */	/* Compare error */
	/*	case 0x22: */	/* Illegal function for device type */
	/*	case 0x23: */	/* Illegal function for Medium type */
	/*	case 0x25: */	/* Illegal LUN */
	/*	case 0x27: */	/* Write Protected */
	/*	case 0x2b: */	/* Firmware has been downloaded */
	/*	case 0x39: */	/* Automatic Reallocation Failure */
	/*	case 0x43: */	/* Message Reject Error */
	/*	case 0x45: */	/* Selection/Reselection failure */
	/*	case 0x48: */	/* Initiator detected error */
	/*	case 0x49: */	/* Inappropriate/illegal message */
	/*	case 0x60: */	/* COPY: STATUS error */
	/*	case 0x85: */	/* Audio address not valid */
	/*	case 0xb0: */	/* Caddy not inserted in drive */
	/*	case 0xb1: */	/* Unable to recover TOC */
	/*	case 0xb3: */	/* CIRC unrecovered data error(L-EC off) */
	/*	case 0xc3: */	/* COPY: Illegale CDB length */
	/*	case 0xc5: */	/* COPY: Catastrophic error */
	/*	case 0xc6: */	/* COPY: Illegal phase change */
	/*	case 0xfc: */	/* COPY: MODE SENSE failed */

			/*
			 *	medium error
			 */
	/*	case 0x19: */	/* Defect list error */
	/*	case 0x1c: */	/* Primary Defect List not found */
	/*	case 0x30: */	/* Incompatible Cartridge */
	/*	case 0x32: */	/* No Spare Defect Location Available */
	/*	case 0x3a: */	/* Defect List Update Failure */
	/*	case 0x3d: */	/* Defect List Not Available */
			goto sdintr_error;
		}
		/*
		 * No error detected or ignored.
		 */
		break;

	/************************************************/
	/*						*/
	/*	PREVENT MEDIUM REMOVABLE COMMAND	*/
	/*						*/
	/************************************************/
	case SDCS_PREVRMB:
		if (tstatus == TGST_CC) {
			sdc->sdc_state = SDCS_PREVRMB|SDCS_RSENSE;
			goto sdintr_rsense;
		}
		sdd->sdd_flags |= SDDF_INHRMV;
		if (sdc->sdc_state & SDCS_ECC) {
			sdc->sdc_state = SDCS_RETRY;
			goto sdintr_msel_reset;
		}
		sdc->sdc_state = SDCS_NORMAL;
		goto sdintr_exec;
		break;

	/****************************************/
	/*					*/
	/*	REZERO done & RETRY COMMAND	*/
	/*					*/
	/****************************************/
	case SDCS_ECC|SDCS_ECC_HOLD|SDCS_REZERO:
		if (sdc->sdc_firmware & SDCFW_DEFMODE) {
			sdc->sdc_state = SDCS_ECC|SDCS_ECC_HOLD|SDCS_RETRY;
			goto sdintr_stst;
		}

		sdc->sdc_state = SDCS_ECC|SDCS_ECC_HOLD;
		goto sdintr_msel_set;

	/********************************/
	/*				*/
	/*	RETRY COMMAND		*/
	/*				*/
	/********************************/
	case SDCS_RETRY:
		sdc->sdc_state = SDCS_NORMAL;
		goto sdintr_exec;

	/************************************************/
	/*						*/
	/*	ERROR CORRECTION ON MODE SELECT result	*/
	/*						*/
	/************************************************/
	case SDCS_ECC:
		if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			goto sdintr_error;
		}
sdintr_ecc:
		if (bp->b_flags & B_READ) {
			sdc->sdc_state = SDCS_ECC|SDCS_RASREAD;
			im->im_tab.b_active = 2;
			scop_rdwr(intr, sc, slave, SCSI_INTEN,
					B_READ, sdwork,
					sdd->sdd_badsect, sdd->sdd_sectsize);
			return;
		}
		goto sdintr_rasblk;

	/************************************************/
	/*						*/
	/*	READ DATA from BAD BLOCK result		*/
	/*						*/
	/************************************************/
	case SDCS_ECC|SDCS_RASREAD:
		if (tstatus == TGST_CC) {
			sdc->sdc_state = SDCS_ECC|SDCS_RASREAD|SDCS_RSENSE;
			goto sdintr_rsense;
		} else if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			printf("sd%d: cannot read block\n", unit);
			goto sdintr_error;
		}
sdintr_rasblk:
		if (sdd->sdd_flags & SDDF_WPROTECT)
			goto sdintr_error;
		sdc->sdc_state = SDCS_ECC|SDCS_RASBLK;
		im->im_tab.b_active = 2;
		scop_rasblk(intr, sc, slave, SCSI_INTEN, sdd->sdd_badsect);
		sdd->sdd_flags &= ~SDDF_VBADSECT;
		return;

	/****************************************/
	/*					*/
	/*	REASSIGN BLOCK result		*/
	/*					*/
	/****************************************/
	case SDCS_ECC|SDCS_RASBLK:
		if (tstatus == TGST_CC) {
			sdc->sdc_state = SDCS_ECC|SDCS_RASBLK|SDCS_RSENSE;
			goto sdintr_rsense;
		} else if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		printf("sd%d: block %d is reassigned\n",
			unit, sdd->sdd_badsect);
		if (bp->b_flags & B_READ) {
sdintr_raswrite:
			sdc->sdc_state = SDCS_ECC|SDCS_RASWRITE;
			im->im_tab.b_active = 2;
			scop_rdwr(intr, sc, slave, SCSI_INTEN,
					B_WRITE, sdwork,
					sdd->sdd_badsect, sdd->sdd_sectsize);
			return;
		}
		sdc->sdc_state = SDCS_RETRY;
		goto sdintr_msel_reset;

	/************************************************/
	/*						*/
	/*	WRITE DATA to REASSIGNED BLOCK result	*/
	/*						*/
	/************************************************/
	case SDCS_ECC|SDCS_RASWRITE:
		if (tstatus == TGST_CC) {
			sdc->sdc_state = SDCS_ECC|SDCS_RASWRITE|SDCS_RSENSE;
			goto sdintr_rsense;
		} else if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
						unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		sdc->sdc_state = SDCS_RETRY;
		goto sdintr_msel_reset;

	/****************************************/
	/*					*/
	/*	reset ECC & RETRY TIMES		*/
	/*					*/
	/****************************************/
	case SDCS_ECCOFF|SDCS_RETRY:
		sdc->sdc_state = SDCS_RETRY;
		goto sdintr_msel_reset;

	/********************************************************/
	/*							*/
	/*	READ DATA from BAD BLOCK result in faliure	*/
	/*							*/
	/********************************************************/
	case SDCS_ECC|SDCS_RASBLK|SDCS_LOSTDATA:
		if (tstatus == TGST_CC) {
			sdc->sdc_state =
				SDCS_ECC|SDCS_RASBLK|SDCS_LOSTDATA|SDCS_RSENSE;
			goto sdintr_rsense;
		} else if (tstatus != TGST_GOOD) {
			printf("sd%d: rasblk: bad target status 0x%x\n",
				unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		bzero(sdwork, sdd->sdd_sectsize);
		scop_rdwr(intr, sc, slave, SCSI_INTDIS,
			B_WRITE, sdwork, sdd->sdd_badsect, sdd->sdd_sectsize);
		printf("sd%d: block %d is reassigned (lost data)\n",
			unit, sdd->sdd_badsect);
		goto sdintr_error;

	/****************************************/
	/*					*/
	/*	issue START UNIT command	*/
	/*					*/
	/****************************************/
	case SDCS_ECC|SDCS_ECC_HOLD:
		/*
		 * Drive not ready... so start..
		 */
		sdc->sdc_state = SDCS_ECC|SDCS_ECC_HOLD|SDCS_RETRY;
sdintr_stst:
		timeout(delay_start, (caddr_t)ii, hz);
		return;

	/****************************************/
	/*					*/
	/*	RETRY with ECC & more RETRYS	*/
	/*					*/
	/****************************************/
	case SDCS_ECC|SDCS_ECC_HOLD|SDCS_RETRY:
		sdc->sdc_state = SDCS_ECCOFF;
sdintr_exec:
		if (sdc->sdc_countcc++ > MAXRETRYCNT)
			goto sdintr_error;
		sdexec(bp);
		return;

	/****************************************/
	/*					*/
	/*	reset ECC & RETRY TIMES		*/
	/*					*/
	/****************************************/
	case SDCS_ECCOFF:
		if (tstatus == TGST_CC) {
			sdc->sdc_state = SDCS_ECCOFF|SDCS_RSENSE;
			goto sdintr_rsense;
		} else if (tstatus != TGST_GOOD) {
			printf("sd%d: bad target status 0x%x\n",
				unit, sc->sc_tstatus);
			goto sdintr_error;
		}
		sdc->sdc_state = SDCS_NORMAL;
		goto sdintr_msel_reset;

sdintr_msel_set:
		/*
		 * set more ERROR RECOVERY PARAMETERS
		 */
		if ((erp_page = sdi->max_ERP_page) == NULL)
			goto check_target_status;
		bzero((caddr_t)sc->sc_param, 4);
		len = *(erp_page + 1) + 2;
		bcopy(erp_page, &sc->sc_param[4], len);

		im->im_tab.b_active = 2;
		scop_mselect(intr, sc, slave, SCSI_INTEN,
				(SDM_PF<<24) + len +4, (caddr_t)0);
		return;

sdintr_msel_reset:
		if (sdc->sdc_firmware & SDCFW_DEFMODE)
			goto sdintr_exec;

		/*
		 * set normal ERROR RECOVERY PARAMETERS
		 */
		erp_page = sdi->ERP_page;
		bzero((caddr_t)sc->sc_param, 4);
		len = *(erp_page + 1) + 2;
		bcopy(erp_page, &sc->sc_param[4], len);

		im->im_tab.b_active = 2;
		scop_mselect(intr, sc, slave, SCSI_INTEN,
				(SDM_PF<<24) + len +4, (caddr_t)0);
		return;

sdintr_error:
		bp->b_flags |= B_ERROR;
		if (sdc->sdc_state & SDCS_ECC) {
			sdc->sdc_state = SDCS_NORMAL;
			goto sdintr_msel_reset;
		}
		break;

	/*
	 * UNKNOWN STATUS
	 */
	default:
		printf("sd%d: unknown status (0x%x)\n", unit, sdc->sdc_state);
		goto sdintr_error;
	}

sdintr_done:

	if (bp->b_flags & B_ERROR) {
        printf("%s%d%c: hard error sn%d ", "sd",
            minor(bp->b_dev) >> 3, 'a'+(minor(bp->b_dev)&07), bp->b_blkno);
		printf("\n");
	}
	sdd->sdd_lastblk = dkblock(bp) + btodb(bp->b_bcount - bp->b_resid);
	sdc->sdc_countcc = 0;
	sdc->sdc_retrycnt = 0;
	sdd->sdd_flags &= ~SDDF_VBADSECT;

	if (im->im_tab.b_active) {
		im->im_tab.b_active = 0;
		im->im_tab.b_errcnt = 0;
		im->im_tab.b_actf = 0;
		dp->b_active = 0;
		dp->b_errcnt = 0;
		dp->b_actf = bp->b_actf;
		if (bp == &csdbuf[unit]) {
			register struct scsi *ksc = &kernscsi[unit];
			/* copy result */
			bcopy((caddr_t)sc, (caddr_t)ksc, sizeof(struct scsi));
		}

		iodone(bp);

		/*
		 * If this unit has more work to do,
		 * then start it up right away.
		 */
		if (dp->b_actf)
			sdustart(ii);
	}
	as &= ~(1 << slave);

	sdc->sdc_state = SDCS_NORMAL;

	/*
	 * UNLOCK SCSI access
	 */
	sdc->sdc_firmware &= ~SDCFW_BUSY;

start:
	/*
	 * Process other units which need attention.
	 * For each unit which needs attention, call
	 * the unit start routine to place the slave
	 * on the controller device queue.
	 */
	sdc->sdc_softas = 0;
	while (unit = ffs(as)) {
		unit--;
		as &= ~(1 << unit);
		sdustart(sdip[im->im_ctlr][unit]);
	}
	/*
	 * If the controller is not transferring,
	 * but there are devices ready to transfer,
	 * start the controller.
	 */
	if (im->im_tab.b_actf && im->im_tab.b_active == 0)
		(void) sdstart(im);
}

wait_re_init_done(bp)
	register struct buf *bp;
{
	if (re_init_done >= 2)
		sdexec(bp);
	else
		timeout(wait_re_init_done, bp, 10*hz);
}

delay_start(ii)
	struct iop/**/_device *ii;
{
	ii->ii_mi->im_tab.b_active = 2;
	scop_stst(ii->ii_intr, get_scsi(ii->ii_intr), ii->ii_slave,
			SCSI_INTEN, SDSS_START);
}

delay_medrmv(ii)
	struct iop/**/_device *ii;
{
	ii->ii_mi->im_tab.b_active = 2;
	scop_medrmv(ii->ii_intr, get_scsi(ii->ii_intr), ii->ii_slave,
			SCSI_INTEN, SDRMV_PREV);
}

sderrordisp(rsen_data, ii)
	u_char *rsen_data;
	struct iop/**/_device *ii;
{
	register struct sc_extnd *sce;
	register struct sdc_softc *sdc;
	register struct sdd_softc *sdd;
	register int unit;
	register int code;
	struct sc_nextnd *scn;
	struct msg_list *ml;

	unit = ii->ii_unit;
	sdc = &sdc_softc[ii->ii_ctlr];
	sdd = &sdd_softc[unit];

	sce = (struct sc_extnd *)rsen_data;

	if (sce->sce_extend == 0x70) {
		/*
		 * Extended Sense data
		 */
		code = sce->sce_sdecode;

		if (code & 0x80)
			ml = ecodelist_mo;
		else
			ml = ecodelist;

		if (sce->sce_advalid) {
			if ((sdd->sdd_flags & SDDF_VBADSECT) == 0) {
#ifdef mips
				sdd->sdd_badsect = (sce->sce_infob1 << 24) +
						   (sce->sce_infob2 << 16) +
						   (sce->sce_infob3 <<  8) +
						   (sce->sce_infob4);
#else
				sdd->sdd_badsect = *((int *)&sce->sce_infob1);
#endif
				sdd->sdd_flags |= SDDF_VBADSECT;
			}
		}

		if (!rsense_msg_disp && !isdispmsg(code, ml, sdc->sdc_countcc))
			return (code);

		if (sce->sce_advalid) {
			int sn;
#ifdef mips
			sn = (sce->sce_infob1 << 24) +
			     (sce->sce_infob2 << 16) +
			     (sce->sce_infob3 <<  8) +
			     (sce->sce_infob4);
#else
			sn = *((int *)&sce->sce_infob1);
#endif
			if (sce->sce_addlen >= 5) {
				printf("sd%d(sn %d): skey=0x%x, code=0x%x\n",
					unit, sn, sce->sce_skey, code);
			} else {
				printf("sd%d(sn %d): skey=0x%x\n",
					unit, sn, sce->sce_skey);
			}
		} else {
			if (sce->sce_addlen >= 5)
				printf("sd%d: skey=0x%x, code=0x%x\n",
					unit, sce->sce_skey, code);
			else
				printf("sd%d: skey=0x%x\n",
					unit, sce->sce_skey);
		}
		if (sce->sce_addlen >= 6)
			printf("sd%d: ASCQ=0x%x\n", unit, sce->sce_ascq);
		{
			u_char *p;
			int len;

			len = 8 + sce->sce_addlen;
			if (len > RSEN_CNT)
				len = RSEN_CNT;
			p = (u_char *)sce;
			printf("sd%d: ", unit);
			while (len--)
				printf("%x ", *p++);
			printf("\n");
		}
	} else {
		/*
		 * Non-extended Sense data
		 */
		scn = (struct sc_nextnd *)rsen_data;

		code = scn->scn_ecode;
		ml = ecodelist;
		if (sce->sce_advalid) {
			if ((sdd->sdd_flags & SDDF_VBADSECT) == 0) {
				sdd->sdd_badsect = scn->scn_secno;
				sdd->sdd_flags |= SDDF_VBADSECT;
			}
		}
		if (rsense_msg_disp || isdispmsg(code, ml, sdc->sdc_countcc)) {
			if (sce->sce_advalid)
				printf("sd%d(sn %d): code=0x%x\n",
					unit, scn->scn_secno, code);
			else
				printf("sd%d: code=0x%x\n", unit, code);
		}
	}
	return (code);
}

void
sdminphys(bp)
	struct buf *bp;
{
	if (bp->b_bcount > MAXSDPHYS)
		bp->b_bcount = MAXSDPHYS;
}


#define	sdphysio	physio

sdread(dev, uio, flag)
	register dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct iop/**/_device *ii;
	register int unit;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0)
		return (ENXIO);

	return (sdphysio(sdstrategy, &rsdbuf[unit], dev, B_READ, sdminphys, uio));
}

sdwrite(dev, uio, flag)
	register dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct iop/**/_device *ii;
	register int unit;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0)
		return (ENXIO);

	return (sdphysio(sdstrategy, &rsdbuf[unit], dev, B_WRITE, sdminphys, uio));
}

#define MAXBL 256

/*ARGSUSED*/
sdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register struct iop/**/_device *ii;
	register struct sc_ureq *scu;
	register struct sdst *st;
	register struct scsi *sc;
	register int unit;
	register int ctlr;
	register int slave;
	struct sdc_softc *sdc;
	struct sdd_softc *sdd;
	struct Partinfo *pi;
	struct dkst *di;
	struct buf *bp;
	struct sddevinfo *sdi;
	struct sdst *stp;
	struct scsi uscsi;
	int error;
	int i;
	int s;
	int tstatus;
	char *p;
	int blkno, count;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0 || ii->ii_alive == 0)
		return (ENXIO);

	slave = ii->ii_slave;
	ctlr = ii->ii_ctlr;
	sdc = &sdc_softc[ctlr];
	sdd = &sdd_softc[unit];
	sc = &uscsi;

	error = 0;
	switch (cmd) {

	case DIOCWLABEL:
		if (*(int *)data & SD_F_ENW)
			ii->ii_flags |= SD_F_ENW;
		else
			ii->ii_flags &= ~SD_F_ENW;
		break;

	case DIOCGDINFO:
		*(struct disklabel *)data = sdlabel[unit];
		break;

	case DIOCSDINFO:
		sdlabel[unit] = *(struct disklabel *)data;
		disklabel2sdst(unit, &sdlabel[unit], &sdstdrv[unit]);
		break;

	case DIOCWDINFO:
	case DKIOCRGEOM:
		switch (cmd) {
		case DKIOCRGEOM:
			st = &sdstdrv[unit];
			sdi = &sddevinfo[ii->ii_type];
			stp = sdi->sdstp;

			st->ncyl = stp->ncyl;	/* # cylinders / drive */
			st->ntrak = stp->ntrak;	/* # tracks / cylinder */
			st->nsect = stp->nsect;	/* # sectors / track */
			st->rps = stp->rps;	/* # revolutions / second */

			sdst2disklabel(unit, st, &sdlabel[unit]);

			if (*(int *)data == RGEOM_SDINFO)
				goto done;

			break;

		case DIOCWDINFO:
			sdlabel[unit] = *(struct disklabel *)data;

			break;
		}

		/*
		 * Common code for DIOCWDINFO and DKIOCRGEOM
		 */

		/**** READ sector 0 ****/
		/*
		 * LOCK sdtmp buffer
		 */
		s = splclock();
		while (sdtmp_stat & B_BUSY) {
			sdtmp_stat |= B_WANTED;
			sleep((caddr_t)sdtmp, PRIBIO);
		}
		sdtmp_stat |= B_BUSY;
		splx(s);

		bzero(sdtmp, DEV_BSIZE);

		if ((sdd->sdd_flags & SDDF_NONFMT) == 0) {
			scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
			uscsi.sc_cpoint = sdtmp;
			uscsi.sc_ctrnscnt = DEV_BSIZE;
			uscsi.sc_opcode = SCOP_READ;
			uscsi.sc_lad = 0;
			uscsi.sc_count = 1;
			error = sdcmd(dev, &uscsi);
		} else {
			error = EIO;
		}

		if (error) {
			/*
			 * UNLOCK sdtmp buffer
			 */
			s = splclock();
			if (sdtmp_stat & B_WANTED)
				wakeup((caddr_t)sdtmp);
			sdtmp_stat &= ~(B_BUSY|B_WANTED);
			splx(s);

			break;
		}

		*(struct disklabel *)(sdtmp + LABELOFFSET) = sdlabel[unit];
		disklabel2diskinfo(unit, &sdlabel[unit],
				    &((struct firstsector *)sdtmp)->diskinfo);

		/**** WRITE sector 0 ****/

		if (error == 0) {
			if ((sdd->sdd_flags & SDDF_NONFMT) == 0) {
				scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize);
				uscsi.sc_cpoint = sdtmp;
				uscsi.sc_ctrnscnt = DEV_BSIZE;
				uscsi.sc_opcode = SCOP_WRITE;

				uscsi.sc_lad = 0;
				uscsi.sc_count = 1;

				error = sdcmd(dev, &uscsi);
			} else
				error = EIO;
		}

		/*
		 * UNLOCK sdtmp buffer
		 */
		s = splclock();
		if (sdtmp_stat & B_WANTED)
			wakeup((caddr_t)sdtmp);
		sdtmp_stat &= ~(B_BUSY|B_WANTED);
		splx(s);

		disklabel2sdst(unit, &sdlabel[unit], &sdstdrv[unit]);

		break;

	case DKIOCGGEOM:
		st = &sdstdrv[unit];
		di = (struct dkst *)data;

		di->dks_ncyl = st->ncyl;	/* # cylinders / drive */
		di->dks_ntrak = st->ntrak;	/* # tracks / cylinder */
		di->dks_nsect = st->nsect;	/* # sectors / track */
		di->dks_rps = st->rps;		/* # revolutions / second */

		break;

	case DKIOCSGEOM:
		st = &sdstdrv[unit];
		di = (struct dkst *)data;

		st->ncyl = di->dks_ncyl;	/* # cylinders / drive */
		st->ntrak = di->dks_ntrak;	/* # tracks / cylinder */
		st->nsect = di->dks_nsect;	/* # sectors / track */
		st->rps = di->dks_rps;		/* # revolutions / second */

		sdst2disklabel(unit, st, &sdlabel[unit]);
		break;

	case DKIOCGPART:
	case DKIOCSPART:
		/*
		 * partition information
		 */
		st = &sdstdrv[unit];
		pi = (struct Partinfo *)data;

		if (cmd == DKIOCGPART) {
			pi->dp_nblocks = st->sizes[dev2part(dev)].sd_nblocks;
			pi->dp_blkoff = st->sizes[dev2part(dev)].sd_blkoff;
		} else {
			st->sizes[dev2part(dev)].sd_nblocks = pi->dp_nblocks;
			st->sizes[dev2part(dev)].sd_blkoff = pi->dp_blkoff;

			sdst2disklabel(unit, st, &sdlabel[unit]);
		}
		break;

	case DKIOCGUNIT:
		*(int *)data = slave;
		break;

	case DKIOCGCHAN:
		*(int *)data = ii->ii_intr;
		break;

	case DKIOCSEEK:
		scinit(sc, slave, sdd->sdd_sectsize); 
		sc->sc_opcode = SCOP_SEEK;
		sc->sc_lad = *(int *)data;

		(void) sdcmd(dev, sc);
		tstatus = sc->sc_tstatus & TGSTMASK;
		if ((sc->sc_istatus != INST_EP) || (tstatus != TGST_GOOD))
			error = ESPIPE;
		break;

	case DKIOCRSEC0:
	case DKIOCRBOOT1:
	case DKIOCRBOOT:
		if (sdd->sdd_flags & SDDF_NONFMT) {
			error = EIO;
			break;
		}
		switch (cmd) {

		case DKIOCRSEC0:
			blkno = 0;
			count = 1;
			break;

		case DKIOCRBOOT1:
			blkno = 1;
			count = 15;
			break;

		default:
			blkno = 0;
			count = 16;
		}
		p = (char *)*(int *)data;
		for (i = 0; !error && i < count; i++) {
			s = splclock();
			while (sdtmp_stat & B_BUSY) {
				sdtmp_stat |= B_WANTED;
				sleep((caddr_t)sdtmp, PRIBIO);
			}
			sdtmp_stat |= B_BUSY;
			splx(s);
			scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize);
			uscsi.sc_cpoint = sdtmp;
			uscsi.sc_ctrnscnt = DEV_BSIZE;
			uscsi.sc_opcode = SCOP_READ;
			uscsi.sc_lad = blkno;
			uscsi.sc_count = 1;
			if (error = sdcmd(dev, &uscsi))
				goto dkior_done;
			if (error = copyout(sdtmp, p, DEV_BSIZE))
				goto dkior_done;
			blkno++;
			p += DEV_BSIZE;

dkior_done:
			s = splclock();
			if (sdtmp_stat & B_WANTED)
				wakeup((caddr_t)sdtmp);
			sdtmp_stat &= ~(B_BUSY|B_WANTED);
			splx(s);
		}
		break;

	case DKIOCWSEC0:
	case DKIOCWBOOT1:
	case DKIOCWBOOT:
		if (sdd->sdd_flags & SDDF_NONFMT) {
			error = EIO;
			break;
		}
		switch (cmd) {

		case DKIOCWSEC0:
			blkno = 0;
			count = 1;
			break;

		case DKIOCWBOOT1:
			blkno = 1;
			count = 15;
			break;

		default:
			blkno = 0;
			count = 16;
		}
		p = (char *)*(int *)data;
		for (i = 0; !error && i < count; i++) {
			s = splclock();
			while (sdtmp_stat & B_BUSY) {
				sdtmp_stat |= B_WANTED;
				sleep(sdtmp, PRIBIO);
			}
			sdtmp_stat |= B_BUSY;
			splx(s);
			if (error = copyin(p, sdtmp, DEV_BSIZE))
				goto dkiow_done;
			scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize);
			uscsi.sc_cpoint = sdtmp;
			uscsi.sc_ctrnscnt = DEV_BSIZE;
			uscsi.sc_opcode = SCOP_WRITE;
			uscsi.sc_lad = blkno;
			uscsi.sc_count = 1;
			if (error = sdcmd(dev, &uscsi))
				goto dkiow_done;
			blkno++;
			p += DEV_BSIZE;

dkiow_done:
			s = splclock();
			if (sdtmp_stat & B_WANTED)
				wakeup(sdtmp);
			sdtmp_stat &= ~(B_BUSY|B_WANTED);
			splx(s);
		}
		break;

	case SDIOC_PRVRMV:
	case SDIOC_ALWRMV:
		/*
		 * prevent/allow medium removal
		 */
		scinit(sc, ii->ii_slave, sdd->sdd_sectsize); 
		sc->sc_opcode = SCOP_MEDRMV;
		sc->sc_count = (cmd==SDIOC_PRVRMV)? SDRMV_PREV : SDRMV_ALLOW;
		if (cmd == SDIOC_PRVRMV)
			sdd->sdd_flags |= SDDF_INHRMV;
		else
			sdd->sdd_flags &= ~SDDF_INHRMV;

		error = sdcmd(dev, sc);
		break;

	case SDIOC_SXUSE:
		if (isalone(unit) != ONLY_ONE)
			return (EBUSY);
		sdd->sdd_flags |= SDDF_XUSE;
		break;

	case SDIOC_RXUSE:
		sdd->sdd_flags &= ~SDDF_XUSE;
		break;

	case SDIOC_ERSON:
		sdd->sdd_flags &= ~SDDF_ERASEOFF;
		break;

	case SDIOC_ERSOFF:
		if ((sdd->sdd_flags & SDDF_XUSE) != 0)
			return (EBUSY);
		sdd->sdd_flags |= SDDF_ERASEOFF;
		sdd->sdd_flags &= ~SDDF_NONFMT;
		break;

	case SDIOC_FORMAT:
		/*
		 * format unit
		 */
		if ((flag & FWRITE) == 0)
			return (EINVAL);
		if (isalone(unit) != ONLY_ONE)
			return (EBUSY);
		sdd->sdd_flags |= SDDF_XUSE;
		sdd->sdd_flags &= ~(SDDF_NONFMT|SDDF_FMTDONE|SDDF_SAMEDSK);
		scinit(sc, ii->ii_slave, sdd->sdd_sectsize); 
		sc->sc_ctrnscnt = 4;
		sc->sc_opcode = SCOP_FMT;

		sc->sc_lad = ((sddevinfo[ii->ii_type].fmt_opts & FMT_DLFMT)
				| SDF_FMTDAT) << 16;

		switch (sddevinfo[ii->ii_type].type) {

		case SMO_S501:
		case SMO_S501_ISO:
		case SMO_S501_ISO2:
			sc->sc_lad |= ((SDF_MKCDA|SDF_MKPLST) << 8);
			break;

		default:
			break;
		}

		{
			struct fmt_data *fdata = (struct fmt_data *)data;

			error = copyin((caddr_t)fdata->dlh, sc->sc_param, 4);
			if (error != 0) {
				sdd->sdd_flags &= ~SDDF_XUSE;
				break;
			}
			if (fdata->noglist)
				sc->sc_lad |= (SDF_CMPLST<<16);
		}

		if (sdd->sdd_flags & SDDF_ERASEOFF)
			sc->sc_ctrl = 0x40;

		error = sdcmd(dev, sc);
		sdd->sdd_flags &= ~SDDF_XUSE;
		break;

	case SDIOC_FORMAT2:
		/*
		 * format unit
		 */
		if ((flag & FWRITE) == 0)
			return (EINVAL);
		if (isalone(unit) != ONLY_ONE)
			return (EBUSY);
		sdd->sdd_flags |= SDDF_XUSE;
		sdd->sdd_flags &= ~(SDDF_NONFMT|SDDF_FMTDONE|SDDF_SAMEDSK);

		scu = (struct sc_ureq *)data;
		error = sd_scu_exec(dev, scu, sc);
		sdd->sdd_flags &= ~SDDF_XUSE;
		break;

	case SDIOC_GSTOPT:
	case SDIOC_SSTOPT:
		/*
		 * get/set stop-unit timer
		 */
		if (cmd == SDIOC_GSTOPT)
			*(int *)data = sdd->sdd_stoptime;
		else {
			if (*(int *)data == 0)
				return (EINVAL);
			sdd->sdd_stoptime = *(int *)data;
		}
		break;

	case SDIOC_SEJECT:
		/*
		 * set auto eject flag
		 */
		sdd->sdd_flags |= SDDF_REQ_EJECT;
		break;

	case SDIOC_GFLAGS:
		/*
		 * get ii->ii_flags
		 */
		*(int *)data = ii->ii_flags;
		break;

	case SDIOC_SFLAGS:
		/*
		 * set ii->ii_flags
		 */
		ii->ii_flags = *(int *)data;
		break;

	case SDIOC_RASBLK:
		/*
		 * reassign block
		 */
		{
			struct sc_rab *sca = (struct sc_rab *)sc->sc_param;

			scinit(sc, ii->ii_slave, sdd->sdd_sectsize); 
			sc->sc_opcode = SCOP_RASBLK;
			sc->sc_ctrnscnt = 8;

			sca->sca_dllen = 4;
			sca->sca_dlad[0] = *(int *)data;

			error = sdcmd(dev, sc);
		}
		break;

	case SDIOC_GNICKNAME:
		{
			int len;

			len = strlen(sddevinfo[ii->ii_type].call_name);

			if (len > IOCPARM_MASK)
				len = IOCPARM_MASK;

			error = copyout(
				(caddr_t) sddevinfo[ii->ii_type].call_name,
				(caddr_t) *(int *)data,
				len);
		}
		break;

	case SDIOC_GTYPINDEX:
		*(int *)data = (int)ii->ii_type;
		break;

#ifdef SDIOC_SSYNCPARAM
	case SDIOC_SSYNCPARAM:
		{
		struct sync_param *syncp;

		syncp = (struct sync_param *)data;
		scinit(sc, ii->ii_slave, sdd->sdd_sectsize); 
		sc->sc_opcode = SCOP_TST;
		sc->sc_message = MSG_EXTND;	/* extended message */
		sc->sc_param[0] = MSG_EXTND;
		sc->sc_param[1] = 0x03;
		sc->sc_param[2] = 0x01;		/* synchronous transfer */
		sc->sc_param[3] = syncp->tr_period;	/* transfer period */
		sc->sc_param[4] = syncp->tr_offset;	/* REQ offset */

		(void) sdcmd(dev, sc);
		
		syncp = &sd_sync_param[unit];
		syncp->tr_period = sc->sc_param[3];
		syncp->tr_offset = sc->sc_param[4];

		if (syncp->tr_offset)
			sdd->sdd_flags |= SDDF_SYNCTR;
		else
			sdd->sdd_flags &= ~SDDF_SYNCTR;
		}
		break;

	case SDIOC_GSYNCPARAM:
		{
		struct sync_param *syncp = (struct sync_param *)data;

		syncp->tr_period = sd_sync_param[unit].tr_period;
		syncp->tr_offset = sd_sync_param[unit].tr_offset;
		}
		break;
#endif /* SDIOC_SSYNCPARAM */

	case MTIOCTOP:
		{
			register struct mtop *mtop = (struct mtop *)data;
			register int lba;
			int rest;
			int blength;

			switch (mtop->mt_op) {

			case MTOFFL:
				/*
				 * set auto eject flag
				 */
				sdd->sdd_flags |= SDDF_REQ_EJECT;
				break;

#ifdef MTERASE
			case MTERASE:
				if (isalone(unit) != ONLY_ONE)
					return (EBUSY);
				sdd->sdd_flags |= SDDF_XUSE;
				st = &sdstdrv[unit];
				/*
				 * MO disk erase
				 *	block 0 to end (C partition)
				 */
				lba = 0;
				rest = sdd->sdd_nsect;	/* C part size */
				while (rest > 0) {
					blength = (rest > MAXBL)? MAXBL : rest;
					scinit(sc, ii->ii_slave,
							sdd->sdd_sectsize); 
					sc->sc_opcode = SCOP_MOERASE;
					sc->sc_lad = lba;
					sc->sc_count = (blength % MAXBL);

					(void) sdcmd(dev, sc);
					lba += blength;
					rest -= blength;
				}
				sdd->sdd_flags &= ~SDDF_XUSE;
				break;
#endif /* MTERASE */

			default:
				return (EINVAL);
			}
		}
		break;

	case SCSIIOCCMD:
		scu = (struct sc_ureq *)data;
		if ((scu->scu_count > 0) && scu->scu_addr) {
			if (useracc(scu->scu_addr, scu->scu_count, B_WRITE)
			    == NULL) {
				error = EFAULT;
				break;
			}
		}
		error = sd_scu_exec(dev, scu, sc);
		break;


	case SDIOC_INQUIRY:
		/*
		 * LOCK sdtmp buffer
		 */
		s = splclock();
		while (sdtmp_stat & B_BUSY) {
			sdtmp_stat |= B_WANTED;
			sleep((caddr_t)sdtmp, PRIBIO);
		}
		sdtmp_stat |= B_BUSY;
		splx(s);

		bzero((caddr_t)sdtmp, sizeof(struct sc_inq));
		scinit(&uscsi, ii->ii_slave, sdd->sdd_sectsize); 
		uscsi.sc_cpoint = sdtmp;
		uscsi.sc_ctrnscnt = sizeof(struct sc_inq);
		uscsi.sc_opcode = SCOP_INQUIRY;
		uscsi.sc_count = sizeof(struct sc_inq);

		if ((error = sdcmd(dev, &uscsi)) == 0)
			bcopy((caddr_t)sdtmp, data, sizeof(struct sc_inq));
		/*
		 * UNLOCK open
		 */
		s = splclock();
		if (sdtmp_stat & B_WANTED)
			wakeup((caddr_t)sdtmp);
		sdtmp_stat &= ~(B_BUSY|B_WANTED);
		splx(s);
		break;


	case SCSIIOCGTIMEO:
		*(int *)data = sdc->sdc_timeo;
		break;

	case SCSIIOCSTIMEO:
		if (*(int *)data == 0)
			return (EINVAL);
		sdc->sdc_timeo = *(int *)data;
		sdc->sdc_wticks = 0;
		break;

	default:
		error = EINVAL;
		break;
	}

done:
	return (error);
}

static
sd_scu_exec(dev, scu, sc)
	dev_t dev;
	register struct sc_ureq *scu;
	register struct scsi *sc;
{
	struct sdd_softc *sdd;
	int error;

	sdd = &sdd_softc[dev2unit(dev)];

	if (((scu->scu_identify & MSG_IDENT) == 0)
		|| (scu->scu_identify & ~(MSG_IDENT|IDT_DISCON|IDT_DRMASK))
		|| (scu->scu_addr && (scu->scu_bytesec == 0))) {
		return (EINVAL);
	}

	bzero((caddr_t)sc, sizeof(struct scsi));
	sc->sc_tstatus = scu->scu_tstatus;
	sc->sc_identify = scu->scu_identify;
	sc->sc_message = scu->scu_message;
	sc->sc_bytesec = scu->scu_bytesec;
	sc->sc_cpoint = scu->scu_addr;
	sc->sc_ctrnscnt = scu->scu_count;
	bcopy((caddr_t)scu->scu_cdb, &sc->sc_cdb, sizeof(sc->sc_cdb));

	bcopy((caddr_t)scu->scu_param, (caddr_t)sc->sc_param,
						sizeof(sc->sc_param));

	sdd->sdd_flags |= SDDF_SKIPCHECK;
	error = sdcmd(dev, sc);
	sdd->sdd_flags &= ~SDDF_SKIPCHECK;

	scu->scu_istatus = sc->sc_istatus;
	scu->scu_tstatus = sc->sc_tstatus;
	scu->scu_message = sc->sc_message;
	bcopy((caddr_t)sc->sc_param, (caddr_t)scu->scu_param,
		sizeof(sc->sc_param));
	return (error);
}

/*ARGSUSED*/
sddump(dev)
	dev_t dev;
{
	return (ENXIO);
}

sdsize(dev)
	register dev_t dev;
{
	register struct iop/**/_device *ii;
	register struct sdd_softc *sdd;
	register struct sdst *st;
	register int unit;
	int i;

	unit = dev2unit(dev);
	if (unit >= nsd || (ii = sddinfo[unit]) == 0 || ii->ii_alive == 0)
		return (-1);

	sdd = &sdd_softc[unit];
	switch (sdc_softc[ii->ii_ctlr].sdc_firmware & SDCFW_DEVMASK) {

	case SDCFW_HD:		/* Hard Disk */
		st = &sdstdrv[unit];
		break;

	case SDCFW_MO:		/* MO only */
		if ((sdd->sdd_flags & SDDF_SAMEDSK) == SDDF_DSKCHGD) {
			/*
			 * read partition information,
			 *	and set up sdstdrv[unit]
			 */
			if (sd_b_open(dev, FREAD|FWRITE) != 0) {
				/*
				 * block device open error 
				 */
				return (-1);
			} else {
				/*
				 * use disk partition information
				 */
				st = &sdstdrv[unit];
				if (isalone(unit) == ONLY_ONE)
					sd_b_close(dev, 0);
			}
		} else if (sdd->sdd_flags & SDDF_NONFMT) {
			/*
			 * medium is not initialized.
			 */
			return (-1);
		} else {
			st = &sdstdrv[unit];
		}
		break;

	default:
    /*  case SDCFW_CD: */
		return (-1);

	}

	if (st->sizes == NULL)
		return (-1);					/* XXX */
	else
		return (st->sizes[dev2part(dev)].sd_nblocks);	/* XXX */
}

/*
 * Reset driver.
 * Cancel software state of all pending transfers,
 * and restart all units and the controller.
 */
sdreset()
{
	register struct iop/**/_ctlr *im;
	register struct iop/**/_device *ii;
	register struct sdc_softc *sdc;
	register struct sdd_softc *sdd;
	register int i;
	register int unit;

	re_init_done = 1;
	for (i = 0; i < nsdc; i++) {
		im = sdminfo[i];
		if (im == 0)
			continue;
		if (im->im_alive == 0)
			continue;
		printf(" sdc%d: ", i);
		sdc = &sdc_softc[i];
		sdc->sdc_wticks = 0;

		/* scop_init() is already called by screset() */

		sdtmp_stat &= ~B_BUSY;

		for (unit = 0; unit < nsd; unit++) {
			ii = sddinfo[unit];
			if (ii == 0)
				continue;
			if (ii->ii_alive == 0)
				continue;
			if (ii->ii_mi != im)
				continue;

			csdbuf[unit].b_flags &= ~B_BUSY;

			sdd = &sdd_softc[unit];
			sdd->sdd_flags = 0;

			/*
			 * UNLOCK SCSI access
			 */
			sdc->sdc_firmware &= ~SDCFW_BUSY;

			if (sdslave(ii, ii->ii_addr, im->im_intr) == 0) {
				printf("sd%d: not ready\n", ii->ii_slave);
				continue;
			}
			sdattach(ii);
		}
	}
	re_init_done = 2;
}

int sd_long_timeout = 24 * 60 * 60;	/* 24 hours */

#define max(a, b) (((a)>(b))?(a):(b))

/*
 * Wake up every second and if interrupt is pending
 * but nothing has happened increment a counter.
 * If nothing happens for sdc_timeo seconds, reset the IOP
 * and begin anew.
 */
sdwatch()
{
	register struct iop/**/_ctlr *im;
	register struct sdc_softc *sdc;
	register int i;
	register int unit;
	int timeo;

	extern int Scsi_Disconnect;

	timeout(sdwatch, (caddr_t)0, hz);
	for (i = 0; i < nsdc; i++) {
		im = sdminfo[i];
		if (im == 0)
			continue;
		if (im->im_alive == 0)
			continue;
		sdc = &sdc_softc[i];

		if (im->im_tab.b_active)
			goto active;

		for (unit = 0; unit < nsd; unit++)
			if (sdutab[unit].b_active && sddinfo[unit]->ii_mi == im)
				goto active;

		sdc->sdc_wticks = 0;
		continue;
active:
		if (Scsi_Disconnect)
			timeo = sdc->sdc_timeo;
		else
			timeo = max(sdc->sdc_timeo, sd_long_timeout);

		if (sdc->sdc_wticks++ >= timeo) {
			register struct scsi *sc;

			sc = get_scsi(im->im_intr);
			sdc->sdc_wticks = 0;
			printf("sdc%d: lost interrupt\n", i);

			screset(im->im_intr);
		}
	}
}

/*
 * sdstop() is timer interrupt routine.
 *	So, can't use sleep().
 */
sdstop()
{
	register struct iop/**/_ctlr *im;
	register struct iop/**/_device *ii;
	register struct sdc_softc *sdc;
	register struct sdd_softc *sdd;
	register int unit;
	register int intr;
	register int i;
	struct scsi *sc;
	int eject_sw;

	timeout(sdstop, (caddr_t)0, hz);

	for (i = 0; i < nsdc; i++) {
		im = sdminfo[i];
		if (im == 0)
			continue;
		if (im->im_alive == 0)
			continue;
		for (unit = 0; unit < nsd; unit++) {
			if ((ii = sddinfo[unit]) == 0)
				continue;
			if (ii->ii_mi != im)
				continue;
			sdc = &sdc_softc[ii->ii_ctlr];
			if ((sdc->sdc_firmware & SDCFW_RMB) == 0)
				continue;
			intr = ii->ii_intr;
			if (isalone(unit))
				continue;
			/**********************/
			/*    MO & CD-ROM     */
			/**********************/
			/*
			 * there is no process which open the unit.
			 */
			sdd = &sdd_softc[unit];
			sc = get_scsi(intr);
			if (sdd->sdd_start > 0)
				sdd->sdd_start--;
			else if (sdd->sdd_start == 0) {
				/*
				 * Now stop the unit.
				 * check SCSI access
				 */
				if (sdc->sdc_firmware & SDCFW_BUSY)
					continue;
				sdc->sdc_firmware |= SDCFW_BUSY;
				sdc->sdc_state |= SDCS_IOCTL|SDCS_SCUNLOCK;

				eject_sw = (sdd->sdd_flags & SDDF_REQ_EJECT) ?
						SDSS_EJECT : SDSS_STOP;
				scop_stst(intr, sc, ii->ii_slave,
						SCSI_INTEN, eject_sw);
				sdd->sdd_start = -2;
			}
		}
	}
}

isalone(unit)
	register int unit;
{
	register int i, n;

	n = 0;
	for (i = 0; i < PNUM; i++)
		n += (sd_b_openf[unit][i] + sd_c_openf[unit][i]);
	return (n);
}

/************************************************
 * Convert Hex and RS code table definition	*
 ************************************************/

#define	X8_L	0x001d
#define	X8_H	0x1d00


#define	hextors(data)	hxtable[(int)((data) & 0xff)]
#define	XORMASK(code)	xortable[(unsigned int)(code)]

int	hxtable[256] = {
	0x00, 0x00, 0x01, 0x19, 0x02, 0x32, 0x1a, 0xc6, 
	0x03, 0xdf, 0x33, 0xee, 0x1b, 0x68, 0xc7, 0x4b, 
	0x04, 0x64, 0xe0, 0x0e, 0x34, 0x8d, 0xef, 0x81, 
	0x1c, 0xc1, 0x69, 0xf8, 0xc8, 0x08, 0x4c, 0x71, 
	0x05, 0x8a, 0x65, 0x2f, 0xe1, 0x24, 0x0f, 0x21, 
	0x35, 0x93, 0x8e, 0xda, 0xf0, 0x12, 0x82, 0x45, 
	0x1d, 0xb5, 0xc2, 0x7d, 0x6a, 0x27, 0xf9, 0xb9, 
	0xc9, 0x9a, 0x09, 0x78, 0x4d, 0xe4, 0x72, 0xa6, 
	0x06, 0xbf, 0x8b, 0x62, 0x66, 0xdd, 0x30, 0xfd, 
	0xe2, 0x98, 0x25, 0xb3, 0x10, 0x91, 0x22, 0x88, 
	0x36, 0xd0, 0x94, 0xce, 0x8f, 0x96, 0xdb, 0xbd, 
	0xf1, 0xd2, 0x13, 0x5c, 0x83, 0x38, 0x46, 0x40, 
	0x1e, 0x42, 0xb6, 0xa3, 0xc3, 0x48, 0x7e, 0x6e, 
	0x6b, 0x3a, 0x28, 0x54, 0xfa, 0x85, 0xba, 0x3d, 
	0xca, 0x5e, 0x9b, 0x9f, 0x0a, 0x15, 0x79, 0x2b, 
	0x4e, 0xd4, 0xe5, 0xac, 0x73, 0xf3, 0xa7, 0x57, 
	0x07, 0x70, 0xc0, 0xf7, 0x8c, 0x80, 0x63, 0x0d, 
	0x67, 0x4a, 0xde, 0xed, 0x31, 0xc5, 0xfe, 0x18, 
	0xe3, 0xa5, 0x99, 0x77, 0x26, 0xb8, 0xb4, 0x7c, 
	0x11, 0x44, 0x92, 0xd9, 0x23, 0x20, 0x89, 0x2e, 
	0x37, 0x3f, 0xd1, 0x5b, 0x95, 0xbc, 0xcf, 0xcd, 
	0x90, 0x87, 0x97, 0xb2, 0xdc, 0xfc, 0xbe, 0x61, 
	0xf2, 0x56, 0xd3, 0xab, 0x14, 0x2a, 0x5d, 0x9e, 
	0x84, 0x3c, 0x39, 0x53, 0x47, 0x6d, 0x41, 0xa2, 
	0x1f, 0x2d, 0x43, 0xd8, 0xb7, 0x7b, 0xa4, 0x76, 
	0xc4, 0x17, 0x49, 0xec, 0x7f, 0x0c, 0x6f, 0xf6, 
	0x6c, 0xa1, 0x3b, 0x52, 0x29, 0x9d, 0x55, 0xaa, 
	0xfb, 0x60, 0x86, 0xb1, 0xbb, 0xcc, 0x3e, 0x5a, 
	0xcb, 0x59, 0x5f, 0xb0, 0x9c, 0xa9, 0xa0, 0x51, 
	0x0b, 0xf5, 0x16, 0xeb, 0x7a, 0x75, 0x2c, 0xd7, 
	0x4f, 0xae, 0xd5, 0xe9, 0xe6, 0xe7, 0xad, 0xe8, 
	0x74, 0xd6, 0xf4, 0xea, 0xa8, 0x50, 0x58, 0xaf, 
};

int xortable[256] = {
	0x00000000, 0x90910101, 0x91210201, 0x01b00300,
	0x92410401, 0x02d00500, 0x03600600, 0x93f10701,
	0x94810801, 0x04100900, 0x05a00a00, 0x95310b01,
	0x06c00c00, 0x96510d01, 0x97e10e01, 0x07700f00,
	0x99011001, 0x09901100, 0x08201200, 0x98b11301,
	0x0b401400, 0x9bd11501, 0x9a611601, 0x0af01700,
	0x0d801800, 0x9d111901, 0x9ca11a01, 0x0c301b00,
	0x9fc11c01, 0x0f501d00, 0x0ee01e00, 0x9e711f01,
	0x82012001, 0x12902100, 0x13202200, 0x83b12301,
	0x10402400, 0x80d12501, 0x81612601, 0x11f02700,
	0x16802800, 0x86112901, 0x87a12a01, 0x17302b00,
	0x84c12c01, 0x14502d00, 0x15e02e00, 0x85712f01,
	0x1b003000, 0x8b913101, 0x8a213201, 0x1ab03300,
	0x89413401, 0x19d03500, 0x18603600, 0x88f13701,
	0x8f813801, 0x1f103900, 0x1ea03a00, 0x8e313b01,
	0x1dc03c00, 0x8d513d01, 0x8ce13e01, 0x1c703f00,
	0xb4014001, 0x24904100, 0x25204200, 0xb5b14301,
	0x26404400, 0xb6d14501, 0xb7614601, 0x27f04700,
	0x20804800, 0xb0114901, 0xb1a14a01, 0x21304b00,
	0xb2c14c01, 0x22504d00, 0x23e04e00, 0xb3714f01,
	0x2d005000, 0xbd915101, 0xbc215201, 0x2cb05300,
	0xbf415401, 0x2fd05500, 0x2e605600, 0xbef15701,
	0xb9815801, 0x29105900, 0x28a05a00, 0xb8315b01,
	0x2bc05c00, 0xbb515d01, 0xbae15e01, 0x2a705f00,
	0x36006000, 0xa6916101, 0xa7216201, 0x37b06300,
	0xa4416401, 0x34d06500, 0x35606600, 0xa5f16701,
	0xa2816801, 0x32106900, 0x33a06a00, 0xa3316b01,
	0x30c06c00, 0xa0516d01, 0xa1e16e01, 0x31706f00,
	0xaf017001, 0x3f907100, 0x3e207200, 0xaeb17301,
	0x3d407400, 0xadd17501, 0xac617601, 0x3cf07700,
	0x3b807800, 0xab117901, 0xaaa17a01, 0x3a307b00,
	0xa9c17c01, 0x39507d00, 0x38e07e00, 0xa8717f01,
	0xd8018001, 0x48908100, 0x49208200, 0xd9b18301,
	0x4a408400, 0xdad18501, 0xdb618601, 0x4bf08700,
	0x4c808800, 0xdc118901, 0xdda18a01, 0x4d308b00,
	0xdec18c01, 0x4e508d00, 0x4fe08e00, 0xdf718f01,
	0x41009000, 0xd1919101, 0xd0219201, 0x40b09300,
	0xd3419401, 0x43d09500, 0x42609600, 0xd2f19701,
	0xd5819801, 0x45109900, 0x44a09a00, 0xd4319b01,
	0x47c09c00, 0xd7519d01, 0xd6e19e01, 0x46709f00,
	0x5a00a000, 0xca91a101, 0xcb21a201, 0x5bb0a300,
	0xc841a401, 0x58d0a500, 0x5960a600, 0xc9f1a701,
	0xce81a801, 0x5e10a900, 0x5fa0aa00, 0xcf31ab01,
	0x5cc0ac00, 0xcc51ad01, 0xcde1ae01, 0x5d70af00,
	0xc301b001, 0x5390b100, 0x5220b200, 0xc2b1b301,
	0x5140b400, 0xc1d1b501, 0xc061b601, 0x50f0b700,
	0x5780b800, 0xc711b901, 0xc6a1ba01, 0x5630bb00,
	0xc5c1bc01, 0x5550bd00, 0x54e0be00, 0xc471bf01,
	0x6c00c000, 0xfc91c101, 0xfd21c201, 0x6db0c300,
	0xfe41c401, 0x6ed0c500, 0x6f60c600, 0xfff1c701,
	0xf881c801, 0x6810c900, 0x69a0ca00, 0xf931cb01,
	0x6ac0cc00, 0xfa51cd01, 0xfbe1ce01, 0x6b70cf00,
	0xf501d001, 0x6590d100, 0x6420d200, 0xf4b1d301,
	0x6740d400, 0xf7d1d501, 0xf661d601, 0x66f0d700,
	0x6180d800, 0xf111d901, 0xf0a1da01, 0x6030db00,
	0xf3c1dc01, 0x6350dd00, 0x62e0de00, 0xf271df01,
	0xee01e001, 0x7e90e100, 0x7f20e200, 0xefb1e301,
	0x7c40e400, 0xecd1e501, 0xed61e601, 0x7df0e700,
	0x7a80e800, 0xea11e901, 0xeba1ea01, 0x7b30eb00,
	0xe8c1ec01, 0x7850ed00, 0x79e0ee00, 0xe971ef01,
	0x7700f000, 0xe791f101, 0xe621f201, 0x76b0f300,
	0xe541f401, 0x75d0f500, 0x7460f600, 0xe4f1f701,
	0xe381f801, 0x7310f900, 0x72a0fa00, 0xe231fb01,
	0x71c0fc00, 0xe151fd01, 0xe0e1fe01, 0x7070ff00
};

/********************************************************
 * EDC (Error Detection Code) check			*
 *			(using CRC code)		*
 *	MODE 1 : Sync + Header + User data + edc	*
 *	MODE 2 : Sub header + User data + edc		*
 ********************************************************/

#define	SYNC_EDC	0x908eff4e	/* sector sync EDC value	*/
#define	HEADER		4		/* header length 4 bytes	*/
#define	SUB_H		8		/* Subheader length 8 bytes	*/
#define	USER_DATA	2048		/* User data length 2048 bytes	*/
#define	EDC_LEN		4		/* EDC code length 4 bytes	*/

cal_edc1(db, mode)
	register unsigned char	*db;
	int mode;
{
	register int j;
	register unsigned int work;

	j = USER_DATA + EDC_LEN;
	if (mode == 2) {
		j += SUB_H;
		work = 0;
	} else {
		j += HEADER;
		work = SYNC_EDC;
	}
	while (--j >= 0)
		work = ((work >> 8) | (*db++ << 24)) ^ XORMASK(work & 0xff);

	return (work);
}


/************************************************/
/*	error detection & correction		*/
/*	if form 1				*/
/*	    header area should be zero		*/
/************************************************/

/*
 *	error detection & correction  P-direction
 */
p_dir(dbuf)
	register unsigned short	*dbuf;
{
	unsigned short	s0, s1, d;
	register int	col, row;
	register int	x;
	unsigned char	s0l, s0h, s1l, s1h;
 
	/*
	 * calculate syndrome S0 S1
	 */
	for (col = 0; col < 43; col++) {
		s0 = s1 = 0;

		for (row = 0; row < 26; row++) {
			d = dbuf[43 * row + col];
			s0 ^= d;
			s1 = rsshift(s1) ^ d; 
		}

		s0h = s0 & 0x00ff;
		s1h = s1 & 0x00ff;
		s0l = (s0 >> 8) & 0x00ff;
		s1l = (s1 >> 8) & 0x00ff;
		/*
		 * calculate error position & correction
		 */
		if (s0l != 0) {
			if((x = hextors(s1l) - hextors(s0l)) < 0)
				x += 255;
			if ((x >= 0) && (x < 26)) {
				x = 25 - x;
				/*
				 * correction
				 */
				dbuf[43 * x + col] ^= (s0 & 0xff00);
			}
		}

		/*
		 * calculate error position & correction
		 */
		if (s0h != 0) {
			if((x = hextors(s1h) - hextors(s0h)) < 0)
				x += 255;
			if ((x >= 0) && (x < 26)) {
				x = 25 - x;
				/*
				 * correction
				 */
				dbuf[43 * x + col] ^= (s0 & 0x00ff);
			}
		}
	}
}

/*
 * error detection & correction  Q-direction
 */
q_dir(dbuf)
	register unsigned short	*dbuf;
{
	unsigned short s0, s1, d;
	register int col, row;
	register int x;
	unsigned char s0l, s0h, s1l, s1h;

	/*
	 * calculate syndrome S0 S1
	 */
	for (row = 0; row < 26; row++) {
		s0 = s1 = 0;

		for (col = 0; col < 45; col++) {
			if (col < 43)
				d = dbuf[(44 * col + 43 * row) % 1118];
			else if (col == 43)
				d = dbuf[43 * 26 + row];
			else
				d = dbuf[44 * 26 + row];
			s0 ^= d;
			s1 = rsshift(s1) ^ d; 
		}

		s0h = s0 & 0x00ff;
		s1h = s1 & 0x00ff;
		s0l = (s0 >> 8) & 0x00ff;
		s1l = (s1 >> 8) & 0x00ff;
		/*
		 * calculate error position & correction
		 */
		if (s0l != 0) {
			if((x = hextors(s1l) - hextors(s0l)) < 0)
				x += 255;
			if (x >= 0 && x < 45) {
				x = 44 - x;
				/*
				 * correction
				 */
				if (x < 43)
					dbuf[(44 * x + 43 * row) % 1118]
						^= s0 & 0xff00;
				else if (x == 43)
					dbuf[43 * 26 + row] ^= s0 & 0xff00;
				else
					dbuf[44 * 26 + row] ^= s0 & 0xff00;
			}
		}

		/*
		 * calculate error position & correction
		 */
		if (s0h != 0) {
			if((x = hextors(s1h) - hextors(s0h)) < 0)
				x += 255;
			if ((x >= 0) && (x < 45)) {
				x = 44 - x;
				/*
				 * correction
				 */
				if (x < 43)
					dbuf[(44 * x + 43 * row) % 1118]
						^= s0 & 0x00ff;
				else if ( x == 43)
					dbuf[43 * 26 + row] ^= s0 & 0x00ff;
				else
					dbuf[44 * 26 + row] ^= s0 & 0x00ff;
			}
		}
	}
}

/*
 *	shift high & low byte at the same time
 */
rsshift(d)
	unsigned short d;
{
	register int x;
	register int dmy;	/* This way is faster */

	dmy = (int)d;
	x = (dmy << 1) & 0xfefe;	/* clear LSB of high & low byte */
	if ((dmy & 0x0080) != 0)
		x ^= X8_L;
	if ((dmy & 0x8000) != 0)
		x ^= X8_H;
	return(x);
}
#endif /* NSD > 0 */
