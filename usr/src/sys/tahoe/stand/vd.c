/*	vd.c	7.4	86/11/04	*/

/*
 * Stand alone driver for the VDDC controller 
 *	TAHOE Version, Oct 1983.
 */
#include "../machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#define VDGENDATA 1
#include "../tahoevba/vdreg.h"
#undef	VDGENDATA
#include "../tahoevba/vbaparam.h"
#include "saio.h"

#define NVD		4			/* Max number of controllers */
#define VDUNIT(x)	(minor(x) & 0x3)
#define VDCTLR(x)	(minor(x) >> 2)

fmt_mdcb	mdcb;
fmt_dcb		dcb;

/*
 * Drive specific information.
 */
struct dkinfo {
	char	configured;
	fs_tab	info;
} dkinfo[NVD][16];

/*
 * Controller specific information.
 */
struct vdinfo {
	u_short	vd_flags;
#define	VDF_INIT	0x1		/* controller initialized */
#define	VDF_BUSY	0x2		/* controller running */
	u_short	vd_type;		/* smd or smde */
	char	*vd_name;
} vdinfo[NVD];

static char	junk[1024];

#define	VDADDR(ctlr)	((cdr *)(vddcaddr[(ctlr)]+VBIOBASE))

vdopen(io)
	register struct iob *io;
{
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	struct vdinfo *vd;
	register int i, j;

	/* Make sure controller number is in range */
	if (ctlr >= NVD) {
		printf(
		    "dk%d: invalid controller (only configured for %d vd's)\n",
		    io->i_unit, NVD);
		_stop("");
	}
	/* check file system for validity */
	if ((unsigned)io->i_boff > 8) {
		printf("dk%d: invalid partition number (%d)\n",
		    io->i_unit, io->i_boff);
		_stop("");
	}
	vd = &vdinfo[ctlr];
	if ((vd->vd_flags&VDF_INIT) == 0) {
		vdinit(io);	/* initialize controller/drive */
		vd->vd_flags |= VDF_INIT;
		for (j = 0; j < 16; j++)
			dkinfo[ctlr][j].configured = 0;
	}
	if (!dkinfo[ctlr][unit].configured) {
		vdconfigure_drive(io);
		dkinfo[ctlr][unit].configured = 1;
	}
	io->i_boff = dkinfo[ctlr][unit].info.partition[io->i_boff].par_start;
}

vdinit(io)
	register struct iob *io;
{
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	register cdr *ctlr_addr = VDADDR(ctlr);
	register struct vdinfo *vd = &vdinfo[ctlr];

	/* Check to see if controller is really there */
	if (badaddr(ctlr_addr, 2)) {
		printf("dk%d: vd%d csr doesn't respond\n", io->i_unit, ctlr);
		_stop("");
	}
	/* Probe further to find what kind of controller it is */
	ctlr_addr->cdr_reset = 0xffffffff;
	DELAY(1000000);
	if (ctlr_addr->cdr_reset != 0xffffffff) {
		vd->vd_type = SMDCTLR;
		vd->vd_name = "smd";
		DELAY(1000000);
	} else {
		vd->vd_type = SMD_ECTLR;
		vd->vd_name = "smde";
		ctlr_addr->cdr_reserved = 0x0;
		DELAY(3000000);
	}
	if (vd->vd_type == SMD_ECTLR) {
		ctlr_addr->cdr_csr =  0;
		ctlr_addr->mdcb_tcf = AM_ENPDA;
		ctlr_addr->dcb_tcf = AM_ENPDA;
		ctlr_addr->trail_tcf = AM_ENPDA;
		ctlr_addr->data_tcf = AM_ENPDA;
		ctlr_addr->cdr_ccf = CCF_STS | XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE /* | CCF_EDE */ | CCF_ECE | CCF_ERR;
	}
	if (vdaccess_with_no_trailer(io, INIT, 8) & HRDERR) {
		vdprint_error(io->i_unit, "init error",
		    dcb.operrsta,dcb.err_code);
		_stop("");
	}
	if (vdaccess_with_no_trailer(io, DIAG, 8) & HRDERR) {
		vdprint_error(io->i_unit, "diagnostic error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
}

vdconfigure_drive(io)
	register struct iob *io;
{
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	register fs_tab	*file_sys;
	register struct dkinfo *dk = &dkinfo[ctlr][unit];
	dskadr daddr;
	register int i;

	for (i = 0; i < nvddrv; i++) {
		dk->info = vdst[i];
		if (vdinfo[ctlr].vd_type == SMDCTLR)
			if (dk->info.nsec != 32)
				continue;
		vdconfigure(io, 0);
		daddr.cylinder = dk->info.ncyl - 2;
		daddr.track = dk->info.ntrak - 1;
		daddr.sector = dk->info.nsec - 1;
		io->i_ma = junk;
		io->i_cc = dk->info.secsize;
		if ((vdaccess(io, &daddr, RD) & HRDERR) == 0)
			return;
	}
	printf("dk%d: unknown drive type\n", io->i_unit);
	_stop("");
}

vdstart_drive(io)
	register struct iob *io;
{
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	int ounit = io->i_unit;

	if (vdinfo[ctlr].vd_flags&VDF_BUSY) {
		DELAY(5500000);
		return;
	}
	io->i_unit &= ~3;
	if (vdaccess_with_no_trailer(io, VDSTART, ((unit * 6) + 62)) & HRDERR) {
		vdprint_error(io->i_unit, "start error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
	vdinfo[ctlr].vd_flags |= VDF_BUSY;
	io->i_unit = ounit;
	DELAY(62000000);
}

/*
 * This routine actually configures a particular drive.
 *
 * If the controller is an SMD/E controller then the number of sectors per
 * track is loaded into the appropriate register, otherwise it is left 
 * alone because the old SMD controller requires a constant 32 sectors
 * per track for it's drives. (an error would be returned if the value is
 * loaded.)
 */
vdconfigure(io, pass)
	register struct iob *io;
	int pass;
{
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	register cdr *ctlr_addr = VDADDR(ctlr);

	dcb.opcode = RSTCFG;		/* command */
	dcb.intflg = NOINT;
	dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)unit;
	dcb.trail.rstrail.ncyl = dkinfo[ctlr][unit].info.ncyl;
	dcb.trail.rstrail.nsurfaces = dkinfo[ctlr][unit].info.ntrak;
	if (vdinfo[ctlr].vd_type == SMD_ECTLR) {
		dcb.trailcnt = (char)5;
		dcb.trail.rstrail.nsectors = dkinfo[ctlr][unit].info.nsec;
		dcb.trail.rstrail.slip_sec = dkinfo[ctlr][unit].info.nslip;
		dcb.trail.rstrail.recovery = 0x18f;
	} else
		dcb.trailcnt = (char)2;
	mdcb.firstdcb = &dcb;
	mdcb.vddcstat = 0;
	VDDC_ATTENTION(ctlr_addr, &mdcb, vdinfo[ctlr].vd_type);
	if (!vdpoll(ctlr_addr,&dcb,10,vdinfo[ctlr].vd_type)) {
		if (pass == 0) {
			VDDC_RESET(ctlr_addr, vdinfo[ctlr].vd_type);
			vdconfigure(io, 1);
		} else
			_stop(" during drive configuration.\n");
	}
	if ((dcb.operrsta & (NOTCYLERR | DRVNRDY)) && !pass) {
		vdstart_drive(io);
		vdconfigure(io, 1);
	}
	if (dcb.operrsta & HRDERR) {
		vdprint_error(io->i_unit, "configuration error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
}

/*
 * Strategy is called to do the actual I/O to the disk drives.
 *
 * Some simple checks are made to make sure we don't do anything rediculous,
 * If everything is sane then the request is issued.
 *
 * If no errors occured then the original byte count is returned,
 * otherwise -1 is returned to indicate an error occured.
 */
vdstrategy(io, func)
	register struct iob *io;
	register int func;
{
	dskadr daddr;
	register int ctlr = VDCTLR(io->i_unit), unit = VDUNIT(io->i_unit);
	register fs_tab	*u_info = &dkinfo[ctlr][unit].info;
	register int op = (func == READ) ? RD : WD;
	register int blk;

	if (io->i_cc == 0 || io->i_cc > 65535) {
		printf("dk%d: invalid transfer size %d\n", io->i_unit,
		    io->i_cc);
		_stop("");
	}
	blk = io->i_bn * DEV_BSIZE / u_info->secsize;
	daddr.sector = blk % u_info->nsec;
	daddr.track = (blk / u_info->nsec) % u_info->ntrak;
	daddr.cylinder = (blk/u_info->nsec) / u_info->ntrak;
	if (vdaccess(io, &daddr, op) & HRDERR) {
		vdprint_error(io->i_unit, "i/o error", dcb.operrsta,
		    dcb.err_code);
		return (-1);
	}
	mtpr(PADC, 0);
	return (io->i_cc);
}

struct	vdstatus {
	int	bit;
	char	*meaning;
} vdstatus[] = {
	{ DRVNRDY,	"drive not ready" },
	{ INVDADR,	"invalid disk address" },
	{ DNEMEM,	"non-existent memory" },
	{ PARERR,	"parity error" },
	{ OPABRT,	"operation aborted" },
	{ WPTERR,	"drive write protect" },
	{ DSEEKERR,	"seek error" },
	{ UCDATERR,	"uncorrectable data error" },
	{ CTLRERR,	"controller error" },
	{ NOTCYLERR,	"not on cylinder" },
	{ INVCMD,	"invalid controller command" },
	{ -1,		"controller error" }
};
#define	NVDSTATUS	(sizeof (vdstatus) / sizeof (vdstatus[0]))

vdprint_error(unit, str, status, smde_status)
	int unit;
	char *str;
	u_long status;
	u_long smde_status;
{
	register struct vdstatus *sp;

	printf("dk%d: %s; ", unit, str);
	for (sp = vdstatus; sp < &vdstatus[NVDSTATUS]; sp++)
		if (status & sp->bit) {
			printf("%s. status %x", sp->meaning, status);
			break;
		}
	if (smde_status)
		printf(", code %x", smde_status);
	printf("\n");
}

vdaccess_with_no_trailer(io, function, time)
	register struct iob *io;
	register int function, time;
{
	register int ctlr = VDCTLR(io->i_unit);
	register cdr *ctlr_addr = VDADDR(ctlr);

	dcb.opcode = function;		/* command */
	dcb.intflg = NOINT;
	dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)VDUNIT(io->i_unit);
	dcb.trailcnt = (char)0;
	mdcb.firstdcb = &dcb;
	mdcb.vddcstat = 0;
	VDDC_ATTENTION(ctlr_addr, &mdcb, vdinfo[ctlr].vd_type);
	if (!vdpoll(ctlr_addr, &dcb, time, vdinfo[ctlr].vd_type))
		_stop(" during initialization operation.\n");
	return dcb.operrsta;
}

vdaccess(io, daddr, func)
	register struct iob *io;
	dskadr *daddr;
	int func;
{
	register int ctlr = VDCTLR(io->i_unit);
	register cdr *ctlr_addr = VDADDR(ctlr);

	dcb.opcode = (short)func;	/* format sector command */
	dcb.intflg = NOINT;
	dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)VDUNIT(io->i_unit);
	dcb.trailcnt = (char)(sizeof(trrw) / 4);
	dcb.trail.rwtrail.memadr = io->i_ma; 
	dcb.trail.rwtrail.wcount = ((io->i_cc + 1) / sizeof(short));
	dcb.trail.rwtrail.disk.cylinder = daddr->cylinder;
	dcb.trail.rwtrail.disk.track = daddr->track;
	dcb.trail.rwtrail.disk.sector = daddr->sector;
	mdcb.firstdcb = &dcb;
	mdcb.vddcstat = 0;
	VDDC_ATTENTION(ctlr_addr, &mdcb, vdinfo[ctlr].vd_type);
	if (!vdpoll(ctlr_addr, &dcb, 60, vdinfo[ctlr].vd_type))
		_stop(" during i/o operation.\n");
	return (dcb.operrsta);
}

/*
 * Dump the MDCB and DCB for diagnostic purposes.  This
 * routine is called whenever a fatal error is encountered.
 */
vdprintdcb(ptr)
	register long *ptr;
{
	register long i, trailer_count;

	printf("mdcb: %lx %lx %lx %lx\n", ptr[0], ptr[1], ptr[2], ptr[3]);
	if (ptr = (long *)*ptr) {
		printf("dcb:");
		trailer_count = ptr[3] & 0xff;
		for (i=0; i<7+trailer_count; i++) {
			uncache(&ptr[i]);
			printf("  %lx", ptr[i]);
		}
	}
	printf("\n");
	DELAY(5000000);
}

/*
 * Poll controller until operation completes
 * or timeout expires.
 */
vdpoll(addr, dcb, t, type)
	register cdr *addr;
	register fmt_dcb *dcb;
	register int t, type;
{

	t *= 1000;
	uncache(&dcb->operrsta);
	while ((dcb->operrsta&(DCBCMP|DCBABT)) == 0) {
		DELAY(1000);
		uncache(&dcb->operrsta);
		if (--t <= 0) {
			printf("vd: controller timeout");
			VDDC_ABORT(addr, type);
			DELAY(30000);
			uncache(&dcb->operrsta);
			return (0);
		}
	}
	if (type == SMD_ECTLR) {
		uncache(&addr->cdr_csr);
		while (addr->cdr_csr&CS_GO) {
			DELAY(50);
			uncache(&addr->cdr_csr);
		}
		DELAY(300);
		uncache(&dcb->err_code);
	}
	DELAY(200);
	uncache(&dcb->operrsta);
	return (1);
}
