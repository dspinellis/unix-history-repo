/*	vd.c	7.1	86/01/12	*/
/*
** Stand alone driver for the VDDC controller 
**	TAHOE Version, Oct 1983.
**
*/
#include "../machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#define VDGENDATA 1
#include "../tahoevba/vddcreg.h"
#undef	VDGENDATA
#include "saio.h"

#define NVD		4			/* Max number of controllers */

#define TRUE		1
#define	FALSE		0
#define VDUNIT(x)	(minor(x) & 0x3)
#define VDCTLR(x)	(minor(x) >> 2)

/*
**	MDCB
*/

fmt_mdcb	mdcb;

/*
**	DCB
*/

fmt_dcb		dcb;

/*
** Unit specific information.
*/

struct {
	char	configured;
	fs_tab	info;
}unit_info[NVD][16];

/*
** Controller specific information.
*/

struct {
	unsigned char	ctlr_type;
	char		*ctlr_name;
	unsigned char	initialized;
	unsigned char	ctlr_started;
} ctlr_info[NVD];

static char	junk[1024];


/*
**
*/

vdopen(io)
register struct iob	*io;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register int	i, j;

	/* Make sure controller number is in range */
	if(ctlr >= NVD) {
		printf("vd%d: Unit number can't be greater than %x!\n",
		    io->i_unit, (NVD * 4) - 1);
		_stop("");
	}
	/* check file system for validity */
	if((io->i_boff < 0) || (io->i_boff > 5)) {
		printf("vd%d: File system #%d, should be less than #6.\n",
		    io->i_unit, io->i_boff);
		_stop("");
	}
	if(!ctlr_info[ctlr].initialized) {
		vdinit(io);	/* initialize controller/drive */
		ctlr_info[ctlr].initialized = TRUE;
		for(i=0; i<NVD; i++)
			for(j=0; j<16; j++)
				unit_info[i][j].configured = FALSE;
	}
	if(!unit_info[ctlr][unit].configured) {
		vdconfigure_drive(io);
		unit_info[ctlr][unit].configured = TRUE;
	}
	io->i_boff = unit_info[ctlr][unit].info.partition[io->i_boff].par_start;
}


/*
**
*/

vdinit(io)
register struct iob	*io;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register cdr	*ctlr_addr = (cdr *)(vddcaddr[ctlr]+IOBASE);
	register char	*ctlr_type;

	/* Check to see if controller is really there */
	if(badaddr(ctlr_addr, 2)) {
		printf("vd%d: Controller %d is non-existant!\n",
		    io->i_unit, ctlr);
		_stop("");
	}
	/* Probe further to find what kind of controller it is */
	ctlr_addr->cdr_reset = 0xffffffff;
	DELAY(1000000);
	/* Probe further to find what kind of controller it is */
	if(ctlr_addr->cdr_reset != 0xffffffff) {
		ctlr_info[ctlr].ctlr_type = SMDCTLR;
		ctlr_info[ctlr].ctlr_name = "SMD";
		DELAY(1000000);
	}
	else {
		ctlr_info[ctlr].ctlr_type = SMD_ECTLR;
		ctlr_info[ctlr].ctlr_name = "SMD/E";
		ctlr_addr->cdr_reserved = 0x0;
		DELAY(3000000);
	}
	if(ctlr_info[ctlr].ctlr_type == SMD_ECTLR) {
		ctlr_addr->cdr_csr =  0;
		ctlr_addr->mdcb_tcf = AM_ENPDA;
		ctlr_addr->dcb_tcf = AM_ENPDA;
		ctlr_addr->trail_tcf = AM_ENPDA;
		ctlr_addr->data_tcf = AM_ENPDA;
		ctlr_addr->cdr_ccf = CCF_STS | XMD_32BIT | BSZ_16WRD |
		    CCF_ENP | CCF_EPE /* | CCF_EDE */ | CCF_ECE | CCF_ERR;
	}
	if(vdaccess_with_no_trailer(io, INIT, 8) & HRDERR) {
		vdprint_error(io->i_unit, "Initialization error",
		    dcb.operrsta,dcb.err_code);
		_stop("");
	}
	if(vdaccess_with_no_trailer(io, DIAG, 8) & HRDERR) {
		vdprint_error(io->i_unit, "Diagnostic error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
}


/*
**
*/

vdconfigure_drive(io)
register struct iob	*io;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register fs_tab	*file_sys;
	dskadr		daddr;
	register int	i;

	for(i=0; i < nvddrv; i++) {
		unit_info[ctlr][unit].info = vdst[i];
		if(ctlr_info[ctlr].ctlr_type == SMDCTLR)
			if(unit_info[ctlr][unit].info.nsec != 32)
				continue;
		vdconfigure(io, 0);
		daddr.cylinder = unit_info[ctlr][unit].info.ncyl - 2;
		daddr.track = unit_info[ctlr][unit].info.ntrak - 1;
		daddr.sector = unit_info[ctlr][unit].info.nsec - 1;
		io->i_ma = junk;
		io->i_cc = unit_info[ctlr][unit].info.secsize;
		if(!(vdaccess(io, &daddr, RD) & HRDERR))
			return;
	}
	printf("vd%d: Unrecognizable drive; controller %d, unit %d!\n",
	    io->i_unit, ctlr, unit);
	_stop("");
}


/*
**
*/

vdstart_drive(io)
register struct iob	*io;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register int	io_unit_save = io->i_unit;

	if(ctlr_info[ctlr].ctlr_started) {
		DELAY(5500000);
		return TRUE;
	}
	io->i_unit &= ~3;
	if(vdaccess_with_no_trailer(io, VDSTART, ((unit * 6) + 62)) & HRDERR) {
		vdprint_error(io->i_unit, "Start error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
	ctlr_info[ctlr].ctlr_started = TRUE;
	io->i_unit = io_unit_save;
	DELAY((unit * 5500000) + 62000000);
	return TRUE;
}


/*
**  This routine actually configures a particular drive.
**
**  If the controller is an SMD/E controller then the number of sectors per
**  track is loaded into the appropriate register, otherwise it is left 
**  alone because the old SMD controller requires a constant 32 sectors
**  per track for it's drives. (an error would be returned if the value is
**  loaded.)
**
**  In the stand-alone spirit of things the system is halted if an error
**  occurs during this operation.
*/

vdconfigure(io, pass)
register struct iob	*io;
int	pass;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register cdr	*ctlr_addr = (cdr *)(vddcaddr[ctlr]+IOBASE);

	dcb.opcode = RSTCFG;		/* command */
	dcb.intflg = NOINT;
	dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)unit;
	dcb.trail.rstrail.ncyl = unit_info[ctlr][unit].info.ncyl;
	dcb.trail.rstrail.nsurfaces = unit_info[ctlr][unit].info.ntrak;
	if(ctlr_info[ctlr].ctlr_type == SMD_ECTLR) {
		dcb.trailcnt = (char)4;
		dcb.trail.rstrail.nsectors = unit_info[ctlr][unit].info.nsec;
		dcb.trail.rstrail.slip_sec = unit_info[ctlr][unit].info.nslip;
	}
	else
		dcb.trailcnt = (char)2;
	mdcb.firstdcb = &dcb;
	mdcb.vddcstat = 0;
	VDDC_ATTENTION(ctlr_addr, &mdcb, ctlr_info[ctlr].ctlr_type);
	POLLTILLDONE(ctlr_addr,&dcb,5,ctlr_info[ctlr].ctlr_type);
	if(vdtimeout <= 0)
		_stop(" during drive configuration.\n");
	if(dcb.operrsta & (NOTCYLERR | DRVNRDY))
		if(!pass) {
			vdstart_drive(io);
			vdconfigure(io, 1);
		}
	if(dcb.operrsta & HRDERR) {
		vdprint_error(io->i_unit, "Configuration error",
		    dcb.operrsta, dcb.err_code);
		_stop("");
	}
}


/*
**  Strategy is called to the actual I/O to the disk drives.
**
**  Some simple checks are made to make sure we don't do anything rediculus,
**  If everything is sane then the request is issued.
**
**  If no errors occured then the original byte count is returned,
**  otherwise -1 is returned to indicate an error occured.
*/

vdstrategy(io, func)
register struct iob	*io;
register int func;
{
	dskadr		daddr;
	register int	ctlr = VDCTLR(io->i_unit);
	register int	unit = VDUNIT(io->i_unit);
	register fs_tab	*u_info = &unit_info[ctlr][unit].info;
	register int	op = (func == READ) ? RD : WD;
	register int	blk;

	if(io->i_cc == 0)
		_stop("vd: Can't transfer zero length records!\n");
	if(io->i_cc > 0xffff)
		_stop("vd: Can't transfer greater than 2 to the 16th bytes!\n");
	blk = io->i_bn * DEV_BSIZE / u_info->secsize;
	daddr.sector = blk % u_info->nsec;
	daddr.track = (blk / u_info->nsec) % u_info->ntrak;
	daddr.cylinder = (blk/u_info->nsec) / u_info->ntrak;
	if(vdaccess(io, &daddr, op) & HRDERR) {
		vdprint_error(io->i_unit,"I/O error",dcb.operrsta,dcb.err_code);
		return(-1);
	}
	mtpr(PADC, 0);
	return(io->i_cc);
}



/*
**
*/

vdprint_error(unit, str, status, smde_status)
int		unit;
char		*str;
unsigned long	status;
unsigned long	smde_status;
{
	printf("vd%d: %s; ", unit, str);
	if(status & DRVNRDY)
		printf("Drive is not ready");
	else if(status & INVDADR)
		printf("Invalid disk address issued");
	else if(status & DNEMEM)
		printf("Non-existent memory error");
	else if(status & PARERR)
		printf("Main memory parity error");
	else if(status & OPABRT) 
		printf("Program aborted operation");
	else if(status & WPTERR)
		printf("Drive is write protected");
	else if(status & DSEEKERR)
		printf("Disk seek error");
	else if(status & UCDATERR)
		printf("Uncorrectable data error");
	else if(status & CTLRERR)
		printf("Controller faulted");
	else if(status & NOTCYLERR)
		printf("Not on cylinder error");
	else if(status & INVCMD)
		printf("Invalid command issued to controller");
	else 
		printf("Controller error");
	printf("!  Status = 0x%x", status);
	if(smde_status)
		printf("  Error code = %x", smde_status);
	printf("\n");
}


/*
**
*/

vdaccess_with_no_trailer(io, function, time)
register struct iob	*io;
register int		function, time;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register cdr	*ctlr_addr = (cdr *)(vddcaddr[ctlr]+IOBASE);

	dcb.opcode = function;		/* command */
	dcb.intflg = NOINT;
	dcb.nxtdcb = (fmt_dcb *)0;	/* end of chain */
	dcb.operrsta  = 0;
	dcb.devselect = (char)VDUNIT(io->i_unit);
	dcb.trailcnt = (char)0;
	mdcb.firstdcb = &dcb;
	mdcb.vddcstat = 0;
	VDDC_ATTENTION(ctlr_addr, &mdcb, ctlr_info[ctlr].ctlr_type);
	POLLTILLDONE(ctlr_addr,&dcb,time,ctlr_info[ctlr].ctlr_type);
	if(vdtimeout <= 0)
		_stop(" during initialization operation.\n");
	return dcb.operrsta;
}


/*
**
*/

vdaccess(io, daddr, func)
register struct iob	*io;
dskadr			*daddr;
int			func;
{
	register int	ctlr = VDCTLR(io->i_unit);
	register cdr	*ctlr_addr = (cdr *)(vddcaddr[ctlr]+IOBASE);

	dcb.opcode = (short)func;		/* format sector command */
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
	VDDC_ATTENTION(ctlr_addr, &mdcb, ctlr_info[ctlr].ctlr_type);
	POLLTILLDONE(ctlr_addr, &dcb, 60, ctlr_info[ctlr].ctlr_type);
	if(vdtimeout <= 0)
		_stop(" during I/O operation.\n");
	return dcb.operrsta;
}

/*
**	Print_dcb() dumps the MDCB and DCB for diagnostic purposes.  This
** routine is called whenever a fatal error is encountered.
*/

vdprintdcb(ptr)
register long	*ptr;
{
	register long	i;
	register long	trailer_count;

	printf("Dump of MDCB: ");
	for(i=0; i<4; i++)
		printf("  %lx", *(ptr+i));
	if(ptr = (long *)*ptr) {
		printf(" and DCB:");
		trailer_count = *(ptr+3) & 0xff;
		for(i=0; i<7+trailer_count; i++) {
			uncache(ptr+i);
			printf("  %lx", *(ptr+i));
		}
	}
	printf("\n");
	for(i=0; i<5000000; i++) ;
}

