/*
 * Stand alone driver for the HDC controller
 *
 *	@(#)hd.c	7.4 (Berkeley) %G%
 */
#define	KERNEL

#include "machine/mtpr.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "buf.h"
#include "ioctl.h"
#include "disklabel.h"
#include "saio.h"

#include "../tahoevba/dsk.h"
#include "../tahoevba/dskio.h"
#include "../tahoevba/hdc.h"

#define	NHD		4
#define	NDRIVE		8		/* drives per controller */
#define	HDSLAVE(x)	((x) % NDRIVE)
#define	HDCTLR(x)	((x) / NDRIVE)

#define	HDC_DEFBUS	0		/* we only handle bus zero, for now */
#define	HDREG(x)	(ctlr_addr->x)	/* standalone io to an hdc register */
#define	HID_HDC		0x01		/* hvme_id for HDC */

/*
 * hdc controller table. It contains information about the hdc controller.
 */
typedef struct {
	int		ctlr;		/* controller number (0-15) */
	hdc_regs_type	*registers;	/* base address of hdc io registers */
	hdc_mid_type	mid;		/* the module id is read to here */
	master_mcb_type	master_mcb;	/* the master mcb for this hdc */
	mcb_type	mcb;		/* an mcb for i/o to the controller */
} hdctlr_type;

hdctlr_type hdc_ctlr[HDC_MAXCTLR][HDC_MAXBUS];

/*
 * hdc unit table. It contains information about the hdc drive.
 * Some information is obtained from the profile prom and geometry block.
 */
typedef struct {
	par_tab	partition[GB_MAXPART];	/* partition definitions */
	int	ctlr;			/* the controller number (0-15) */
	int	slave;			/* the slave number (0-4) */
	int	unit;			/* the unit number (0-31) */
	int	id;			/* identifies the disk model */
 	int	cylinders;		/* number of logical cylinders */
	int	heads;			/* number of logical heads */
	int	sectors;		/* number of logical sectors/track */
	int	phys_cylinders;		/* number of physical cylinders */
	int	phys_heads;		/* number of physical heads */
	int	phys_sectors;		/* number of physical sectors/track */
	int	def_cyl;		/* logical cylinder of drive def */
	int	def_cyl_count;		/* number of logical def cylinders */
	int	diag_cyl;		/* logical cylinder of diag area */
	int	diag_cyl_count;		/* number of logical diag cylinders */
	int	rpm;			/* disk rpm */
	int	bytes_per_sec;		/* bytes/sector -vendorflaw conversn */
	int	format;			/* format program is active */
	u_long	phio_data[HDC_PHIO_SIZE];	/* data for physical io */
} hdunit_type;

hdunit_type	hdc_unit [HDC_MAXDRIVE] [HDC_MAXCTLR] [HDC_MAXBUS];

/*
 * hdopen --
 *	initialize the hdc and read the disk label
 */
hdopen(io)
	register struct iob	*io;	/* i/o block */
{
	drive_stat_type	status;		/* the hdc status is read to here */
	hdc_mid_type	*id;		/* the hdc module id */
	hdc_regs_type	*ctlr_addr;	/* hdc i/o registers */
	hdctlr_type	*hc;		/* hdc ctlr information table */
	hdunit_type	*hu;		/* disk unit information table */
	geometry_sector	geometry;	/* the geometry block sector */
	geometry_block	*geo;		/* the geometry block */
	mcb_type	*mcb;		/* an mcb to send commands to hdc */
	long	junk = 0;		/* badaddr will write junk here */
	int	par;			/* partition number */
	int	bus, ctlr, drive, error, i, unit;

	/* validate the device specification */
	if ((ctlr = HDCTLR(io->i_unit)) >= HDC_MAXCTLR) {
		printf("invalid controller number\n");
		return(ENXIO);
	}
	if ((drive = HDSLAVE(io->i_unit)) < 0 || drive > HDC_MAXDRIVE - 1) {
		printf("hdc: bad drive number.\n");
		return(EUNIT);
	}
	if ((par = io->i_boff) < 0 || par > 7) {
		printf("hdc: bad partition number.\n");
		return(EUNIT);
	}
	bus = HDC_DEFBUS;

	ctlr_addr = (hdc_regs_type *)(bus ?
		0x80000000 | ctlr << 24 | HDC_MID << 16 :
		0xC0000000 | ctlr << 24 | HDC_MID << 16);

	hu = &hdc_unit[drive][ctlr][bus];
	hc = &hdc_ctlr[ctlr][bus];
	mcb = &hc->mcb;

	/* init drive structure. */
	hu->slave = drive;
	hc->registers = ctlr_addr;

	/* insure that this is an hdc, then reset the hdc. */
	if (wbadaddr(&ctlr_addr->module_id_reg, 4, &junk)) {
		printf("hd%d: %x: invalid csr\n", ctlr, (u_int)ctlr_addr);
		return(ENXIO);
	}
	HDREG(soft_reset_reg) = 0;
	DELAY(1000000);

	/*
	 * Read in the hdc module id word.  The controller is bad if the
	 * hdc's writeable control store is not loaded or if the hdc failed
	 * the functional integrity test for any reason.
	 */
	id = &hc->mid;
	HDREG(module_id_reg) = (u_long)id;
	DELAY(10000);
	mtpr(PADC, 0);

	if (id->module_id != (u_char)HDC_MID) {
		printf("hdc: Controller bad module id: id = %x\n",
		    id->module_id);
		return(ENXIO);
	}
	if (id->code_rev == (u_char)0xFF) {
		printf("hdc: Controller micro-code is not loaded.\n");
		return(ENXIO);
	}
	if (id->fit != (u_char)0xFF) {
		printf("hdc:  Controller FIT test failed: error= %x\n",
		    id->fit);
		return(ENXIO);
	}

	/* Read the drive status. Save important info. */
	mcb->command = HCMD_STATUS;
	mcb->drive = drive;
	mcb->cyl = 0;
	mcb->head = 0;
	mcb->sector = 0;
	mcb->chain[0].lwc = (long)sizeof(drive_stat_type) / sizeof(long);
	mcb->chain[0].ta  = (long)&status;
	if (hdmcb(mcb, io))
		return(EIO);

	/*
	 * Report drive down if anything in the drive status is bad.
	 * If fault condition, reading will try to clear the fault.
	 */
	if (status.drs & DRS_FAULT)
		printf("hdc: clearing drive fault.\n");
	if (!(status.drs & DRS_ONLINE)) {
		printf("hdc: drive is not online.\n");
		return(EIO);
	}

	hu->cylinders = status.max_cyl+1;
	hu->heads = status.max_head+1;
	hu->sectors = status.max_sector+1;
	hu->def_cyl = status.def_cyl;
	hu->def_cyl_count = status.def_cyl_count;
	hu->diag_cyl = status.diag_cyl;
	hu->diag_cyl_count = status.diag_cyl_count;
	hu->phys_cylinders = status.max_phys_cyl+1;
	hu->phys_heads = status.max_phys_head+1;
	hu->phys_sectors = status.max_phys_sector+1;
	hu->bytes_per_sec = status.bytes_per_sec;
	hu->id = status.id;
	hu->rpm = status.rpm;
	hu->partition[HDC_DEFPART].start=
		hu->def_cyl * hu->sectors * hu->heads / HDC_SPB;
	hu->partition[HDC_DEFPART].length =
		hu->def_cyl_count * hu->sectors * hu->heads / HDC_SPB;

	/*
	 * Read the geometry block (at head=0 sector=0 of the drive
	 * definition cylinder), validate it (must have the correct
	 * version number, header, and checksum).
	 */
	geo = &geometry.geometry_block;
	mcb->command = HCMD_READ;
	mcb->drive = drive;
	mcb->cyl = hu->def_cyl;
	mcb->head = 0;
	mcb->sector = 0;
	mcb->chain[0].lwc = sizeof(geometry_sector) / sizeof(long);
	mcb->chain[0].ta  = (long)&geometry;
	io->i_boff = hu->partition[HDC_DEFPART].start;		/* default */
	if (hdmcb(mcb, io)) {
 		printf("hdc: could not read geometry block\n");
		return(EIO);
	}
	io->i_boff = 0;
 	if (geo->version > 64000  ||  geo->version < 0) {
 		printf("hdc: bad geometry block version#\n");
		return(ENXIO);
	}
 	if (strcmp(&geo->id[0], GB_ID) != 0) {
 		printf("hdc: bad geometry block header\n");
		return(ENXIO);
	}
	GB_CHECKSUM(geo, i);
	if (geometry.checksum != i) {
		printf("hdc: bad geometry block checksum\n");
		return(ENXIO);
	}

	/*
	 * Set the partition start/size info.
	 * Note: this info was already defaulted to be the disk
	 * definition partition.
	 */
	if (par != HDC_DEFPART)
		if (geo->partition[par].length == 0) {	/* XXX */
			printf("hdc: null partition\n");
			return(ENXIO);
		}
		else {
			hu->partition[par].start  = geo->partition[par].start;
			hu->partition[par].length = geo->partition[par].length;
			io->i_boff = hu->partition[par].start;
		}
	return(0);
}

/*
 * hdstrategy --
 *	The hdc strategy routine. This routine does the disk reads/writes. If
 *	this is the format program, read/writes are forced to be within the
 *	disk definition partition.  Returns the number of bytes transferred.
 */
hdstrategy(io, cmd)
	register struct iob	*io;	/* i/o block */
	int	cmd;			/* i/o operation to perform */
{
	mcb_type	*mcb;		/* mcb to send to the hdc */
	hdunit_type	*hu;		/* disk unit information table */
	hdctlr_type	*hc;		/* hdc ctlr information table */
	long		err;		/* error code */
	long		sector;		/* sector number for i/o */
	int		partstart;	/* block number of partition start */
	int		partlen;	/* number of blocks in partition */
	int		bytes;		/* number of bytes to transfer */
	int		bus, ctlr, drive;

	bus = HDC_DEFBUS;
	ctlr = HDCTLR(io->i_unit);
	drive = HDSLAVE(io->i_unit);
	hu = &hdc_unit[drive][ctlr][bus];
	hc = &hdc_ctlr[ctlr][bus];

	/*
	 * Only the format program can access the disk definition tracks.
	 */
	if (io->i_boff == HDC_DEFPART && !hu->format) {
		printf("hdc: partition 7 is protected\n");
		return(0);
	}

	/*
	 * Insure the transfer fits in the partition.
	 * Set and validate transfer size.
	 */
	partstart = hu->partition[io->i_boff].start;
	partlen = hu->partition[io->i_boff].length;
	if (io->i_bn < partstart || io->i_bn >= partstart + partlen)
		return(0);
	bytes = MIN(io->i_cc, DEV_BSIZE * (partstart + partlen-io->i_bn));
	if (io->i_cc & 3) {
		printf("hdc: i/o not a longword multiple\n");
		return(0);
	}

	/*
	 * Set up the mcb and send it to the hdc.
	 */
	mcb = &hc->mcb;
	sector = io->i_bn * HDC_SPB;
	mcb->command = (cmd == READ) ? HCMD_READ : HCMD_WRITE;
	mcb->drive = hu->slave;
	mcb->cyl = sector / (hu->sectors * hu->heads);
	mcb->head = (sector / hu->sectors) % hu->heads;
	mcb->sector = sector % hu->sectors;
	mcb->chain[0].ta  = (u_long)io->i_ma;
	mcb->chain[0].lwc = (bytes + 3) / sizeof(long);
	err = hdmcb(mcb, io);
	io->i_error = err;
	return(err ? 0 : bytes);
}

hdioctl(io, command, arg)
	struct iob	*io; 		/* i/o block */
	int	command;		/* ioctl commmand */
	int	arg;			/* data; format depends on ioctl */
{
	register int	i;
	mcb_type	*mcb;
	hdunit_type	*hu;		/* disk unit information table */
	hdctlr_type	*hc;		/* hdc ctlr information table */
	int	bus, ctlr, drive;

	bus = HDC_DEFBUS;
	ctlr = HDCTLR(io->i_unit);
	drive = HDSLAVE(io->i_unit);
	hu = &hdc_unit[drive][ctlr][bus];
	hc = &hdc_ctlr[ctlr][bus];

	switch (command) {

	case DSKIOCFORMAT: {

		/*
		 * Format a disk track. The received argument is a pointer
		 * to a "formatop" structure describing the track to format.
		 *
		 * Set up a buffer with each longword corresponding to a
		 * sector on the track; a 1 means no flaw, a 0 means a flaw.
		 * Send an mcb to the hdc to format the track.
		 */

		register struct formatop *track;

		if (!hu->format)
			return(1);
		track = (struct formatop *)arg;
		mcb = &hc->mcb;
		for (i = 0; i < hu->phys_sectors; i++)
			hu->phio_data[i] = 1;
		for (i = 0; i < track->flaw_count; i++)
			hu->phio_data[track->flaw[i]] = 0;
		mcb->command = HCMD_FORMAT;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = (u_long)hu->phio_data;
		mcb->chain[0].lwc = hu->phys_sectors;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return(EIO);
		break;
	}

	case DSKIOCCERTIFY: {

		/*
		 * Certify a disk track. The received argument is a pointer
		 * to a "formatop" structure describing the track to certify.
		 *
		 * Send an mcb to the hdc to certify the track.
		 * The controller returns data in which each longword
		 * corresponds to a sector on the track; a 1 means no flaw,
		 * a 0 means a flaw.
		 */

		register struct formatop *track;

		if (!hu->format)
			return(1);
		track = (struct formatop *)arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_CERTIFY;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = (u_long)hu->phio_data;
		mcb->chain[0].lwc = hu->phys_sectors;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return(EIO);
		track->flaw_count = 0;
		for (i = 0; i < hu->phys_sectors; i++) {
			if (track->flaw_count >= MAXVFLAW)
				break;
			if (hu->phio_data[i] == 0) {
				track->flaw[track->flaw_count] = i;
				track->flaw_count++;
			}
		}
		break;
	}

	case DSKIOCVERIFY: {

		/*
		 * Verify a disk track. The received argument is a pointer
		 * to a "formatop" structure describing the track to verify.
		 */

		register struct formatop *track;

		if (!hu->format)
			return(1);
		track = (struct formatop *)arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_VERIFY;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = 0;
		mcb->chain[0].lwc = 0;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return(EIO);
		break;
	}

	case DSKIOCFORMATCTL: {

		/*
		 * This ioctl provides special format control.
		 * Currently the valid arguments are:
		 *
		 * arg = 0	disable formatting;
		 *
		 * arg = 1	enable formatting (allow privileged access);
		 *		formatting must not already be enabled;
		 *		For formatting, change to use partition 7.
		 */

		if (arg < 0 || arg > 1)
			return(1);
		if (arg == 1) {
			if (hu->format)
				return(1);
			/* If not already formatting.... */
			hu->format = 1;
			/* io->i_part = HDC_DEFPART; */
			io->i_boff = hu->partition[HDC_DEFPART].start;
		}
		else
			hu->format = 0;
		break;
	}

	case DSKIOCSTATUS: {

		/*
		 * Return info about the disk. Caller's parameter is a
		 * pointer to a dsk_status structure.
		 */

		register dsk_status *status;

		status = (dsk_status *)arg;
		status->id = hu->id;
		status->drive_status = 0;
		status->rpm = hu->rpm;
		status->bytes_per_sec = hu->bytes_per_sec;
		status->cylinders = hu->cylinders;
		status->heads = hu->heads;
		status->sectors = hu->sectors;
		status->phys_cylinders = hu->phys_cylinders;
		status->phys_heads = hu->phys_heads;
		status->phys_sectors = hu->phys_sectors;
		status->diag_cyl = hu->diag_cyl;
		status->diag_cylinders = hu->diag_cyl_count;
		status->def_cyl = hu->def_cyl;
		status->def_cylinders = hu->def_cyl_count;
		break;
	}

	case DSKIOCVENDORFLAW: {

		/*
		 * Return vendor flaw info.
		 *
		 * Read in the vendor data (data for each track is at
		 * relative sector 0 of the track); then copy the
		 * vendor flaw data to the caller's buffer.
		 */

		register vflaw_type *vflaw;
		register struct flaw *vendor;

		if (!hu->format)
			return(1);
		vflaw = (vflaw_type *)arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_VENDOR;
		mcb->drive = hu->slave;
		mcb->chain[0].lwc = HDC_VDATA_SIZE;
		mcb->chain[0].ta  = (u_long)hu->phio_data;
		mcb->cyl = vflaw->cylinder;
		mcb->head = vflaw->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return(EIO);
		vendor = (struct flaw *)&hu->phio_data[0];
		for (i = 0; i < MAXVFLAW; i++) {
			vflaw->flaw[i].offset = vendor[i].offset;
			vflaw->flaw[i].length = vendor[i].length;
		}
		break;
	}
	}
	return(0);
}

/*
 * hdmcb --
 *	internal routine used to send mcb's to the hdc
 */
static
hdmcb(mcb, io)
	register mcb_type	*mcb;	/* mcb to send to the hdc */
	register struct iob	*io;	/* i/o block */
{
	register u_int	*ptr;
	master_mcb_type	*master_mcb;	/* the hdc's master mcb */
	hdctlr_type	*hc;		/* hdc ctlr information table */
	hdc_regs_type	*ctlr_addr;	/* pointer to hdc i/o registers */
	int	timeout;		/* used to timeout the mcb */
	int	bus, ctlr, i, end;

	bus = HDC_DEFBUS;
	ctlr = HDCTLR(io->i_unit);
	hc = &hdc_ctlr[ctlr][bus];

	mcb->interrupt = FALSE;
	mcb->priority = 0;
	mcb->forw_phaddr = 0;
	mcb->context = 0;
	mcb->reserved[0] = 0;
	mcb->reserved[1] = 0;
	master_mcb = &hc->master_mcb;
	master_mcb->forw_phaddr = (long)&mcb->forw_phaddr;
	master_mcb->mcs = 0;
	master_mcb->interrupt = 0;
	master_mcb->reserve1 = 0;
	master_mcb->reserve2 = 0;
	master_mcb->context = 0;
	master_mcb->mcl = MCL_IMMEDIATE;
	for (i = 0; i < HDC_XSTAT_SIZE; i++)
		master_mcb->xstatus[i] = 0;
	ctlr_addr = hc->registers;
	HDREG(master_mcb_reg) = (u_long)master_mcb;
	timeout = 15000;
	for (;;) {
		DELAY(1000);
		mtpr(PADC, 0);
		if (master_mcb->mcs & MCS_DONE &&
		    !(master_mcb->mcs & MCS_FATALERROR))
			return(0);
		if (--timeout > 0 && !(master_mcb->mcs & MCS_FATALERROR))
			continue;
		if (master_mcb->mcs & MCS_FATALERROR)
			printf("hdc: controller fatal error\n");
		else
			printf("hdc: controller timed out\n");

		printf("mmcb: ");
		ptr = (u_int *)master_mcb;
		for (i = 0; i < 8; i++)
			printf(" %x", ptr[i]);
		for (i = 7 + HDC_XSTAT_SIZE; i > 7; i--) {
			end = i;
			if (ptr[i] != 0)
				break;
		}
		for (i = 8; i <= end; i++)
			printf(" %x", ptr[i]);

		printf("\nmcb:  ");
		ptr = (u_int *)&mcb->forw_phaddr;
		for (i = 0; i < 6; i++)
			printf(" %x", ptr[i]);
		for (i = 6; i < 72; i += 2) {
			printf("  %x %x", ptr[i], ptr[i+1]);
			if (!(ptr[i] & 0x80000000))
				break;
		}
		printf("\n");
		return(1);
	}
}
