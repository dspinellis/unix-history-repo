/*
 * Stand alone driver for the HDC controller
 */

#define KERNEL

#include "../uts/machine/ml/mtpr.h"
#include "../uts/machine/sys/param.h"
#include "../uts/machine/sys/systm.h"
#include "../uts/machine/sys/buf.h"
#include "../uts/machine/sys/time.h"
#include "../uts/machine/sys/vnode.h"
#include "../uts/machine/ufs/inode.h"
#include "../uts/machine/ufs/fs.h"
#include "../uts/machine/sys/vbavar.h"
#include "../uts/machine/sys/ioctl.h"
#include "../uts/machine/sys/dsk.h"
#include "../uts/machine/sys/dskio.h"
#include "../uts/machine/sys/hdc.h"
#include "../stand/saio.h"

#define HDREG(x)	(ctlr_addr->x)	/* standalone io to an hdc register */

/*
 * hdc controller table. It contains information about the hdc controller.
 */

typedef struct {
	int		ctlr;		/* controller number (0-15)         */
	hdc_regs_type	*registers;	/* base address of hdc io registers */
	hdc_mid_type	mid;		/* the module id is read to here    */
	master_mcb_type master_mcb;	/* the master mcb for this hdc      */
	mcb_type	mcb;		/* an mcb for i/o to the controller */
} hdctlr_type;

hdctlr_type hdc_ctlr[HDC_MAXCTLR][HDC_MAXBUS];

/*
 * hdc unit table. It contains information about the hdc drive.
 * Some information is obtained from the profile prom and geometry block.
 */

typedef struct {
	par_tab		partition[GB_MAXPART]; /* partition definitions     */
	int		ctlr;		/* the controller number (0-15)     */
	int		slave;		/* the slave number (0-4)           */
	int		unit;		/* the unit number (0-31)           */
	int		id;		/* identifies the disk model        */
 	int		cylinders;	/* number of logical cylinders      */
	int		heads;		/* number of logical heads          */
	int		sectors;	/* number of logical sectors/track  */
	int		phys_cylinders;	/* number of physical cylinders     */
	int		phys_heads;	/* number of physical heads         */
	int		phys_sectors;	/* number of physical sectors/track */
	int		def_cyl;	/* logical cylinder of drive def    */
	int		def_cyl_count;	/* number of logical def cylinders  */
	int		diag_cyl;	/* logical cylinder of diag area    */
	int		diag_cyl_count;	/* number of logical diag cylinders */
	int		rpm;		/* disk rpm                         */
	int		bytes_per_sec;	/* bytes/sector -vendorflaw conversn*/
	int		format;		/* format program is active         */
	unsigned long	phio_data[HDC_PHIO_SIZE]; /* data for physical io   */
} hdunit_type;

hdunit_type hdc_unit [HDC_MAXDRIVE] [HDC_MAXCTLR] [HDC_MAXBUS] ;

/*************************************************************************
*  Procedure:	hdopen
*
*  Description: The hdc open routine. Initializes the hdc and reads the
*               hdc status and the geometry block.
*
*  Returns:     0   open was not successful
*               1   open was successful
*               -1  this is not an hdc controller
**************************************************************************/

hdopen(io)

register struct iob	*io ;	/* i/o block
				 */
{
	mcb_type	*mcb;		/* an mcb to send commands to hdc   */
	hdunit_type	*hu;		/* disk unit information table      */
	hdctlr_type	*hc;		/* hdc ctlr information table       */
	hdc_mid_type	*id;		/* the hdc module id                */
	geometry_sector geometry;	/* the geometry block sector        */
	geometry_block	*geo;		/* the geometry block               */
	drive_stat_type	status;		/* the hdc status is read to here   */
	int		par;		/* partition number                 */
	int	        ctlr;		/* the controller number            */
	int	        drive;		/* the drive number                 */
	int	        bus;		/* the bus number                   */
	int		unit;		/* the unit number		    */
	int	        i;		/* temp                             */
	hdc_regs_type	*ctlr_addr;	/* hdc i/o registers                */
	int		junk;		/* badaddr will write junk here     */

	par = io->i_part;
	bus = io->i_bus;
	ctlr = io->i_ctlr;
	drive = io->i_drive;
	hu = &hdc_unit[drive][ctlr][bus];
	hc = &hdc_ctlr[ctlr][bus];
	mcb = &hc->mcb;

	/*
	 * Validate the device specification
	 */

	if (ctlr < 1 || ctlr > HDC_MAXCTLR)
		return( -1 );
	if (drive < 0 || drive > (HDC_MAXDRIVE-1)) {
		printf("hdc: bad drive number.\n");
		return( 0 );
	}
	if (par < 0 || par > 7) {
		printf("hdc: bad partition number.\n");
		return( 0 );
	}
	io->i_ctlr_addr =    bus == 0 ?
		0xC0000000 | ctlr << 24 | HDC_MID << 16  :
		0x80000000 | ctlr << 24 | HDC_MID << 16;
	ctlr_addr = (hdc_regs_type *) io->i_ctlr_addr;

	/*
	 * Init drive structure.
	 */

	hu->slave = drive;
	hc->registers = ctlr_addr;

	/*
	 * Insure that this is an hdc, then reset the hdc.
	 */

	if (badaddr(&ctlr_addr->module_id_reg,4,&junk))
  		return( -1 );
	HDREG(soft_reset_reg) = 0;
	DELAY(1000000);

	/*
	 * Read in the hdc module id word.
	 * The controller is bad if the hdc's writeable control
	 * store is not loaded or if the hdc failed the
	 * functional integrity test for any reason.
	 */

	id = &hc->mid;
	HDREG(module_id_reg) = (unsigned long) id;
	DELAY(10000);
	mtpr(0,PADC);
	if (id->module_id != (unsigned char) HDC_MID) {
		printf("hdc: Controller bad module id: id= %x\n",id->module_id);
		return( -1 );
	}
	if (id->code_rev == (unsigned char) 0xFF ) {
		printf("hdc:  Controller micro-code is not loaded.\n");
		return( 0 );
	}
	if (id->fit != (unsigned char) 0xFF ) {
		printf("hdc:  Controller FIT test failed: error= %x\n",id->fit);
		return( 0 );
	}

	/*
	 * Read the drive status. Save important info.
	 */

	mcb->command = HCMD_STATUS;
	mcb->drive = drive;
	mcb->cyl = 0;
	mcb->head = 0;
	mcb->sector = 0;
	mcb->chain[0].lwc = (long) sizeof(drive_stat_type) / 4;
	mcb->chain[0].ta  = (long) &status;
	if (hdmcb(mcb, io))
		return( 0 );
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
	io->i_boff = hu->partition[HDC_DEFPART].start; /* default */

	/*
	 * Report drive down if anything in the drive status is bad.
	 * If fault condition, reading geo will try to clear the fault.
	 */

	if (status.drs & DRS_FAULT)
		printf("hdc: clearing drive fault.\n");
	if ( !(status.drs & DRS_ONLINE)) {
		printf("hdc: drive is not online.\n");
		return( 0 );
	}

	/*
	 * Read the geometry block (at head=0 sector=0 of the drive
	 * definition cylinder), validate it (must have the correct
	 * version number, header, and checksum).
	 */

	geo = &geometry.geometry_block;
	mcb->command = HCMD_READ;
	mcb->drive = drive;
	mcb->cyl = status.def_cyl;
	mcb->head = 0;
	mcb->sector = 0;
	mcb->chain[0].lwc = sizeof(geometry_sector) / 4;
	mcb->chain[0].ta  = (long) &geometry;
	if (hdmcb(mcb, io)) {
 		printf("hdc: could not read geometry block\n");
		return( 1 );
	}
	io->i_boff = 0;
 	if ( geo->version > 64000  ||  geo->version < 0 ) {
 		printf("hdc: bad geometry block version#\n");
		return( 1 );
	}
 	if (strcmp(&geo->id[0],GB_ID) != 0) {
 		printf("hdc: bad geometry block header\n");
		return( 1 );
	}
	GB_CHECKSUM( geo, i );
	if (geometry.checksum != i) {
		printf("hdc: bad geometry block checksum\n");
		return( 1 );
	}

	/*
	 * Set the partition start/size info.
	 * Note: this info was already defaulted to be the disk
	 * definition partition.
	 */

	if (par != HDC_DEFPART) {
		if (geo->partition[par].length == 0)
			printf("hdc:  null partition\n");
		else {
			hu->partition[par].start  = geo->partition[par].start;
			hu->partition[par].length = geo->partition[par].length;
			io->i_boff = hu->partition[par].start;
		}
	}
	return( 1 ) ;
}

/*************************************************************************
*  Procedure:	hdstrategy
*
*  Description: The hdc strategy routine. This routine does the disk
*               reads/writes. If this is the format program, read/writes
*               are forced to be within the disk definition partition.
*
*  Returns:     The number of bytes transfered.
**************************************************************************/

hdstrategy(io,func)

register struct iob	*io ;	/* i/o block
				 */
long			func ;	/* i/o operation to perform
				 */
{
	mcb_type	*mcb;		/* mcb to send to the hdc           */
	hdunit_type	*hu;		/* disk unit information table      */
	hdctlr_type	*hc;		/* hdc ctlr information table       */
	long		err;		/* error code                       */
	long		sector;		/* sector number for i/o            */
	int		partstart;      /* block number of partition start  */
	int		partlen;        /* number of blocks in partition    */
	int		bytes;          /* number of bytes to transfer      */
	int		bus;		/* bus number	                    */
	int	        ctlr;		/* the controller number            */
	int	        drive;		/* the drive number                 */

	bus = io->i_bus;
	ctlr = io->i_ctlr;
	drive = io->i_drive;
	hu = &hdc_unit[drive][ctlr][bus];
	hc = &hdc_ctlr[ctlr][bus];

	/*
	 * Only the format program can access the disk definition tracks.
	 */

	if (io->i_part == HDC_DEFPART)
		if (!hu->format) {
			printf("hdc: partition 7 is protected\n");
			return 0;
		};

	/*
	 * Insure the transfer fits in the partition.
	 * Set and validate transfer size.
	 */

	partstart = hu->partition[io->i_part].start ;
	partlen = hu->partition[io->i_part].length ;
	if ( (io->i_bn < partstart) || (io->i_bn >= partstart+partlen) )
		return( 0 ) ;
	bytes = min( io->i_cc, DEV_BSIZE*(partstart+partlen-io->i_bn) );
	if (io->i_cc & 3) {
		printf("hdc:  i/o not a longword multiple\n");
		return 0;
	}

	/*
	 * Set up the mcb and send it to the hdc.
	 */

	mcb = &hc->mcb;
	sector = io->i_bn * HDC_SPB;
	mcb->command = (func == READ) ? HCMD_READ : HCMD_WRITE;
	mcb->drive = hu->slave;
	mcb->cyl = sector / (hu->sectors * hu->heads);
	mcb->head = (sector/hu->sectors) % hu->heads;
	mcb->sector = sector % hu->sectors;
	mcb->chain[0].ta  = (unsigned long) io->i_ma;
	mcb->chain[0].lwc = (bytes + 3) / 4;
	err = hdmcb(mcb, io);
	io->i_error = err;
	return (err ? 0 : bytes );
}

/*************************************************************************
*  Procedure:	hdioctl
*
*  Description: ioctl routine.
*
*  Returns:     0       no errors
*               non-0    error
**************************************************************************/

int
hdioctl(io, command, arg)

struct iob	*io ; 		/* i/o block.
				 */
int		command ;	/* The ioctl commmand.
				 */
int		arg ; 		/* Data. Format depends on ioctl.
				 */
{
	mcb_type	*mcb;
	hdunit_type	*hu;		/* disk unit information table      */
	hdctlr_type	*hc;		/* hdc ctlr information table       */
	register int	i;
	int		bus;		/* bus number	                    */
	int	        ctlr;		/* the controller number            */
	int	        drive;		/* the drive number                 */

	bus = io->i_bus;
	ctlr = io->i_ctlr;
	drive = io->i_drive;
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
		track = (struct formatop *) arg;
		mcb = &hc->mcb;
		for (i=0; i<hu->phys_sectors; i++)
			hu->phio_data[i] = 1;
		for (i=0; i<track->flaw_count; i++)
			hu->phio_data[track->flaw[i]]=0;
		mcb->command = HCMD_FORMAT;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = (unsigned long) hu->phio_data;
		mcb->chain[0].lwc = hu->phys_sectors;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return EIO;
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
			return 1;
		track = (struct formatop *) arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_CERTIFY;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = (unsigned long) hu->phio_data;
		mcb->chain[0].lwc = hu->phys_sectors;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return EIO;
		track->flaw_count = 0;
		for (i=0; i<hu->phys_sectors; i++) {
			if (track->flaw_count >= MAXVFLAW) break;
			if (hu->phio_data[i]==0) {
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
		track = (struct formatop *) arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_VERIFY;
		mcb->drive = hu->slave;
		mcb->chain[0].ta  = 0;
		mcb->chain[0].lwc = 0;
		mcb->cyl = track->cylinder;
		mcb->head = track->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return EIO;
		break;
	}

	case DSKIOCFORMATCTL: {

		/*
		 * This ioctl provides special format control.
		 * Currently the valid arguments are:
		 *
		 * arg= 0  disable formatting;
		 *
		 * arg= 1  enable formatting (allow privileged access);
		 *         formatting must not already be enabled;
		 *         For formatting, change to use partition 7.
		 */

		if (arg<0 || arg>1)
			return (1);
		if (arg==1) {
			if (hu->format) return (1);
			/* If not already formatting.... */
			hu->format = 1 ;
			io->i_part = HDC_DEFPART;
			io->i_boff = hu->partition[HDC_DEFPART].start;
		}
		else
			hu->format = 0 ;
		break;
	}

	case DSKIOCSTATUS: {

		/*
		 * Return info about the disk. Caller's parameter is a
		 * pointer to a dsk_status structure.
		 */

		register dsk_status *status;

		status = (dsk_status *) arg;
		status->id =		hu->id;
		status->drive_status =	0;
		status->rpm =		hu->rpm;
		status->bytes_per_sec = hu->bytes_per_sec;
		status->cylinders =	hu->cylinders;
		status->heads =		hu->heads;
		status->sectors =	hu->sectors;
		status->phys_cylinders= hu->phys_cylinders;
		status->phys_heads =	hu->phys_heads;
		status->phys_sectors =	hu->phys_sectors;
		status->diag_cyl =	hu->diag_cyl;
		status->diag_cylinders= hu->diag_cyl_count;
		status->def_cyl =	hu->def_cyl;
		status->def_cylinders =	hu->def_cyl_count;
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
		vflaw = (vflaw_type *) arg;
		mcb = &hc->mcb;
		mcb->command = HCMD_VENDOR;
		mcb->drive = hu->slave;
		mcb->chain[0].lwc = HDC_VDATA_SIZE;
		mcb->chain[0].ta  = (unsigned long) hu->phio_data;
		mcb->cyl = vflaw->cylinder;
		mcb->head = vflaw->head;
		mcb->sector = 0;
		if (hdmcb(mcb, io))
			return EIO;
		vendor = (struct flaw *) &hu->phio_data[0];
		for (i=0; i<MAXVFLAW; i++) {
			vflaw->flaw[i].offset = vendor[i].offset;
			vflaw->flaw[i].length = vendor[i].length;
		}
		break;
	}
	}
	return 0;
}

/*************************************************************************
*  Procedure:	hdmcb
*
*  Description: Internal routine used to send mcb's to the hdc.
*
*  Returns:     0          normal
*               non-zero   error occurred
**************************************************************************/

int
hdmcb(mcb, io)

register mcb_type	*mcb ;	/* mcb to send to the hdc		    */
register struct iob	*io ;	/* i/o block				    */

{
	master_mcb_type *master_mcb;	/* the hdc's master mcb             */
	hdctlr_type	*hc;		/* hdc ctlr information table       */
	hdc_regs_type	*ctlr_addr;	/* pointer to hdc i/o registers     */
	int		timeout;	/* used to timeout the mcb          */
	int		bus;		/* bus number	                    */
	int	        ctlr;		/* the controller number            */
	int		i,end;
	unsigned int	*ptr;

	bus = io->i_bus;
	ctlr = io->i_ctlr;
	hc = &hdc_ctlr[ctlr][bus];

	mcb->interrupt = FALSE;
	mcb->priority = 0;
	mcb->forw_phaddr = 0;
	mcb->context = 0;
	mcb->reserved[0] = 0;
	mcb->reserved[1] = 0;
	master_mcb = &hc->master_mcb;
	master_mcb->forw_phaddr = (long) &mcb->forw_phaddr;
	master_mcb->mcs = 0;
	master_mcb->interrupt = 0;
	master_mcb->reserve1  = 0;
	master_mcb->reserve2  = 0;
	master_mcb->context   = 0;
	master_mcb->mcl = MCL_IMMEDIATE;
	for (i=0;i<HDC_XSTAT_SIZE;i++) master_mcb->xstatus[i] = 0;
        ctlr_addr = hc->registers;
	HDREG(master_mcb_reg) = (unsigned long) master_mcb;
	timeout = 15000;
	while (TRUE) {
		DELAY(1000);
		mtpr(0,PADC);
		if ( (master_mcb->mcs & MCS_DONE) &&
			!(master_mcb->mcs & MCS_FATALERROR) ) return 0;
		timeout--;
		if ( timeout > 0   &&
			!(master_mcb->mcs & MCS_FATALERROR) ) continue;
		if ( master_mcb->mcs & MCS_FATALERROR )
			printf("hdc: controller fatal error\n");
		else
			printf("hdc: controller timed out\n");

		printf("mmcb: ");
		ptr = (unsigned int *) master_mcb;
		for (i=0;i<8;i++)
			printf(" %x",ptr[i]);
		for (i=7+HDC_XSTAT_SIZE; i>7; i--) {
			end = i;
			if (ptr[i] != 0) break;
		}
		for (i=8;i<=end;i++)
			printf(" %x",ptr[i]);
		printf("\n");

		printf("mcb:  ");
		ptr = (unsigned int *) &mcb->forw_phaddr;
		for (i=0; i<6; i++)
			printf(" %x",ptr[i]);
		for (i=6; i<72; i+=2) {
			printf("  %x %x", ptr[i], ptr[i+1]);
			if ( !(ptr[i] & 0x80000000)) break;
		}
		printf("\n");
		return(1);
	}
}
