/*
 *  Driver for HCX Disk Controller (HDC)
 *
 *	@(#)hd.c	7.2 (Berkeley) %G%
 */

#include <sys/types.h>
#include <ctype.h>
#include "../sys/param.h"
#include "../sys/buf.h"
#include "../sys/conf.h"
#include "../sys/dir.h"
#include "../sys/dk.h"
#include "../ml/mtpr.h"
#include "../sys/systm.h"
#include "../sys/vbavar.h"
#include "../sys/user.h"
#include "../sys/vmmac.h"
#include "../sys/uio.h"
#include "../sys/elog.h"
#include "../sys/iobuf.h"
#include "../sys/kernel.h"
#include "../sys/reboot.h"
#include "../sys/ioctl.h"
#define DSKGENDATA
#include "../sys/dsk.h"
#undef DSKGENDATA
#include "../sys/dskio.h"
#include "../sys/hdc.h"
#include "../sys/proc.h"

/*
 * External data.
 */

extern unsigned int		blkacty;	/* for error logging      */
extern hdc_ctlr_type		hdc_ctlrs[];	/* hdc controller info    */
extern hdc_unit_type		hdc_units[];	/* hdc unit info          */
extern struct vba_ctlr		*hdminfo[];	/* vba controller info    */
extern struct vba_device	*vddinfo[];	/* vba device info        */
extern struct iotime		vdstat[];	/* for disk activity info */
extern struct iobuf		vdtab[];	/* for disk activity info */
extern int			maxfree;	/* no. of blocks for dump */

/*
 * Procedure forward references.
 */

int  hdprobe();
int  hdslave();
int  hdstrategy();
int  hdattach();

/*
 * Driver structure.
 */

struct  vba_driver hddriver = {
	hdprobe,		/* handler probe routine        */
	hdslave,		/* handler slave routine        */
	hdattach,		/* handler attach routine       */
	0,			/* handler go routine           */
	0,			/*                              */
	"dsk",			/* name of the device           */
	vddinfo,		/* table of unit info           */
	"HDC Controller #",	/* name of the controller       */
	hdminfo,		/* table of ctlr info           */
	HDC_MID,		/* controller's module id       */
	0			/* no exclusive use of bdp's    */
};

#ifdef HDCLOG
/*************************************************************************
*  Procedure:	hdlog
*
*  Description: logs mcb's, master mcb's, etc.
*
*  Returns:
**************************************************************************/

#define ENT_SIZE 16
#define ENT_COUNT 256
static int  hdclog_index = 0;
static unsigned int  hdclog[ ENT_SIZE * ENT_COUNT ];

hdlog(ptr,id)
register unsigned int   *ptr;
register unsigned int   id;
{
	int i;

	hdclog[hdclog_index++] = id;
	hdclog[hdclog_index++] = time.tv_sec;
	hdclog[hdclog_index++] = time.tv_usec;
	for (i=3; i<ENT_SIZE; i++) {
		hdclog[hdclog_index++] = *ptr;
		ptr++;
	}
	if (hdclog_index >= ENT_SIZE * ENT_COUNT) hdclog_index=0;
}
#endif

/*************************************************************************
*  Procedure:	hdattach
*
*  Description: "hdattach" does device-dependent initialization of
* 		hdc drives. It is called during the configuration phase
*               of a reboot for each disk device on an hdc controller.
*               Note that most things get initialized in "hdslave",
*               because "slave" initializes what it needs to determine
*               whether the drive is ready (which turns out to be a lot).
*
*  Returns:
**************************************************************************/

hdattach(vba_unit)

register struct vba_device *vba_unit;	/* Pointer to vba drive info
					 */
{
	register hdc_unit_type	*hu;	/* hdc unit info
					 */
	register int		unit;	/* drive's unit# (0-31)
					 */
	unit = vba_unit->ui_unit;
	hu = &hdc_units[ unit ];

	/*
	 * Initialize the hdc unit information structure.
	 * A lot of this is done in "hdslave".
	 */

	hu->spc = hu->heads * hu->sectors;

	/*
	 * bytes per second:
	 *      (number of sectors per track) * (bytes per sector) * rpm / 60
 	 */

	dk_bps[unit] = hu->sectors * BPS * hu->rpm / 60;
}

/*************************************************************************
*  Procedure:	hddump
*
*  Description: Dump system memory to disk. The hdc controller is reset.
*               After this call, queued operations on this hdc are no
*               longer possible until the next reboot.
*
*  Returns:     ENXIO    the dump was truncated for some reason.
*               EIO      there were controller problems
*               0        normal
**************************************************************************/

int
hddump(dev)

int	dev;		/* the major/minor device number.
			 */
{
	register hdc_unit_type	*hu;		/* hdc unit info            */
	register hdc_ctlr_type	*hc;		/* hdc controller info      */
	register mcb_type	*mcb;		/* hdc controller info      */
	register int		current_block;	/* next disk block to write */
	register int		block_count;	/* #blocks to dump total    */
	register int		blocks;		/* #blocks to dump at a time*/
	register int		mem_addr;	/* memory address to dump   */
	int			sector;		/* sector to write to       */
	int			par;		/* disk partition number    */
	int			parlen;		/* disk partition # blocks  */
	int			dump_short;	/* TRUE= dump was truncated */
	int			chn;		/* temporary data chain no. */
	int			bc;		/* temporary byte count     */


	mem_addr = 0;
	dump_short = FALSE;
	par = HDC_PARTITION(dev);
	hu = &hdc_units[ HDC_UNIT(dev) ];
	hc = &hdc_ctlrs[hu->ctlr];
	mcb = &hu->phio_mcb;
	parlen = hu->partition[par].length;
	printf("\nhdc: resetting controller #%d.\n", hc->ctlr);
	HDC_REGISTER(soft_reset_reg) = 0;
	DELAY(1000000);
	mtpr(PADC, 0);

	/*
	 * If the drive has not been initialized yet, abort the dump.
	 * Set dump limits. The dump must fit in the partition.
	 */

	if (hu->sectors <= 0 || hu->heads <= 0 || hu->cylinders <= 0 ) {
		printf("\nhdc: dump device is not initialized - no dump!\n");
		return EIO;
	}
	block_count = dumpsize;
	if ((dumplo + block_count) > parlen) {
		block_count = parlen - dumplo;
		dumpsize = block_count;  /* let savecore know */
		printf("\nhdc: only dumping first %dmb of memory!\n",
		    block_count/1024);
		dump_short = TRUE;
	}
	current_block = hu->partition[par].start + dumplo;

	/*
	 * Dump memory to disk. For each disk transfer, fill in the
	 * mcb with information describing the transfer, then send
	 * the mcb to the hdc controller.
	 */

	while (block_count > 0) {
		blocks = MIN(block_count, HDC_DUMPSIZE);
		sector = HDC_SPB * current_block;
		mcb->command   = HCMD_WRITE;
		mcb->cyl = sector/hu->spc;
		mcb->head = (sector/hu->sectors) % hu->heads;
		mcb->sector = sector % hu->sectors;
	        chn = 0;
	        bc = blocks * DEV_BSIZE;
	        while (bc > 0) {
	        	mcb->chain[chn].ta  = mem_addr;
	        	mcb->chain[chn].lwc = (bc > HDC_MAXBC) ?
        			(LWC_DATA_CHAIN | (HDC_MAXBC/4)) : bc/4;
	        	mem_addr += ((bc > HDC_MAXBC) ? HDC_MAXBC : bc);
	        	chn++;
	        	bc -= HDC_MAXBC;
		}
		if (!hdimcb(hu,mcb))
			return EIO;
		block_count -= blocks;
		current_block += blocks;
	}
	return (dump_short ? ENXIO : 0);
}

/*************************************************************************
*  Procedure:	hddumpmcb
*
*  Description: Dumps a single mcb to the console - up to the last
*               active data chain lword.
*
*  Returns:
**************************************************************************/

hddumpmcb(mcb)

register mcb_type *mcb; /* the mcb pointer
			 */
{
	unsigned int *ptr,i;

	printf("mcb: ");
	ptr = (unsigned int *) &mcb->forw_phaddr;
	for (i=0; i<6; i++)
		printf(" %x",ptr[i]);
	for (i=6; i<72; i+=2) {
		printf("  %x %x", ptr[i], ptr[i+1]);
		if ( !(ptr[i] & 0x80000000)) break;
	}
	printf("\n");
}

/*************************************************************************
*  Procedure:	hddumpmmcb
*
*  Description: dumps the master mcb on the console up to the
*               last non-zero byte of the extended status.
*
*  Returns:
**************************************************************************/

hddumpmmcb(master)

register master_mcb_type *master; /* the master mcb pointer
				   */
{
	unsigned int *ptr,i,end;

	printf("mmcb:  ");
	ptr = (unsigned int *) master;
	for (i=0;i<8;i++)
		printf("%x ",ptr[i]);
	for (i=7+HDC_XSTAT_SIZE; i>7; i--) {
		end = i;
		if (ptr[i] != 0) break;
	}
	for (i=8;i<=end;i++)
		printf(" %x",ptr[i]);
	printf("\n");
};

/*************************************************************************
*  Procedure:	hdimcb
*
*  Description: "hdc immediate mcb" sends an mcb to the hdc and returns
*               when the hdc has completed the operation (polled io).
*               "hdimcb" is called during system configuration or
*               when the system is being dumped after a fatal error.
*
*  Entry:       o  There is no active process.
*
*               o  "hdimcb" cannot be called from interrupt level.
*
*               o  There can be no queued operations pending; i.e.
*                  this routine assumes exclusive use of the hdc.
*                  Note: a soft reset will terminate queued operations.
*
*  Returns:     Returns FALSE if a controller error occurred.
**************************************************************************/

int
hdimcb(hu,mcb)

register hdc_unit_type	*hu;		/* unit information
					 */
register mcb_type	*mcb;		/* mcb to send to the hdc
					 */
{
	register hdc_ctlr_type	 *hc;		/* controller information   */
	register master_mcb_type *master;	/* the hdc's master mcb     */
	register int		 timeout;	/* used to timeout the mcb  */
	register int		 ctlr;		/* controller number        */
	int	 i,ok;
	unsigned int *ptr;


	ok = TRUE;
	ctlr = hu->ctlr;
	hc = &hdc_ctlrs[ctlr];
	master = &hc->master_mcb;

	/*
	 * Complete the setup of the mcb and master mcb.
	 */

	mcb->priority   = 0;
	mcb->interrupt  = FALSE;
	mcb->drive      = hu->slave;
	mcb->forw_phaddr= 0;
	mcb->context	= 0;
	mcb->reserved[0]= 0;
	mcb->reserved[1]= 0;
	master->forw_phaddr = (long) vtoph(0,&mcb->forw_phaddr);
	master->mcs = 0;
	master->reserve1 = 0;
	master->reserve2 = 0;
	master->context = 0;
	master->cmcb_phaddr = 0;
	master->mcl = MCL_IMMEDIATE;
	bzero( (caddr_t)&master->xstatus[0], HDC_XSTAT_SIZE );

	/*
	 * Tell hdc to xqt the mcb; wait for completion.
	 * If a controller error or timeout occurs, print
	 * out the mcb and master mcb on the console.
	 */

	HDC_REGISTER(master_mcb_reg) = hc->master_phaddr;
	timeout = 15000;
	while (TRUE) {
		DELAY(1000);
		mtpr(PADC, 0);
		if ( (master->mcs & MCS_DONE) &&
		    !(master->mcs & MCS_FATALERROR ) ) break;
		timeout--;
		if ( timeout > 0   &&
			!(master->mcs & MCS_FATALERROR) ) continue;
		if ( master->mcs & MCS_FATALERROR )
			printf("hdc: controller %d fatal error\n",ctlr);
		else
			printf("hdc: controller %d timed out\n",ctlr);
	        hddumpmcb(mcb);
	        hddumpmmcb(master);
		ok = FALSE;
		break;
	}
	master->mcl = MCL_QUEUED;
	return(ok);
}

/*************************************************************************
*  Procedure:	hdintr
*
*  Description: The hdc interrupt routine.
*
*  Returns:
**************************************************************************/

hdintr(ctlr)

int	ctlr;		/* the hdc controller number.
			 */
{
	register master_mcb_type *master;	/* master mcb for this hdc  */
	register mcb_type	 *mcb;		/* the mcb just completed   */
	register struct buf	 *bp;		/* buf for the completed mcb*/
	register hdc_ctlr_type   *hc;		/* info for this controller */
	register struct iobuf    *iobp;		/* iobuf for this unit      */
	register int		 unit;		/* unit# of the hdc drive   */
	register int		 i;		/* temporary                */


	hc = &hdc_ctlrs[ctlr];
	master = &hc->master_mcb;
	uncache( &master->mcs );
	uncache( &master->context );
#ifdef HDCLOG
	hdlog(master,1 + 16*hc->ctlr);
#endif
	if ( !(master->mcs & MCS_DONE) ) {
	        printf("\nhdc: spurious interrupt from controller #%d\n",ctlr);
	        return;
	}
	mcb = (mcb_type *) master->context;
	bp = mcb->buf_ptr;
	unit = HDC_UNIT(bp->b_dev);
	iobp = &vdtab[unit];

	/*
	 * Error log and system activity.
	 *
	 * Turn off the activity bit for this device.
	 * Record the time required to process the buf.
	 * If there is no more activity on this unit, record the
	 * amount of time that the unit was active.
	 * Update dkprf and lastcyl for "sadp".
	 */

	blkacty &= ~(1 << major(bp->b_dev));
	if (iobp->b_active) {
		vdstat[unit].io_resp += (time.tv_sec - bp->b_start);
		if (--iobp->b_active == 0)
			vdstat[unit].io_act += (time.tv_sec - iobp->io_start);
	}
	i = mcb->cyl;
	dkprf[unit][i >> 3]++;
	i -= lastcyl[unit];
	if (i < 0) i = -i;
	skprf[unit][i >> 3]++;
	lastcyl[unit] = mcb->cyl;
	dk_busy &= ~(1 << unit);
	dk_seek[unit]++;
	dk_xfer[unit]++;

	/*
	 * If there are no free mcb's, wake up anyone that might
	 * be waiting for one.  Remove the completed mcb from the
	 * queue of active mcb's and add it to the free-mcb queue.
	 */

	if (hc->forw_free == (mcb_type *)&hc->forw_free)
		wakeup(hc);
	remque(mcb);
	insque(mcb,&hc->forw_free);

	/*
	 * If there was a fatal error, dump the mcb and master mcb on the
	 * console, then halt if the system was booted with the debug option.
	 *
	 * Record fatal and soft errors in the error log.
	 */

	bp->b_resid = 0;
	if (master->mcs & (MCS_SOFTERROR | MCS_FATALERROR) ) {
		mtpr(P1DC, (caddr_t)master);
		mtpr(P1DC, (caddr_t)&master->xstatus[HDC_XSTAT_SIZE]-1);
		if (master->mcs & MCS_FATALERROR) {
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
			harderr(bp,"hdc");
			printf("\nhdc:  fatal error on controller #%d\n",ctlr);
			hddumpmmcb(master);
			hddumpmcb(mcb);
			if (boothowto & RB_DEBUG) asm("halt");
		};
		vdstat[unit].ios.io_misc++ ;
		iobp->io_erec = 0;
		iobp->io_addr = (caddr_t) hc->registers;
		iobp->io_stp  = &vdstat[unit].ios;
		iobp->io_nreg = HDC_XSTAT_SIZE;
		for (i=HDC_XSTAT_SIZE-1; i>0; i--) {
			if (master->xstatus[i] != 0) break;
			iobp->io_nreg--;
		}
		iobp->b_actf  = bp;
		iobp->b_dev   = bp->b_dev;
		fmtberr( iobp, mcb->cyl, &master->xstatus[0] );
		logberr(iobp, master->mcs & MCS_FATALERROR);
		bzero( (caddr_t)&master->xstatus[0], HDC_XSTAT_SIZE );
	}

	/*
	 * If there are any waiting mcb's, move them to the active queue.
	 * Physically link the new mcb's from the master mcb.
	 */

	master->forw_phaddr = 0;
next:   mcb = hc->forw_wait;
	remque(mcb);
	asm(" bvs done");
	insque(mcb,&hc->forw_active);
	mcb->forw_phaddr = master->forw_phaddr;
#ifdef HDCLOG
	hdlog(mcb,2 + 16*hc->ctlr);
#endif
	master->forw_phaddr = mcb->mcb_phaddr;
	goto next;
done:   asm("done:");

	/*
	 * If there are any mcb's active, initialize the master mcb
	 * and tell the hdc to continue queued operation.
	 * New mcb's (if any) are linked off of "forw_phaddr".
	 */

	if (hc->forw_active != (mcb_type *) &hc->forw_active) {
		master->mcs = 0;
#ifdef HDCLOG
		hdlog(master,3 + 16*hc->ctlr);
#endif
		HDC_REGISTER(master_mcb_reg)= hc->master_phaddr;
	}

	/*
	 * Return the buf for the completed operation.
	 */

	iodone(bp);
	return;
}

/*************************************************************************
*  Procedure:	hdioctl
*
*  Description: Character device ioctl routine.
*
*  Returns:     EACCES    formatting is active on the drive
*                         (or) function is valid only for the format program
*                         (or) formatting ioctl's must be done on partition 7
*               EIO       controller error occurred
*               ENXIO     invalid parameter value
*               0         normal
**************************************************************************/

int
hdioctl(dev, command, arg, flag)

dev_t		dev ; 		/* Device type. Major/minor dev#.
				 */
int		command ;	/* The ioctl commmand.
				 */
int		*arg ; 		/* Data. Format depends on ioctl.
				 */
int		flag ;		/* Not used.
				 */
{
	register hdc_unit_type	*hu;		/* unit information        */
	int			formatok;	/* TRUE= it's ok to format */
	register int		i;

	hu = &hdc_units[ HDC_UNIT(dev) ];
	formatok = ( HDC_PARTITION(dev)==7  &&  hu->format );
	switch (command) {

	case DSKIOCFORMAT: {

		/*
		 * Format a disk track. The received argument is a pointer
		 * to a "formatop" structure describing the track to format.
		 *
		 * Set up a buffer with each longword corresponding to a
		 * sector on the track; a 1 means no flaw, a 0 means a flaw.
		 * Call hdphysio to send the data from the phio_data buffer
		 * to the hdc to format the track.
		 */

		register struct formatop *track;

		if (!formatok) return EACCES;
		track = (struct formatop *) arg;
		for (i=0; i<hu->phys_sectors; i++)
			hu->phio_data[i] = 1;
		for (i=0; i<track->flaw_count; i++)
			hu->phio_data[track->flaw[i]] = 0;
		if (!hdphysio(
			dev,
			HCMD_FORMAT,
			track->cylinder,
			track->head,
			0,
			hu->phio_data,
			hu->phys_sectors * 4) )
			   return EIO;
		break;
	}

	case DSKIOCCERTIFY: {

		/*
		 * Certify a disk track. The received argument is a pointer
		 * to a "formatop" structure describing the track to certify.
		 *
		 * Call hdphysio to read data into the phio_data buffer.
		 * The controller returns data in which each longword
		 * corresponds to a sector on the track; a 1 means no flaw,
		 * a 0 means a flaw.
		 */

		register struct formatop *track;

		if (!formatok) return EACCES;
		track = (struct formatop *) arg;
		if (!hdphysio(
			dev,
			HCMD_CERTIFY,
			track->cylinder,
			track->head,
			0,
			hu->phio_data,
			hu->phys_sectors * 4) )
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

		if (!formatok) return EACCES;
		track = (struct formatop *) arg;
		if (!hdphysio(
			dev,
			HCMD_VERIFY,
			track->cylinder,
			track->head,
			0,
			0,
			0) )
				return EIO;
		break;
	}

	case DSKIOCFORMATCTL: {

		/*
		 * This ioctl provides special format control.
		 *
		 * Currently the valid arguments are:
		 * arg= 0  disable formatting;
		 * arg= 1  enable formatting (allow privileged access);
		 *
		 * Partition must be the disk definition tracks of
		 * the raw device.
		 */

	        if (HDC_PARTITION(dev) != HDC_DEFPART )
	                return EACCES;
		switch (*arg) {

	        case 0: hu->format = FALSE;
	                break;

	        case 1: if (hu->format)
				return EACCES;
	        	hu->format = TRUE;
	                break;

	        default: return ENXIO;
		}
		break;
	}

	case DSKIOCGEOMETRY: {

		/*
		 * Return info about disk geometry (partitions).
		 * Caller's parameter is a pointer to a geometry
		 * status structure.
		 */

		register geometry_status *geo_status;

		geo_status = (geometry_status *) arg;
		for (i=0; i<GB_MAXPART; i++) {
			geo_status->partition[i].start = hu->partition[i].start;
			geo_status->partition[i].length=hu->partition[i].length;
		}
		break;
	}

	case DSKIOCSETGEOMETRY: {

		/*
		 * Set new geometry - new partition sizes.
		 * Caller must have formatting privilege.
		 * Caller's parameter is a pointer to a geometry
		 * status structure containing the new geometries.
		 * The disk definition partition cannot be changed.
		 */

		register geometry_status *geo_status;

		if (!formatok) return EACCES;
		geo_status = (geometry_status *) arg;
		for (i=0; i<GB_MAXPART; i++) {
			if (i==HDC_DEFPART) continue;
			hu->partition[i].start = geo_status->partition[i].start;
			hu->partition[i].length=geo_status->partition[i].length;
		}
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
		status->rpm =		hu->rpm;
		status->bytes_per_sec=	hu->bytes_per_sec;
		status->cylinders =	hu->cylinders;
		status->heads =		hu->heads;
		status->sectors =	hu->sectors;
		status->phys_cylinders=	hu->phys_cylinders;
		status->phys_heads =	hu->phys_heads;
		status->phys_sectors =	hu->phys_sectors;
		status->diag_cyl =	hu->diag_cyl;
		status->diag_cylinders=	hu->diag_cyl_count;
		status->def_cyl =	hu->def_cyl;
		status->def_cylinders =	hu->def_cyl_count;
		break;
	}

	case DSKIOCVENDORFLAW: {

		/*
		 * Return vendor flaw info.
		 *
		 * Read in the vendor data from relative sector 0 of
		 * the track to the phio_data buffer; then copy the
		 * vendor flaw data to the caller's buffer.
		 */

		register vflaw_type *vflaw;
		register struct flaw *vendor;

		if (!formatok) return EACCES;
		vflaw = (vflaw_type *) arg;
		if (!hdphysio(
			dev,
			HCMD_VENDOR,
			vflaw->cylinder,
			vflaw->head,
			0,
			hu->phio_buf,
			HDC_VDATA_SIZE << 2 ))
 				return EIO;
		vendor = (struct flaw *) &hu->phio_data[0];
		for (i=0; i<MAXVFLAW; i++) {
			vflaw->flaw[i].offset = vendor[i].offset;
			vflaw->flaw[i].length = vendor[i].length;
		}
		break;
	}

	default: return ENXIO;

	}
	return 0;
}

/*************************************************************************
*  Procedure:	hdopen
*
*  Description: The character device and block device open routine.
*
*  Returns:     ENXIO     the partition or device isn't defined
*               EACCES    Formatting is active on this drive
*               0         normal
**************************************************************************/

int
hdopen(dev, flag)

dev_t		dev ; 		/* Device type. Major/minor dev#.
				 */
int		flag ;		/* Not used.
				 */
{
	register int			unit;		/* hdc unit#  (0-31)*/
	register int			par;		/* partition# (0-7) */
	register struct vba_device	*vba_unit;	/* vba unit info    */
	register hdc_unit_type		*hu;		/* hdc unit info    */


	unit = HDC_UNIT(dev);
	par = HDC_PARTITION(dev);
	vba_unit = vddinfo[unit];
	hu = &hdc_units[unit];
	if ( !vba_unit->ui_alive || hu->partition[par].length == 0)
	      return ENXIO;
	if (hu->format)
	      return EACCES;
	vdtab[unit].io_stp  = &vdstat[unit].ios;
	return 0;
}

/*************************************************************************
*  Procedure:	hdphysio
*
*  Description: "hdphysio" does the physical i/o initiated by this
*               handler. It does the things which "physio" does for
*               raw read/writes; i.e. it provides an interface to the
*               hdstrategy routine.
*
*               hdphysio assumes that it has exclusive access to the
*               drive; it uses the drive's phio buf.
*
*  Returns:     FALSE     an i/o error occurred.
*               0         normal; data is in phio_data if read was done
**************************************************************************/

int
hdphysio(dev,command,cylinder,head,sector,ta,bc)

dev_t	dev;			/*  major/minor device number
				 */
int	command;		/*  the hdc command to execute
				 */
int	cylinder;		/*  disk cylinder address
				 */
int	head;			/*  disk head address
				 */
int	sector;			/*  disk sector address
				 */
int	ta;			/*  memory transfer address
				 */
int	bc;			/*  byte count
				 */
{
	register struct buf	*bp;	/* buf structure built here */
	hdc_unit_type		*hu;	/* hdc device unit info     */
	int			s;	/* processor level save     */

	hu = &hdc_units[ HDC_UNIT(dev) ];
	bp = (struct buf *) &hu->phio_buf;
	bp->b_error = 0;
	bp->b_proc = u.u_procp;
	bp->b_un.b_addr = (caddr_t) ta;
	bp->b_flags = B_BUSY | B_PHYS | B_READ | B_LOCALIO;
	bp->b_dev = dev;
	bp->b_blkno = 0;
	bp->b_hdccommand = command;
	bp->b_cyl = cylinder;
	bp->b_head = head;
	bp->b_sector = sector;
	bp->b_bcount = bc;
	hdstrategy(bp);
	s = spl8();
	while ((bp->b_flags & B_DONE) == 0)
		slumber((caddr_t)bp, 0, iocomboost);
	splx(s);
	bp->b_flags &= ~(B_BUSY | B_PHYS | B_WANTED | B_LOCALIO);
	if (bp->b_error != 0)
		return FALSE;
	return TRUE;
}

/*************************************************************************
*  Procedure:	hdprobe
*
*  Description: "hdprobe" verifies that an hdc controller is really
*               there and then initializes the controller. It is called
*		during the configuration phase of a reboot for each
*		hdc controller in the configuration.
*
*  Returns:	TRUE means the controller is ready.
**************************************************************************/

int
hdprobe(vba_ctlr)

register struct vba_ctlr	*vba_ctlr;	/* vba controller information
						 */
{
	register hdc_ctlr_type	*hc;		/* hdc controller info      */
	register hdc_mid_type	*id;		/* returned module id word  */
	register int		ctlr;		/* the controller number    */
	register int		i;		/* temporary                */
	mcb_type		*mcb;		/* temporary mcb pointer    */
	extern	int		Xhdintr0, Xhdintr1, Xhdintr2, Xhdintr3,
				Xhdintr4, Xhdintr5, Xhdintr6, Xhdintr7 ;
	static  int		hd_proc[] = {
					(int)& Xhdintr0, (int)& Xhdintr1,
					(int)& Xhdintr2, (int)& Xhdintr3,
					(int)& Xhdintr4, (int)& Xhdintr5,
					(int)& Xhdintr6, (int)& Xhdintr7
	} ;


	ctlr = vba_ctlr->um_ctlr;
	hc = &hdc_ctlrs[ctlr];
	/*
	 * Initialize the hdc controller structure.
	 * Initially all mcb's are in the free-mcb list.
	 * The interrupt acknowledge word is the vector offset
	 * for this controller's interrupts.
	 */

	hc->ctlr = ctlr;
	hc->registers = (hdc_regs_type *) vba_ctlr->um_addr;
	id = &hc->mid;
	if (badaddr(&hc->registers->module_id_reg,4,vtoph(0,id)))
                return FALSE;
	hc->forw_active = (mcb_type *) &hc->forw_active;
	hc->back_active = (mcb_type *) &hc->forw_active;
	hc->forw_wait   = (mcb_type *) &hc->forw_wait;
	hc->back_wait   = (mcb_type *) &hc->forw_wait;
	hc->forw_free   = (mcb_type *) &hc->forw_free;
	hc->back_free   = (mcb_type *) &hc->forw_free;
	for (i=HDC_MAXMCBS-1; i>=0; i--) {
		mcb = &hc->mcbs[i];
		mcb->mcb_phaddr = vtoph( 0, &mcb->forw_phaddr);
		insque( mcb, &hc->forw_free);
	}
	vba_ctlr -> um_ivct = get_ivct( 0, 1 ) ;
	if ( vba_ctlr -> um_ivct == (-1) )
		return FALSE ;
	init_ivct( vba_ctlr -> um_ivct, hd_proc[ vba_ctlr -> um_ctlr ] ) ;
	hc->master_mcb.interrupt = vba_ctlr -> um_ivct ;
	hc->master_phaddr = (u_long) vtoph( 0, &hc->master_mcb) ;

	/*
	 * Read in the hdc module id word.
	 */

	HDC_REGISTER(module_id_reg) = (unsigned long) vtoph(0,id);
	DELAY(10000);
	mtpr(PADC, 0);

	/*
	 * hdc's are reset and downloaded by the console processor.
	 * Check the module id; the controller is bad if:
	 * 1) it is not an hdc;
	 * 2) the hdc's writeable control store is not loaded;
	 * 3) the hdc failed the functional integrity test;
	 */

	printf("hdc controller %d module id is %x\n", ctlr, *id);
	if (id->module_id != (unsigned char) HDC_MID) {
		printf("hdc:  controller #%d bad module id.\n",ctlr);
		return FALSE;
	}
	if (id->code_rev == (unsigned char) 0xFF ) {
		printf("hdc:  controller #%d micro-code not loaded.\n",ctlr);
		return FALSE;
	}
	if (id->fit != (unsigned char) 0xFF ) {
		printf("hdc:  controller #%d FIT test failed.\n",ctlr);
		return FALSE;
	}
	/*
	 * Reset the hdc in case it still has queued mcb's.
	 */

	HDC_REGISTER(soft_reset_reg) = 0;
	DELAY(1000000);
	return TRUE;
}

/*************************************************************************
*  Procedure:	hdsize
*
*  Description: Return the partition size for a specified partition.
*
*  Returns:     Partition size in blocks.
*               -1 means the device isn't there
**************************************************************************/

int
hdsize(dev)

register dev_t	dev ; 		/* Major/minor dev#.
				 */
{
	int			unit;		/* hdc unit#  (0-31)  */
	int			par;		/* partition# (0-7)   */
	struct vba_device	*vba_unit;	/* vba unit info      */
	hdc_unit_type		*hu;		/* hdc unit info      */

	unit = HDC_UNIT(dev);
	par = HDC_PARTITION(dev);
	vba_unit = vddinfo[unit];
	hu = &hdc_units[unit];
	if (vba_unit==0 || !vba_unit->ui_alive) return -1;
	return (hu->partition[par].length);
}

/*************************************************************************
*  Procedure:	hdslave
*
*  Description: "hdslave" verifies that an hdc drive is really there.
*               It is called during the configuration phase of a reboot
*               for each drive on an hdc.
*
*               Note: a lot of device initialization is done here, which
*               should normally be done in hdattach; however, it is
*               done here since it is info needed to determine whether
*               the drive is really there and is functional.
*
*  Returns:	TRUE means the drive is there.
**************************************************************************/

int
hdslave(vba_unit,regs)

struct vba_device	*vba_unit;		/* vba drive info
						 */
hdc_regs_type		*regs;			/* hdc io address (not used)
						 */
{
	register hdc_ctlr_type	*hc;		/* hdc ctlr info            */
	register hdc_unit_type	*hu;		/* hdc unit info            */
	register mcb_type	*mcb;		/* mcb to send to the hdc   */
	register int		unit;		/* hdc unit# (0-31)         */
	register int		ctlr;		/* hdc ctlr# (0-15)         */
	register int		i;		/* temp                     */
	geometry_block		*geo;		/* ptr to the geometry block*/
	drive_stat_type		*drive_status;	/* status returned by hdc   */

	ctlr = vba_unit->ui_ctlr;
	hc = &hdc_ctlrs[ctlr];
	unit = vba_unit->ui_unit;
	hu = &hdc_units[unit];
	mcb = (mcb_type *) &hu->phio_mcb;

	/*
	 * Initialize things in the hdc unit structure which are used
	 * by this routine. The rest is initialized by hdattach.
	 */

	hu->ctlr = ctlr;
	hu->unit = unit;
	hu->slave = vba_unit->ui_slave;

	/*
	 * Read the drive status and keep a permanent copy of the
	 * info in the hdc unit structure.
	 */

	drive_status = (drive_stat_type *) hu->phio_data;
	mcb->command = HCMD_STATUS;
	mcb->chain[0].lwc = sizeof(drive_stat_type) / 4;
	mcb->chain[0].ta  = (u_long) vtoph(0,drive_status);
	if (!hdimcb(hu,mcb))
		return FALSE;
	hu->id =		drive_status->id;
	hu->cylinders =		drive_status->max_cyl+1;
	hu->heads =		drive_status->max_head+1;
	hu->sectors =		drive_status->max_sector+1;
	hu->phys_cylinders =	drive_status->max_phys_cyl+1;
	hu->phys_heads =	drive_status->max_phys_head+1;
	hu->phys_sectors =	drive_status->max_phys_sector+1;
	hu->def_cyl =		drive_status->def_cyl;
	hu->def_cyl_count =	drive_status->def_cyl_count;
	hu->diag_cyl =		drive_status->diag_cyl;
	hu->diag_cyl_count =	drive_status->diag_cyl_count;
	hu->bytes_per_sec =	drive_status->bytes_per_sec;
	hu->rpm =		drive_status->rpm;
	hu->partition[HDC_DEFPART].start  =
		hu->def_cyl * hu->sectors * hu->heads / HDC_SPB;
	hu->partition[HDC_DEFPART].length =
		hu->def_cyl_count * hu->sectors * hu->heads / HDC_SPB;

	/*
	 * Report the drive down if anything in the drive status
	 * looks bad.  If the drive is offline and it is not on
	 * cylinder, then the drive is not there.
	 * If there is a fault condition, the hdc will try to clear
	 * it when we read the geometry block.
	 */

	if (drive_status->drs & DRS_FAULT)
		printf("hdc: clearing fault on unit #%d.\n",unit);
	if ( !(drive_status->drs  &  DRS_ONLINE)) {
		if ( drive_status->drs  &  DRS_ON_CYLINDER )
			printf("hdc: unit #%d is not online.\n",unit);
	        return FALSE;
	}

	/*
	 * Read the geometry block from the start of the drive
	 * definition cylinder, validate it (must have the correct
	 * header and checksum), and set partition starts and sizes
	 * (definition partition has already been set above).
	 */

 	geo = (geometry_block *) hu->phio_data;
	mcb->command      = HCMD_READ;
	mcb->cyl          = hu->def_cyl;
	mcb->head         = 0;
	mcb->sector       = 0;
	mcb->chain[0].lwc = sizeof(geometry_sector) / 4;
	mcb->chain[0].ta  = (unsigned long) vtoph(0,geo);
	if (!hdimcb(hu,mcb))
		goto badgeo;
 	if ( geo->version > 64000  ||  geo->version < 0 ) {
 		printf("hdc: bad geometry block version# on unit #%d\n",unit);
		goto badgeo;
	}
	if (strcmp(&geo->id[0],GB_ID) != 0) {
		printf("hdc: bad geometry block header on unit #%d\n",unit);
		goto badgeo;
	}
 	GB_CHECKSUM( geo, i );
 	if ( ((geometry_sector *)geo)->checksum != i) {
 		printf("hdc: bad geometry block checksum on unit #%d\n",unit);
 		goto badgeo;
	}
	for (i=0; i<GB_MAXPART; i++) {
		if (i==HDC_DEFPART) continue;
		hu->partition[i].start  = geo->partition[i].start;
		hu->partition[i].length = geo->partition[i].length;
	}
	return TRUE;

	/*
	 * If the geometry block is bad, return ok status so that
	 * the disk can be formatted etc, but zero the partitions
	 * so that no one except "format" can read/write the disk.
	 */

badgeo: for (i=0; i<GB_MAXPART; i++) {
		if (i==HDC_DEFPART) continue;
		hu->partition[i].start  = 0;
		hu->partition[i].length = 0;
	}
	return TRUE;
}

/*************************************************************************
*  Procedure:	hdstrategy
*
*  Description: The hdc strategy routine. It is called by the kernel
*               to do a disk operation  ('physio' if raw i/o, the block
*               i/o routines if block i/o); i.e. this is the point where
*               raw i/o and block i/o merge. This routine is also called
*               internally by this handler to do misc disk operations.
*
*  Returns:
**************************************************************************/

hdstrategy(bp)

register struct buf *bp;	/* This buf structure contains info
       				 * describing the requested disk xfer.
				 */
{
	register hdc_unit_type	 *hu;	   /* hdc device unit info     */
	register mcb_type	 *mcb;	   /* the mcb built here       */
	register int		 vaddr;    /* virtual address of data  */
	hdc_ctlr_type		 *hc;	   /* hdc controller info      */
	int			 sector;   /* absolute sector number   */
	int			 unit;	   /* minor device unit#       */
	int			 par;	   /* disk partition number    */
	int			 blocks;   /* number of blocks to xfer */
	int			 priority; /* processor level save     */
	int			 bytes;	   /* bytecount requested      */
	int			 i;	   /* temporary                */

	/*
	 * Initialize pointers and data.
	 */

	unit = HDC_UNIT(bp->b_dev);
	par = HDC_PARTITION(bp->b_dev);
	hu = &hdc_units[unit];
	hc = &hdc_ctlrs[hu->ctlr];
	bytes = bp->b_bcount;
	vaddr = (int) bp->b_un.b_addr;

	/*
	 * Make some preliminary checks of the i/o request.
	 * Terminate the i/o immediately if: the request is for zero
	 * bytes or more than 32k bytes; the xfer does not start or
	 * end on a longword boundary.
	 * "format" sometimes requires bytes=0; e.g. for verify and
	 * format ioctls.
	 */

	if (bytes==0 || bytes>32*1024)
		if (!hu->format) goto enxio;
	if ( (bytes&3) || (vaddr&3) )
		goto efault;

	/*
	 * Round up requested byte count to a multiple of the block size.
	 * If the transfer would exceed the end of the partition,
	 * truncate the byte count at the partition boundary (except that
	 * the format program is allowed to access the entire disk).
	 * Determine absolute sector number of the start of the transfer
	 * (requested start plus the start of the partition).
	 */

	{
		register int par_start;  /* partition start blk */
		register int par_length; /* partition blk count */

		par_start = hu->partition[par].start;
		par_length= hu->partition[par].length;
		blocks = (bytes + DEV_BSIZE - 1) >> DEV_BSHIFT;
		if ( par_length < (bp->b_blkno + blocks) )
			if ( !hu->format) {
				blocks = par_length - bp->b_blkno;
				if(blocks <= 0) goto enxio;
				bytes = blocks * DEV_BSIZE;
			}
		sector = HDC_SPB * (bp->b_blkno + par_start);
	}

	/*
	 * Insure that nobody except the format program writes to
	 * the drive definition tracks in partition 7.
	 * Note: they may access other tracks in partition 7
	 * (i.e. diagnostic tracks).
	 */

	if (par==HDC_DEFPART)
		if (!hu->format && !(bp->b_flags & B_READ))
		{
			register int defs;  /* definition cyl start */
			register int defe;  /* (def cylinder end)+1 */

			defs = hu->def_cyl * hu->spc;
			defe = defs + hu->def_cyl_count * hu->spc;
	        	if (sector < defe && (sector + blocks * HDC_SPB) > defs)
				goto eacces;
		}

	/*
	 * Get a free mcb. Wait if no mcb's are available
	 */

	priority = spl7();
get:	mcb = hc->forw_free;
	remque(mcb);
	asm(" bvc got");
	slumber(hc, 0, iocomboost);
	goto get;
got:    asm("got:");
	splx(priority);

	/*
	 * Fill in the mcb with information about the xfer.
	 *
	 * Currently everything is given equal priority.
	 * Keep a pointer to the buf associated with the mcb.
	 * Add virtual address of this mcb to the software context
	 * word of the mcb; the hdc firmware copies this word to
	 * the master mcb when the mcb is complete.
	 *
	 * If the buf was sent locally by this handler (via 'hdphysio')
	 * then there may be commands other than just read or write.
	 * 'hdphysio' also provides a cylinder/head/sector address.
	 */

	{
		/*
		 * The following priority calculation is based on the
		 * real time functional specification.
		 */
		register struct  proc *p = u.u_procp;
		mcb->priority = 0;
		if ((p->p_ppid) &&	/* not a system process */
		    ((p->p_nice < MIN_NON_RT_NICE_VAL) ||
                     (rt_disk_scheduling))) {
			mcb->priority = 32 - p->p_basepri;
		}
	}

	mcb->interrupt = TRUE;
	mcb->drive     = hu->slave;
	mcb->buf_ptr   = bp;
	mcb->context   = (unsigned long) mcb;
	if (bp->b_flags & B_LOCALIO) {
 		mcb->command = bp->b_hdccommand;
		mcb->cyl     = bp->b_cyl;
		mcb->head    = bp->b_head;
		mcb->sector  = bp->b_sector;
	}
	else {
		mcb->command = (bp->b_flags & B_READ) ? HCMD_READ:HCMD_WRITE;
		mcb->cyl     = sector/hu->spc;
		mcb->head    = (sector/hu->sectors) % hu->heads;
		mcb->sector  = sector % hu->sectors;
	}

	/*
	 * Build the data chain - address/count pairs for each page.
	 * The first transfer might not start on a page boundary.
	 * Purge the data cache for pages to be dma'd into.
	 *
	 * There is no attempt to combine physically contiguous
	 * pages into the same data chain, since it is faster
	 * to just dma the extra data chain into the controller
	 * than it is to combine the pages;
	 */

	{
		register struct  proc *procp;	 /* process structure   */
		register int	 bc;		 /* bytecount this page */
		register int	 bcremain=bytes; /* bytecount remaining */

		if ( bp->b_flags & B_DIRTY )
			procp = (struct proc *) &proc[2] ;
		else
			procp = bp->b_proc;
		if (bp->b_flags & B_READ)
			mtpr(P1DC, vaddr);
		bc = min( bcremain, (NBPG-(vaddr&(NBPG-1))) );
		mcb->chain[0].ta = vtoph(procp,vaddr);
		mcb->chain[0].lwc = bc/4;
		for (bcremain -= bc, i = 0; bcremain > 0;) {
			vaddr += bc;
			if (bp->b_flags & B_READ)
				mtpr(P1DC, vaddr);
			bc = min(bcremain, NBPG);
			mcb->chain[i].lwc |= LWC_DATA_CHAIN;
			i++;
			mcb->chain[i].ta = vtoph(procp,vaddr);
			mcb->chain[i].lwc= bc/4;
			bcremain -= bc;
		}
	}

	/*
	 * Set up information for error logging and system activity
	 * for programs such as iostat, sadp, sadc, sar, sag.
	 * Time-stamp the buf (and the unit if it is just becoming busy).
	 * Record the total number of transfer operations and the total
	 * no. of 512-byte blocks xferred.
	 * Turn on the activity bit for this device - for error logging.
	 */

	bp->b_start = time.tv_sec;
	if (vdtab[unit].b_active++ == 1)
		vdtab[unit].io_start = time.tv_sec;
	vdstat[unit].io_cnt++;
	vdstat[unit].io_bcnt += blocks * HDC_SPB;
	blkacty |= (1 << major(bp->b_dev));
	dk_wds[unit] += bytes/32;
  	dk_busy |= 1 << unit;

	/*
	 * If the controller has active mcb's:
	 *    don't send this mcb until the next interrupt occurs.
	 *
	 * Otherwise:
	 *    1) add the mcb to the active queue;
	 *    2) physically link the mcb from the master mcb;
	 *    3) fill in the master mcb;
	 *    4) tell the hdc to scan the new mcb.
	 */

	{
		register master_mcb_type *master; /* hdc's master mcb */

		master= &hc->master_mcb;
		priority = spl7();
		if ( hc->forw_active != (mcb_type *) &hc->forw_active ) {
	        	insque(mcb, &hc->forw_wait);
#ifdef HDCLOG
			hdlog(mcb,4 + 16*hc->ctlr);
#endif
		}
		else
		{
			insque(mcb, &hc->forw_active);
			master->forw_phaddr = mcb->mcb_phaddr;
			mcb->forw_phaddr = 0;
			master->mcs = 0;
#ifdef HDCLOG
			hdlog(mcb,5 + 16*hc->ctlr);
#endif
			HDC_REGISTER(master_mcb_reg) = hc->master_phaddr;
		}
		splx(priority);
	}

	/*
	 * Returns.
	 */

	return;
eacces:	bp->b_error = EACCES;
	goto errcom;
efault:	bp->b_error = EFAULT;
	goto errcom;
enxio:	bp->b_error = ENXIO;
errcom:	bp->b_flags |= B_ERROR;
	bp->b_resid = bytes;
	iodone(bp);
}

hdread(dev, uio)
	dev_t dev;
	int *uio;
{
	hdc_unit_type	*hu;

	hu = &hdc_units[HDC_UNIT(dev)];
	return(physio(hdstrategy, &hu->raw_buf, dev, B_READ, minphys, uio));
}

hdwrite(dev, uio)
	dev_t dev;
	int *uio;
{
	hdc_unit_type	*hu;

	hu = &hdc_units[HDC_UNIT(dev)];
	return(physio(hdstrategy, &hu->raw_buf, dev, B_WRITE, minphys, uio));
}
