/*	cy.c	1.2	86/01/05	*/

#include "cy.h"
#if NCY > 0
/*
 * Cipher Tapemaster driver.
 */
int	cydebug = 0;

#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "param.h"
#include "systm.h"
#include "vm.h"
#include "buf.h"
#include "file.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "signal.h"
#include "uio.h"
#include "ioctl.h"
#include "mtio.h"
#include "errno.h"
#include "cmap.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/cyreg.h"

#define	MAXCONTROLLERS		4
#define MAX_BLOCKSIZE		(TBUFSIZ*NBPG)
#define NUM_UNIT		(NCY * 4)

#define	TRUE			1
#define	FALSE			0

#define	RETRY			1
#define EXTEND			2
#define	FATAL			3

#define	MAINTAIN_POSITION	0
#define	DONT_MAINTAIN_POSITION	1

#define	PROCESSED		0x80000000
#define	SLEEPING		0x80000000
#define	b_cmd	av_back		/* only unused word in request */

extern	int cywrite_filemark(), cysearch_fm_forw(), cysearch_fm_back();
extern	int cy_space_forw(), cy_space_back(), cyrewind_tape_ta();
extern	int cyrewind_tape_unl(), cydrive_status(), cyrewind_tape_ov();
extern	int cyraw_read(), cyraw_write(), cybuf_read(), cybuf_write();
extern	int cywait_until_ready(), cywrite_0_fm(), cywrite_1_fm();
extern	int cywrite_2_fm(), cyno_op(), cywrite_eov();

static	int (*cmd_tbl[15])() = {
	cywrite_filemark,
#define	DO_W_FM	0
	cysearch_fm_forw,
#define	DO_SFMF	1
	cysearch_fm_back,
#define	DO_SFMB	2
	cy_space_forw,
#define	DO_SPF	3
	cy_space_back,
#define	DO_SPB	4
	cyrewind_tape_ta,
#define	DO_RWTA	5
	cyrewind_tape_unl,
#define	DO_RWUN	6
	cydrive_status,
#define	DO_STAT	7
	cyrewind_tape_ov,
#define	DO_RWOV	8
	cywait_until_ready,
#define DO_WAIT 9
	cywrite_eov,
#define DO_WEOV	10
	cyraw_read,
#define DO_RRD	11
	cyraw_write,
#define DO_RWT	12
	cybuf_read,
#define DO_BRD	13
	cybuf_write
#define DO_BWT	14
};


extern	int cyprobe(), cyslave(), cyattach(), cydgo();
extern unsigned	cyminsize();
#if NCY > 0
extern	char	cy0utl[];
#endif
#if NCY > 1
extern	char	cy1utl[];
#endif
static	fmt_scp *scp_ptrs[MAXCONTROLLERS] =
    { (fmt_scp *)0xc0000c06, (fmt_scp *)0xc0000c16, };
struct	vba_ctlr *cyminfo[NCY];
struct	vba_device *cydinfo[NUM_UNIT];
struct vba_driver cydriver = {
    cyprobe, cyslave, cyattach, cydgo, (long *)scp_ptrs,
    "yc", cydinfo, "cy", cyminfo
};

/*
 * Per-controller data structure.
 */
typedef struct {
	struct pte	*map;
	char		*utl;
	int		(*interupt_path)();
	label_t		environ;  /* Environment variable for longjmps */
	struct buf	*my_request;
	struct buf	*wakeup_request;
	short		bs;	  /* buffer size */
	fmt_ccb		ccb;	  /* Channel control blocks */
	fmt_scb		scb;	  /* System configuration blocks */
	fmt_tpb		tpb;	  /* Tape parameter blocks */
	fmt_tpb		last;	  /* Tape parameter blocks */
	fmt_tpb		noop;	  /* Tape parameter blocks */
	long		rawbuf[MAX_BLOCKSIZE/sizeof(long)+1];
} ctlr_tab;

extern	int cy_normal_path();
ctlr_tab ctlr_info[NCY] = {
#if NCY > 0
	{CY0map, cy0utl, cy_normal_path},
#endif
#if NCY > 1
	{CY1map, cy1utl, cy_normal_path},
#endif
};

/*
 * Per-drive information.
 */
typedef struct {
	int		(*cleanup)();
	struct buf	u_queue;
	struct buf	rawbp;
	long		blkno;
	long		file_number;
	short		last_control;
	short		last_status;
	short		last_resid;
	unsigned long	bad_count;
	unsigned	control_proto: 16;
	unsigned	error_count  : 8;
	unsigned	open	     : 1;
	unsigned	eof	     : 1;
	unsigned	bot	     : 1;
	unsigned	eot	     : 1;
	char		*message;
} unit_tab;
unit_tab unit_info[NUM_UNIT];

cyprobe(ctlr_vaddr)
	register caddr_t ctlr_vaddr;
{
	static int ctlr = -1;			/* XXX */

	ctlr++;
	if (badcyaddr(ctlr_vaddr + 1) || 
	    !cy_init_controller(ctlr_vaddr, ctlr, 1))
		return (0);
	return (sizeof (caddr_t));		/* XXX */
}

/*
 * Initialize the controller after a controller reset or during autoconfigure.
 * All of the system control blocks are initialized and the controller is
 * asked to configure itself for later use.
 *
 * If the print value is true cy_first_TM_attention will anounce
 * the type of controller we are (Tapemasher) and will print the size
 * of the internal controller buffer.
 */
cy_init_controller(ctlr_vaddr, ctlr, print)
	register caddr_t ctlr_vaddr;
	register int ctlr, print;
{
	register int *pte;
	register fmt_scp *SCP;
	register fmt_scb *SCB;
	register fmt_ccb *CCB;
	register ctlr_tab *ci;

	/*
	 * Initialize the system configuration pointer.
	 */
	SCP = scp_ptrs[ctlr];
	/* make kernel writable */
	pte = (int *)vtopte((struct proc *)0, btop(SCP)); 
	*pte &= ~PG_PROT; *pte |= PG_KW;
	mtpr(TBIS, SCP);
	/* load the correct values in the scp */
	SCP->bus_size = _16_BITS;
	load_mbus_addr((caddr_t)&ctlr_info[ctlr].scb, SCP->scb_ptr);
	/* put it back to read-only */
	*pte &= ~PG_PROT; *pte |= PG_KR;
	mtpr(TBIS, SCP);

	/*
	 * Init system configuration block.
	 */
	SCB = &ctlr_info[ctlr].scb;
	SCB->fixed_value = 0x3;
	/* set pointer to the channel control block */
	load_mbus_addr((caddr_t)&ctlr_info[ctlr].ccb, SCB->ccb_ptr);

	/*
	 * Initialize the chanel control block.
	 */
	CCB = &ctlr_info[ctlr].ccb;
	CCB->ccw = CLEAR_INTERUPT;
	CCB->gate = GATE_OPEN;
	/* set pointer to the tape parameter block */
	load_mbus_addr((caddr_t)&ctlr_info[ctlr].tpb, CCB->tpb_ptr);

	/*
	 * Issue a noop cmd and get the internal buffer size for buffered i/o.
	 */
	ci = &ctlr_info[ctlr];
	/* set command to be CONFIGURE */
	ci->tpb.cmd = NO_OP;
	ci->tpb.control = CW_16bits;
	ci->ccb.gate = GATE_CLOSED;	
	CY_ATTENTION(ctlr_vaddr);	/* execute! */
	if (cywait(&ci->ccb) || (ci->tpb.status & CS_ERm)) {
		printf("yc%d: time-out during init\n", ctlr);
		return (0);
	}
	ci->tpb.cmd = CONFIG;
	ci->tpb.control = CW_16bits;
	ci->ccb.gate = GATE_CLOSED;	
	CY_ATTENTION(ctlr_vaddr);	/* execute! */
	if (cywait(&ci->ccb) || (ci->tpb.status & CS_ERm)) {
		cyprint_err("Tapemaster configuration failure",
		    0, ci->tpb.status);
		return (0);
	}
	uncache(&ci->tpb.count);
	ci->bs = MULTIBUS_SHORT(ci->tpb.count);
	if (print)
		printf("yc%d: %dKb buffer\n", ctlr, ci->bs/1024);
	return (1);
}

/*
 * Check to see if a drive is attached to a controller.
 * Since we can only tell that a drive is there if a tape is loaded and
 * the drive is placed online, we always indicate the slave is present.
 */
cyslave(vi, addr)
	struct vba_device *vi;
	caddr_t addr;
{

#ifdef lint
	vi = vi; addr = addr;
#endif
	return (1);
}

cyattach(dev_info)
	struct vba_device *dev_info;
{
	register unit_tab *ui = &unit_info[dev_info->ui_unit];
	register struct buf *cq = &dev_info->ui_mi->um_tab;
	register struct buf *uq = cq->b_forw;
	register struct buf *start_queue = uq;

	/* Add unit to controllers queue */
	if (cq->b_forw == NULL) {
		cq->b_forw = &ui->u_queue;
		ui->u_queue.b_forw = &ui->u_queue;
	} else {
		while(uq->b_forw != start_queue)
			uq = uq->b_forw;
		ui->u_queue.b_forw = start_queue;
		uq->b_forw = &ui->u_queue;
	}
	ui->cleanup = cyno_op;
	ui->last_status = 0;
	ui->last_control = 0;
	ui->file_number = 0;
	ui->bad_count = 0;
	ui->blkno = 0;
	ui->open = FALSE;
	ui->bot = TRUE;
	ui->eot = FALSE;
	ui->eof = FALSE;
	ui->message = NULL;
}

cydgo()
{

}

/* macro to pack the unit number into Tapemaster format */
#define	UNIT(unit) \
    (((cydinfo[unit]->ui_slave & 1) << 11) | \
     ((cydinfo[unit]->ui_slave & 2) << 9) | \
     ((cydinfo[unit]->ui_slave & 4) >> 2))

cyopen(dev, flag)
	register int flag;
	register dev_t dev;
{
	register int unit = CYUNIT(dev);
	register unit_tab *ui;

	if (unit >= NUM_UNIT || cydinfo[unit] == 0 ||
	    (ui = &unit_info[unit])->open)
		return (ENXIO);
	ui->control_proto = UNIT(unit) | CW_INTR | CW_16bits;
	ui->blkno = 0;
	ui->bad_count = 0;
	ui->eof = 0;
	ui->open = 1;
	cycmd(dev, DO_WAIT, 1);			/* wait for tape to rewind */
	if ((ui->last_status&CS_OL) == 0) {	/* not on-line */
		ui->open = 0;
		return (ENXIO);
	}
	if ((flag&FWRITE) && (ui->last_status&CS_P)) {
		uprintf("cy%d: write protected\n", unit);
		ui->open = 0;
		return (ENXIO);
	}
	if (ui->last_status&CS_LP) {
		ui->file_number = 0;
		ui->bot = 1;
		ui->eof = ui->eot = 0;
	}
	return (0);
}

cyclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register int unit = CYUNIT(dev);
	register unit_tab *ui = &unit_info[unit];

	if (ui->last_status&CS_OL) {
		if ((flag&FWRITE) && (minor(dev)&T_NOREWIND))
			cycmd(dev, DO_WEOV, 1);
		else if ((minor(dev) & T_NOREWIND) == 0)
			cycmd(dev, DO_RWOV, 1); 
	}
	if (ui->bad_count != 0) {
#ifdef notdef
		ui->bad_count *= 889;
		uprintf("cy%d: Warning - %d.%dcm of tape were used for recovering bad spots.\n", unit, ui->bad_count/100, ui->bad_count%100);
#endif
		ui->bad_count = 0;
	}
	ui->open = 0;
}

/*
 * Cycmd is used internally to implement all the ioctl functions.
 * We duplicate the code in physio
 * that is used for syncronizing the processes (sleep / wakeup) so
 * that we can treat our internal command requests exactly like
 * regular reads and writes.  They get put on the controller queue,
 * start processes them and iodone is called to wake us up on completion.
 *
 * We don't call physio directly because it expects data to be moved
 * and has a lot more overhead than we really need.
 */
cycmd(dev, command, count)
	register dev_t dev;
	register int command, count;
{
	register int unit = CYUNIT(dev);
	register unit_tab *ui = &unit_info[unit];
	register struct buf *uq;
	int s;
	
	s = spl3();
	while (ui->rawbp.b_flags & B_BUSY) {
		ui->rawbp.b_flags |= B_WANTED;
		sleep((caddr_t)&ui->rawbp, PRIBIO+1);
	}
	splx(s);
	/* load the request queue element */
	ui->rawbp.b_error = 0;
	ui->rawbp.b_dev = dev;
	ui->rawbp.b_cmd = (struct buf *)command;
	ui->rawbp.b_bcount = count;
	ui->rawbp.b_flags = B_PHYS | B_BUSY;
	s = spl3();
	uq = &ui->u_queue;
	ui->rawbp.av_forw = NULL;
	if (uq->av_forw == NULL) 
		uq->av_forw = &ui->rawbp;
	else
		uq->av_back->av_forw = &ui->rawbp;
	uq->av_back = &ui->rawbp;
	cystart(cydinfo[unit]->ui_mi, &ui->rawbp, s);

	/* wait for operation to complete */
	while ((ui->rawbp.b_flags&B_DONE) == 0)
		sleep((caddr_t)&ui->rawbp, PRIBIO);
	ui->rawbp.b_flags &= ~(B_PHYS | B_BUSY);
	if (ui->rawbp.b_flags & B_WANTED)
		wakeup((caddr_t)&ui->rawbp);
	return (geterror(&ui->rawbp));
}

cystrategy(bp)
	register struct buf *bp;
{
	register int unit = CYUNIT(bp->b_dev);
	register unit_tab *ui = &unit_info[unit];	
	register struct buf *uq;
	int s;

	/* check the validity of the request */
	if (bp->b_bcount > MAX_BLOCKSIZE) {
		uprintf("cy%d: Maximum block size is %dk!\n",
		    unit, MAX_BLOCKSIZE/1024);
		bp->b_error = EIO;
		bp->b_resid = bp->b_bcount;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	vbasetup(bp, MAX_BLOCKSIZE);
	if (bp->b_flags & B_PHYS)
		bp->b_cmd = (struct buf *)(bp->b_flags&B_READ? DO_RRD : DO_RWT);
	else
		bp->b_cmd = (struct buf *)(bp->b_flags&B_READ? DO_BRD : DO_BWT);
	/* place request on queue and start it */
	s = spl3();
	uq = &ui->u_queue;
	bp->av_forw = NULL;
	if (uq->av_forw == NULL)
		uq->av_forw = bp;
	else
		uq->av_back->av_forw = bp;
	uq->av_back = bp;
	cystart(cydinfo[unit]->ui_mi, bp, s);
}

struct	buf *cyget_next();
int	cystart_timeout();
/*
 * Cystart is called once for every request that is placed on a
 * controller's queue.  Start is responsible for fetching requests for
 * a controller queue, starting the operation, and waiting for completion,
 * and releasing the buf structure back to UNIX or cycmd, before fetching
 * the next request.
 *
 * The controller's queue looks like this:
 *
 *                      +---------------------------------------+
 *                      |                                       | 
 *      +-----------+   |   +-----------+        +-----------+  |
 *      |  b_forw   |---+-->|  b_forw   |--~ ~-->|  b_forw   |--+
 *      +-----------+       +-----------+        +-----------+
 *      |  b_back   |       | ......... |        | ......... |
 *      +-----------+       +-----------+        +-----------+
 *      | ......... |      First unit queue     Last unit queue
 *      +-----------+          element              element
 * head of controller queue
 *  (cyminfo[ctlr].um_tab)
 */
cystart(vi, bp, s)
	register struct vba_ctlr *vi;
	register struct buf *bp;
{
	int unit = CYUNIT(bp->b_dev), ctlr = vi->um_ctlr;
	register struct buf *next, *cq = &vi->um_tab;
	register unit_tab *ui = &unit_info[unit];
	register ctlr_tab *ci = &ctlr_info[ctlr];

	if (cq->b_active&SLEEPING) {
		untimeout(cystart_timeout, (caddr_t)cq);
		cystart_timeout(cq);
	}
	if (cq->b_active) {
		sleep((caddr_t)bp, PRIBIO-1);
		if (bp->b_flags&PROCESSED) {
			if (ui->message) {
				uprintf("cy%d: %s\n", unit, ui->message);
				ui->message = 0;
			}
			bp->b_flags &= ~PROCESSED;
			iodone(bp);
			return;
		}
	}
	cq->b_active = 1;
	splx(s);
	ci->my_request = bp;
	cydo_my_command(ctlr, cq, ci);
	if (ui->message) {
		uprintf("cy%d: %s\n", unit, ui->message);
		ui->message = 0;
	}
	bp->b_flags &= ~PROCESSED;
	iodone(bp);
	if ((next = cyget_next(cq)) != NULL)
		wakeup((caddr_t)next);
	else
		cq->b_active = 0;
}

/*
 * Cystart_timeout wakes up the start routine after it's 3
 * second wait time is up or when a new command enters the queue.
 * The timer is used to give up the processor while all drives
 * on the queue are rewinding and we need to wait for them to be dome.
 */
cystart_timeout(cq)
	register struct buf *cq;
{

	cq->b_active &= ~SLEEPING;
	wakeup((caddr_t)cq);
}

/*
 * Cydo_my command scans the request queues once for a
 * particular controller and calls the appropriate processing routine
 * each time we find a request that can be started.
 */
cydo_my_command(ctlr, cq, ci)
	register struct buf *cq;
	register ctlr_tab *ci;
{
	register struct buf *next;

	while ((next = cyget_next(cq)) != NULL) {
		if (cq->b_forw->b_active&SLEEPING) {
			cq->b_active |= SLEEPING;
			timeout(cystart_timeout, (caddr_t)cq, 1*60);
			sleep((caddr_t)cq, PRIBIO);
			continue;
		}
		if (setjmp(&ctlr_info[ctlr].environ))
			cydone(cq);
		else {
			register int cmd = (int)next->b_cmd;

			(*cmd_tbl[cmd])(next, cq);
		}
		if (next->b_flags & PROCESSED) {
			if (ci->my_request == next)
				break;
			wakeup((caddr_t)next);
		}
	}
}

struct buf *
cyget_next(cq)
	register struct	buf *cq;
{
	register struct buf *bp, *uq, *next = NULL;

	cq->b_forw = cq->b_forw->b_forw;
	uq = cq->b_forw;
	do {
		if ((bp = uq->av_forw) != NULL) {
			if ((uq->b_active&SLEEPING) == 0) {
				cq->b_forw = uq;
				return (bp);
			}
			next = uq;
		}
		uq = uq->b_forw;
	} while(uq != cq->b_forw);
	if (next != NULL) {
		cq->b_forw = next;
		return (next->av_forw);
	}
	return (NULL);
}

/*
 * Mark the current command on the controller's q completed and remove it.
 */
cydone(cq)
	struct buf *cq;
{
	register struct buf *uq = cq->b_forw;
	int s;

	uq->av_forw->b_flags |= PROCESSED;
	s = spl3();
	if ((uq->av_forw = uq->av_forw->av_forw) == NULL) 
		uq->av_back = NULL;
	splx(s);
}

/*
 * The following routines implement the individual commands.
 *
 * Each command is responsible for a few things. 1) Each has to keep
 * track of special cases that are related to the individual command and
 * the previous commands sequence, 2) each is required to call iodone when
 * command is actually finished, 3) it must use cyexecute to actually
 * start the controller, and 4) they are required to keep the tape in
 * a consistant state so that other commands will not be messed up.
 */

/*
 * Read requests from the raw device.
 * The special cases are:
 *  1) we can not read after a write.  (writting defines end of file)
 *  2) reading past end of file returns 0 bytes;
 */
cyraw_read(bp, cq)
	register struct buf *bp;
	struct buf *cq;
{
	int unit = CYUNIT(bp->b_dev);
	register unit_tab *ui = &unit_info[unit];
	register ctlr_tab *ci = &ctlr_info[cydinfo[unit]->ui_ctlr];
	int addr, lock_flag, command;

	if (ui->cleanup != cyno_op || ui->eof) {
		bp->b_resid = bp->b_bcount;
		bp->b_error = ENXIO, bp->b_flags |= B_ERROR;
		cydone(cq);
		return;
	}
	if (bp->b_bcount > ci->bs)
		command = READ_TA, lock_flag = CW_LOCK;
	else
		command = READ_BU, lock_flag = 0;
	ui->blkno++;
	addr = vbastart(bp, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
	cyexecute(command, bp->b_bcount, addr, lock_flag, unit, 10, FALSE);
	vbadone(bp, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
	cydone(cq);
}

/*
 * Write requests from the raw device.
 * The special cases are:
 *  1) we don't allow writes after end of tape is reached.
 */
cyraw_write(bp, cq)
	register struct buf *bp;
	struct buf *cq;
{
	int unit = CYUNIT(bp->b_dev);
	register unit_tab *ui = &unit_info[CYUNIT(unit)];
	register ctlr_tab *ci = &ctlr_info[cydinfo[unit]->ui_ctlr];
	int addr, lock_flag, command;

	if (ui->eot) {
		bp->b_resid = bp->b_bcount;
		bp->b_error = ENXIO, bp->b_flags |= B_ERROR;
		longjmp(&ci->environ);
	}
	ui->cleanup = cywrite_2_fm;
	if (bp->b_bcount > ci->bs)
		command = WRIT_TA, lock_flag = CW_LOCK;
	else
		command = WRIT_BU, lock_flag = 0;
	ui->blkno++;
	addr = vbastart(bp, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
	cyexecute(command, bp->b_bcount, addr, lock_flag, unit, 10, FALSE);
	vbadone(bp, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
	cydone(cq);
}

/*
 * Write a filemark on a tape.
 */
cywrite_filemark(bp, cq)
	register struct buf *bp;
	struct buf *cq;
{
	int unit = CYUNIT(bp->b_dev);
	register unit_tab *ui = &unit_info[CYUNIT(unit)];

	if (bp->b_bcount == 0) {
		cydone(cq);
		return;
	}
	bp->b_bcount--;
	if (ui->cleanup == cywrite_1_fm)
		ui->cleanup = cywrite_0_fm;
	if (ui->cleanup == cywrite_2_fm || ui->cleanup == cyno_op)
		ui->cleanup = cywrite_1_fm;
	ui->file_number++;
	ui->eof = 1;
	ui->blkno = 0;
	cyexecute(WRIT_FM, (long)1, 0, 0, unit, 10, FALSE);
}

/*
**	cysearch_fm_forw is the ioctl to search for a filemark in the
**  forward direction on tape.
**
**	Since only one device can be active on a given controller at any
**  given instant in time, we try to be nice and let onther devices  on
**  this controller be scheduled after we space over each record.  This will
**  at least give the apperance of overlapped operations on the controller.
**
**  The special cases are:
**  1) if the last command was a write the we can't search.
*/

cysearch_fm_forw(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register ctlr_tab	*ci = &ctlr_info[ctlr];

	if((ui->cleanup != cyno_op) || ui->eot) {
		request->b_resid = request->b_bcount;
		request->b_error = ENXIO, request->b_flags |= B_ERROR;
		longjmp(&ci->environ);
	}
	if(request->b_bcount && !ui->eot) {
		if(!ui->eot) {
			ui->blkno++;
			cyexecute(SPAC_FM, (long)1, 0, 0, unit, 5, FALSE);
			if(!(ui->eof || ui->eot))
				return;
		}
		request->b_bcount--;
		ui->eof = FALSE;
		if(!ui->eot) {
			ui->file_number++;
			ui->blkno = 0;
			return;
		}
	}
	if(ui->eot) {
		request->b_resid = request->b_bcount;
		request->b_flags |= B_ERROR, request->b_error = ENXIO;
	}
	cydone(cq);
}


/*
**	cysearch_fm_back is the ioctl to search for a filemark in the
**  reverse direction on tape.
**
**	Since only one device can be active on a given controller at any
**  given instant in time, we try to be nice and let onther devices  on
**  this controller be scheduled after we space over each record.  This will
**  at least give the apperance of overlapped operations on the controller.
**
**  The special cases are:
**  1) can't search past begining of tape.
**  2) if the lasr operation was a write data then we need to add
**     an end of volume record before we start searching.
*/

cysearch_fm_back(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];

	if(!ui->bot) {
		(*ui->cleanup)(unit, MAINTAIN_POSITION);
		if(ui->blkno == 0)
			request->b_bcount++;
		ui->blkno = 0xffffffff;
		if(request->b_bcount && !ui->bot) {
			cyexecute(SPAC_FM, (long)1, 0, CW_REV, unit, 6, FALSE);
			if(ui->eof) {
				ui->eof = FALSE;
				ui->file_number--;
				request->b_bcount--;
			}
			return;
		}
		if(ui->bot) {
			ui->file_number = 0;
			if(request->b_bcount) {
				request->b_resid = request->b_bcount;
				request->b_error = ENXIO;
				request->b_flags |= B_ERROR;
			}
		}
		else {
			request->b_cmd = (struct buf *)DO_SFMF;
			request->b_bcount = 1;
			return;
		}
	}
	ui->blkno = 0;
	ui->eof = FALSE;
	cydone(cq);
}


/*
**	cy_space_forw is used to search forward a given number of records on
**  tape.
**
**	Since only one device can be active on a given controller at any
**  given instant in time, we try to be nice and let onther devices  on
**  this controller be scheduled after we space over each record.  This will
**  at least give the apperance of overlapped operations on the controller.
**
**  The special cases are:
**  1) we can't space over a filemark.
**  2) if the last command was a write data or filemark we can't space forward.
*/

cy_space_forw(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register ctlr_tab	*ci = &ctlr_info[ctlr];

	if((ui->cleanup != cyno_op) || ui->eof) {
		request->b_resid = request->b_bcount;
		request->b_error = ENXIO, request->b_flags |= B_ERROR;
		longjmp(&ci->environ);
	}
	if(request->b_bcount) {
		ui->blkno++;
		cyexecute(SPAC_FM, (long)1, 0, 0, unit, 10, FALSE);
		if(!ui->eof && request->b_bcount) {
			request->b_bcount--;
			return;
		}
	}
	if(ui->eof) {
		request->b_resid = request->b_bcount;
		request->b_error = ENXIO, request->b_flags |= B_ERROR;
	}
	cydone(cq);
}


/*
**	Cy_space_back spaces backward a given number of records.
**
**	Since only one device can be active on a given controller at any
**  given instant in time, we try to be nice and let onther devices  on
**  this controller be scheduled after we space over each record.  This will
**  at least give the apperance of overlapped operations on the controller.
**
**  The special cases are:
**  1) we can't space over a filemark.
**  2) we can't space past the beginning of tape.
**  3) if the last operation was a write data then we need to add
**     an end of volume record before we start searching.
*/

cy_space_back(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];

	if(!ui->bot) {
		(*ui->cleanup)(unit, MAINTAIN_POSITION);
		if(request->b_bcount+1 && !ui->bot && !ui->eof) {
			request->b_bcount--;
			ui->blkno--;
			cyexecute(SPACE, (long)1, 0, CW_REV, unit, 15, FALSE);
			return;
		}
		if(!ui->bot) {
			request->b_bcount = 1;
			cy_space_forw(request, cq);
		}
		ui->eof = FALSE;
	}
	cydone(cq);
}

/*
 * Rewind tape and wait for completion.
 * An overlapped rewind is issued and then we change the command type to
 * a wait for ready ioctl.  Wait for ready contains the logic to poll
 * without blocking anything in the system, until the drive becomes ready or
 * drops off line whichever comes first.
 */
/*ARGSUSED*/
cyrewind_tape_ta(bp, cq)
	struct buf *bp, *cq;
{

	cyrewind_tape(bp, REWD_OV);
	bp->b_cmd = (struct buf *)DO_WAIT;
}

/*
 * Do an overlapped rewind and then unload the tape.
 * This feature is handled by the individual tape drive and
 * in some cases can not be performed.
 */
cyrewind_tape_unl(bp, cq)
	struct buf *bp, *cq;
{

	cyrewind_tape(bp, OFF_UNL);
	cydone(cq);
}

/*
 * Do an overlapped rewind.
 */
cyrewind_tape_ov(bp, cq)
	struct buf *bp, *cq;
{

	cyrewind_tape(bp, REWD_OV);
	cydone(cq);
}

/*
 * Common code for all rewind commands.
 * The special cases are:
 *  3) if the last operation was a write data then we need to add
 *     an end of volume record before we start searching.
 */
cyrewind_tape(bp, cmd)
	register struct buf *bp;
	int cmd;
{
	register int unit = CYUNIT(bp->b_dev);
	register unit_tab *ui = &unit_info[unit];

	(*ui->cleanup)(unit, DONT_MAINTAIN_POSITION);
	ui->blkno = 0;
	ui->eof = FALSE;
	ui->bot = TRUE;
	ui->eot = FALSE;
	ui->file_number = 0;
	bp->b_resid = 0;
	ui->cleanup = cyno_op;
	cyexecute(cmd, (long)0, 0, 0, unit, cmd == REWD_OV ? 10 : 10*60, 0);
}

/*
**	Cywait_until_ready is used to wait for rewinds to complete.
**  We check the status and if the tape is still rewinding we re-enter ourself
**  on the activity queue to give other requests a chance to execute before we
**  check the status again.  One other thing is that we only want to  check
**  the status every five seconds.  so we set a timer for five seconds and
**  check the time left every time we enter this routine.  If there is still
**  time left then we simply reinsert ourself on the queue again and wait
**  until next time ..
*/
cywait_until_ready(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	extern int		cywait_timeout();
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[unit];

	cyexecute(DRIVE_S, (long)0, 0, 0, unit, 10, FALSE);
	if((!(ui->last_status & CS_OL)) || (ui->last_status & CS_RDY)) {
		cydone(cq);
		return;
	}
	cq->b_forw->b_active |= SLEEPING;
	timeout(cywait_timeout, (caddr_t)cq->b_forw, 2*60);
}

/*
 * Reset the timing flag for nice_wait after 3 seconds.
 * This makes this drive eligible for scheduling again.
 */
cywait_timeout(uq)
	struct buf *uq;
{

	uq->b_active &= ~SLEEPING;
}

/*
 * Process a status ioctl request.
 * It depends entirly on the interupt routines to load the last_XXX
 * registers in unit_info[].
 */
cydrive_status(bp, cq)
	struct buf *bp, *cq;
{

	cyexecute(DRIVE_S, (long)0, 0, 0, CYUNIT(bp->b_dev), 10, FALSE);
	cydone(cq);
}

/*
**	cybuf_read handles the read requests from the block device.
**
**  The special cases are:
**  1)	we can not read after a write.  (writting defines end of file)
**  2)  reading past end of file returns 0 bytes;
**  3)  if we are mispositioned we have to seek to the correct block.
**  4)  we can hit end of tape while seeking.
**  5)  we want to be nice to other processes while seeking so we
**  	break the request up into smaller requests.
**  6)  returns error if the block was larger than requested. 
*/
cybuf_read(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register ctlr_tab	*ci = &ctlr_info[ctlr];
	register int		addr, command, bus_lock;

	cydebug = 1;
	if(cyseek(request, cq)) {
		if(ui->cleanup != cyno_op) {
			clrbuf(request);
			longjmp(&ci->environ);
		}
		if(request->b_bcount > ci->bs)
			command = READ_TA, bus_lock = CW_LOCK;
		else
			command = READ_BU, bus_lock = 0;
		ui->blkno++;
		addr = vbastart(request, (caddr_t)ci->rawbuf, (long *)ci->map,
		    ci->utl);
		cyexecute(command,request->b_bcount,addr,bus_lock,unit,8,FALSE);
		vbadone(request, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
		cydone(cq);
	}
}


/*
**	cybuf_write handles the write requests from the block device.
**
**  The special cases are:
**  1)  if we are mispositioned we have to seek to the correct block.
**  2)  we can hit end of tape while seeking.
**  3)  we want to be nice to other processes while seeking so we
**  	break the request up into smaller requests.
**  4) we don't allow writes after end of tape is reached.
*/

cybuf_write(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register ctlr_tab	*ci = &ctlr_info[ctlr];
	register int		addr, command, bus_lock;
	
	if(ui->eot && (request->b_blkno >= ui->blkno)) {
		request->b_error = ENXIO, request->b_flags |= B_ERROR;
		request->b_resid = request->b_bcount;
		longjmp(&ci->environ);
	}
	if(cyseek(request, cq)) {
		ui->cleanup = cywrite_2_fm;
		ui->blkno++;
		if(request->b_bcount > ci->bs)
			command = WRIT_TA, bus_lock = CW_LOCK;
		else
			command = WRIT_BU, bus_lock = 0;
		addr = vbastart(request, (caddr_t)ci->rawbuf, (long *)ci->map,
		    ci->utl);
		load_mbus_addr((caddr_t)addr, (short *)&ci->tpb.data_ptr);
		cyexecute(command,request->b_bcount,addr,bus_lock,unit,5,FALSE);
		vbadone(request, (caddr_t)ci->rawbuf, (long *)ci->map, ci->utl);
		cydone(cq);
	}
}


/*
**	cyseek is used by the block device to position the tape correctly
**  before each read or write request.
**
**  The special cases are:
**  1)  we can hit end of tape while seeking.
**  2)  we want to be nice to other processes while seeking so we
**  	break the request up into smaller requests.
*/
cyseek(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register ctlr_tab	*ci = &ctlr_info[ctlr];

#ifdef lint
	cq = cq;
#endif
	if(request->b_blkno < ui->blkno) {
		register int	count;

		(*ui->cleanup)(unit, MAINTAIN_POSITION);
		count = ((request->b_blkno+1) == ui->blkno) ? 2 : 1;
		ui->blkno -= count;
		cyexecute(SPAC_FM, (long)1, 0, CW_REV, unit, 10, FALSE);
		if(!ui->eof)
			return FALSE;
		ui->eof = FALSE;
		request->b_blkno = ui->blkno + 1;
	}
	if(request->b_blkno > ui->blkno) {
		if((ui->cleanup != cyno_op) || ui->eof || ui->eot) {
			request->b_resid = request->b_bcount;
			request->b_error = ENXIO, request->b_flags |= B_ERROR;
			longjmp(&ci->environ);
		}
		ui->blkno++;
		cyexecute(SPAC_FM, (long)1, 0, 0, unit, 10, FALSE);
		return FALSE;
	}
	return TRUE;
}


/*
*/

cywrite_eov(request, cq)
register struct buf	*request;
register struct buf	*cq;
{
	extern int		cyno_op();
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[CYUNIT(unit)];

	if(ui->cleanup != cyno_op) {
		(*ui->cleanup)(unit, DONT_MAINTAIN_POSITION);
		cyexecute(SPACE, (long)2, 0, CW_REV, unit, 10, FALSE);
		cyexecute(SPACE, (long)1, 0, 0, unit, 10, FALSE);
		unit_info[unit].cleanup = cyno_op;
		ui->blkno = 0;
	}
	cydone(cq);
}


/*
**	Do nothing
*/
/*ARGSUSED*/
cyno_op(unit, action)
int	unit, action;
{
}


/*
**	Write 0 file marks to tape
*/
/*ARGSUSED*/
cywrite_0_fm(unit, action)
int	unit, action;
{
	unit_info[unit].cleanup = cyno_op;
}


/*
**	Write 1 file mark to tape
*/

cywrite_1_fm(unit, action)
int	unit, action;
{

	cyexecute(WRIT_FM, (long)1, 0, 0, unit, 5, FALSE);
	if(action == MAINTAIN_POSITION) {
		cyexecute(SPACE, (long)2, 0, CW_REV, unit, 10, FALSE);
		cyexecute(SPACE, (long)1, 0, 0, unit, 10, FALSE);
	}
	unit_info[unit].cleanup = cyno_op;
}


/*
**	Write 2 file marks to tape
*/

cywrite_2_fm(unit, action)
int	unit, action;
{

	cyexecute(WRIT_FM, (long)1, 0, 0, unit, 5, FALSE);
	cyexecute(WRIT_FM, (long)1, 0, 0, unit, 5, FALSE);
	if(action == MAINTAIN_POSITION) {
		cyexecute(SPACE, (long)3, 0, CW_REV, unit, 10, FALSE);
		cyexecute(SPACE, (long)1, 0, 0, unit, 2, FALSE);
	}
	unit_info[unit].cleanup = cyno_op;
}


extern	int cytimeout();
extern	int cy_normal_path();
/*
**	Cyexecute is used to start all commands to the controller.  We
**  do all common code here before starting.
*/

cyexecute(command, count, addr, control_flags, unit, time, interupt_routine)
	register int command;
	long count;
	int addr, control_flags, unit, time, interupt_routine;
{
	register int		priority;
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register unit_tab	*ui = &unit_info[unit];
	register ctlr_tab	*ci = &ctlr_info[ctlr];
	register struct buf	*request = ui->u_queue.av_forw;

	ci->tpb.cmd = command;
	ci->tpb.control = ui->control_proto | control_flags;
	ci->tpb.status = ci->tpb.count = (short)0;
	load_mbus_addr((caddr_t)addr, (short *)&ci->tpb.data_ptr);
	switch(command) {
		case READ_BU:
		case READ_TA:
		case WRIT_BU:
		case WRIT_TA:
			ci->tpb.size = MULTIBUS_SHORT((short)count);
			ci->tpb.rec_over = (short)0;
			break;
		default:
			ci->tpb.size = (short)0;
			ci->tpb.rec_over = MULTIBUS_SHORT((short)count);
			break;
	}
	load_mbus_addr((caddr_t)0, ci->tpb.link_ptr);
	if(!interupt_routine)
		ci->last = ci->tpb;
	/*
	gag! but it the last possible moment to wait 
	for this controller to get out of it's own way.....
	*/
	uncache(&ci->ccb.gate);
	while(ci->ccb.gate == GATE_CLOSED)
		uncache(&ci->ccb.gate);
	load_mbus_addr((caddr_t)&ci->tpb, ci->ccb.tpb_ptr);
	ci->ccb.ccw = NORMAL_INTERUPT;
	ci->ccb.gate = GATE_CLOSED;
	if(!interupt_routine)
		ci->interupt_path = cy_normal_path;
	timeout(cytimeout, (caddr_t)ctlr, time*60);
	priority = spl3();
	CY_ATTENTION(cyminfo[ctlr]->um_addr);
	if(!interupt_routine) {
		sleep((caddr_t)ci, PRIBIO+3);
		splx(priority);
		if(request->b_flags & B_ERROR) {
			if((command == READ_BU) || (command == READ_TA) ||
			    (command == WRIT_BU) || (command == WRIT_TA))
				vbadone(request, (caddr_t)ci->rawbuf,
				     (long *)ci->map,ci->utl);
			longjmp(&ci->environ);
		}
		return;
	}
	splx(priority);
}


/*
**	cytimeout is the interupt timeout routine.  We assume that a
**  particular command has gone astray, so we completely reset the controller,
**  and call the interupt routine to help us clean up.  Before the interupt
**  routine is called we jam a controller timeout value in the status register
**  to fake out the calling routines.
*/

cytimeout(ctlr)
register int	ctlr;
{
	register int	priority = spl3();
	register char	*ctlr_vaddr = cyminfo[ctlr]->um_addr;
	register int	tmp_stat;

	uncache(&ctlr_info[ctlr].tpb.status);
	tmp_stat = ctlr_info[ctlr].tpb.status;
	CY_RESET(ctlr_vaddr);
	cy_init_controller(ctlr_vaddr, ctlr, 0);
	splx(priority);
	ctlr_info[ctlr].tpb = ctlr_info[ctlr].last;
	ctlr_info[ctlr].tpb.status = (tmp_stat & ~CS_ERm) | CS_OL | ER_TIMOUT;
	cyintr(ctlr);
}

/*
**	Cyintr is the interupt routine for the Tapemaster controller.
**
**	Due to controller problems, the first thing we have to do is turn
**  off the Tapemaster interupting mechanism.  If we don't we will be flooded
**  with bogus interupts and the system will spend all it's time processing
**  them.  To Turn the interupts off we issue a NOOP command with the 'turn
**  off interupts' code in the ccb.
**
**	  take note that since this command TURNS OFF the interupts it
**	  itself CANNOT interupt...  This means that polling must be done
**	  at sometime to make sure that tis command is completed.  The polling
**	  is done before the next command is issued to reduce polling (halting
**	  UNIX) time.
**
**	After we turn off interupts we uncache all the values in the tpb
**  and call the correct processing routine.  This routine can be for normal
**  interupts or for interupts generated during a retry operation.
*/

cyintr(ctlr)
register int ctlr;
{
	extern int		cytimeout();
	register ctlr_tab	*ci = &ctlr_info[ctlr]; 

	untimeout(cytimeout, (caddr_t)ctlr);
	/* turn off interupts for the stupid controller */
	ci->ccb.ccw = CLEAR_INTERUPT;
	ci->noop.cmd = NO_OP;
	ci->noop.control = (short)0;
	load_mbus_addr((caddr_t)&ci->noop, ci->ccb.tpb_ptr);
	ci->ccb.gate = GATE_CLOSED;
	CY_ATTENTION(cyminfo[ctlr]->um_addr);
	uncache_tpb(ci);
	(*ci->interupt_path)(ctlr);
}


/*
**	This is the portion of the interupt routine that processes all
**  normal cases i.e. non retry cases.   We check the operations status
**  if it is retryable we set the interupt path to the retry routines and
**  start the backward spaceing.  when the spacing is done the retry logic
**  will be called and this routine will be skipped entirely.
**
**	If the command is ok or not retryable we set the status accordingly
**  and wakeup cyexecute to continue processing.
*/

cy_normal_path(ctlr)
register int ctlr;
{
	extern int		cy_retry_path();
	extern int		cy_extended_gap_path();
	register int		error;
	register struct buf	*cq = &cyminfo[ctlr]->um_tab;
	register struct buf	*uq = cq->b_forw;
	register struct buf	*request = uq->av_forw;
	register int		unit = CYUNIT(request->b_dev);
	register unit_tab	*ui = &unit_info[unit];
	register ctlr_tab	*ci = &ctlr_info[ctlr]; 

	if (error = cydecode_error(unit, ci->tpb.status)) {
		if(error != FATAL) {
			if (error == RETRY)
				ci->interupt_path = cy_retry_path;
			else
				ci->interupt_path = cy_extended_gap_path;
			cyexecute(SPACE, (long)2, 0, CW_REV, unit, 5, TRUE);
			return;
		}
	}
	request->b_resid=request->b_bcount-MULTIBUS_SHORT(ci->tpb.count);
	ui->error_count = 0;
	ui->last_resid = request->b_resid;
	ui->last_status = ci->tpb.status;
	ui->last_control = ci->tpb.control;
	if (error == FATAL)
		request->b_flags |= B_ERROR, request->b_error = EIO;
	wakeup((caddr_t)ci);
}


/*
**	Cy_retry_path finishes up the retry sequence for the tape.
** If we were going in the reverse direction it means that we have to
** space forward to correctly position ourselfs in back of the tape gap
** instead of in front of it.  If we were going forward it means that
** we are positioned correctly and we can actually restart the instruction
** that failed before.
*/

cy_retry_path(ctlr)
register int	ctlr;
{
	extern int		cy_do_again_path();
	register struct buf	*cq = &cyminfo[ctlr]->um_tab;
	register struct buf	*uq = cq->b_forw;
	register struct buf	*request = uq->av_forw;
	register int		unit = CYUNIT(request->b_dev);
	register ctlr_tab	*ci = &ctlr_info[ctlr]; 

	if(!(ci->tpb.status & CS_OL)) {
		ci->interupt_path = cy_normal_path;
		cy_normal_path(ctlr);
		return;
	}
	if(ci->tpb.control & CW_REV) {
		if(!(ci->tpb.status & CS_LP)) {
			ci->interupt_path = cy_do_again_path;
			cyexecute(SPACE, (long)1, 0, 0, unit, 5, TRUE);
			return;
		}
		cy_do_again_path(ctlr);
	}
}


/*
**
*/

cy_extended_gap_path(ctlr)
register int	ctlr;
{
	extern int		cy_do_again_path();
	register ctlr_tab	*ci = &ctlr_info[ctlr]; 
	register struct buf	*cq = &cyminfo[ctlr]->um_tab;
	register struct buf	*uq = cq->b_forw;
	register struct buf	*request = uq->av_forw;
	register int		unit = CYUNIT(request->b_dev);

	if(!(ci->tpb.status & CS_OL)) {
		ci->interupt_path = cy_normal_path;
		cy_normal_path(ctlr);
		return;
	}
	if(ci->tpb.control & CW_REV) {
		if(!(ci->tpb.status & CS_LP)) {
			cyexecute(SPACE, (long)1, 0, 0, unit, 5, TRUE);
			return;
		}
	}
	ci->interupt_path = cy_do_again_path;
	cyexecute(ERASE_F, (long)unit_info[unit].error_count, 0, 0,
	    unit, 5, TRUE);
}


/*
**
*/

cy_do_again_path(ctlr)
register int	ctlr;
{
	extern int		cy_normal_path();
	register ctlr_tab	*ci = &ctlr_info[ctlr]; 

	if(!(ci->tpb.status & CS_OL)) {
		ci->interupt_path = cy_normal_path;
		cy_normal_path(ctlr);
		return;
	}
	ci->tpb = ci->last;
	uncache(&ci->ccb.gate);
	while(ci->ccb.gate == GATE_CLOSED)
		uncache(&ci->ccb.gate);
	load_mbus_addr((caddr_t)&ci->tpb, ci->ccb.tpb_ptr);
	ci->ccb.ccw = NORMAL_INTERUPT;
	ci->ccb.gate = GATE_CLOSED;
	ci->interupt_path = cy_normal_path;
	CY_ATTENTION(cyminfo[ctlr]->um_addr);
}


/*
**	for each longword in the tpb we call uncache to  purge it from
**  the cache.  This is done so that we can correctly access tpb data
**  that was placed there by the controller.
*/

uncache_tpb(ci)
ctlr_tab	*ci;
{
	register long	*ptr = (long *)&ci->tpb;
	register int	i;

	for(i=0; i<((sizeof(fmt_tpb)+sizeof(long)-1)/sizeof(long)); i++)
		uncache(ptr++);
}


/*
**	Cyprint_error is the common printing routine for all messages
**  that need to print the tape status along with it.  This is so we
**  we can save space, have consistant messages, and we can send the messages
**  to the correct places.
*/

cyprint_err(message, unit, status)
register char	*message;
register int	unit, status;
{
	status &= 0xffff;
	printf("cy%d: %s!   Status = %x\n", unit, message, status);
}

/*
**	Decode the error to determine whether the previous command was
**  ok, retryable, or fatal and return the value.  If it was a hardware
**  problem we print the message to the console, otherwise we print it
**  to the user's terminal later when execute returns.
*/

cydecode_error(unit, status)
register int	unit,	status;
{
	register unit_tab	*ui = &unit_info[unit];
	register ctlr_tab	*ci = &ctlr_info[cydinfo[unit]->ui_ctlr];

	if(!(status & CS_OL) && (ci->tpb.cmd != OFF_UNL)) {
		ui->message = "Drive is not on-line";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	}
	ui->bot = ((status & CS_LP) != 0);
	ui->eof = ((status & CS_FM) != 0);
	switch(status & CS_ERm) {
	case ER_EOT:
		if(ci->tpb.control & CW_REV) {
			ui->bot = TRUE;
			ui->eot = FALSE;
		}
		else if(!ui->eot){
			ui->message = "End of tape";
			ui->bot = FALSE;
			ui->eot = TRUE;
		}
	case 0 :
	case ER_FM:
	case ER_NOSTRM:
		return	0;
	case ER_TIMOUT:
	case ER_TIMOUT1:
	case ER_TIMOUT2:
	case ER_TIMOUT3:
	case ER_TIMOUT4:
		ui->message = "Drive timed out during transfer";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	case ER_NEX:	
		ui->message =
		    "Controller referenced non-existant system memory";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	case ER_DIAG:
	case ER_JUMPER:
		ui->message = "Controller diagnostics failed";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	case ER_STROBE:
		if (ci->tpb.cmd == READ_BU) {
			ci->last.cmd = READ_TA;	
			return RETRY;
		}
		if(ci->tpb.cmd == READ_TA)
			return 0;
		ui->message = "Unsatisfactory media found";
		return	FATAL;
	case ER_FIFO:
	case ER_NOTRDY:
		ui->error_count = 1;
		return RETRY;
	case ER_PROT:
		ui->message = "Tape is write protected";
		return FATAL;
	case ER_CHKSUM:
		ui->message = "Checksum error in controller proms";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	case ER_HARD:
		ui->error_count++;
		if((ci->tpb.cmd == WRIT_TA) ||
		    (ci->tpb.cmd == WRIT_BU) ||
		    (ci->tpb.cmd == WRIT_FM)) {
			ui->bad_count++;
			return EXTEND;
		}
		ui->message = "Unrecoverable media error during read";
		return FATAL;
	case ER_PARITY:
		if(++ui->error_count < 8)
			return	RETRY;
		ui->message = "Unrecoverable tape parity error";
		return FATAL;
	case ER_BLANK:
		ui->message="Blank tape found (data expected)";
		return FATAL;
	case ER_HDWERR:
	default:
		ui->message = "Unrecoverble hardware error";
		cyprint_err(ui->message, unit, status);
		return FATAL;
	}
}

cyread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	unit_tab *ui = &unit_info[CYUNIT(dev)];
	
	return (physio(cystrategy, &ui->rawbp, dev, B_READ, cyminsize, uio));
}


cywrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	unit_tab *ui = &unit_info[CYUNIT(dev)];

	return (physio(cystrategy,&ui->rawbp, dev, B_WRITE, cyminsize, uio));
}

/*ARGSUSED*/
cyioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{

	switch (cmd) {

	case MTIOCTOP: {
		struct mtop *mp = (struct mtop *)data;

		if (mp->mt_op <= DO_WAIT)
			return (cycmd(dev, (int)mp->mt_op, (int)mp->mt_count));
		return (EIO);
	}

	case MTIOCGET: {
		register unit_tab *ui = &unit_info[CYUNIT(dev)];
		register struct mtget *mp = (struct mtget *)data;

		mp->mt_type = MT_ISCY;
		mp->mt_dsreg = ui->last_control;
		mp->mt_erreg = ui->last_status;
		mp->mt_resid = ui->last_resid;
		mp->mt_fileno = ui->file_number;
		mp->mt_blkno = ui->blkno;
		cycmd(dev, DO_STAT, 1);
		break;
	}

	default:
		return (ENXIO);
	}
	return (0);
}

/*
 * Dump routine.
 */
cydump(dev)
	dev_t dev;
{
	register int		unit = CYUNIT(dev);
	register int		ctlr = cydinfo[unit]->ui_ctlr;
	register unit_tab	*ui = &unit_info[unit];
	register ctlr_tab	*ci = &ctlr_info[ctlr];
	register int		blk_siz;
	register int		num = maxfree;
	register int		start = 0x800;

	if ((unit >= NCY) || cydinfo[unit]) 
		return(ENXIO);
	ui->control_proto = CW_LOCK | CW_25ips | CW_16bits;
	if (cywait(&ci->ccb))
		return(EFAULT);
	while (num > 0) {
		blk_siz = num > TBUFSIZ ? TBUFSIZ : num;
		bcopy((caddr_t)(start*NBPG), (caddr_t)ci->rawbuf,
		    (unsigned)(blk_siz*NBPG));
		ci->tpb.cmd = WRIT_TA;	
		ci->tpb.control = ui->control_proto;
		ci->tpb.status = 0;
		ci->tpb.size = MULTIBUS_SHORT(blk_siz*NBPG);
		load_mbus_addr((caddr_t)0, ci->tpb.link_ptr);
		load_mbus_addr((caddr_t)ci->rawbuf, (short *)&ci->tpb.data_ptr);
		load_mbus_addr((caddr_t)&ci->tpb, ci->ccb.tpb_ptr);
		ci->ccb.gate = GATE_CLOSED;	
		CY_ATTENTION(cyminfo[ctlr]->um_addr);
		start += blk_siz;
		num -= blk_siz;
		if (cywait(&ci->ccb))
			return(EFAULT);
		uncache(&ci->tpb);
		if (ci->tpb.status&CS_ERm)		/* error */
			return (EIO);
	}
	for(num=0; num<2; num++) {
		ci->tpb.cmd = WRIT_FM;	
		ci->tpb.control = ui->control_proto;
		ci->tpb.status = ci->tpb.size = 0;
		ci->tpb.count = MULTIBUS_SHORT(1);
		load_mbus_addr((caddr_t)0, ci->tpb.link_ptr);
		load_mbus_addr((caddr_t)ci->rawbuf, (short *)&ci->tpb.data_ptr);
		load_mbus_addr((caddr_t)&ci->tpb, ci->ccb.tpb_ptr);
		ci->ccb.gate = GATE_CLOSED;	
		CY_ATTENTION(cyminfo[ctlr]->um_addr);
		if (cywait(&ci->ccb))
			return(EFAULT);
		uncache(&ci->tpb);
		if (ci->tpb.status&CS_ERm)		/* error */
			return (EIO);
	}
	ci->tpb.cmd = REWD_OV;	
	ci->tpb.control = ui->control_proto;
	ci->tpb.status = ci->tpb.size = 0;
	ci->tpb.count = MULTIBUS_SHORT(1);
	load_mbus_addr((caddr_t)0, ci->tpb.link_ptr);
	load_mbus_addr((caddr_t)ci->rawbuf, (short *)&ci->tpb.data_ptr);
	load_mbus_addr((caddr_t)&ci->tpb, ci->ccb.tpb_ptr);
	ci->ccb.gate = GATE_CLOSED;	
	CY_ATTENTION(cyminfo[ctlr]->um_addr);
	if (cywait(&ci->ccb))
		return EFAULT;
	uncache(&ci->tpb);
	return 0;
}

/*
 * Poll until the controller is ready.
 */
cywait(cp)
	register fmt_ccb *cp;
{
	register int i = 5000;

	uncache(&cp->gate);
	while (i-- > 0 && cp->gate == GATE_CLOSED) {
		DELAY(1000);
		uncache(&cp->gate);
	}
	return (i <= 0);
}

/*
 * Load a 20 bit pointer into the i/o registers.
 */
load_mbus_addr(in, out)
	caddr_t in;
	short *out;
{
	register int tmp_in = (int)in;
	register char *out_ptr = (char *)out;

	*out_ptr++ = (char)(tmp_in & 0xff);
	*out_ptr++ = (char)((tmp_in >> 8) & 0xff);
	*out_ptr++ = (char)0;
	*out_ptr++ = (char)((tmp_in & 0xf0000) >> 12);
}

/*
**	CYMINSIZE s supposed to adjust the buffer size for any raw i/o.
**  since tapes can not read  the tail end of partial blocks we ignore
**  this request and strategy will return an appropriate error message later.
**
**	If this is not done UNIX will lose data that is on the tape.
*/
unsigned
cyminsize(bp)
	struct buf *bp;
{
	if (bp->b_bcount > MAX_BLOCKSIZE)
		bp->b_bcount = MAX_BLOCKSIZE;	
}

/*
 * Unconditionally reset all controllers to their initial state.
 */
cyreset(vba)
	int vba;
{
	register caddr_t addr;
	register int ctlr;

	for (ctlr = 0; ctlr < NCY; ctlr++)
		if (cyminfo[ctlr] && cyminfo[ctlr]->um_vbanum == vba) {
			addr = cyminfo[ctlr]->um_addr;
			CY_RESET(addr);
			if (!cy_init_controller(addr, ctlr, 0)) {
				printf("cy%d: reset failed\n", ctlr);
				cyminfo[ctlr] = NULL;
			}
		}
}
#endif
