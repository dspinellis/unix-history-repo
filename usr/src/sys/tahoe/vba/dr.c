/*	dr.c	1.2	86/11/23	*/

#include "dr.h"
#if NDR > 0

/*      DRV11-W DMA interface driver.
 */

#include "../machine/mtpr.h"
#include "../machine/pte.h"

#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "map.h"
#include "ioctl.h"
#include "buf.h"
#include "vm.h"
#include "uio.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/drreg.h"

#define YES 1
#define NO  0

struct  vba_device  *drinfo[NDR];
struct  dr_aux dr_aux[NDR];

caddr_t vtoph();
unsigned drminphys();
int     drprobe(), drintr(), drattach(), drtime(), drrwtimo();
int     drstrategy();
extern struct  vba_device  *drinfo[];
static long drstd[] = { 0 };
struct  vba_driver drdriver =
	{ drprobe, 0, drattach, 0, drstd, "rs", drinfo };
extern long hz;

#define RSUNIT(dev) (minor(dev) & 7)
#define SPL_UP spl5

/* -------- Per-unit data -------- */

extern struct dr_aux dr_aux[];

struct rs_data {
    struct buf  rs_buf;
    int         rs_ubainfo;
    short       rs_debug;
    short       rs_busy;
    short       rs_tout; 
    short       rs_uid;
    short       rs_isopen;
    short       rs_func;
} rs_data[NDR];


#ifdef DR_DEBUG
long DR11 = 0;
#endif

drprobe(reg, vi)
    caddr_t reg;
    struct vba_device *vi;
{
    register int br, cvec;		/* must be r12, r11 */
    register struct rsdevice *dr;
    register ushort status;

    dr = (struct rsdevice *)reg;
#ifdef notdef
    dr->dr_intvec = --vi->ui_hd->vh_lastiv;
#else
    dr->dr_intvec = DRINTV+vi->ui_unit;
#endif
#ifdef DR_DEBUG
    printf("dprobe: Set interrupt vector %lx and init\n",dr->dr_intvec);
#endif
    /* generate interrupt here for autoconfig */
    dr->dr_cstat = MCLR;		/* init board and device */
    status = dr->dr_cstat;		/* read initial status */
#ifdef DR_DEBUG
    printf("drprobe: Initial status %lx\n",status & 0xffff);
#endif
    br = 0x18, cvec = dr->dr_intvec;	/* XXX */
    return (sizeof (struct rsdevice));		/* DR11 exist */
}

/* ARGSUSED */
drattach(ui)
struct vba_device *ui;
{
    register struct dr_aux *rsd;

    rsd = &dr_aux[ui->ui_unit];
    rsd->dr_flags = DR_PRES;		/* This dr11 is present */
    rsd->dr_addr = (struct rsdevice *)ui->ui_addr; /* Save addr of this dr11 */
    rsd->dr_istat = 0;
    rsd->dr_bycnt = 0;
    rsd->dr_cmd = 0;
    rsd->currenttimo = 0;
    return;
}

dropen (dev, flag)
dev_t dev;
int flag;
{
    register int unit = RSUNIT(dev);
    register struct rsdevice *dr;
    register struct dr_aux *rsd;

    if ((drinfo[unit] == 0) || (!drinfo[unit]->ui_alive))
	return ENXIO;

    dr = RSADDR(unit);
    rsd = &dr_aux[unit];
    if (rsd->dr_flags & DR_OPEN) {
#ifdef DR_DEBUG
	printf("\ndropen: dr11 unit %ld already open",unit);
#endif
	return ENXIO;      		/* DR11 already open */
    }
    rsd->dr_flags |= DR_OPEN;		/* Mark it OPEN */
    rsd->dr_istat = 0;			/* Clear status of previous interrupt */
    rsd->rtimoticks = hz;		/* Set read no stall timout to 1 sec */
    rsd->wtimoticks = hz*60;		/* Set write no stall timout to 1 min */
    dr->dr_cstat = DR_ZERO;		/* Clear function & latches */
    dr->dr_pulse = (RDMA | RATN);	/* clear leftover attn & e-o-r flags */
    drtimo(dev);			/* start the self kicker */
    return 0;
}

drclose (dev)
dev_t dev;
{
    register int unit = RSUNIT(dev);
    register struct dr_aux *dra;
    register struct rsdevice *rs;
    register short s;

    dra = &dr_aux[unit];
    if (!(dra->dr_flags & DR_OPEN)) {
#ifdef DR_DEBUG
	printf("\ndrclose: DR11 device %ld not open",unit);
#endif
	return;
    }
    dra->dr_flags &= ~(DR_OPEN|DR_ACTV);
    rs = dra->dr_addr;
    s=SPL_UP();
    rs->dr_cstat = DR_ZERO;
    if (dra->dr_buf.b_flags & B_BUSY) {
    	dra->dr_buf.b_flags &= ~B_BUSY;
	wakeup(&dra->dr_buf.b_flags);
    }
    splx(s);
    return;
}


/*	drread() works exactly like drwrite() except that the
	B_READ flag is used when physio() is called
*/
drread (dev, uio)
dev_t dev;
struct uio *uio;
{	register struct dr_aux *dra;
	register struct buf *bp;
	register long spl, err;
    	register int unit = RSUNIT(dev);

    if (   uio->uio_iov->iov_len <= 0		/* Negative count */
	|| uio->uio_iov->iov_len & 1		/* odd count */
	|| (int)uio->uio_iov->iov_base & 1	/* odd destination address */
       )
	return EINVAL;

#ifdef DR_DEBUG
    if (DR11 & 8) {
	printf("\ndrread: (len:%ld)(base:%lx)",
    		uio->uio_iov->iov_len,(int)uio->uio_iov->iov_base); 
    }
#endif

    dra = &dr_aux[RSUNIT(dev)];
    dra->dr_op = DR_READ;
    bp =  &dra->dr_buf;
    bp->b_resid = 0;
    if (dra->dr_flags & DR_NORSTALL) {
	/* We are in no stall mode, start the timer, raise IPL so nothing
	   can stop us once the timer's running */
	spl = SPL_UP();
	timeout(drrwtimo,(caddr_t)((dra->currenttimo<<8) | unit),
				dra->rtimoticks);
    	err = physio (drstrategy, bp, dev,B_READ, drminphys, uio);
	splx(spl);
	if (err)
		return(err);
	dra->currenttimo++;		/* Update current timeout number */
	/* Did we timeout */
	if (dra->dr_flags & DR_TMDM) {
		dra->dr_flags &= ~DR_TMDM;	/* Clear timeout flag */
		u.u_error = 0;		/* Made the error ourself, ignore it */
	}
    }
    else {
    	return physio (drstrategy, bp, dev,B_READ, drminphys, uio);
    }
}

drwrite (dev, uio)
dev_t dev;
struct uio *uio;
{	register struct dr_aux *dra;
	register struct buf *bp;
    	register int unit = RSUNIT(dev);
	register long spl, err;

    if (   uio->uio_iov->iov_len <= 0
	|| uio->uio_iov->iov_len & 1
	|| (int)uio->uio_iov->iov_base & 1
       )
	return EINVAL;

#ifdef DR_DEBUG
    if (DR11 & 4) {
	printf("\ndrwrite: (len:%ld)(base:%lx)",
    		uio->uio_iov->iov_len,(int)uio->uio_iov->iov_base); 
    }
#endif

    dra = &dr_aux[RSUNIT(dev)];
    dra->dr_op = DR_WRITE;
    bp =  &dra->dr_buf;
    bp->b_resid = 0;
    if (dra->dr_flags & DR_NOWSTALL) {
	/* We are in no stall mode, start the timer, raise IPL so nothing
	   can stop us once the timer's running */
	spl = SPL_UP();
	timeout(drrwtimo,(caddr_t)((dra->currenttimo<<8) | unit),
				dra->wtimoticks);
    	err = physio (drstrategy, bp, dev,B_WRITE, drminphys, uio);
	splx(spl);
	if (err)
		return(err);
	dra->currenttimo++;		/* Update current timeout number */
	/* Did we timeout */
	if (dra->dr_flags & DR_TMDM) {
		dra->dr_flags &= ~DR_TMDM;	/* Clear timeout flag */
		u.u_error = 0;		/* Made the error ourself, ignore it */
	}
    }
    else {
    	return physio (drstrategy, bp, dev,B_WRITE, drminphys, uio);
    }
}

/*  Routine used by calling program to issue commands to dr11 driver and 
    through it to the device.
    It is also used to read status from the device and driver and to wait 
    for attention interrupts.
    Status is returned in an 8 elements unsigned short integer array, the 
    first two elements of the array are also used to pass arguments to 
    drioctl() if required.
    The function bits to be written to the dr11 are included in the cmd
    argument. Even if they are not being written to the dr11 in a particular
    drioctl() call, they will update the copy of cmd that is stored in the
    driver. When drstrategy() is called, this updated copy is used if a 
    deferred function bit write has been specified. The "side effect" of
    calls to the drioctl() requires that the last call prior to a read or
    write has an appropriate copy of the function bits in cmd if they are
    to be used in drstrategy().
    When used as command value, the contents of data[0] is the command
    parameter.
*/

drioctl(dev, cmd, data, flag)
dev_t dev;
int cmd;
long *data;
int flag;
{
    register int unit = RSUNIT(dev);
    register struct dr_aux *dra;
    register struct rsdevice *rsaddr = RSADDR(unit);
    struct dr11io dio;
    ushort s, errcode, status;
    long temp;

#ifdef DR_DEBUG
    if (DR11 & 0x10)
    printf("\ndrioctl: (dev:%lx)(cmd:%lx)(data:%lx)(data[0]:%lx)",
	dev,cmd,data,data[0]);
#endif

    dra = &dr_aux[unit];
    dra->dr_cmd = 0;		/* Fresh copy; clear all previous flags */

    switch (cmd) {

    case DRWAIT:
	/* Wait for attention interrupt */
#ifdef DR_DEBUG
	printf("\ndrioctl: wait for attention interrupt");
#endif
	s = SPL_UP();
	/* If the attention flag in dr_flags is set, it probably means that
	   an attention has arrived by the time a previous DMA end-of-range
	   interrupt was serviced. If ATRX is set, we will return with out
	   sleeping, since we have received an attention since the last call
	   to wait on attention.
	   This may not be appropriate for some applications.
	*/
	if (!(dra->dr_flags & DR_ATRX)) {
		dra->dr_flags |= DR_ATWT;	/* Set waiting flag */
		rsaddr->dr_pulse = IENB;	/* Enable interrupt; use pulse
						   reg. so function bits are
						   not changed */
		sleep((caddr_t)&dra->dr_cmd,DRPRI);
	}
	splx(s);
	break;

    case DRPIOW:
	/* Write to p-i/o register */
	rsaddr->dr_data = data[0];
	break;

    case DRPACL:
	/* Send pulse to device */
	rsaddr->dr_pulse = FCN2;
	break;

    case DRDACL:
	/* Defer alco pulse until go */
	dra->dr_cmd |= DR_DACL;
	break;

    case DRPCYL:
	/* Set cycle with next go */
	dra->dr_cmd |= DR_PCYL;
	break;

    case DRDFCN:
	/* Do not update function bits until next go issued */
	dra->dr_cmd |= DR_DFCN;
	break;

    case DRRATN:
	/* Reset attention flag -- use with extreme caution */
	rsaddr->dr_pulse = RATN;
	break;

    case DRRDMA:
	/* Reset DMA e-o-r flag -- should never used */
	rsaddr->dr_pulse = RDMA;
	break;

    case DRSFCN:
	/* Set function bits */
	temp = data[0] & DR_FMSK;
	rsaddr->dr_cstat = temp;	/* Write to control register */
	/* This has a very important side effect -- It clears the interrupt
	   enable flag. That is fine for this driver, but if it is desired
	   to leave interrupt enable at all times, it will be necessary to
	   to read the status register first to get IENB, or carry a software
	   flag that indicates whether interrupts are set, and or this into 
	   the controll register value being written.
	*/
	break;

    case DRRPER:
	/* Clear parity flag */
	rsaddr->dr_pulse = RPER;
	break;

    case DRSETRSTALL:
	/* Set read stall mode. */
	dra->dr_flags &= (~DR_NORSTALL);
	break;

    case DRSETNORSTALL:
	/* Set no stall read  mode. */
	dra->dr_flags |= DR_NORSTALL;
	break;

    case DRGETRSTALL:
	/* Returns true if in read stall mode. */
	data[0]  = (dra->dr_flags & DR_NORSTALL)? 0 : 1;
	break;

    case DRSETRTIMEOUT:
	/* Set the number of ticks before a no stall read times out.
	   The argument is given in tenths of a second. */
	if (data[0] < 1) {
		u.u_error = EINVAL;
		temp = 1;
	}
	dra->rtimoticks = (data[0] * hz )/10;
	break;

    case DRGETRTIMEOUT:
	/* Returns the number of tenths of seconds before
	   a no stall read times out. */
	/* The argument is given in tenths of a second. */
	data[0] = ((dra->rtimoticks)*10)/hz;
	break;

    case DRSETWSTALL:
	/* Set write stall mode. */
	dra->dr_flags &= (~DR_NOWSTALL);
	break;

    case DRSETNOWSTALL:
	/* Set write stall mode. */
	dra->dr_flags |= DR_NOWSTALL;
	break;

    case DRGETWSTALL:
	/* Returns true if in write stall mode. */
	data[0] = (dra->dr_flags & DR_NOWSTALL)? 0 : 1;
	break;

    case DRSETWTIMEOUT:
	/* Set the number of ticks before a no stall write times out.
	   The argument is given in tenths of a second. */
	if (data[0] < 1) {
		u.u_error = EINVAL;
		temp = 1;
	}
	dra->wtimoticks = (data[0] * hz )/10;
	break;

    case DRGETWTIMEOUT:
	/* Returns the number of tenths of seconds before
	   a no stall write times out. */
	/* The argument is given in tenths of a second. */
	data[0] = ((dra->wtimoticks)*10)/hz;
	break;

    case DRWRITEREADY:
	/* Returns a value of 1 if the device can accept
	   data, 0 otherwise. Internally this is the
	   DR11-W STAT A bit. */

	data[0] = (rsaddr->dr_cstat & STTA)? 1 : 0;
	break;

    case DRREADREADY:
	/* Returns a value of 1 if the device has data
	   for host to be read, 0 otherwise. Internally
	   this is the DR11-W STAT B bit. */
	data[0] = (rsaddr->dr_cstat & STTB)? 1 : 0;
	break;

    case DRBUSY:
	/* Returns a value of 1 if the device is busy,
	   0 otherwise. Internally this is the DR11-W
	   STAT C bit, but there is a bug in the Omega 500/FIFO interface
	   board that it cannot drive this signal low for certain DR11-W
	   ctlr such as the Ikon. We use the REDY signal of the CSR on
	   the Ikon DR11-W instead. 

	data[0] = (rsaddr->dr_cstat & STTC)? 1 : 0;
	*/

	data[0] = ((rsaddr->dr_cstat & REDY)? 0 : 1);
	break;

    case DRRESET:
	rsaddr->dr_pulse = (MCLR|RDMA|RATN|RPER);/* Reset DMA ATN RPER flag */
	DELAY(0x1f000);
	while (!(rsaddr->dr_cstat & REDY)) {
		sleep((caddr_t)dra, DRPRI);	/* Wakeup by drtimo() */
	}
    	dra->dr_istat = 0;
    	dra->dr_cmd = 0;
    	dra->currenttimo = 0;
	break;

    default:
	printf("\ndrioctl: Invalid ioctl cmd : %lx",cmd);
	return EINVAL;
    }

#ifdef DR_DEBUG
    if (DR11 & 0x10)
    	printf("**** (data[0]:%lx)",data[0]);
#endif
    return 0;
}

/* Reset state on Unibus reset */
drreset(uban)
int uban;
{
    register int i;
    register struct vba_device *ui;
    register struct dr_aux *dra;

    for (i = 0; i < NDR; i++, dra++) {
	if (   (ui = drinfo[i]) == 0
	    || !ui->ui_alive
	    || ui->ui_vbanum != uban
	   )
	    continue;
	printf("\ndrreset: %ld",i);
	/* Do something; reset board */
    }
    return;
}

/*
 * An interrupt is caused either by an error,
 * base address overflow, or transfer complete
 */
drintr (unit)
register long unit;
{
    register struct dr_aux *dra = &dr_aux[unit];
    register struct rsdevice *rsaddr = RSADDR(unit);
    register struct buf *bp;
    register short status, csrtmp;

    status = rsaddr->dr_cstat & 0xffff;		/* get board status register */
    dra->dr_istat = status;

#ifdef DR_DEBUG
    if (DR11 & 2)
    	printf("\ndrintr: dr11 status : %lx",status & 0xffff);
#endif

    if (dra->dr_flags & DR_LOOPTST) {
	/* Controller is doing loopback test */
    	dra->dr_flags &= ~DR_LOOPTST;
	return;
    }

    /* Make sure this is not a stray interrupt; at least one of dmaf or attf
       must be set. Note that if the dr11 interrupt enable latch is reset 
       during a hardware interrupt ack sequence, and by the we get to this 
       point in the interrupt code it will be 0. This is done to give the
       programmer some control over how the two more-or-less independent
       interrupt sources on the board are handled.
       If the attention flag is set when drstrategy() is called to start a
       dma read or write an interrupt will be generated as soon as the
       strategy routine enables interrupts for dma end-of-range. This will
       cause execution of the interrupt routine (not necessarily bad) and
       will cause the interrupt enable mask to be reset (very bad since the
       dma end-of-range condition will not be able to generate an interrupt
       when it occurs) causing the dma operation to time-out (even though
       the dma transfer will be done successfully) or hang the process if a
       software time-out capability is not implemented. One way to avoid 
       this situation is to check for a pending attention interrupt (attf
       set) by calling drioctl() before doing a read or a write. For the
       time being this driver will solve the problem by clearing the attf
       flag in the status register before enabling interrupts in drstrategy().

       **** The IKON 10084 for which this driver is written will set both
       attf and dmaf if dma is terminated by an attention pulse. This will
       cause a wakeup(&dr_aux), which will be ignored since it is not being 
       waited on, and an iodone(bp) which is the desired action. Some other
       dr11 emulators, in particular the IKON 10077 for the Multibus, donot
       dmaf in this case. This may require some addtional code in the inter-
       rupt routine to ensure that en iodone(bp) is issued when dma is term-
       inated by attention.
    */

    bp = dra->dr_actf;
    if (!(status & (ATTF | DMAF))) {
	printf("\ndrintr: Stray interrupt, dr11 status : %lx",status);
	return;
    }
    if (status & DMAF) {
	/* End-of-range interrupt */
	dra->dr_flags |= DR_DMAX;

#ifdef DR_DEBUG
    if (DR11 & 2)
	printf("\ndrintr: e-o-r interrupt,cstat:%lx,dr_flags:%lx",
		status&0xffff,dra->dr_flags & DR_ACTV);
#endif
	if (!(dra->dr_flags & DR_ACTV)) {
		/* We are not doing DMA !! */
		bp->b_flags |= B_ERROR;
	}
	else {
		if (dra->dr_op == DR_READ) mtpr(bp->b_un.b_addr,P1DC);
		dra->dr_bycnt -= bp->b_bcount;
		if (dra->dr_bycnt >0) {
			bp->b_un.b_addr += bp->b_bcount;
			bp->b_bcount = (dra->dr_bycnt > NBPG) ? NBPG:
					dra->dr_bycnt;
			drstart(rsaddr,dra,bp);
			return;
		}
	}
	dra->dr_flags &= ~DR_ACTV;
	wakeup(dra);			/* Wakeup proc waiting in drwait() */
	rsaddr->dr_pulse = (RPER|RDMA|RATN);	/* reset dma e-o-r flag */
    }

    /* Now test for attention interrupt -- It may be set in addition to 
       the dma e-o-r interrupt. If we get one we will issue a wakeup to
       the drioctl() routine which is presumable waiting for one.
       The program may have to monitor the attention interrupt received
       flag in addition to doing waits for the interrupt. Futhermore, 
       interrupts are not enabled unless dma is in progress or drioctl()
       has been called to wait for attention -- this may produce some
       strange results if attf is set on the dr11 when a read or a write
       is initiated, since that will enables interrupts.
       **** The appropriate code for this interrupt routine will probably
       be rather application dependent.
    */

    if (status & ATTF) {
	dra->dr_flags |= DR_ATRX;
	dra->dr_flags &= ~DR_ATWT;
	rsaddr->dr_cstat = RATN;	/* reset attention flag */
	wakeup((caddr_t)&dra->dr_cmd);
	/* Some applications which use attention to terminate dma may also
	   want to issue an iodone() here to wakeup physio().
 	*/
    }
    return;
}

unsigned
drminphys(bp)
struct buf *bp;
{
    if (bp->b_bcount > 65536)
	bp->b_bcount = 65536;
}

/*
 *  This routine performs the device unique operations on the DR11W
 *  it is passed as an argument to and invoked by physio
 */
drstrategy (bp)
register struct buf *bp;
{
    register int s;
    int unit = RSUNIT(bp->b_dev);
    register struct rsdevice *rsaddr = RSADDR(unit);
    register struct dr_aux *dra = &dr_aux[unit];
    register short go = 0;
    register long baddr, ok;
#ifdef DR_DEBUG
    register char *caddr;
    long drva();
#endif


    if (!(dra->dr_flags & DR_OPEN)) {
	/* Device not open */
	bp->b_error = ENXIO;
	bp->b_flags |= B_ERROR;
	iodone (bp);
	return;
    }

    while (dra->dr_flags & DR_ACTV) {
	/* Device is active; should never be in here... */
	sleep((caddr_t)&dra->dr_flags,DRPRI);
    }

    dra->dr_actf = bp;

#ifdef DR_DEBUG
    drva(dra,bp->b_proc,bp->b_un.b_addr,bp->b_bcount);
#endif

    dra->dr_oba = bp->b_un.b_addr;	/* Save original addr, count */
    dra->dr_obc = bp->b_bcount;
    dra->dr_bycnt = bp->b_bcount;	/* Save xfer count used by drintr() */

    if ((((long)bp->b_un.b_addr & 0x3fffffff) >> PGSHIFT) !=
	((((long)bp->b_un.b_addr & 0x3fffffff) + bp->b_bcount) >> PGSHIFT)) {
    	bp->b_bcount = NBPG - (((long)bp->b_un.b_addr) & PGOFSET);
    }

    dra->dr_flags |= DR_ACTV;	/* Mark it active (use in intr handler) */
    s = SPL_UP();
    drstart(rsaddr,dra,bp);
    splx(s);

    ok = drwait(rsaddr,dra);
#ifdef DR_DEBUG
    if (DR11 & 0x40) {
	caddr = (char *)dra->dr_oba;
    	if (dra->dr_op == DR_READ)
		printf("\nAfter read: (%lx)(%lx)",caddr[0]&0xff,caddr[1]&0xff);
    }
#endif
    dra->dr_flags &= ~DR_ACTV;		/* Clear active flag */
    bp->b_un.b_addr = dra->dr_oba;	/* Restore original addr, count */
    bp->b_bcount = dra->dr_obc;

    if (!ok) bp->b_flags |= B_ERROR;
    iodone(bp);				/* Mark buffer B_DONE,so physstrat()
					   in ml/machdep.c won't sleep */
    wakeup((caddr_t)&dra->dr_flags);

    /* Return to the calling program (physio()). Physio() will sleep
       until awaken by a call to iodone() in the interupt handler --
       which will be called by the dispatcher when it receives dma
       end-of-range interrupt.
    */
    return;
}

drwait(rs,dr)
register struct rsdevice *rs;
register struct dr_aux *dr;
{
	register long status, s;

	s = SPL_UP();
    	while (dr->dr_flags & DR_ACTV)
		sleep((caddr_t)dr,DRPRI);
	splx(s);

	if (dr->dr_flags & DR_TMDM) {
		/* DMA timed out */
		dr->dr_flags &= ~DR_TMDM;
		return(0);
	}
	else {
		if (rs->dr_cstat & (PERR|BERR|TERR)) {
			(dr->dr_actf)->b_flags |= B_ERROR;
			return(0);
		}
	}
	dr->dr_flags &= ~DR_DMAX;
	return(1);
}


drrwtimo(tinfo)
register unsigned long tinfo;
/*
 * 	The lower 8-bit of tinfo is the minor device number, the
 *	remaining higher 8-bit is the current timout number
*/
{	register long unit = tinfo & 0xff;
	register struct dr_aux *dr = &dr_aux[unit];
	register struct rsdevice *rs = dr->dr_addr;

	/* If this is not the timeout that drwrite/drread is waiting
	   for then we should just go away */
	if ((tinfo & (~0xff)) != (dr->currenttimo << 8)) return;

	/* Mark the device timed out */
	dr->dr_flags |= DR_TMDM;
	dr->dr_flags &= ~DR_ACTV;
	rs->dr_pulse = RMSK;			/* Inihibit interrupt */
	rs->dr_pulse = (RPER|RDMA|RATN|IENB);	/* Clear DMA logic */

	/* Some applications will not issue a master after dma timeout,
	   since doing so sends an INIT H pulse to the external device,
	   which may produce undesirable side-effects.  */

	/* Wake up process waiting in drwait() and flag the error */
	(dr->dr_actf)->b_flags |= B_ERROR;
	wakeup((caddr_t)dr->dr_cmd);
}


/*
 *	Kick the driver every second
*/
drtimo(dev)
dev_t dev;
{
    	register int unit = RSUNIT(dev);
	register struct dr_aux *dr;

    	dr = &dr_aux[unit];
	if (dr->dr_flags & DR_OPEN)
		timeout(drtimo,(caddr_t)dev,hz);
	wakeup((caddr_t)dr);	/* Wakeup any process waiting for interrupt */
}


#ifdef DR_DEBUG

drva(dra,p,va,bcnt)
struct dr_aux *dra;
struct proc *p;
char *va;
long bcnt;
{	register long first, last , np;

	if (DR11 & 0x20)  {
		first = ((long)(vtoph(p,va))) >> 10;
		last = ((long)(vtoph(p,va+bcnt))) >> 10;
		np = bcnt / 0x3ff;
		printf("\ndrva: (op:%ld)(first:%ld)(last:%ld)(np:%ld)(cnt:%ld)",
			dra->dr_op,first,last,np,bcnt);
	}
}
#endif


drstart(rsaddr,dra,bp)
register struct rsdevice *rsaddr;
register struct dr_aux *dra;
register struct buf *bp;
{	register long baddr;
	ushort go;
	register char *caddr;

#ifdef DR_DEBUG
	if ((dra->dr_op == DR_READ) && (DR11 & 8)) {
		printf("\ndrstart: READ, bcnt:%ld",bp->b_bcount);
    		caddr = (char *)bp->b_un.b_addr;
		printf(",(%lx)(%lx)",caddr[0]&0xff,caddr[1]&0xff);
	}
#endif
    /* we are doing raw IO, bp->b_un.b_addr is user's address */
    baddr = (long)vtoph(bp->b_proc,(caddr_t)bp->b_un.b_addr);

    /* Set DMA address into DR11 interace registers: DR11 requires that
       the address be right shifted 1 bit position before it is written
       to the board (The board will left shift it one bit position before
       it places the address on the bus
    */
    rsaddr->dr_walo = (ushort)((baddr >> 1) & 0xffff);
    rsaddr->dr_wahi = (ushort)((baddr >> 17) & 0x7fff);

    /* Set DMA range count: (number of words - 1) */
    rsaddr->dr_range = (ushort)((bp->b_bcount >> 1) - 1);

    /* Set address modifier code to be used for DMA access to memory */
    rsaddr->dr_addmod = (char)DRADDMOD;

    /* Now determine whether this is a read or a write. ***** This is
       probably only usefull for link mode operation, since dr11 doesnot
       controll the direction of data transfer. The C1 control input 
       controls whether the hardware is doing a read or a write. In link
       mode this is controlled by function 1 latch (looped back by the
       cable) and could be set the program. In the general case, the dr11
       doesnot know in advance what the direction of transfer is - although
       the program and protocol logic probably is
    */

#ifdef DR_DEBUG
   if (DR11 & 1)
    printf("\ndrstrat: about to GO..,dr_cmd:%lx,drstat:%lx,drcnt:%ld,cdata:%lx,OP:%ld",
	dra->dr_cmd,rsaddr->dr_cstat,rsaddr->dr_range,rsaddr->dr_data,dra->dr_op);
#endif

    /* Update function latches may have been done already by drioctl() if
       request from drioctl()
    */
    if (dra->dr_cmd & DR_DFCN) {
	/* deferred function write */
    	dra->dr_cmd &= ~DR_DFCN;	/* Clear request */
	go = dra->dr_cmd & DR_FMSK;	/* mask out fcn bits */
	rsaddr->dr_cstat = go;		/* Write it to the board */
    }

    /* Clear dmaf and attf to assure a clean dma start */
    rsaddr->dr_pulse = (ushort)(RATN|RDMA|RPER);
    rsaddr->dr_cstat = (ushort)(IENB|GO|CYCL|dra->dr_op); /* GO...... */

    /* Now check for software cycle request -- usually by transmitter in
       link mode.
    */
    if (dra->dr_cmd & DR_PCYL) {
    	dra->dr_cmd &= ~DR_PCYL;	/* Clear request */
	rsaddr->dr_pulse = CYCL;	/* Use pulse register again */
    }

    /* Now check for deferred ACLO FCNT2 pulse request -- usually to tell
       the transmitter (via its attention) that we have enabled dma.
    */
    if (dra->dr_cmd & DR_DACL) {
    	dra->dr_cmd &= ~DR_DACL;	/* Clear request */
	rsaddr->dr_pulse = FCN2;	/* Use pulse register again */
    }
}

#endif  NDR
