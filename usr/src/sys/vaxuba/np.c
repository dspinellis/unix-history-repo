/*
 * Copyright (c) 1986 MICOM-Interlan, Inc., Boxborough Mass
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)np.c	7.7 (Berkeley) 6/21/90
 *
 * From:
 *	np.c version 1.5
 *
 *	This version retrieved: 8/18/86 @ 18:58:54
 *	    This delta created: 8/18/86 @ 18:19:24
 *
 *	static		char	*SCCSID = "@(#)np.c	1.5";
 *
 */

		/******************************************
 		*					  *
 		*		NPDRIVER		  *
 		*					  *
 		******************************************/

/*
 * The NP Driver is used to route requests, independent of protocol type,
 * to the NP series Intelligent Board. The facilities it provides are
 * used for board maintainance by the superuser and by protocol pseudo-drivers, 
 * such as WN, for sending requests to a board. The board maintainance and
 * control functions are accessed via npioctl() by the NP support utilities.
 */

/*
 * Modification History:
 * 4/9/86 DDW Removed pseudo-driver initialization flag resets from NpReset
 * 5/28/86 CJM Changed iodone() to wakeup() in NpProcQueue().
 *
 */

/*
 * Include Files
 */

#include "np.h"
#if NNP > 0
#include "param.h"
#include "buf.h"
#include "ubavar.h"
#include "signal.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "uio.h"
#include "errno.h"

#include "../vaxuba/npreg.h"

#define b_uio b_forw
#define b_rp  av_back
/*
 * Global variables for pseudo-drivers.
 */

int WnInitFlag = 0;
int IsInitFlag = 0;
int (*IxAttach)();
int (*IxReset)();

/*
 * Debugging level.
 */

int	NpDebug = 0;		

/* Driver Wide State used by the ICP */

int NpState = NPCLEAR;


/*
 * Master structure, one per board, contains request queue header,
 * shared memory address, and memory mapping information.
 */

struct npmaster npmasters[NNP];

/* Structure of the shared memory area */

static struct npspace npspaces[NNP];

/* Panic Message data structures */

static int panicmap;			/* Mapping information */
static char	NpPbuf[PANLEN] = 0;	/* Panic message buffer */
static caddr_t pstring;			/* Panic string address on board, absolute */
static unsign16 panaddr[2];		/* Panic string address on board (seg/offset) */

/* Driver Wide Connection Table */

static struct npconn npcnxtab[NNP][NNPCNN];

/* Head of the request queue, one per board */

static struct npreq reqhdr[NNP];

/* Require for diagnostic packages */

typedef struct npreq *reqptr;
reqptr np_mapreq[NNP];

/* The request structures, one pool per board */

static struct npreq npreqs[NNP][NUMCQE];


/*
 * Data structures needed for BSD 4.2 Device Drivers
 */

int	npprobe(), npattach(), npintr();
struct	uba_device *npdinfo[NNP];

/* UNIBUS address of Network Processors */

u_short	npstd[] = { 0166000, 0166020, 0 };

/* Interrupt vectors used by the Network Processors */

static unsign16 npvectors[NNP];

struct	uba_driver npdriver =
    { npprobe, 0, npattach, 0, npstd, "np", npdinfo };
struct	buf	np_tab[NNP];
static unsigned long np_icount[NNP];


/*
 * External function and data structure declarations.
 */

struct npreq * NpGetReq();
struct npmaster	*NpBoardChange();
int NpTimer();
struct CQE * NpRemCQE();

extern struct user u;

/*
 * Np_init() is responsible for hardware initializiation and the software 
 * initialization of the connection table and driver software data structures.
 */

npinit(unit)
int unit;
{
	register int j;


		/* Software Initialization */

	npmasters[unit].flags = NPCLEAR;

	NpSWinit(unit);

		/* Hardware Initialization */

	NpHWinit(unit);		

		/* Connection Table Initialization */

	for(j=0;j<NNPCNN;j++) {
		npcnxtab[unit][j].protocol = NPCLCONN;
		npcnxtab[unit][j].unit = &npmasters[unit];
	}
}

/*
 * Np_open establishes a connection to the NP Driver using the minor
 * device number as an identifier. A default protocol, NPMAINT, is assigned
 * with the specified unit. Protocol and unit may be changed using the 
 * NpProtChange and NpBoardChange functions.
 * Since the maintainance protocol does not need a working I-Board, entries
 * are always made in the Connection Table, npcnxtab, if the board exists.
 */

/*ARGSUSED*/
npopen(dev,flag)
dev_t dev;
int flag;
{
	int unit;
	unsign16 conn;
	struct npmaster *mp;
	int error;

	if(NpDebug & DEBENTRY)
		printf("npopen\n");

	/* Clear error */

	error = 0;

	/* Make sure it's the superuser */

	if(u.u_uid) 
		return(EPERM);
	
	/* Get the connection identifier */

	if(((conn = NPCONN(dev)) >= NNPCNN) ||
	    ((unit = NPUNIT(dev)) >= NNP)) 
		return(ENODEV);
	

	if(NpDebug  & DEBOPEN)
		printf("conn = %x unit = %d\n",conn,unit);

	/* Get the board for the specified unit */

	mp = NpBoardChange(NPMAINT,unit);

	if(mp != (struct npmaster *) 0) {
		npcnxtab[unit][conn].unit = mp;
		npcnxtab[unit][conn].protocol = NPMAINT;
	}
	else error = ENXIO;

	if(NpDebug & DEBENTRY)
		printf("npopen...\n");

	return(error);
}

/*
 * Np_close is responsible updating the connection table for
 * that connection by marking it closed.
 */

npclose(dev)
dev_t dev;
{

	if(NpDebug & DEBENTRY)
		printf("npclose\n");

	/* Get the connection identifier */

	npcnxtab[NPUNIT(dev)][NPCONN(dev)].protocol = NPCLCONN;

	if(NpDebug & DEBENTRY)
		printf("npclose...\n");

	return(0);

}

/*
 * Npioctl is the main conduit of commands between the I-Board and the
 * NP support utilities. Relevant information for the request is found in the
 * cmd and addr parameters. Cmd specifies the function to perform, addr is 
 * command specific. Npioctl returns 0 if successful, or an error number
 * (which winds up in errno).
 */

/*ARGSUSED*/
npioctl(dev,cmd,addr,flag)
dev_t dev;
int cmd;
caddr_t *addr;
int flag;
{
	unsign16 protocol;
	unsign16 conn;
	unsign16 unit;
	int error;

	register struct npmaster *mp;
	register struct npreq *rp;
	unsigned usrarg;

	if(NpDebug & DEBENTRY)
		printf("npioctl\n");

	/* Clear error */

	error = 0;

	/* Strip off IOC_VOID bit */

	cmd &= CMDMASK;

	/* Get connection identifier */

	conn = NPCONN(dev);
	unit = NPUNIT(dev);

	/* Master pointer for this unit */

	mp = npcnxtab[unit][conn].unit;

	protocol = npcnxtab[unit][conn].protocol;

	/* Get a request structure from the pool and initialize it */

	while((rp = NpGetReq(mp->reqtab)) == NULL) {
		mp->reqtab->flags |= WANTREQ;
		sleep((caddr_t)(mp->reqtab),PZERO -1);
	}

	if(NpDebug & DEBREQ)
		printf("NP Reqp is %x\n",rp);

	/* Initializations of request structure */

	rp->intr = (int (*)())0;	/* Do not call interrupt routine */
	rp->bufoffset = 0;		/* Offset into data buffer */
	rp->procp = u.u_procp; 	/* Process structure for this user */

	/* Copy in user's argument to ioctl() call */

	if(error = copyin(*addr,&usrarg,sizeof(usrarg)))
		return(error);
	

	if(NpDebug & DEBIOCTL)
		printf("arg = %x\n",usrarg);

	/* Execute the specified command */

	switch(cmd) {

	    case NPSETPROT:
	    	if((error = NpProtChange(usrarg,mp->unit)) == 0)
			npcnxtab[unit][conn].protocol = usrarg;
		break;
	    case NPSETBOARD:
		if(mp = NpBoardChange(protocol,usrarg))
			npcnxtab[unit][conn].unit = mp;
		else {
			mp = npcnxtab[unit][conn].unit;
			error = ENXIO;
		}
		break;
	    case NPRESET:
		error = NpReset(mp,rp);
		break;
	    case NPSETNPDEB:
		NpDebug = usrarg;
		break;
	    case NPINIT:
		error = NpSWinit(mp->unit);
		break;
	    case NPSTART:

#ifdef OLDROM
		/*
		 * Kludge to work around I-Board boot from Host. Read two bytes
		 * from the board into the Device Configuration Word
		 * in Shared Memory.
		 */

		NPIO(mp,(paddr_t)0x500,(paddr_t)(&mp->shmemp->statblock.sb_dcw),2,B_READ);

		mp->shmemp->statblock.sb_drw = 0;
#endif

		/* Set the Address at which to begin On-Board execution */

		error = NpSetXeqAddr(mp,(caddr_t)usrarg);
		break;
	    case NPSTATS:
		error = NpStats();
		break;
	    case NPGPANIC:
		error = copyout((caddr_t)NpPbuf,*addr,PANLEN);

		/* Clear panic request flag and leave */

		mp->flags &= ~PANICREQ;
		break;
	    case NPPOLL:
		error = NpPoll(mp,*addr);
		break;
	    case NPKILL:
		error = NpKill(mp,rp);
		break;
	    case NPSETADDR:
		error = NpSetMemAddr(mp,*addr);
		break;
	    case NPRCSR0:
		usrarg = RCSR0(mp->iobase);
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPRCSR1:
		usrarg = RCSR1(mp->iobase);
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPRCSR2:
		usrarg = RCSR2(mp->iobase);
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPRCSR3:
		usrarg = RCSR3(mp->iobase);
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPWCSR0:
		WCSR0(mp->iobase,usrarg);
		break;
	    case NPWCSR1:
		WCSR1(mp->iobase,usrarg);
		break;
	    case NPWCSR2:
		WCSR2(mp->iobase,usrarg);
		break;
	    case NPWCSR3:
		WCSR3(mp->iobase,usrarg);
		break;
	    case NPNETBOOT:
		error = NpSetIntLevel(mp,mp->vector);
		if(error) break;
		error = NpSetXeqAddr(mp,(caddr_t)INETBOOT);
		break;
	    case NPSETLAST:
		if (usrarg)
			mp->flags &= ~LSTCMD;
		else
			mp->flags |= LSTCMD;
		break;
	    case NPCLRICNT:
		np_icount[unit] = NPCLEAR;
		break;
	    case NPGETICNT:
		usrarg = np_icount[unit];
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPGETIVEC:
		usrarg = mp->vector;
		error = copyout((caddr_t)&usrarg,*addr,sizeof(usrarg));
		break;
	    case NPMAPMEM:
		error = NpMem(mp, rp, *addr);
		break;
	    default:
		printf("Bad Maintenance command: %d!\n",cmd);
		error = EIO;
		break;
	}
	if((cmd != NPRESET) && (cmd != NPINIT) && (cmd != NPMAPMEM))
		NpFreeReq(mp->reqtab,rp);

	if(NpDebug & DEBENTRY)
		printf("npioctl...\n");

	return(error);
}

/*
 * np_start - start io activity
 */
npstart(mp)
register struct npmaster *mp;
{

	register struct uio 	*uio;
	register struct buf	*bp;
	register struct npreq	*rp;

	int error;			/* Return from NPIO call */

	if(NpDebug & DEBENTRY)
		printf("npstart\n");

	if((bp = np_tab[mp->unit].b_actf) == (struct buf *)0) {
		np_tab[mp->unit].b_active = 0;
		return;
	}
	if((rp = (struct npreq *)(bp->b_rp)) == (struct npreq *)0) {
		bp->b_flags = B_ERROR;
		iodone(bp);
		return;
	}
	if ((uio = (struct uio *)bp->b_uio) == (struct uio *)0) {
		bp->b_flags = B_ERROR;
		iodone(bp);
		return;
	}
	np_tab[mp->unit].b_active = 1;

	if(NpDebug & DEBIO)
		printf("NP IO src %x dst = %x cnt = %x\n", bp->b_un.b_addr,
			uio->uio_offset, bp->b_bcount);

	/* Send the request to the board via the CSR0 command interface */

	if(bp->b_flags & B_READ) 
		error = NPIO(mp, (paddr_t)uio->uio_offset, (paddr_t)rp->bufaddr,
	    		bp->b_bcount, (bp->b_flags & B_READ)); 
	else
		error = NPIO(mp, (paddr_t)rp->bufaddr, (paddr_t)uio->uio_offset,
			bp->b_bcount, (bp->b_flags & B_READ)); 
	

	/* Check return from I/O */

	if(error) {
		bp->b_flags |= B_ERROR;
		np_tab[mp->unit].b_actf = bp->av_forw;
		if(NpDebug & DEBIO)
			printf("NPIO return error: b_flags is %x \n",bp->b_flags);
		iodone(bp);
	}

	if(NpDebug & DEBENTRY)
		printf("npstart...\n");

}
/*
 * npstrategy - the strategy routine
 */

npstrategy(bp)
register struct buf *bp;
{

	register struct buf *ip;	/* quick pointer */
	register struct npmaster *mp;	/* master structure for this device */
	register struct npreq *rp;	/* reqest struct pointer */
	int s;				/* priority to return to */

	if(NpDebug & DEBENTRY)
		printf("npstrategy\n");
	if(NpDebug & DEBIO)
		printf("flag = %x count = %x paddr = %x %x blkno = %x %x\n",
		    bp->b_flags, bp->b_bcount, bp->b_un.b_addr, bp->b_un.b_addr,
		    bp->b_blkno,bp->b_blkno);

	/* get master structure */

	mp = npcnxtab[NPUNIT(bp->b_dev)][NPCONN(bp->b_dev)].unit;

	/* make sure the boards ok */

	if (mp->flags & BADBOARD) {
		bp->b_flags |= B_ERROR;

		if(NpDebug & DEBMEM)
			printf("Bad Board %x bp %x\n",mp->flags,bp->b_flags);

		np_tab[mp->unit].b_actf = bp->av_forw;
		iodone(bp);
		return;
	}

	/* Initializations of request structure */

	while((rp = NpGetReq(mp->reqtab)) == NULL) {
		mp->reqtab->flags |= WANTREQ;
		sleep((caddr_t)(mp->reqtab),PZERO -1);
	}

	rp->bufoffset = 0;		/* This is the start of the buffer */
	ip = &np_tab[mp->unit];
	bp->b_rp = (struct buf *)rp;

	rp->flags |= KERNREQ;		/* Mark it as kernel so not to map */

	rp->mapbase = ubasetup(mp->devp->ui_ubanum,bp,0);
	rp->bufaddr = (caddr_t)((int)(rp->mapbase) & UBADDRMASK);

	s = spl5();
	if(ip->b_actf ==(struct buf *)0)
		ip->b_actf = bp;
	else {
		if(ip->b_actf->av_forw)
			printf("Panic NP100 bad buffer chain\n");
		ip->b_actf->av_forw = bp;
	}
	ip->b_actl = bp;

	NpAddReq(mp->reqtab,rp);		/* Queue onto active list */

	if(ip->b_active == 0) {

		if(NpDebug & DEBIO)
			printf("calling npstart %x\n",mp);

		npstart(mp);
	}
	splx(s);

	if(NpDebug & DEBIO)
		printf("back from npstart\n");

	/* Await completion of I/O */

	iowait(bp);

	if(NpDebug & DEBIO)
		printf("after iowait in npstrategy\n");

	/* Remove request from queue */

	NpRemReq(rp);

	/* Release mapping registers */

	ubarelse(mp->devp->ui_ubanum,&rp->mapbase);

	/* Free up request structure */

	NpFreeReq(mp->reqtab,rp);

	if(NpDebug & DEBENTRY)
		printf("Leaving npstrategy flags is %x\n",bp->b_flags);
}

unsigned
nptrim(bp)
register struct buf *bp;
{

	if(bp->b_bcount > NPMAXXFR)
		bp->b_bcount = NPMAXXFR;
}

/*
 * Npread dumps data from the board to the user's buffer
 */
npread(dev,uio)
dev_t dev;
struct uio *uio;
{
	struct buf *bp;
	bp = &npcnxtab[NPUNIT(dev)][NPCONN(dev)].np_rbuf;

	if(NpDebug & DEBENTRY)
		printf("in npread\n");

	bp->b_uio = (struct buf *)uio;
	return(physio(npstrategy,bp,dev,B_READ ,nptrim,uio));
}

/*
 * Npwrite loads the np100 board from the user's buffer
 */

npwrite(dev,uio)
dev_t dev;
struct uio *uio;
{
	struct buf *bp;
	bp = &npcnxtab[NPUNIT(dev)][NPCONN(dev)].np_wbuf;

	if(NpDebug & DEBENTRY)
		printf("in npwrite \n");

	bp->b_uio = (struct buf *)uio;
	return(physio(npstrategy,bp,dev,B_WRITE ,nptrim,uio));
}

/*
 * npreset - called as result of a UNIBUS reset.
 */

npreset(uban)
int uban;
{

	register struct npmaster *mp;
	register struct npreq *rp;
	register struct uba_device *ui;
	int i;

	if(NpDebug & DEBENTRY)
		printf("npreset(ubareset)\n");
	for(i = 0; i < NNP; i++) {

		if(((ui = npdinfo[i]) == (struct uba_device *)NULL) ||
			(ui->ui_ubanum != uban))
			continue;

		mp = &npmasters[i];

		/* Get a Request structure */

		while((rp = NpGetReq(mp->reqtab)) == NULL) {
			mp->reqtab->flags |= WANTREQ;
			sleep((caddr_t)(mp->reqtab),PZERO -1);
		}

		NpReset(mp,rp);
	}
	if(NpDebug & DEBENTRY)
		printf("npreset(ubareset)...\n");
}


/*
 * Nppoll looks for work by polling each board. He goes to sleep if there are
 * no outstanding requests for him but reminds the board that he's there when
 * needed.
 */

NpPoll(mp,addr)
struct npmaster *mp;
caddr_t	addr;
{
	int error;

	struct {
		unsign16 request;
		unsign16 unit;
	}icpreq;

	if(NpDebug & DEBMAINT)
		printf("NpPoll: flags is %x.\n",mp->flags);

	while(TRUE) {

		for(mp = npmasters; mp; mp = mp->next) {

			if(mp->flags & BOARDREQ) {

				/* Get request type from master structure */

				if(mp->flags & BRDRESET) {
					icpreq.request = ICPPOLL;
					mp->reqtab->reqcnt--;

					if(NpDebug & DEBMAINT)
						printf("Waking NpResetter!\n");

					wakeup((caddr_t)(&mp->reqtab));
				}
				else if(mp->flags & PANICREQ)
					icpreq.request = ICPPANIC;
				else if(mp->flags & DUMPREQ)
					icpreq.request = ICPDUMP;
				else if(mp->flags & LOADREQ)
					icpreq.request = ICPLOAD;
				else {
					mp->flags &= ~BOARDREQ;
					continue;
				}

				if(NpDebug & DEBMAINT)
					printf("ProcICP servicing %d \n",icpreq.request );

				/* Request and unit number to be sent */

				icpreq.unit = mp->unit;

				/* Copy service request to calling process */

				error = copyout(&icpreq,addr,sizeof(icpreq));

				/* Mark Poller as being unavailable */

				NpState &= ~ICPAVAIL;

				return(error);
			}
		}

		/* Mark Poller as being available */

		NpState |= ICPAVAIL;

		if (error = tsleep((caddr_t)&NpState, (PZERO + 1) | PCATCH,
		    devio, 0))
			return (error);

		if(NpDebug & DEBMAINT)
			printf("wakeup in NpPoll\n");

	}
}

/*
 * Software initialization of Driver data structures for the specified unit.
 */

NpSWinit(unit)
int unit;
{

	register int j;
	register struct npmaster *mp;
	register struct npspace *npsp;
	register struct CmdQue *cqp;
	int offset;

	if(NpDebug & DEBINIT)
		printf("SW reset on unit %d.\n",unit);

	np_icount[unit] = NPCLEAR;
	np_mapreq[unit] = (struct npreq *) NPCLEAR;

	/* Initialize master structure pointer for this unit */

	mp = &npmasters[unit];

	/* Initialize unit buffer headers */

	np_tab[unit].b_active = 0;
	np_tab[unit].b_actf = 0;

	/* UBA device structure for this unit */

	mp->devp = npdinfo[unit];

	/* Interrupt vector for this unit */

	mp->vector = npvectors[unit];

	if(unit == (NNP -1))
		mp->next = (struct npmaster *)NULL;
	else mp->next = &npmasters[unit + 1];

	/*
	 * Guarantee alignment of shared memory area on a
         * 16 byte boundary as required by I-Board
	 */

	mp->shmemp = &npspaces[unit];
	mp->shmemp = (struct npspace *)ROUND16((int)(mp->shmemp));

	/* Base address of this controller */

	mp->iobase = (struct NPREG *)(mp->devp->ui_addr);

	if(NpDebug & DEBMEM) { 
		printf("Npspaces starts at %x.\n",npspaces);
		printf("Shared memory starts at %x.\n",mp->shmemp);
		printf("End of shared memory is %x.\n",&npspaces[unit + 1]);
		printf("Iobase is %x.\n",mp->iobase);
		printf("Npmasters start at %x\n",npmasters);
		printf("Reqhdr start at %x\n",reqhdr);
		printf("Npreqs start at %x\n",npreqs);
 	}

	/* Initialize the request header */

	mp->reqtab = &reqhdr[unit];

	/* Unit initialization */

	mp->unit = unit;

	/* Initialize Status Block */

	npsp = mp->shmemp;
	offset = (int) (mp->shmemp);

	npsp->statblock.sb_drw = 0;
	npsp->statblock.sb_hcw = HOSTCONF;
	npsp->statblock.sb_dcw = 0;
	npsp->statblock.sb_dpm = 0;

	npsp->statblock.sb_dcq = (unsign16)((int)(&npsp->devcq))-offset;

	npsp->statblock.sb_hcq = (unsign16)((int)(&npsp->hostcq))-offset;

	/* Initialize Device Command Queue */

	cqp = (struct CmdQue *) &npsp->devcq;

	if(NpDebug & DEBCQ) 
		printf("Device CQ at %x\n",cqp);

	cqp->scanflag = NPCLEAR;
	cqp->chngflag = NPCLEAR;

	cqp->cq_add = (unsign16)(int)(&cqp->cq_cqe[0]) - offset;
	cqp->cq_rem = cqp->cq_add;

	cqp->cq_wrap = (unsign16)(int)(&cqp->cq_cqe[NUMCQE]) - offset;

	for(j = 0; j < NUMCQE; j++)
		cqp->cq_cqe[j] = (unsign16)NULL;

	/* Initialize Host Command Queue */

	cqp = (struct CmdQue *) &npsp->hostcq;

	if(NpDebug & DEBCQ) 
		printf("HOST CQ at %x\n",cqp);

	cqp->scanflag = NPCLEAR;
	cqp->chngflag = NPCLEAR;

	cqp->cq_add = (unsign16)(int)(&cqp->cq_cqe[0]) - offset;
	cqp->cq_rem = cqp->cq_add;

	cqp->cq_wrap = (unsign16)(int)(&cqp->cq_cqe[NUMCQE]) - offset;

	for(j = 0; j < NUMCQE; j++)
		cqp->cq_cqe[j] = (unsign16)NULL;

	/*
	 * Initialize the reqid of the elements to the address
	 * of the corresponding Npreq structure. These don't change.
 	 */

	for(j = 0; j < NUMCQE; j++)
		npsp->elements[j].cqe_reqid = &npreqs[unit][j];

	/*
	 * Initialize the Request Header (reqhdr), free list of
 	 * npreqs, and pointers to CQEs.
 	 */

	reqhdr[unit].forw = reqhdr[unit].back = &reqhdr[unit];
	reqhdr[unit].free = &npreqs[unit][0];

	for(j = 0; j < NUMCQE; j++) {
		npreqs[unit][j].free = &npreqs[unit][j + 1];
		npreqs[unit][j].element = &npsp->elements[j];
		npreqs[unit][j].forw = npreqs[unit][j].back = (struct npreq *)NULL;
		npreqs[unit][j].flags = NPCLEAR;
	}
	npreqs[unit][--j].free = &reqhdr[unit];

	/*
	 * Set up the UNIBUS I/O Map Registers for the
	 * Shared memory area.
 	 */

	mp->iomapbase = uballoc(mp->devp->ui_ubanum,(caddr_t)(mp->shmemp),sizeof(struct npspace),0);


	if(NpDebug & DEBENTRY)
		printf("SW_Init...\n");
	return(0);
}

/*
 * NpHWinit() issues a hardware reset to the specified board and waits
 * for on-board diagnostics to complete. It returns 0 if the board is
 * present and passed diagnostics, an error value otherwise.
 */

NpHWinit(unit)
int unit;
{
	register struct npmaster *mp;
	struct NPREG *REG;
	unsign16 status;
	int dflag;

	if(unit >= NNP)
		return(ENXIO);

	mp = &npmasters[unit];

	if(NpDebug & DEBENTRY)
		printf("NpHWinit\n");

	/* See if the board is out there */

	REG = (struct NPREG *)mp->iobase;

	if(NpDebug & DEBINIT)
		printf("REG in HWinit is %x.\n",mp->iobase);

	if(!(mp->flags & BRDRESET))

		if(badaddr(REG,2)) {
			mp->flags |= BADBOARD;
			printf("\nNP100 unit %d not found!\n",unit);
			return(ENXIO);
		}


	if(NpDebug & DEBENTRY)
		printf("Resetting the NP100 Board at %x\n",mp->iobase);

	/* Reset the Board */

	RESET(mp);

	dflag = NPCLEAR;

	timeout(NpTimer,&dflag,DIAGTIME);

	/* Wait for Enable and Read Data Ready to go high */

	while(! ((RCSR1(mp->iobase) & NPENB) && (RCSR1(mp->iobase) & NPRDR))) {
		if(dflag)
			break;

	}

	untimeout(NpTimer,&dflag);

	if(NpDebug & DEBINIT)
		printf("np reset %d \n",dflag);

	if(dflag) {
		mp->flags |= BADBOARD;
		printf("NP100 Unit %d timed out!\n",unit);
		return(EIO);
	}

	status = RCSR0(mp->iobase);

	/* Check for Hardware OK */

	if(!(RCSR1(mp->iobase) & NPHOK)) {
		mp->flags |= BADBOARD;
		printf("NP100 Unit %d Failed diagnostics!\n",unit);
		printf("Status from CSR0: %x.\n",status);
		return(EIO);
	}

	if(NpDebug & DEBENTRY)
		printf("HWinit...\n");

	return(0);
}

/*
 * NP Driver Interrupt Handler
 */

npintr(unit)
int unit;
{
	register struct npmaster *mp;
	register struct buf	*bp;

	if(NpDebug & DEBENTRY)
		printf("npintr on unit %d!\n",unit);

	mp = &npmasters[unit];
	np_icount[unit]++;

	if(NpDebug & DEBINTR)
		printf("npintr mp->flags = %x  interupt count = %x\n",
			mp->flags, np_icount[unit]);

	/* Wake up anyone sleeping on a CSR0 Command */

	if(mp->flags & CSRPEND) {

		mp->flags &= ~CSRPEND;
		if(np_tab[mp->unit].b_active) {
			np_tab[mp->unit].b_active = 0;
			bp = np_tab[mp->unit].b_actf;
			np_tab[mp->unit].b_actf = bp->av_forw;

			if(NpDebug & DEBINTR)
				printf("bp = %x resid = %d forw = %x\n",bp,
				    bp->b_resid,bp->av_forw);

			bp->b_resid = 0;
			iodone(bp);
		}
		if(mp->flags & PANIC3) {
			mp->flags &= ~PANIC3;
			mp->flags = AVAILABLE;
			ubarelse(mp->devp->ui_ubanum,&panicmap);
		}
		if(mp->flags & PANIC2) {
			mp->flags &= ~PANIC2;
			printf("Panic Message: %s",NpPbuf);
			mp->flags |= PANIC3;
			NpPbuf[0] = 0;
			NPIO(mp,(paddr_t)((int) panicmap & UBADDRMASK),(paddr_t)pstring,sizeof(NpPbuf),B_WRITE);
		}
		if(mp->flags & PANIC1) {
			mp->flags &= ~PANIC1;
			mp->flags |= PANIC2;
			ubarelse(mp->devp->ui_ubanum,&panicmap);
			panicmap = uballoc(mp->devp->ui_ubanum,(caddr_t)NpPbuf,sizeof(NpPbuf),0);
			pstring = (caddr_t)((panaddr[1] << 4) + panaddr[0]);
			NPIO(mp,(paddr_t)pstring,(paddr_t)((int) panicmap & UBADDRMASK),sizeof(NpPbuf),B_READ);
		}

		wakeup((caddr_t)mp);
		goto out;
	}

	/* Mark unit as being available if Device Protocol Mask set */

	if(!(mp->flags & AVAILABLE)) {

		if((mp->shmemp->statblock.sb_dpm) && (!(mp->flags & BRDRESET)))

			mp->flags = AVAILABLE;
	}

	/* Honor service requests from the device */

	switch(mp->shmemp->statblock.sb_drw) {

	    case NOREQ:
		break;

	    case NPPANIC:

		printf("\nPanic from NP100 unit %d!\n",mp->unit);
		mp->flags &= ~AVAILABLE;
		mp->flags |= PANIC1;

		/* Clear device request word */

		mp->shmemp->statblock.sb_drw = 0;

		panicmap = uballoc(mp->devp->ui_ubanum,(caddr_t)panaddr,sizeof(panaddr),0);
		NPIO(mp,(paddr_t)NPPSADDR,(paddr_t)((int)panicmap & UBADDRMASK),sizeof(panaddr),B_READ);
		goto out;
		break;

	    case NPDUMP:
		mp->flags |= (DUMPREQ | BOARDREQ);

		/* Clear device request word */

		mp->shmemp->statblock.sb_drw = 0;

		if(NpState & ICPAVAIL)
			wakeup((caddr_t)&NpState);
		break;

	    case NPLOAD:
		mp->flags |= (LOADREQ | BOARDREQ);

		/* Clear device request word */

		mp->shmemp->statblock.sb_drw = 0;

		if(NpState & ICPAVAIL)
			wakeup((caddr_t)&NpState);
		break;

	    default:
		printf("Bad Req: %x.\n",mp->shmemp->statblock.sb_drw);
		goto out;

	}

 	/* Process the Host Command Queue for this device */

	NpProcQueue(mp);

out:
	CLEARINT(mp);	/* Clear the interrupt */

	if(NpDebug & DEBENTRY)
		printf("npintr...\n");

	return(1);	/* Interrupt serviced */

}

/*
 * This routine, called from the interrupt handler, is used to process the
 * Host Command Queue for the specified device.
 */

NpProcQueue(mp)
struct npmaster *mp;
{
	register struct CmdQue *cqp;
	register struct CQE *ep;
	register struct npreq *rp;
	register int base;
	int s;

	if(NpDebug & DEBENTRY)
		printf("NpProcQueue\n");

	cqp = &mp->shmemp->hostcq;	/* Command Queue pointer */

	s = spl5();
	if(mp->flags & SCANNING) {
		splx(s);
            	return;
	}
	mp->flags |= SCANNING;
	splx(s);

	cqp->scanflag | = ON;

	base = (int)mp->shmemp;		/* Shared memory base address */

	while(1) {

	       	cqp->scanflag |= ON;
		cqp->chngflag &= ~ON;
		while(ep = NpRemCQE(cqp,base)) {

			rp = ep->cqe_reqid;

			if(NpDebug & DEBCQE)
				printf("cqe_sts is %x ep = %x\n",ep->cqe_sts,ep);

			switch (ep->cqe_sts)  {

			    case NPDONE:
				rp->flags |= REQDONE;	/* Normal completion */
				break;
			    case NPIFC:			/* IFC Request */
				rp->flags |= IOIFC;
				break;
			    case NPPERR:		/* Protocol Error */
				rp->flags |= (NPPERR | REQDONE);
				break;
			    case NPMERR:		/* Memory allocation */
				rp->flags |= (NPMERR | REQDONE);
				break;
			    default:			/* Error on Board */
				rp->flags |= (IOERR | REQDONE);
				break;

			}

			if(NpDebug & DEBCQE) {
				printf("flag is %x reqid = %x\n",rp->flags,ep->cqe_reqid);
				printf("wakeup in procqueue\n");
			}

			if(rp->intr) {

				if(NpDebug & DEBINTR)
					printf("calling usr intr at %x\n",
						rp->intr);

				/* Call interrupt routine */

				(*rp->intr)(mp,rp);
			}
			else {

			if(NpDebug & DEBINTR)
				printf("waking up %x\n",rp);

				/* if(rp->flags & NPUIO)
					iodone(&rp->buf);
				else	wakeup((caddr_t) (rp)); /* Awaken */

				wakeup((caddr_t)(rp)); 	/* Awaken */
			if(NpDebug & DEBINTR)
				printf("AWAKE\n");
			}
		}

		cqp->scanflag &= ~ON;
		if(!(cqp->chngflag & ON))
			break;

	}

	mp->flags &= ~SCANNING;
	if(NpDebug & DEBENTRY)
		printf("NpProcQueue...\n");
}

/*
 * NpIFC - processes an IFC (Internal Fuction Call) request 
 *		NOTE: this function must be called from the user context
 *			on all virtual pageing systems
 *
 */
NpIFC(mp,rp)
register struct npmaster *mp;
register struct npreq *rp;
{
	register struct CQE	*ep;

	if(NpDebug & DEBENTRY)
		printf("NpIFC\n");

	ep = rp->element;
	rp->flags &= ~IOIFC;
	switch(ep->cqe_func) {

	    case NPUNLOCK:	/* Unlock process, free up mapping registers  */

		if(NpDebug & DEBIFC)
			printf("NPUNLOCK\n");

		if(rp->mapbase)
			NpUnMapMem(mp,rp);
		break;

	    case NPLOCK:	/* Lock process, get mapping registers */

		if(NpDebug & DEBIFC)
			printf("NPLOCK\n");
		NpMapMem(mp,rp,rp->virtmem,rp->bytecnt);
		ep->cqe_dma[0] = LOWORD(rp->bufaddr);
		ep->cqe_dma[1] = HIWORD(rp->bufaddr);
		break;

	    case NPREMAP:

		if(NpDebug & DEBIFC)
			printf("NPREMAP\n");

		/* Remap user buffer and update buffer offset */
#ifdef USG
		np_remapmem(rp,rp->virtmem); 
		ep->cqe_dma[0] = LOWORD(rp->bufaddr);
		ep->cqe_dma[1] = HIWORD(rp->bufaddr);
		break;
#endif

	    default:
		if(NpDebug & DEBIFC)
			printf("Bad case %x in IFC\n", ep->cqe_func);

		rp->flags |= (REQDONE | IOERR);
		break;
	}
}

/*
 * The following contains various routines for allocating and deallocating
 * structures used by the NP Driver. Routines are also here for addding
 * and removing Command Queue Elements from a Command Queue.
 */

/*
 * Get a free NP Request structure from the list pointed to by head. Returns
 * a pointer to a npreq or NULL if none left.
 */

struct npreq *
NpGetReq(head)
struct npreq *head;
{

	register struct npreq *p;

	p = head->free;
	head->free = p->free;
	if (p->flags & REQALOC)
		printf("GetReq: Req %x already allocated\n", p);
	p->flags &= WANTREQ;
	if (p != head)
		p->flags |= REQALOC;
	return(p==head ? (struct npreq *)NULL : p);
}

/*
 * Return a NP Request structure to the free list pointed to by head.
 */

NpFreeReq(head,nprp)
register struct npreq *head, *nprp;
{
	int s;

	if(NpDebug & DEBREQ)
		printf("NpFreeReq, head is %x rp is %x\n",head,nprp);

	if (nprp == NULL) {
		printf("FREEREQ: attempt to free null pointer\n");
		return;
	}
	if (!(nprp->flags & REQALOC)) {
		printf("FREEREQ: attempt to free unallocated request %x\n",
			nprp);
		return;
	}
	if (nprp->flags & REQUSE)
		printf("FREEREQ: freeing unremoved request %x\n", nprp);

	s = spl5();
	nprp->forw = nprp->back = (struct npreq *)NULL;
	nprp->free = head->free;
	head->free = nprp;
	nprp->flags &= ~REQALOC;
	splx(s);

	/* Wake up any processes waiting for a request structure */

	if(head->flags & WANTREQ) {
		head->flags &= ~WANTREQ;
		wakeup((caddr_t)head);
	}

	if(NpDebug & DEBENTRY)
		printf("NpFreeReq...\n");
}

/*
 * Add a Command Queue Element onto the specified Command Queue and
 * update its Add offset.
 */

NpAddCQE(ep,cqp,mp)
struct CQE *ep;
struct CmdQue *cqp;
struct npmaster *mp;
{

	register unsign16 *temp;
	register unsign16 cqe_offset;
	register int base;

	base = (int)mp->shmemp;		/* Shared memory base address */

	temp = (unsign16 *)(base + cqp->cq_add); /* Offset to add element */

	cqe_offset = (unsign16)((int)ep - base);

	if(*temp) {			/* Should never happen */

		printf("No more room on Command Queue!\n");
		return;
	}
	else *temp = cqe_offset;	/* Enter this request's offset */

	/* Update cqe_add where next request is to be added */

	cqp->cq_add += sizeof(unsign16);

	if(cqp->cq_add == cqp->cq_wrap)	/* Wrap if necessary */
		cqp->cq_add = (unsign16)((int)cqp->cq_cqe - base);

	cqp->chngflag |= ON;		/* Set change flag unconditionally */

	/* Interrupt the Board if his scan flag isn't on */

	if(!(cqp->scanflag & ON))

		INTNI(mp);		/* Interrupt the Board */

}

/*
 * The NpRemCQE routine is used to remove the next CQE from the Command Queue
 * specified by cqp. The common offset of shared memory used by the device
 * is specified by base. NpRemCQE returns a pointer to the next CQE or
 * NULL if there are none left. This routine will also update the cqe_rem
 * offset which specifies where the next element to be removed from the
 * queue is located.
 */

struct CQE *
NpRemCQE(cqp,base)
struct CmdQue *cqp;
int base;
{

	register unsign16 *temp;
	register unsign16 cqe_offset;

	cqp->chngflag &= ~ON;			/* Turn off unconditionally */

	/* Get address of element to remove */

	temp = (unsign16 *)(base +cqp->cq_rem);

	if(*temp == NULL)			/* If none left, go home */
		return((struct CQE *) NULL);

	else cqe_offset = *temp;		/* Offset of CQE to remove */

	/* Update the Command Queue's cqe_rem offset */

	*temp = NULL;				/* Clear out this entry */

	cqp->cq_rem += sizeof(unsign16);	/* Bump offset */

	if(cqp->cq_rem == cqp->cq_wrap)		/* Wrap if necessary */
		cqp->cq_rem = (unsign16)((int)cqp->cq_cqe - base);

	temp = (unsign16 *)(base + cqe_offset);	/* CQE address */
	return((struct CQE *)temp);		/* is returned */
}

/*
 * NpAddReq will add the specified npreq structure to the queue controlled
 * by head.
 */

NpAddReq(head,rp)
register struct npreq *head, *rp;
{
	int s;

	if (NpDebug & (DEBENTRY|DEBREQ))
		printf("NpAddReq: %x\n",rp);

	if (rp->flags & REQUSE)
		printf("ADDREQ: Request %x allready in use\n", rp);

	s = spl7();
	rp->forw = head->forw;
	rp->forw->back = rp;
	rp->back = head;
	head->forw = rp;
	rp->flags |= REQUSE;
	splx(s);

	if(NpDebug & DEBENTRY)
		printf("NpAddReq...\n");
}

/*
 * NpRemReq is used to remove a npreq structure from the queue specified by
 * head.
 */

NpRemReq(rp)
register struct npreq *rp;
{
	int s;

	if (NpDebug & (DEBENTRY|DEBREQ))
		printf("NpRemReq: %x\n",rp);

	if (rp == NULL) {
		printf("REMREQ: null pointer removal requested\n");
		return;
	}
	if (!(rp->flags & REQUSE)) {
		printf("REMREQ: trying to rem unused req %x\n", rp);
		return;
	}
	if (!(rp->flags & REQALOC)) {
		printf("REMREQ: trying to rem unallocated req %x\n", rp);
		return;
	}
		
	s = spl7();
	rp->back->forw = rp->forw;
	rp->forw->back = rp->back;
	rp->flags &= ~REQUSE;
	splx(s);

	if(NpDebug & DEBENTRY)
		printf("NpRemReq...\n");
}


/*
 * The following routines are used to communicate with the
 * NI Hardware via the CSR0 commands. These commands are issued during
 * the hardware initializtion process and may also be used subsequently
 * by privileged processes who wish to communicate in this way. The
 * convention for passing data as a Command Block is discussed in detail
 * in the NI1510 UNIBUS Compatible Ethernet Communications Processor
 * Hardware Specification.
 */

NpSendCSR0(iobase,src,bcount)
struct NPREG *iobase;
register unsign16 *src;
int bcount;
{
	register int wcount;
	int i;
	int csrflag;
	unsign16 tmp;

	if(NpDebug & DEBENTRY)
		printf("NpSendCSR0\n");

	/* Jolt the board into CSR0 command mode if necessary */

	if(!(RCSR1(iobase) & NPENB)){   
		tmp = NPCLEAR;		/* MC68000 clr reads before writing */
		WCSR0(iobase,tmp); 	
	}

	wcount = (bcount +1) >> 1;	/* Convert byte count to word count */

	/* Clear timer flag before beginning the timer */

	csrflag = NPCLEAR;
	timeout(NpTimer,&csrflag,DIAGTIME);

	for(i = 0; (i < wcount) & (csrflag == NPCLEAR); i++) {
		while(! ((RCSR1(iobase) & NPENB) && (RCSR1(iobase) & NPRDY)))
			if(csrflag) break;
		WCSR0(iobase,*src);
		src++;			/* Better do this WCSR is a macro */
	}

	/* Clear the timer entry */

	untimeout(NpTimer,&csrflag);

	/* Error if timer went off */

	if(csrflag)
		return(EIO);	

	if(NpDebug & DEBENTRY)
		printf("NpSendCSR0...\n");
	return(0);
}

/*
 * NpSetIntLev sets the UNIBUS interrupt vector to be used by the NP board when
 * interupting the host. The board is specified by mp.
 */

NpSetIntLevel(mp,level)
struct npmaster *mp;
int level;
{

	struct {
		unsign16 cmd_word;
		unsign16 int_level;
	}cmd_block;

	cmd_block.cmd_word = NPCBI | CBICNT;
	cmd_block.int_level = level;

	return(NpSendCSR0(mp->iobase,(unsign16 *)&cmd_block,(int)sizeof(cmd_block)));
}

/*
 * NpSetMemAddr is used to declare the shared memory area address to be used
 * for communication between the driver and the device. This address is used
 * to access data structures by acting as a base from which defined offsets
 * locate data. The board is specified by mp.
 */

NpSetMemAddr(mp,addr)
struct npmaster *mp;
caddr_t addr;
{

	caddr_t shmaddr;
	int error;

	struct {
		unsign16 cmd_word;
		unsign16 hi_addr;
		unsign16 lo_addr;
	} cmd_block;

	if(NpDebug & DEBENTRY)
		printf("NpSetMemAddr\n");

	shmaddr = addr;

	if(NpDebug & DEBMEM)
		printf("NpSetMemAddr, addr is %x shmaddr is %x.\n",addr,shmaddr);

	cmd_block.cmd_word = NPCMD | CMDCNT;
	cmd_block.hi_addr = HIWORD(shmaddr);
	cmd_block.lo_addr = LOWORD(shmaddr);

	error = NpSendCSR0(mp->iobase,(unsign16 *)&cmd_block,(int)sizeof(cmd_block));

	if(NpDebug & DEBENTRY)
		printf("NpSetMemAddr...\n");

	return(error);
}


/*
 * NpSetXeqAddr specifies the address at which the board should begin
 * execution of its on-board software. It also indicates the shared memory
 * address to be used. The board is specified by mp.
 */

NpSetXeqAddr(mp,addr)
struct npmaster *mp;
caddr_t addr;
{
	caddr_t shmaddr;
	int error;

	struct {
		unsign16 cmd_word;
		unsign16 hi_addr;
		unsign16 lo_addr;
		unsign16 mhi_addr;
		unsign16 mlo_addr;
	} cmd_block;

	if(NpDebug & DEBENTRY)
		printf("NpSetXeqAddr\n");

	shmaddr = (caddr_t)((int)mp->iomapbase & UBADDRMASK);

	cmd_block.cmd_word = NPBGN | NPCMD | NPLST | (BGNCNT + CMDCNT);
	cmd_block.hi_addr = HIWORD(addr);
	cmd_block.lo_addr = LOWORD(addr);
	cmd_block.mhi_addr = HIWORD(shmaddr);
	cmd_block.mlo_addr = LOWORD(shmaddr);

	if(NpDebug & DEBINIT) {
		printf("NpSetXeqAdddr: hi: %x lo: %x\n",HIWORD(addr), LOWORD(addr));
		printf("NpSetXeqAdddr: mhi: %x mlo: %x\n",HIWORD(shmaddr),LOWORD(shmaddr));
	}

	error = NpSendCSR0(mp->iobase,(unsign16 *)&cmd_block,(int)sizeof(cmd_block));

	if(NpDebug & DEBENTRY)
		printf("NpSetXeqAddr...\n");

	return(error);
}

/*
 * NPIO issues a CSR0 load or dump request to the I-Board after packaging a
 * CSR0 Command Block.
 */

NPIO(mp,src,dest,count,dir)
struct npmaster *mp;
paddr_t dest;
paddr_t src;
unsign16 count;
int dir;		/* Direction  READ/WRITE */
{

	int error;

	struct {
		unsign16 cmd_word;	/* Command Word */
		unsign16 shi_addr;	/* High word of Source Address */
		unsign16 slo_addr;	/* Low word of Source Address */
		unsign16 dhi_addr;	/* High word of Destination Address */
		unsign16 dlo_addr;	/* Low word of Destination Address */
		unsign16 count;		/* Byte count */
		unsign16 intlevel;	/* Interrupt level to host */
	} cmd_block;

	if(NpDebug & DEBENTRY)
		printf("NPIO\n");
	if(NpDebug & DEBMAINT) {
		printf("I/O src addr = %x, dest addr = %x \n",src,dest);
		printf("I/O count = %d \n",count);
	}

	cmd_block.cmd_word = NPCBI | (CBICNT + IOCNT);
	cmd_block.intlevel = mp->vector;
	cmd_block.shi_addr = HIWORD(src);
	cmd_block.slo_addr = LOWORD(src);
	cmd_block.dhi_addr = HIWORD(dest);
	cmd_block.dlo_addr = LOWORD(dest);
	cmd_block.count = count;
	if ((mp->flags & LSTCMD) == 0)
		cmd_block.cmd_word |= NPLST;
	if(dir == B_READ)
		cmd_block.cmd_word |= NPDMP;
	else
		cmd_block.cmd_word |= NPLD;


	if(NpDebug & DEBIO) {
		printf("cmd: %x int: %o shi: %x slo: %x dhi: %x dlo: %x cnt: %x\n",
	cmd_block.cmd_word,cmd_block.intlevel,cmd_block.shi_addr,cmd_block.slo_addr,
	cmd_block.dhi_addr,cmd_block.dlo_addr,cmd_block.count);
	}
	
	mp->flags |= CSRPEND;		/* CSR0 command pending */

	error = NpSendCSR0(mp->iobase,(unsign16 *)&cmd_block,(int)sizeof(cmd_block));
	if(NpDebug & DEBENTRY)
		printf("NPIO...\n");

	return(error);
}


/*
 * NpKill will terminate all outstanding requests for the specified board.
 */

NpKill(mp,curr_rp)
struct npmaster *mp;
struct npreq *curr_rp;
{
	struct npreq *rp;
	int s;

	if(NpDebug & DEBENTRY)
		printf("NpKill\n");

	mp->reqtab->reqcnt = 0;		/* Init request count */

	s = spl5();			/* Disable interrupts */

	/* Mark each active request as having an error and wake him up */

	for(rp = mp->reqtab->forw;rp != mp->reqtab;rp = rp->forw) {

		if(rp == curr_rp) continue;

		rp->flags |= (IOABORT | REQDONE);
		mp->reqtab->reqcnt++;
		/* if(rp->flags & NPUIO)
			iodone(&rp->buf);
		else */
		wakeup((caddr_t)rp);
	}

	if(NpDebug & DEBMAINT)
		printf("NpKill, req count is %d\n",mp->reqtab->reqcnt);

	splx(s);

	if(NpDebug & DEBENTRY)
		printf("NpKill...\n");

	return(0);

}

/* Hardware and Software Initializations for the specified unit */

NpReset(mp,rp)
register struct npmaster *mp;
struct npreq *rp;
{
	int error;

	if(NpDebug & DEBENTRY)
		printf("NpReset!\n");

	/* Mark board as being reset and make unavailable */

	mp->flags = BRDRESET;

	/* Abort outstanding requests for this board */

	mp->reqtab->reqcnt = 0;		/* Init request count */

	/* Wakeup Poller if available and wait until he's gone */

	if(NpState & ICPAVAIL) {

		mp->flags |= BOARDREQ;
		mp->reqtab->reqcnt++;

		if(NpDebug & DEBMAINT)
			printf("Waking ICP in reset!\n");

		wakeup((caddr_t)&NpState);

		while(mp->reqtab->reqcnt)
			if (error = tsleep((caddr_t)(&mp->reqtab),
			    (PZERO + 1) | PCATCH, devio, 0))
				return (error);

		if(NpDebug & DEBMAINT)
			printf("Reset:awoken by ICP senior!\n");

	}

	/* Abort outstanding requests and wait till they're gone */

	NpKill(mp,rp);

	while(mp->reqtab->reqcnt) {

		if(NpDebug & DEBMAINT) {
			printf("Sleeping in NpReset on reqtab!\n");
			printf("Reqcnt is %d.\n",mp->reqtab->reqcnt);
		}

		if (error = tsleep((caddr_t)(&mp->reqtab),
		    (PZERO + 1) | PCATCH, devio, 0))
			return (error);
	}

	/* Free up I/O Map registers if any allocated */

	if(mp->iomapbase) {

		if(NpDebug & DEBMEM)
			printf("freeing shared memory map.\n");

		ubarelse(mp->devp->ui_ubanum,&mp->iomapbase);
		mp->iomapbase = 0;
	}

	/* Initialize S/W data structures in NP Driver */

	NpSWinit(mp->unit);		/* Software initialization */

	/* Hardware initialization of the board */

	error = NpHWinit(mp->unit);	/* Hardware initialization */

	mp->flags &= ~BRDRESET;		/* Initialization complete */

	/* Initialize Pseudo-Drivers */

	if (IxReset)
		(*IxReset)(mp->unit, mp->devp->ui_ubanum, rp);

	/* Clear Poller's State Flag */

	NpState = NPCLEAR;

	if(NpDebug & DEBENTRY)
		printf("NpReset...\n");
	
	return(error);
}

/*
 * General purpose timeout function which sets the flag passed to it
 * as argument.
 */

NpTimer(flagp)
int *flagp;
{
	*flagp = NPSET;
}

NpStats()
{
	if(NpDebug & DEBENTRY)
		printf("npstats\n");
	return(0);
}

/*
 * NpCloseConn is called to issue a close connection command to the I-Board.
 */

NpCloseConn(mp,protocol)
struct npmaster *mp;
unsign16 protocol;
{

	register struct npreq *rp;
	register struct CQE *ep;
	int pri;

	if(NpDebug & DEBENTRY)
		printf("NpCloseConn\n");

	/*
	 * Don't issue the Close Connection command if the Board
         * isn't up.
         */

	if(!((mp->shmemp->statblock.sb_dpm) & PROTOMASK(protocol))) {
		return;
	}

	/* Get a Request structure */

	while((rp = NpGetReq(mp->reqtab)) == NULL) {
		mp->reqtab->flags |= WANTREQ;
		sleep((caddr_t)(mp->reqtab),PZERO -1);
	}

	rp->intr = (int (*)())0;	/* Do not call interrupt routine */
	rp->mapbase = 0;		/* Clear mapping information */

	ep = rp->element;		/* Handy pointer */

	/* Fill in CQE */

	ep->cqe_wind = 0;		/* Entire buffer mapped */
	ep->cqe_nbuf = 1;		/* Must be 1, no buffer chaining */
	ep->cqe_char = 0;		/* Set to 0 for now */

	ep->cqe_func = NPSTOP;		/* OS_STP to I-Board */

	ep->cqe_prot = protocol;	/* Protocol of this connection */
	ep->cqe_lenrpb = 0;		/* Parameter block length */

	ep->cqe_ust0 = ep->cqe_ust1 = NPCLEAR;	/* Clear status flags */

	ep->cqe_famid = (unsign32)u.u_procp->p_pid;  /* Process ID */

	NpAddReq(mp->reqtab,rp);	/* Queue onto active list */

	pri = spl5();			/* Mask our interrupts */

	NpAddCQE(ep,&mp->shmemp->devcq,mp); /* Add CQE to device's queue */

	/* Wait for command to complete */

	while(!(rp->flags & REQDONE)) 
		sleep((caddr_t)rp,PZERO - 1);

	splx(pri);

	NpRemReq(rp);			/* Remove request from active list */

	NpFreeReq(mp->reqtab,rp);	/* Deallocate request structure */

	if(NpDebug & DEBENTRY)
		printf("NpCloseConn...\n");

}

/*
 * This function allows the protocol to be changed for a given connection.
 * It returns 0 for success, error code otherwise.
 */

NpProtChange(protocol,unit)
register unsign16 protocol;
register int unit;
{

	register struct npmaster *mp;

	/* Privileged users only for Maintenance Protocol */

	if((protocol == NPMAINT) && (u.u_uid != 0)) 
		return(EPERM);

	if(NpDebug & DEBMAINT)
		printf("NpProtChange = %x\n",protocol);

	if(protocol != NPMAINT) {

		/* Make sure the I-Board supports the protocol */

		mp = &npmasters[unit];

		if(!((mp->shmemp->statblock.sb_dpm) & PROTOMASK(protocol)))
			return(ENXIO);
	}

	return(0);
}

/*
 * This function allows for the changing of the unit for a given connection.
 */

struct npmaster *
NpBoardChange(protocol,unit)
register unsign16 protocol;
register int unit;			/* Unit number */
{
	register struct npmaster *mp;


	if(unit > NNP)
		return((struct npmaster *)0);

	if(protocol != NPMAINT) {

		/*
		 * Loop through the master structures finding a board which 
		 * supports the requested protocol.
		 */

		for(mp = npmasters; mp ; mp = mp->next) {

			if(mp->flags & BADBOARD)
				continue;

			if(((mp->shmemp->statblock.sb_dpm) & PROTOMASK(protocol)))
				return(mp);
		}
		return((struct npmaster *)0);
	}
	return(&npmasters[unit]);
}

/*
 * NpMapMem - maps the user's memory updating the fields in the npreq
 * structure and returning the mapped address in rp->buffaddr.
 */
NpMapMem(mp,rp,addr,count)
register struct npmaster *mp;
register struct npreq *rp;
caddr_t	addr;
int	count;
{

	if(NpDebug & DEBENTRY)
		printf("NpMapMem\n");
	if(NpDebug & DEBIO)
		printf("mp %x rp %x addr %x count %x\n",mp,rp,addr,count);

	rp->virtmem = addr;
	rp->bytecnt = count;

	rp->buf.b_un.b_addr = addr;
	rp->buf.b_flags = B_PHYS | B_BUSY;
	rp->buf.b_bcount = count;
	rp->buf.b_proc = rp->procp;
		
	rp->procp->p_flag |= SPHYSIO;
	if(NpDebug & DEBENTRY)
		printf("vslock\n");
	vslock(addr,count);
	if(NpDebug & DEBENTRY)
		printf("vslock...\n");

	rp->mapbase = ubasetup(mp->devp->ui_ubanum,&rp->buf,0);

	rp->bufaddr = (caddr_t)(rp->mapbase & UBADDRMASK);

	if(NpDebug & DEBENTRY)
		printf("NpMapMem...\n");
}

/*
 * Unmap the user's memory and free up mapping registers 
 */

NpUnMapMem(mp,rp)
struct npmaster *mp;
struct npreq *rp;
{
	if(NpDebug & DEBENTRY)
		printf("NpUnMapMem\n");

	ubarelse(mp->devp->ui_ubanum,&rp->mapbase);
	rp->mapbase = 0;
	vsunlock(rp->virtmem,rp->bytecnt,B_READ);
	rp->procp->p_flag &= ~SPHYSIO;

	if(NpDebug & DEBENTRY)
		printf("NpUnMapMem...\n");
}

npprobe(reg, ui)
caddr_t reg;
struct uba_device *ui;
{
register int br,cvec;
u_short csraddr;
int i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif

	if(NpDebug & DEBINIT)
		printf("In npprobe, regaddr is %x!\n",reg);

	cvec = (uba_hd[numuba].uh_lastiv -= 4); 

#ifdef OLDBSD
	/* Find unit number from npstd[] by matching the csr address */

	csraddr = (u_short)((int)reg & 0x0FFFF);

	for(i = 0; i < NNP; i++) {

		if(csraddr == npstd[i]) {
			npvectors[i] = cvec;
			break;
		}
	}
	if(i == NNP)
		printf("Couldn't find device in npstd[]!\n");

#else
	npvectors[ui->ui_unit] = cvec;
#endif
	br = 0x15;

	if(NpDebug & DEBINIT)
		printf("npprobe...\n");

	return(sizeof(struct NPREG));		/* CSR Registers */

}

npattach(ui)
register struct uba_device *ui;
{

	if(NpDebug & DEBINIT)
		printf("In npattach, ui is %x.\n",ui);

	npinit(ui->ui_unit);
	if (IxAttach)
		(*IxAttach)(ui);

	if(NpDebug & DEBINIT)
		printf("npattach...\n");
}


NpMem(mp, rp, uaddr)
struct npmaster *mp;
struct npreq *rp;
unsigned long uaddr;
{
	struct np_mem mem;
	register int error = 0;

	if(NpDebug & DEBENTRY)
		printf("npmem\n");

	if (error = copyin(uaddr, &mem, sizeof(mem)))
		return (error);

	if (mem.mem_type == NP_SET) {
		if (np_mapreq[mp->unit] != (struct npreq *)NPCLEAR)
			error = EBUSY;
		else {
			error = NpMapMem(mp, rp, mem.mem_addr, mem.mem_count);
			if (error != 0) {
				np_mapreq[mp->unit] = rp;
				mem.mem_addr = rp->bufaddr;
			}
		}
	} else if (mem.mem_type == NP_USET) {
		error = NpUnMapMem(mp, np_mapreq[mp->unit]);
		NpFreeReq(mp->reqtab, rp);
		NpFreeReq(mp->reqtab, np_mapreq[mp->unit]);
		np_mapreq[mp->unit] = (struct npreq *)NPCLEAR;
	} else 
		error = EIO;

	if (error != 0)
		error = copyout(&mem, uaddr, sizeof(mem));

	if(NpDebug & DEBENTRY)
		printf("npmem...\n");
	return (error);
}
#endif
