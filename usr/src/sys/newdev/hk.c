static	char *sccsid = "@(#)hk.c	1.1 (Berkeley) 10/10/80";

/*
 * RK07 disk driver with bad-sector forwarding
 *
 *  This driver, revised from a UCSB RK07 driver, is modeled
 * after the Stanford SI 9500 driver with bad-sector forwarding.
 * The bad-sector information on the last track of each pack is
 * used to patch up transfers by forwarding accesses to bad sectors
 * to spares on the last cylinder.  The last cylinder is otherwise
 * write-protected.
 *  The bad-sector table is read when a minor device is first
 * opened after a close.  No Volume-Valid checking is done;
 * hamfists beware.  To be correct, the open routine needs a
 * mechanism for reading drive status and for doing a PACKACK.
 *  A pack is 1 minor device; there is no pack mapping.
 *  No attempt at ECC logic is included.  Be my guest.
 *
 *  BUGS:  The same bomb is planted here as in most UBA DMA drivers,
 * to wit, ubasetup is called from the interrupt level.  Ubasetup
 * will sleep if it cannot allocate; ka-boom!
 *
 * Author: Rob Mathews, EE Information Systems Lab, Stanford 4/80
 */

/*
 * Debug control
 *
 *  1 - open/close
 *  2 - strategy
 *  4 - intr entry/exit
 *  8 - intr detail
 * 16 - intr sector mapping
 * 32 - strat errors/lock-opens
 * 64 - bst reading
 *1024 - loop after errors
 */

#define debug(bit)	if (rk7debug & (bit)) printf
int rk7debug = (0);

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"

/*
 * Disk parameters
 */

#define	NRK7	8
#define NCYLS	815
#define NTRKS	3
#define NSECTS	22
#define NBYTES	512
#define	NBLKS	(NCYLS*NTRKS*NSECTS)
#define BSCYL	(NCYLS-1)		/* bad-sector repair cylinder */
#define BSBASE	(BSCYL*NTRKS*NSECTS)	/*  track 0,1 - repair pool */
#define BSTBLK	(BSBASE + 2*NSECTS)	/*  track 2 - bad-sector table */
#define MAXBB	10			/* max tolerable bad sectors */

/*
 * Register definitions
 */

#define	RK7ADDR	((struct rk7_regs *)(UBA0_DEV + 0177440))

struct	rk7_regs
{
	short rkcs1, rkwc;
	unsigned short rkba;
	short rkda, rkcs2, rkds, rker, rkasof, rkdc, rknull, rkdb, rkmr1;
	short rkecps, rkecpt, rkmr2, rkmr3;
};

/*
 * Register bits, per RK07 manual
 */

#define GO	01	/* cs1 */
#define DRCLR	04
#define	READ	020
#define	WRITE	022
#define	PACKACK	02
#define	IE	0100
#define	RDY	0200
#define BA1617	01400
#define CDT	02000
#define CERR	0100000
#define CCLR	0100000
#define SCLR	040	/* cs2 */
#define SVAL	0100000	/* ds */
#define DRDY	0200
#define VV	0100
#define DRA	01
#define DCK	0100000	/* er */
#define BSE	0200
#define NXF	04

#define Error	((RK7ADDR->rkcs1 & CERR) != 0)

/*
 * Driver parameters, data, and definitions
 */

#define RETRIES	10

#define IDLE	0
#define NORMAL	1
#define MAPPED	2
#define RESTART	3

struct buf rk7tab, rk7bsbuf, rrk7buf;

struct rk7
  {
	long access;
	short retries, errors, mapped, spurious;
	int ubinfo, rwcommand;
	short unsigned errer, errcs2, errds;
	short cylnow, trksecnow;
	unsigned short wcnow;
	long manow;
  }
 rk7;

struct bb
  {
	short serial, zeros[2], alignpack;
	struct bbtbl
	  {
		short bbtcyl, bbtts;
	  }
	 bbt[MAXBB];
  }
 rk7bb[NRK7];
#define OPENF	bbt[MAXBB-1].bbtcyl
#define LOCKF	zeros[0]

#define b_trksec av_back
#define b_cylin	 b_resid
#define nsects(x) x/NBYTES

/*
 */

rk7open (dev, flag)
  {
	register struct bb *bbp;
	register m;

	if ((m = minor (dev)) >= NRK7)
	  {
		u.u_error = ENXIO;
		return;
	  }
	bbp = &rk7bb[m];
	debug (1) ("open\n");
	if (bbp->OPENF != -1)
	  {
		bbp->OPENF = 1;
		debug (1|64) ("bs read\n");

		rk7bsbuf.b_flags = B_BUSY | B_READ;
		rk7bsbuf.b_dev = minor (dev);  /* major better not matter ! */
		rk7bsbuf.b_bcount = sizeof (struct bb);
		rk7bsbuf.b_un.b_addr = (caddr_t) bbp;
		rk7bsbuf.b_blkno = BSTBLK;
		rk7bsbuf.b_error = 0;
		rk7strategy (&rk7bsbuf);
		iowait (&rk7bsbuf);

		if (rk7bsbuf.b_flags & B_ERROR)
		  {
			printf ("rk7: error reading bad sector table!\n");
		  }
		 else if (bbp->OPENF != -1)
		  {
		    printf ("rk7: too many bad sectors, drive %d\n", m);
		    bbp->OPENF = -1;
		  }
		wakeup (&bbp->OPENF);
		debug (1|64) ("bs read done\n");
	  }
	 else
	  while (bbp->OPENF != -1)
	    sleep (&bbp->OPENF, PSWP);
  }

rk7close (dev, flag)
  {
	debug (1) ("close\n");
	if (!rk7bb[minor (dev)].LOCKF)
	  rk7bb[minor (dev)].OPENF = 0;
  }

rk7strategy (bp)
register struct buf *bp;
  {
	debug (2) ("strat %s ", bp->b_flags & B_READ ? "r" : "w");
	if (rk7bb[minor (bp->b_dev)].OPENF == 0)
	  {
		rk7open (bp->b_dev, 0);	/* in case root or swap is here */
		rk7bb[minor (bp->b_dev)].LOCKF++;
		debug (32) ("lock-open\n");
	  }

	if (bp->b_blkno + nsects (bp->b_bcount)
	      > (bp->b_flags & B_READ ? NBLKS : BSBASE))
	  {
		debug (2|32) ("error, blk %d, dev %d\n",
				bp->b_blkno, minor (bp->b_dev));
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	  }
	
	debug (2) ("queue ");
	bp->b_cylin = bp->b_blkno / (NTRKS*NSECTS);
	bp->b_trksec = (struct buf *) ((((bp->b_blkno / NSECTS) % NTRKS) << 8)
					+ (bp->b_blkno % NSECTS));
	
	spl5();
	disksort (&rk7tab, bp);

	if(rk7tab.b_active == IDLE)
	  rk7intr();
	spl0();
	debug (2) ("end\n");
  }

/*
disksort (tabp, bp)
register struct buf *tabp, bp;
  {
	bp->av_forw = (struct buf *)NULL;
	if(tabp->b_actf == NULL)
	  tabp->b_actf = bp;
	 else
	  tabp->b_actl->av_forw = bp;
	tabp->b_actl = bp;
  }
 */

/*
 * Interrupt (co)routine
 */

#define iwait(x)	rk7tab.b_active = x; return; case x:
rk7intr()
  {
   register struct buf *bp;

   bp = rk7tab.b_actf;
   switch (rk7tab.b_active)
    {
      case IDLE:
	debug (4) ("intr ");
	if (bp == NULL)
	  {
		rk7.spurious++;
		return;
	  }
	
	do { /* empty the queue */

		rk7.access++;
		rk7tab.b_errcnt = 0;

		do { /* transfer */
		     register short s;
		     short rk7map (), ms;

		     rk7.rwcommand = bp->b_flags & B_READ
					? (READ|IE|CDT|GO)
					: (WRITE|IE|CDT|GO);
		     if (Error)
		      {
			debug (8) ("clear ");
		        rk7clear ();
		      }

		     RK7ADDR->rkcs2 = minor (bp->b_dev);
		     RK7ADDR->rkcs1 = PACKACK|CDT|GO;
		     while (RK7ADDR->rkcs1 & GO);

		     debug (8) ("setup ");
		     rk7.ubinfo = ubasetup (bp, 1);
		     rk7start ( rk7.rwcommand,
				(short) bp->b_cylin, (short)(long) bp->b_trksec,
				(long) rk7.ubinfo,
				(short) (-(bp->b_bcount>>1)));
		     debug (8) ("started ");
		     iwait (NORMAL);

		     debug (8) ("NORMAL ");
		     while (RK7ADDR->rker == BSE && (ms = rk7map ()) != -1)
		      { /* forwarding */
			rk7.mapped++;
		     	debug (8|16) ("mapping ");

			rk7clear ();

			rk7start (rk7.rwcommand,
				  (short) BSCYL, (short) ms, (long) rk7.manow,
				  (short) (-(rk7.wcnow > NBYTES/2
						? NBYTES/2 : rk7.wcnow)));
			iwait (MAPPED);

		     	debug (8|16) ("mapped\n");
			if (!Error && rk7.wcnow > NBYTES/2)
			  {
				debug (8|16) ("restarting ");
				rk7restart ();
				iwait (RESTART);
				debug (8|16) ("restarted\n");
			  }
		      } /* forwarding */

		     if (Error)
		      {
			rk7.errer = RK7ADDR->rker;
			rk7.errcs2 = RK7ADDR->rkcs2;
			rk7.errds = RK7ADDR->rkds;
		      }
		     debug (8) (Error ? "error\n" : "free ");
		     ubafree (rk7.ubinfo);
		   } /* transfer */
		 while (Error && ++rk7tab.b_errcnt != RETRIES);

		if (rk7tab.b_errcnt != 0)
		  rk7errs (bp);

		debug (8) ("next\n");
		bp->b_resid = 0;
		rk7tab.b_actf = bp->av_forw;
		iodone (bp);

	   } /* empty the queue */
	 while ((bp = rk7tab.b_actf) != NULL);

	 rk7tab.b_active = IDLE;
	 debug (4) ("exit\n");
	 return;
    }
  }

rk7clear ()
  {
	register drive;

	drive = RK7ADDR->rkcs2 & 07;
	RK7ADDR->rkcs1 = CCLR;
	RK7ADDR->rkcs2 = drive;
	RK7ADDR->rkcs1 = DRCLR|CDT|GO;
	while ((RK7ADDR->rkcs1 & RDY) == 0);
  }

rk7start (rwcommand, cyl, tsect, mem, wc)
short cyl, tsect, wc;
long mem;
  {
	RK7ADDR->rkdc = cyl;
	RK7ADDR->rkda = tsect;
	RK7ADDR->rkba = mem;
	RK7ADDR->rkwc = wc;
	RK7ADDR->rkcs1 = ((mem >> 8) & BA1617) | rwcommand;
  }

short rk7map ()
  {
	register struct bbtbl *bbp;
	register i;

	rk7.wcnow = -RK7ADDR->rkwc;
	rk7.cylnow = RK7ADDR->rkdc;
	rk7.trksecnow = RK7ADDR->rkda;
	rk7.manow = (((long)(RK7ADDR->rkcs1 & BA1617)) << 8) + RK7ADDR->rkba;

	for (bbp = rk7bb[RK7ADDR->rkcs2 & 07].bbt, i = 0;
	     bbp->bbtcyl != -1 &&
	       (bbp->bbtcyl != rk7.cylnow || bbp->bbtts != rk7.trksecnow);
	     bbp++, i++);
	return (bbp->bbtcyl == -1 ? -1 : i /* trk & sec */);
  }

rk7restart ()
  {
	register short ts;

	ts = rk7.trksecnow;
	if ((++ts & 0377) == NSECTS)
	  if (((ts += (1<<8) - NSECTS) >> 8) == NTRKS)
	    {
		ts = 0;
		rk7.cylnow++;
	    }
	rk7start (rk7.rwcommand, (short) rk7.cylnow, (short) ts,
		  (long) rk7.manow + NBYTES, (short) (-(rk7.wcnow - NBYTES/2)));
  }

rk7errs (bp)
register struct buf *bp;
  {
	register nerr;

	nerr = rk7tab.b_errcnt;
	rk7.retries += nerr;
	if (nerr == RETRIES)
	  {
		bp->b_flags |= B_ERROR;
		rk7.errors++;
		printf ("Hard rk7 ");
	  }
	 else
	  printf ("%d * rk7 ", nerr);
	deverror (bp, rk7.errer, rk7.errcs2);
	printf ("ds %X\n", rk7.errds);
	while (nerr == RETRIES && (rk7debug & 1024));
  }

rk7read(dev)
dev_t dev;
{

	physio(rk7strategy, &rrk7buf, dev, B_READ, minphys);
}

rk7write(dev)
dev_t dev;
{

	physio(rk7strategy, &rrk7buf, dev, B_WRITE, minphys);
}
