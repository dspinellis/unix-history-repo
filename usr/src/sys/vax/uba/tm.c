/*	tm.c	4.9	%G%	*/

#include "tm.h"
#if NTM > 0
/*
 * TM tape driver
 */
#define	DELAY(N)		{ register int d; d = N; while (--d > 0); }
#include "../h/param.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/conf.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/cpu.h"

#include "../h/tmreg.h"

struct	buf	tmtab;
struct	buf	ctmbuf;
struct	buf	rtmbuf;

int	tmcntrlr(), tmslave(), tmdgo(), tmintr();
struct	uba_dinfo *tminfo[NTM];
u_short	tmstd[] = { 0 };
int	(*tmivec[])() = { tmintr, 0 };
struct	uba_driver tmdriver =
	{ tmcntrlr, tmslave, tmdgo, 0, 0, tmstd, tminfo, tmivec };
int	tm_ubinfo;

/* bits in minor device */
#define	T_NOREWIND	04
#define	T_1600BPI	08

#define	INF	(daddr_t)1000000L

/*
 * Really only handle one tape drive... if you have more than one,
 * you can put all these (and some of the above) in a structure,
 * change the obvious things, and make tmslave smarter, but
 * it is not clear what happens when some drives are transferring while
 * others rewind, so we don't pretend that this driver handles multiple
 * tape drives.
 */
char	t_openf;
daddr_t	t_blkno;
char	t_flags;
daddr_t	t_nxrec;
u_short	t_erreg;
u_short	t_dsreg;
short	t_resid;

#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */

#define	LASTIOW 1		/* last op was a write */
#define	WAITREW	2		/* someone is waiting for a rewind */

tmcntrlr(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	((struct device *)reg)->tmcs = IENABLE;
	/*
	 * If this is a tm03/tc11, it ought to have interrupted
	 * by now, if it isn't (ie: it is a ts04) then we just
	 * pray that it didn't interrupt, so autoconf will ignore it
	 * - just in case out prayers fail, we will reference one
	 * of the more distant registers, and hope for a machine
	 * check, or similar disaster
	 */
	if (badaddr(&((struct device *)reg)->tmrd, 2))
		return(0);
	return(1);
}

tmslave(ui, reg, slaveno)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	/*
	 * Due to a design flaw, we cannot ascertain if the tape
	 * exists or not unless it is on line - ie: unless a tape is
	 * mounted. This is too servere a restriction to bear.
	 * As we can only handle one tape, we might just as well insist
	 * that it be slave #0, and just assume that it exists.
	 * Something better will have to be done if you have two
	 * tapes on one controller, or two controllers
	 */
	if (slaveno != 0 || tminfo[0])
		return(0);
	return(1);
}

tmopen(dev, flag)
	dev_t dev;
	int flag;
{
	register ds, unit;
	register struct uba_dinfo *ui;

	tmtab.b_flags |= B_TAPE;
	unit = minor(dev)&03;
	if (unit>=NTM || t_openf || !(ui = tminfo[minor(dev)>>3])->ui_alive) {
		u.u_error = ENXIO;		/* out of range or open */
		return;
	}
	tcommand(dev, NOP, 1);
	if ((t_erreg&SELR) == 0) {
		u.u_error = EIO;		/* offline */
		return;
	}
	t_openf = 1;
	if (t_erreg&RWS)
		tmwaitrws(dev);			/* wait for rewind complete */
	while (t_erreg&SDWN)
		tcommand(dev, NOP, 1);		/* await settle down */
	if ((t_erreg&TUR)==0 ||
	    ((flag&(FREAD|FWRITE)) == FWRITE && (t_erreg&WRL))) {
		((struct device *)ui->ui_addr)->tmcs = DCLR|GO;
		u.u_error = EIO;		/* offline or write protect */
	}
	if (u.u_error != 0) {
		t_openf = 0;
		return;
	}
	t_blkno = (daddr_t)0;
	t_nxrec = INF;
	t_flags = 0;
	t_openf = 1;
}

tmwaitrws(dev)
	register dev;
{
	register struct device *addr =
	    (struct device *)tminfo[minor(dev)>>3]->ui_addr;

	spl5();
	for (;;) {
		if ((addr->tmer&RWS) == 0) {
			spl0();		/* rewind complete */
			return;
		}
		t_flags |= WAITREW;
		sleep((caddr_t)&t_flags, PRIBIO);
	}
}

tmclose(dev, flag)
	register dev_t dev;
	register flag;
{

	if (flag == FWRITE || ((flag&FWRITE) && (t_flags&LASTIOW))) {
		tcommand(dev, WEOF, 1);
		tcommand(dev, WEOF, 1);
		tcommand(dev, SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		tcommand(dev, REW, 1);
	t_openf = 0;
}

tcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;

	bp = &ctmbuf;
	(void) spl5();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	(void) spl0();
	bp->b_dev = dev;
	bp->b_repcnt = -count;
	bp->b_command = com;
	bp->b_blkno = 0;
	tmstrategy(bp);
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

tmstrategy(bp)
	register struct buf *bp;
{
	register daddr_t *p;

	tmwaitrws(bp->b_dev);
	if (bp != &ctmbuf) {
		p = &t_nxrec;
		if (dbtofsb(bp->b_blkno) > *p) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;		/* past EOF */
			iodone(bp);
			return;
		} else if (dbtofsb(bp->b_blkno) == *p && bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);			/* at EOF */
			iodone(bp);
			return;
		} else if ((bp->b_flags&B_READ) == 0)
			*p = dbtofsb(bp->b_blkno) + 1;	/* write sets EOF */
	}
	bp->av_forw = NULL;
	(void) spl5();
	if (tmtab.b_actf == NULL)
		tmtab.b_actf = bp;
	else
		tmtab.b_actl->av_forw = bp;
	tmtab.b_actl = bp;
	if (tmtab.b_active == 0)
		tmstart();
	(void) spl0();
}

tmstart()
{
	register struct buf *bp;
	register struct uba_dinfo *ui;
	register struct device *addr;
	register cmd;
	register daddr_t blkno;
	int s;

loop:
	if ((bp = tmtab.b_actf) == 0)
		return;
	ui = tminfo[minor(bp->b_dev)>>3];
	addr = (struct device *)ui->ui_addr;
	t_dsreg = addr->tmcs;
	t_erreg = addr->tmer;
	t_resid = addr->tmbc;
	t_flags &= ~LASTIOW;
	if (t_openf < 0 || (addr->tmcs&CUR) == 0) {
		/* t_openf = -1; ??? */
		bp->b_flags |= B_ERROR;		/* hard error'ed or !SELR */
		goto next;
	}
	cmd = IENABLE | GO;
	if ((minor(bp->b_dev) & T_1600BPI) == 0)
		cmd |= D800;
	if (bp == &ctmbuf) {
		if (bp->b_command == NOP)
			goto next;		/* just get status */
		else {
			cmd |= bp->b_command;
			tmtab.b_active = SCOM;
			if (bp->b_command == SFORW || bp->b_command == SREV)
				addr->tmbc = bp->b_repcnt;
			addr->tmcs = cmd;
			return;
		}
	}
	if ((blkno = t_blkno) == dbtofsb(bp->b_blkno)) {
		addr->tmbc = -bp->b_bcount;
		s = spl6();
		if (tm_ubinfo == 0)
			tm_ubinfo = ubasetup(ui->ui_ubanum, bp, 1);
		splx(s);
		if ((bp->b_flags&B_READ) == 0) {
			if (tmtab.b_errcnt)
				cmd |= WIRG;
			else
				cmd |= WCOM;
		} else
			cmd |= RCOM;
		cmd |= (tm_ubinfo >> 12) & 0x30;
		tmtab.b_active = SIO;
		addr->tmba = tm_ubinfo;
		addr->tmcs = cmd; 
		return;
	}
	tmtab.b_active = SSEEK;
	if (blkno < dbtofsb(bp->b_blkno)) {
		cmd |= SFORW;
		addr->tmbc = blkno - dbtofsb(bp->b_blkno);
	} else {
		cmd |= SREV;
		addr->tmbc = dbtofsb(bp->b_blkno) - blkno;
	}
	addr->tmcs = cmd;
	return;

next:
	ubarelse(ui->ui_ubanum, &tm_ubinfo);
	tmtab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

tmdgo()
{
}

tmintr(d)
{
	register struct buf *bp;
	register struct device *addr = (struct device *)tminfo[d]->ui_addr;
	register state;

	if (t_flags&WAITREW && (addr->tmer&RWS) == 0) {
		t_flags &= ~WAITREW;
		wakeup((caddr_t)&t_flags);
	}
	if ((bp = tmtab.b_actf) == NULL)
		return;
	t_dsreg = addr->tmcs;
	t_erreg = addr->tmer;
	t_resid = addr->tmbc;
	if ((bp->b_flags & B_READ) == 0)
		t_flags |= LASTIOW;
	state = tmtab.b_active;
	tmtab.b_active = 0;
	if (addr->tmcs&ERROR) {
		while(addr->tmer & SDWN)
			;			/* await settle down */
		if (addr->tmer&EOF) {
			tmseteof(bp);	/* set blkno and nxrec */
			state = SCOM;
			addr->tmbc = -bp->b_bcount;
			goto errout;
		}
		if ((bp->b_flags&B_READ) && (addr->tmer&(HARD|SOFT)) == RLE)
			goto out;
		if ((addr->tmer&HARD)==0 && state==SIO) {
			if (++tmtab.b_errcnt < 7) {
				if((addr->tmer&SOFT) == NXM)
					printf("TM UBA late error\n");
				else
					t_blkno++;
				ubarelse(tminfo[d]->ui_ubanum, &tm_ubinfo);
				tmstart();
				return;
			}
		} else if (t_openf>0 && bp != &rtmbuf)
			t_openf = -1;
		deverror(bp, t_erreg, t_dsreg);
		bp->b_flags |= B_ERROR;
		state = SIO;
	}
out:
	switch (state) {

	case SIO:
		t_blkno++;
		/* fall into ... */

	case SCOM:
		if (bp == &ctmbuf) {
			switch (bp->b_command) {
			case SFORW:
				t_blkno -= bp->b_repcnt;
				break;

			case SREV:
				t_blkno += bp->b_repcnt;
				break;

			default:
				if (++bp->b_repcnt < 0) {
					tmstart();	/* continue */
					return;
				}
			}
		}
errout:
		tmtab.b_errcnt = 0;
		tmtab.b_actf = bp->av_forw;
		bp->b_resid = -addr->tmbc;
		ubarelse(tminfo[d]->ui_ubanum, &tm_ubinfo);
		iodone(bp);
		break;

	case SSEEK:
		t_blkno = dbtofsb(bp->b_blkno);
		break;

	default:
		return;
	}
	tmstart();
}

tmseteof(bp)
	register struct buf *bp;
{
	register struct device *addr = 
	    (struct device *)tminfo[minor(bp->b_dev)>>3]->ui_addr;

	if (bp == &ctmbuf) {
		if (t_blkno > dbtofsb(bp->b_blkno)) {
			/* reversing */
			t_nxrec = dbtofsb(bp->b_blkno) - addr->tmbc;
			t_blkno = t_nxrec;
		} else {
			/* spacing forward */
			t_blkno = dbtofsb(bp->b_blkno) + addr->tmbc;
			t_nxrec = t_blkno - 1;
		}
		return;
	} 
	/* eof on read */
	t_nxrec = dbtofsb(bp->b_blkno);
}

tmread(dev)
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_READ, minphys);
}

tmwrite(dev)
{

	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_WRITE, minphys);
}

tmphys(dev)
{
	register daddr_t a;

	a = dbtofsb(u.u_offset >> 9);
	t_blkno = a;
	t_nxrec = a + 1;
}

/*ARGSUSED*/
tmioctl(dev, cmd, addr, flag)
	caddr_t addr;
	dev_t dev;
{
	register callcount;
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static tmops[] = {WEOF, SFORW, SREV, SFORW, SREV, REW, OFFL, NOP};

	switch(cmd) {
		case MTIOCTOP:	/* tape operation */
		if (copyin((caddr_t)addr, (caddr_t)&mtop, sizeof(mtop))) {
			u.u_error = EFAULT;
			return;
		}
		switch(mtop.mt_op) {
		case MTWEOF: case MTFSF: case MTBSF:
			callcount = mtop.mt_count;
			fcount = INF;
			break;
		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop.mt_count;
			break;
		case MTREW: case MTOFFL: case MTNOP:
			callcount = 1;
			fcount = 1;
			break;
		default:
			u.u_error = ENXIO;
			return;
		}
		if (callcount <= 0 || fcount <= 0)
			u.u_error = ENXIO;
		else while (--callcount >= 0) {
			tcommand(dev, tmops[mtop.mt_op], fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    ctmbuf.b_resid) {
				u.u_error = EIO;
				break;
			}
			if ((ctmbuf.b_flags&B_ERROR) || t_erreg&BOT)
				break;
		}
		geterror(&ctmbuf);
		return;
	case MTIOCGET:
		mtget.mt_dsreg = t_dsreg;
		mtget.mt_erreg = t_erreg;
		mtget.mt_resid = t_resid;
		if (copyout((caddr_t)&mtget, addr, sizeof(mtget)))
			u.u_error = EFAULT;
		return;
	default:
		u.u_error = ENXIO;
	}
}

#define	DBSIZE	20

tmdump()
{

	tmwall((char *)0, maxfree);	/* write out memory */
	tmeof();
	tmeof();
	tmrewind();
	tmwait();
}

tmwall(start, num)
	int start, num;
{
	register struct uba_dinfo *ui;
	register struct uba_regs *up;
	register struct device *addr;
	int blk, bdp;

#define	phys1(a,b)	((b)((int)(a)&0x7fffffff))
#define	phys(a,b)	phys1(*phys1(&a, b*), b)
	if (tminfo[0] == 0) {
		printf("dna\n");
		return (-1);
	}
	ui = phys(tminfo[0], struct uba_dinfo *);
	up = phys(ui->ui_hd, struct uba_hd *)->uh_physuba;
#if VAX780
	if (cpu == VAX_780)
		ubainit(up);
#endif
	DELAY(1000000);
	addr = (struct device *)ui->ui_physaddr;
	tmwait(addr);
	addr->tmcs = DCLR | GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tmdwrite(start, blk, addr, up);
		start += blk;
		num -= blk;
	}
	bdp = 1;		/* crud to fool c compiler */
	up->uba_dpr[bdp] |= UBA_BNE;
	return (0);
}

tmdwrite(buf, num, addr, up)
	register buf, num;
	register struct device *addr;
	struct uba_regs *up;
{
	register struct pte *io;
	register int npf;
	int bdp;

	tmwait(addr);
	bdp = 1;		/* more dastardly tricks on pcc */
	up->uba_dpr[bdp] |= UBA_BNE;
	io = up->uba_map;
	npf = num+1;
	while (--npf != 0)
		 *(int *)io++ = (buf++ | (1<<UBA_DPSHIFT) | UBA_MRV);
	*(int *)io = 0;
	addr->tmbc = -(num*NBPG);
	addr->tmba = 0;
	addr->tmcs = WCOM | GO;
}

tmwait(addr)
	register struct device *addr;
{
	register s;

	do
		s = addr->tmcs;
	while ((s & CUR) == 0);
}

tmrewind(addr)
	struct device *addr;
{

	tmwait(addr);
	addr->tmcs = REW | GO;
}

tmeof(addr)
	struct device *addr;
{

	tmwait(addr);
	addr->tmcs = WEOF | GO;
}
#endif
