/*	tm.c	4.2	%G%	*/

#include "../conf/tm.h"
#if NTM > 0
/*
 * TM tape driver
 */

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

struct device {
	u_short	tmer;
	u_short	tmcs;
	short	tmbc;
	u_short tmba;
	short	tmdb;
	short	tmrd;
};

#define	b_repcnt  b_bcount
#define	b_command b_resid

struct	buf	tmtab;
struct	buf	ctmbuf;
struct	buf	rtmbuf;

int	tm_ubinfo;

/* bits in minor device */
#define	T_NOREWIND	04
#define	T_1600BPI	08

#define	INF	(daddr_t)1000000L

/*
 * Really only handle one tape drive... if you have more than one,
 * you can make all these arrays and change the obvious things, but
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

/* bits in tmcs */
#define	GO	01
#define	OFFL	0
#define	RCOM	02
#define	WCOM	04
#define	WEOF	06
#define	SFORW	010
#define	SREV	012
#define	WIRG	014
#define	REW	016
#define	IENABLE	0100
#define	CUR	0200
#define	NOP	IENABLE
#define	DCLR	010000
#define	D800	060000
#define	ERROR	0100000

/* bits in tmer */
#define	TUR	1
#define	RWS	02
#define	WRL	04
#define	SDWN	010
#define	BOT	040
#define	SELR	0100
#define	NXM	0200
#define	TMBTE	0400
#define	RLE	01000
#define	EOT	02000
#define	BGL	04000
#define	PAE	010000
#define	CRE	020000
#define	EOF	040000
#define	ILC	0100000

#define	HARD    (ILC|EOT)
#define	SOFT	(CRE|PAE|BGL|RLE|TMBTE|NXM)

#define	SSEEK	1		/* seeking */
#define	SIO	2		/* doing seq i/o */
#define	SCOM	3		/* sending control command */

#define	LASTIOW 1		/* last op was a write */
#define	WAITREW	2		/* someone is waiting for a rewind */

tmopen(dev, flag)
	dev_t dev;
	int flag;
{
	register ds, unit;

	tmtab.b_flags |= B_TAPE;
	unit = minor(dev)&03;
	if (unit >= NTM || t_openf) {
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
		TMADDR->tmcs = DCLR|GO;
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

	spl5();
	for (;;) {
		if ((TMADDR->tmer&RWS) == 0) {
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
	register cmd;
	register daddr_t blkno;

loop:
	if ((bp = tmtab.b_actf) == 0)
		return;
	t_dsreg = TMADDR->tmcs;
	t_erreg = TMADDR->tmer;
	t_resid = TMADDR->tmbc;
	t_flags &= ~LASTIOW;
	if (t_openf < 0 || (TMADDR->tmcs&CUR) == 0) {
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
				TMADDR->tmbc = bp->b_repcnt;
			TMADDR->tmcs = cmd;
			return;
		}
	}
	if ((blkno = t_blkno) == dbtofsb(bp->b_blkno)) {
		TMADDR->tmbc = -bp->b_bcount;
		if (tm_ubinfo == 0)
			tm_ubinfo = ubasetup(bp,1);
		if ((bp->b_flags&B_READ) == 0) {
			if (tmtab.b_errcnt)
				cmd |= WIRG;
			else
				cmd |= WCOM;
		} else
			cmd |= RCOM;
		cmd |= (tm_ubinfo >> 12) & 0x30;
		tmtab.b_active = SIO;
		TMADDR->tmba = tm_ubinfo;
		TMADDR->tmcs = cmd; 
		return;
	}
	tmtab.b_active = SSEEK;
	if (blkno < dbtofsb(bp->b_blkno)) {
		cmd |= SFORW;
		TMADDR->tmbc = blkno - dbtofsb(bp->b_blkno);
	} else {
		cmd |= SREV;
		TMADDR->tmbc = dbtofsb(bp->b_blkno) - blkno;
	}
	TMADDR->tmcs = cmd;
	return;

next:
	if (tm_ubinfo != 0) {
		ubafree(tm_ubinfo);
		tm_ubinfo = 0;
	}
	tmtab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

tmintr()
{
	register struct buf *bp;
	register state;

	if (t_flags&WAITREW && (TMADDR->tmer&RWS) == 0) {
		t_flags &= ~WAITREW;
		wakeup((caddr_t)&t_flags);
	}
	if ((bp = tmtab.b_actf) == NULL)
		return;
	t_dsreg = TMADDR->tmcs;
	t_erreg = TMADDR->tmer;
	t_resid = TMADDR->tmbc;
	if ((bp->b_flags & B_READ) == 0)
		t_flags |= LASTIOW;
	state = tmtab.b_active;
	tmtab.b_active = 0;
	if (TMADDR->tmcs&ERROR) {
		while(TMADDR->tmer & SDWN)
			;			/* await settle down */
		if (TMADDR->tmer&EOF) {
			tmseteof(bp);	/* set blkno and nxrec */
			state = SCOM;
			TMADDR->tmbc = -bp->b_bcount;
			goto errout;
		}
		if ((bp->b_flags&B_READ) && (TMADDR->tmer&(HARD|SOFT)) == RLE)
			goto out;
		if ((TMADDR->tmer&HARD)==0 && state==SIO) {
			if (++tmtab.b_errcnt < 7) {
				if((TMADDR->tmer&SOFT) == NXM)
					printf("TM UBA late error\n");
				else
					t_blkno += 2;		/* ???????? */
				if (tm_ubinfo) {
					ubafree(tm_ubinfo);
					tm_ubinfo = 0;
				}
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
		bp->b_resid = -TMADDR->tmbc;
		if (tm_ubinfo != 0) {
			ubafree(tm_ubinfo);
			tm_ubinfo = 0;
		}
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

	if (bp == &ctmbuf) {
		if (t_blkno > dbtofsb(bp->b_blkno)) {
			/* reversing */
			t_nxrec = dbtofsb(bp->b_blkno) - TMADDR->tmbc;
			t_blkno = t_nxrec;
		} else {
			/* spacing forward */
			t_blkno = dbtofsb(bp->b_blkno) + TMADDR->tmbc;
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
	static tmops[] = {WEOF, SFORW, SREV, SFORW, SREV, REW, OFFL};

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
		case MTREW: case MTOFFL:
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

twall(start, num)
	int start, num;
{
#if VAX==780
	register struct uba_regs *up = (struct uba_regs *)PHYSUBA0;
#endif
	int blk;

	TMPHYS->tmcs = DCLR | GO;
#if VAX==780
	up->uba_cr = ADINIT;
	up->uba_cr = IFS|BRIE|USEFIE|SUEFIE;
	while ((up->uba_cnfgr & UBIC) == 0)
		;
#endif
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tmdwrite(start, blk);
		start += blk;
		num -= blk;
	}
	((struct uba_regs *)PHYSUBA0)->uba_dpr[1] |= BNE;
}

tmdwrite(buf, num)
register buf, num;
{
	register int *io, npf;

	tmwait();
	((struct uba_regs *)PHYSUBA0)->uba_dpr[1] |= BNE;
	io = (int *)((struct uba_regs *)PHYSUBA0)->uba_map;
	npf = num+1;
	while (--npf != 0)
		 *io++ = (int)(buf++ | (1<<21) | MRV);
	*io = 0;
	TMPHYS->tmbc = -(num*NBPG);
	TMPHYS->tmba = 0;
	TMPHYS->tmcs = WCOM | GO | D800;
}

tmwait()
{
	register s;

	do
		s = TMPHYS->tmcs;
	while ((s & CUR) == 0);
}

tmrewind()
{

	tmwait();
	TMPHYS->tmcs = REW | GO;
}

tmeof()
{

	tmwait();
	TMPHYS->tmcs = WEOF | GO | D800;
}
#endif
