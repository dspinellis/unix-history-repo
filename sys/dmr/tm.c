#
/*
 * TM tape driver
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"

struct {
	int tmer;
	int tmcs;
	int tmbc;
	int tmba;
};

char	t_openf[8];
char	*t_blkno[8];
char	*t_nxrec[8];

#define	TMADDR	0172520
#define	JTM	3

#define	GO	01
#define	RCOM	02
#define	WCOM	04
#define	WEOF	06
#define	SFORW	010
#define	SREV	012
#define	WIRG	014
#define	REW	016
#define	DENS	060000		/* 9-channel */
#define	IENABLE	0100
#define	CRDY	0200
#define	HARD	0102200	/* ILC, EOT, NXM */

#define	SSEEK	1
#define	SIO	2

tmopen(dev, flag)
{
	register dminor;

	dminor = dev.d_minor;
	if (t_openf[dminor])
		u.u_error = ENXIO;
	else {
		t_openf[dminor]++;
		t_blkno[dminor] = 0;
		t_nxrec[dminor] = 65535;
	}
}

tmclose(dev, flag)
{
	t_openf[dev.d_minor] = 0;
	if (flag)
		tcommand(dev.d_minor, WEOF);
	tcommand(dev.d_minor, REW);
}

tcommand(unit, com)
{
	extern lbolt;

	while (devtab[JTM].d_active || (TMADDR->tmcs & CRDY)==0)
		sleep(&lbolt, 1);
	TMADDR->tmcs = DENS|com|GO | (unit<<8);
}

tmstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register char **p;

	bp = abp;
	p = &t_nxrec[bp->b_dev.d_minor];
	if (*p <= bp->b_blkno) {
		if (*p < bp->b_blkno) {
			bp->b_flags =| B_ERROR;
			iodone(bp);
			return;
		}
		if (bp->b_flags&B_READ) {
			clrbuf(bp);
			iodone(abp);
			return;
		}
	}
	if ((bp->b_flags&B_READ)==0)
		*p = bp->b_blkno + 1;
	bp->av_forw = 0;
	spl5();
	if (devtab[JTM].d_actf==0)
		devtab[JTM].d_actf = bp;
	else
		devtab[JTM].d_actl->av_forw = bp;
	devtab[JTM].d_actl = bp;
	if (devtab[JTM].d_active==0)
		tmstart();
	spl0();
}

tmstart()
{
	register struct buf *bp;
	register int com;
	int unit;
	register char *blkno;

    loop:
	if ((bp = devtab[JTM].d_actf) == 0)
		return;
	unit = bp->b_dev.d_minor;
	blkno = t_blkno[unit];
	if (t_openf[unit] < 0 || (TMADDR->tmcs & CRDY)==0) {
		bp->b_flags =| B_ERROR;
		devtab[JTM].d_actf = bp->av_forw;
		iodone(bp);
		goto loop;
	}
	com = (unit<<8) | (bp->b_flags&B_XMEM) | IENABLE|DENS|GO;
	if (blkno != bp->b_blkno) {
		devtab[JTM].d_active = SSEEK;
		if (blkno < bp->b_blkno) {
			com =| SFORW;
			TMADDR->tmbc = blkno - bp->b_blkno;
		} else {
			if (blkno==0)
				com =| REW;
			else {
				com =| SREV;
				TMADDR->tmbc = bp->b_blkno - blkno;
			}
		}
		TMADDR->tmcs = com;
		return;
	}
	devtab[JTM].d_active = SIO;
	TMADDR->tmbc = bp->b_wcount << 1;
	TMADDR->tmba = bp->b_addr;		/* core address */
	TMADDR->tmcs = com | ((bp->b_flags&B_READ)? RCOM:
	    ((devtab[JTM].d_errcnt)? WIRG: WCOM));
}

tmintr()
{
	register struct buf *bp;
	register int unit;

	if ((bp = devtab[JTM].d_actf)==0)
		return;
	unit = bp->b_dev.d_minor;
	if (TMADDR->tmcs < 0) {		/* error bit */
		if ((TMADDR->tmer&HARD)==0 && devtab[JTM].d_active==SIO) {
			if (++devtab[JTM].d_errcnt < 10) {
				t_blkno[unit]++;
				devtab[JTM].d_active = 0;
				tmstart();
				return;
			}
		} else
			t_openf[unit] = -1;
		bp->b_flags =| B_ERROR;
		devtab[JTM].d_active = SIO;
	}
	if (devtab[JTM].d_active == SIO) {
		devtab[JTM].d_errcnt = 0;
		t_blkno[unit]++;
		devtab[JTM].d_actf = bp->av_forw;
		devtab[JTM].d_active = 0;
		iodone(bp);
	} else
		t_blkno[unit] = bp->b_blkno;
	tmstart();
}
