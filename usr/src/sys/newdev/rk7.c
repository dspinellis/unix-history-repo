/*
 * RK7 disk driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/uba.h"

#define	RK7ADDR	((struct rk7_regs *)( UBA0_DEV + 0177440))
#define ERR_CNT 10
#define	NRK7	8
#define	NRK7BLK	53790
#define FORMAT_22 0
#define	RESET	0102000
#define	WCOM	022
#define	RCOM	020
#define RK07	02000
#define	GO	01
#define RELEASE	010
#define	IENABLE	0000100
#define	CTLRDY	0200
#define	PACKAK	000003	/* Pack Acknowledge */


struct	rk7_regs
{
	short rk7cs1;
	short rk7wc;
	unsigned short rk7ba;
	short rk7da;
	short rk7cs2;
	short rk7ds;
	short	rk7er;
	short rk7asof;
	short rk7dc;
	short rk7null;
	short rk7db;
	short rk7mr1;
	short rk7ecps;
	short rk7ecpt;
	short rk7mr2;
	short rk7mr3;
};

struct	buf	rk7tab;
struct	buf	rrk7buf;

int rk7_ubinfo;

rk7strategy(bp)
register struct buf *bp;
{

	if (bp->b_blkno >= NRK7BLK) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = (struct buf *)NULL;
	spl5();
	if(rk7tab.b_actf == NULL)
		rk7tab.b_actf = bp;
	else
		rk7tab.b_actl->av_forw = bp;
	rk7tab.b_actl = bp;
	if(rk7tab.b_active == NULL)
		rk7start();
	spl0();
}

rk7start()
{
	register struct buf *bp;
	register short com;
	daddr_t bn;
	short dn, cn, sn, tn;

	if ((bp = rk7tab.b_actf) == NULL)
		return;
	rk7tab.b_active++;
	rk7_ubinfo = ubasetup( bp, 1 );
	bn = bp->b_blkno;
	dn = minor(bp->b_dev);
	cn = bn/66;
	sn = bn%22;
	tn = (bn / 22) % 3;
/*
printf("in start bn = %d dn = %d cn = %d sn = %d tn = %d\n",bn,dn,cn,sn,tn);
*/
	RK7ADDR -> rk7cs2 = dn;
	RK7ADDR -> rk7cs1 = PACKAK | RK07;
	while(RK7ADDR->rk7cs1 & 01);

	RK7ADDR -> rk7da = sn | (tn << 8);
	RK7ADDR -> rk7dc = cn;
	RK7ADDR->rk7ba = rk7_ubinfo;
	RK7ADDR->rk7wc = -(bp->b_bcount>>1);

	com = ((rk7_ubinfo & 0x30000) >> 8 ) | IENABLE | RK07 | GO | ( FORMAT_22<< 12);
	if(bp->b_flags & B_READ)
		com |= RCOM; else
		com |= WCOM;
	RK7ADDR->rk7cs1 = com;
/*
printf("cs1 in start = %d(bit15) %o(remainder)\n",(RK7ADDR->rk7cs1>>15)&01,RK7ADDR->rk7cs1 & 077777);
*/
}

rk7intr()
{
	register struct buf *bp;


/*
printf("in intr->rk7ds = %d(bit 15) %o(remainder)",(RK7ADDR->rk7ds>>15)&01,RK7ADDR->rk7ds & 077777);
printf("cs1 = %d(bit 15) %o(remainder)\n",(RK7ADDR->rk7cs1>>15)&01,RK7ADDR->rk7cs1 & 077777);
*/
	if (rk7tab.b_active == NULL)
		return;
	bp = rk7tab.b_actf;
	rk7tab.b_active = NULL;
	while(RK7ADDR->rk7ds >= 0);
/*
	printf("rk7ds = %o(bit 15)  %o\n",(RK7ADDR -> rk7ds >> 15) & 01,RK7ADDR -> rk7ds & 077777);
printf("after ready wait, cs1 = %d (bit 15) %o (remainder)\n",(RK7ADDR->rk7cs1>>15)&01,RK7ADDR->rk7cs1&077777);
*/

	if (RK7ADDR->rk7cs1 < 0) {		/* error bit */
		deverror(bp, RK7ADDR->rk7er, RK7ADDR->rk7cs2);
		printf("err reg = %o octal\n",RK7ADDR->rk7er);
		RK7ADDR->rk7cs1 = RESET|GO;
		while((RK7ADDR->rk7cs1&CTLRDY) == 0)
			;
		if (++rk7tab.b_errcnt <= ERR_CNT) {
			ubafree(rk7_ubinfo);
			rk7start();
			return;
		}
		bp->b_flags |= B_ERROR;
	}
	rk7tab.b_errcnt = 0;
	rk7tab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	ubafree(rk7_ubinfo);
	iodone(bp);
	RK7ADDR->rk7cs2 |= RELEASE;
	RK7ADDR->rk7cs1 = GO;
	rk7start();
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
