#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 * DV disk driver
 */

#include "../param.h"
#include "../buf.h"
#include "../conf.h"
#include "../user.h"

struct {
	int	dvcsr;
	int	dvdbr;
	int	dvmar;
	int	dvwcr;
	int	dvcbr;
	int	dvssr;
	int	dvair;
	int	dvusr;
};
struct { char lbyte, hbyte; };

#define	DVADDR	0164000
#define	NDV	1

struct {
	char	*nblocks;
	int	cyloff;
} dv_sizes[] {
	48720,	0,		/* cyl 0 thru 202	*/
	48720,	203,		/* cyl 203 thru 405	*/
	5040,	0,		/* cyl 0 thru 20	*/
	21840,	21,		/* cyl 21 thru 111	*/
	21840,	112,		/* cyl 112 thru 202	*/
	21840,	203,		/* cyl 203 thru 293	*/
	21840,	294,		/* cyl 294 thru 384	*/
	5040,	385,		/* cyl 385 thru 405	*/
};

struct {
	int	hd1;
	int	hd2;
	int	cksum;
	char	*chadr;
} dvhdr;
int	dv_unit -1, dv_cyl, dv_head, dv_sctr, dv_count, dv_addr[2];
int	dvhist;

struct	devtab	dvtab;
struct	buf	rdvbuf;

#define	HTBCOM	000000
#define	CTBCOM	020000
#define	CNBCOM	030000
#define		INCHD	01
#define		RECAL	02
#define		RESET	020
#define		SEEK	040
#define		CLRDA	0100
#define	CHRCOM	070000
#define	CHWCOM	0130000
#define	LDSCOM	0140000
#define	CTLRST	0170000

#define	DRVRDY	04000
#define	ATTN	0400
#define	DONE	0200
#define	IENABLE	0100
#define	GO	01

/*
 * Use av_back to save sector,
 * b_resid for cylinder+track.
 */

#define	dvsec	av_back
#define	cylhd	b_resid

dvstrategy(abp)
struct buf *abp;
{
	register struct buf *bp;
	register char *p1, *p2;

	bp = abp;
	p1 = &dv_sizes[bp->b_dev.d_minor&07];
	if (bp->b_dev.d_minor >= (NDV<<3) ||
	    bp->b_blkno >= p1->nblocks) {
		bp->b_flags =| B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = 0;
	p2 = ldiv(bp->b_blkno,12);
	bp->cylhd = ((p1->cyloff+p2/20)<<5)|(p2%20);
	bp->dvsec = lrem(bp->b_blkno,12);
	spl5();
	if ((p1 = dvtab.d_actf)==0)
		dvtab.d_actf = bp;
	else {
		for (; p2 = p1->av_forw; p1 = p2) {
			if (p1->cylhd <= bp->cylhd
			 && bp->cylhd <  p2->cylhd
			 || p1->cylhd >= bp->cylhd
			 && bp->cylhd >  p2->cylhd) 
				break;
		}
		bp->av_forw = p2;
		p1->av_forw = bp;
	}
	if (dvtab.d_active==0)
		dvstart();
	spl0();
}

dvstart()
{
	register struct buf *bp;

	if ((bp = dvtab.d_actf) == 0)
		return;
	dv_cyl = bp->cylhd>>5;
	dv_head = bp->cylhd&037;
	dv_sctr = bp->dvsec;
	dv_count = bp->b_wcount<<1;
	dv_addr[0] = (bp->b_flags&B_XMEM)>>4;
	dv_addr[1] = bp->b_addr;
	dvexec();
}

dvexec()
{
	register struct buf *bp;

	bp = dvtab.d_actf;
	dvtab.d_active++;
	if (dv_unit!=(bp->b_dev.d_minor>>3)) {	/* select unit */
		dv_unit = bp->b_dev.d_minor>>3;
		DVADDR->dvcbr = LDSCOM | dv_unit;
	}
	DVADDR->dvcbr = CNBCOM | RESET | CLRDA;	/* reset and clear */
	if (dv_cyl != (~(DVADDR->dvssr|0177000))) {	/* seek */
		DVADDR->dvcbr = CTBCOM | dv_cyl;
		if(dvrdy()) return;
		DVADDR->dvcbr = CNBCOM | SEEK | RESET;
		DVADDR->dvair = 1<<dv_unit;
		DVADDR->dvcsr = DONE | IENABLE;
		return;
	}
	DVADDR->dvcbr = HTBCOM | dv_head;	/* select head */
	if(dv_count <= -512)
		DVADDR->dvwcr = -512; else
		DVADDR->dvwcr = dv_count;
	dvhdr.hd1 = (dv_head<<8)+dv_cyl;	/* set up header */
	dvhdr.hd2 = 0170000|dv_sctr;
	dvhdr.cksum = -dvhdr.hd1-dvhdr.hd2;
	dvhdr.chadr = dv_addr[1];
	if(dvrdy()) return;
	DVADDR->dvmar = &dvhdr;
	DVADDR->dvcbr = ((bp->b_flags&B_READ)? CHRCOM : CHWCOM ) |
		(dv_sctr<<1);
	DVADDR->dvcsr = IENABLE | GO | (dv_addr[0]<<4);
}

dvintr()
{
	register struct buf *bp;
	register int csr;

	if (dvtab.d_active == 0)
		return;
	bp = dvtab.d_actf;
	dvtab.d_active = 0;
	csr = DVADDR->dvcsr;
	DVADDR->dvcsr = DONE;
	if (csr&ATTN) { /* seek complete */
		DVADDR->dvair.lbyte = 0;
		if(DVADDR->dvssr>0) { /* error */
printf("ssr %o\n",DVADDR->dvssr);
			DVADDR->dvcbr = CNBCOM | RECAL | RESET;
			dv_unit = -1;
			if(dvrdy()) return;
			DVADDR->dvcbr = CTLRST;
			if (++dvtab.d_errcnt<=10) {
				dvexec();
				return;
			}
			dvherr(0);
			return;
		} else {
			dvexec();
			return;
		}
	} else {	/* r/w complete */
		if (csr<0) { /* error */
			dvhist++;
			dv_unit = -1;
			if((dvtab.d_errcnt&03)==03) {
				DVADDR->dvcbr = CNBCOM | RECAL | RESET;
printf("csr %o\n",csr);
				if(dvrdy()) return;
			}
			DVADDR->dvcbr = CTLRST;
			if(++dvtab.d_errcnt<=12) {
				dvexec();
				return;
			}
			dvherr(0);
			return;
		} else {
			if ((dv_count =+ 512)<0) { /* more to do */
				dpadd(dv_addr,512);
				if (++dv_sctr>=12) {
					dv_sctr = 0;
					if (++dv_head>=20) {
						dv_head = 0;
						dv_cyl++;
					}
				}
				dvexec();
				return;
			}
		}
	}
	dvtab.d_errcnt = 0;
	dvtab.d_actf = bp->av_forw;
	bp->b_resid = DVADDR->dvwcr+0160000;
	iodone(bp);
	dvstart();
}


dvrdy()
{
	register int cnt;
	cnt = 0;
	while(--cnt && (DVADDR->dvssr&DRVRDY));
	if (cnt==0) {
printf("diva not ready\n");
		dvherr(1);
		return(1);
	} else
		return(0);
}

dvherr(n)
{
	register struct buf *bp;

	bp = dvtab.d_actf;
printf("dev %o,blk %o,flg %o\n",bp->b_dev,bp->b_blkno,bp->b_flags);
	bp->b_flags =| B_ERROR;
	dvtab.d_errcnt = 0;
	dvtab.d_active = 0;
	dvtab.d_actf = bp->av_forw;
	iodone(bp);
	if(n==0)
		dvstart();
}
dvread(dev)
{

	if(dvphys(dev))
	physio(dvstrategy, &rdvbuf, dev, B_READ);
}

dvwrite(dev)
{

	if(dvphys(dev))
	physio(dvstrategy, &rdvbuf, dev, B_WRITE);
}

dvphys(dev)
{
	register c;

	c = lshift(u.u_offset, -9);
	c =+ ldiv(u.u_count+511, 512);
	if(c > dv_sizes[dev.d_minor & 07].nblocks) {
		u.u_error = ENXIO;
		return(0);
	}
	return(1);
}
