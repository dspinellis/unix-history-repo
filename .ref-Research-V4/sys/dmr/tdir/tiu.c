#
/*
 * TIU (DR11-B) interface to Spider
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/file.h"
#include "/sys/nsys/reg.h"

#define	NCHAN	8

/* bits in tiuch flags */
#define	T_WRITR	01
#define	T_RAVL	02
#define	T_ERROR	04
#define	T_DONE	010
#define	T_OPEN	020
#define	T_EOF	040

/* drst bits */
#define	T_TERROR 0100000
#define	T_REJ	04000
#define	T_IDLE	02000

/* drdb bits */
#define	T_BKSTS	0100000
#define	T_SLSTS	040000
#define	T_WTSTS	010000
#define	T_TROUB	02000
#define	T_ODD	01000
#define	T_SIGNL	0400
#define	T_SELW	0200

#define	TIUPRI	2
#define	TIUADDR	172430

/* tiu command bits */
#define	IENABLE	0100
#define	GO	01
#define	STOP	0
#define	RCH	02
#define	RDC	04
#define	RNM	06
#define	WSB	010
#define	WCH	012
#define	WDC	014
#define	WDB	016

#define	SREAD	1
#define	SWRITE	2
#define	SWSIG	3
#define	SWDONE	4

struct {
	int	drwc;
	int	drba;
	int	drst;
	int	drdb;
};
struct tiuch {
	char	t_flags;
	char	t_isig;
	char	t_osig;
	char	t_troub;
	char	*t_buffer;
};

struct tiuch tiu_dchan[NCHAN];
struct tiuch tiu_cchan[NCHAN];

struct {
	char	lbyte;
	char	hbyte;
};

struct tiu {
	char	t_state;
	char	t_chan;
	char	t_wflg;
	struct buf	*t_actf;
	struct buf	*t_actl;
} tiu;

tiuopen(dev, flag)
{
	struct tiuch *cp;

	for (cp=tiu_dchan; cp < &tiu_dchan[NCHAN]; cp++) {
		if (cp->t_flags&T_OPEN || (cp+NCHAN)->t_flags&T_OPEN)
			continue;
		cp->t_flags = T_OPEN;
		cp->t_osig = 1;
		return;
	}
	u.u_error = ENXIO;
}

tiuclose()
{
}

tiuwrite(dev)
{
	int n, i, c;
	struct tiuch *cp;
	struct buf *bp;
	char *p;

	if ((cp = tiuchan(&i)) == NULL)
		return;
	spl5();
	if (cp->t_flags&T_WRITR == 0) {
		while(tiubusy()) {
			tiu.t_wflg++;
			sleep(&tiu, TIUPRI);
		}
		stiuchan(i+0200);	/* select W */
	}
	spl0();
	do {
		bp = getblk(NODEV);
		bp->b_flags = 0;
		p = bp->b_addr;
		for (n=0; n<512 && passc(&c) >= 0; n++)
			*p++ = c;
		if (u.u_count == 0 || u.u_error)
			bp->b_flags.hbyte = cp->t_osig;
		bp->b_wcount = n;
		bp->b_blkno = i;
		spl5();
		while ((cp->t_flags&(T_WRITR|T_ERROR)) == 0)
			sleep(cp, TIUPRI);
		cp->t_flags =& ~T_DONE;
		tiustrategy(bp);
		while ((cp->t_flags&(T_DONE|T_ERROR)) == 0)
			sleep(bp, TIUPRI);
		spl0();
		if (cp->t_flags&T_ERROR)
			u.u_error = EIO;
		brelse(bp);
	} while (u.u_count>0 && u.u_error==0);
}

tiuread(dev)
{
	int i, n;
	char *p;
	struct tiuch *cp;
	struct buf *bp;

	if ((cp = tiuchan(&i)) == NULL)
		return;
	i = cp - tiu_dchan;
	do {
		if (bp = cp->t_buffer) {
			n = bp->b_wcount;
			p = bp->av_forw;
		} else {
			spl5();
			while ((cp->t_flags&(T_RAVL|T_ERROR|T_EOF))==0)
				sleep(cp, TIUPRI);
			if (cp->t_flags&T_ERROR)
				goto rerr;
			if (cp->t_flags&T_EOF)
				return;
			bp = getblk(NODEV);
			bp->b_flags = B_READ;
			bp->b_blkno = i;
			cp->t_flags =& ~T_DONE;
			tiustrategy(bp);
			while ((cp->t_flags&(T_DONE|T_ERROR|T_EOF)) == 0)
				sleep(cp, TIUPRI);
			cp->t_isig = bp->b_flags.hbyte;
			if (cp->t_flags&T_ERROR)
		    rerr:
				u.u_error = EIO;
			n = bp->b_wcount;
			p = bp->b_addr;
			spl0();
		}
		if (cp->t_flags&T_EOF) {
			n = 0;
			u.u_count = 0;
		}
		while ((n--)>0 && cpass(*p++)>=0);
		if (n<=0 || u.u_error) {
			bp->b_flags = 0;
			brelse(bp);
		} else {
			bp->b_wcount = n;
			bp->av_forw = p;
			cp->t_buffer = bp;
		}
	} while (u.u_count && u.u_error==0);
}

tiustart()
{
	struct buf *bp;

	if (tiubusy())
		return;
	if ((bp = tiu.t_actf)==0) {
		if (tiu.t_wflg) {
			tiu.t_wflg = 0;
			wakeup(&tiu);
		}
		return;
	}
	stiuchan(bp->b_blkno);
	TIUADDR->drba = bp->b_addr;
	if (bp->b_flags&B_READ) {
		TIUADDR->drwc = -256;
		tiu.t_state = SREAD;
		TIUADDR->drst = IENABLE|RDC|GO;
	} else {
		tiu.t_state = SWRITE;
		if ((TIUADDR->drwc = -(bp->b_wcount>>1))==0) {
			tiuintr();
			return;
		}
		TIUADDR->drst = IENABLE|WDC|GO;
	}
}

stiuchan(c)
{
	if (c != tiu.t_chan) {
		tiu.t_chan = c&0177;
		if ((c&0177)>=NCHAN)
			c =+ 64-NCHAN;
		TIUADDR->drdb = c;
		TIUADDR->drst = IENABLE|WCH|GO;
	}
}

tiuintr()
{
	struct buf *bp;
	struct tiuch *cp, *bcp;
	int i, s;

	if (TIUADDR->drst&(T_TERROR|T_REJ)) {
		tiuerr(-1, 2);
		return;
	}
	if (tiu.t_chan>=0)
		cp = &tiu_dchan[tiu.t_chan];
	else
		cp = NULL;
	s = tiu.t_state;
	tiu.t_state = 0;
	if (s) {
		bp = tiu.t_actf;
		if (bp==NULL || cp==NULL) {
			tiuerr(-1, 0);
			return;
		}
	}
	if (TIUADDR->drdb&T_TROUB) {
		tiuerr(tiustop(), TIUADDR->drdb);
		goto done;
	}
	switch (s) {

	case SREAD:
		if (TIUADDR->drdb&T_SIGNL == 0) {
			tiuerr(tiustop(), 10);
			goto done;
		}
		s = 512 + (TIUADDR->drwc<<1);
		if ((TIUADDR->drst&T_ODD) == 0)
			s--;
		if ((bp->b_flags.hbyte = TIUADDR->drdb) != 0)
			cp->t_flags =& ~T_RAVL;
		goto done;

	case SWRITE:
		if (bp->b_wcount&01) {
			TIUADDR->drdb = bp->b_addr[bp->b_wcount-1];
			TIUADDR->drst = IENABLE|WDB|GO;
			if ((TIUADDR->drst & T_IDLE)==0) {
				tiu.t_state = SWSIG;
				return;
			}
		}

	case SWSIG:
		cp->t_flags =& ~T_WRITR;
		TIUADDR->drdb = bp->b_flags.hbyte;
		TIUADDR->drst = IENABLE|WSB|GO;
		if ((TIUADDR->drst & T_IDLE)==0) {
			tiu.t_state = SWDONE;
			return;
		}

	done:
	case SWDONE:
		tiu.t_state = 0;
		tiu.t_actf = bp->av_forw;
		wakeup(cp);
		cp->t_flags =| T_DONE;

	default:
		if (TIUADDR->drdb&T_WTSTS && cp)
			cp->t_flags =| T_WRITR;
		while (TIUADDR->drdb&T_BKSTS) {
			TIUADDR->drst = IENABLE|RCH|GO;
			i = TIUADDR->drdb&0177;
			if (i>=64)
				i =+ NCHAN-64;
			if (i<0 || i >= 2*NCHAN) {
				i = 0;
				tiuerr(-1, 0);
			}
			bcp = &tiu_dchan[i];
			if (s==0 || bcp != cp)
				bcp =| (TIUADDR->drdb&T_SELW)?
					T_RAVL:T_WRITR;
			wakeup(bcp);
		}
		tiustart();
	}
}

tiustop()
{
	register int lastchan;

	lastchan = tiu.t_chan;
	tiu.t_chan = -1;
	tiu.t_state = 0;
	TIUADDR->drst = IENABLE|STOP|GO;
	return(lastchan);
}

tiuerr(chan, code)
{
	struct tiuch *cp;

	if (0>=chan && chan<2*NCHAN || (code&0177)>2)
		tiucherr(&tiu_dchan[chan], code);
	else {
		tiu.t_state = 0;
		tiu.t_actf = 0;
		wakeup(&tiu);
		for (cp = tiu_dchan; cp < &tiu_dchan[2*NCHAN]; cp++)
			tiucherr(cp, code);
	}
}

tiucherr(cp, code)
struct tiuch *cp;
{
	cp->t_flags =& ~(T_WRITR|T_RAVL);
	cp->t_flags =| T_ERROR|T_DONE;
	cp->t_troub = code;
	if (cp->t_buffer) {
		brelse(cp->t_buffer);
		cp->t_buffer = NULL;
	}
	wakeup(cp);
}

tiuchan(ip)
int *ip;
{
	register struct tiuch *cp;

	*ip = u.u_offset[1].hbyte;
	cp = &tiu_dchan[*ip];
	if (cp->t_flags&(T_ERROR|T_EOF)) {
		if (cp->t_flags&T_ERROR)
			u.u_error = EIO;
		return(NULL);
	}
	return(cp);
}

tiubusy()
{
	if (TIUADDR->drst&T_IDLE && TIUADDR->drdb&T_SLSTS)
		return(1);
	return(0);
}

tiustrategy(abp)
struct buf *abp;
{
	register struct buf *bp;

	bp = abp;
	bp->av_forw = NULL;
	if (tiu.t_actf==NULL) {
		tiu.t_actf = bp;
		tiustart();
	} else
		tiu.t_actl->av_forw = bp;
	tiu.t_actl = bp;
}

snstat()
{
	struct file *fp;
	struct tiu *cp;
	int op;

	op = fubyte(u.u_arg[1]);
	if ((fp=getf(u.u_ar0[R0]))==NULL)
		return;
	cp = &tiu_dchan[fp->f_offset[1].hbyte];
	switch (op) {

	/* get signal byte */
	case 0:
		cp->t_osig = fubyte(u.u_arg[0]);
		return;

	/* get signal byte */
	case 1:
		subyte(u.u_arg[0], cp->t_isig);
		cp->t_isig = 0;
		return;

	/* get channel # */
	case 2:
		op = fp->f_offset[1].hbyte;
		if (op>=NCHAN)
			op =+ 64-NCHAN;
		subyte(u.u_arg[0], op);
		return;

	/* get trouble code */
	case 3:
		subyte(u.u_arg[0], cp->t_troub);
		cp->t_troub = 0;
		cp->t_flags =& ~T_ERROR;
		return;

	/* clear EOF request */
	case 4:
		if (cp >= &tiu_dchan[NCHAN])
			cp =- NCHAN;
		cp->t_flags =& ~T_EOF;
		return;

	/* set EOF request */
	case 5:
		if (cp >= &tiu_dchan[NCHAN])
			cp =- NCHAN;
		cp->t_flags =| T_EOF;
		return;

	/* open control channel */
	case 6:
		if (cp < &tiu_dchan[NCHAN])
			cp =+ NCHAN;
		if (cp->t_flags&T_OPEN) {
			u.u_error = EINVAL;
			return;
		}
		if ((fp = falloc())==NULL)
			return;
		cp->t_flags = T_OPEN;
		return;
	}
	u.u_error = EINVAL;
}
