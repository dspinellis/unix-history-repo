#ifndef lint
static char sccsid[] = "@(#)pk0.c	5.9	(Berkeley) 4/5/88";
#endif

#include "uucp.h"
#include "pk.h"

/*
 * packet driver
 */

char next[8] = { 1, 2, 3, 4, 5, 6, 7, 0};	/* packet sequence numbers */
char mask[8] = { 1, 2, 4, 010, 020, 040, 0100, 0200 };

struct pack *pklines[NPLINES];

int Reacks;

#define PKRTIME 4
#define PKWTIME 4
#define PKRSKEW 3
#define PKWSKEW 2
extern int pktimeout, pktimeskew, Ntimeout;

/*
 * receive control messages
 */
pkcntl(c, pk)
register struct pack *pk;
{
	register cntl, val;

	val = c & MOD8;
	cntl = (c>>3) & MOD8;

	if (!ISCNTL(c)) {
		logent("PK0", "not cntl");
		return;
	}

	switch(cntl) {
	case INITB:
		val++;
		pk->p_xsize = pksizes[val];
		pk->p_lpsize = val;
		pk->p_bits = 1;
		if (pk->p_state & LIVE) {
			pk->p_msg |= M_INITC;
			break;
		}
		pk->p_state |= INITb;
		if ((pk->p_state & INITa)==0) {
			break;
		}
		pk->p_rmsg &= ~M_INITA;
		pk->p_msg |= M_INITC;
		break;

	case INITC:
		if ((pk->p_state&INITab)==INITab) {
			pk->p_state = LIVE;
			pk->p_rmsg &= ~M_INITB;
		} else
			pk->p_msg |= M_INITB;
		if (val)
			pk->p_swindow = val;
		break;
	case INITA:
		if (val == 0 && pk->p_state&LIVE) {
			logent("PK0", "alloc change not implemented");
			break;
		}
		if (val) {
			pk->p_state |= INITa;
			pk->p_msg |= M_INITB;
			pk->p_rmsg |= M_INITB;
			pk->p_swindow = val;
		}
		break;
	case RJ:
		pk->p_state |= RXMIT;
		pk->p_msg |= M_RR;
		pk->p_rpr = val;
		(void) pksack(pk);
		break;
	case RR:
		pk->p_rpr = val;
		if (pk->p_rpr == pk->p_ps) {
			DEBUG(9, "Reack count is %d\n", ++Reacks);
			if (Reacks >= 4) {
				DEBUG(6, "Reack overflow on %d\n", val);
				pk->p_state |= RXMIT;
				pk->p_msg |= M_RR;
				Reacks = 0;
			}
		} else {
			Reacks = 0;
			(void) pksack(pk);
		}
		break;
	case SRJ:
		logent("PK0", "srj not implemented");
		break;
	case CLOSE:
		pk->p_state = DOWN+RCLOSE;
		return;
	}
	if (pk->p_msg)
		pkoutput(pk);
}

pkaccept(pk)
register struct pack *pk;
{
	register x, seq;
	char m, cntl, *p, imask, **bp;
	int bad, accept, skip, t,  cc;
	unsigned short sum;

	bad = accept = skip = 0;
	/*
	 * wait for input
	 */
	x = next[pk->p_pr];
	while ((imask=pk->p_imap) == 0 && pk->p_rcount == 0) {
		pkgetpack(pk);
	}
	pk->p_imap = 0;

	/*
	 * determine input window in m.
	 */
	t = (~(-1<<(int)(pk->p_rwindow))) <<x;
	m = t;
	m |= t>>8;

	/*
	 * mark newly accepted input buffers
	 */
	for(x=0; x<8; x++) {
		if ((imask & mask[x]) == 0)
			continue;

		if (((cntl=pk->p_is[x])&0200) == 0) {
			bad++;
free:
			bp = (char **)pk->p_ib[x];
			*bp = (char *)pk->p_ipool;
			pk->p_ipool = bp;
			pk->p_is[x] = 0;
			continue;
		}

		pk->p_is[x] = ~(B_COPY+B_MARK);
		sum = (unsigned)chksum(pk->p_ib[x], pk->p_rsize) ^ (unsigned)(cntl&0377);
		sum += pk->p_isum[x];
		if (sum == CHECK) {
			seq = (cntl>>3) & MOD8;
			if (m & mask[seq]) {
				if (pk->p_is[seq] & (B_COPY | B_MARK)) {
				dup:
					pk->p_msg |= M_RR;
					skip++;
					goto free;
				}
				if (x != seq) {
					p = pk->p_ib[x];
					pk->p_ib[x] = pk->p_ib[seq];
					pk->p_is[x] = pk->p_is[seq];
					pk->p_ib[seq] = p;
				}
				pk->p_is[seq] = B_MARK;
				accept++;
				cc = 0;
				if (cntl&B_SHORT) {
					pk->p_is[seq] = B_MARK+B_SHORT;
					p = pk->p_ib[seq];
					cc = (unsigned)*p++ & 0377;
					if (cc & 0200) {
						cc &= 0177;
						cc |= *p << 7;
					}
				}
				pk->p_isum[seq] = pk->p_rsize - cc;
			} else {
				goto dup;
			}
		} else {
			bad++;
			goto free;
		}
	}

	/*
	 * scan window again turning marked buffers into
	 * COPY buffers and looking for missing sequence
	 * numbers.
	 */
	accept = 0;
	t = -1;
	for(x=next[pk->p_pr]; m & mask[x]; x = next[x]) {
		if (pk->p_is[x] & B_MARK)
			pk->p_is[x] |= B_COPY;

		if (pk->p_is[x] & B_COPY) {
			if (t >= 0) {
				bp = (char **)pk->p_ib[x];
				*bp = (char *)pk->p_ipool;
				pk->p_ipool = bp;
				pk->p_is[x] = 0;
				skip++;
			} else
				accept++;
		} else {
			if (t<0)
				t = x;
		}
	}

	if (bad) {
		pk->p_msg |= M_RJ;
	}

	if (skip) {
		pk->p_msg |= M_RR;
	}

	pk->p_rcount = accept;
	return accept;
}

/*ARGSUSED*/
pkread(pk, ibuf, icount)
register struct pack *pk;
char *ibuf;
int icount;
{
	register x;
	int is, cc, xfr, count;
	char *cp, **bp;

	xfr = 0;
	count = 0;
	pktimeout = PKRTIME;
	pktimeskew = PKRSKEW;
	Ntimeout = 0;
	while (pkaccept(pk) == 0)
		;

	while (icount) {
		x = next[pk->p_pr];
		is = pk->p_is[x];

		if (is & B_COPY) {
			cc = MIN(pk->p_isum[x], icount);
			if (cc==0 && xfr) {
				break;
			}
			if (is & B_RESID)
				cp = pk->p_rptr;
			else {
				cp = pk->p_ib[x];
				if (is & B_SHORT) {
					if (*cp++ & 0200)
						cp++;
				}
			}
			bcopy(cp, ibuf, cc);
			ibuf += cc;
			icount -= cc;
			count += cc;
			xfr++;
			pk->p_isum[x] -= cc;
			if (pk->p_isum[x] == 0) {
				pk->p_pr = x;
				bp = (char **)pk->p_ib[x];
				*bp = (char *)pk->p_ipool;
				pk->p_ipool = bp;
				pk->p_is[x] = 0;
				pk->p_rcount--;
				pk->p_msg |= M_RR;
			} else {
				pk->p_rptr = cp+cc;
				pk->p_is[x] |= B_RESID;
			}
			if (cc==0)
				break;
		} else
			break;
	}
	pkoutput(pk);
	return count;
}

/*ARGSUSED*/
pkwrite(pk, ibuf, icount)
register struct pack *pk;
char *ibuf;
int icount;
{
	register x;
	int partial;
	caddr_t cp;
	int cc, fc, count;

	if (pk->p_state&DOWN || !pk->p_state&LIVE) {
		return -1;
	}

	pktimeout = PKWTIME;
	pktimeskew = PKWSKEW;
	Ntimeout = 0;
	count = icount;
	do {
		while (pk->p_xcount>=pk->p_swindow)  {
			pkoutput(pk);
			pkgetpack(pk);
		}
		x = next[pk->p_pscopy];
		while (pk->p_os[x]!=B_NULL)  {
			pkgetpack(pk);
		}
		pk->p_os[x] = B_MARK;
		pk->p_pscopy = x;
		pk->p_xcount++;

		cp = pk->p_ob[x] = malloc((unsigned)pk->p_xsize);
		partial = 0;
		if ((int)icount < pk->p_xsize) {
			cc = icount;
			fc = pk->p_xsize - cc;
			*cp = fc&0177;
			if (fc > 127) {
				*cp++ |= 0200;
				*cp++ = fc>>7;
			} else
				cp++;
			partial = B_SHORT;
		} else
			cc = pk->p_xsize;
		bcopy(ibuf, cp, cc);
		ibuf += cc;
		icount -= cc;
		pk->p_osum[x] = chksum(pk->p_ob[x], pk->p_xsize);
		pk->p_os[x] = B_READY+partial;
		pkoutput(pk);
	} while (icount);

	return count;
}

pksack(pk)
register struct pack *pk;
{
	register x, i;

	i = 0;
	for(x=pk->p_ps; x!=pk->p_rpr; ) {
		x = next[x];
		if (pk->p_os[x]&B_SENT) {
			i++;
			pk->p_os[x] = B_NULL;
			pk->p_state &= ~WAITO;
			pk->p_xcount--;
			free((char *)pk->p_ob[x]);
			pk->p_ps = x;
		}
	}
	return i;
}

pkoutput(pk)
register struct pack *pk;
{
	register x;
	int i;
	char bstate;

	if (pk->p_obusy++) {
		pk->p_obusy--;
		return;
	}

	/*
	 * find seq number and buffer state
	 * of next output packet
	 */
	if (pk->p_state&RXMIT)  {
		pk->p_nxtps = next[pk->p_rpr];
	}
	x = pk->p_nxtps;
	bstate = pk->p_os[x];

	/*
	 * Send control packet if indicated
	 */
	if (pk->p_msg) {
		if (pk->p_msg & ~M_RR || !(bstate&B_READY) ) {
			x = pk->p_msg;
			for(i=0; i<8; i++)
				if (x&1)
					break;
				else
					x >>= 1;
			x = i;
			x <<= 3;
			switch(i) {
			case CLOSE:
				break;
			case RJ:
			case RR:
				x += pk->p_pr;
				break;
			case SRJ:
				break;
			case INITB:
				x += pksize(pk->p_rsize);
				break;
			case INITC:
				x += pk->p_rwindow;
				break;
			case INITA:
				x += pk->p_rwindow;
				break;
			}

			pk->p_msg &= ~mask[i];
			pkxstart(pk, x, -1);
			goto out;
		}
	}


	/*
	 * Don't send data packets if line is marked dead.
	 */
	if (pk->p_state&DOWN) {
		goto out;
	}
	/*
	 * Start transmission (or retransmission) of data packets.
	 */
	if (bstate & (B_READY|B_SENT)) {
		char seq;

		bstate |= B_SENT;
		seq = x;
		pk->p_nxtps = next[x];

		x = 0200+pk->p_pr+(seq<<3);
		if (bstate & B_SHORT)
			x |= 0100;
		pkxstart(pk, x, seq);
		pk->p_os[seq] = bstate;
		pk->p_state &= ~RXMIT;
		goto out;
	}
	/*
	 * enable timeout if there's nothing to send
	 * and transmission buffers are languishing
	 */
	if (pk->p_xcount) {
		pk->p_state |= WAITO;
	} else
		pk->p_state &= ~WAITO;
out:
	pk->p_obusy = 0;
}

/*
 * shut down line by
 *	ignoring new input
 *	letting output drain
 *	releasing space and turning off line discipline
 */
/*ARGSUSED*/
pkclose(pk)
register struct pack *pk;
{
	register i;
	char **bp;
	int rcheck = 0;

	pk->p_state |= DRAINO;

	/*
	 * try to flush output
	 */
	i = 0;
	while (pk->p_xcount && pk->p_state&LIVE) {
		if (pk->p_state&(RCLOSE+DOWN) || ++i > 2)
			break;
		pkoutput(pk);
	}
	pk->p_state |= DOWN;

	/*
	 * try to exchange CLOSE messages
	 */
	i = 0;
	while ((pk->p_state&RCLOSE)==0 && i<2) {
		pk->p_msg = M_CLOSE;
		pkoutput(pk);
		i++;
	}

	for(i=0;i<NPLINES;i++)
		if (pklines[i]==pk)  {
			pklines[i] = NULL;
		}

	/*
	 * free space
	 */
	rcheck = 0;
	for (i=0;i<8;i++) {
		if (pk->p_os[i] != B_NULL) {
			free((char *)pk->p_ob[i]);
			pk->p_xcount--;
		}
		if (pk->p_is[i] != B_NULL)  {
			free((char *)pk->p_ib[i]);
			rcheck++;
		}
	}
	while (pk->p_ipool != NULL) {
		bp = pk->p_ipool;
		pk->p_ipool = (char **)*bp;
		rcheck++;
		free((char *)bp);
	}
	if (rcheck != pk->p_rwindow) {
		syslog(LOG_WARNING, "%s: pk0: rc %d rw %d", Rmtname, rcheck,
			pk->p_rwindow);
	}
	free((char *)pk);
}

pkreset(pk)
register struct pack *pk;
{

	pk->p_ps = pk->p_pr =  pk->p_rpr = 0;
	pk->p_nxtps = 1;
}

#ifndef BSD4_2
bzero(s,n)
register char *s;
register n;
{
	while (n--)
		*s++ = 0;
}
#endif !BSD4_2

pksize(n)
register n;
{
	register k;

	n >>= 5;
	for(k=0; n >>= 1; k++)
		;
	return k;
}
