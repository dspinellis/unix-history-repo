#define	KERNEL	1
#include "../h/pk.p"

/*
 * packet driver
 */

char next[8]	={ 1,2,3,4,5,6,7,0};	/* packet sequence numbers */
char mask[8]	={ 1,2,4,010,020,040,0100,0200 };

struct pack *pklines[NPLINES];


/*
 * receive control messages
 */

pkcntl(cntl, pk)
register cntl;
register struct pack *pk;
{
register val;
register m;

	val = cntl & MOD8;

	if ( NOTCNTL(cntl) ) {
		pkbadframe(pk);
		return;
	}

	cntl >>= 3;
	pk->p_state &= ~BADFRAME;
	m = pk->p_msg;

	switch(cntl) {

	case INITB:
		val++;
		pk->p_xsize = pksizes[val];
		pk->p_lpsize = val;
		pk->p_bits = DTOM(pk->p_xsize);
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
			WAKEUP(&pk->p_state);
			pk->p_rmsg &= ~(M_INITA|M_INITB);
		} else
			if ((pk->p_state&LIVE)==0)
				pk->p_msg |= M_INITB;
		if (val)
			pk->p_swindow = val;
		break;
	case INITA:
		if (val==0 && pk->p_state&LIVE) {
			printf("alloc change\n");
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
	case RR:
		pk->p_rpr = val;
		if (pksack(pk)==0) {
			WAKEUP(&pk->p_ps);
		}
		break;
	case CLOSE:
		pk->p_state = DOWN+RCLOSE;
		pk->p_rmsg = 0;
		WAKEUP(&pk->p_pr);
		WAKEUP(&pk->p_ps);
		WAKEUP(&pk->p_state);
		return;
	}
	if (pk->p_msg==0)
		pk->p_msg |= pk->p_rmsg;


	if (m==0 && pk->p_msg)
		pkoutput(pk);
}


/*
 * Send RJ message on first framing error.
 * Cleared by receiving a good frame 
 * (pkcntl or pkdata).
 */
pkbadframe(pk)
register struct pack *pk;
{
	WAKEUP(&pk->p_pr);
	if (pk->p_state & BADFRAME)
		return;
	pk->p_state |= BADFRAME;
	pk->p_timer = 2;
}




/*
 * Look at sequence numbers (mostly).
 */
pkaccept(pk)
register struct pack *pk;
{
register x, seq;
char m, cntl, *p, imask, **bp;
int bad, accept, skip, s, t, cc;
unsigned short sum;

	bad = accept = skip = 0;
	/*
	 * wait for input
	 */
	LOCK;
	x = next[pk->p_pr];
	if((imask=pk->p_imap)==0 && pk->p_rcount==0) {
		UNLOCK;
		goto out;
	}
	pk->p_imap = 0;
	UNLOCK;


	/*
	 * determine input window in m.
	 */
	t = (~(-1<<pk->p_rwindow)) <<x;
	m = t;
	m |= t>>8;


	/*
	 * mark newly accepted input buffers
	 */
	for(x=0; x<8; x++) {

		if ((imask & mask[x]) == 0)
			continue;

		if (((cntl=pk->p_is[x])&0200)==0) {
			bad++;
free:
			bp = (char **)pk->p_ib[x];
			LOCK;
			*bp = (char *)pk->p_ipool;
			pk->p_ipool = bp;
			pk->p_is[x] = 0;
			UNLOCK;
			continue;
		}

		pk->p_is[x] = ~(B_COPY+B_MARK);
		sum = (unsigned)chksum(pk->p_ib[x], pk->p_rsize) ^ (unsigned)cntl;
		sum += pk->p_isum[x];
		if (sum == CHECK) {
			seq = (cntl>>3) & MOD8;
			if (m & mask[seq]) {
				if (pk->p_is[seq] & (B_COPY | B_MARK)) {
				dup:
					skip++;
					goto free;
				}
				if (x != seq) {
					LOCK;
					p = pk->p_ib[x];
					pk->p_ib[x] = pk->p_ib[seq];
					pk->p_is[x] = pk->p_is[seq];
					pk->p_ib[seq] = p;
					UNLOCK;
				}
				pk->p_is[seq] = B_MARK;
				accept++;
				cc = 0;
				if (cntl&B_SHORT) {
					pk->p_is[seq] = B_MARK+B_SHORT;
					p = pk->p_ib[seq];
					cc = (unsigned)*p++;
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
	for(x=next[pk->p_pr],t= -1; m & mask[x]; x = next[x]) {
		if (pk->p_is[x] & B_MARK)
			pk->p_is[x] |= B_COPY;
		if (pk->p_is[x] & B_COPY) {
			if (t >= 0) {
				bp = (char **)pk->p_ib[x];
				LOCK;
				*bp = (char *)pk->p_ipool;
				pk->p_ipool = bp;
				pk->p_is[x] = 0;
				UNLOCK;
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
	} else 

	if (skip) {
		pk->p_msg |= M_RR;
	}

	pk->p_rcount = accept;
out:
	if (pk->p_msg)
		pkoutput(pk);
	return(accept);
}


pkread(S)
SDEF;
{
register struct pack *pk;
register x,s;
int is,cc,xfr,count;
char *cp, **bp;

	pk = PADDR;
	xfr = 0;
	count = -1;
	while (pkaccept(pk) == 0) {
		PKGETPKT(pk);
		if (pk->p_state&DOWN) {
			SETERROR;
			goto out;
		}
		if (SLEEPNO) {
			count++;
			goto out;
		}
		SLEEP(&pk->p_pr, PKIPRI);
	}
	count = 0;


	while (UCOUNT) {

		x = next[pk->p_pr];
		is = pk->p_is[x];

		if (is & B_COPY) {
			cc = MIN(pk->p_isum[x], UCOUNT);
			if (cc==0 && xfr) {
				break;
			}
			if (is & B_RESID)
				cp = pk->p_rptr;
			else {
				cp = pk->p_ib[x];
				if (is & B_SHORT) {
					if (*cp++ & 0200)
						*cp++;
				}
			}
			IOMOVE(cp,cc,B_READ);
			count += cc;
			xfr++;
			pk->p_isum[x] -= cc;
			if (pk->p_isum[x] == 0) {
				LOCK;
				pk->p_pr = x;
				bp = (char **)pk->p_ib[x];
				*bp = (char *)pk->p_ipool;
				pk->p_ipool = bp;
				pk->p_is[x] = 0;
				pk->p_rcount--;
				UNLOCK;
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
	if (SLEEPNO)
		count = pk->p_rcount;
out:
	return(count);
}




pkwrite(S)
SDEF;
{
register struct pack *pk;
register x;
int partial;
caddr_t cp;
int cc, s, fc, count;
int pktimeout();

	pk = PADDR;
	if ((pk->p_state&LIVE)==0) {
	down:
		SIGNAL;
		SETERROR;
		return(-1);
	}

	count = UCOUNT;
	do {
		LOCK;
		while (pk->p_xcount>=pk->p_swindow)  {
			pkoutput(pk);
			PKGETPKT(pk);
			SLEEP(&pk->p_ps,PKOPRI);
			if (pk->p_state&DOWN)
				goto down;
		}
		x = next[pk->p_pscopy];
		while (pk->p_os[x]!=B_NULL)  {
			goto down;
		}
		pk->p_os[x] = B_MARK;
		pk->p_pscopy = x;
		pk->p_xcount++;
		UNLOCK;

		cp = pk->p_ob[x] = (char *)GETEPACK;
		partial = 0;
		if ((int)UCOUNT < pk->p_xsize) {
			cc = UCOUNT;
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
		IOMOVE(cp,cc,B_WRITE);
		pk->p_osum[x] = chksum(pk->p_ob[x], pk->p_xsize);
		pk->p_os[x] = B_READY+partial;
		pkoutput(pk);
	} while (UCOUNT);

	return(count-UCOUNT);
}

pksack(pk)
register struct pack *pk;
{
register x, i;
int s;

	i = 0;
	LOCK;
	for(x=pk->p_ps; x!=pk->p_rpr; ) {
		x = next[x];
		if (pk->p_os[x]&B_SENT) {
			i++;
			FREEPACK(pk->p_ob[x], pk->p_bits);
			pk->p_os[x] = B_NULL;
			pk->p_state &= ~WAITO;
			pk->p_xcount--;
			pk->p_ps = x;
			WAKEUP(&pk->p_ps);
		}
	}
	UNLOCK;
	return(i);
}



pkoutput(pk)
register struct pack *pk;
{
register x;
int s;
char bstate;
int i;
SDEF;
extern pkzot;

	ISYSTEM;
	LOCK;
	if (OBUSY) {
		UNLOCK;
		return;
	}


	/*
	 * find seq number and buffer state
	 * of next output packet
	 */
	if (pk->p_state&RXMIT)  {
		pk->p_nxtps = next[pk->p_rpr];
		pk->p_state &= ~RXMIT;
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
					break; else
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
	if (pk->p_state&DOWN || (pk->p_state&LIVE)==0) {
		WAKEUP(&pk->p_ps);
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
		if (pk->p_os[seq])
			pk->p_os[seq] = bstate;
		pk->p_nout++;
		goto out;
	}
	/*
	 * enable timeout if there's nothing to send
	 * and transmission buffers are languishing
	 */
	if (pk->p_xcount) {
		pk->p_timer = 10+pkzot;
		pk->p_state |= WAITO;
	} else
		pk->p_state &= ~WAITO;
	WAKEUP(&pk->p_ps);
out:
	pk->p_obusy = 0;
	UNLOCK;
}


/*
 * shut down line by
 *	ignoring new input
 *	letting output drain
 *	releasing space and turning off line discipline
 */
pkclose(S)
SDEF;
{
register struct pack *pk;
register i,s,rbits;
char **bp;

#define	NTRIES	1

	pk = PADDR;
	pk->p_state |= DRAINO;


	/*
	 * try to flush output
	 */
	i = 0;
	LOCK;
	if (pklive(pk)==0) {
		pk->p_state = DOWN;
		UNLOCK;
		goto final;
	}
	pk->p_timer = 2;
	while (pk->p_xcount && pk->p_state&LIVE) {
		if (pk->p_state&(RCLOSE+DOWN) || ++i > NTRIES)
			break;
		pkoutput(pk);
		SLEEP(&pk->p_ps,PKOPRI);
	}
	pk->p_timer = 0;
	pk->p_state |= DOWN;
	UNLOCK;


	/*
	 * try to exchange CLOSE messages
	 */
	i = 0;
	while ((pk->p_state&RCLOSE)==0 && i<NTRIES) {
		pk->p_msg = M_CLOSE;
		pk->p_timer = 2;
		pkoutput(pk);
		SLEEP(&pk->p_ps, PKOPRI);
		i++;
	}


final:
	TURNOFF;


	/*
	 * free space
	 */
	rbits = DTOM(pk->p_rsize);
	for (i=0;i<8;i++) {
		if (pk->p_os[i]!=B_NULL) {
			FREEPACK(pk->p_ob[i],pk->p_bits);
			pk->p_xcount--;
		}
		if (pk->p_is[i]!=B_NULL)  {
			FREEPACK(pk->p_ib[i],rbits);
		}
	}
	LOCK;
	while (pk->p_ipool != NULL) {
		bp = pk->p_ipool;
		pk->p_ipool = (char **)*bp;
		FREEPACK((caddr_t)bp, rbits);
	}
	UNLOCK;
	for(i=0;i<NPLINES;i++)
		if (pklines[i]==pk)  {
			pklines[i] = NULL;
		}
	FREEPACK((caddr_t)pk, npbits);
}



pkreset(pk)
register struct pack *pk;
{

	pk->p_ps = pk->p_pr =  pk->p_rpr = 0;
	pk->p_nxtps = 1;
}

chksum(s,n)
register char *s;
register n;
{
	register short sum;
	register unsigned t;
	register x;

	sum = -1;
	x = 0;

	do {
		if (sum<0) {
			sum <<= 1;
			sum++;
		} else
			sum <<= 1;
		t = sum;
		sum += (unsigned)*s++;
		x += sum^n;
		if ((unsigned)sum <= t) {
			sum ^= x;
		}
	} while (--n > 0);

	return(sum);
}

pkline(pk)
register struct pack *pk;
{
register i;
	for(i=0;i<NPLINES;i++) {
		if (pklines[i]==pk)
			return(i);
	}
	return(-i);
}

pkzero(s,n)
register char *s;
register n;
{
	while (n--)
		*s++ = 0;
}

pksize(n)
register n;
{
register k;

	n >>= 5;
	for(k=0; n >>= 1; k++);
	return(k);
}
