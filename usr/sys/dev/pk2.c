#define	KERNEL	1
#include "../h/pk.p"

/*
 * input framing and block checking.
 * frame layout for most devices is:
 *	
 *	S|K|X|Y|C|Z|  ... data ... |
 *
 *	where 	S	== initial synch byte
 *		K	== encoded frame size (indexes pksizes[])
 *		X, Y	== block check bytes
 *		C	== control byte
 *		Z	== XOR of header (K^X^Y^C)
 *		data	== 0 or more data bytes
 *
 * device driver interfaces on input are:
 *	pkrint  - byte level
 *	pkrend  - dma or pseudo-dma transfer level
 *	pkdata - packet level
 */

int pksizes[] ={
	1, 32, 64, 128, 256, 512, 1024, 2048, 4096, 1
};

/*
 * Pseudo-dma byte collection.
 * This code can be put in the device driver
 * interrupt routine to eliminate the per byte
 * subroutine call.
 */
pkrint(c, tp)
register c;
register struct tty *tp;
{

	if (q1.c_cc<0) {
		if (q1.c_cf != NULL) {
			tp->t_erase = 0;
			*q1.c_cf++ = c;
		}
		if (++q1.c_cc)
			return;
		pkrend(tp);
		return;
	}
}



/*
 * End of input transfer.
 */
pkrend(tp)
register struct tty *tp;
{
register char *p;
struct pack *pk;
struct header *h;
register x;
char	cntl, hdcheck;
unsigned short sum;
int i,j,k;
char **bp;

	p = q1.c_cl;
	x = (int)q1.c_cf - (int)p;
	pk = (struct pack *)tp->t_linep;
	h = (struct header * )&pk->p_ihbuf;
	if (x == HDRSIZ) {
		if (*p++ == SYN ) {
			hdcheck = k = *p++;
			sum = (unsigned)*p;
			hdcheck ^= *p++;
			sum |= (unsigned)*p << 8;
			hdcheck ^= *p++;
			hdcheck ^= cntl = *p++;
			if (hdcheck != *p) {
				goto bad;
			}
			if (k == 9) {
				pkcntl(cntl, pk);
				q1.c_cf = q1.c_cl;
				q1.c_cc = -HDRSIZ;
				goto istart1;
			}
			if (k && pksizes[k]==pk->p_rsize) {
				pk->p_rpr = cntl&MOD8;
				pksack(pk);
				bp = pk->p_ipool;
				if (bp) {
					pk->p_ipool = (char **)*bp;
					pk->p_io = bp;
				} else {
				}
				q1.c_cf = (char *)bp;
				q1.c_cc = -pk->p_rsize;
				h->sum = sum;
				h->cntl = cntl;
				goto istart1;
			}
bad:
			pkbadframe(pk);
		}
scan:
		x = HDRSIZ;
		j = 0;
		p = (caddr_t)h;
		for (i = 1; i < HDRSIZ; i++)
			if (p[i] == SYN)
				for(x=i; i<HDRSIZ; i++,j++)
					p[j] = p[i];

		q1.c_cc = -x;
		q1.c_cf = (caddr_t)((int)p + j);
		goto istart2;
	}
	if (x == pk->p_rsize) {
		pkdata(h->cntl, h->sum, pk, q1.c_cl);
		pk->p_io = NULL;
		q1.c_cf = (char *)h;
		q1.c_cc = -HDRSIZ;
		goto istart1;
	}
	if (x == 0) {
		q1.c_cf = (char *)h;
		q1.c_cc = -HDRSIZ;
		pkbadframe(pk);
	} else {
		pkbadframe(pk);
		goto scan;
	}
istart1:
	q1.c_cl = q1.c_cf;
istart2:
	if (tp->t_iproc != NULL)
		(*tp->t_iproc)(tp);
}



/*
 * Put packet located at address bp
 * in an input slot for further processing.
 */
pkdata(c, sum, pk, cp)
char c;
unsigned short sum;
register struct pack *pk;
char *cp;
{
register struct tty *tp;
register x;
char **bp;
int t;

	pk->p_state &= ~BADFRAME;
	bp = (char **)cp;
	tp = pk->p_ttyp;
	if (pk->p_state&DRAINO || !(pk->p_state&LIVE)) {
		pk->p_msg |= pk->p_rmsg;
		pkoutput(pk);
		goto drop;
	}
	t = next[pk->p_pr];
	for(x=pk->p_pr; x!=t; x = (x-1)&7) {
		if (pk->p_is[x] == 0)
			goto slot;
	}
	/*
	 * this can't happen
	 */
	printf("no slot\n");
drop:
	*bp = (char *)pk->p_ipool;
	pk->p_ipool = bp;
	return;

slot:
	pk->p_imap |= mask[x];
	pk->p_is[x] = c;
	pk->p_isum[x] = sum;
	pk->p_ib[x] = cp;
	if (tp->t_chan)
		sdata(tp->t_chan); else
		wakeup(&pk->p_pr);
}


/*
 * Start transmission on output device associated with pk.
 * For asynch devices (t_line==1) framing is
 * imposed.  For devices with framing and crc
 * in the driver (t_line==2) the transfer is
 * passed on to the driver.
 */
pkxstart(pk, cntl, x)
struct pack *pk;
char cntl;
register x;
{
struct tty *tp;
register char *p;
short checkword;
char hdcheck;

	p = (caddr_t)&pk->p_ohbuf;
	tp = pk->p_ttyp;

	if (tp->t_line==1) {
		*p++ = SYN;
		if (x < 0) {
			*p = 9;
			checkword = cntl;
			q3.c_cl = NULL;
		} else {
			*p = pk->p_lpsize;
			checkword = pk->p_osum[x] ^ (unsigned)cntl;
			q3.c_cl = pk->p_ob[x];
		}
		checkword = CHECK - checkword;
		hdcheck = *p++;
		hdcheck ^= *p++ = checkword;
		hdcheck ^= *p++ = checkword>>8;
		q3.c_cc = -HDRSIZ;
	} else {
		q3.c_cc = -1;
	}

	hdcheck ^= *p++ = cntl;
	*p = hdcheck;
	q3.c_cf = (caddr_t)&pk->p_ohbuf;
/*
	pk->p_srxmit++;
*/
	(*tp->t_oproc)(tp);
}

/*
 * transmitter interrupt.
 */
int	pkdelay	= 2;

pkxint(tp)
register struct tty *tp;
{
register struct pack *pk;
register s;
extern int pkoutput();

	pk = (struct pack *)tp->t_linep;
	s = spl6();
	tp->t_state &= ~BUSY;
	if (q3.c_cl == NULL) {
			pkoutput(pk);
	} else {
		q3.c_cf = q3.c_cl;
		q3.c_cl = NULL;
		q3.c_cc = -pk->p_xsize;
		(*tp->t_oproc)(tp);
	}
	splx(s);
}

