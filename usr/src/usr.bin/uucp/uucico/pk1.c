#ifndef lint
static char sccsid[] = "@(#)pk1.c	5.10 (Berkeley) %G%";
#endif

#include <signal.h>
#include "uucp.h"
#include "pk.h"
#include <setjmp.h>
#ifdef BSD4_2
#include <sys/time.h>
#endif BSD4_2

#ifdef VMS
#include <eunice/eunice.h>
#include <vms/iodef.h>
#include <vms/ssdef.h>
int iomask[2];
#endif VMS

#define PKMAXSTMSG 40
#define	MAXPKTIME 32	/* was 16 */
#define CONNODATA 10
#define MAXTIMEOUT 32

extern int errno;
extern int Retries;
extern char *sys_errlist[];
extern jmp_buf Sjbuf;
extern	char *malloc();

int Connodata = 0;
int Ntimeout = 0;
int pktimeout = 4;
int pktimeskew = 2;
/*
 * packet driver support routines
 *
 */

extern struct pack *pklines[];

/*
 * start initial synchronization.
 */

struct pack *
pkopen(ifn, ofn)
int ifn, ofn;
{
	register struct pack *pk;
	register char **bp;
	register int i;

	if ((pk = (struct pack *) malloc(sizeof (struct pack))) == NULL)
		return NULL;
	bzero((caddr_t) pk, sizeof (struct pack));
	pk->p_ifn = ifn;
	pk->p_ofn = ofn;
	pk->p_xsize = pk->p_rsize = PACKSIZE;
	pk->p_rwindow = pk->p_swindow = WINDOWS;
	/*  allocate input windows */
	for (i = 0; i < pk->p_rwindow; i++) {
		if ((bp = (char **) malloc((unsigned)pk->p_xsize)) == NULL)
			break;
		*bp = (char *) pk->p_ipool;
		pk->p_ipool = bp;
	}
	if (i == 0) {
		DEBUG(1, "pkopen: can't malloc i = 0\n", CNULL);
		return NULL;
	}
	pk->p_rwindow = i;

	/* start synchronization */
	pk->p_msg = pk->p_rmsg = M_INITA;
	for (i = 0; i < NPLINES; i++) {
		if (pklines[i] == NULL) {
			pklines[i] = pk;
			break;
		}
	}
	if (i >= NPLINES) {
		DEBUG(1,"pkopen: i>=NPLINES\n", CNULL);
		return NULL;
	}
	pkoutput(pk);

	for (i = 0; i < PKMAXSTMSG; i++) {
		pkgetpack(pk);
		if ((pk->p_state & LIVE) != 0)
			break;
	}
	if (i >= PKMAXSTMSG) {
		DEBUG(1, "pkopen: i>= PKMAXSTMSG\n", CNULL);
		return NULL;
	}

	pkreset(pk);
	return pk;
}


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
 */

int pksizes[] = {
	1, 32, 64, 128, 256, 512, 1024, 2048, 4096, 1
};

#define GETRIES 10
/*
 * Pseudo-dma byte collection.
 */

pkgetpack(pk)
register struct pack *pk;
{
	int k, tries, noise;
	register char *p;
	register struct header *h;
	unsigned short sum;
	int ifn;
	char **bp;
	char hdchk;

	if ((pk->p_state & DOWN) || Connodata > CONNODATA || Ntimeout > MAXTIMEOUT)
		pkfail();
	ifn = pk->p_ifn;

	/* find HEADER */
	for (tries = 0, noise = 0; tries < GETRIES; ) {
		p = (caddr_t) &pk->p_ihbuf;
		if (pkcget(ifn, p, 1) == SUCCESS) {
			if (*p++ == SYN) {
				if (pkcget(ifn, p, HDRSIZ-1) == SUCCESS)
					break;
			} else {
				if (noise++ < 10 || noise < (3*pk->p_rsize))
					continue;
			}
			DEBUG(4, "Noisy line - set up RXMIT\n", CNULL);
			noise = 0;
		}
		/* set up retransmit or REJ */
		tries++;
		Retries++;
		pk->p_msg |= pk->p_rmsg;
		if (pk->p_msg == 0)
			pk->p_msg |= M_RR;
		if ((pk->p_state & LIVE) == LIVE)
			pk->p_state |= RXMIT;
		pkoutput(pk);
	}
	if (tries >= GETRIES) {
		DEBUG(4, "tries = %d\n", tries);
		pkfail();
	}

	Connodata++;
	h = (struct header *) &pk->p_ihbuf;
	p = (caddr_t) h;
	hdchk = p[1] ^ p[2] ^ p[3] ^ p[4];
	p += 2;
	sum = (unsigned) *p++ & 0377;
	sum |= (unsigned) *p << 8;
	h->sum = sum;
	DEBUG(7, "rec h->cntl 0%o\n", h->cntl&0xff);
	k = h->ksize;
	if (hdchk != h->ccntl) {
		/* bad header */
		DEBUG(7, "bad header 0%o,", hdchk&0xff);
		DEBUG(7, "h->ccntl 0%o\n", h->ccntl&0xff);
		return;
	}
	if (k == 9) {
		if (((h->sum + h->cntl) & 0xffff) == CHECK) {
			pkcntl(h->cntl, pk);
			DEBUG(7, "state - 0%o\n", pk->p_state);
		} else {
			/*  bad header */
			pk->p_state |= BADFRAME;
			DEBUG(7, "bad header (k==9) 0%o\n", h->cntl&0xff);
		}
		return;
	}
	if (k && pksizes[k] == pk->p_rsize) {
		pk->p_rpr = h->cntl & MOD8;
		DEBUG(7, "end pksack 0%o\n", pk->p_rpr);
		pksack(pk);
		bp = pk->p_ipool;
		if (bp == NULL) {
			DEBUG(7, "bp NULL %s\n", "");
			return;
		}
		pk->p_ipool = (char **) *bp;
		Connodata = 0;
	} else
		return;

	if (pkcget(pk->p_ifn, (char *) bp, pk->p_rsize) == SUCCESS) {
		pkdata(h->cntl, h->sum, pk, (char **) bp);
	} else {
		*bp = (char *)pk->p_ipool;
		pk->p_ipool = bp;
	}
}

pkdata(c, sum, pk, bp)
char c;
unsigned short sum;
register struct pack *pk;
char **bp;
{
	register x;
	int t;
	char m;

	if (pk->p_state & DRAINO || !(pk->p_state & LIVE)) {
		pk->p_msg |= pk->p_rmsg;
		pkoutput(pk);
		goto drop;
	}
	t = next[pk->p_pr];
	for(x=pk->p_pr; x!=t; x = (x-1)&7) {
		if (pk->p_is[x] == 0)
			goto slot;
	}
drop:
	*bp = (char *)pk->p_ipool;
	pk->p_ipool = bp;
	return;

slot:
	m = mask[x];
	pk->p_imap |= m;
	pk->p_is[x] = c;
	pk->p_isum[x] = sum;
	pk->p_ib[x] = (char *)bp;
}

/*
 * setup input transfers
 */
#define PKMAXBUF 128
/*
 * Start transmission on output device associated with pk.
 * For asynch devices (t_line==1) framing is
 * imposed.  For devices with framing and crc
 * in the driver (t_line==2) the transfer is
 * passed on to the driver.
 */
pkxstart(pk, cntl, x)
register struct pack *pk;
char cntl;
register x;
{
	register char *p;
	short checkword;
	char hdchk;

	p = (caddr_t) &pk->p_ohbuf;
	*p++ = SYN;
	if (x < 0) {
		*p++ = hdchk = 9;
		checkword = cntl;
	} else {
		*p++ = hdchk = pk->p_lpsize;
		checkword = pk->p_osum[x] ^ (unsigned)(cntl & 0377);
	}
	checkword = CHECK - checkword;
	*p = checkword;
	hdchk ^= *p++;
	*p = checkword>>8;
	hdchk ^= *p++;
	*p = cntl;
	hdchk ^= *p++;
	*p = hdchk;
	/*  writes  */
	DEBUG(7, "send 0%o\n", cntl&0xff);
	p = (caddr_t) & pk->p_ohbuf;
	if (x < 0) {
		if(write(pk->p_ofn, p, HDRSIZ) != HDRSIZ) {
			alarm(0);
			logent("PKXSTART write failed", sys_errlist[errno]);
			longjmp(Sjbuf, 4);
		}
	} else {
		char buf[PKMAXBUF + HDRSIZ + TAILSIZE], *b;
		int i;
		for (i = 0, b = buf; i < HDRSIZ; i++)
			*b++ = *p++;
		for (i = 0, p = pk->p_ob[x]; i < pk->p_xsize; i++)
			*b++ = *p++;
#if TAILSIZE != 0
		for (i = 0; i < TAILSIZE; i++)
			*b++ = '\0';
#endif TAILSIZE
		if (write(pk->p_ofn, buf, pk->p_xsize + HDRSIZ + TAILSIZE)
		    != (HDRSIZ + TAILSIZE + pk->p_xsize)) {
			alarm(0);
			logent("PKXSTART write failed", sys_errlist[errno]);
			longjmp(Sjbuf, 5);
		}
		Connodata = 0;
	}
	if (pk->p_msg)
		pkoutput(pk);
}


pkmove(p1, p2, count, flag)
char *p1, *p2;
int count, flag;
{
	register char *s, *d;
	register int i;

	if (flag == B_WRITE) {
		s = p2;
		d = p1;
	} else {
		s = p1;
		d = p2;
	}
	for (i = 0; i < count; i++)
		*d++ = *s++;
}


/*
 *	get n characters from input
 *
 *	return codes:
 *		n - number of characters returned
 *		0 - end of file
 */

jmp_buf Getjbuf;
cgalarm()
{
	longjmp(Getjbuf, 1);
}

pkcget(fn, b, n)
int fn;
register char *b;
register int n;
{
	register int ret;
	extern int linebaudrate;
#ifdef BSD4_2
	long r, itime = 100000L; /* guess it's been 1/10th second since we
				    last read the line */
	struct timeval tv;
#endif BSD4_2
#ifdef VMS
	short iosb[4];
	int SYS$QioW();	/* use this for long reads on vms */
#endif VMS

	if (setjmp(Getjbuf)) {
		Ntimeout++;
		DEBUG(4, "pkcget: alarm %d\n", pktimeout * 1000 + Ntimeout);
		pktimeout += pktimeskew;
		if (pktimeout > MAXPKTIME)
			pktimeout = MAXPKTIME;
		return FAIL;
	}
	signal(SIGALRM, cgalarm);

	alarm(pktimeout);
	while (n > 0) {
#ifdef BSD4_2
		if (linebaudrate > 0) {
			r = n  * 100000L;
			r = r / linebaudrate;
			r = (r * 100) - itime;
			itime = 0;
			/* we predict that more than 1/50th of a
			   second will go by before the read will
			   give back all that we want. */
			if (r > 20000) {
				tv.tv_sec = r / 1000000L;
				tv.tv_usec = r % 1000000L;
				DEBUG(11, "PKCGET stall for %d", tv.tv_sec);
				DEBUG(11, ".%06d sec\n", tv.tv_usec);
				(void) select (0, (int *)0, (int *)0, (int *)0, &tv);
			}
		}
#endif BSD4_2
#ifndef VMS
		ret = read(fn, b, n);
#else VMS
		_$Cancel_IO_On_Signal = FD_FAB_Pointer[fn];
		ret = SYS$QioW(_$EFN,(FD_FAB_Pointer[fn]->fab).fab$l_stv,
				IO$_READVBLK|IO$M_NOFILTR|IO$M_NOECHO,
				iosb,0,0,b,n,0,
				iomask,0,0);
		_$Cancel_IO_On_Signal = 0;
		if (ret == SS$_NORMAL)
			ret = iosb[1]+iosb[3];   /* get length of transfer */
		else
			ret = 0;
#endif VMS
		if (ret == 0) {
			alarm(0);
			return FAIL;
		}
		if (ret <= 0) {
			alarm(0);
			logent(sys_errlist[errno],"FAILED pkcget Read");
			longjmp(Sjbuf, 6);
		}
 		b += ret;
		n -= ret;
	}
	alarm(0);
	return SUCCESS;
}
