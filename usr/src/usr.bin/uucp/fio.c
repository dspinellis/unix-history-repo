#ifndef lint
static char sccsid[] = "@(#)fio.c	5.6	(Berkeley) 6/29/90";
#endif

/*
 * flow control protocol.
 *
 * This protocol relies on flow control of the data stream.
 * It is meant for working over links that can (almost) be
 * guaranteed to be errorfree, specifically X.25/PAD links.
 * A sumcheck is carried out over a whole file only. If a
 * transport fails the receiver can request retransmission(s).
 * This protocol uses a 7-bit datapath only, so it can be
 * used on links that are not 8-bit transparent.
 *
 * When using this protocol with an X.25 PAD:
 * Although this protocol uses no control chars except CR,
 * control chars NULL and ^P are used before this protocol
 * is started; since ^P is the default char for accessing
 * PAD X.28 command mode, be sure to disable that access
 * (PAD par 1). Also make sure both flow control pars
 * (5 and 12) are set. The CR used in this proto is meant
 * to trigger packet transmission, hence par 3 should be 
 * set to 2; a good value for the Idle Timer (par 4) is 10.
 * All other pars should be set to 0.
 *
 * Normally a calling site will take care of setting the
 * local PAD pars via an X.28 command and those of the remote
 * PAD via an X.29 command, unless the remote site has a
 * special channel assigned for this protocol with the proper
 * par settings.
 *
 * Additional comments for hosts with direct X.25 access:
 * - the global variable IsTcpIp, when set, excludes the ioctl's,
 *   so the same binary can run on X.25 and non-X.25 hosts;
 * - reads are done in small chunks, which can be smaller than
 *   the packet size; your X.25 driver must support that.
 *
 *
 * Author:
 *	Piet Beertema, CWI, Amsterdam, Sep 1984
 * Modified for X.25 hosts:
 *	Robert Elz, Melbourne Univ, Mar 1985
 */

#include "uucp.h"
#include <signal.h>
#ifdef USG
#include <termio.h>
#else !USG
#include <sgtty.h>
#endif !USG
#include <setjmp.h>

#define FIBUFSIZ	4096	/* for X.25 interfaces: set equal to packet size,
				 * but see comment above
				 */

#define FOBUFSIZ	4096	/* for X.25 interfaces: set equal to packet size;
				 * otherwise make as large as feasible to reduce
				 * number of write system calls 
				 */

#ifndef MAXMSGLEN
#define MAXMSGLEN	BUFSIZ
#endif MAXMSGLEN

static int fchksum;
static jmp_buf Ffailbuf;

extern long Bytes_Sent, Bytes_Received;

static
falarm()
{
	signal(SIGALRM, falarm);
	longjmp(Ffailbuf, 1);
}

static void (*fsig)();

#ifndef USG
#define TCGETA	TIOCGETP
#define TCSETAF	TIOCSETP
#define termio	sgttyb
#endif USG
static struct	termio ttbuf;

fturnon()
{
	int ttbuf_flags;

	if (!IsTcpIp) {
		ioctl(Ifn, TCGETA, &ttbuf);
#ifdef USG
		ttbuf_flags = ttbuf.c_iflag;
		ttbuf.c_iflag = IXOFF|IXON|ISTRIP;
		ttbuf.c_cc[VMIN] = FIBUFSIZ > 64 ? 64 : FIBUFSIZ;
		ttbuf.c_cc[VTIME] = 5;
		if (ioctl(Ifn, TCSETAF, &ttbuf) < 0) {
			syslog(LOG_ERR, "ioctl(TCSETAF) failed: %m");
			cleanup(FAIL);
		}
		ttbuf.c_iflag = ttbuf_flags;
#else !USG
		ttbuf_flags = ttbuf.sg_flags;
		ttbuf.sg_flags = ANYP|CBREAK;
		if (ioctl(Ifn, TCSETAF, &ttbuf) < 0) {
			syslog(LOG_ERR, "ioctl(TCSETAF) failed: %m");
			cleanup(FAIL);
		}
		/* this is two seperate ioctls to set the x.29 params */
		ttbuf.sg_flags |= TANDEM;
		if (ioctl(Ifn, TCSETAF, &ttbuf) < 0) {
			syslog(LOG_ERR, "ioctl(TCSETAF) failed: %m");
			cleanup(FAIL);
		}
		ttbuf.sg_flags = ttbuf_flags;
#endif USG
	}
	fsig = signal(SIGALRM, falarm);
	/* give the other side time to perform its ioctl;
	 * otherwise it may flush out the first data this
	 * side is about to send.
	 */
	sleep(2);
	return SUCCESS;
}

fturnoff()
{
	if (!IsTcpIp)
		ioctl(Ifn, TCSETAF, &ttbuf);
	(void) signal(SIGALRM, fsig);
	sleep(2);
	return SUCCESS;
}

fwrmsg(type, str, fn)
register char *str;
int fn;
char type;
{
	register char *s;
	char bufr[MAXMSGLEN];

	s = bufr;
	*s++ = type;
	while (*str)
		*s++ = *str++;
	if (*(s-1) == '\n')
		s--;
	*s++ = '\r';
	*s = 0;
	(void) write(fn, bufr, s - bufr);
	return SUCCESS;
}

frdmsg(str, fn)
register char *str;
register int fn;
{
	register char *smax;

	if (setjmp(Ffailbuf))
		return FAIL;
	smax = str + MAXMSGLEN - 1;
	(void) alarm(2*MAXMSGTIME);
	for (;;) {
		if (read(fn, str, 1) <= 0)
			goto msgerr;
		*str &= 0177;
		if (*str == '\r')
			break;
		if (*str < ' ') {
			continue;
		}
		if (str++ >= smax)
			goto msgerr;
	}
	*str = '\0';
	(void) alarm(0);
	return SUCCESS;
msgerr:
	(void) alarm(0);
	return FAIL;
}

fwrdata(fp1, fn)
FILE *fp1;
int fn;
{
	register int alen, ret;
	char ack, ibuf[MAXMSGLEN];
	int flen, mil, retries = 0;
	long abytes, fbytes;
	struct timeb t1, t2;
	float ft;

	ret = FAIL;
retry:
	fchksum = 0xffff;
	abytes = fbytes = 0L;
	ack = '\0';
#ifdef USG
	time(&t1.time);
	t1.millitm = 0;
#else !USG
	ftime(&t1);
#endif !USG
	do {
		alen = fwrblk(fn, fp1, &flen);
		fbytes += flen;
		if (alen <= 0) {
			abytes -= alen;
			goto acct;
		}
		abytes += alen;
	} while (!feof(fp1) && !ferror(fp1));
	DEBUG(8, "\nchecksum: %04x\n", fchksum);
	if (frdmsg(ibuf, fn) != FAIL) {
		if ((ack = ibuf[0]) == 'G')
			ret = SUCCESS;
		DEBUG(4, "ack - '%c'\n", ack);
	}
acct:
#ifdef USG
	time(&t2.time);
	t2.millitm = 0;
#else !USG
	ftime(&t2);
#endif !USG
	Now = t2;
	t2.time -= t1.time;
	mil = t2.millitm - t1.millitm;
	if (mil < 0) {
		--t2.time;
		mil += 1000;
	}
	ft = (float)t2.time + (float)mil/1000.;
	sprintf(ibuf, "sent data %ld bytes %.2f secs %ld bps",
		fbytes, ft, (long)((float)fbytes*8./ft));
	sysacct(abytes, t2.time);
	Bytes_Sent += fbytes;
	if (retries > 0)
		sprintf(&ibuf[strlen(ibuf)], ", %d retries", retries);
	DEBUG(1, "%s\n", ibuf);
	log_xferstats(ibuf);
	if (ack == 'R') {
		DEBUG(4, "RETRY:\n", 0);
		fseek(fp1, 0L, 0);
		retries++;
		goto retry;
	}
#ifdef SYSACCT
	if (ret == FAIL)
		sysaccf(NULL);		/* force accounting */
#endif SYSACCT
	return ret;
}

/* max. attempts to retransmit a file: */
#define MAXRETRIES	(fbytes < 10000L ? 2 : 1)

frddata(fn, fp2)
register int fn;
register FILE *fp2;
{
	register int flen;
	register char eof;
	char ibuf[FIBUFSIZ];
	int ret, mil, retries = 0;
	long alen, abytes, fbytes;
	struct timeb t1, t2;
	float ft;

	ret = FAIL;
retry:
	fchksum = 0xffff;
	abytes = fbytes = 0L;
#ifdef USG
	time(&t1.time);
	t1.millitm = 0;
#else !USG
	ftime(&t1);
#endif !USG
	do {
		flen = frdblk(ibuf, fn, &alen);
		abytes += alen;
		if (flen < 0)
			goto acct;
		if (eof = flen > FIBUFSIZ)
			flen -= FIBUFSIZ + 1;
		fbytes += flen;
		if (fwrite(ibuf, sizeof (char), flen, fp2) != flen)
			goto acct;
	} while (!eof);
	ret = SUCCESS;
acct:
#ifdef USG
	time(&t2.time);
	t2.millitm = 0;
#else !USG
	ftime(&t2);
#endif !USG
	Now = t2;
	t2.time -= t1.time;
	mil = t2.millitm - t1.millitm;
	if (mil < 0) {
		--t2.time;
		mil += 1000;
	}
	ft = (float)t2.time + (float)mil/1000.;
	sprintf(ibuf, "received data %ld bytes %.2f secs %ld bps",
		fbytes, ft, (long)((float)fbytes*8./ft));
	if (retries > 0) 
		sprintf(&ibuf[strlen(ibuf)]," %d retries", retries);
	sysacct(abytes, t2.time);
	Bytes_Received += fbytes;
	DEBUG(1, "%s\n", ibuf);
	log_xferstats(ibuf);
	if (ret == FAIL) {
		if (retries++ < MAXRETRIES) {
			DEBUG(8, "send ack: 'R'\n", 0);
			fwrmsg('R', "", fn);
			fseek(fp2, 0L, 0);
			DEBUG(4, "RETRY:\n", 0);
			goto retry;
		}
		DEBUG(8, "send ack: 'Q'\n", 0);
		fwrmsg('Q', "", fn);
#ifdef SYSACCT
		sysaccf(NULL);		/* force accounting */
#endif SYSACCT
	}
	else {
		DEBUG(8, "send ack: 'G'\n", 0);
		fwrmsg('G', "", fn);
	}
	return ret;
}

static
frdbuf(blk, len, fn)
register char *blk;
register int len;
register int fn;
{
	static int ret = FIBUFSIZ / 2;

	if (setjmp(Ffailbuf))
		return FAIL;
	(void) alarm(MAXMSGTIME);
	ret = read(fn, blk, len);
	alarm(0);
	return ret <= 0 ? FAIL : ret;
}

#if !defined(BSD4_2) && !defined(USG)
/* call ultouch every TC calls to either frdblk or fwrblk  */
#define TC	20
static int tc = TC;
#endif !defined(BSD4_2) && !defined(USG)

/* Byte conversion:
 *
 *   from	 pre	   to
 * 000-037	 172	 100-137
 * 040-171		 040-171
 * 172-177	 173	 072-077
 * 200-237	 174	 100-137
 * 240-371	 175	 040-171
 * 372-377	 176	 072-077
 */

static
fwrblk(fn, fp, lenp)
int fn;
register FILE *fp;
int *lenp;
{
	register char *op;
	register int c, sum, nl, len;
	char obuf[FOBUFSIZ + 8];
	int ret;

#if !defined(BSD4_2) && !defined(USG)
	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
#endif !defined(BSD4_2) && !defined(USG)
	op = obuf;
	nl = 0;
	len = 0;
	sum = fchksum;
	while ((c = getc(fp)) != EOF) {
		len++;
		if (sum & 0x8000) {
			sum <<= 1;
			sum++;
		} else
			sum <<= 1;
		sum += c;
		sum &= 0xffff;
		if (c & 0200) {
			c &= 0177;
			if (c < 040) {
				*op++ = '\174';
				*op++ = c + 0100;
			} else
			if (c <= 0171) {
				*op++ = '\175';
				*op++ = c;
			}
			else {
				*op++ = '\176';
				*op++ = c - 0100;
			}
			nl += 2;
		} else {
			if (c < 040) {
				*op++ = '\172';
				*op++ = c + 0100;
				nl += 2;
			} else
			if (c <= 0171) {
				*op++ = c;
				nl++;
			} else {
				*op++ = '\173';
				*op++ = c - 0100;
				nl += 2;
			}
		}
		if (nl >= FOBUFSIZ - 1) {
			/*
			 * peek at next char, see if it will fit
			 */
			c = getc(fp);
			if (c == EOF)
				break;
			(void) ungetc(c, fp);
			if (nl >= FOBUFSIZ || c < 040 || c > 0171)
				goto writeit;
		}
	}
	/*
	 * At EOF - append checksum, there is space for it...
	 */
	sprintf(op, "\176\176%04x\r", sum);
	nl += strlen(op);
writeit:
	*lenp = len;
	fchksum = sum;
	DEBUG(8, "%d/", len);
	DEBUG(8, "%d,", nl);
	ret = write(fn, obuf, nl);
	return ret == nl ? nl : ret < 0 ? 0 : -ret;
}

static
frdblk(ip, fn, rlen)
register char *ip;
int fn;
long *rlen;
{
	register char *op, c;
	register int sum, len, nl;
	char buf[5], *erbp = ip;
	int i;
	static char special = 0;

#if !defined(BSD4_2) && !defined(USG)
	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
#endif !defined(BSD4_2) && !defined(USG)
	if ((len = frdbuf(ip, FIBUFSIZ, fn)) == FAIL) {
		*rlen = 0;
		goto dcorr;
	}
	*rlen = len;
	DEBUG(8, "%d/", len);
	op = ip;
	nl = 0;
	sum = fchksum;
	do {
		if ((*ip &= 0177) >= '\172') {
			if (special) {
				DEBUG(8, "%d", nl);
				special = 0;
				op = buf;
				if (*ip++ != '\176' || (i = --len) > 5)
					goto dcorr;
				while (i--)
					*op++ = *ip++ & 0177;
				while (len < 5) {
					i = frdbuf(&buf[len], 5 - len, fn);
					if (i == FAIL) {
						len = FAIL;
						goto dcorr;
					}
					DEBUG(8, ",%d", i);
					len += i;
					*rlen += i;
					while (i--)
						*op++ &= 0177;
				}
				if (buf[4] != '\r')
					goto dcorr;
				sscanf(buf, "%4x", &fchksum);
				DEBUG(8, "\nchecksum: %04x\n", sum);
				if (fchksum == sum)
					return FIBUFSIZ + 1 + nl;
				else {
					DEBUG(8, "\n", 0);
					DEBUG(4, "Bad checksum\n", 0);
					return FAIL;
				}
			}
			special = *ip++;
		} else {
			if (*ip < '\040') {
				/* error: shouldn't get control chars */
				goto dcorr;
			}
			switch (special) {
			case 0:
				c = *ip++;
				break;
			case '\172':
				c = *ip++ - 0100;
				break;
			case '\173':
				c = *ip++ + 0100;
				break;
			case '\174':
				c = *ip++ + 0100;
				break;
			case '\175':
				c = *ip++ + 0200;
				break;
			case '\176':
				c = *ip++ + 0300;
				break;
			}
			*op++ = c;
			if (sum & 0x8000) {
				sum <<= 1;
				sum++;
			} else
				sum <<= 1;
			sum += c & 0377;
			sum &= 0xffff;
			special = 0;
			nl++;
		}
	} while (--len);
	fchksum = sum;
	DEBUG(8, "%d,", nl);
	return nl;
dcorr:
	DEBUG(8, "\n", 0);
	DEBUG(4, "Data corrupted\n", 0);
	while (len != FAIL) {
		if ((len = frdbuf(erbp, FIBUFSIZ, fn)) != FAIL)
			*rlen += len;
	}
	return FAIL;
}

