#ifndef lint
static char sccsid[] = "@(#)tio.c	4.1 (Berkeley) %G%";
#endif

#include <sys/types.h>
#include <setjmp.h>
#include "uucp.h"
#include <signal.h>
#include <sys/stat.h>
#ifdef USG
#define ftime time
#else V7
#include <sys/timeb.h>
#endif V7

extern int pkfail();
extern	time_t	time();
#define TPACKSIZE	512
#define TBUFSIZE	1024
#define min(a,b)	(((a)<(b))?(a):(b))

/*
 *	htonl is a function that converts a long from host
 *		order to network order
 *	ntohl is a function that converts a long from network
 *		order to host order
 *
 *	network order is 		0 1 2 3 (bytes in a long)
 *	host order on a vax is		3 2 1 0
 *	host order on a pdp11 is	1 0 3 2
 *	host order on a 68000 is	0 1 2 3
 *	most other machines are		0 1 2 3
 */

struct tbuf {
	long t_nbytes;
	char t_data[TBUFSIZE];
};

jmp_buf Failbuf;

tnullf()
{
	return SUCCESS;
}

twrmsg(type, str, fn)
char type;
register char *str;
{
	char bufr[TBUFSIZE];
	register char *s;
	int len, i;

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
	alarm(MAXMSGTIME);
	bufr[0] = type;
	s = &bufr[1];
	while (*str)
		*s++ = *str++;
	*s = '\0';
	if (*(--s) == '\n')
		*s = '\0';
	len = strlen(bufr) + 1;
	if ((i = len % TPACKSIZE)) {
		len = len + TPACKSIZE - i;
		bufr[len - 1] = '\0';
	}
	twrblk(bufr, len, fn);
	alarm(0);
	return SUCCESS;
}

trdmsg(str, fn)
register char *str;
{
	int len, cnt = 0;

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
	alarm(MAXMSGTIME);
	for (;;) {
		len = read(fn, str, TPACKSIZE);
		if (len == 0)
			continue;
		if (len < 0) {
			alarm(0);
			return FAIL;
		}
		str += len;
		cnt += len;
		if (*(str - 1) == '\0' && (cnt % TPACKSIZE) == 0)
			break;
	}
	alarm(0);
	return SUCCESS;
}

twrdata(fp1, fn)
FILE *fp1;
{
	struct tbuf bufr;
	register int len;
	int ret, mil;
#ifdef USG
	time_t t1, t2;
#else v7
	struct timeb t1, t2;
#endif v7
	long bytes;
	char text[TBUFSIZE];

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
	bytes = 0L;
	ftime(&t1);
	while ((len = read(fileno(fp1), bufr.t_data, TBUFSIZE)) > 0) {
		bytes += len;
#if defined(vax) || defined(pdp11)
		bufr.t_nbytes = htonl((long)len);
#else !vax and !pdp11
		bufr.t_nbytes = len;
#endif !vax and !pdp11
		DEBUG(8,"twrdata sending %d bytes\n",len);
		len += sizeof(long);
		alarm(MAXMSGTIME);
		ret = twrblk(&bufr, len, fn);
		alarm(0);
		if (ret != len)
			return FAIL;
		if (len != TBUFSIZE+sizeof(long))
			break;
	}
	bufr.t_nbytes = 0;
	len = sizeof(long);
	alarm(MAXMSGTIME);
	ret = twrblk(&bufr, len, fn);
	alarm(0);
	if (ret != len)
		return FAIL;
	ftime(&t2);
#ifndef USG
	t2.time -= t1.time;
	mil = t2.millitm - t1.millitm;
	if (mil < 0) {
		--t2.time;
		mil += 1000;
	}
	sprintf(text, "sent data %ld bytes %ld.%03d secs",
				bytes, (long)t2.time, mil);
	sysacct(bytes, t2.time - t1.time);
#else USG
	sprintf(text, "sent data %ld bytes %ld secs", bytes, t2 - t1);
	sysacct(bytes, t2 - t1);
#endif USG
	DEBUG(1, "%s\n", text);
	syslog(text);
	return SUCCESS;
}


trddata(fn, fp2)
FILE *fp2;
{
	register int len, nread;
	char bufr[TBUFSIZE];
#ifdef USG
	time_t t1, t2;
#else V7
	struct timeb t1, t2;
	int mil;
#endif V7
	long bytes, Nbytes;

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
	ftime(&t1);
	bytes = 0L;
	for (;;) {
		alarm(MAXMSGTIME);
		len = trdblk(&Nbytes,sizeof Nbytes,fn);
		alarm(0);
		if (len != sizeof Nbytes)
			return FAIL;
#if defined(vax) || defined(pdp11)
		Nbytes = ntohl(Nbytes);
#endif vax or pdp11
		DEBUG(8,"trddata expecting %ld bytes\n",Nbytes);
		nread = Nbytes;
		if (nread == 0)
			break;
		alarm(MAXMSGTIME);
		len = trdblk(bufr, nread, fn);
		alarm(0);
		if (len < 0) {
			return FAIL;
		}
		bytes += len;
		DEBUG(11,"trddata got %ld\n",bytes);
		if (write(fileno(fp2), bufr, len) != len) {
			alarm(0);
			return FAIL;
		}
	}
	ftime(&t2);
#ifndef USG
	t2.time -= t1.time;
	mil = t2.millitm - t1.millitm;
	if (mil < 0) {
		--t2.time;
		mil += 1000;
	}
	sprintf(bufr, "received data %ld bytes %ld.%03d secs",
				bytes, (long)t2.time, mil);
	sysacct(bytes, t2.time - t1.time);
#else USG
	sprintf(bufr, "received data %ld bytes %ld secs", bytes, t2 - t1);
	sysacct(bytes, t2 - t1);
#endif USG
	DEBUG(1, "%s\n", bufr);
	syslog(bufr);
	return SUCCESS;
}


#define	TC	1024
static	int tc = TC;

trdblk(blk, len,  fn)
register int len;
char *blk;
{
	register int i, ret;

	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
	for (i = 0; i < len; i += ret) {
		ret = read(fn, blk, len - i);
		if (ret < 0)
			return FAIL;
		blk += ret;
		if (ret == 0)
			return i;
	}
	return i;
}


twrblk(blk, len, fn)
register char *blk;
{
	register int ret;
	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
	ret = write(fn, blk, len);
	return ret;
}
