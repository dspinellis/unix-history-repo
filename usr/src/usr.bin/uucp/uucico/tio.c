#ifndef lint
static char sccsid[] = "@(#)tio.c	4.9	(Berkeley) %G%";
#endif

#include <signal.h>
#include "uucp.h"
#include <setjmp.h>
#include <sys/stat.h>
#include <machine/machparam.h>

extern int pkfail();
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

extern jmp_buf Failbuf;

extern long Bytes_Sent, Bytes_Received;

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
	alarm(MAXMSGTIME*5);
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
	alarm(MAXMSGTIME*5);
	for (;;) {
		len = read(fn, str, TPACKSIZE);
		if (len <= 0) {
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
	struct timeb t1, t2;
	long bytes;
	char text[TBUFSIZE];
	float ft;

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
	bytes = 0L;
#ifdef USG
	time(&t1.time);
	t1.millitm = 0;
#else !USG
	ftime(&t1);
#endif !USG
	while ((len = read(fileno(fp1), bufr.t_data, TBUFSIZE)) > 0) {
		bytes += len;
#if defined(vax) || defined(pdp11) || defined(ns32000)
		bufr.t_nbytes = htonl((long)len);
#else !vax and !pdp11 and !ns32000
		bufr.t_nbytes = len;
#endif !vax and !pdp11 and !ns32000
		DEBUG(8,"twrdata sending %d bytes\n",len);
		len += sizeof(long);
		alarm(MAXMSGTIME*5);
		ret = twrblk((char *)&bufr, len, fn);
		alarm(0);
		if (ret != len)
			return FAIL;
		if (len != TBUFSIZE+sizeof(long))
			break;
	}
	bufr.t_nbytes = 0;
	len = sizeof(long);
	alarm(MAXMSGTIME*5);
	ret = twrblk((char *)&bufr, len, fn);
	alarm(0);
	if (ret != len)
		return FAIL;
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
	sprintf(text, "sent data %ld bytes %.2f secs %ld bps",
		bytes, ft, (long)((float)bytes*8./ft));
	sysacct(bytes, t2.time);
	Bytes_Sent += bytes;
	DEBUG(1, "%s\n", text);
	log_xferstats(text);
	return SUCCESS;
}

trddata(fn, fp2)
FILE *fp2;
{
	register int len, nread;
	char bufr[TBUFSIZE];
	struct timeb t1, t2;
	int mil;
	long bytes, Nbytes;
	float ft;

	if(setjmp(Failbuf))
		return FAIL;
	signal(SIGALRM, pkfail);
#ifdef USG
	time(&t1.time);
	t1.millitm = 0;
#else !USG
	ftime(&t1);
#endif !USG
	bytes = 0L;
	for (;;) {
		alarm(MAXMSGTIME*5);
		len = trdblk((char *)&Nbytes,sizeof Nbytes,fn);
		alarm(0);
		if (len != sizeof Nbytes)
			return FAIL;
#if defined(vax) || defined(pdp11) || defined(ns32000)
		Nbytes = ntohl(Nbytes);
#endif vax or pdp11 or ns32000
		DEBUG(8,"trddata expecting %ld bytes\n",Nbytes);
		nread = Nbytes;
		if (nread == 0)
			break;
		alarm(MAXMSGTIME*5);
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
	sprintf(bufr, "received data %ld bytes %.2f secs %ld bps",
		bytes, ft, (long)((float)bytes*8./ft));
	sysacct(bytes, t2.time);
	Bytes_Received += bytes;
	DEBUG(1, "%s\n", bufr);
	log_xferstats(bufr);
	return SUCCESS;
}

#if !defined(BSD4_2) && !defined(USG)
#define	TC	1024
static	int tc = TC;
#endif !BSD4_2 && !USG

trdblk(blk, len,  fn)
register int len;
char *blk;
{
	register int i, ret;

#if !defined(BSD4_2) && !defined(USG)
	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
#endif !BSD4_2 && !USG
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
#if !defined(BSD4_2) && !defined(USG)
	/* call ultouch occasionally */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
#endif !BSD4_2 && !USG
	return write(fn, blk, len);
}
