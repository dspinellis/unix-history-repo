#ifdef lint
static char sccsid[] = "@(#)nio.c	5.1 (BERKELEY) %G%";
#endif

#include "uucp.h"
#include <sys/param.h>

extern unsigned short ntohs(), htons();

nturnon()
{
	return(0);
}

nturnoff()
{
	return(0);
}

nwrmsg(type, str, fn)
char type;
register char *str;
{
	char bufr[BUFSIZ];
	register char *cp = bufr;
	unsigned short nlen;
	int len;

	for (*cp++ = type; *cp = *str++; cp++)
		;
	if (cp[-1] == '\n')
		*--cp = (char)0;
	len = (cp - bufr) + 1;
	nlen = htons(len);
	if (write(fn, &nlen, sizeof nlen) != sizeof nlen ||
	    write(fn, bufr, len) != len)
		return(FAIL);
	return(0);
}

nrdmsg(str, fn)
char *str;
{
	int count, cc;
	unsigned short ncount;

	if ((cc = read(fn, &ncount, sizeof ncount)) != sizeof ncount) {
		DEBUG(7, "nrdmsg bad byte count read (%d)\n", cc);
		return(FAIL);
	}
	count = ntohs(ncount);
	DEBUG(7, "nrdmsg expecting %d bytes, got ", count);
	if ((cc = read(fn, str, count)) != count) {
		DEBUG(7, "%d\n", cc);
		return(FAIL);
	}
	DEBUG(7, "them%c", '\n');
	return(0);
}

/* call ultouch every TC calls to either grdblk or gwrblk -- rti!trt */
#define	TC	60
static	int tc = TC;

nwrdata(fp1, fn)
FILE *fp1;
{
	int len, cc;
	unsigned short nlen;
	char bufr[BUFSIZ], msg[64];
	extern long time();
	long bytes = 0L, secs = time((long *)0);

	while ((len = fread(bufr, sizeof(char), BUFSIZ, fp1)) > 0) {
		/* call ultouch occasionally -- rti!trt */
		if (--tc < 0) {
			tc = TC;
			ultouch();
		}
		DEBUG(7, "nwrdata writing %d bytes, ", len);
		nlen = htons((unsigned short)len);
		if ((cc = write(fn, &nlen, sizeof nlen)) != sizeof nlen) {
			DEBUG(7, "bad byte count write (%d)\n", cc);
			return(FAIL);
		}
		if ((cc = write(fn, bufr, len)) != len) {
			DEBUG(7, "wrote %d\n", cc);
			return(FAIL);
		}
		DEBUG(7, "succeeded%c", '\n');
		bytes += len;
	}
	nlen = 0;
	DEBUG(7, "nwrdata writing eof marker, ", 0);
	if ((cc = write(fn, &nlen, sizeof nlen)) != sizeof nlen) {
		DEBUG(7, "bad byte count write (%d)\n", cc);
		return(FAIL);
	}
	DEBUG(7, "succeeded%c", '\n');
	secs = time((long *)0) - secs;
	sprintf(msg, "sent data %ld bytes %ld secs", bytes, secs);
	DEBUG(1, "%s\n", msg);
	syslog(msg);
	sysacct(bytes, secs);
	return(0);
}

nrddata(fn, fp2)
FILE *fp2;
{
	int len, cc;
	unsigned short nlen;
	char bufr[BUFSIZ], msg[64];
	extern long time();
	long bytes = 0L, secs = time((long *)0);

	for (;;) {
		/* call ultouch occasionally -- rti!trt */
		if (--tc < 0 ) {
			tc = TC;
			ultouch();
		}
		if ((cc = read(fn, &nlen, sizeof nlen)) != sizeof nlen) {
			DEBUG(7, "nrddata bad byte count read (%d)\n", cc);
			return(FAIL);
		}
		len = ntohs(nlen);
		if (len == 0)
			break;
		DEBUG(7, "nrddata expecting %d bytes, got ", len);
		if ((cc = read(fn, bufr, len)) != len ||
		    fwrite(bufr, sizeof(char), len, fp2) != len) {
			DEBUG(7, "%d\n", cc);
			return(FAIL);
		}
		DEBUG(7, "them%c", '\n');
		bytes += len;
	}
	secs = time((long *)0) - secs;
	sprintf(msg, "received data %ld bytes %ld secs", bytes, secs);
	DEBUG(1, "%s\n", msg);
	syslog(msg);
	sysacct(bytes, secs);
	return(0);
}
