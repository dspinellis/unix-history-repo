#ifndef lint
static char sccsid[] = "@(#)gio.c	5.1 (Berkeley) %G%";
#endif

#define USER 1
#include "pk.p"
#include <sys/types.h>
#include "pk.h"
#include <setjmp.h>
#include "uucp.h"

extern	time_t	time();


jmp_buf Failbuf;

struct pack *Pk;

pkfail()
{
	longjmp(Failbuf, 1);
}

gturnon()
{
	int ret;
	struct pack *pkopen();
	if (setjmp(Failbuf))
		return(FAIL);
	if (Pkdrvon) {
		ret = pkon(Ofn, PACKSIZE);
		DEBUG(4, "pkon - %d ", ret);
		DEBUG(4, "Ofn - %d\n", Ofn);
		if (ret <= 0)
			return(FAIL);
	}
	else {
		if (Debug > 4)
			pkdebug = 1;
		Pk = pkopen(Ifn, Ofn);
		if ((int) Pk == NULL)
			return(FAIL);
	}
	return(0);
}


gturnoff()
{
	if(setjmp(Failbuf))
		return(FAIL);
	if (Pkdrvon)
		pkoff(Ofn);
	else
		pkclose(Pk);
	return(0);
}


gwrmsg(type, str, fn)
char type;
register char *str;
{
	char bufr[BUFSIZ];
	register char *s;
	int len, i;

	if(setjmp(Failbuf))
		return(FAIL);
	bufr[0] = type;
	s = &bufr[1];
	while (*str)
		*s++ = *str++;
	*s = '\0';
	if (*(--s) == '\n')
		*s = '\0';
	len = strlen(bufr) + 1;
	if ((i = len % PACKSIZE)) {
		len = len + PACKSIZE - i;
		bufr[len - 1] = '\0';
	}
	gwrblk(bufr, len, fn);
	return(0);
}


grdmsg(str, fn)
register char *str;
{
	unsigned len;

	if(setjmp(Failbuf))
		return(FAIL);
	for (;;) {
		if (Pkdrvon)
			len = read(fn, str, PACKSIZE);
		else
			len = pkread(Pk, str, PACKSIZE);
		if (len == 0)
			continue;
		str += len;
		if (*(str - 1) == '\0')
			break;
	}
	return(0);
}


gwrdata(fp1, fn)
FILE *fp1;
{
	char bufr[BUFSIZ];
	register int len;
	int ret;
	time_t t1, t2;
	long bytes;
	char text[BUFSIZ];

	if(setjmp(Failbuf))
		return(FAIL);
	bytes = 0L;
	time(&t1);
	while ((len = fread(bufr, sizeof (char), BUFSIZ, fp1)) > 0) {
		bytes += len;
		ret = gwrblk(bufr, len, fn);
		if (ret != len) {
			return(FAIL);
		}
		if (len != BUFSIZ)
			break;
	}
	ret = gwrblk(bufr, 0, fn);
	time(&t2);
	sprintf(text, "sent data %ld bytes %ld secs", bytes, t2 - t1);
	DEBUG(1, "%s\n", text);
	syslog(text);
	sysacct(bytes, t2 - t1);
	return(0);
}


grddata(fn, fp2)
FILE *fp2;
{
	register int len;
	char bufr[BUFSIZ];
	time_t t1, t2;
	long bytes;
	char text[BUFSIZ];

	if(setjmp(Failbuf))
		return(FAIL);
	bytes = 0L;
	time(&t1);
	for (;;) {
		len = grdblk(bufr, BUFSIZ, fn);
		if (len < 0) {
			return(FAIL);
		}
		bytes += len;
		/* ittvax!swatt: check return value of fwrite */
		if (fwrite(bufr, sizeof (char), len, fp2) != len)
			return(FAIL);
		if (len < BUFSIZ)
			break;
	}
	time(&t2);
	sprintf(text, "received data %ld bytes %ld secs", bytes, t2 - t1);
	DEBUG(1, "%s\n", text);
	syslog(text);
	sysacct(bytes, t2 - t1);
	return(0);
}


/* call ultouch every TC calls to either grdblk or gwrblk -- rti!trt */
#define	TC	20
static	int tc = TC;

grdblk(blk, len,  fn)
register int len;
char *blk;
{
	register int i, ret;

	/* call ultouch occasionally -- rti!trt */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
	for (i = 0; i < len; i += ret) {
		if (Pkdrvon)
			ret = read(fn, blk, len - i);
		else
			ret = pkread(Pk, blk, len - i);
		if (ret < 0)
			return(FAIL);
		blk += ret;
		if (ret == 0)
			return(i);
	}
	return(i);
}


gwrblk(blk, len, fn)
register char *blk;
{
	register int ret;

	/* call ultouch occasionally -- rti!trt */
	if (--tc < 0) {
		tc = TC;
		ultouch();
	}
	if (Pkdrvon)
		ret = write(fn, blk, len);
	else
		ret = pkwrite(Pk, blk, len);
	return(ret);
}
