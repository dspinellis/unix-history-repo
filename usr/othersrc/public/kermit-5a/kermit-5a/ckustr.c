#include <stdio.h>
#include <sysexits.h>

/* string extraction/restoration routines */

/* Modify the pathname below to suit system requirements */

char	*StringFile =	"/usr/local/lib/kermit5.sr";

static int strfile = -1, ourpid = 0;

#define BUFLEN 256

errprep(offset, buf)
unsigned short offset;
char *buf;
{
register int pid = getpid();

	if (pid != ourpid) {
		ourpid = pid;
		if (strfile >= 0) {
			close(strfile);
			strfile = -1;
		}
	}
	if (strfile < 0) {
	        char *p, *getenv();
		if (p = getenv("KSTR")) StringFile = p;
		strfile = open(StringFile, 0);
		if (strfile < 0) {
oops:
			fprintf(stderr, "Cannot find %s\r\n", StringFile);
			exit(EX_OSFILE);
		}
	}
	if (lseek(strfile, (long) offset, 0) < 0
			|| read(strfile, buf, BUFLEN) <= 0)
		goto oops;
}

/* extracted string front end for printf() */
/*VARARGS1*/
strprerror(fmt, a, b, c, d, e)
	int fmt;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	printf(buf, a, b, c, d, e);
}

/* extracted string front end for sprintf() */
/*VARARGS1*/
strsrerror(fmt, obuf, a, b, c, d, e)
	int fmt;
	char *obuf;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	sprintf(obuf, buf, a, b, c, d, e);
}

/* extracted string front end for fprintf() */
/*VARARGS1*/
strfrerror(fmt, fd, a, b, c, d, e)
	int fmt, fd;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	fprintf(fd, buf, a, b, c, d, e);
}

/* extracted string front end for perror() */
strperror(fmt)
	int fmt;
{
	char buf[BUFLEN];

	errprep(fmt, buf);
	perror(buf);
}
