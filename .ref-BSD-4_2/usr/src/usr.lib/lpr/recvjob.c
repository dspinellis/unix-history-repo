#ifndef lint
static char sccsid[] = "@(#)recvjob.c	4.6 (Berkeley) 7/17/83";
#endif

/*
 * Receive printer jobs from the network, queue them and
 * start the printer daemon.
 */

#include "lp.h"

static char    tfname[40];	/* tmp copy of cf before linking */
static char    *dfname;		/* data files */

recvjob()
{
	struct stat stb;
	char *bp = pbuf;
	int status;

	/*
	 * Perform lookup for printer name or abbreviation
	 */
	if ((status = pgetent(line, printer)) < 0)
		fatal("cannot open printer description file");
	else if (status == 0)
		fatal("unknown printer");
	if ((LF = pgetstr("lf", &bp)) == NULL)
		LF = DEFLOGF;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;

	(void) close(2);
	(void) open(LF, O_WRONLY|O_APPEND);
	if (chdir(SD) < 0)
		fatal("cannot chdir to %s", SD);
	if (stat(LO, &stb) == 0 && (stb.st_mode & 010)) {
		/* queue is disabled */
		putchar('\1');		/* return error code */
		exit(1);
	}

	if (readjob())
		printjob();
}

char	*sp = "";
#define ack()	(void) write(1, sp, 1);

/*
 * Read printer jobs sent by lpd and copy them to the spooling directory.
 * Return the number of jobs successfully transfered.
 */
static
readjob(printer)
	char *printer;
{
	register int size, nfiles;
	register char *cp;

	ack();
	nfiles = 0;
	for (;;) {
		/*
		 * Read a command to tell us what to do
		 */
		cp = line;
		do {
			if ((size = read(1, cp, 1)) != 1) {
				if (size < 0)
					fatal("Lost connection");
				return(nfiles);
			}
		} while (*cp++ != '\n');
		*--cp = '\0';
		cp = line;
		switch (*cp++) {
		case '\1':	/* cleanup because data sent was bad */
			cleanup();
			continue;

		case '\2':	/* read cf file */
			size = 0;
			while (*cp >= '0' && *cp <= '9')
				size = size * 10 + (*cp++ - '0');
			if (*cp++ != ' ')
				break;
			strcpy(tfname, cp);
			tfname[0] = 't';
			if (!readfile(tfname, size)) {
				cleanup();
				continue;
			}
			if (link(tfname, cp) < 0)
				fatal("cannot rename %s", tfname);
			(void) unlink(tfname);
			tfname[0] = '\0';
			nfiles++;
			continue;

		case '\3':	/* read df file */
			size = 0;
			while (*cp >= '0' && *cp <= '9')
				size = size * 10 + (*cp++ - '0');
			if (*cp++ != ' ')
				break;
			(void) readfile(dfname = cp, size);
			continue;
		}
		fatal("protocol screwup");
	}
}

/*
 * Read files send by lpd and copy them to the spooling directory.
 */
static
readfile(file, size)
	char *file;
	int size;
{
	register char *cp;
	char buf[BUFSIZ];
	register int i, j, amt;
	int fd, err;

	fd = open(file, O_WRONLY|O_CREAT, FILMOD);
	if (fd < 0)
		fatal("cannot create %s", file);
	ack();
	err = 0;
	for (i = 0; i < size; i += BUFSIZ) {
		amt = BUFSIZ;
		cp = buf;
		if (i + amt > size)
			amt = size - i;
		do {
			j = read(1, cp, amt);
			if (j <= 0)
				fatal("Lost connection");
			amt -= j;
			cp += j;
		} while (amt > 0);
		amt = BUFSIZ;
		if (i + amt > size)
			amt = size - i;
		if (write(fd, buf, amt) != amt) {
			err++;
			break;
		}
	}
	(void) close(fd);
	if (err)
		fatal("%s: write error", file);
	if (noresponse()) {		/* file sent had bad data in it */
		(void) unlink(file);
		return(0);
	}
	ack();
	return(1);
}

static
noresponse()
{
	char resp;

	if (read(1, &resp, 1) != 1)
		fatal("Lost connection");
	if (resp == '\0')
		return(0);
	return(1);
}

/*
 * Remove all the files associated with the current job being transfered.
 */
static
cleanup()
{
	register int i;

	if (tfname[0])
		(void) unlink(tfname);
	if (dfname)
		do {
			do
				(void) unlink(dfname);
			while (dfname[i]-- != 'A');
			dfname[i] = 'z';
		} while (dfname[i-2]-- != 'd');
}

static
fatal(msg, a1)
	char *msg;
{
	cleanup();
	log(msg, a1);
	putchar('\1');		/* return error code */
	exit(1);
}
