/*	rlpr.c	4.1	83/01/06	*/
/*
 *      rlpr -- remote off line print.
 *              Invoked by lpd from other machines to transfer files.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include "lp.local.h"

char    tfname[40];		/* tmp copy of cf before linking */
char    cfname[40];		/* daemon control files, linked from tf's */

int	qflag;			/* q job, but don't exec daemon */
char	*SD;			/* spool directory */
char	*DN;			/* path name to daemon program */
char	*LF;			/* log file for errors */
int     MX;			/* maximum size in blocks of a print file */
char	*name;			/* program name */
char	*printer = DEFLP;	/* printer name */

char	*pgetstr();
char	*mktmp();
char	*malloc();
char	*getenv();
char	*rindex();

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;

	name = argv[0];
	while (--argc && (arg = *++argv)[0] == '-') {
		switch (arg[1]) {
		case 'P':		/* specifiy printer name */
			printer = &arg[2];
			break;

		case 'q':		/* just q job */
			qflag++;
			break;
		}
	}
	if (!chkprinter(printer))
		error(1);

	(void) close(2);
	(void) open(LF, 1);		/* standard error */
	(void) lseek(2, 0L, 2);
	if (chdir(SD) < 0) {
		log("cannot chdir to %s", SD);
		error(1);
	}

	if (transfer()) {
		if (qflag)		/* just q things up */
			exit(0);
		execl(DN, arg = rindex(DN, "/") ? arg+1 : DN, printer, 0);
		log("can't execl printer daemon (%s)", DN);
		error(1);
	}
}

char	*sp = "";
#define ack()	(void) write(1, sp, 1);

/*
 * Read files sent by lpd and copy them to the spooling directory.
 * Return the number of jobs successfully transfered.
 */
transfer()
{
	register char *cp;
	char buf[100];
	register int size, nfiles;

	ack();
	nfiles = 0;
	for (;;) {
		/*
		 * Read a command to tell us what to do
		 */
		cp = buf;
		do {
			if ((size = read(0, cp, 1)) != 1) {
				if (size < 0)
					lostconn();
				return(nfiles);
			}
		} while (*cp++ != '\n');
		*--cp = '\0';
		cp = buf;
		switch (*cp++) {
		case '\1':	/* cleanup and die */
			log("error from lpd");
			cleanup();
			exit(1);

		case '\2':	/* read cf file */
			size = 0;
			while (*cp >= '0' && *cp <= '9')
				size = size * 10 + (*cp++ - '0');
			if (*cp++ != ' ')
				break;
			strcpy(cfname, cp);
			strcpy(tfname, cp);
			tfname[0] = 't';
			readfile(tfname, size);
			if (link(tfname, cfname) < 0) {
				log("cannot rename %s\n", tfname);
				error(1);
			}
			unlink(tfname);
			nfiles++;
			tfname[0] = cfname[0] = '\0';
			continue;

		case '\3':	/* read df file */
			size = 0;
			while (*cp >= '0' && *cp <= '9')
				size = size * 10 + (*cp++ - '0');
			if (*cp++ != ' ')
				break;
			readfile(cp, size);
			continue;
		}
		log("protocol screwup");
		error(0);
		return(nfiles);
	}
}

/*
 * Read files send by lpd and copy them to the spooling directory.
 */
readfile(file, size)
	char *file;
	int size;
{
	register char *cp;
	char buf[BUFSIZ];
	register int i, j, amt;
	int fd, err;

	i = umask(0);
	fd = creat(file, FILMOD);
	(void) umask(i);
	if (fd < 0) {
		log("cannot create %s\n", file);
		error(1);
	}
	ack();
	err = 0;
	for (i = 0; i < size; i += BUFSIZ) {
		amt = BUFSIZ;
		cp = buf;
		if (i + amt > size)
			amt = size - i;
		do {
			j = read(0, cp, amt);
			if (j <= 0)
				lostconn();
			amt -= j;
			cp += j;
		} while (amt > 0);
		amt = BUFSIZ;
		if (i + amt > size)
			amt = size - i;
		if (err == 0 && write(fd, buf, amt) != amt)
			err++;
	}
	(void) close(fd);
	if (response() || err) {
		log("%s: write error", file);
		error(1);
	}
	ack();
}

response()
{
	char resp;

	if (read(0, &resp, 1) != 1)
		lostconn();
	if (resp == '\0')
		return(0);
	return(-1);
}

lostconn()
{
	log("lost connection");
	error(1);
}

/*
 * Tell lpd that we had an error.
 */
error()
{
	char c;

	c = '\1';
	(void) write(1, &c, 1);
	exit(1);
}

/*VARARGS1*/
log(message, a1, a2, a3)
	char *message;
{
	short console = isatty(fileno(stderr));

	fprintf(stderr, console ? "\r\n%s: " : "%s: ", name);
	fprintf(stderr, message, a1, a2, a3);
	if (console)
		putc('\r', stderr);
	putc('\n', stderr);
	fflush(stderr);
}

/*
 * Perform lookup for printer name or abbreviation
 */
chkprinter(s)
	register char *s;
{
	static char buf[BUFSIZ/2];
	char b[BUFSIZ];
	char *bp = buf;

	if (pgetent(b, s) <= 0)
		return(0);
	if ((DN = pgetstr("dn", &bp)) == NULL)
		DN = DEFDAEMON;
	if ((LF = pgetstr("lf", &bp)) == NULL)
		LF = DEFLOGF;
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((MX = pgetnum("mx")) < 0)
		MX = DEFMX;
	return(1);
}

cleanup()
{
	if (tfname[0])
		(void) unlink(tfname);
	if (cfname[0])
		(void) unlink(cfname);
}
