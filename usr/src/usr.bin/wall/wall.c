/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)wall.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * This program is not related to David Wall, whose Stanford Ph.D. thesis
 * is entitled "Mechanisms for Broadcast and Selective Broadcast".
 */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/signal.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <fcntl.h>
#include <utmp.h>
#include <pwd.h>
#include <errno.h>
#include <stdio.h>

#define	IGNOREUSER	"sleeper"
#define	UTMP		"/etc/utmp"

static int mbufsize;
static char *mbuf;

/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	struct utmp utmp;
	FILE *fp;

	if (argc > 2) {
		fprintf(stderr, "usage: wall [file]\n");
		exit(1);
	}
	makemsg(argv);

	if (!(fp = fopen(UTMP, "r"))) {
		fprintf(stderr, "wall: cannot read /etc/utmp.\n");
		exit(1);
	}
	/* NOSTRICT */
	while (fread((char *)&utmp, sizeof(utmp), 1, fp) == 1) {
		if (!utmp.ut_name[0] ||
		    !strncmp(utmp.ut_name, IGNOREUSER, sizeof(utmp.ut_name)))
			continue;
		sendmsg(utmp.ut_line);
	}
	exit(0);
}

makemsg(argv)
	char **argv;
{
	register int ch, cnt;
	struct tm *lt;
	struct passwd *pw, *getpwuid();
	struct stat sbuf;
	time_t now, time();
	FILE *fp;
	int fd;
	char *p, *whom, hostname[MAXHOSTNAMELEN], lbuf[100], tmpname[15];
	char *getlogin(), *malloc(), *strcpy(), *ttyname();

	(void)strcpy(tmpname, "/tmp/wall.XXX");
	if (!(fd = mkstemp(tmpname)) || !(fp = fdopen(fd, "r+"))) {
		fprintf(stderr, "wall: can't open temporary file.\n");
		exit(1);
	}
	(void)unlink(tmpname);

	if (!(whom = getlogin()))
		whom = (pw = getpwuid(getuid())) ? pw->pw_name : "???";
	(void)gethostname(hostname, sizeof(hostname));
	(void)time(&now);
	lt = localtime(&now);

	/*
	 * all this stuff is to blank out a square for the message; we
	 * limit message lines to 75 characters, and blank out to 79.
	 * Not 80 'cause some terminals do weird stuff then.
	 */
	fprintf(fp, "\r%79s\r\n", " ");
	(void)sprintf(lbuf, "Broadcast Message from %s@%s", whom, hostname);
	fprintf(fp, "%-79.79s\007\007\r\n", lbuf);
	(void)sprintf(lbuf, "        (%s) at %d:%02d ...", ttyname(2),
	    lt->tm_hour, lt->tm_min);
	fprintf(fp, "%-79.79s\r\n", lbuf);
	fprintf(fp, "%79s\r\n", " ");

	if (*++argv && !(freopen(*argv, "r", stdin))) {
		fprintf(stderr, "wall: can't read %s.\n", *argv);
		exit(1);
	}
	while (fgets(lbuf, sizeof(lbuf), stdin))
		for (cnt = 0, p = lbuf; ch = *p; ++p, ++cnt)
			if (cnt == 75 || ch == '\n') {
				for (; cnt < 79; ++cnt)
					putc(' ', fp);
				putc('\r', fp);
				putc('\n', fp);
				cnt = 1;
			} else
				putc(ch, fp);
	fprintf(fp, "%79s\r\n", " ");
	rewind(fp);

	if (fstat(fd, &sbuf)) {
		fprintf(stderr, "wall: can't stat temporary file.\n");
		exit(1);
	}
	mbufsize = sbuf.st_size;
	if (!(mbuf = malloc((u_int)mbufsize))) {
		fprintf(stderr, "wall: out of memory.\n");
		exit(1);
	}
	(void)fread(mbuf, sizeof(*mbuf), mbufsize, fp);
	(void)close(fd);
}

sendmsg(line)
	char *line;
{
	extern int errno;
	static char device[MAXNAMLEN] = "/dev/";
	register int fd, flags, nread;
	char *lp, *strcpy();

	(void)strcpy(device + 5, line);
	if ((fd = open(device, O_WRONLY, 0)) < 0) {
		fprintf(stderr, "wall: %s: ", device);
		perror((char *)NULL);
	}
	flags = fcntl(fd, F_GETFL, 0);
	if (!(flags & FNDELAY)) {
		/* NDELAY bit not set; if can't set, fork instead */
		if (fcntl(fd, F_SETFL, flags|FNDELAY) == -1) {
			flags = 0;
			goto forkit;
		}
	}
	else
		flags = 0;
	lp = mbuf;
	while ((nread = write(fd, lp, mbufsize)) != mbufsize) {
		if (mbufsize > 0) {
			mbufsize -= nread;
			lp += nread;
		} else if (errno == EWOULDBLOCK) {
			/* child resets FNDELAY if necessary; parent leaves */
forkit:			if (fork()) {
				(void)close(fd);
				return;
			}
			if (flags)
				(void)fcntl(fd, F_SETFL, flags);
			/* wait 5 minutes and then quit */
			(void)alarm((u_int)(60 * 5));
			(void)write(fd, mbuf, mbufsize);
			exit(0);
		} else {
			fprintf(stderr, "wall: %s: ", device);
			perror((char *)NULL);
			break;
		}
	}
	/* write was successful, or error != EWOULDBLOCK; cleanup */
	if (flags)
		(void)fcntl(fd, F_SETFL, flags);
	(void)close(fd);
}
