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
static char sccsid[] = "@(#)wall.c	5.9 (Berkeley) %G%";
#endif /* not lint */

/*
 * This program is not related to David Wall, whose Stanford Ph.D. thesis
 * is entitled "Mechanisms for Broadcast and Selective Broadcast".
 */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <utmp.h>
#include <pwd.h>
#include <errno.h>
#include <stdio.h>
#include <paths.h>

#define	IGNOREUSER	"sleeper"

int mbufsize;
char *mbuf;

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

	if (!(fp = fopen(_PATH_UTMP, "r"))) {
		fprintf(stderr, "wall: cannot read %s.\n", _PATH_UTMP);
		exit(1);
	}
	/* NOSTRICT */
	while (fread((char *)&utmp, sizeof(utmp), 1, fp) == 1)
		if (utmp.ut_name[0] &&
		    strncmp(utmp.ut_name, IGNOREUSER, sizeof(utmp.ut_name)))
			sendmsg(utmp.ut_line, 1, mbufsize);
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

	(void)strcpy(tmpname, _PATH_TMP);
	(void)strcat(tmpname, "/wall.XXXXXX");
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
	 * wrap message lines at column 79, not 80, because some terminals
	 * wrap after 79, some do not, and we can't tell.
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
		for (cnt = 0, p = lbuf; ch = *p; ++p, ++cnt) {
			if (cnt == 79 || ch == '\n') {
				putc('\r', fp);
				putc('\n', fp);
				cnt = 0;
			} else
				putc(ch, fp);
		}
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
	if (fread(mbuf, sizeof(*mbuf), mbufsize, fp) != mbufsize) {
		fprintf(stderr, "wall: can't read temporary file.\n");
		exit(1);
	}
	(void)close(fd);
}

sendmsg(line, nonblock, left)
	char *line;
{
	extern int errno;
	static char device[MAXNAMLEN] = _PATH_DEV;
	register int fd, wret;
	char *lp, *strcpy(), *strerror();

	(void)strcpy(device + 5, line);
	/*
	 * open will fail on slip lines or exclusive-use lines
	 * if not running as root
	 */
	if ((fd = open(device, O_WRONLY|(nonblock ? O_NONBLOCK : 0), 0)) < 0) {
		if (errno != EBUSY && errno != EPERM)
			(void)fprintf(stderr, "wall: %s: %s\n",
			    device, strerror(errno));
		return;
	}
	lp = mbuf + mbufsize - left;
	while (left) {
		wret = write(fd, lp, left);
		if (wret >= 0) {
			lp += wret;
			left -= wret;
		} else
		if (errno == EWOULDBLOCK) {
			if (fork()) {
				(void)close(fd);
				return;
			}
			/* wait at most 5 minutes */
			(void)alarm((u_int)(60 * 5));
			sendmsg(line, 0, left);
			exit(0);
		} else {
			/*
			 * We get ENODEV on a slip line
			 * if we're running as root
			 */
			if (errno != ENODEV)
				(void)fprintf(stderr, "wall: %s: %s\n",
				    device, strerror(errno));
			break;
		}
	}
	(void)close(fd);
}
