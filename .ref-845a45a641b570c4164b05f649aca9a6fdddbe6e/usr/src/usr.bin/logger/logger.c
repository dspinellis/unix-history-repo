/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)logger.c	6.16 (Berkeley) %G%";
#endif /* not lint */

#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define	SYSLOG_NAMES
#include <syslog.h>

int	decode __P((char *, CODE *));
int	pencode __P((char *));
void	usage __P((void));

/*
 * logger -- read and log utility
 *
 *	Reads from an input and arranges to write the result on the system
 *	log.
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, logflags, pri;
	char *tag, buf[1024];

	tag = NULL;
	pri = LOG_NOTICE;
	logflags = 0;
	while ((ch = getopt(argc, argv, "f:ip:st:")) != EOF)
		switch((char)ch) {
		case 'f':		/* file to log */
			if (freopen(optarg, "r", stdin) == NULL) {
				(void)fprintf(stderr, "logger: %s: %s.\n",
				    optarg, strerror(errno));
				exit(1);
			}
			break;
		case 'i':		/* log process id also */
			logflags |= LOG_PID;
			break;
		case 'p':		/* priority */
			pri = pencode(optarg);
			break;
		case 's':		/* log to standard error */
			logflags |= LOG_PERROR;
			break;
		case 't':		/* tag */
			tag = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* setup for logging */
	openlog(tag ? tag : getlogin(), logflags, 0);
	(void) fclose(stdout);

	/* log input line if appropriate */
	if (argc > 0) {
		register char *p, *endp;
		int len;

		for (p = buf, endp = buf + sizeof(buf) - 2; *argv;) {
			len = strlen(*argv);
			if (p + len > endp && p > buf) {
				syslog(pri, "%s", buf);
				p = buf;
			}
			if (len > sizeof(buf) - 1)
				syslog(pri, "%s", *argv++);
			else {
				if (p != buf)
					*p++ = ' ';
				bcopy(*argv++, p, len);
				*(p += len) = '\0';
			}
		}
		if (p != buf)
			syslog(pri, "%s", buf);
	} else
		while (fgets(buf, sizeof(buf), stdin) != NULL)
			syslog(pri, "%s", buf);
	exit(0);
}

/*
 *  Decode a symbolic name to a numeric value
 */
int
pencode(s)
	register char *s;
{
	char *save;
	int fac, lev;

	for (save = s; *s && *s != '.'; ++s);
	if (*s) {
		*s = '\0';
		fac = decode(save, facilitynames);
		if (fac < 0) {
			(void)fprintf(stderr,
			    "logger: unknown facility name: %s.\n", save);
			exit(1);
		}
		*s++ = '.';
	}
	else {
		fac = 0;
		s = save;
	}
	lev = decode(s, prioritynames);
	if (lev < 0) {
		(void)fprintf(stderr,
		    "logger: unknown priority name: %s.\n", save);
		exit(1);
	}
	return ((lev & LOG_PRIMASK) | (fac & LOG_FACMASK));
}

int
decode(name, codetab)
	char *name;
	CODE *codetab;
{
	register CODE *c;

	if (isdigit(*name))
		return (atoi(name));

	for (c = codetab; c->c_name; c++)
		if (!strcasecmp(name, c->c_name))
			return (c->c_val);

	return (-1);
}

void
usage()
{
	(void)fprintf(stderr,
	    "logger: [-is] [-f file] [-p pri] [-t tag] [ message ... ]\n");
	exit(1);
}
