/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)logger.c	6.7 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <syslog.h>
#include <ctype.h>

/*
**  LOGGER -- read and log utility
**
**	This routine reads from an input and arranges to write the
**	result on the system log, along with a useful tag.
*/

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int pri = LOG_NOTICE;
	int ch, logflags = 0;
	char *tag, buf[200], *getlogin();

	tag = NULL;
	while ((ch = getopt(argc, argv, "f:ip:t:")) != EOF)
		switch((char)ch) {
		case 'f':		/* file to log */
			if (freopen(optarg, "r", stdin) == NULL) {
				fprintf("logger: ");
				perror(optarg);
				exit(1);
			}
			break;
		case 'i':		/* log process id also */
			logflags |= LOG_PID;
			break;
		case 'p':		/* priority */
			pri = pencode(optarg);
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

		for (p = buf, endp = buf + sizeof(buf) - 1;;) {
			len = strlen(*argv);
			if (p + len < endp && p > buf) {
				*--p = '\0';
				syslog(pri, buf);
				p = buf;
			}
			if (len > sizeof(buf) - 1) {
				syslog(pri, *argv++);
				if (!--argc)
					break;
			} else {
				bcopy(*argv++, p, len);
				p += len;
				if (!--argc)
					break;
				*p++ = ' ';
				*--p = '\0';
			}
		}
		if (p != buf) {
			*p = '\0';
			syslog(pri, buf);
		}
		exit(0);
	}

	/* main loop */
	while (fgets(buf, sizeof(buf), stdin) != NULL)
		syslog(pri, buf);

	exit(0);
}


struct code {
	char	*c_name;
	int	c_val;
};

struct code	PriNames[] = {
	"panic",	LOG_EMERG,
	"emerg",	LOG_EMERG,
	"alert",	LOG_ALERT,
	"crit",		LOG_CRIT,
	"err",		LOG_ERR,
	"error",	LOG_ERR,
	"warn",		LOG_WARNING,
	"warning",	LOG_WARNING,
	"notice",	LOG_NOTICE,
	"info",		LOG_INFO,
	"debug",	LOG_DEBUG,
	NULL,		-1
};

struct code	FacNames[] = {
	"kern",		LOG_KERN,
	"user",		LOG_USER,
	"mail",		LOG_MAIL,
	"daemon",	LOG_DAEMON,
	"auth",		LOG_AUTH,
	"security",	LOG_AUTH,
	"syslog",	LOG_SYSLOG,
	"lpr",		LOG_LPR,
	"news",		LOG_NEWS,
	"uucp",		LOG_UUCP,
	"local0",	LOG_LOCAL0,
	"local1",	LOG_LOCAL1,
	"local2",	LOG_LOCAL2,
	"local3",	LOG_LOCAL3,
	"local4",	LOG_LOCAL4,
	"local5",	LOG_LOCAL5,
	"local6",	LOG_LOCAL6,
	"local7",	LOG_LOCAL7,
	NULL,		-1
};


/*
 *  Decode a symbolic name to a numeric value
 */

pencode(s)
	register char *s;
{
	char *save;
	int fac, lev;

	for (save = s; *s && *s != '.'; ++s);
	if (*s) {
		*s = '\0';
		fac = decode(save, FacNames);
		if (fac < 0)
			bailout("unknown facility name: ", save);
		*s++ = '.';
	}
	else {
		fac = 0;
		s = save;
	}
	lev = decode(s, PriNames);
	if (lev < 0)
		bailout("unknown priority name: ", save);
	return ((lev & LOG_PRIMASK) | (fac & LOG_FACMASK));
}


decode(name, codetab)
	char *name;
	struct code *codetab;
{
	register struct code *c;

	if (isdigit(*name))
		return (atoi(name));

	for (c = codetab; c->c_name; c++)
		if (!strcasecmp(name, c->c_name))
			return (c->c_val);

	return (-1);
}

bailout(msg, arg)
	char *msg, *arg;
{
	fprintf(stderr, "logger: %s%s\n", msg, arg);
	exit(1);
}

usage()
{
	fputs("logger: [-i] [-f file] [-p pri] [-t tag] [ message ... ]\n",
	    stderr);
	exit(1);
}
