/*
 * Copyright (c) 1983, 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1983, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vacation.c	5.8 (Berkeley) %G%";
#endif /* not lint */

/*
**  Vacation
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
*/

#include <sys/param.h>
#include <sys/file.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include <sysexits.h>
#include <syslog.h>

/*
**  VACATION -- return a message to the sender when on vacation.
**
**	This program could be invoked as a message receiver when someone is
**	on vacation.  It returns a message specified by the user to whoever
**	sent the mail, taking care not to return a message too often to
**	prevent "I am on vacation" loops.
*/

#define	MAXLINE	500			/* max line from mail header */
#define	PERIOD	(60L*60L*24L*7L)	/* week between notifications */
#define	VMSG	".vacation.msg"		/* vacation message */
#define	VACAT	".vacation"		/* dbm's database prefix */
#define	VDIR	".vacation.dir"		/* dbm's DB prefix, part 1 */
#define	VPAG	".vacation.pag"		/* dbm's DB prefix, part 2 */

typedef struct alias {
	struct alias *next;
	char	*name;
} ALIAS;
ALIAS	*names;

static char from[MAXLINE];		/* sender's address */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	struct passwd *pw;
	ALIAS *cur;
	int ch, iflag;
	char *malloc();
	uid_t getuid();

	iflag = 0;
	while ((ch = getopt(argc, argv, "a:Ii")) != EOF)
		switch((char)ch) {
		case 'a':			/* alias */
			if (!(cur = (ALIAS *)malloc((u_int)sizeof(ALIAS))))
				break;
			cur->name = optarg;
			cur->next = names;
			names = cur;
			break;
		case 'i':			/* init the database */
		case 'I':			/* backward compatible */
			iflag = 1;
			break;
		case '?':
		default:
			goto usage;
		}
	argc -= optind;
	argv += optind;

	if (argc != 1) {
		if (!iflag) {
usage:			syslog(LOG_ERR, "uid %u: usage: vacation [-i] [-a alias] login\n", getuid());
			exit(EX_USAGE);
		}
		if (!(pw = getpwuid(getuid()))) {
			syslog(LOG_ERR, "vacation: no such user uid %u.\n", getuid());
			exit(EX_USAGE);
		}
	}
	else if (!(pw = getpwnam(*argv))) {
		syslog(LOG_ERR, "vacation: no such user %s.\n", *argv);
		exit(EX_USAGE);
	}
	if (chdir(pw->pw_dir)) {
		syslog(LOG_ERR, "vacation: no such directory %s.\n", pw->pw_dir);
		exit(EX_USAGE);
	}

	if (iflag) {
		initialize();
		exit(EX_OK);
	}

	if (!(cur = (ALIAS *)malloc((u_int)sizeof(ALIAS))))
		exit(EX_SOFTWARE);
	cur->name = pw->pw_name;
	cur->next = names;
	names = cur;

	readheaders();

	if (access(VDIR, F_OK))
		initialize();
	else
		dbminit(VACAT);

	if (!recent()) {
		setreply();
		sendmessage(pw->pw_name);
	}
	exit(EX_OK);
}

/*
 * readheaders --
 *	read mail headers
 */
getfrom(shortp)
char **shortp;
{
	register ALIAS *cur;
	register char *p;
	int tome, cont;
	char buf[MAXLINE], *strcpy(), *index();

	cont = tome = 0;
	while (fgets(buf, sizeof(buf), stdin) && *buf != '\n')
		switch(*buf) {
		case 'F':		/* "From " */
			cont = 0;
			if (!strncmp(buf, "From ", 5)) {
				for (p = buf + 5; *p && *p != ' '; ++p);
				*p = '\0';
				(void)strcpy(from, buf + 5);
				if (junkmail())
					exit(EX_OK);
			}
			break;
		case 'P':		/* "Precedence:" */
			cont = 0;
			if (strncasecmp(buf, "Precedence", 10) || buf[10] != ':' && buf[10] != ' ' && buf[10] != '\t')
				break;
			if (!(p = index(buf, ':')))
				break;
			while (*++p && isspace(*p));
			if (!*p)
				break;
			if (!strncasecmp(p, "junk", 4) || !strncasecmp(p, "bulk", 4))
				exit(EX_OK);
			break;
		case 'C':		/* "Cc:" */
			if (strncmp(buf, "Cc:", 3))
				break;
			cont = 1;
			goto findme;
		case 'T':		/* "To:" */
			if (strncmp(buf, "To:", 3))
				break;
			cont = 1;
			goto findme;
		default:
			if (!isspace(*buf) || !cont || tome) {
				cont = 0;
				break;
			}
findme:			for (cur = names; !tome && cur; cur = cur->next)
				tome += nsearch(cur->name, buf);
		}
	if (!tome)
		exit(EX_OK);
	if (!*from) {
		syslog(LOG_ERR, "vacation: no initial \"From\" line.\n");
		exit(EX_DATAERR);
	}
}

	return start;
}

/*
 * junkmail --
 *	read the header and return if automagic/junk/bulk mail
 */
static
junkmail()
{
	static struct ignore {
		char	*name;
		int	len;
	} ignore[] = {
		"-REQUEST", 8,	"Postmaster", 10,
		"uucp", 4,	"MAILER-DAEMON", 13,
		"MAILER", 6,	NULL, NULL,
	};
	register struct ignore *cur;
	register int len;
	register char *p;
	char *index(), *rindex();

	/*
	 * This is mildly amusing, and I'm not positive it's right; trying
	 * to find the "real" name of the sender, assuming that addresses
	 * will be some variant of:
	 *
	 * From site!site!SENDER%site.domain%site.domain@site.domain
	 */
	if (!(p = index(from, '%')))
		if (!(p = index(from, '@'))) {
			if (p = rindex(from, '!'))
				++p;
			else
				p = from;
			for (; *p; ++p);
		}
	len = p - from;
	for (cur = ignore; cur->name; ++cur)
		if (len >= cur->len && !strncasecmp(cur->name, p - cur->len, cur->len))
			return(1);
	return(0);
}

typedef struct {
	char	*dptr;
	int	dsize;
} DATUM;

typedef struct {
	time_t	sentdate;
} DBREC;

/*
 * recent --
 *	find out if user has gotten a vacation message recently.
 */
static
char *user;
{
	DATUM k, d, fetch();
	time_t now, then, time();

	k.dptr = from;
	k.dsize = strlen(from) + 1;
	d = fetch(k);
	if (d.dptr == NULL)
		return FALSE;
	bcopy(d.dptr, (char *)&ldbrec, sizeof ldbrec);	/* realign data */
	return ldbrec.sentdate + Timeout >= time((time_t *)0);
}

#ifndef VMUNIX
bcopy(from, to, size)
register char *to, *from;
register unsigned size;
{
	while (size-- > 0)
		*to++ = *from++;
}
#endif

/*
 * setreply --
 *	store that this user knows about the vacation.
 */
static
setreply()
{
	DBREC xrec;
	DATUM k, d;
	time_t time();

	k.dptr = from;
	k.dsize = strlen(from) + 1;
	(void)time(&xrec.sentdate);
	d.dptr = (char *)&xrec;
	d.dsize = sizeof(xrec);
	store(k, d);
}

/*
 * sendmessage --
 *	exec sendmail to send the vacation file to sender
 */
static
sendmessage(myname)
	char *myname;
{
	if (!freopen(VMSG, "r", stdin)) {
		syslog(LOG_ERR, "vacation: no ~%s/%s file.\n", myname, VMSG);
		exit(EX_NOINPUT);
	}
	execl("/usr/lib/sendmail", "sendmail", "-f", myname, from, NULL);
	syslog(LOG_ERR, "vacation: can't exec /usr/lib/sendmail.\n");
	exit(EX_OSERR);
}

/*
 * initialize --
 *	initialize the dbm database
 */
static
initialize()
{
	extern int errno;
	extern char *sys_errlist[];
	int fd;

	if ((fd = open(VDIR, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		syslog(LOG_ERR, "vacation: %s: %s\n", VDIR, sys_errlist[errno]);
		exit(EX_OSERR);
	}
	(void)close(fd);
	if ((fd = open(VPAG, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
		syslog(LOG_ERR, "vacation: %s: %s\n", VPAG, sys_errlist[errno]);
		exit(EX_OSERR);
	}
	(void)close(fd);
	dbminit(VACAT);
}
