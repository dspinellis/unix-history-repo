/*
 * Copyright (c) 1980, 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkpasswd.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <ndbm.h>
#include <pwd.h>
#include <stdio.h>

static FILE *_pw_fp;
static struct passwd _pw_passwd;
static off_t offset;

#define	MAXLINELENGTH	1024
static char line[MAXLINELENGTH];

/*
 * Mkpasswd does two things -- use the ``arg'' file to create ``arg''.{pag,dir}
 * for ndbm, and, if the -p flag is on, create a password file in the original
 * format.  It doesn't use the getpwent(3) routines because it has to figure
 * out offsets for the encrypted passwords to put in the dbm files.  One other
 * problem is that, since the addition of shadow passwords, getpwent(3) has to
 * use the dbm databases rather than simply scanning the actual file.  This
 * required the addition of a flag field to the dbm database to distinguish
 * between a record keyed by name, and one keyed by uid.
 */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	register char *flag, *p, *t;
	register int makeold;
	FILE *oldfp;
	DBM *dp;
	datum key, content;
	int ch;
	char buf[8192], nbuf[50], *strerror();
	static int scanpw();

	makeold = 0;
	while ((ch = getopt(argc, argv, "pv")) != EOF)
		switch(ch) {
		case 'p':			/* create ``password.orig'' */
			makeold = 1;
			/* FALLTHROUGH */
		case 'v':			/* backward compatible */
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1)
		usage();

	if (!(_pw_fp = fopen(*argv, "r"))) {
		(void)fprintf(stderr,
		    "mkpasswd: %s: can't open for reading.\n", *argv);
		exit(1);
	}

	rmall(*argv);
	(void)umask(0);

	/* open old password format file, dbm files */
	if (makeold) {
		int oldfd;

		(void)sprintf(buf, "%s.orig", *argv);
		if ((oldfd = open(buf, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
			(void)fprintf(stderr, "mkpasswd: %s: %s\n", buf,
			    strerror(errno));
			exit(1);
		}
		if (!(oldfp = fdopen(oldfd, "w"))) {
			(void)fprintf(stderr, "mkpasswd: %s: fdopen failed.\n",
			    buf);
			exit(1);
		}
	}
	if (!(dp = dbm_open(*argv, O_WRONLY|O_CREAT|O_EXCL, 0644))) {
		(void)fprintf(stderr, "mkpasswd: %s: %s\n", *argv,
		    strerror(errno));
		exit(1);
	}

	content.dptr = buf;
	while (scanpw()) {
		/* create dbm entry */
		p = buf;
#define	COMPACT(e)	t = e; while (*p++ = *t++);
		COMPACT(_pw_passwd.pw_name);
		(void)sprintf(nbuf, "%ld", offset);
		COMPACT(nbuf);
		bcopy((char *)&_pw_passwd.pw_uid, p, sizeof(int));
		p += sizeof(int);
		bcopy((char *)&_pw_passwd.pw_gid, p, sizeof(int));
		p += sizeof(int);
		bcopy((char *)&_pw_passwd.pw_change, p, sizeof(time_t));
		p += sizeof(time_t);
		COMPACT(_pw_passwd.pw_class);
		COMPACT(_pw_passwd.pw_gecos);
		COMPACT(_pw_passwd.pw_dir);
		COMPACT(_pw_passwd.pw_shell);
		bcopy((char *)&_pw_passwd.pw_expire, p, sizeof(time_t));
		p += sizeof(time_t);
		flag = p;
		*p++ = _PW_KEYBYNAME;
		content.dsize = p - buf;
#ifdef debug
		(void)printf("store %s, uid %d\n", _pw_passwd.pw_name,
		    _pw_passwd.pw_uid);
#endif
		key.dptr = _pw_passwd.pw_name;
		key.dsize = strlen(_pw_passwd.pw_name);
		if (dbm_store(dp, key, content, DBM_INSERT) < 0)
			goto bad;
		key.dptr = (char *)&_pw_passwd.pw_uid;
		key.dsize = sizeof(int);
		*flag = _PW_KEYBYUID;
		if (dbm_store(dp, key, content, DBM_INSERT) < 0)
			goto bad;

		/* create original format password file entry */
		if (!makeold)
			continue;
		fprintf(oldfp, "%s:*:%d:%d:%s:%s:%s\n", _pw_passwd.pw_name,
		    _pw_passwd.pw_uid, _pw_passwd.pw_gid, _pw_passwd.pw_gecos,
		    _pw_passwd.pw_dir, _pw_passwd.pw_shell);
	}
	dbm_close(dp);
	exit(0);

bad:	(void)fprintf(stderr, "mkpasswd: dbm_store failed.\n");
	rmall(*argv);
	exit(1);
}

rmall(fname)
	char *fname;
{
	register char *p;
	char buf[MAXPATHLEN], *strcpy();

	for (p = strcpy(buf, fname); *p; ++p);
	bcopy(".pag", p, 5);
	(void)unlink(buf);
	bcopy(".dir", p, 5);
	(void)unlink(buf);
	bcopy(".orig", p, 6);
	(void)unlink(buf);
}

usage()
{
	(void)fprintf(stderr, "usage: mkpasswd [-p] passwd_file\n");
	exit(1);
}

/* from libc/gen/getpwent.c */

static
scanpw()
{
	register char *cp;
	long atol(), ftell();
	char *bp;
	char *fgets(), *strsep(), *index();

	for (;;) {
		offset = ftell(_pw_fp);
		if (!(fgets(line, sizeof(line), _pw_fp)))
			return(0);
		/* skip lines that are too big */
		if (!index(line, '\n')) {
			int ch;

			while ((ch = getc(_pw_fp)) != '\n' && ch != EOF)
				;
			continue;
		}
		bp = line;
		_pw_passwd.pw_name = strsep(&bp, ":\n");
		_pw_passwd.pw_passwd = strsep(&bp, ":\n");
		offset += _pw_passwd.pw_passwd - line;
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_uid = atoi(cp);
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_gid = atoi(cp);
		_pw_passwd.pw_class = strsep(&bp, ":\n");
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_change = atol(cp);
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_expire = atol(cp);
		_pw_passwd.pw_gecos = strsep(&bp, ":\n");
		_pw_passwd.pw_dir = strsep(&bp, ":\n");
		_pw_passwd.pw_shell = strsep(&bp, ":\n");
		return(1);
	}
	/* NOTREACHED */
}
