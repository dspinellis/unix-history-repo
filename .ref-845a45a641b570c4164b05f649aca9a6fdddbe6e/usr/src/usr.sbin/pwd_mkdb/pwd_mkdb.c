/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd_mkdb.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <db.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define	INSECURE	1
#define	SECURE		2
#define	PERM_INSECURE	(S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH)
#define	PERM_SECURE	(S_IRUSR|S_IWUSR)

HASHINFO openinfo = {
	4096,		/* bsize */
	32,		/* ffactor */
	256,		/* nelem */
	2048 * 1024,	/* cachesize */
	NULL,		/* hash() */
	0		/* lorder */
};

char *progname = "pwd_mkdb";

static enum state { FILE_INSECURE, FILE_SECURE, FILE_ORIG } clean;
static struct passwd pwd;			/* password structure */
static char *pname;				/* password file name */

void	cleanup __P((void));
void	error __P((char *));
void	mv __P((char *, char *));
int	scan __P((FILE *, struct passwd *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	register int len, makeold;
	register char *p, *t;
	DB *dp, *edp;
	DBT data, key;
	FILE *fp, *oldfp;
	sigset_t set;
	int ch, cnt, tfd;
	char buf[MAX(MAXPATHLEN, LINE_MAX * 2)], tbuf[1024];

	makeold = 0;
	while ((ch = getopt(argc, argv, "pv")) != EOF)
		switch(ch) {
		case 'p':			/* create V7 "file.orig" */
			makeold = 1;
			break;
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

	/*
	 * This could be changed to allow the user to interrupt.
	 * Probably not worth the effort.
	 */
	sigemptyset(&set);
	sigaddset(&set, SIGTSTP);
	sigaddset(&set, SIGHUP);
	sigaddset(&set, SIGINT);
	sigaddset(&set, SIGQUIT);
	sigaddset(&set, SIGTERM);
	(void)sigprocmask(SIG_BLOCK, &set, (sigset_t *)NULL);

	pname = *argv;
	/* Open the original password file */
	if (!(fp = fopen(pname, "r")))
		error(pname);

	/* Open the temporary insecure password database. */
	(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_MP_DB);
	dp = dbopen(buf,
	    O_RDWR|O_CREAT|O_EXCL, PERM_INSECURE, DB_HASH, &openinfo);
	if (dp == NULL)
		error(buf);
	clean = FILE_INSECURE;

	/*
	 * Open file for old password file.  Minor trickiness -- don't want to
	 * chance the file already existing, since someone (stupidly) might
	 * still be using this for permission checking.  So, open it first and
	 * fdopen the resulting fd.  Don't really care who reads it.
	 */
	if (makeold) {
		(void)snprintf(buf, sizeof(buf), "%s.orig", pname);
		if ((tfd = open(buf,
		    O_WRONLY|O_CREAT|O_EXCL, PERM_INSECURE)) < 0)
			error(buf);
		if ((oldfp = fdopen(tfd, "w")) == NULL)
			error(buf);
		clean = FILE_ORIG;
	}

	/*
	 * The databases actually contain three copies of the original data.
	 * Each password file entry is converted into a rough approximation
	 * of a ``struct passwd'', with the strings placed inline.  This
	 * object is then stored as the data for three separate keys.  The
	 * first key * is the pw_name field prepended by the _PW_KEYBYNAME
	 * character.  The second key is the pw_uid field prepended by the
	 * _PW_KEYBYUID character.  The third key is the line number in the
	 * original file prepended by the _PW_KEYBYNUM character.  (The special
	 * characters are prepended to ensure that the keys do not collide.)
	 */
	data.data = (u_char *)buf;
	key.data = (u_char *)tbuf;
	for (cnt = 1; scan(fp, &pwd); ++cnt) {
#define	COMPACT(e)	t = e; while (*p++ = *t++);
		/* Create insecure data. */
		p = buf;
		COMPACT(pwd.pw_name);
		COMPACT("*");
		memmove(p, &pwd.pw_uid, sizeof(int));
		p += sizeof(int);
		memmove(p, &pwd.pw_gid, sizeof(int));
		p += sizeof(int);
		memmove(p, &pwd.pw_change, sizeof(time_t));
		p += sizeof(time_t);
		COMPACT(pwd.pw_class);
		COMPACT(pwd.pw_gecos);
		COMPACT(pwd.pw_dir);
		COMPACT(pwd.pw_shell);
		memmove(p, &pwd.pw_expire, sizeof(time_t));
		p += sizeof(time_t);
		data.size = p - buf;

		/* Store insecure by name. */
		tbuf[0] = _PW_KEYBYNAME;
		len = strlen(pwd.pw_name);
		memmove(tbuf + 1, pwd.pw_name, len);
		key.size = len + 1;
		if ((dp->put)(dp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");

		/* Store insecure by number. */
		tbuf[0] = _PW_KEYBYNUM;
		memmove(tbuf + 1, &cnt, sizeof(cnt));
		key.size = sizeof(cnt) + 1;
		if ((dp->put)(dp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");

		/* Store insecure by uid. */
		tbuf[0] = _PW_KEYBYUID;
		memmove(tbuf + 1, &pwd.pw_uid, sizeof(pwd.pw_uid));
		key.size = sizeof(pwd.pw_uid) + 1;
		if ((dp->put)(dp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");

		/* Create original format password file entry */
		if (makeold)
			(void)fprintf(oldfp, "%s:*:%d:%d:%s:%s:%s\n",
			    pwd.pw_name, pwd.pw_uid, pwd.pw_gid, pwd.pw_gecos,
			    pwd.pw_dir, pwd.pw_shell);
	}
	(void)(dp->close)(dp);
	if (makeold) {
		(void)fflush(oldfp);
		(void)fclose(oldfp);
	}

	/* Open the temporary encrypted password database. */
	(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_SMP_DB);
	edp = dbopen(buf,
	    O_RDWR|O_CREAT|O_EXCL, PERM_SECURE, DB_HASH, &openinfo);
	if (!edp)
		error(buf);
	clean = FILE_SECURE;

	rewind(fp);
	for (cnt = 1; scan(fp, &pwd); ++cnt) {

		/* Create secure data. */
		p = buf;
		COMPACT(pwd.pw_name);
		COMPACT(pwd.pw_passwd);
		memmove(p, &pwd.pw_uid, sizeof(int));
		p += sizeof(int);
		memmove(p, &pwd.pw_gid, sizeof(int));
		p += sizeof(int);
		memmove(p, &pwd.pw_change, sizeof(time_t));
		p += sizeof(time_t);
		COMPACT(pwd.pw_class);
		COMPACT(pwd.pw_gecos);
		COMPACT(pwd.pw_dir);
		COMPACT(pwd.pw_shell);
		memmove(p, &pwd.pw_expire, sizeof(time_t));
		p += sizeof(time_t);
		data.size = p - buf;

		/* Store secure by name. */
		tbuf[0] = _PW_KEYBYNAME;
		len = strlen(pwd.pw_name);
		memmove(tbuf + 1, pwd.pw_name, len);
		key.size = len + 1;
		if ((dp->put)(edp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");

		/* Store secure by number. */
		tbuf[0] = _PW_KEYBYNUM;
		memmove(tbuf + 1, &cnt, sizeof(cnt));
		key.size = sizeof(cnt) + 1;
		if ((dp->put)(edp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");

		/* Store secure by uid. */
		tbuf[0] = _PW_KEYBYUID;
		memmove(tbuf + 1, &pwd.pw_uid, sizeof(pwd.pw_uid));
		key.size = sizeof(pwd.pw_uid) + 1;
		if ((dp->put)(edp, &key, &data, R_NOOVERWRITE) == -1)
			error("put");
	}

	(void)(edp->close)(edp);

	/* Set master.passwd permissions, in case caller forgot. */
	(void)fchmod(fileno(fp), S_IRUSR|S_IWUSR);
	(void)fclose(fp);

	/* Install as the real password files. */
	(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_MP_DB);
	mv(buf, _PATH_MP_DB);
	(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_SMP_DB);
	mv(buf, _PATH_SMP_DB);
	if (makeold) {
		(void)snprintf(buf, sizeof(buf), "%s.orig", pname);
		mv(buf, _PATH_PASSWD);
	}
	/*
	 * Move the master password LAST -- chpass(1), passwd(1) and vipw(8)
	 * all use flock(2) on it to block other incarnations of themselves.
	 * The rename means that everything is unlocked, as the original file
	 * can no longer be accessed.
	 */
	mv(pname, _PATH_MASTERPASSWD);
	exit(0);
}

int
scan(fp, pw)
	FILE *fp;
	struct passwd *pw;
{
	static int lcnt;
	static char line[LINE_MAX];
	char *p;

	if (!fgets(line, sizeof(line), fp))
		return(0);
	++lcnt;
	/*
	 * ``... if I swallow anything evil, put your fingers down my
	 * throat...''
	 *	-- The Who
	 */
	if (!(p = strchr(line, '\n'))) {
		(void)fprintf(stderr, "pwd_mkdb: line too long\n");
		goto fmt;

	}
	*p = '\0';
	if (!pw_scan(line, pw)) {
		(void)fprintf(stderr, "pwd_mkdb: at line #%d.\n", lcnt);
fmt:		errno = EFTYPE;
		error(pname);
	}
}

void
mv(from, to)
	char *from, *to;
{
	int sverrno;
	char buf[MAXPATHLEN];

	if (rename(from, to)) {
		sverrno = errno;
		(void)snprintf(buf, sizeof(buf), "%s to %s", from, to);
		errno = sverrno;
		error(buf);
	}
}

void
error(name)
	char *name;
{
	(void)fprintf(stderr, "pwd_mkdb: %s: %s\n", name, strerror(errno));
	cleanup();
	exit(1);
}

void
cleanup()
{
	char buf[MAXPATHLEN];

	switch(clean) {
	case FILE_ORIG:
		(void)snprintf(buf, sizeof(buf), "%s.orig", pname);
		(void)unlink(buf);
		/* FALLTHROUGH */
	case FILE_SECURE:
		(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_SMP_DB);
		(void)unlink(buf);
		/* FALLTHROUGH */
	case FILE_INSECURE:
		(void)snprintf(buf, sizeof(buf), "%s.tmp", _PATH_MP_DB);
		(void)unlink(buf);
	}
}

void
usage()
{
	(void)fprintf(stderr, "usage: pwd_mkdb [-p] file\n");
	exit(1);
}
