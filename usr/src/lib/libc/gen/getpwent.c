/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpwent.c	5.14 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

static DBM *_pw_db;
static FILE *_pw_fp;
static struct passwd _pw_passwd;
static int _pw_getfirstkey, _pw_stayopen;
static char _pw_flag, *_pw_file = _PATH_PASSWD, _pw_master;

#define	MAXLINELENGTH	1024
static char line[MAXLINELENGTH];

struct passwd *
getpwent()
{

	if (!_pw_fp && !start_pw(1))
		return((struct passwd *)NULL);
	if (!scanpw())
		return((struct passwd *)NULL);
	getpw();
	return(&_pw_passwd);
}

struct passwd *
getpwnam(nam)
	char *nam;
{
	int rval;

	if (!start_pw(0))
		return((struct passwd *)NULL);
	if (_pw_db) {
		datum key;

		key.dptr = nam;
		key.dsize = strlen(nam);
		rval = fetch_pw(key);
	} else /* _pw_fp */
		for (rval = 0; scanpw();)
			if (!strcmp(nam, _pw_passwd.pw_name)) {
				rval = 1;
				break;
			}
	if (!_pw_stayopen)
		endpwent();
	if (rval)
		getpw();
	return(rval ? &_pw_passwd : (struct passwd *)NULL);
}

struct passwd *
getpwuid(uid)
	int uid;
{
	int rval;

	if (!start_pw(0))
		return((struct passwd *)NULL);
	if (_pw_db) {
		datum key;

		key.dptr = (char *)&uid;
		key.dsize = sizeof(uid);
		rval = fetch_pw(key);
	} else /* _pw_fp */
		for (rval = 0; scanpw();)
			if (_pw_passwd.pw_uid == uid) {
				rval = 1;
				break;
			}
	if (!_pw_stayopen)
		endpwent();
	if (rval)
		getpw();
	return(rval ? &_pw_passwd : (struct passwd *)NULL);
}

static
start_pw(want_fp)
	char want_fp;		/* open _pw_fp also */
{
	char *p;

	if (_pw_db) {
		_pw_getfirstkey = 1;
		if (!want_fp)
			return(1);
	}
	if (_pw_fp) {
		rewind(_pw_fp);
		return(1);
	}
	if (!_pw_db && (_pw_db = dbm_open(_pw_file, O_RDONLY, 0))) {
		_pw_getfirstkey = 1;
		if (!want_fp)
			return(1);
	}
	/*
	 * special case; if it's the official password file, look in
	 * the master password file, otherwise, look in the file itself.
	 */
	p = strcmp(_pw_file, _PATH_PASSWD) ? _pw_file : _PATH_MASTERPASSWD;
	if (_pw_fp = fopen(p, "r")) {
		_pw_master = 1;
		return(1);
	}
	/*
	 * If we really want to set up _pw_fp, then try again
	 * with the old file.
	 */
	if (want_fp && p != _pw_file && (_pw_fp = fopen(_pw_file, "r"))) {
		_pw_master = 0;
		return(1);
	}
	return(0);
}

setpwent()
{
	return(setpassent(0));
}

setpassent(stayopen)
	int stayopen;
{
	if (!start_pw(0))
		return(0);
	_pw_stayopen = stayopen;
	return(1);
}

void
endpwent()
{
	if (_pw_db) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)NULL;
	}
	if (_pw_fp) {
		(void)fclose(_pw_fp);
		_pw_fp = (FILE *)NULL;
	}
}

void
setpwfile(file)
	char *file;
{
	_pw_file = file;
}

static
scanpw()
{
	register char *cp;
	long atol();
	char *bp;
	char *fgets(), *strsep(), *index();

	for (;;) {
		if (!(fgets(line, sizeof(line), _pw_fp)))
			return(0);
		bp = line;
		/* skip lines that are too big */
		if (!index(line, '\n')) {
			int ch;

			while ((ch = getc(_pw_fp)) != '\n' && ch != EOF)
				;
			continue;
		}
		_pw_passwd.pw_name = strsep(&bp, ":\n");
		_pw_passwd.pw_passwd = strsep(&bp, ":\n");
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_uid = atoi(cp);
		if (!(cp = strsep(&bp, ":\n")))
			continue;
		_pw_passwd.pw_gid = atoi(cp);
		if (_pw_master) {
			_pw_passwd.pw_class = strsep(&bp, ":\n");
			if (!(cp = strsep(&bp, ":\n")))
				continue;
			_pw_passwd.pw_change = atol(cp);
			if (!(cp = strsep(&bp, ":\n")))
				continue;
			_pw_passwd.pw_expire = atol(cp);
		}
		_pw_passwd.pw_gecos = strsep(&bp, ":\n");
		_pw_passwd.pw_dir = strsep(&bp, ":\n");
		_pw_passwd.pw_shell = strsep(&bp, ":\n");
		if (!_pw_passwd.pw_shell)
			continue;
		return(1);
	}
	/* NOTREACHED */
}

static
fetch_pw(key)
	datum key;
{
	register char *p, *t;

	/*
	 * the .dir file is LOCK_EX locked by programs that are
	 * renaming the various password files.
	 */
	if (flock(dbm_dirfno(_pw_db), LOCK_SH))
		return(0);
	if (!key.dptr)
		if (_pw_getfirstkey) {
			_pw_getfirstkey = 0;
			key = dbm_firstkey(_pw_db);
		} else
			key = dbm_nextkey(_pw_db);
	if (key.dptr)
		key = dbm_fetch(_pw_db, key);
	(void)flock(dbm_dirfno(_pw_db), LOCK_UN);
	if (!(p = key.dptr))
		return(0);
	t = line;
#define	EXPAND(e)	e = t; while (*t++ = *p++);
	EXPAND(_pw_passwd.pw_name);
	EXPAND(_pw_passwd.pw_passwd);
	bcopy(p, (char *)&_pw_passwd.pw_uid, sizeof(int));
	p += sizeof(int);
	bcopy(p, (char *)&_pw_passwd.pw_gid, sizeof(int));
	p += sizeof(int);
	bcopy(p, (char *)&_pw_passwd.pw_change, sizeof(time_t));
	p += sizeof(time_t);
	EXPAND(_pw_passwd.pw_class);
	EXPAND(_pw_passwd.pw_gecos);
	EXPAND(_pw_passwd.pw_dir);
	EXPAND(_pw_passwd.pw_shell);
	bcopy(p, (char *)&_pw_passwd.pw_expire, sizeof(time_t));
	p += sizeof(time_t);
	_pw_flag = *p;
	return(1);
}

#define	_MAX_PASSWD_SIZE	50
static char pwbuf[_MAX_PASSWD_SIZE];

static
getpw()
{
	long pos, atol();
	int fd, n;
	char *p;
	off_t lseek();

	if (geteuid())
		return;
	/*
	 * special case; if it's the official password file, look in
	 * the master password file, otherwise, look in the file itself.
	 */
	p = strcmp(_pw_file, _PATH_PASSWD) ? _pw_file : _PATH_MASTERPASSWD;
	if ((fd = open(p, O_RDONLY, 0)) < 0)
		return;
	pos = atol(_pw_passwd.pw_passwd);
	if (lseek(fd, pos, L_SET) != pos)
		goto bad;
	if ((n = read(fd, pwbuf, sizeof(pwbuf) - 1)) < 0)
		goto bad;
	pwbuf[n] = '\0';
	for (p = pwbuf; *p; ++p)
		if (*p == ':') {
			*p = '\0';
			_pw_passwd.pw_passwd = pwbuf;
			break;
		}
bad:	(void)close(fd);
}
