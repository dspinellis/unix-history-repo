/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpwent.c	5.17 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <fcntl.h>
#include <pwd.h>
#include <ndbm.h>
#include <utmp.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

static struct passwd _pw_passwd;	/* password structure */
static DBM *_pw_db;			/* password database */
static int _pw_keynum;			/* key counter */
static int _pw_stayopen;		/* keep fd's open */
static int __hashpw(), __initdb();

struct passwd *
getpwent()
{
	datum key;
	char bf[sizeof(_pw_keynum) + 1];

	if (!_pw_db && !__initdb())
		return((struct passwd *)NULL);

	++_pw_keynum;
	bf[0] = _PW_KEYBYNUM;
	bcopy((char *)&_pw_keynum, bf + 1, sizeof(_pw_keynum));
	key.dptr = bf;
	key.dsize = sizeof(_pw_keynum) + 1;
	return(__hashpw(key) ? &_pw_passwd : (struct passwd *)NULL);
}

struct passwd *
getpwnam(name)
	const char *name;
{
	datum key;
	int len, rval;
	char bf[UT_NAMESIZE + 1];

	if (!_pw_db && !__initdb())
		return((struct passwd *)NULL);

	bf[0] = _PW_KEYBYNAME;
	len = strlen(name);
	bcopy(name, bf + 1, MIN(len, UT_NAMESIZE));
	key.dptr = bf;
	key.dsize = len + 1;
	rval = __hashpw(key);

	if (!_pw_stayopen) {
		(void)dbm_close(_pw_db);
		_pw_db = (DBM *)NULL;
	}
	return(rval ? &_pw_passwd : (struct passwd *)NULL);
}

struct passwd *
#ifdef __STDC__
getpwuid(uid_t uid)
#else
getpwuid(uid)
	int uid;
#endif
{
	datum key;
	int keyuid, rval;
	char bf[sizeof(uid) + 1];

	if (!_pw_db && !__initdb())
		return((struct passwd *)NULL);

	bf[0] = _PW_KEYBYUID;
	keyuid = uid;
	bcopy(&keyuid, bf + 1, sizeof(keyuid));
	key.dptr = bf;
	key.dsize = sizeof(keyuid) + 1;
	rval = __hashpw(key);

	if (!_pw_stayopen) {
		(void)dbm_close(_pw_db);
		_pw_db = (DBM *)NULL;
	}
	return(rval ? &_pw_passwd : (struct passwd *)NULL);
}

int
setpassent(stayopen)
	int stayopen;
{
	_pw_keynum = 0;
	_pw_stayopen = stayopen;
	return(1);
}

int
setpwent()
{
	_pw_keynum = 0;
	_pw_stayopen = 0;
	return(1);
}

void
endpwent()
{
	_pw_keynum = 0;
	if (_pw_db) {
		(void)dbm_close(_pw_db);
		_pw_db = (DBM *)NULL;
	}
}

static
__initdb()
{
	static int warned;
	char *p;

	p = (geteuid()) ? _PATH_MP_DB : _PATH_SMP_DB;
	if (_pw_db = dbm_open(p, O_RDONLY, 0))
		return(1);
	if (!warned) {
		(void)write(STDERR_FILENO, "getpwent: ", 10);
		(void)write(STDERR_FILENO, p, strlen(p));
		(void)write(STDERR_FILENO, ": ", 2);
		p = strerror(errno);
		(void)write(STDERR_FILENO, p, strlen(p));
		(void)write(STDERR_FILENO, "\n", 1);
		warned = 1;
	}
	return(0);
}

static
__hashpw(key)
	datum key;
{
	register char *p, *t;
	static u_int max;
	static char *line;
	datum dp;

	dp = dbm_fetch(_pw_db, key);
	if (!(p = dp.dptr))
		return(0);
	if (dp.dsize > max && !(line = (char *)realloc(line, max += 1024)))
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
	return(1);
}
