/*
 * Copyright (c) 1988 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpwent.c	5.10 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

static DBM *_pw_db;
static FILE *_pw_fp;
static struct passwd _pw_passwd;
static int _pw_rewind = 1, _pw_stayopen;
static char _pw_flag, *_pw_file = _PATH_PASSWD;

#define	MAXLINELENGTH	1024
static char line[MAXLINELENGTH];

struct passwd *
getpwent()
{
	datum key;
	int rval;

	if (!_pw_db && !_pw_fp && !start_pw())
		return((struct passwd *)NULL);
	do {
		if (_pw_db) {
			key.dptr = NULL;
			rval = fetch_pw(key);
		} else /* _pw_fp */
			rval = scanpw();
	} while (rval && _pw_flag != _PW_KEYBYNAME);
	if (rval)
		getpw();
	return(rval ? &_pw_passwd : (struct passwd *)NULL);
}

struct passwd *
getpwnam(nam)
	char *nam;
{
	int rval;

	if (!start_pw())
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

	if (!start_pw())
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
start_pw()
{
	char *p;

	if (_pw_db) {
		_pw_rewind = 1;
		return(1);
	}
	if (_pw_fp) {
		rewind(_pw_fp);
		return(1);
	}
	if (_pw_db = dbm_open(_pw_file, O_RDONLY, 0))
		return(1);
	/*
	 * special case; if it's the official password file, look in
	 * the master password file, otherwise, look in the file itself.
	 */
	p = strcmp(_pw_file, _PATH_PASSWD) ? _pw_file : _PATH_MASTERPASSWD;
	if (_pw_fp = fopen(p, "r"))
		return(1);
	return(0);
}

setpwent()
{
	return(setpassent(0));
}

setpassent(stayopen)
	int stayopen;
{
	if (!start_pw())
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
	} else if (_pw_fp) {
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
	char *fgets(), *strsep(), *index();

	for (;;) {
		if (!(fgets(line, sizeof(line), _pw_fp)))
			return(0);
		/* skip lines that are too big */
		if (!index(line, '\n')) {
			int ch;

			while ((ch = getc(_pw_fp)) != '\n' && ch != EOF)
				;
			continue;
		}
		_pw_passwd.pw_name = strsep(line, ":\n");
		_pw_passwd.pw_passwd = strsep((char *)NULL, ":\n");
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_uid = atoi(cp);
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_gid = atoi(cp);
		_pw_passwd.pw_class = strsep((char *)NULL, ":\n");
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_change = atol(cp);
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_expire = atol(cp);
		_pw_passwd.pw_gecos = strsep((char *)NULL, ":\n");
		_pw_passwd.pw_dir = strsep((char *)NULL, ":\n");
		_pw_passwd.pw_shell = strsep((char *)NULL, ":\n");
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
		if (_pw_rewind) {
			_pw_rewind = 0;
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
