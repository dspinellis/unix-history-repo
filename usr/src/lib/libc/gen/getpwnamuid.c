/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getpwnamuid.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

#include <sys/file.h>

static char line[BUFSIZ+1];
static struct passwd passwd;

/*
 * The following are shared with getpwent.c
 */
extern	char *_pw_file;
DBM	*_pw_db;
int	_pw_stayopen;

static struct passwd *
fetchpw(key)
	datum key;
{
        register char *cp, *tp;

        if (key.dptr == 0)
                return ((struct passwd *)NULL);
	key = dbm_fetch(_pw_db, key);
	if (key.dptr == 0)
                return ((struct passwd *)NULL);
        cp = key.dptr;
	tp = line;

#define	EXPAND(e)	passwd.pw_/**/e = tp; while (*tp++ = *cp++);
	EXPAND(name);
	EXPAND(passwd);
	bcopy(cp, (char *)&passwd.pw_uid, sizeof (int));
	cp += sizeof (int);
	bcopy(cp, (char *)&passwd.pw_gid, sizeof (int));
	cp += sizeof (int);
	bcopy(cp, (char *)&passwd.pw_quota, sizeof (int));
	cp += sizeof (int);
	EXPAND(comment);
	EXPAND(gecos);
	EXPAND(dir);
	EXPAND(shell);
        return (&passwd);
}

struct passwd *
getpwnam(nam)
	char *nam;
{
        datum key;
	register struct passwd *pw;

        if (_pw_db == (DBM *)0 &&
	    (_pw_db = dbm_open(_pw_file, O_RDONLY)) == (DBM *)0) {
	oldcode:
		setpwent();
		while ((pw = getpwent()) && strcmp(nam, pw->pw_name))
			;
		if (!_pw_stayopen)
			endpwent();
		return (pw);
	}
	if (flock(dbm_dirfno(_pw_db), LOCK_SH) < 0) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)0;
		goto oldcode;
	}
        key.dptr = nam;
        key.dsize = strlen(nam);
	pw = fetchpw(key);
	(void) flock(dbm_dirfno(_pw_db), LOCK_UN);
	if (!_pw_stayopen) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)0;
	}
        return (pw);
}

struct passwd *
getpwuid(uid)
	int uid;
{
        datum key;
	register struct passwd *pw;

        if (_pw_db == (DBM *)0 &&
	    (_pw_db = dbm_open(_pw_file, O_RDONLY)) == (DBM *)0) {
	oldcode:
		setpwent();
		while ((pw = getpwent()) && pw->pw_uid != uid)
			;
		if (!_pw_stayopen)
			endpwent();
		return (pw);
	}
	if (flock(dbm_dirfno(_pw_db), LOCK_SH) < 0) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)0;
		goto oldcode;
	}
        key.dptr = (char *) &uid;
        key.dsize = sizeof uid;
	pw = fetchpw(key);
	(void) flock(dbm_dirfno(_pw_db), LOCK_UN);
	if (!_pw_stayopen) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)0;
	}
        return (pw);
}
