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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpwent.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

static DBM *_pw_db;
static FILE *_pw_fp;
static struct passwd _pw_passwd;
static int _pw_set;
static char *_pw_file = "/etc/passwd";

#define	MAXLINELENGTH	200
static char line[MAXLINELENGTH];

static
pwscan()
{
	register char *cp;
	char *fgets(), *strsep();

	for (;;) {
		if (!(cp = fgets(line, sizeof(line), _pw_fp)))
			return(0);
		_pw_passwd.pw_name = strsep(cp, ":\n");
		_pw_passwd.pw_passwd = strsep((char *)NULL, ":\n");
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_uid = atoi(cp);
		if (!(cp = strsep((char *)NULL, ":\n")))
			continue;
		_pw_passwd.pw_gid = atoi(cp);
		_pw_passwd.pw_gecos = strsep((char *)NULL, ":\n");
		_pw_passwd.pw_dir = strsep((char *)NULL, ":\n");
		_pw_passwd.pw_shell = strsep((char *)NULL, ":\n");
		if (_pw_passwd.pw_shell) {
			_pw_passwd.pw_quota = 0;
			_pw_passwd.pw_comment = "";
			return(1);
		}
	}
	/* NOTREACHED */
}

static
fetchpw(key)
	datum key;
{
	register char *cp, *tp;

	if (_pw_db == (DBM *)NULL &&
	    (_pw_db = dbm_open(_pw_file, O_RDONLY, 0)) == (DBM *)NULL)
		return(0);
	if (flock(dbm_dirfno(_pw_db), LOCK_SH)) {
		dbm_close(_pw_db);
		_pw_db = NULL;
		return(0);
	}
	key = dbm_fetch(_pw_db, key);
	(void)flock(dbm_dirfno(_pw_db), LOCK_UN);
	if ((cp = key.dptr) == 0)
		return(0);

	tp = line;
#define	EXPAND(e)	_pw_passwd.e = tp; while (*tp++ = *cp++);
	EXPAND(pw_name);
	EXPAND(pw_passwd);
	bcopy(cp, (char *)&_pw_passwd.pw_uid, sizeof(int));
	cp += sizeof(int);
	bcopy(cp, (char *)&_pw_passwd.pw_gid, sizeof(int));
	cp += sizeof(int);
	bcopy(cp, (char *)&_pw_passwd.pw_quota, sizeof(int));
	cp += sizeof(int);
	EXPAND(pw_comment);
	EXPAND(pw_gecos);
	EXPAND(pw_dir);
	EXPAND(pw_shell);
	return(1);
}

struct passwd *
getpwent()
{
	if (!_pw_fp && !setpwent() || !pwscan())
		return((struct passwd *)NULL);
	return(&_pw_passwd);
}

struct passwd *
getpwnam(nam)
	char *nam;
{
	datum key;

	key.dptr = nam;
	key.dsize = strlen(nam);
	if (!fetchpw(key)) {
		if (setpwent())
			while (pwscan())
				if (!strcmp(nam, _pw_passwd.pw_name))
					return(&_pw_passwd);
		return((struct passwd *)NULL);
	}
	return(&_pw_passwd);
}

struct passwd *
getpwuid(uid)
	uid_t uid;
{
	datum key;

	key.dptr = (char *)&uid;
	key.dsize = sizeof(uid);
	if (!fetchpw(key)) {
		if (setpwent())
			while (pwscan())
				if (_pw_passwd.pw_uid == uid)
					return(&_pw_passwd);
		return((struct passwd *)NULL);
	}
	return(&_pw_passwd);
}

void
setpwfile(file)
	char *file;
{
	_pw_file = file;
	_pw_set = 1;
}

setpwent()
{
	if (_pw_set)
		endpwent();
	if (_pw_fp)
		rewind(_pw_fp);
	else if ((_pw_fp = fopen(_pw_file, "r")) == NULL)
		return(0);
	_pw_set = 0;
	return(1);
}

void
endpwent()
{
	if (_pw_fp) {
		(void)fclose(_pw_fp);
		_pw_fp = (FILE *)NULL;
	}
	if (_pw_db) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)NULL;
	}
}
