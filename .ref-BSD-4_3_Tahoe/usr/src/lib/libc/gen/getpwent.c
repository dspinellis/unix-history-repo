/*
 * Copyright (c) 1984 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getpwent.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1];
static struct passwd passwd;

/*
 * The following are shared with getpwnamuid.c
 */
char	*_pw_file = "/etc/passwd";
DBM	*_pw_db;
int	_pw_stayopen;

setpwent()
{
	if (pwf == NULL)
		pwf = fopen(_pw_file, "r");
	else
		rewind(pwf);
}

endpwent()
{
	if (pwf != NULL) {
		fclose(pwf);
		pwf = NULL;
	}
	if (_pw_db != (DBM *)0) {
		dbm_close(_pw_db);
		_pw_db = (DBM *)0;
		_pw_stayopen = 0;
	}
}

static char *
pwskip(p)
register char *p;
{
	while (*p && *p != ':' && *p != '\n')
		++p;
	if (*p)
		*p++ = 0;
	return(p);
}

struct passwd *
getpwent()
{
	register char *p;

	if (pwf == NULL) {
		if ((pwf = fopen( _pw_file, "r" )) == NULL)
			return(0);
	}
	p = fgets(line, BUFSIZ, pwf);
	if (p == NULL)
		return(0);
	passwd.pw_name = p;
	p = pwskip(p);
	passwd.pw_passwd = p;
	p = pwskip(p);
	passwd.pw_uid = atoi(p);
	p = pwskip(p);
	passwd.pw_gid = atoi(p);
	passwd.pw_quota = 0;
	passwd.pw_comment = EMPTY;
	p = pwskip(p);
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	while (*p && *p != '\n')
		p++;
	*p = '\0';
	return(&passwd);
}

setpwfile(file)
	char *file;
{
	_pw_file = file;
}
