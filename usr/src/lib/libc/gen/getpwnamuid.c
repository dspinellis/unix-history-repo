/*	getpwnamuid.c	4.4	84/04/26	*/

#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

#include <sys/file.h>

static char PASSWD[] = "/etc/passwd";
static char EMPTY[] = "";
static char line[BUFSIZ+1];
static struct passwd passwd;
DBM *_pw_db = 0;
int _pw_stayopen = 0;

static struct passwd *
fetchpw(key)
	datum key;
{
        register char *cp, *tp;

        if (key.dptr == 0)
                return ((struct passwd *)NULL);
	key = dbmfetch(_pw_db, key);
	if (key.dptr == 0)
                return ((struct passwd *)NULL);
        cp = key.dptr;
	tp = line;

#define	EXPAND(e)	passwd.pw_/**/e = tp; while (*tp++ = *cp++);
	EXPAND(name);
	EXPAND(passwd);
	passwd.pw_uid = *(int *)cp; cp += sizeof (int);
	passwd.pw_gid = *(int *)cp; cp += sizeof (int);
	passwd.pw_quota = *(int *)cp; cp += sizeof (int);
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
	    (_pw_db = ndbmopen(PASSWD, O_RDONLY)) == (DBM *)0) {
	oldcode:
		setpwent();
		while ((pw = getpwent()) && strcmp(nam, pw->pw_name));
		endpwent();
		return (pw);
	}
	if (flock(_pw_db->db_dirf, LOCK_SH) < 0) {
		ndbmclose(_pw_db);
		_pw_db = (DBM *)0;
		goto oldcode;
	}
        key.dptr = nam;
        key.dsize = strlen(nam);
	pw = fetchpw(key);
	(void) flock(_pw_db->db_dirf, LOCK_UN);
	if (!_pw_stayopen) {
		ndbmclose(_pw_db);
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
	    (_pw_db = ndbmopen(PASSWD, O_RDONLY)) == (DBM *)0) {
	oldcode:
		setpwent();
		while ((pw = getpwent()) && pw->pw_uid != uid);
		endpwent();
		return (pw);
	}
	if (flock(_pw_db->db_dirf, LOCK_SH) < 0) {
		ndbmclose(_pw_db);
		_pw_db = (DBM *)0;
		goto oldcode;
	}
        key.dptr = (char *) &uid;
        key.dsize = sizeof uid;
	pw = fetchpw(key);
	(void) flock(_pw_db->db_dirf, LOCK_UN);
	if (!_pw_stayopen) {
		ndbmclose(_pw_db);
		_pw_db = (DBM *)0;
	}
        return (pw);
}
