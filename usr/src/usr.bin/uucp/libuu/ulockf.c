#ifndef lint
static char sccsid[] = "@(#)ulockf.c	5.4 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/stat.h>

/* File mode for lock files */
#define	LCKMODE	0444

/*LINTLIBRARY*/

/*
 *	this routine will create a lock file (file).
 *	If one already exists, the create time is checked for
 *	older than the age time (atime).
 *	If it is older, an attempt will be made to unlink it
 *	and create a new one.
 *
 *	return codes:  SUCCESS  |  FAIL
 */

ulockf(hfile, atime)
char *hfile;
time_t atime;
{
	struct stat stbuf;
	time_t ptime;
	register int ret;
	static int pid = -1;
	static char tempfile[NAMESIZE];
	char file[NAMESIZE];

	if (pid < 0) {
		pid = getpid();
		sprintf(tempfile, "%s/LTMP.%d", LOCKDIR, pid);
	}
	sprintf(file, "%s/%s", LOCKDIR, hfile);
	if (onelock(pid, tempfile, file) == -1) {
		/* lock file exists */
		/* get status to check age of the lock file */
		ret = stat(file, &stbuf);
		if (ret != -1) {
			time(&ptime);
			if ((ptime - stbuf.st_ctime) < atime) {
				/* file not old enough to delete */
				return FAIL;
			}
			ret = unlink(file);
			logent(file, "DEAD LOCK");
			sleep(5);	/* rti!trt: avoid a race */
			ret = onelock(pid, tempfile, file);
		}
		if (ret != 0)
			return FAIL;
	}
	stlock(file);
	return SUCCESS;
}


#define MAXLOCKS 10	/* maximum number of lock files */
char *Lockfile[MAXLOCKS];
int Nlocks = 0;

/***
 *	stlock(name)	put name in list of lock files
 *	char *name;
 *
 *	return codes:  none
 */

stlock(name)
register char *name;
{
	register char *p;
	register int i;

	for (i = 0; i < Nlocks; i++) {
		if (Lockfile[i] == NULL)
			break;
	}
	ASSERT(i < MAXLOCKS, "TOO MANY LOCKS", CNULL, i);
	if (i >= Nlocks)
		i = Nlocks++;
	p = calloc((unsigned)(strlen(name)+1), sizeof (char));
	ASSERT(p != NULL, "CAN NOT ALLOCATE FOR", name, 0);
	strcpy(p, name);
	Lockfile[i] = p;
}


/*
 *	remove all lock files in list *	or name
 *
 *	return codes: none
 */

rmlock(name)
register char *name;
{
	register int i;

	for (i = 0; i < Nlocks; i++) {
		if (Lockfile[i] == NULL)
			continue;
		if (name == NULL || strcmp(name, Lockfile[i]) == SAME) {
			unlink(Lockfile[i]);
			free(Lockfile[i]);
			Lockfile[i] = NULL;
		}
	}
}

/*
 *	isalock(name) returns 0 if the name is a lock.
 *	unlock(name)  unlocks name if it is a lock.
 *	onelock(pid,tempfile,name) makes lock a name
 *	on behalf of pid.  Tempfile must be in the same
 *	file system as name.
 *	lock(pid,tempfile,names) either locks all the
 *	names or none of them.
 */
isalock(name)
char *name;
{
	struct stat xstat;
	if (stat(name,&xstat) < 0)
		return 0;
	if (xstat.st_size != sizeof(int))
		return 0;
	return 1;
}
unlock(name)
char *name;
{
	if (isalock(name))
		return unlink(name);
	else
		return -1;
}
onelock(pid, tempfile, name)
int pid;
char *tempfile,*name;
{
	register int fd;
#ifdef VMS
	fd = creat(name, LCKMODE, "1version");
#else !VMS
	fd = creat(tempfile, LCKMODE);
#endif !VMS
	if (fd < 0)
		return FAIL;
	write(fd, (char *)&pid, sizeof(int));
	close(fd);
#ifndef	VMS
	if (link(tempfile, name) < 0) {
		unlink(tempfile);
		return FAIL;
	}
	unlink(tempfile);
#endif
	return SUCCESS;
}


lock(pid, tempfile, names)
char *tempfile;
register char **names;
{
	register int i, j;

	for(i=0; names[i] != 0; i++) {
		if (onelock(pid, tempfile, names[i]) == 0)
			continue;
		for(j=0; j < i ;j++)
			unlink(names[j]);
		return FAIL;
	}
	return SUCCESS;
}

#define LOCKPRE "LCK."

/*
 *	remove a lock file
 */
delock(s)
char *s;
{
	char ln[NAMESIZE];

	sprintf(ln, "%s/%s.%s", LOCKDIR, LOCKPRE, s);
	rmlock(ln);
}

/*
 *	create system lock
 *
 *	return codes:  SUCCESS  |  FAIL
 */
mlock(sys)
char *sys;
{
	char lname[NAMESIZE];

	sprintf(lname, "%s.%s", LOCKPRE, sys);
	return ulockf(lname, (time_t) SLCKTIME ) < 0 ? FAIL : SUCCESS;
}

/***
 *	update 'change' time for lock files
 *
 *	Only update ctime, not mtime or atime.
 *	The 'chmod' method permits cu(I)-like programs
 *	to determine how long uucp has been on the line.
 *	The old "change access, mod, and change time" method
 *	can be had by defining OLDTOUCH
 *
 *	return code - none
 */

ultouch()
{
	time_t time();
	static time_t lasttouch = 0;
	register int i;
	struct ut {
		time_t actime;
		time_t modtime;
	} ut;

	ut.actime = time(&ut.modtime);
	/* Do not waste time touching locking files too often */
	/* (But, defend against backward time changes) */
	if (ut.actime >= lasttouch && ut.actime < lasttouch+60)
		return;
	lasttouch = ut.actime;
	DEBUG(4, "ultouch\n", 0);

	for (i = 0; i < Nlocks; i++) {
		if (Lockfile[i] == NULL)
			continue;
#ifdef	OLDTOUCH
		utime(Lockfile[i], &ut);
#else 	!OLDTOUCH
		chmod(Lockfile[i], LCKMODE);
#endif !OLDTOUCH
	/*
	 * set 'nologinflag' if the file /etc/nologin exists.
	 * This permits graceful shutdown of uucp.
	 */
	if (nologinflag == 0 && access(NOLOGIN, 0) == 0)
		nologinflag = 1;
	}
}
