#ifndef lint
static char sccsid[] = "@(#)ulockf.c	5.7	(Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/stat.h>
#include <errno.h>

#define	LCKMODE	0444	/* File mode for lock files */
#define MAXLOCKS 16	/* Maximum number of lock files */

char *Lockfile[MAXLOCKS];
char *LockDirectory = LOCKDIR;
int Nlocks = 0;
extern int errno;

/*LINTLIBRARY*/

/*
 *	This routine will attempt to create a lock file (file).
 *	It makes sure that the lock file is valid if it already exists.
 *
 *	return codes:  SUCCESS  |  FAIL
 */
ulockf(hfile, atime)
char *hfile;
time_t atime;
{
	register char *p;
	register int i;
	static char tempfile[NAMESIZE];
	char file[NAMESIZE];
	static int pid = -1;

	if (pid < 0) {
		pid = getpid();
		sprintf(tempfile, "%s/LTMP.%d", LockDirectory, pid);
	}
	sprintf(file, "%s/LCK..%s", LockDirectory, hfile);
	i = 0;
	while (onelock(pid, tempfile, file) == -1) { /* lock file exists */
#if !defined(BSD4_2) && !defined(USG)
		struct stat stbuf;
		time_t ptime;
		/* get status to check age of the lock file */
		if (stat(file, &stbuf) == 0) {
			(void) time(&ptime);
			if ((ptime - stbuf.st_ctime) < atime)
				return FAIL; /* file not old enough to delete */
		}
#else	BSD4_2 || USG
		register int fd;
		fd = open(file, 0);
		if (fd >= 0) {
			int upid, ret;
			ret = read(fd, &upid, sizeof upid);
			close(fd);
			if (ret == sizeof upid && (kill(upid, 0) == 0
				|| errno != ESRCH))
				return FAIL; /* process is still running */
		}
#endif BSD4_2 || USG
		syslog(LOG_WARNING, "%s: dead lock %s", Rmtname, file);
		logent(file, "DEAD LOCK");
		(void) unlink(file);
		sleep(5);	/* avoid a possible race */
		if (i++ >= 5) {
			syslog(LOG_ERR, "%s: can't get lockfile %s: %m",
				Rmtname, tempfile);
			cleanup(FAIL);
		}
	}

	for (i = 0; i < Nlocks; i++) {
		if (Lockfile[i] == NULL)
			break;
	}
	if (i >= MAXLOCKS) {
		syslog(LOG_ERR, "Too many locks");
		cleanup(FAIL);
	}
	if (i >= Nlocks)
		i = Nlocks++;
	p = malloc((unsigned)(strlen(file)+1));
	if (p == NULL) {
		syslog(LOG_ERR, "malloc failed: %m");
		cleanup(FAIL);
	}
	strcpy(p, file);
	Lockfile[i] = p;

	return SUCCESS;
}

/*
 *	remove all lock files in list or name
 */
rmlock(name)
register char *name;
{
	register int i;
	char file[MAXFULLNAME];

	if (name != NULL) {
		sprintf(file, "%s/LCK..%s", LockDirectory, name);
		name = file;
	}
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
 *	makes lock a name on behalf of pid. Tempfile must be in the same
 *	file system as name.
 */
onelock(pid, tempfile, name)
int pid;
char *tempfile, *name;
{
	register int fd, ret;
#ifdef VMS
	fd = creat(name, LCKMODE, "1version");
#else !VMS
	fd = creat(tempfile, LCKMODE);
#endif !VMS
	if (fd < 0) {
		DEBUG(1,"Can't creat temp file %s ", tempfile);
		DEBUG(1,"-- errno %d", errno);
		return FAIL;
	}
	ret = write(fd, (char *)&pid, sizeof(int));
	(void) close(fd);

	if (ret != sizeof(int)) {
		DEBUG(1,"Temp file write failed -- errno %d\n", errno);
#ifdef VMS
		(void) unlink(name);
#else !VMS
		(void) unlink(tempfile);
#endif !VMS
		return FAIL;
	}
#ifndef VMS
	if (link(tempfile, name) < 0) {
		(void) unlink(tempfile);
		return FAIL;
	}
	unlink(tempfile);
#endif	!VMS
	return SUCCESS;
}

#if !defined(BSD4_2) && !defined(USG)
/*
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
	static time_t lasttouch = 0;
	register int i;
	struct ut {
		time_t actime;
		time_t modtime;
	} ut;

#ifdef USG
	time(&Now.time);
	t1.millitm = 0;
#else !USG
	ftime(&Now);
#endif !USG
	ut.actime = ut.modtime = Now.time;
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
	}
}
#endif !BSD4_2 && ! USG
