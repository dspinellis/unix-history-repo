# include	"../hdr/macros.h"
# include	"errno.h"
SCCSID(@(#)lockit	2.1);
/*
	Process semaphore.
	Try repeatedly (`count' times) to create `lockfile' mode 444.
	Sleep 10 seconds between tries.
	If `lockfile' is successfully created, write the process ID
	`pid' in `lockfile' (in binary), and return 0.
	If `lockfile' exists and it hasn't been modified within the last
	minute, and either the file is empty or the process ID contained
	in the file is not the process ID of any existing process,
	`lockfile' is removed and it tries again to make `lockfile'.
	After `count' tries, or if the reason for the create failing
	is something other than EACCES, return xmsg().
 
	Unlockit will return 0 if the named lock exists, contains
	the given pid, and is successfully removed; -1 otherwise.
*/


lockit(lockfile,count,pid)
register char *lockfile;
register unsigned count;
unsigned pid;
{
	register int fd;
	unsigned opid;
	long ltime;
	extern int errno;

	for (++count; --count; sleep(10)) {
		if ((fd=creat(lockfile,0444)) >= 0) {
			write(fd,&pid,sizeof(pid));
			close(fd);
			return(0);
		}
		if (errno == ENFILE) {
			unlink(lockfile);
			++count;
			continue;
		}
		if (errno != EACCES)
			return(xmsg(lockfile,"lockit"));
		if (exists(lockfile)) {
			time(&ltime);
			ltime -= Statbuf.st_mtime;
			if ((fd = open(lockfile,0)) < 0)
				continue;
			if (ltime < 60L)
				sleep(60);
			if (read(fd,&opid,sizeof(opid)) != sizeof(opid)) {
				close(fd);
				unlink(lockfile);
				continue;
			}
			close(fd);
			if (kill(opid,0) == -1 && errno == ESRCH) {
				unlink(lockfile);
				continue;
			}
		}
	}
	return(-1);
}


unlockit(lockfile,pid)
register char *lockfile;
unsigned pid;
{
	register int fd, n;
	unsigned opid;

	if ((fd = open(lockfile,0)) < 0)
		return(-1);
	n = read(fd,&opid,sizeof(opid));
	close(fd);
	if (n == sizeof(opid) && opid == pid)
		return(unlink(lockfile));
	else
		return(-1);
}
