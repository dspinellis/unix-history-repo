/* Copyright (c) 1979 Regents of the University of California */
char	maillock[]	= ".lock";		/* Lock suffix for mailname */
char	locktmp[]	= "/usr/spool/mail/tmXXXXXX";
char	curlock[50];				/* Last used name of lock */
static	int		locked;			/* To note that we locked it */

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to remove the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, to check its modify time.  If it
 * is older than 200 seconds, we assume error and set our own file.
 * Otherwise, we wait for 5 seconds and try again.
 */

lock(file)
char *file;
{
	register int f;
	struct stat sbuf;
	long curtime;

	if (locked)
		return(0);
	strcpy(curlock, mailname);
	strcat(curlock, maillock);
	mktemp(locktmp);
	unlink(locktmp);
	for (;;) {
		f = lock1(locktmp, curlock);
		if (f == 0) {
			locked = 1;
			return(0);
		}
		stat(curlock, &sbuf);
		time(&curtime);
		if (curtime < sbuf.st_ctime + 200) {
			sleep(5);
			continue;
		}
		unlink(curlock);
		cnt++;
	}
	return(-1);
}

/*
 * Remove the mail lock, and note that we no longer
 * have it locked.
 */

unlock()
{

	unlink(curlock);
	locked = 0;
}

/*
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */

lock1(tempfile, name)
	char tempfile[], name[];
{
	register int fd;

	fd = creat(tempfile, 0);
	if (fd < 0)
		return(-1);
	if (link(tempfile, name) < 0) {
		unlink(tempfile);
		return(-1);
	}
	unlink(tempfile);
	close(fd);
	return(0);
}
