/*	@(#)subr.c	1.3 90/01/03 NFS Rev 2 Testsuite
 * 	1.6 Lachman ONC Test Suite source
 *
 * Useful subroutines shared by all tests
 */

#include <sys/param.h>
#ifndef major
#include <sys/types.h>
#endif
#ifdef SVR3
#include <sys/sysmacros.h>
#include <sys/fs/nfs/time.h>
#else
#include <sys/time.h>
#endif
#include <sys/stat.h>
#include <stdio.h>
#include "tests.h"

char *Myname;

/*
 * Build a directory tree "lev" levels deep
 * with "files" number of files in each directory
 * and "dirs" fan out.  Starts at the current directory.
 * "fname" and "dname" are the base of the names used for
 * files and directories.
 */
dirtree(lev, files, dirs, fname, dname, totfiles, totdirs)
	int lev;
	int files;
	int dirs;
	char *fname;
	char *dname;
	int *totfiles;
	int *totdirs;
{
	int fd;
	int f, d;
	char name[MAXPATHLEN];

	if (lev-- == 0) {
		return;
	}
	for ( f = 0; f < files; f++) {
		sprintf(name, "%s%d", fname, f);
		if ((fd = creat(name, 0666)) < 0) {
			error("creat %s failed", name);
			exit(1);
		}
		(*totfiles)++;
		if (close(fd) < 0) {
			error("close %d failed", fd);
			exit(1);
		}
	}
	for ( d = 0; d < dirs; d++) {
		sprintf(name, "%s%d", dname, d);
		if (mkdir(name, 0777) < 0) {
			error("mkdir %s failed", name);
			exit(1);
		}
		(*totdirs)++;
		if (chdir(name) < 0) {
			error("chdir %s failed", name);
			exit(1);
		}
		dirtree(lev, files, dirs, fname, dname, totfiles, totdirs);
		if (chdir("..") < 0) {
			error("chdir .. failed");
			exit(1);
		}
	}
}

/*
 * Remove a directory tree starting at the current directory.
 * "fname" and "dname" are the base of the names used for
 * files and directories to be removed - don't remove anything else!
 * "files" and "dirs" are used with fname and dname to generate
 * the file names to remove.
 *
 * This routine will fail if, say after removing known files,
 * the directory is not empty.
 *
 * This is used to test the unlink function and to clean up after tests.
 */
rmdirtree(lev, files, dirs, fname, dname, totfiles, totdirs, ignore)
	int lev;
	int files;
	int dirs;
	char *fname;
	char *dname;
	int *totfiles;		/* total removed */
	int *totdirs;		/* total removed */
	int ignore;
{
	int f, d;
	char name[MAXPATHLEN];

	if (lev-- == 0) {
		return;
	}
	for ( f = 0; f < files; f++) {
		sprintf(name, "%s%d", fname, f);
		if (unlink(name) < 0 && !ignore) {
			error("unlink %s failed", name);
			exit(1);
		}
		(*totfiles)++;
	}
	for ( d = 0; d < dirs; d++) {
		sprintf(name, "%s%d", dname, d);
		if (chdir(name) < 0) {
			if (ignore)
				continue;
			error("chdir %s failed", name);
			exit(1);
		}
		rmdirtree(lev, files, dirs, fname, dname, totfiles, totdirs, ignore);
		if (chdir("..") < 0) {
			error("chdir .. failed");
			exit(1);
		}
		if (rmdir(name) < 0) {
			error("rmdir %s failed", name);
			exit(1);
		}
		(*totdirs)++;
	}
}

/* VARARGS */
error(str, ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9)
	char *str;
{
#ifdef SVR3
	char *ret, *getcwd(), path[MAXPATHLEN];
#else
	char *ret, *getwd(), path[MAXPATHLEN];
#endif

#ifdef SVR3
	if ((ret = getcwd(path, sizeof(path))) == NULL)
		fprintf(stderr, "%s: getcwd failed\n", Myname);
	else
		fprintf(stderr, "\t%s: (%s) ", Myname, path);
#else
	if ((ret = getwd(path)) == NULL)
		fprintf(stderr, "%s: getwd failed\n", Myname);
	else
		fprintf(stderr, "\t%s: (%s) ", Myname, path);
#endif

	fprintf(stderr, str, ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9);
	if (errno)
		perror(" ");
	else
		fprintf(stderr, "\n");
	fflush(stderr);
	if (ret == NULL)
		exit(1);
}

static struct timeval ts, te;

/*
 * save current time in struct ts
 */
starttime()
{

	gettimeofday(&ts, (struct timezone *)0);
}

/*
 * sets the struct tv to the difference in time between
 * current time and the time in struct ts.
 */
endtime(tv)
	struct timeval *tv;
{

	gettimeofday(&te, (struct timezone *)0);
	if (te.tv_usec < ts.tv_usec) {
		te.tv_sec--;
		te.tv_usec += 1000000;
	}
	tv->tv_usec = te.tv_usec - ts.tv_usec;
	tv->tv_sec = te.tv_sec - ts.tv_sec;
}

/*
 * Set up and move to a test directory
 */
testdir(dir)
	char *dir;
{
	struct stat statb;
	char str[MAXPATHLEN];
	extern char *getenv();

	/*
	 *  If dir is non-NULL, use that dir.  If NULL, first
	 *  check for env variable NFSTESTDIR.  If that is not
	 *  set, use the compiled-in TESTDIR.
	 */
	if (dir == NULL)
		if ((dir = getenv("NFSTESTDIR")) == NULL)
			dir = TESTDIR;

	if (stat(dir, &statb) == 0) {
		sprintf(str, "rm -r %s", dir);
		if (system(str) != 0) {
			error("can't remove old test directory %s", dir);
			exit(1);
		}
	}

	if (mkdir(dir, 0777) < 0) {
		error("can't create test directory %s", dir);
		exit(1);
	}
	if (chdir(dir) < 0) {
		error("can't chdir to test directory %s", dir);
		exit(1);
	}
}

/*
 * Move to a test directory
 */
mtestdir(dir)
	char *dir;
{
	struct stat statb;
	char str[MAXPATHLEN];
	char *getenv();

	/*
	 *  If dir is non-NULL, use that dir.  If NULL, first
	 *  check for env variable NFSTESTDIR.  If that is not
	 *  set, use the compiled-in TESTDIR.
	 */
	if (dir == NULL)
		if ((dir = getenv("NFSTESTDIR")) == NULL)
			dir = TESTDIR;

	if (chdir(dir) < 0) {
		error("can't chdir to test directory %s", dir);
		return(-1);
	}
	return(0);
}

/*
 *  get parameter at parm, convert to int, and make sure that
 *  it is at least min.
 */
getparm(parm, minimum, label)
	char *parm, *label;
	int minimum;
{
	int val;
	
	val = atoi(parm);
	if (val < minimum) {
		error("Illegal %s parameter %d, must be at least %d",
		      label, val, minimum);
		exit(1);
	}
	return(val);
}

/*
 *  exit point for successful test
 */
complete()
{

	fprintf(stdout, "\t%s ok.\n", Myname);
	exit(0);
}
