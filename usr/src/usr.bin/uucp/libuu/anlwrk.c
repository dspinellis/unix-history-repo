#ifndef lint
static char sccsid[] = "@(#)anlwrk.c	5.3 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "uust.h"
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif

/* Re-written to be reasonable
 * Mon Nov 15 17:19:52 EST 1982
 * Alan S. Watt (ittvax!swatt)
 *
 * Tom Truscott (rti!trt):
 * Priority ordering cleaned up.  New 'pcompar' subroutine.
 * 'stat' removed (speeds things up).
 * Possible infinite loop in gtwvec defended against.
 * Feb 23, 1983
 *
 * Changes:
 *
 *  1)	The check for work is much faster; the first filename
 *	that matches the prefix causes a "yes" return.
 *
 *  2)	The filename is not "stat" ed , so
 *	there is no massive delay while the list of potential
 *	names is built.
 *
 *  3)	Requesting work for a new system is now detected so
 *	internal variables are re-initialized properly.  In
 *	particular, the stream pointer for the current work
 *	file is properly closed so work for a system which
 *	hangs up will not be sent to the next system called.
 *
 * Fri Dec  3 09:31:45 EST 1982
 *
 *  5)	As new work files are requested, a check is made
 *	every TLIMIT seconds (5 minutes at present) to see
 *	if new files have entered the spool area.  Since
 *	work file names are now cached up to LLEN, this can
 *	represent a very long transmission time before new
 *	work enters the list to be processed.  If people want
 *	to use the "grade" character to specify a higher
 *	priority, the list must be re-built and re-sorted for
 *	higher priority stuff to have an immediate effect.
 */


#define TLIMIT	(5*60L)
#define NITEMS(X)	(sizeof (X) / sizeof ((X)[0]))

int Nfiles = 0;
char Filent[LLEN][NAMESIZE];

/*******
 *	anlwrk(file, wvec)	create a vector of command arguments
 *	char *file, **wvec;
 *
 *	return codes:
 *		0  -  no more work in this file
 *		positive number  -  number of arguments
 */

/* LOCAL only */
int
anlwrk(file, wvec)
register char *file, **wvec;
{
	static char str[MAXRQST];
	static FILE *fp = NULL;

	/*
	 * If called with a null string, force a shutdown
	 * of the current work file.
	 */
	if (file[0] == '\0') {
		if (fp != NULL)
			fclose (fp);
		fp = NULL;
		return 0;
	}
	if (fp == NULL) {
		fp = fopen(subfile(file), "r");
		if (fp == NULL) {
			unlink(subfile(file));
			return 0;
		}
		Usrf = 0;
	}

	/* This is what deletes the current work file when EOF
	 * is reached.  As this is called from gtwvec, which is
	 * in turn called externally, it is not possible to save
	 * "C." files in case of error, except for line errors,
	 * which shuts down the whole system.
	 */
	if (fgets(str, MAXRQST, fp) == NULL) {
		fclose(fp);
		unlink(subfile(file));
		USRF(USR_COMP);
		US_RRS(file, Usrf);
		Usrf = 0;
		file[0] = '\0';
		fp = NULL;
		return 0;
	}
	return getargs(str, wvec, 20);
}


/***
 *	bldflst - build list of work files for given system
 *	 Nfiles, Filent are global
 *
 *	return value - 1 if work was found, else 0
 *
 * Jul 26 19:17 1982 (ittvax!swatt). fixed this obnoxious
 * routine to NOT read all the way through the damned directory
 * "stat"'ing every file in sight just to get 10 names!!!
 *
 * It still reads through the directory from the beginning until
 * the list is filled, but this is only once every LLEN names.
 */

/* LOCAL only */
int
bldflst (reqst, dir, pre)
char *reqst;
register char *dir, *pre;
{
	static DIR  *dirp = NULL;
	register nfound;
	char filename[NAMESIZE];
	int plen = strlen (pre);
	int flen;
	extern char MaxGrade;

	if (dirp == NULL) {
		if ((dirp = opendir(subdir(dir,pre[0]))) == NULL) {
			DEBUG(1,"opendir(%s) FAILS\n",subdir(dir,pre[0]));
			return 0;
		}
	}
	else
		rewinddir(dirp);
	for (nfound = 0, Nfiles = 0; gnamef(dirp, filename);) {
		/* Check for two systems with the same prefix.
		 * Magic number "5" is 1 for "grade" character plus
		 * 4 for sequence number.  The point here is to not
		 * send work for a system which has as a prefix the
		 * name of the system called for.
		 * Special case: prefix "X." does not do this check
		 * so uuxqt can use bldflst.
		 */
		flen = strlen(filename);
		if (!prefix(pre, filename) || (plen != 2 && flen-plen != 5)) {
			DEBUG(99,"bldflst rejects %s\n",filename);
			continue;
		}
		if (filename[flen-5] > MaxGrade ) {
			DEBUG(8,"bldflst rejects %s, grade too low\n",filename);
			continue;
		}
		nfound++;
		if (*reqst == 'c')
			return 1;
		entflst(filename);
	}
	return  nfound? 1: 0;
}

/***
 *	entflst - put new name if list is not full
 *		  or new name is less than the MAX
 *		  now in the list.
 *	Nfiles, Filent[] are modified.
 *	return value - none
 *
 */

/* LOCAL only */
int
entflst(file)
register char *file;
{
	register int i;

	/* locate position for the new file and make room for it */
	for (i = Nfiles; i > 0; i--) {
		if (pcompar(file, Filent[i-1]) >= 0)
			break;
		if (i <LLEN)
			strcpy(Filent[i], Filent[i-1]);
	}

	/* add new file (if there is room), and increase Nfiles if need be */
	if (i < LLEN) {
		strcpy(Filent[i], file);
		if (Nfiles < LLEN)
			Nfiles++;
	}
}

/*
  Compare priority of filenames p1 and p2.  Return:
 *	< 0	if p1 "has lower priority than" p2.
 *	= 0	if p1 "has priority equal to" p2.
 *	> 0	if p1 "has greater priority than" p2.
 * Priority:
 *	lower grade wins.
 *	lower sequence number wins (unless wrap-around is suspected).
 *
 */
/* LOCAL only */
int
pcompar(p1, p2)
register char *p1, *p2;
{
	register int rc;

	/* assert: strlen(p1) and strlen(p2) are >= 5 */
	p1 += strlen(p1)-5;
	p2 += strlen(p2)-5;
	/* check 'grade' */
	if (rc = *p2++ - *p1++)
		return rc;
	/* check for  sequence wrap-around */
	if (rc = *p2++ - *p1++)
		if (rc < -10 || rc > 10)
			return -rc;
		else
			return rc;
	/* check remaining digits */
	return strcmp(p2, p1);
}

/***
 *	gtwrkf - get next work file
 *	 Nfiles, Filent[] are modified.
 *
 *	return value:
 *
 *		0  - No file gotten
 *		1  - File successfully gotten.
 *
 */

/* LOCAL only */
gtwrkf(dir, file)
char *file, *dir;
{
	if (Nfiles <= 0)
		return 0;
	sprintf(file, "%s/%s", dir, Filent[--Nfiles]);
	return 1;
}

/***
 *	gtwvec(file, dir, wkpre, wrkvec)	get work vector
 *	char *file, *dir, *wkpre, **wrkvec;
 *
 *	return codes:
 *		positive number  -  number of arguments
 *		0 -  no arguments - fail
 */

/* EXTERNALLY CALLED */
int
gtwvec(file, dir, wkpre, wrkvec)
char *dir, *wkpre, **wrkvec;
register char *file;
{
	register int nargs, n;

	n = 0;
	while ((nargs = anlwrk(file, wrkvec)) == 0) {
		if (++n > 3 || !iswrk(file, "get", dir, wkpre))
			return 0;
	}
	return nargs;
}

/***
 *	iswrk(file, reqst, dir, pre)
 *	char *file, *reqst, *dir, *pre;
 *
 *	iswrk  -  this routine will check the work list (list).
 *	If it is empty or the present work is exhausted, it
 *	will call bldflst to generate a new list.
 *	The "reqst" field will be the string "chk" or "get" to
 *	check for work, or get the next work file respectively.
 *
 *	return codes:
 *		0  -  no more work (or some error)
 *		1  -  there is work
 *
 */

/* EXTERNALLY CALLED */
int
iswrk(file, reqst, dir, pre)
register char *file, *reqst, *dir, *pre;
{
	static char *lastpre = 0;
	register ret;

	/* Starting new system; re-init */
	if (lastpre == 0 || strcmp(lastpre,pre) != 0) {
		anlwrk ("", (char **)0);	/* Force close of work file */

		/* Save last worked-on prefix */
		if (lastpre != 0)
			free (lastpre);
		lastpre = malloc((unsigned)(strlen(pre)+1));
		strcpy (lastpre, pre);

		/* Set the external indexes properly
		 */
		Nfiles = 0;
	}

	/* If the list is empty or new files have entered
	 * the spool area, call "bldflst" to read
	 * some file names into it.  Because names can
	 * be put in the list that later turn out to
	 * be unusable (from "gtwrkf"), this operation
	 * continues until either "bldflst" can't find
	 * any new files, or "gtwrkf" signals success.
	 */
	for (;;) {
		ret = 0;
		if (Nfiles == 0 || newspool((time_t)TLIMIT)) {
			ret = bldflst (reqst, dir, pre);
			DEBUG(99,"bldflst returns %d\n",ret);
		}

		/* If they only wanted to check, return
		 * boolean list not empty.  NB: the list
		 * will be forcibly emptied as soon as
		 * a new system name is mentioned.
		 */
		if (*reqst == 'c')
			return ret;

		if (Nfiles == 0)
			return 0;

		if (gtwrkf(dir, file))
			return 1;
	}
}

/* Return non-zero if there is new work in the spool
 * area since last check.  Assumes that if the sequence
 * file has been modified, there is new work. This is
 * not absolutely correct, but should be close enough.
 * Only checks every <limit> seconds at most.  Called
 * from "iswrk()" when a new work file is requested.
 */
/* LOCAL only */
int
newspool(limit)
time_t	limit;
{
	static time_t lastcheck = 0, lastmod = 0;
	time_t check;
	struct stat mod;
	register int ret = 0;

	/* (void) */ time (&check);
	if (check - lastcheck > limit || lastcheck - check > limit) {
		mod.st_mtime = 0;
		/* (void) */ stat (SEQFILE, &mod);
		if (mod.st_mtime != lastmod)
			ret = 1;
		lastmod = mod.st_mtime;
	}
	lastcheck = check;
	return ret;
}
