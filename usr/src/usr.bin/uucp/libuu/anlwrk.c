#ifndef lint
static char sccsid[] = "@(#)anlwrk.c	5.6 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/stat.h>
#include "uust.h"
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif
#include <ctype.h>

#define TLIMIT	(15*60L)
#define NITEMS(X)	(sizeof (X) / sizeof ((X)[0]))

int Nfiles = 0;
char Filent[LLEN][NAMESIZE];
long fseek(), ftell();
extern int TransferSucceeded;

/*LINTLIBRARY*/

/*
 *	create a vector of command arguments
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
	static char str[MAXRQST], nstr[MAXRQST], lastfile[MAXFULLNAME] = "";
	static FILE *fp = NULL;
	static long nextread, nextwrite;

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
		if (strncmp(file, lastfile, MAXFULLNAME) == 0) { 
			DEBUG(5,"Workfilename repeated: %s\n", file);
			return 0;
		}
		strncpy(lastfile, file, MAXFULLNAME);
		fp = fopen(subfile(file), "r+w");
		if (fp == NULL) {
			char *bnp, rqstr[MAXFULLNAME];
			bnp = rindex(file, '/');
			sprintf(rqstr, "%s/%s", CORRUPT, bnp ? bnp + 1 : file);
			xmv(file, rqstr);
			assert("CMD FILE UNREADABLE", subfile(file), 0);
			unlink(subfile(file));
			return 0;
		}
		Usrf = 0;
		nstr[0] = '\0';
		nextread = nextwrite = 0L;
	}

	if (nstr[0] != '\0' && TransferSucceeded) {
		fseek(fp, nextwrite, 0);
		fputs(nstr, fp);
		fseek(fp, nextread, 0);
	}

	do {
		nextwrite = ftell(fp);
		if (fgets(str, MAXRQST, fp) == NULL) {
			fclose(fp);
			if (TransferSucceeded)
				unlink(subfile(file));
			USRF(USR_COMP);
			US_RRS(file, Usrf);
			Usrf = 0;
			file[0] = '\0';
			nstr[0] = '\0';
			fp = NULL;
			return 0;
		}
	} while (!isupper(str[0]));

	nextread = ftell(fp);
	strncpy(nstr, str, MAXRQST);
	nstr[0] = tolower(nstr[0]);
	return getargs(str, wvec, 20);
}


/*
 *	build list of work files for given system
 *
 *	return value - 1 if work was found, else 0
 *
 */

/* LOCAL only */
int
bldflst (reqst, dir, pre)
char *reqst;
register char *dir, *pre;
{
	static DIR  *dirp = NULL;
	register struct direct *dentp;
	register int i;
	int plen = strlen(pre);
	extern char MaxGrade;

	if (dirp == NULL) {
		if ((dirp = opendir(subdir(dir,pre[0]))) == NULL) {
			DEBUG(1,"opendir(%s) FAILS\n",subdir(dir,pre[0]));
			return 0;
		}
	} else
		rewinddir(dirp);

	Nfiles = 0;
	while ((dentp = readdir(dirp)) != NULL && Nfiles < LLEN) {
		/* Check for two systems with the same prefix.
		 * Magic number "5" is 1 for "grade" character plus
		 * 4 for sequence number.  The point here is to not
		 * send work for a system which has as a prefix the
		 * name of the system called for.
		 * Special case: prefix "X." does not do this check
		 * so uuxqt can use bldflst.
		 */
		if (!prefix(pre, dentp->d_name) ||
			(plen != 2 && (dentp->d_namlen-plen) != 5)) {
			DEBUG(99,"bldflst rejects %s\n",dentp->d_name);
			continue;
		}
		if (dentp->d_name[dentp->d_namlen-5] > MaxGrade) {
			DEBUG(8, "bldflst rejects %s, grade too low\n",
				dentp->d_name);
			continue;
		}
		if (*reqst == 'c')
			return 1;

		/* locate position for the new file and make room for it */
		for (i = Nfiles; i > 0; i--) {
			if (pcompar(dentp->d_name, Filent[i-1]) <= 0)
				break;
			if (i <LLEN)
				strcpy(Filent[i], Filent[i-1]);
		}

		/* add new file (if there is room), and increase Nfiles if need be */
		if (i < LLEN) {
			DEBUG(99,"bldflst accepts %s",dentp->d_name);
			DEBUG(99," as Filent[%d]\n", i);
			strcpy(Filent[i], dentp->d_name);
			if (Nfiles < LLEN)
				Nfiles++;
		} else
			DEBUG(99,"Filent full, %s rejected by bldflst\n", dentp->d_name);
		

	}
	if (Debug >99)
		for(i=0;i<Nfiles;i++)
			fprintf(stderr,"Filent[%d]=%s\n",i,Filent[i]);

	return Nfiles > 0;
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

/*
 *	get work vector
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

/*
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
	register ret = 0;
	int i;

	/* Starting new system; re-init */
	if (lastpre == 0 || strcmp(lastpre, pre) != SAME) {
		/* Force close of work file */
		anlwrk("", (char **)0);

		/* Save last worked-on prefix */
		if (lastpre != 0)
			free(lastpre);
		lastpre = malloc((unsigned)(strlen(pre)+1));
		strcpy(lastpre, pre);

		/* Set the external indexes properly */
		Nfiles = 0;
	}

	/*
	 * If the list is empty or new files have entered
	 * the spool area, call "bldflst" to read
	 * some file names into it. 
	 */
	if (Nfiles <= 0 || newspool((time_t)TLIMIT)) {
		ret = bldflst(reqst, dir, pre);
		DEBUG(99, "bldflst returns %d\n", ret);
	}

	/* If they only wanted to check, return
	 * boolean list not empty.  NB: the list
	 * will be forcibly emptied as soon as
	 * a new system name is mentioned.
	 */
	if (*reqst == 'c')
		return ret;

	if (Nfiles-- <= 0) {
		/* Didn't find any files in the spool area */
		Nfiles = 0;
		return 0;
	}
	/* Found some files, return the first one */
	sprintf(file, "%s/%s", dir, Filent[0]);
	for (i = 0; i < Nfiles; i++)
		strcpy(Filent[i], Filent[i+1]);
	return 1;
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
