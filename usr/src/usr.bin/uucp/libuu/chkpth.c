#ifndef lint
static char sccsid[] = "@(#)chkpth.c	5.1 (Berkeley) %G%";
#endif

/*
 * Doug Kingston, 30 July 82 to fix handling of the "userpath" structures.
 * (brl-bmd) 
 * rti!trt: the code here is bizarre.  There must be a zillion holes.
 * chkpth should not be called for implied Spool requests .
 * But explicit requests (foo!/usr/spoo/uucp/*) should use chkpth.
 */
#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>


struct userpath {
	char *us_lname;
	char *us_mname;
	char us_callback;
	char **us_path;
	struct userpath *unext;
};
struct userpath *Uhead = NULL;
struct userpath *Mchdef = NULL, *Logdef = NULL;
int Uptfirst = 1;


/*******
 *	chkpth(logname, mchname, path)
 *	char *path, *logname, *mchname;
 *
 *	chkpth  -  this routine will check the path table for the
 *	machine or log name (non-null parameter) to see if the
 *	input path (path)
 *	starts with an acceptable prefix.
 *
 *	return codes:  0  |  FAIL
 */

chkpth(logname, mchname, path)
char *path, *logname, *mchname;
{
	register struct userpath *u;
	extern char *lastpart();
	register char **p, *s;

	/* Allow only rooted pathnames.  Security wish.  rti!trt */
	if (*path != '/')
		return(FAIL);

	if (Uptfirst) {
		rdpth();
		ASSERT(Uhead != NULL, "INIT USERFILE, No entrys!", "", 0);
		Uptfirst = 0;
	}
	for (u = Uhead; u != NULL; ) {
		if (*logname != '\0' && strcmp(logname, u->us_lname) == SAME)
			break;
		if (*mchname != '\0' && strncmp(mchname, u->us_mname, 7) == SAME)
			break;
		u = u->unext;
	}
	if (u == NULL) {
		if (*logname == '\0')
			u = Mchdef;
		else
			u = Logdef;
		if (u == NULL)
			return(FAIL);
	}
	/* found user name */
	p = u->us_path;

	/*  check for /../ in path name  */
	for (s = path; *s != '\0'; s++) {
		if (prefix("/../",s))
			return(FAIL);
	}

	/* Check for access permission */
	for (p = u->us_path; *p != NULL; p++)
		if (prefix(*p, path))
			return(0);

	/* path name not valid */
	return(FAIL);
}


/***
 *	rdpth()
 *
 *	rdpth  -  this routine will read the USERFILE and
 *	construct the userpath structure pointed to by (u);
 *
 *	return codes:  0  |  FAIL
 *
 * 5/3/81 - changed to enforce the uucp-wide convention that system
 *	    names be 7 chars or less in length
 */

rdpth()
{
	char buf[100 + 1], *pbuf[50 + 1];
	register struct userpath *u;
	register char *pc, **cp;
	FILE *uf;

	if ((uf = fopen(USERFILE, "r")) == NULL) {
		/* can not open file */
		return;
	}

	while (cfgets(buf, sizeof(buf), uf) != NULL) {
		int nargs, i;

		if ((u = (struct userpath *)malloc(sizeof (struct userpath))) == NULL) {
			DEBUG (1, "*** Userpath malloc failed\n", 0);
			fclose (uf);
			return;
		}
		if ((pc = calloc((unsigned)strlen(buf) + 1, sizeof (char)))
			== NULL) {
			/* can not allocate space */
			DEBUG (1, "Userpath calloc 1 failed\n", 0);
			fclose(uf);
			return;
		}

		strcpy(pc, buf);
		nargs = getargs(pc, pbuf);
		u->us_lname = pbuf[0];
		pc = index(u->us_lname, ',');
		if (pc != NULL)
			*pc++ = '\0';
		else
			pc = u->us_lname + strlen(u->us_lname);
		u->us_mname = pc;
		if (strlen(u->us_mname) > 7)
			u->us_mname[7] = '\0';
		if (*u->us_lname == '\0' && Logdef == NULL)
			Logdef = u;
		/* rti!trt: commented following else so
		 * chkpth("","",file) works okay.
		 * I don't understand this, though.
		 */
		/*else*/ if (*u->us_mname == '\0' && Mchdef == NULL)
			Mchdef = u;
		i = 1;
		if (strcmp(pbuf[1], "c") == SAME) {
			u->us_callback = 1;
			i++;
		}
		else
			u->us_callback = 0;
		if ((cp = u->us_path =
		  (char **)calloc((unsigned)(nargs-i+1), sizeof(char *))) == NULL) {
			/*  can not allocate space */
			DEBUG (1, "Userpath calloc 2 failed!\n", 0);
			fclose(uf);
			return;
		}

		while (i < nargs)
			*cp++ = pbuf[i++];
		*cp = NULL;
		u->unext = Uhead;
		Uhead = u;
	}

	fclose(uf);
	return;
}

/***
 *	callback(name)	check for callback
 *	char *name;
 *
 *	return codes:
 *		0  -  no call back
 *		1  -  call back
 */

callback(name)
register char *name;
{
	register struct userpath *u;

	if (Uptfirst) {
		rdpth();
		ASSERT(Uhead != NULL, "INIT USERFILE, No Users!", "", 0);
		Uptfirst = 0;
	}

	for (u = Uhead; u != NULL; ) {
		if (strcmp(u->us_lname, name) == SAME)
			/* found user name */
			return(u->us_callback);
		u = u->unext;
	}

	/* userid not found */
	return(0);
}


/***
 *	chkperm(file, mopt)	check write permission of file
 *	char *mopt;		none NULL - create directories
 *
 *	if mopt != NULL and permissions are ok,
 *	a side effect of this routine is to make
 *	directories up to the last part of the
 *	filename (if they do not exist).
 *
 *	return 0 | FAIL
 */

chkperm(file, mopt)
char *file, *mopt;
{
	struct stat s;
	int ret;
	char dir[MAXFULLNAME];
	extern char *lastpart();

	if (stat(subfile(file), &s) == 0) {
		/* Forbid scribbling on a not-generally-writable file */
		/* rti!trt */
		if ((s.st_mode & ANYWRITE) == 0)
			return(FAIL);
		return(0);
	}

	strcpy(dir, file);
	*lastpart(dir) = '\0';
	if ((ret = stat(subfile(dir), &s)) == -1
	  && mopt == NULL)
		return(FAIL);

	if (ret != -1) {
		if ((s.st_mode & ANYWRITE) == 0)
			return(FAIL);
		else
			return(0);
	}

	/*  make directories  */
	return(mkdirs(file));
}

/*
 * Check for sufficient privilege to request debugging.
 * Suggested by seismo!stewart, John Stewart.
 */
chkdebug(uid)
int uid;
{
	if (uid > PRIV_UIDS) {
		fprintf(stderr, "Sorry, uid must be <= %d for debugging\n",
			PRIV_UIDS);
		cleanup(1);
		exit(1);	/* Just in case */
	}
}
