#ifndef lint
static char sccsid[] = "@(#)chkpth.c	5.5	(Berkeley) %G%";
#endif

#include "uucp.h"
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

/*LINTLIBRARY*/

/*
 *	this routine will check the path table for the
 *	machine or log name (non-null parameter) to see if the
 *	input path (path) starts with an acceptable prefix.
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
	if (*path != '/') {
		DEBUG(4, "filename doesn't begin with /\n", CNULL);
		return FAIL;
	}

	if (Uptfirst) {
		rdpth();
		if (Uhead == NULL) {
			syslog(LOG_ERR, "USERFILE empty!");
			cleanup(FAIL);
		}
		Uptfirst = 0;
	}
	for (u = Uhead; u != NULL; ) {
		if (*logname != '\0' && strcmp(logname, u->us_lname) == SAME)
			break;
		if (*mchname != '\0' && strncmp(mchname, u->us_mname, MAXBASENAME) == SAME)
			break;
		u = u->unext;
	}
	if (u == NULL) {
		if (*logname == '\0')
			u = Mchdef;
		else
			u = Logdef;
		if (u == NULL)
			return FAIL;
	}

	/*  check for /../ in path name  */
	for (s = path; *s != '\0'; s++) {
		if (prefix("/../",s)) {
			DEBUG(4, "filename has /../ in it\n", CNULL);
			return FAIL;
		}
	}

	/* Check for access permission */
	for (p = u->us_path; *p != NULL; p++)
		if (prefix(*p, path))
			return SUCCESS;
	DEBUG(4, "filename not in list\n", CNULL);

	/* path name not valid */
	return FAIL;
}


/***
 *	rdpth()
 *
 *	rdpth  -  this routine will read the USERFILE and
 *	construct the userpath structure pointed to by (u);
 *
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

		u = (struct userpath *)malloc(sizeof (struct userpath));
		if (u == NULL) {
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
		nargs = getargs(pc, pbuf, 50);
		u->us_lname = pbuf[0];
		pc = index(u->us_lname, ',');
		if (pc != NULL)
			*pc++ = '\0';
		else
			pc = u->us_lname + strlen(u->us_lname);
		u->us_mname = pc;
		if (strlen(u->us_mname) > MAXBASENAME)
			u->us_mname[MAXBASENAME] = '\0';
		if (*u->us_lname == '\0' && Logdef == NULL)
			Logdef = u;
		if (*u->us_mname == '\0' && Mchdef == NULL)
			Mchdef = u;
		i = 1;
		if (strcmp(pbuf[1], "c") == SAME) {
			u->us_callback = 1;
			i++;
		}
		else
			u->us_callback = 0;
		cp = (char **)calloc((unsigned)(nargs-i+1), sizeof(char *));
		if (cp == NULL) {
			/*  can not allocate space */
			DEBUG (1, "Userpath calloc 2 failed!\n", 0);
			fclose(uf);
			return;
		}
		u->us_path = cp;

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
		if (Uhead == NULL) {
			syslog(LOG_ERR, "USERFILE empty!");
			cleanup(FAIL);
		}
		Uptfirst = 0;
	}

	for (u = Uhead; u != NULL; ) {
		if (strcmp(u->us_lname, name) == SAME)
			/* found user name */
			return u->us_callback;
		u = u->unext;
	}

	/* userid not found */
	return 0;
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
 *	return SUCCESS | FAIL
 */

chkperm(file, mopt)
char *file, *mopt;
{
	struct stat s;
	int ret;
	char dir[MAXFULLNAME];
	extern char *lastpart();

	if (stat(subfile(file), &s) == 0) {
		if ((s.st_mode & ANYWRITE) == 0) {
			DEBUG(4,"file is not writable: mode %o\n", s.st_mode);
			return FAIL;
		}
		return SUCCESS;
	}

	strcpy(dir, file);
	*lastpart(dir) = '\0';
	if ((ret = stat(subfile(dir), &s)) == -1 && mopt == NULL) {
		DEBUG(4, "can't stat directory %s\n", subfile(dir));
		return FAIL;
	}

	if (ret != -1) {
		if ((s.st_mode & ANYWRITE) == 0)
			return FAIL;
		else
			return SUCCESS;
	}

	/*  make directories  */
	return mkdirs(file);
}

/*
 * Check for sufficient privilege to request debugging.
 */
chkdebug()
{
	if (access(SYSFILE, 04) < 0) {
		fprintf(stderr, "Sorry, you must be able to read L.sys for debugging\n");
		cleanup(1);
		exit(1);	/* Just in case */
	}
}
