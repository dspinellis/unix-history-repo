	/*  chkpth  2.2  5/21/79  13:20:43  */
#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

static char SiD[] = "@(#)chkpth	2.2";


#define DFLTNAME "default"
#define MAXUSERS 20
struct userpath {
	char *us_lname;
	char *us_mname;
	char us_callback;
	char **us_path;
};
struct userpath Upt[15];
struct userpath *Mchdef = NULL, *Logdef = NULL;
int Nbrusers = 0;
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
	struct userpath *u;
	extern char *lastpart();
	char **p, *s;
	char c;
	int ret, i;

	if (Uptfirst) {
		ret = rdpth(Upt);
		ASSERT(ret == 0, "INIT USERFILE %d", Nbrusers);
		Uptfirst = 0;
	}
	for (u = Upt, i = 0; i < Nbrusers; i++, u++) {
		if (*logname != '\0' && strcmp(logname, u->us_lname) == SAME)
			break;
		if (*mchname != '\0' && strcmp(mchname, u->us_mname) == SAME)
			break;

	}
	if (i >= Nbrusers) {
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
		if (*s == '/' && prefix("../", (++s)))
			return(FAIL);
	}

	for (p = u->us_path; *p != NULL; p++)
		if (prefix(*p, path))
			return(0);

	if (prefix(Spool, path)) {
		if ((c = *lastpart(path)) == DATAPRE
		  || c == XQTPRE)
			return(0);
	}
	/* path name not valid */
	return(FAIL);
}


/***
 *	rdpth(u)
 *	struct userpath *u;
 *
 *	rdpth  -  this routine will read the USFILE and
 *	construct the userpath structure pointed to by (u);
 *
 *	return codes:  0  |  FAIL
 */

rdpth(u)
struct userpath *u;
{
	char buf[BUFSIZ + 1], *pbuf[BUFSIZ + 1], *pc, **cp;
	extern char *calloc(), *index();
	FILE *uf;

	if ((uf = fopen(USERFILE, "r")) == NULL) {
		/* can not open file */
		return(FAIL);
	}

	while (fgets(buf, BUFSIZ, uf) != NULL) {
		int nargs, i;
		if (++Nbrusers > MAXUSERS) {
			fclose(uf);
			return(FAIL);
		}
		if ((pc = calloc(strlen(buf) + 1, sizeof (char)))
			== NULL) {
			/* can not allocate space */
			fclose(uf);
			return(FAIL);
		}

		strcpy(pc, buf);
		nargs = getargs(pc, pbuf);
		u->us_lname = pbuf[0];
		pc = index(u->us_lname, ',');
		if (pc != NULL)
			*pc++ = '\0';
		else
			pc = u + strlen(u->us_lname);
		u->us_mname = pc;
		if (*u->us_lname == '\0' && Logdef == NULL)
			Logdef = u;
		else if (*u->us_mname == '\0' && Mchdef == NULL)
			Mchdef = u;
		i = 1;
		if (strcmp(pbuf[1], "c") == SAME) {
			u->us_callback = 1;
			i++;
		}
		else
			u->us_callback = 0;
		if ((cp = u->us_path =
		  calloc(nargs - i + 1, sizeof (char *))) == NULL) {
			/*  can not allocate space */
			fclose(uf);
			return(FAIL);
		}

		while (i < nargs)
			*cp++ = pbuf[i++];
		*cp = NULL;
		u++;
	}

	fclose(uf);
	return(0);
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
char *name;
{
	struct userpath *u;
	int ret, i;

	if (Uptfirst) {
		ret = rdpth(Upt);
		ASSERT(ret == 0, "INIT USERFILE %d", Nbrusers);
		Uptfirst = 0;
	}

	for (u = Upt, i = 0; i < Nbrusers; u++, i++) {
		if (strcmp(u->us_lname, name) != SAME)
			continue;

		/* found user name */
		return(u->us_callback);
	}

	/* userid not found */
	return(0);
}


/***
 *	chkperm(file, user, mopt)	check write permission of file
 *	char *file, *user;
 *	char *mopt;		none NULL - create directories
 *
 *	if mopt != NULL and permissions are ok,
 *	a side effect of this routine is to make
 *	directories up to the last part of the
 *	filename (if they do not exist).
 *
 *	return 0 | FAIL
 */

chkperm(file, user, mopt)
char *file, *user, *mopt;
{
	struct stat s;
	int ret;
	char dir[MAXFULLNAME];
	extern char *lastpart();

	if (stat(file, &s) != -1)
		return(0);

	strcpy(dir, file);
	*lastpart(dir) = '\0';
	if ((ret = stat(dir, &s)) == -1
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
