#ifndef lint
static char sccsid[] = "@(#)expfile.c	5.5 (Berkeley) 6/19/85";
#endif

#include "uucp.h"
#include <sys/stat.h>

/*LINTLIBRARY*/

/*
 *	expand file name
 *
 *	return codes: 0 - Ordinary spool area file
 *		      1 - Other normal file
 *		      FAIL - no Wrkdir name available
 */

expfile(file)
char *file;
{
	register char *fpart, *p;
	char user[WKDSIZE], *up;
	char full[MAXFULLNAME];
	int uid;

	switch(file[0]) {
	case '/':
		return 1;
	case '~':
		for (fpart = file + 1, up = user; *fpart != '\0'
			&& *fpart != '/'; fpart++)
				*up++ = *fpart;
		*up = '\0';
		if (!*user || gninfo(user, &uid, full) != 0) {
			strcpy(full, PUBDIR);
		}
	
		strcat(full, fpart);
		strcpy(file, full);
		return 1;
	default:
		p = index(file, '/');
		strcpy(full, Wrkdir);
		strcat(full, "/");
		strcat(full, file);
		strcpy(file, full);
		if (Wrkdir[0] == '\0')
			return FAIL;
		else if (p != NULL)
			return 1;
		return 0;
	}
}


/*
 *	check if directory name
 *
 *	return codes:  0 - not directory  |  1 - is directory
 */

isdir(name)
char *name;
{
	register int ret;
	struct stat s;

	ret = stat(subfile(name), &s);
	if (ret < 0)
		return 0;
	if ((s.st_mode & S_IFMT) == S_IFDIR)
		return 1;
	return 0;
}


/*
 *	make all necessary directories
 *
 *	return SUCCESS  |  FAIL
 */

mkdirs(name)
char *name;
{
	int ret, mask;
	char dir[MAXFULLNAME];
	register char *p;

	for (p = dir + 1;; p++) {
		strcpy(dir, name);
		if ((p = index(p, '/')) == NULL)
			return SUCCESS;
		*p = '\0';
		if (isdir(dir))
			continue;

		DEBUG(4, "mkdir - %s\n", dir);
		mask = umask(0);
		ret = mkdir(dir, 0777);
		umask(mask);
		if (ret != 0)
			return FAIL;
	}
	/* NOTREACHED */
}

/*
 *	expfile and check return
 *		print error if it failed.
 *
 *	return code - SUCCESS - ok; FAIL if expfile failed
 */

ckexpf(file)
register char *file;
{

	if (expfile(file) != FAIL)
		return SUCCESS;

	/*  could not expand file name */
	/* the gwd routine failed */

	logent("CAN'T EXPAND FILENAME - PWD FAILED", file+1);
	return FAIL;
}
