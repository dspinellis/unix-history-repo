#ifndef lint
static char sccsid[] = "@(#)expfile.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

/*******
 *	expfile(file)	expand file name
 *	char *file;
 *
 *	return codes: 0 - Ordinary spool area file
 *		      1 - Other normal file
 *		      FAIL - no Wrkdir name available
 */

expfile(file)
char *file;
{
	register char *fpart, *p;
	char user[20], *up;
	char full[100];
	int uid;

	switch(file[0]) {
	case '/':
		return(1);
	case '~':
		for (fpart = file + 1, up = user; *fpart != '\0'
			&& *fpart != '/'; fpart++)
				*up++ = *fpart;
		*up = '\0';
		/* ll1b.105, mn, Mark Nettleingham, defend against
		 * null login name in /etc/passwd
		 */
		if (!*user || gninfo(user, &uid, full) != 0) {
			strcpy(full, PUBDIR);
		}
	
		strcat(full, fpart);
		strcpy(file, full);
		return(1);
	default:
		p = index(file, '/');
		strcpy(full, Wrkdir);
		strcat(full, "/");
		strcat(full, file);
		strcpy(file, full);
		if (Wrkdir[0] == '\0')
			return(FAIL);
		else if (p != NULL)
			return(1);
		return(0);
	}
}


/***
 *	isdir(name)	check if directory name
 *	char *name;
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
		return(0);
	if ((s.st_mode & S_IFMT) == S_IFDIR)
		return(1);
	return(0);
}


/***
 *	mkdirs(name)	make all necessary directories
 *	char *name;
 *
 *	return 0  |  FAIL
 */

mkdirs(name)
char *name;
{
	int ret, mask;
	char cmd[100], dir[100];
	register char *p;

	for (p = dir + 1;; p++) {
		strcpy(dir, name);
		if ((p = index(p, '/')) == NULL)
			return(0);
		*p = '\0';
		if (isdir(dir))
			continue;

		/* rti!trt: add chmod ala 4.1c uucp */
		sprintf(cmd, "mkdir %s;chmod 0777 %s", dir, dir);
		DEBUG(4, "mkdir - %s\n", dir);
		mask = umask(0);
		ret = shio(cmd, CNULL, CNULL, User);
		umask(mask);
		if (ret != 0)
			return(FAIL);
	}
}

/***
 *	ckexpf - expfile and check return
 *		print error if it failed.
 *
 *	return code - 0 - ok; FAIL if expfile failed
 */

ckexpf(file)
register char *file;
{

	if (expfile(file) != FAIL)
		return(0);

	/*  could not expand file name */
	/* the gwd routine failed */

	fprintf(stderr, "Can't expand filename (%s). Pwd failed.\n", file+1);
	return(FAIL);
}
