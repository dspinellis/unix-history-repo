#ifndef lint
static char sccsid[] = "@(#)mailst.c	5.2 (Berkeley) 7/19/83";
#endif

#include "uucp.h"

/*******
 *	mailst(user, str, file)
 *
 *	mailst  -  this routine will fork and execute
 *	a mail command sending string (str) to user (user).
 *	If file is non-null, the file is also sent.
 *	(this is used for mail returned to sender.)
 */

mailst(user, str, file)
char *user, *str, *file;
{
	register FILE *fp, *fi;
	char cmd[100], buf[BUFSIZ];
	register int nc;

	sprintf(cmd, "mail %s", user);
	if ((fp = rpopen(cmd, "w")) == NULL)
		return;
	fprintf(fp, "%s", str);

	if (*file != '\0' && (fi = fopen(subfile(file), "r")) != NULL) {
		while ((nc = fread(buf, sizeof (char), BUFSIZ, fi)) > 0)
			fwrite(buf, sizeof (char), nc, fp);
		fclose(fi);
	}

	pclose(fp);
	return;
}
