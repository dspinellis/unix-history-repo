/* pwd.c - return the current working directory */
#ifndef	lint
static char ident[] = "@(#)$Id: pwd.c,v 2.5 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#if	!defined (BSD42) && !defined (SYS5DIR)
#include "../h/local.h"
#endif	/* not BSD42 and not SYS5DIR */
#include <stdio.h>

#define	MAXPATHLEN	1024

static char curwd[MAXPATHLEN];



char   *pwd () {
    register char  *cp;

#ifndef	BSD42
#ifndef	SYS5DIR
    if (getwd (curwd) == NOTOK) {
	admonish (NULL, "unable to determine working directory");
#else	/* SYS5DIR */
    if (getcwd (curwd, MAXPATHLEN) == NULL) {
	admonish (NULL, "unable to determine working directory");
#endif	/* SYS5DIR */
#else	/* BSD42 */
    if (getwd (curwd) == 0) {
	admonish (NULLCP, "unable to determine working directory: %s", curwd);
#endif	/* BSD42 */
	if (mypath == NULL
		|| *mypath == 0
		|| ((void) strcpy (curwd, mypath), chdir (curwd)) == NOTOK) {
	    (void) strcpy (curwd, "/");
	    (void) chdir (curwd);
	}
	return curwd;
    }

    if ((cp = curwd + strlen (curwd) - 1) > curwd && *cp == '/')
	*cp = 0;

    return curwd;
}

/*  */

#if	!defined (BSD42) && !defined (SYS5DIR)
/* getwd() - get the current working directory */

/* Algorithm from several sources, -ljobs, pwd.c, etc., etc. */

getwd (cwd)
register char   *cwd;
{
    int     found;
    char    tmp1[BUFSIZ],
            tmp2[BUFSIZ];
    struct stat st1,
                st2,
                root;
    register struct direct *dp;
    register    DIR * dd;

    (void) strcpy (cwd, "/");
    (void) stat ("/", &root);

    for (;;) {
	if ((dd = opendir ("..")) == NULL)
	    return NOTOK;
	if (stat (".", &st2) == NOTOK || stat ("..", &st1) == NOTOK)
	    goto out;
	if (st2.st_ino == root.st_ino && st2.st_dev == root.st_dev) {
	    closedir (dd);
	    return chdir (cwd);
	}

	if (st2.st_ino == st1.st_ino && st2.st_dev == st1.st_dev) {
	    closedir (dd);
	    (void) chdir ("/");
	    if ((dd = opendir (".")) == NULL)
		return NOTOK;
	    if (stat (".", &st1) < 0)
		goto out;
	    if (st2.st_dev != st1.st_dev)
		while (dp = readdir (dd)) {
		    if (stat (dp -> d_name, &st1) == NOTOK)
			goto out;
		    if (st2.st_dev == st1.st_dev) {
			(void) sprintf (tmp1, "%s%s", dp -> d_name, cwd);
			(void) strcpy (cwd + 1, tmp1);
			closedir (dd);
			return (chdir (cwd));
		    }
		}
	    else {
		closedir (dd);
		return (chdir (cwd));
	    }
	}

	found = 0;
	while (dp = readdir (dd)) {
	    (void) sprintf (tmp2, "../%s", dp -> d_name);
	    if (stat (tmp2, &st1) != NOTOK
		    && st1.st_ino == st2.st_ino
		    && st1.st_dev == st2.st_dev) {
		closedir (dd);
		found++;
		(void) chdir ("..");
		(void) sprintf (tmp1, "%s%s", dp -> d_name, cwd);
		(void) strcpy (cwd + 1, tmp1);
		break;
	    }
	}
	if (!found)
	    goto out;
    }

out: ;
    closedir (dd);
    return NOTOK;
}
#endif	/* not BSD42 and not SYS5DIR */
