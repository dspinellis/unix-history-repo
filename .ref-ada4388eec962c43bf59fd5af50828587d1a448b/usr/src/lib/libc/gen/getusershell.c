/*
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getusershell.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <paths.h>

/*
 * Local shells should NOT be added here.  They should be added in
 * /etc/shells.
 */

static char *okshells[] = { _PATH_BSHELL, _PATH_CSHELL, NULL };
static char **curshell, **shells, *strings;
static char **initshells __P((void));

/*
 * Get a list of shells from _PATH_SHELLS, if it exists.
 */
char *
getusershell()
{
	char *ret;

	if (curshell == NULL)
		curshell = initshells();
	ret = *curshell;
	if (ret != NULL)
		curshell++;
	return (ret);
}

void
endusershell()
{
	
	if (shells != NULL)
		free(shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	curshell = NULL;
}

void
setusershell()
{

	curshell = initshells();
}

static char **
initshells()
{
	register char **sp, *cp;
	register FILE *fp;
	struct stat statb;

	if (shells != NULL)
		free(shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	if ((fp = fopen(_PATH_SHELLS, "r")) == NULL)
		return (okshells);
	if (fstat(fileno(fp), &statb) == -1) {
		(void)fclose(fp);
		return (okshells);
	}
	if ((strings = malloc((u_int)statb.st_size)) == NULL) {
		(void)fclose(fp);
		return (okshells);
	}
	shells = calloc((unsigned)statb.st_size / 3, sizeof (char *));
	if (shells == NULL) {
		(void)fclose(fp);
		free(strings);
		strings = NULL;
		return (okshells);
	}
	sp = shells;
	cp = strings;
	while (fgets(cp, MAXPATHLEN + 1, fp) != NULL) {
		while (*cp != '#' && *cp != '/' && *cp != '\0')
			cp++;
		if (*cp == '#' || *cp == '\0')
			continue;
		*sp++ = cp;
		while (!isspace(*cp) && *cp != '#' && *cp != '\0')
			cp++;
		*cp++ = '\0';
	}
	*sp = NULL;
	(void)fclose(fp);
	return (shells);
}
