/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getusershell.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdio.h>

#define SHELLS "/etc/shells"

/*
 * Do not add local shells here.  They should be added in /etc/shells
 */
static char *okshells[] =
    { "/bin/sh", "/bin/csh", 0 };

static int inprogress;
static char **shells, *strings;
extern char **initshells();

/*
 * Get a list of shells from SHELLS, if it exists.
 */
char *
getusershell()
{
	char *ret;
	static char **shells;

	if (!inprogress)
		shells = initshells();
	ret = *shells;
	if (*shells != NULL)
		shells++;
	return (ret);
}

endusershell()
{
	
	if (shells != NULL)
		free((char *)shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	inprogress = 0;
}

setusershell()
{

	shells = initshells();
}

static char **
initshells()
{
	register char **sp, *cp;
	register FILE *fp;
	struct stat statb;
	extern char *malloc(), *calloc();

	inprogress = 1;
	if (shells != NULL)
		free((char *)shells);
	shells = NULL;
	if (strings != NULL)
		free(strings);
	strings = NULL;
	if ((fp = fopen(SHELLS, "r")) == (FILE *)0)
		return(okshells);
	if (fstat(fileno(fp), &statb) == -1) {
		(void)fclose(fp);
		return(okshells);
	}
	if ((strings = malloc((unsigned)statb.st_size)) == NULL) {
		(void)fclose(fp);
		return(okshells);
	}
	shells = (char **)calloc((unsigned)statb.st_size / 3, sizeof (char *));
	if (shells == NULL) {
		(void)fclose(fp);
		free(strings);
		strings = NULL;
		return(okshells);
	}
	sp = shells;
	cp = strings;
	while (fgets(cp, MAXPATHLEN + 1, fp) != NULL) {
		while (*cp != '/' && *cp != '\0')
			cp++;
		if (*cp == '\0')
			continue;
		*sp++ = cp;
		while (!isspace(*cp) && *cp != '#' && *cp != '\0')
			cp++;
		*cp++ = '\0';
	}
	*sp = (char *)0;
	(void)fclose(fp);
	return (shells);
}
