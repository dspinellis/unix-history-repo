/*  $Revision: 1.9 $
**
**  Build an active file from either an old copy or by calling find
**  to get the directory names.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "mydir.h"
#include "macros.h"


STATIC char	ACTIVE[] = _PATH_ACTIVE;


/*
**  Given an newsgroup name, write the active file entry.
*/
STATIC BOOL
MakeEntry(name, rest, oldhimark, oldlomark, ComputeMarks)
    char		*name;
    char		*rest;
    long		oldhimark;
    long		oldlomark;
    BOOL		ComputeMarks;
{
    register long	himark;
    register long	lomark;
    register DIR	*dp;
    register DIRENTRY	*ep;
    register long	j;
    register char	*p;

    /* Turn group name into directory name. */
    for (p = name; *p; p++)
	if (*p == '.')
	    *p = '/';

    /* Set initial hi and lo marks. */
    if (ComputeMarks) {
	himark = 0;
	lomark = 0;
    }
    else {
	himark = oldhimark;
	lomark = oldlomark;
    }

    if ((dp = opendir(name)) != NULL) {
	/* Scan through all entries in the directory. */
	while ((ep = readdir(dp)) != NULL) {
	    p = ep->d_name;
	    if (!CTYPE(isdigit, p[0]) || strspn(p, "0123456789") != strlen(p)
	     || (j = atol(p)) == 0)
		continue;
	    if (lomark == 0 || j < lomark)
		lomark = j;
	    if (j > himark)
		himark = j;
	}
	(void)closedir(dp);
    }
    if (lomark == 0 || lomark - 1 > himark)
	lomark = himark + 1;

    /* Reset marks if computed them and didn't find any articles. */
    if (ComputeMarks && lomark == 1 && himark == 0) {
	himark = oldhimark;
	lomark = oldlomark;
    }
    /* Turn the directory name back into a newsgroup name. */
    for (p = name; *p; p++)
	if (*p == '/')
	    *p = '.';
    if (printf("%s %010.10ld %010.10ld %s\n",
	    name, himark, lomark, rest) == EOF
     || fflush(stdout) == EOF
     || ferror(stdout)) {
	(void)fprintf(stderr, "Error writing %s entry, %s\n",
		name, strerror(errno));
	return FALSE;
    }
    return TRUE;
}


/*
**  See if a line is too long to be a newsgroup name, return TRUE if so.
*/
STATIC BOOL
TooLong(buff, i)
    char		*buff;
    int			i;
{
    register char	*p;

    if ((p = strchr(buff, '\n')) == NULL) {
	(void)fprintf(stderr, "Line %d is too long:  \"%.40s\"...\n",
		i, buff);
	return TRUE;
    }
    *p = '\0';
    if (p - buff > SMBUF) {
	(void)fprintf(stderr, "Group line %d is too long: \"%.40s\"...\n",
		i, buff);
	return TRUE;
    }
    return FALSE;
}


/*
**  Renumber the active file based on the old active file.
*/
STATIC BOOL
RebuildFromOld(ComputeMarks)
    BOOL		ComputeMarks;
{
    register FILE	*F;
    register char	*p;
    register int	i;
    register BOOL	Ok;
    char		buff[BUFSIZ];
    STRING		rest;
    long		lomark;
    long		himark;
    char		*save1;
    char		*save2;

    /* Open the file. */
    if ((F = fopen(ACTIVE, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\", %s\n",
		ACTIVE, strerror(errno));
	exit(1);
    }

    /* Process each entry. */
    for (i = 1, Ok = TRUE; fgets(buff, sizeof buff, F) != NULL; i++) {
	if (TooLong(buff, i)) {
	    Ok = FALSE;
	    continue;
	}

	/* Set default fields. */
	lomark = 0;
	himark = 0;
	rest = "y";

	/* Try to parse the other fields. */
	if ((p = strchr(buff, ' ')) != NULL) {
	    *p++ = '\0';
	    save1 = p;
	    if ((p = strchr(p, ' ')) != NULL) {
		*p++ = '\0';
		save2 = p;
		if ((p = strchr(p, ' ')) != NULL) {
		    *p++ = '\0';
		    rest = p;
		    lomark = atol(save2);
		    himark = atol(save1);
		}
	    }
	}

	if (!MakeEntry(buff, rest, himark, lomark, ComputeMarks)) {
	    Ok = FALSE;
	    break;
	}
    }

    (void)fclose(F);
    return Ok;
}


STATIC BOOL
RebuildFromFind()
{
    register int	i;
    register char	*p;
    register FILE	*F;
    register BOOL	Ok;
    char		buff[BUFSIZ];

    /* Start getting a list of the directories. */
#if	defined(DO_HAVE_SYMLINK)
    F = popen("exec find . -follow -type d -print", "r");
#else
    F = popen("exec find . -type d -print", "r");
#endif	/* defined(DO_HAVE_SYMLINK) */
    if (F == NULL) {
	(void)fprintf(stderr, "Can't start find, %s\n", strerror(errno));
	exit(1);
    }

    /* Loop over all input. */
    for (i = 1, Ok = TRUE; fgets(buff, sizeof buff, F) != NULL; i++) {
	if (TooLong(buff, i)) {
	    Ok = FALSE;
	    continue;
	}

	/* Skip leading "./" and some known-to-be-bad directories. */
	p = buff[0] == '.' && buff[1] == '/' ? &buff[2] : buff;
	if (EQ(p, "lost+found") || strchr(p, '.') != NULL)
	    continue;
	if (!MakeEntry(p, "y", 0L, 0L, FALSE)) {
	    Ok = FALSE;
	    break;
	}
    }

    /* Clean up. */
    i = pclose(F) >> 8;
    if (i) {
	(void)fprintf(stderr, "Find exited with status %d\n", i);
	Ok = FALSE;
    }
    return Ok;
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: makeactive [-o [-m] ] >output\n");
    exit(1);
    /* NOTREACHED */
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    BOOL		Ok;
    register int	i;
    BOOL		OldFile;
    BOOL		ComputeMarks;

    /* Set defaults. */
    OldFile = FALSE;
    ComputeMarks = FALSE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "mo")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'm':
	    ComputeMarks = TRUE;
	    break;
	case 'o':
	    OldFile = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac || (ComputeMarks && !OldFile))
	Usage();

    /* Go to where the articles are. */
    if (chdir(_PATH_SPOOL) < 0) {
	(void)fprintf(stderr, "Can't change to spool directory, %s\n",
		strerror(errno));
	exit(1);
    }

    if (OldFile)
	Ok = RebuildFromOld(ComputeMarks);
    else
	Ok = RebuildFromFind();

    if (fflush(stdout) || ferror(stdout)) {
	(void)fprintf(stderr, "Can't flush stdout, %s\n", strerror(errno));
	Ok = FALSE;
    }

    exit(Ok ? 0 : 1);
    /* NOTREACHED */
}
