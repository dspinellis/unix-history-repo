/*  $Revision: 1.7 $
**
**  Read batchfiles on standard input and archive them.
*/
#include "configdata.h"
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include "paths.h"
#include "libinn.h"
#include "qio.h"
#include "clibrary.h"
#include "macros.h"


STATIC STRING	Archive = _PATH_ARCHIVEDIR;
STATIC char	SPOOL[] = _PATH_SPOOL;
static char	BATCHDIR[] = _PATH_BATCHDIR;


/*
**  Try to make one directory.  Return FALSE on error.
*/
STATIC BOOL
MakeDir(Name)
    char		*Name;
{
    struct stat		Sb;

    if (mkdir(Name, GROUPDIR_MODE) >= 0)
	return TRUE;

    /* See if it failed because it already exists. */
    return stat(Name, &Sb) >= 0 && S_ISDIR(Sb.st_mode);
}


/*
**  Given an entry, comp/foo/bar/1123, create the directory and all
**  parent directories needed.  Return FALSE on error.
*/
STATIC BOOL
MakeArchiveDirectory(Name)
    register char	*Name;
{
    register char	*p;
    register char	*save;
    BOOL		made;

    if ((save = strrchr(Name, '/')) != NULL)
	*save = '\0';

    /* Optimize common case -- parent almost always exists. */
    if (MakeDir(Name)) {
	if (save)
	    *save = '/';
	return TRUE;
    }

    /* Try to make each of comp and comp/foo in turn. */
    for (p = Name; *p; p++)
	if (*p == '/') {
	    *p = '\0';
	    made = MakeDir(Name);
	    *p = '/';
	    if (!made) {
		if (save)
		    *save = '/';
		return FALSE;
	    }
	}

    made = MakeDir(Name);
    if (save)
	*save = '/';
    return made;
}


/*
**  Write an index entry.  Ignore I/O errors; our caller checks for them.
*/
STATIC void
WriteIndex(FullName, ShortName)
    char		*FullName;
    char		*ShortName;
{
    static char		SUBJECT[] = "Subject:";
    static char		MESSAGEID[] = "Message-ID:";
    register char	*p;
    register QIOSTATE	*qp;
    char		Subject[BUFSIZ];
    char		MessageID[BUFSIZ];

    /* Open the file. */
    if ((qp = QIOopen(FullName, QIO_BUFFER)) == NULL) {
	(void)printf("%s <open error> %s\n", ShortName, strerror(errno));
	return;
    }

    /* Scan for the desired headers. */
    for (Subject[0] = '\0', MessageID[0] = '\0'; ; ) {
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOerror(qp)) {
		(void)printf("%s <read error> %s\n",
			ShortName, strerror(errno));
		QIOclose(qp);
		return;
	    }
	    if (QIOtoolong(qp)) {
		(void)QIOread(qp);
		continue;
	    }
	}

	/* End of headers -- we're done. */
	if (*p == '\0')
	    break;

	/* Is this a header we want? */
	switch (*p) {
	default:
	    continue;
	case 'S': case 's':
	    if (caseEQn(p, SUBJECT, STRLEN(SUBJECT))) {
		for (p += STRLEN(SUBJECT); ISWHITE(*p); p++)
		    continue;
		(void)strcpy(Subject, p);
	    }
	    break;
	case 'M': case 'm':
	    if (caseEQn(p, MESSAGEID, STRLEN(MESSAGEID))) {
		for (p += STRLEN(MESSAGEID); ISWHITE(*p); p++)
		    continue;
		(void)strcpy(MessageID, p);
	    }
	    break;
	}

	/* Got them all? */
	if (Subject[0] && MessageID[0])
	    break;
    }

    /* Close file, write the line. */
    QIOclose(qp);
    (void)printf("%s %s %s\n",
	    ShortName,
	    MessageID[0] ? MessageID : "<none>",
	    Subject[0] ? Subject : "<none>");
}


/*
**  Copy a file.  Return FALSE if error.
*/
STATIC BOOL
Copy(src, dest)
    char		*src;
    char		*dest;
{
    register FILE	*in;
    register FILE	*out;
    register SIZE_T	i;
    char		*p;
    char		buff[BUFSIZ];

    /* Open the output file. */
    if ((out = fopen(dest, "w")) == NULL) {
	/* Failed; make any missing directories and try again. */
	if ((p = strrchr(dest, '/')) != NULL) {
	    if (!MakeArchiveDirectory(dest)) {
		(void)fprintf(stderr, "Can't mkdir for \"%s\", %s\n",
			dest, strerror(errno));
		return FALSE;
	    }
	    out = fopen(dest, "w");
	}
	if (p == NULL || out == NULL) {
	    (void)fprintf(stderr, "Can't open \"%s\" for writing, %s\n",
		    src, strerror(errno));
	    return FALSE;
	}
    }

    /* Opening the input file is easier. */
    if ((in = fopen(src, "r")) == NULL) {
	(void)fprintf(stderr, "Can't open \"%s\" for reading, %s\n",
		src, strerror(errno));
	(void)fclose(out);
	(void)unlink(dest);
	return FALSE;
    }

    /* Write the data. */
    while ((i = fread((POINTER)buff, (SIZE_T)1, (SIZE_T)sizeof buff, in)) != 0)
	if (fwrite((POINTER)buff, (SIZE_T)1, i, out) != i) {
	    (void)fprintf(stderr, "Can't write \"%s\", %s\n",
		    dest, strerror(errno));
	    (void)fclose(in);
	    (void)fclose(out);
	    (void)unlink(dest);
	    return FALSE;
	}
    (void)fclose(in);

    /* Flush and close the output. */
    if (ferror(out) || fflush(out) == EOF) {
	(void)fprintf(stderr, "Can't close \"%s\", %s\n",
		dest, strerror(errno));
	(void)unlink(dest);
	(void)fclose(out);
	return FALSE;
    }
    if (fclose(out) == EOF) {
	(void)fprintf(stderr, "Can't close \"%s\", %s\n",
		dest, strerror(errno));
	(void)unlink(dest);
	return FALSE;
    }

    return TRUE;
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage error.\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register char	*Name;
    register char	*p;
    register FILE	*F;
    register int	i;
    BOOL		Flat;
    BOOL		Move;
    BOOL		Redirect;
    char		*Index;
    char		*last;
    char		buff[BUFSIZ];
    char		temp[BUFSIZ];
    char		dest[BUFSIZ];
    struct stat		Sb;

    /* Set defaults. */
    Flat = FALSE;
    Index = NULL;
    Move = FALSE;
    Redirect = TRUE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "a:fi:mr")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'a':
	    Archive = optarg;
	    break;
	case 'f':
	    Flat = TRUE;
	    break;
	case 'i':
	    Index = optarg;
	    break;
	case 'm':
	    Move = TRUE;
	    break;
	case 'r':
	    Redirect = FALSE;
	    break;
	}
#if	defined(DONT_HAVE_SYMLINK)
	if (Move)
	    (void)fprintf(stderr, "archive:  Ignoring ``-m'' flag\n");
#endif	/* defined(DONT_HAVE_SYMLINK) */

    /* Parse arguments -- at most one, the batchfile. */
    ac -= optind;
    av += optind;
    if (ac > 2)
	Usage();

    /* Do file redirections. */
    if (Redirect)
	(void)freopen(_PATH_ERRLOG, "a", stderr);
    if (ac == 1 && freopen(av[0], "r", stdin) == NULL) {
	(void)fprintf(stderr, "archive:  Can't open \"%s\" for input, %s\n",
		av[0], strerror(errno));
	    exit(1);
    }
    if (Index && freopen(Index, "a", stdout) == NULL) {
	(void)fprintf(stderr, "archive:  Can't open \"%s\" for output, %s\n",
		Index, strerror(errno));
	exit(1);
    }

    /* Go to where the action is. */
    if (chdir(SPOOL) < 0) {
	(void)fprintf(stderr, "archive:  Can't cd to \"%s\", %s\n",
		SPOOL, strerror(errno));
	exit(1);
    }

    /* Set up the destination. */
    (void)strcpy(dest, Archive);
    Name = dest + strlen(dest);
    *Name++ = '/';

    /* Read input. */
    while (fgets(buff, sizeof buff, stdin) != NULL) {
	if ((p = strchr(buff, '\n')) == NULL) {
	    (void)fprintf(stderr,
		    "archive:  Skipping \"%.40s...\" -- too long\n",
		    buff);
	    continue;
	}
	*p = '\0';
	if (buff[0] == '\0' || buff[0] == COMMENT_CHAR)
	    continue;

	/* Make sure we're only copying files. */
	if (stat(buff, &Sb) < 0) {
	    if (errno != ENOENT)
		(void)fprintf(stderr, "Can't stat \"%s\", %s\n",
			buff, strerror(errno));
	    continue;
	}
	if (!S_ISREG(Sb.st_mode)) {
	    (void)fprintf(stderr, "\"%s\" is not a regular file\n", buff);
	    continue;
	}

	/* Set up the destination name. */
	(void)strcpy(Name, buff);
	if (Flat) {
	    for (last = NULL, p = Name; *p; p++)
		if (*p == '/') {
		    last = p;
		    *p = '.';
		}
	    if (last)
		*last = '/';
	}

#if	defined(DO_HAVE_SYMLINK)
	if (Move) {
	    if (!Copy(buff, dest))
		continue;
	    if (unlink(buff) < 0 && errno != ENOENT)
		(void)fprintf(stderr, "Can't remove \"%s\", %s\n",
			buff, strerror(errno));
	    if (symlink(dest, buff) < 0)
		(void)fprintf(stderr, "Can't symlink \"%s\" to \"%s\", %s\n",
			buff, dest, strerror(errno));
	    continue;
	}
#endif	/* defined(DO_HAVE_SYMLINK) */

	/* Try to link the file into the archive. */
	if (link(buff, dest) < 0) {

	    /* Make the archive directory. */
	    if (!MakeArchiveDirectory(dest)) {
		(void)fprintf(stderr, "Can't mkdir for \"%s\", %s\n",
			dest, strerror(errno));
		continue;
	    }

	    /* Try to link again; if that fails, make a copy. */
	    if (link(buff, dest) < 0 && !Copy(buff, dest))
		continue;
	}

	/* Write index. */
	if (Index) {
	    WriteIndex(dest, Name);
	    if (ferror(stdout) || fflush(stdout) == EOF)
		(void)fprintf(stderr, "Can't write index for \"%s\", %s\n",
			Name, strerror(errno));
	}
    }

    /* If we read all our input, try to remove the file, and we're done. */
    if (feof(stdin)) {
	(void)fclose(stdin);
	if (av[0])
	    (void)unlink(av[0]);
	exit(0);
    }

    /* Make an appropriate spool file. */
    p = av[0];
    if (p == NULL)
	(void)sprintf(temp, "%s/%s", BATCHDIR, "archive");
    else if (*p == '/')
	(void)sprintf(temp, "%s.bch", p);
    else
	(void)sprintf(temp, "%s/%s.bch", BATCHDIR, p);
    if ((F = xfopena(temp)) == NULL) {
	(void)fprintf(stderr, "archive: Can't spool to \"%s\", %s\n",
	    temp, strerror(errno));
	exit(1);
    }

    /* Write the rest of stdin to the spool file. */
    i = 0;
    if (fprintf(F, "%s\n", buff) == EOF) {
	(void)fprintf(stderr, "archive:  Can't start spool, %s\n",
		strerror(errno));
	i = 1;
    }
    while (fgets(buff, sizeof buff, stdin) != NULL) 
	if (fputs(buff, F) == EOF) {
	    (void)fprintf(stderr, "archive: Can't write spool, %s\n",
		    strerror(errno));
	    i = 1;
	    break;
	}
    if (fclose(F) == EOF) {
	(void)fprintf(stderr, "archive: Can't close spool, %s\n",
	    strerror(errno));
	i = 1;
    }

    /* If we had a named input file, try to rename the spool. */
    if (p != NULL && rename(temp, av[0]) < 0) {
	(void)fprintf(stderr, "archive: Can't rename spool, %s\n",
	    strerror(errno));
	i = 1;
    }

    exit(i);
    /* NOTREACHED */
}
