/*  $Revision: 1.3 $
**
**  Parse input to add to news overview database.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <fcntl.h>
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"
#include "paths.h"
#include "qio.h"


STATIC BOOL	InSpoolDir;


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
**  Make overview directory if not in spool directory.  Return 0 if ok,
**  else -1.
*/
STATIC BOOL
MakeOverDir(Name)
    register char	*Name;
{
    register char	*p;
    BOOL		made;

    if (InSpoolDir)
	return FALSE;

    /* Optimize common case -- parent almost always exists. */
    if (MakeDir(Name))
	return TRUE;

    /* Try to make each of comp and comp/foo in turn. */
    for (p = Name; *p; p++)
	if (*p == '/') {
	    *p = '\0';
	    made = MakeDir(Name);
	    *p = '/';
	    if (!made)
		return FALSE;
	}

    return MakeDir(Name);
}


/*
**  Get the lock for the group, then open the data file and append the
**  new data.  Return FALSE on error.
*/
STATIC BOOL
WriteData(Dir, Art, Rest)
    char		*Dir;
    char		*Art;
    char		*Rest;
{
    static char		TAB[] = "\t";
    static char		NL[] = "\n";
    struct iovec	iov[4];
    register int	fd;
    register int	i;
    register BOOL	ok;
    char		file[SPOOLNAMEBUFF];
    struct stat		Sb;

    /* Build the I/O vector. */
    i = 0;
    iov[i].iov_base = Art;
    iov[i++].iov_len = strlen(Art);
    iov[i].iov_base = TAB;
    iov[i++].iov_len = 1;
    iov[i].iov_base = Rest;
    iov[i++].iov_len = strlen(Rest);
    iov[i].iov_base = NL;
    iov[i++].iov_len = 1;

    /* Name the data file. */
    (void)sprintf(file, "%s/%s", Dir, _PATH_OVERVIEW);

    /* Open and lock the file. */
    for ( ; ; ) {
	if ((fd = open(file, O_WRONLY | O_CREAT | O_APPEND, ARTFILE_MODE)) < 0) {
	    (void)fprintf(stderr, "overchan cant open %s, %s\n",
		    file, strerror(errno));
	    return FALSE;
	}
	if (LockFile(fd, FALSE) < 0)
	    /* Wait for it. */
	    (void)LockFile(fd, TRUE);
	else {
	    /* Got the lock; make sure the file is still there. */
	    if (fstat(fd, &Sb) < 0) {
		(void)fprintf(stderr, "overchan cant fstat %s, %s\n",
			file, strerror(errno));
		(void)close(fd);
		return FALSE;
	    }
	    if (Sb.st_nlink > 0)
		break;
	}
	/* Close file -- expireover might have removed it -- and try again. */
	(void)close(fd);
    }

    ok = TRUE;
    if (xwritev(fd, iov, i) < 0) {
	(void)fprintf(stderr, "overchan cant write %s %s\n",
		file, strerror(errno));
	ok = FALSE;
    }

    /* Close up and return. */
    if (close(fd) < 0) {
	(void)fprintf(stderr, "overchan cant close %s %s\n",
		file, strerror(errno));
	ok = FALSE;
    }
    return ok;
}


/*
**  Process the input.  Data can come from innd:
**	news/group/name/<number> [space news/group/<number>]... \t data
**  or from mkov:
**	news/group/name \t number \t data
*/
STATIC void
ProcessIncoming(INNinput, qp)
    BOOL		INNinput;
    QIOSTATE		*qp;
{
    register char	*Dir;
    register char	*Art;
    register char	*Rest;
    register char	*p;

    for ( ; ; ) {
	/* Read the first line of data. */
	if ((p = QIOread(qp)) == NULL) {
	    if (QIOtoolong(qp)) {
		(void)fprintf(stderr, "overchan line too long\n");
		continue;
	    }
	    break;
	}

	/* If doing mkov output, process that line and continue. */
	if (!INNinput) {
	    /* Split up the fields. */
	    Dir = p;
	    if ((p = strchr(p, '\t')) == NULL || p[1] == '\0') {
		(void)fprintf(stderr, "overchan missing field 1: %s\n", Dir);
		return;
	    }
	    *p++ = '\0';
	    Art = p;
	    if ((p = strchr(p, '\t')) == NULL || p[1] == '\0') {
		(void)fprintf(stderr, "overchan missing field 2: %s\n", Dir);
		return;
	    }
	    *p++ = '\0';

	    /* Write data. */
	    if (!WriteData(Dir, Art, p)
	     && (!MakeOverDir(Dir) || !WriteData(Dir, Art, p)))
		(void)fprintf(stderr, "overchan cant update %s %s\n",
			Dir, strerror(errno));
	    continue;
	}

	/* Nip off the first part. */
	Dir = p;
	if ((Rest = strchr(p, '\t')) == NULL) {
	    (void)fprintf(stderr, "overchan bad input\n");
	    continue;
	}
	*Rest++ = '\0';

	/* Process all fields in the first part. */
	for ( ; *Dir; Dir = p) {

	    /* Split up this field, then split it up. */
	    for (p = Dir; *p; p++)
		if (ISWHITE(*p)) {
		    *p++ = '\0';
		    break;
		}

	    if ((Art = strrchr(Dir, '/')) == NULL || Art[1] == '\0') {
		(void)fprintf(stderr, "overchan bad entry %s\n", Dir);
		continue;
	    }
	    *Art++ = '\0';

	    /* Write data. */
	    if (!WriteData(Dir, Art, Rest)
	     && (!MakeOverDir(Dir) || !WriteData(Dir, Art, Rest)))
		(void)fprintf(stderr, "overchan cant update %s %s\n",
			Dir, strerror(errno));
	}
    }

    if (QIOerror(qp))
	(void)fprintf(stderr, "overchan cant read %s\n", strerror(errno));
    QIOclose(qp);
}


STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "usage:  overchan [-c] [-D dir] [files...]\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register int	i;
    QIOSTATE		*qp;
    char		*Dir;
    BOOL		INNinput;

    /* Set defaults. */
    Dir = _PATH_OVERVIEWDIR;
    INNinput = TRUE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "cD:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'c':
	    INNinput = FALSE;
	    break;
	case 'D':
	    Dir = optarg;
	    break;
	}
    ac -= optind;
    av += optind;
    InSpoolDir = EQ(Dir, _PATH_SPOOL);

    if (chdir(Dir) < 0) {
	(void)fprintf(stderr, "overchan cant chdir %s %s\n",
		Dir, strerror(errno));
	exit(1);
    }

    if (ac == 0)
	ProcessIncoming(INNinput, QIOfdopen(STDIN, QIO_BUFFER));
    else {
	for ( ; *av; av++)
	    if (EQ(*av, "-"))
		ProcessIncoming(INNinput, QIOfdopen(STDIN, QIO_BUFFER));
	    else if ((qp = QIOopen(*av, QIO_BUFFER)) == NULL)
		(void)fprintf(stderr, "overchan cant open %s %s\n",
			*av, strerror(errno));
	    else
		ProcessIncoming(INNinput, qp);
    }

    exit(0);
    /* NOTREACHED */
}
