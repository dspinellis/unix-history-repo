/*  $Revision: 1.6 $
**
**  Produce reliable locks for shell scripts, by Peter Honeyman as told
**  to Rich $alz.
*/
#include "configdata.h"
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include "clibrary.h"


STATIC BOOL	BinaryLock;
STATIC char	CANTUNLINK[] = "Can't unlink \"%s\", %s\n";
STATIC char	CANTOPEN[] = "Can't open \"%s\", %s\n";


/*
**  See if the process named in an existing lock still exists by
**  sending it a null signal.
*/
STATIC BOOL
ValidLock(name, JustChecking)
    char		*name;
    BOOL		JustChecking;
{
    register int	fd;
    register int	i;
    int			pid;
    char		buff[BUFSIZ];

    /* Open the file. */
    if ((fd = open(name, O_RDONLY)) < 0) {
	if (!JustChecking)
	    (void)fprintf(stderr, CANTOPEN, name, strerror(errno));
	return TRUE;
    }

    /* Read the PID that is written there. */
    if (BinaryLock) {
	if (read(fd, (char *)&pid, sizeof pid) != sizeof pid) {
	    (void)close(fd);
	    return FALSE;
	}
    }
    else {
	if ((i = read(fd, buff, sizeof buff - 1)) <= 0) {
	    (void)close(fd);
	    return FALSE;
	}
	buff[i] = '\0';
	pid = atoi(buff);
    }
    (void)close(fd);
    if (pid <= 0)
	return FALSE;

    /* Send the signal. */
    if (kill((PID_T)pid, 0) < 0 && errno == ESRCH)
	return FALSE;

    /* Either the kill worked, or we're optimistic about the error code. */
    return TRUE;
}


/*
**  Unlink a file, print a message on error, and exit.
*/
STATIC NORETURN
UnlinkAndExit(name, x)
    char	*name;
    int		x;
{
    if (unlink(name) < 0)
	(void)fprintf(stderr, CANTUNLINK, name, strerror(errno));
    exit(x);
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage: shlock [-u|-b] -f file -p pid\n");
    exit(1);
    /* NOTREACHED */
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register int	i;
    register char	*p;
    register int	fd;
    char		tmp[BUFSIZ];
    char		buff[BUFSIZ];
    char		*name;
    int			pid;
    BOOL		ok;
    BOOL		JustChecking;

    /* Set defaults. */
    pid = 0;
    name = NULL;
    JustChecking = FALSE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "bcup:f:")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'b':
	case 'u':
	    BinaryLock = TRUE;
	    break;
	case 'c':
	    JustChecking = TRUE;
	    break;
	case 'p':
	    pid = atoi(optarg);
	    break;
	case 'f':
	    name = optarg;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac || pid == 0 || name == NULL)
	Usage();

    /* Create the temp file in the same directory as the destination. */
    if ((p = strrchr(name, '/')) != NULL) {
	*p = '\0';
	(void)sprintf(tmp, "%s/shlock%ld", name, (long)getpid());
	*p = '/';
    }
    else
	(void)sprintf(tmp, "shlock%ld", (long)getpid());

    /* Loop until we can open the file. */
    while ((fd = open(tmp, O_RDWR | O_CREAT | O_EXCL, 0644)) < 0)
	switch (errno) {
	default:
	    /* Unknown error -- give up. */
	    (void)fprintf(stderr, CANTOPEN, tmp, strerror(errno));
	    exit(1);
	case EEXIST:
	    /* If we can remove the old temporary, retry the open. */
	    if (unlink(tmp) < 0) {
		(void)fprintf(stderr, CANTUNLINK, tmp, strerror(errno));
		exit(1);
	    }
	    break;
	}

    /* Write the process ID. */
    if (BinaryLock)
	ok = write(fd, (POINTER)pid, (SIZE_T)sizeof pid) == sizeof pid;
    else {
	(void)sprintf(buff, "%d\n", pid);
	i = strlen(buff);
	ok = write(fd, (POINTER)buff, (SIZE_T)i) == i;
    }
    if (!ok) {
	(void)fprintf(stderr, "Can't write PID to \"%s\", %s\n",
	    tmp, strerror(errno));
	(void)close(fd);
	UnlinkAndExit(tmp, 1);
    }

    (void)close(fd);

    /* Handle the "-c" flag. */
    if (JustChecking) {
	if (ValidLock(name, TRUE))
	    UnlinkAndExit(tmp, 1);
	UnlinkAndExit(tmp, 0);
    }

    /* Try to link the temporary to the lockfile. */
    while (link(tmp, name) < 0)
	switch (errno) {
	default:
	    /* Unknown error -- give up. */
	    (void)fprintf(stderr, "Can't link \"%s\" to \"%s\", %s\n",
		    tmp, name, strerror(errno));
	    UnlinkAndExit(tmp, 1);
	    /* NOTREACHED */
	case EEXIST:
	    /* File exists; if lock is valid, give up. */
	    if (ValidLock(name, FALSE))
		UnlinkAndExit(tmp, 1);
	    if (unlink(name) < 0) {
		(void)fprintf(stderr, CANTUNLINK, name, strerror(errno));
		UnlinkAndExit(tmp, 1);
	    }
	}

    UnlinkAndExit(tmp, 0);
    /* NOTREACHED */
}
