/*  $Revision: 1.4 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "configdata.h"
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Read a big amount, looping until it is all done.  Return TRUE if
**  successful.
*/
int
xread(fd, p, i)
    register int	fd;
    register char	*p;
    register off_t	i;
{
    register int	count;

    for ( ; i; p += count, i -= count)
	if ((count = read(fd, p, (SIZE_T)i)) <= 0)
	    return -1;
    return 0;
}


/*
**  Read an already-open file into memory.
*/
char *
ReadInDescriptor(fd, Sbp)
    int		fd;
    struct stat	*Sbp;
{
    struct stat	mystat;
    char	*p;
    int		oerrno;

    if (Sbp == NULL)
	Sbp = &mystat;

    /* Get the size, and enough memory. */
    if (fstat(fd, Sbp) < 0) {
	oerrno = errno;
	(void)close(fd);
	errno = oerrno;
	return NULL;
    }
    p = NEW(char, Sbp->st_size + 1);

    /* Slurp, slurp. */
    if (xread(fd, p, Sbp->st_size) < 0) {
	oerrno = errno;
	DISPOSE(p);
	(void)close(fd);
	errno = oerrno;
	return NULL;
    }

    /* Terminate the string; terminate the routine. */
    p[Sbp->st_size] = '\0';
    return p;
}


/*
**  Read a file into allocated memory.  Optionally fill in the stat(2) data.
**  Return a pointer to the file contents, or NULL on error.
*/
char *
ReadInFile(name, Sbp)
    char	*name;
    struct stat	*Sbp;
{
    char	*p;
    int		fd;

    if ((fd = open(name, O_RDONLY)) < 0)
	return NULL;

    p = ReadInDescriptor(fd, Sbp);
    (void)close(fd);
    return p;
}
