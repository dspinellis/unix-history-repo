/*  $Revision: 1.2 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include "clibrary.h"


/*
**  Keep writing until everything has been written or we get an error.
*/
int
xwrite(fd, p, i)
    register int	fd;
    register char	*p;
    register int	i;
{
    register int	c;

    for ( ; i; p += c, i -= c)
	if ((c = write(fd, (POINTER)p, (SIZE_T)i)) <= 0)
	    return -1;
    return 0;
}
