/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include <errno.h>
#include "configdata.h"
#include "clibrary.h"


/*
**  Memory failure handler; print an error and exit.
*/
STATIC int
xmemerr(what, i)
    char		*what;
    unsigned int	i;
{
    /* We want large values to show up as negative, hence %d. */
    (void)fprintf(stderr, "Can't %s %d bytes, %s", what, i, strerror(errno));
    exit(1);
    /* NOTREACHED */
}

int (*xmemfailure)() = xmemerr;
