/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>


/*
**  "Fake" writev for sites without it.
*/
int
writev(fd, vp, vpcount)
    int			fd;
    struct iovec	*vp;
    int			vpcount;
{
    int			count;

    for (count = 0; --vpcount >= 0; count += vp->iov_len, vp++)
	if (xwrite(fd, vp->iov_base, vp->iov_len) < 0)
	    return -1;
    return count;
}
