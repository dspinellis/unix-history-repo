/*  $Revision: 1.3 $
**
*/
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include "clibrary.h"


/*
**  Do a writev, return -1 on error; 0 if okay.  Handling the return value
**  of writev is a pain.  It should return the number of iov's it fully
**  wrote out, and update the fields in all of them to contain the new
**  startpoints.
*/
int
xwritev(fd, vp, vpcount)
    int			fd;
    struct iovec	*vp;
    register int	vpcount;
{
    register int	i;
    register long	left;

    /* Get the total bytecount. */
    for (left = 0, i = vpcount; --i >= 0; )
	left += vp[i].iov_len;

    while (vpcount) {
	if ((i = writev(fd, vp, (SIZE_T)vpcount)) < 0)
	    return -1;
	if ((left -= i) <= 0)
	    break;
	for (; i >= vp->iov_len; vp++, vpcount--)
	    i -= vp->iov_len;
	vp->iov_base += i;
	vp->iov_len -= i;
    }
    return 0;
}
