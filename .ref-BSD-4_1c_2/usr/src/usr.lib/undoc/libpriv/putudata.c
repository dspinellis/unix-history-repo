/*	@(#)putudata.c	4.2	(Melbourne)	82/02/20	*/

#include <sys/types.h>
#include <udata.h>

putudata(uid, up)
register uid;
register struct udata *up;
{
	register fd;
	register res;

	if ((fd = open(UPRIVFILE, 1)) < 0) {
		perror(UPRIVFILE);
		return(-1);
	}

	lseek(fd, (long)uid * (long)sizeof(struct udata), 0);
	switch (write(fd, up, sizeof(struct udata))) {
	default:
		res = -1;
		break;
	case sizeof(struct udata):
		res = 0;
		break;
	}
	close(fd);
	return(res);
}
