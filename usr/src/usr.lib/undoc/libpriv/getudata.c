/*	@(#)getudata.c	4.1	(Melbourne)	82/01/04	*/

#include <sys/types.h>
#include <udata.h>

getudata(uid, up)
register uid;
register struct udata *up;
{
	register fd;
	register res;

	if ((fd = open(UPRIVFILE, 0)) < 0) {
		perror(UPRIVFILE);
		return(-1);
	}

	lseek(fd, (long)uid * (long)sizeof(struct udata), 0);
	switch (read(fd, up, sizeof(struct udata))) {
	default:
		res = 1;
		break;
	case sizeof(struct udata):
		res = 0;
		break;
	case -1:
		res = -1;
		break;
	}
	close(fd);
	return(res);
}
