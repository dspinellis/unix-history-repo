/*	@(#)putlpquota.c	4.1	(Melbourne)	82/02/20	*/

#include <sys/types.h>
#include <lpdquota.h>

putlpquota(uid, lp)
register uid;
register struct lpquota *lp;
{
	register fd;
	register res;

	if ((fd = open(QFILE, 1)) < 0) {
		perror(QFILE);
		return(-1);
	}

	lseek(fd, (long)uid * (long)sizeof(struct lpquota), 0);
	switch (write(fd, lp, sizeof(struct lpquota))) {
	default:
		res = -1;
		break;
	case sizeof(struct lpquota):
		res = 0;
		break;
	}
	close(fd);
	return(res);
}
