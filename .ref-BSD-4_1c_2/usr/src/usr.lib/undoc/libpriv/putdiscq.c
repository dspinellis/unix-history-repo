/*	@(#)putdiscq.c	4.2	(Melbourne)	82/07/17	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <sys/stat.h>

putdiscq(uid, dq, dqf)
register uid;
register struct dquot *dq;
register char (*dqf)[32];
{
	register fd;
	register cnt;
	register bit;
	register res;
	char file[32+16];
	struct stat sb;

	cnt = 0;
	res = 0;
	bit = 1;

	while (++cnt <= 16 && **dqf) {
		strcpy(file, *dqf);
		strcat(file, "/quota");
		if (stat(file, &sb) < 0 || (fd = open(file, 1)) < 0)
			res |= bit;
		else {
			lseek(fd, (long)uid * (long)sizeof(struct dqblk), 0);
			if (write(fd, &dq->dq_dqb, sizeof(struct dqblk)) !=
			    sizeof(struct dqblk))
				res |= bit;
			close(fd);
		}
		quota(Q_SETDLIM, uid, sb.st_dev, &dq->dq_dqb);
		dq++;
		dqf++;
		bit <<= 1;
	}
	return (-res);
}
