/*	@(#)getdiscq.c	4.1	(Melbourne)	82/02/06	*/

#include <sys/types.h>
#include <sys/quota.h>
#include <fstab.h>
#include <sys/stat.h>
#define	NULL	((struct fstab *)0)

getdiscq(uid, dq, dqf)
register uid;
register struct dquot *dq;
register char (*dqf)[32];
{
	register struct fstab *fs;

	setfsent();
	while ((fs = getfsent()) != NULL) {
		struct	stat statb;
		struct	dqblk dqblk;

		if (stat(fs->fs_spec, &statb) < 0)
			continue;

		if (quota(Q_GETDLIM, uid, statb.st_rdev, &dqblk) != 0)
			continue;
		
		dq->dq_dqb = dqblk;
		dq->dq_dev = statb.st_rdev;
		strcpy(dqf[0], fs->fs_file);
		dq++;
		dqf++;
	}
	endfsent();
	dqf[0][0] = 0;
}
