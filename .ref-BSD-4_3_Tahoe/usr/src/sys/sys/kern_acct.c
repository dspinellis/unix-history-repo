/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_acct.c	7.2 (Berkeley) 11/25/86
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "inode.h"
#include "fs.h"
#include "kernel.h"
#include "acct.h"
#include "uio.h"

/*
 * SHOULD REPLACE THIS WITH A DRIVER THAT CAN BE READ TO SIMPLIFY.
 */
struct	inode *acctp;
struct	inode *savacctp;

/*
 * Perform process accounting functions.
 */
sysacct()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	if (suser()) {
		if (savacctp) {
			acctp = savacctp;
			savacctp = NULL;
		}
		if (uap->fname==NULL) {
			if (ip = acctp) {
				acctp = NULL;
				irele(ip);
			}
			return;
		}
		ndp->ni_nameiop = LOOKUP | FOLLOW;
		ndp->ni_segflg = UIO_USERSPACE;
		ndp->ni_dirp = uap->fname;
		ip = namei(ndp);
		if (ip == NULL)
			return;
		if ((ip->i_mode&IFMT) != IFREG) {
			u.u_error = EACCES;
			iput(ip);
			return;
		}
		if (ip->i_fs->fs_ronly) {
			u.u_error = EROFS;
			iput(ip);
			return;
		}
		if (acctp && (acctp->i_number != ip->i_number ||
		    acctp->i_dev != ip->i_dev))
			irele(acctp);
		acctp = ip;
		iunlock(ip);
	}
}

int	acctsuspend = 2;	/* stop accounting when < 2% free space left */
int	acctresume = 4;		/* resume when free space risen to > 4% */

struct	acct acctbuf;
/*
 * On exit, write a record on the accounting file.
 */
acct()
{
	register int i;
	register struct inode *ip;
	register struct fs *fs;
	register struct rusage *ru;
	off_t siz;
	struct timeval t;
	register struct acct *ap = &acctbuf;

	if (savacctp) {
		fs = savacctp->i_fs;
		if (freespace(fs, fs->fs_minfree + acctresume) > 0) {
			acctp = savacctp;
			savacctp = NULL;
			printf("Accounting resumed\n");
		}
	}
	if ((ip = acctp) == NULL)
		return;
	fs = acctp->i_fs;
	if (freespace(fs, fs->fs_minfree + acctsuspend) <= 0) {
		savacctp = acctp;
		acctp = NULL;
		printf("Accounting suspended\n");
		return;
	}
	ilock(ip);
	for (i = 0; i < sizeof (ap->ac_comm); i++)
		ap->ac_comm[i] = u.u_comm[i];
	ru = &u.u_ru;
	ap->ac_utime = compress(ru->ru_utime.tv_sec, ru->ru_utime.tv_usec);
	ap->ac_stime = compress(ru->ru_stime.tv_sec, ru->ru_stime.tv_usec);
	t = time;
	timevalsub(&t, &u.u_start);
	ap->ac_etime = compress(t.tv_sec, t.tv_usec);
	ap->ac_btime = u.u_start.tv_sec;
	ap->ac_uid = u.u_ruid;
	ap->ac_gid = u.u_rgid;
	t = ru->ru_stime;
	timevaladd(&t, &ru->ru_utime);
	if (i = t.tv_sec * hz + t.tv_usec / tick)
		ap->ac_mem = (ru->ru_ixrss+ru->ru_idrss+ru->ru_isrss) / i;
	else
		ap->ac_mem = 0;
	ap->ac_mem >>= CLSIZELOG2;
	ap->ac_io = compress(ru->ru_inblock + ru->ru_oublock, (long)0);
	if (u.u_ttyp)
		ap->ac_tty = u.u_ttyd;
	else
		ap->ac_tty = NODEV;
	ap->ac_flag = u.u_acflag;
	siz = ip->i_size;
	u.u_error = 0;				/* XXX */
	u.u_error =
	    rdwri(UIO_WRITE, ip, (caddr_t)ap, sizeof (acctbuf), siz,
		1, (int *)0);
	if (u.u_error)
		itrunc(ip, (u_long)siz);
	iunlock(ip);
}

/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
compress(t, ut)
	register long t;
	long ut;
{
	register exp = 0, round = 0;

	t = t * AHZ;  /* compiler will convert only this format to a shift */
	if (ut)
		t += ut / (1000000 / AHZ);
	while (t >= 8192) {
		exp++;
		round = t&04;
		t >>= 3;
	}
	if (round) {
		t++;
		if (t >= 8192) {
			t >>= 3;
			exp++;
		}
	}
	return ((exp<<13) + t);
}
