/*	kern_acct.c	6.1	83/07/29	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/kernel.h"
#include "../h/acct.h"
#include "../h/nami.h"
#include "../h/uio.h"

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

	if (suser()) {
		if (savacctp) {
			acctp = savacctp;
			savacctp = NULL;
		}
		if (uap->fname==NULL) {
			if (ip = acctp) {
				irele(ip);
				acctp = NULL;
			}
			return;
		}
		ip = namei(uchar, LOOKUP, 1);
		if(ip == NULL)
			return;
		if((ip->i_mode & IFMT) != IFREG) {
			u.u_error = EACCES;
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
	off_t siz;
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
	ap->ac_utime = compress((long)u.u_ru.ru_utime.tv_sec);
	ap->ac_stime = compress((long)u.u_ru.ru_stime.tv_sec);
	ap->ac_etime = compress((long)time.tv_sec - u.u_start);
	ap->ac_btime = u.u_start;
	ap->ac_uid = u.u_ruid;
	ap->ac_gid = u.u_rgid;
	ap->ac_mem = 0;
	if (i = u.u_ru.ru_utime.tv_sec + u.u_ru.ru_stime.tv_sec)
		ap->ac_mem =
		    (u.u_ru.ru_ixrss + u.u_ru.ru_idrss + u.u_ru.ru_isrss) / i;
	ap->ac_io = compress((long)(u.u_ru.ru_inblock + u.u_ru.ru_oublock));
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
compress(t)
	register long t;
{
	register exp = 0, round = 0;

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
