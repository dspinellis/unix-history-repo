/*	ufs_xxx.c	6.1	83/07/29	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"

/*
 * Return the next character fromt the
 * kernel string pointed at by dirp.
 */
schar()
{
	return (*u.u_dirp++ & 0377);
}

/*
 * Return the next character from the
 * user string pointed at by dirp.
 */
uchar()
{
	register c;

	c = fubyte(u.u_dirp++);
	if (c == -1) {
		u.u_error = EFAULT;
		c = 0;
	}
	return (c);
}

#ifdef COMPAT
#include "../h/file.h"
#include "../h/nami.h"
#include "../h/kernel.h"

/*
 * Oh, how backwards compatibility is ugly!!!
 */
struct	ostat {
	dev_t	ost_dev;
	u_short	ost_ino;
	u_short ost_mode;
	short  	ost_nlink;
	short  	ost_uid;
	short  	ost_gid;
	dev_t	ost_rdev;
	int	ost_size;
	int	ost_atime;
	int	ost_mtime;
	int	ost_ctime;
};

/*
 * The old fstat system call.
 */
ofstat()
{
	register struct file *fp;
	register struct a {
		int	fd;
		struct ostat *sb;
	} *uap = (struct a *)u.u_ap;
	extern struct file *getinode();

	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ostat1((struct inode *)fp->f_data, uap->sb);
}

/*
 * Old stat system call.  This version follows links.
 */
ostat()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		struct ostat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	ostat1(ip, uap->sb);
	iput(ip);
}

ostat1(ip, ub)
	register struct inode *ip;
	struct ostat *ub;
{
	struct ostat ds;

	IUPDAT(ip, &time, &time, 0);
	/*
	 * Copy from inode table
	 */
	ds.ost_dev = ip->i_dev;
	ds.ost_ino = (short)ip->i_number;
	ds.ost_mode = (u_short)ip->i_mode;
	ds.ost_nlink = ip->i_nlink;
	ds.ost_uid = (short)ip->i_uid;
	ds.ost_gid = (short)ip->i_gid;
	ds.ost_rdev = (dev_t)ip->i_rdev;
	ds.ost_size = (int)ip->i_size;
	ds.ost_atime = (int)ip->i_atime;
	ds.ost_mtime = (int)ip->i_mtime;
	ds.ost_ctime = (int)ip->i_ctime;
	u.u_error = copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds));
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
outime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip;
	time_t tv[2];
	struct timeval tv0, tv1;

	if ((ip = owner(1)) == NULL)
		return;
	u.u_error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
	if (u.u_error == 0) {
		ip->i_flag |= IACC|IUPD|ICHG;
		tv0.tv_sec = tv[0]; tv0.tv_usec = 0;
		tv1.tv_sec = tv[1]; tv1.tv_usec = 0;
		iupdat(ip, &tv0, &tv1, 0);
	}
	iput(ip);
}
#endif
