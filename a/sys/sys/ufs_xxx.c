/*	ufs_xxx.c	4.4	82/12/28	*/

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

#ifndef NOCOMPAT
#include "../h/file.h"
#include "../h/nami.h"
#include "../h/descrip.h"
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
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		struct ostat ub;

		bzero((caddr_t)&ub, sizeof (ub));
		(void) copyout((caddr_t)&ub, (caddr_t)uap->sb, sizeof (ub));
	} else
		ostat1(fp->f_inode, uap->sb);
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
#endif
