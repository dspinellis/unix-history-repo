/*	vfs_vnops.c	4.14	81/11/08	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/filsys.h"
#include "../h/file.h"
#include "../h/conf.h"
#include "../h/inode.h"
#include "../h/reg.h"
#include "../h/acct.h"
#include "../h/mount.h"

/*
 * Convert a user supplied file descriptor into a pointer
 * to a file structure.  Only task is to check range of the descriptor.
 * Critical paths should use the GETF macro, defined in inline.h.
 */
struct file *
getf(f)
	register int f;
{
	register struct file *fp;

	if ((unsigned)f >= NOFILE || (fp = u.u_ofile[f]) == NULL) {
		u.u_error = EBADF;
		return (NULL);
	}
	return (fp);
}

/*
 * Internal form of close.
 * Decrement reference count on
 * file structure.
 * Also make sure the pipe protocol
 * does not constipate.
 *
 * Decrement reference count on the inode following
 * removal to the referencing file structure.
 * Call device handler on last close.
 */
closef(fp)
	register struct file *fp;
{
	register struct inode *ip;
	register struct mount *mp;
	int flag, mode;
	dev_t dev;
	register int (*cfunc)();

	if (fp == NULL)
		return;
	if (fp->f_count > 1) {
		fp->f_count--;
		return;
	}
	flag = fp->f_flag;
	if (flag & FSOCKET) {
		skclose(fp->f_socket);
		fp->f_socket = 0;
		fp->f_count = 0;
		return;
	}
	ip = fp->f_inode;
	dev = (dev_t)ip->i_un.i_rdev;
	mode = ip->i_mode & IFMT;
	ilock(ip);
	iput(ip);
	fp->f_count = 0;

	switch (mode) {

	case IFCHR:
		cfunc = cdevsw[major(dev)].d_close;
		break;

	case IFBLK:
		/*
		 * We don't want to really close the device if it is mounted
		 */
		for (mp = mount; mp < &mount[NMOUNT]; mp++)
			if (mp->m_bufp != NULL && mp->m_dev == dev)
				return;
		cfunc = bdevsw[major(dev)].d_close;
		break;

	default:
		return;
	}
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_flag & FSOCKET)
			continue;
		if (fp->f_count && (ip = fp->f_inode) &&
		    ip->i_un.i_rdev == dev && (ip->i_mode&IFMT) == mode)
			return;
	}
	if (mode == IFBLK) {
		/*
		 * On last close of a block device (that isn't mounted)
		 * we must invalidate any in core blocks
		 */
		bflush(dev);
		binval(dev);
	}
	(*cfunc)(dev, flag, fp);
}

/*
 * Openi called to allow handler
 * of special files to initialize and
 * validate before actual IO.
 */
openi(ip, rw)
	register struct inode *ip;
{
	dev_t dev;
	register unsigned int maj;

	dev = (dev_t)ip->i_un.i_rdev;
	maj = major(dev);
	switch (ip->i_mode&IFMT) {

	case IFCHR:
		if (maj >= nchrdev)
			goto bad;
		(*cdevsw[maj].d_open)(dev, rw);
		break;

	case IFBLK:
		if (maj >= nblkdev)
			goto bad;
		(*bdevsw[maj].d_open)(dev, rw);
	}
	return;

bad:
	u.u_error = ENXIO;
}

/*
 * Check mode permission on inode pointer.
 * Mode is READ, WRITE or EXEC.
 * In the case of WRITE, the
 * read-only status of the file
 * system is checked.
 * Also in WRITE, prototype text
 * segments cannot be written.
 * The mode is shifted to select
 * the owner/group/other fields.
 * The super user is granted all
 * permissions.
 */
access(ip, mode)
	register struct inode *ip;
	int mode;
{
	register m;

	m = mode;
	if (m == IWRITE) {
		if (getfs(ip->i_dev)->s_ronly != 0) {
			u.u_error = EROFS;
			return (1);
		}
		if (ip->i_flag&ITEXT)		/* try to free text */
			xrele(ip);
		if (ip->i_flag & ITEXT) {
			u.u_error = ETXTBSY;
			return (1);
		}
	}
	if (u.u_uid == 0)
		return (0);
	if (u.u_uid != ip->i_uid) {
		m >>= 3;
		if (u.u_gid != ip->i_gid)
			m >>= 3;
	}
	if ((ip->i_mode&m) != 0)
		return (0);
	u.u_error = EACCES;
	return (1);
}

/*
 * Look up a pathname and test if
 * the resultant inode is owned by the
 * current user.
 * If not, try for super-user.
 * If permission is granted,
 * return inode pointer.
 */
struct inode *
owner()
{
	register struct inode *ip;

	ip = namei(uchar, 0);
	if (ip == NULL)
		return (NULL);
	if (u.u_uid == ip->i_uid)
		return (ip);
	if (suser())
		return (ip);
	iput(ip);
	return (NULL);
}

/*
 * Test if the current user is the
 * super user.
 */
suser()
{

	if (u.u_uid == 0) {
		u.u_acflag |= ASU;
		return (1);
	}
	u.u_error = EPERM;
	return (0);
}

/*
 * Allocate a user file descriptor.
 */
ufalloc()
{
	register i;

	for (i=0; i<NOFILE; i++)
		if (u.u_ofile[i] == NULL) {
			u.u_r.r_val1 = i;
			u.u_pofile[i] = 0;
			return (i);
		}
	u.u_error = EMFILE;
	return (-1);
}

struct	file *lastf;
/*
 * Allocate a user file descriptor
 * and a file structure.
 * Initialize the descriptor
 * to point at the file structure.
 */
struct file *
falloc()
{
	register struct file *fp;
	register i;

	i = ufalloc();
	if (i < 0)
		return (NULL);
	if (lastf == 0)
		lastf = file;
	for (fp = lastf; fp < fileNFILE; fp++)
		if (fp->f_count == 0)
			goto slot;
	for (fp = file; fp < lastf; fp++)
		if (fp->f_count == 0)
			goto slot;
	tablefull("file");
	u.u_error = ENFILE;
	return (NULL);
slot:
	u.u_ofile[i] = fp;
	fp->f_count++;
	fp->f_offset = 0;
	fp->f_inode = 0;
	lastf = fp + 1;
	return (fp);
}
