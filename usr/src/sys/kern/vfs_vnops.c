/*	vfs_vnops.c	4.30	82/11/13	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/fs.h"
#include "../h/file.h"
#include "../h/conf.h"
#include "../h/inode.h"
#include "../h/reg.h"
#include "../h/acct.h"
#include "../h/mount.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/proc.h"
#include "../h/nami.h"

/*
 * Openi called to allow handler
 * of special files to initialize and
 * validate before actual IO.
 */
openi(ip, mode)
	register struct inode *ip;
{
	dev_t dev = (dev_t)ip->i_rdev;
	register u_int maj = major(dev);

	switch (ip->i_mode&IFMT) {

	case IFCHR:
		if (maj >= nchrdev)
			return (ENXIO);
		return ((*cdevsw[maj].d_open)(dev, mode));

	case IFBLK:
		if (maj >= nblkdev)
			return (ENXIO);
		return ((*bdevsw[maj].d_open)(dev, mode));
	}
	return (0);
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
	register int *gp;

	m = mode;
	if (m == IWRITE) {
		if (ip->i_fs->fs_ronly != 0) {
			if ((ip->i_mode & IFMT) != IFCHR &&
			    (ip->i_mode & IFMT) != IFBLK) {
				u.u_error = EROFS;
				return (1);
			}
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
		for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++)
			if (ip->i_gid != *gp)
				goto found;
		m >>= 3;
found:
		;
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
owner(follow)
	int follow;
{
	register struct inode *ip;

	ip = namei(uchar, LOOKUP, follow);
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
