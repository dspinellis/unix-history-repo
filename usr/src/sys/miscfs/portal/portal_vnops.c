/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)portal_vnops.c	8.2 (Berkeley) %G%
 *
 * $Id: portal_vnops.c,v 1.4 1992/05/30 10:05:24 jsp Exp jsp $
 */

/*
 * Portal Filesystem
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <sys/filedesc.h>
#include <sys/vnode.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mount.h>
#include <sys/malloc.h>
#include <sys/namei.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/un.h>
#include <sys/unpcb.h>
#include <miscfs/portal/portal.h>

static int portal_fileid = PORTAL_ROOTFILEID+1;

static void
portal_closefd(p, fd)
	struct proc *p;
	int fd;
{
	int error;
	struct {
		int fd;
	} ua;
	int rc;

	ua.fd = fd;
	error = close(p, &ua, &rc);
	/*
	 * We should never get an error, and there isn't anything
	 * we could do if we got one, so just print a message.
	 */
	if (error)
		printf("portal_closefd: error = %d\n", error);
}

/*
 * vp is the current namei directory
 * cnp is the name to locate in that directory...
 */
portal_lookup(ap)
	struct vop_lookup_args /* {
		struct vnode * a_dvp;
		struct vnode ** a_vpp;
		struct componentname * a_cnp;
	} */ *ap;
{
	char *pname = ap->a_cnp->cn_nameptr;
	struct portalnode *pt;
	int error;
	struct vnode *fvp = 0;
	char *path;
	int size;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_lookup(%s)\n", pname);
#endif
	if (ap->a_cnp->cn_namelen == 1 && *pname == '.') {
		*ap->a_vpp = ap->a_dvp;
		VREF(ap->a_dvp);
		/*VOP_LOCK(ap->a_dvp);*/
		return (0);
	}


#ifdef PORTAL_DIAGNOSTIC
	printf("portal_lookup: allocate new vnode\n");
#endif
	error = getnewvnode(VT_PORTAL, ap->a_dvp->v_mount, portal_vnodeop_p, &fvp);
	if (error)
		goto bad;
	fvp->v_type = VREG;
	MALLOC(fvp->v_data, void *, sizeof(struct portalnode),
		M_TEMP, M_WAITOK);

	pt = VTOPORTAL(fvp);
	/*
	 * Save all of the remaining pathname and
	 * advance the namei next pointer to the end
	 * of the string.
	 */
	for (size = 0, path = pname; *path; path++)
		size++;
	ap->a_cnp->cn_consume = size - ap->a_cnp->cn_namelen;

	pt->pt_arg = malloc(size+1, M_TEMP, M_WAITOK);
	pt->pt_size = size+1;
	bcopy(pname, pt->pt_arg, pt->pt_size);
	pt->pt_fileid = portal_fileid++;

	*ap->a_vpp = fvp;
	/*VOP_LOCK(fvp);*/
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_lookup: newvp = %x\n", fvp);
#endif
	return (0);

bad:;
	if (fvp) {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_lookup: vrele(%x)\n", fvp);
#endif
		vrele(fvp);
	}
	*ap->a_vpp = NULL;
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_lookup: error = %d\n", error);
#endif
	return (error);
}

static int
portal_connect(so, so2)
	struct socket *so;
	struct socket *so2;
{
	/* from unp_connect, bypassing the namei stuff... */

	struct socket *so3;
	struct unpcb *unp2;
	struct unpcb *unp3;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_connect\n");
#endif

	if (so2 == 0)
		return (ECONNREFUSED);

	if (so->so_type != so2->so_type)
		return (EPROTOTYPE);

	if ((so2->so_options & SO_ACCEPTCONN) == 0)
		return (ECONNREFUSED);

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_connect: calling sonewconn\n");
#endif

	if ((so3 = sonewconn(so2, 0)) == 0)
		return (ECONNREFUSED);

	unp2 = sotounpcb(so2);
	unp3 = sotounpcb(so3);
	if (unp2->unp_addr)
		unp3->unp_addr = m_copy(unp2->unp_addr, 0, (int)M_COPYALL);

	so2 = so3;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_connect: calling unp_connect2\n");
#endif

	return (unp_connect2(so, so2));
}

portal_open(ap)
	struct vop_open_args /* {
		struct vnode *a_vp;
		int  a_mode;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	struct socket *so = 0;
	struct portalnode *pt;
	struct proc *p = ap->a_p;
	struct vnode *vp = ap->a_vp;
	int s;
	struct uio auio;
	struct iovec aiov[2];
	int res;
	struct mbuf *cm = 0;
	struct cmsghdr *cmsg;
	int newfds;
	int *ip;
	int fd;
	int error;
	int len;
	struct portalmount *fmp;
	struct file *fp;
	struct portal_cred pcred;

	/*
	 * Nothing to do when opening the root node.
	 */
	if (vp->v_flag & VROOT)
		return (0);

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open(%x)\n", vp);
#endif

	/*
	 * Can't be opened unless the caller is set up
	 * to deal with the side effects.  Check for this
	 * by testing whether the p_dupfd has been set.
	 */
	if (p->p_dupfd >= 0)
		return (ENODEV);

	pt = VTOPORTAL(vp);
	fmp = VFSTOPORTAL(vp->v_mount);

	/*
	 * Create a new socket.
	 */
	error = socreate(AF_UNIX, &so, SOCK_STREAM, 0);
	if (error)
		goto bad;

	/*
	 * Reserve some buffer space
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: calling soreserve\n");
#endif
	res = pt->pt_size + sizeof(pcred) + 512;	/* XXX */
	error = soreserve(so, res, res);
	if (error)
		goto bad;

	/*
	 * Kick off connection
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: calling portal_connect\n");
#endif
	error = portal_connect(so, (struct socket *)fmp->pm_server->f_data);
	if (error)
		goto bad;

	/*
	 * Wait for connection to complete
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: waiting for connect\n");
#endif
	/*
	 * XXX: Since the mount point is holding a reference on the
	 * underlying server socket, it is not easy to find out whether
	 * the server process is still running.  To handle this problem
	 * we loop waiting for the new socket to be connected (something
	 * which will only happen if the server is still running) or for
	 * the reference count on the server socket to drop to 1, which
	 * will happen if the server dies.  Sleep for 5 second intervals
	 * and keep polling the reference count.   XXX.
	 */
	s = splnet();
	while ((so->so_state & SS_ISCONNECTING) && so->so_error == 0) {
		if (fmp->pm_server->f_count == 1) {
			error = ECONNREFUSED;
			splx(s);
#ifdef PORTAL_DIAGNOSTIC
			printf("portal_open: server process has gone away\n");
#endif
			goto bad;
		}
		(void) tsleep((caddr_t) &so->so_timeo, PSOCK, "portalcon", 5 * hz);
	}
	splx(s);

	if (so->so_error) {
		error = so->so_error;
		goto bad;
	}
		
	/*
	 * Set miscellaneous flags
	 */
	so->so_rcv.sb_timeo = 0;
	so->so_snd.sb_timeo = 0;
	so->so_rcv.sb_flags |= SB_NOINTR;
	so->so_snd.sb_flags |= SB_NOINTR;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: constructing data uio\n");
#endif

	pcred.pcr_flag = ap->a_mode;
	pcred.pcr_uid = ap->a_cred->cr_uid;
	pcred.pcr_ngroups = ap->a_cred->cr_ngroups;
	bcopy(ap->a_cred->cr_groups, pcred.pcr_groups, NGROUPS * sizeof(gid_t));
	aiov[0].iov_base = (caddr_t) &pcred;
	aiov[0].iov_len = sizeof(pcred);
	aiov[1].iov_base = pt->pt_arg;
	aiov[1].iov_len = pt->pt_size;
	auio.uio_iov = aiov;
	auio.uio_iovcnt = 2;
	auio.uio_rw = UIO_WRITE;
	auio.uio_segflg = UIO_SYSSPACE;
	auio.uio_procp = p;
	auio.uio_offset = 0;
	auio.uio_resid = aiov[0].iov_len + aiov[1].iov_len;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: sending data to server\n");
#endif
	error = sosend(so, (struct sockaddr *) 0, &auio,
			(struct mbuf *) 0, (struct mbuf *) 0, 0);
	if (error)
		goto bad;

	len = auio.uio_resid = sizeof(int);
	do {
		struct mbuf *m = 0;
		int flags = MSG_WAITALL;
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: receiving data from server\n");
		printf("portal_open: so = %x, cm = %x, resid = %d\n",
				so, cm, auio.uio_resid);
		printf("portal_open, uio=%x, mp0=%x, controlp=%x\n", &auio, &cm);
#endif
		error = soreceive(so, (struct mbuf **) 0, &auio,
					&m, &cm, &flags);
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: after receiving data\n");
		printf("portal_open: so = %x, cm = %x, resid = %d\n",
				so, cm, auio.uio_resid);
#endif
		if (error)
			goto bad;

		/*
		 * Grab an error code from the mbuf.
		 */
		if (m) {
			m = m_pullup(m, sizeof(int));	/* Needed? */
			if (m) {
				error = *(mtod(m, int *));
				m_freem(m);
			} else {
				error = EINVAL;
			}
#ifdef PORTAL_DIAGNOSTIC
			printf("portal_open: error returned is %d\n", error);
#endif
		} else {
			if (cm == 0) {
#ifdef PORTAL_DIAGNOSTIC
				printf("portal_open: no rights received\n");
#endif
				error = ECONNRESET;	 /* XXX */
#ifdef notdef
				break;
#endif
			}
		}
	} while (cm == 0 && auio.uio_resid == len && !error);

	if (cm == 0)
		goto bad;

	if (auio.uio_resid) {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: still need another %d bytes\n", auio.uio_resid);
#endif
		error = 0;
#ifdef notdef
		error = EMSGSIZE;
		goto bad;
#endif
	}

	/*
	 * XXX: Break apart the control message, and retrieve the
	 * received file descriptor.  Note that more than one descriptor
	 * may have been received, or that the rights chain may have more
	 * than a single mbuf in it.  What to do?
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: about to break apart control message\n");
#endif
	cmsg = mtod(cm, struct cmsghdr *);
	newfds = (cmsg->cmsg_len - sizeof(*cmsg)) / sizeof (int);
	if (newfds == 0) {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: received no fds\n");
#endif
		error = ECONNREFUSED;
		goto bad;
	}
	/*
	 * At this point the rights message consists of a control message
	 * header, followed by a data region containing a vector of
	 * integer file descriptors.  The fds were allocated by the action
	 * of receiving the control message.
	 */
	ip = (int *) (cmsg + 1);
	fd = *ip++;
	if (newfds > 1) {
		/*
		 * Close extra fds.
		 */
		int i;
		printf("portal_open: %d extra fds\n", newfds - 1);
		for (i = 1; i < newfds; i++) {
			portal_closefd(p, *ip);
			ip++;
		}
	}

	/*
	 * Check that the mode the file is being opened for is a subset 
	 * of the mode of the existing descriptor.
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: checking file flags, fd = %d\n", fd);
#endif
 	fp = p->p_fd->fd_ofiles[fd];
	if (((ap->a_mode & (FREAD|FWRITE)) | fp->f_flag) != fp->f_flag) {
		portal_closefd(p, fd);
		error = EACCES;
		goto bad;
	}

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_open: got fd = %d\n", fd);
#endif
	/*
	 * Save the dup fd in the proc structure then return the
	 * special error code (ENXIO) which causes magic things to
	 * happen in vn_open.  The whole concept is, well, hmmm.
	 */
	p->p_dupfd = fd;
	error = ENXIO;

bad:;
	/*
	 * And discard the control message.
	 */
	if (cm) { 
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: free'ing control message\n");
#endif
		m_freem(cm);
	}

	if (so) {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: calling soshutdown\n");
#endif
		soshutdown(so, 2);
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_open: calling soclose\n");
#endif
		soclose(so);
	}
#ifdef PORTAL_DIAGNOSTIC
	if (error != ENODEV)
		printf("portal_open: error = %d\n", error);
#endif
	return (error);
}

portal_getattr(ap)
	struct vop_getattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	struct vnode *vp = ap->a_vp;
	struct vattr *vap = ap->a_vap;
	unsigned fd;
	int error;

	bzero((caddr_t) vap, sizeof(*vap));
	vattr_null(vap);
	vap->va_uid = 0;
	vap->va_gid = 0;
	vap->va_fsid = vp->v_mount->mnt_stat.f_fsid.val[0];
	vap->va_size = DEV_BSIZE;
	vap->va_blocksize = DEV_BSIZE;
	microtime(&vap->va_atime);
	vap->va_mtime = vap->va_atime;
	vap->va_ctime = vap->va_ctime;
	vap->va_gen = 0;
	vap->va_flags = 0;
	vap->va_rdev = 0;
	/* vap->va_qbytes = 0; */
	vap->va_bytes = 0;
	/* vap->va_qsize = 0; */
	if (vp->v_flag & VROOT) {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_getattr: stat rootdir\n");
#endif
		vap->va_type = VDIR;
		vap->va_mode = S_IRUSR|S_IWUSR|S_IXUSR|
				S_IRGRP|S_IWGRP|S_IXGRP|
				S_IROTH|S_IWOTH|S_IXOTH;
		vap->va_nlink = 2;
		vap->va_fileid = 2;
	} else {
#ifdef PORTAL_DIAGNOSTIC
		printf("portal_getattr: stat portal\n");
#endif
		vap->va_type = VREG;
		vap->va_mode = S_IRUSR|S_IWUSR|
				S_IRGRP|S_IWGRP|
				S_IROTH|S_IWOTH;
		vap->va_nlink = 1;
		vap->va_fileid = VTOPORTAL(vp)->pt_fileid;
	}
	return (0);
}

portal_setattr(ap)
	struct vop_setattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	/*
	 * Can't mess with the root vnode
	 */
	if (ap->a_vp->v_flag & VROOT)
		return (EACCES);

	return (0);
}

/*
 * Fake readdir, just return empty directory.
 * It is hard to deal with '.' and '..' so don't bother.
 */
portal_readdir(ap)
	struct vop_readdir_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		struct ucred *a_cred;
	} */ *ap;
{
	/* *ap->a_eofflagp = 1; */
	return (0);
}

portal_inactive(ap)
	struct vop_inactive_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
#ifdef PORTAL_DIAGNOSTIC
	if (VTOPORTAL(ap->a_vp)->pt_arg)
		printf("portal_inactive(%x, %s)\n", ap->a_vp, VTOPORTAL(ap->a_vp)->pt_arg);
	else
		printf("portal_inactive(%x)\n", ap->a_vp);
#endif
	/*vgone(ap->a_vp);*/
	return (0);
}

portal_reclaim(ap)
	struct vop_reclaim_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct portalnode *pt = VTOPORTAL(ap->a_vp);
#ifdef PORTAL_DIAGOISTIC
	printf("portal_reclaim(%x)\n", ap->a_vp);
#endif
	if (pt->pt_arg) {
		free((caddr_t) pt->pt_arg, M_TEMP);
		pt->pt_arg = 0;
	}
	FREE(pt, M_TEMP);
	return (0);
}

/*
 * Print out the contents of a Portal vnode.
 */
/* ARGSUSED */
portal_print(ap)
	struct vop_print_args /* {
		struct vnode *a_vp;
	} */ *ap;
{

	printf("tag VT_PORTAL, portal vnode\n");
	return (0);
}

/*void*/
portal_vfree(ap)
	struct vop_vfree_args /* {
		struct vnode *a_pvp;
		ino_t a_ino;
		int a_mode;
	} */ *ap;
{

	return (0);
}


/*
 * Portal vnode unsupported operation
 */
portal_enotsupp()
{

	return (EOPNOTSUPP);
}

/*
 * Portal "should never get here" operation
 */
portal_badop()
{

	panic("portal: bad op");
	/* NOTREACHED */
}

/*
 * Portal vnode null operation
 */
portal_nullop()
{

	return (0);
}

#define portal_create ((int (*) __P((struct vop_create_args *)))portal_enotsupp)
#define portal_mknod ((int (*) __P((struct  vop_mknod_args *)))portal_enotsupp)
#define portal_close ((int (*) __P((struct  vop_close_args *)))nullop)
#define portal_access ((int (*) __P((struct  vop_access_args *)))nullop)
#define portal_read ((int (*) __P((struct  vop_read_args *)))portal_enotsupp)
#define portal_write ((int (*) __P((struct  vop_write_args *)))portal_enotsupp)
#define portal_ioctl ((int (*) __P((struct  vop_ioctl_args *)))portal_enotsupp)
#define portal_select ((int (*) __P((struct vop_select_args *)))portal_enotsupp)
#define portal_mmap ((int (*) __P((struct  vop_mmap_args *)))portal_enotsupp)
#define portal_fsync ((int (*) __P((struct  vop_fsync_args *)))nullop)
#define portal_seek ((int (*) __P((struct  vop_seek_args *)))nullop)
#define portal_remove ((int (*) __P((struct vop_remove_args *)))portal_enotsupp)
#define portal_link ((int (*) __P((struct  vop_link_args *)))portal_enotsupp)
#define portal_rename ((int (*) __P((struct vop_rename_args *)))portal_enotsupp)
#define portal_mkdir ((int (*) __P((struct  vop_mkdir_args *)))portal_enotsupp)
#define portal_rmdir ((int (*) __P((struct  vop_rmdir_args *)))portal_enotsupp)
#define portal_symlink \
	((int (*) __P((struct  vop_symlink_args *)))portal_enotsupp)
#define portal_readlink \
	((int (*) __P((struct  vop_readlink_args *)))portal_enotsupp)
#define portal_abortop ((int (*) __P((struct  vop_abortop_args *)))nullop)
#define portal_lock ((int (*) __P((struct  vop_lock_args *)))nullop)
#define portal_unlock ((int (*) __P((struct  vop_unlock_args *)))nullop)
#define portal_bmap ((int (*) __P((struct  vop_bmap_args *)))portal_badop)
#define portal_strategy \
	((int (*) __P((struct  vop_strategy_args *)))portal_badop)
#define portal_islocked ((int (*) __P((struct  vop_islocked_args *)))nullop)
#define portal_advlock \
	((int (*) __P((struct  vop_advlock_args *)))portal_enotsupp)
#define portal_blkatoff \
	((int (*) __P((struct  vop_blkatoff_args *)))portal_enotsupp)
#define portal_valloc ((int(*) __P(( \
		struct vnode *pvp, \
		int mode, \
		struct ucred *cred, \
		struct vnode **vpp))) portal_enotsupp)
#define portal_truncate \
	((int (*) __P((struct  vop_truncate_args *)))portal_enotsupp)
#define portal_update ((int (*) __P((struct vop_update_args *)))portal_enotsupp)
#define portal_bwrite ((int (*) __P((struct vop_bwrite_args *)))portal_enotsupp)

int (**portal_vnodeop_p)();
struct vnodeopv_entry_desc portal_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, portal_lookup },		/* lookup */
	{ &vop_create_desc, portal_create },		/* create */
	{ &vop_mknod_desc, portal_mknod },		/* mknod */
	{ &vop_open_desc, portal_open },		/* open */
	{ &vop_close_desc, portal_close },		/* close */
	{ &vop_access_desc, portal_access },		/* access */
	{ &vop_getattr_desc, portal_getattr },		/* getattr */
	{ &vop_setattr_desc, portal_setattr },		/* setattr */
	{ &vop_read_desc, portal_read },		/* read */
	{ &vop_write_desc, portal_write },		/* write */
	{ &vop_ioctl_desc, portal_ioctl },		/* ioctl */
	{ &vop_select_desc, portal_select },		/* select */
	{ &vop_mmap_desc, portal_mmap },		/* mmap */
	{ &vop_fsync_desc, portal_fsync },		/* fsync */
	{ &vop_seek_desc, portal_seek },		/* seek */
	{ &vop_remove_desc, portal_remove },		/* remove */
	{ &vop_link_desc, portal_link },		/* link */
	{ &vop_rename_desc, portal_rename },		/* rename */
	{ &vop_mkdir_desc, portal_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, portal_rmdir },		/* rmdir */
	{ &vop_symlink_desc, portal_symlink },		/* symlink */
	{ &vop_readdir_desc, portal_readdir },		/* readdir */
	{ &vop_readlink_desc, portal_readlink },	/* readlink */
	{ &vop_abortop_desc, portal_abortop },		/* abortop */
	{ &vop_inactive_desc, portal_inactive },	/* inactive */
	{ &vop_reclaim_desc, portal_reclaim },		/* reclaim */
	{ &vop_lock_desc, portal_lock },		/* lock */
	{ &vop_unlock_desc, portal_unlock },		/* unlock */
	{ &vop_bmap_desc, portal_bmap },		/* bmap */
	{ &vop_strategy_desc, portal_strategy },	/* strategy */
	{ &vop_print_desc, portal_print },		/* print */
	{ &vop_islocked_desc, portal_islocked },	/* islocked */
	{ &vop_advlock_desc, portal_advlock },		/* advlock */
	{ &vop_blkatoff_desc, portal_blkatoff },	/* blkatoff */
	{ &vop_valloc_desc, portal_valloc },		/* valloc */
	{ &vop_vfree_desc, portal_vfree },		/* vfree */
	{ &vop_truncate_desc, portal_truncate },	/* truncate */
	{ &vop_update_desc, portal_update },		/* update */
	{ &vop_bwrite_desc, portal_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc portal_vnodeop_opv_desc =
	{ &portal_vnodeop_p, portal_vnodeop_entries };
