/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vnode.h	7.41 (Berkeley) %G%
 */

#ifndef KERNEL
#include <machine/endian.h>
#endif

/*
 * The vnode is the focus of all file activity in UNIX.  There is a unique
 * vnode allocated for each active file, each current directory, each
 * mounted-on file, text file, and the root.
 */

/*
 * vnode types. VNON means no type.
 */
enum vtype 	{ VNON, VREG, VDIR, VBLK, VCHR, VLNK, VSOCK, VFIFO, VBAD };

/*
 * Vnode tag types.
 * These are for the benefit of external programs only (e.g., pstat)
 * and should NEVER be inspected inside the kernel.
 */
enum vtagtype	{ VT_NON, VT_UFS, VT_NFS, VT_MFS, VT_LFS };

/*
 * This defines the maximum size of the private data area for any file system
 * type.  A defined constant is used rather than a union structure to reduce
 * the number of header files that must be included.
 */
#define	VN_MAXPRIVATE	188

struct vnode {
	u_long		v_flag;			/* vnode flags (see below) */
	short		v_usecount;		/* reference count of users */
	short		v_writecount;		/* reference count of writers */
	long		v_holdcnt;		/* page & buffer references */
	off_t		v_lastr;		/* last read (read-ahead) */
	u_long		v_id;			/* capability identifier */
	struct mount	*v_mount;		/* ptr to vfs we are in */
	struct vnodeops	*v_op;			/* vnode operations */
	struct vnode	*v_freef;		/* vnode freelist forward */
	struct vnode	**v_freeb;		/* vnode freelist back */
	struct vnode	*v_mountf;		/* vnode mountlist forward */
	struct vnode	**v_mountb;		/* vnode mountlist back */
	struct buf	*v_cleanblkhd;		/* clean blocklist head */
	struct buf	*v_dirtyblkhd;		/* dirty blocklist head */
	long		v_numoutput;		/* num of writes in progress */
	enum vtype	v_type;			/* vnode type */
	union {
		struct mount	*vu_mountedhere;/* ptr to mounted vfs (VDIR) */
		struct socket	*vu_socket;	/* unix ipc (VSOCK) */
		caddr_t		vu_vmdata;	/* private data for vm (VREG) */
		struct specinfo	*vu_specinfo;	/* device (VCHR, VBLK) */
		struct fifoinfo	*vu_fifoinfo;	/* fifo (VFIFO) */
	} v_un;
	enum vtagtype	v_tag;			/* type of underlying data */
	char v_data[VN_MAXPRIVATE];		/* private data for fs */
};
#define	v_mountedhere	v_un.vu_mountedhere
#define	v_socket	v_un.vu_socket
#define	v_vmdata	v_un.vu_vmdata
#define	v_specinfo	v_un.vu_specinfo
#define	v_fifoinfo	v_un.vu_fifoinfo

/*
 * vnode flags.
 */
#define	VROOT		0x0001	/* root of its file system */
#define	VTEXT		0x0002	/* vnode is a pure text prototype */
#define	VSYSTEM		0x0004	/* vnode being used by kernel */
#define	VXLOCK		0x0100	/* vnode is locked to change underlying type */
#define	VXWANT		0x0200	/* process is waiting for vnode */
#define	VBWAIT		0x0400	/* waiting for output to complete */
#define	VALIASED	0x0800	/* vnode has an alias */

/*
 * Vnode attributes.  A field value of VNOVAL represents a field whose value
 * is unavailable (getattr) or which is not to be changed (setattr).
 */
struct vattr {
	enum vtype	va_type;	/* vnode type (for create) */
	u_short		va_mode;	/* files access mode and type */
	short		va_nlink;	/* number of references to file */
	uid_t		va_uid;		/* owner user id */
	gid_t		va_gid;		/* owner group id */
	long		va_fsid;	/* file system id (dev for now) */
	long		va_fileid;	/* file id */
	u_quad		va_qsize;	/* file size in bytes */
	long		va_blocksize;	/* blocksize preferred for i/o */
	struct timeval	va_atime;	/* time of last access */
	struct timeval	va_mtime;	/* time of last modification */
	struct timeval	va_ctime;	/* time file changed */
	u_long		va_gen;		/* generation number of file */
	u_long		va_flags;	/* flags defined for file */
	dev_t		va_rdev;	/* device the special file represents */
	u_quad		va_qbytes;	/* bytes of disk space held by file */
};
#if BYTE_ORDER == LITTLE_ENDIAN
#define	va_size		va_qsize.val[0]
#define	va_size_rsv	va_qsize.val[1]
#define	va_bytes	va_qbytes.val[0]
#define	va_bytes_rsv	va_qbytes.val[1]
#else
#define	va_size		va_qsize.val[1]
#define	va_size_rsv	va_qsize.val[0]
#define	va_bytes	va_qbytes.val[1]
#define	va_bytes_rsv	va_qbytes.val[0]
#endif

/*
 * Operations on vnodes.
 */
#ifdef __STDC__
struct flock;
struct nameidata;
#endif

struct vnodeops {
#define	VOP_LOOKUP(v,n,p)	(*((v)->v_op->vop_lookup))(v,n,p)
	int	(*vop_lookup)	__P((struct vnode *vp, struct nameidata *ndp,
				    struct proc *p));
#define	VOP_CREATE(n,a,p)	(*((n)->ni_dvp->v_op->vop_create))(n,a,p)
	int	(*vop_create)	__P((struct nameidata *ndp, struct vattr *vap,
				    struct proc *p));
#define	VOP_MKNOD(n,a,c,p)	(*((n)->ni_dvp->v_op->vop_mknod))(n,a,c,p)
	int	(*vop_mknod)	__P((struct nameidata *ndp, struct vattr *vap,
				    struct ucred *cred, struct proc *p));
#define	VOP_OPEN(v,f,c,p)	(*((v)->v_op->vop_open))(v,f,c,p)
	int	(*vop_open)	__P((struct vnode *vp, int mode,
				    struct ucred *cred, struct proc *p));
#define	VOP_CLOSE(v,f,c,p)	(*((v)->v_op->vop_close))(v,f,c,p)
	int	(*vop_close)	__P((struct vnode *vp, int fflag,
				    struct ucred *cred, struct proc *p));
#define	VOP_ACCESS(v,f,c,p)	(*((v)->v_op->vop_access))(v,f,c,p)
	int	(*vop_access)	__P((struct vnode *vp, int mode,
				    struct ucred *cred, struct proc *p));
#define	VOP_GETATTR(v,a,c,p)	(*((v)->v_op->vop_getattr))(v,a,c,p)
	int	(*vop_getattr)	__P((struct vnode *vp, struct vattr *vap,
				    struct ucred *cred, struct proc *p));
#define	VOP_SETATTR(v,a,c,p)	(*((v)->v_op->vop_setattr))(v,a,c,p)
	int	(*vop_setattr)	__P((struct vnode *vp, struct vattr *vap,
				    struct ucred *cred, struct proc *p));
#define	VOP_READ(v,u,i,c)	(*((v)->v_op->vop_read))(v,u,i,c)
	int	(*vop_read)	__P((struct vnode *vp, struct uio *uio,
				    int ioflag, struct ucred *cred));
#define	VOP_WRITE(v,u,i,c)	(*((v)->v_op->vop_write))(v,u,i,c)
	int	(*vop_write)	__P((struct vnode *vp, struct uio *uio,
				    int ioflag, struct ucred *cred));
#define	VOP_IOCTL(v,o,d,f,c,p)	(*((v)->v_op->vop_ioctl))(v,o,d,f,c,p)
	int	(*vop_ioctl)	__P((struct vnode *vp, int command,
				    caddr_t data, int fflag,
				    struct ucred *cred, struct proc *p));
#define	VOP_SELECT(v,w,f,c,p)	(*((v)->v_op->vop_select))(v,w,f,c,p)
	int	(*vop_select)	__P((struct vnode *vp, int which, int fflags,
				    struct ucred *cred, struct proc *p));
#define	VOP_MMAP(v,c,p)		(*((v)->v_op->vop_mmap))(v,c,p)
	int	(*vop_mmap)	__P((struct vnode *vp, int fflags,
				    struct ucred *cred, struct proc *p));
#define	VOP_FSYNC(v,f,c,w,p)	(*((v)->v_op->vop_fsync))(v,f,c,w,p)
	int	(*vop_fsync)	__P((struct vnode *vp, int fflags,
				    struct ucred *cred, int waitfor,
				    struct proc *p));
#define	VOP_SEEK(v,p,o,w)	(*((v)->v_op->vop_seek))(v,p,o,w)
	int	(*vop_seek)	__P((struct vnode *vp, off_t oldoff,
				    off_t newoff, struct ucred *cred));
#define	VOP_REMOVE(n,p)		(*((n)->ni_dvp->v_op->vop_remove))(n,p)
	int	(*vop_remove)	__P((struct nameidata *ndp, struct proc *p));
#define	VOP_LINK(v,n,p)		(*((n)->ni_dvp->v_op->vop_link))(v,n,p)
	int	(*vop_link)	__P((struct vnode *vp, struct nameidata *ndp,
				    struct proc *p));
#define	VOP_RENAME(s,t,p)	(*((s)->ni_dvp->v_op->vop_rename))(s,t,p)
	int	(*vop_rename)	__P((struct nameidata *fndp,
				    struct nameidata *tdnp, struct proc *p));
#define	VOP_MKDIR(n,a,p)	(*((n)->ni_dvp->v_op->vop_mkdir))(n,a,p)
	int	(*vop_mkdir)	__P((struct nameidata *ndp, struct vattr *vap,
				    struct proc *p));
#define	VOP_RMDIR(n,p)		(*((n)->ni_dvp->v_op->vop_rmdir))(n,p)
	int	(*vop_rmdir)	__P((struct nameidata *ndp, struct proc *p));
#define	VOP_SYMLINK(n,a,m,p)	(*((n)->ni_dvp->v_op->vop_symlink))(n,a,m,p)
	int	(*vop_symlink)	__P((struct nameidata *ndp, struct vattr *vap,
				    char *target, struct proc *p));
#define	VOP_READDIR(v,u,c,e)	(*((v)->v_op->vop_readdir))(v,u,c,e)
	int	(*vop_readdir)	__P((struct vnode *vp, struct uio *uio,
				    struct ucred *cred, int *eofflagp));
#define	VOP_READLINK(v,u,c)	(*((v)->v_op->vop_readlink))(v,u,c)
	int	(*vop_readlink)	__P((struct vnode *vp, struct uio *uio,
				    struct ucred *cred));
#define	VOP_ABORTOP(n)		(*((n)->ni_dvp->v_op->vop_abortop))(n)
	int	(*vop_abortop)	__P((struct nameidata *ndp));
#define	VOP_INACTIVE(v,p)	(*((v)->v_op->vop_inactive))(v,p)
	int	(*vop_inactive)	__P((struct vnode *vp, struct proc *p));
#define	VOP_RECLAIM(v)		(*((v)->v_op->vop_reclaim))(v)
	int	(*vop_reclaim)	__P((struct vnode *vp));
#define	VOP_LOCK(v)		(*((v)->v_op->vop_lock))(v)
	int	(*vop_lock)	__P((struct vnode *vp));
#define	VOP_UNLOCK(v)		(*((v)->v_op->vop_unlock))(v)
	int	(*vop_unlock)	__P((struct vnode *vp));
#define	VOP_BMAP(v,s,p,n)	(*((v)->v_op->vop_bmap))(v,s,p,n)
	int	(*vop_bmap)	__P((struct vnode *vp, daddr_t bn,
				    struct vnode **vpp, daddr_t *bnp));
#define	VOP_STRATEGY(b)		(*((b)->b_vp->v_op->vop_strategy))(b)
	int	(*vop_strategy)	__P((struct buf *bp));
#define	VOP_PRINT(v)		(*((v)->v_op->vop_print))(v)
	int	(*vop_print)	__P((struct vnode *vp));
#define	VOP_ISLOCKED(v)		(((v)->v_flag & VXLOCK) || \
				(*((v)->v_op->vop_islocked))(v))
	int	(*vop_islocked)	__P((struct vnode *vp));
#define	VOP_ADVLOCK(v,p,o,l,f)	(*((v)->v_op->vop_advlock))(v,p,o,l,f)
	int	(*vop_advlock)	__P((struct vnode *vp, caddr_t id, int op,
				    struct flock *fl, int flags));
#define	VOP_BLKATOFF(v,o,r,b)	(*((v)->v_op->vop_blkatoff))(v,o,r,b)
	int	(*vop_blkatoff) __P((struct vnode *vp,
		    off_t offset, char **res, struct buf **bpp));
#define	VOP_VGET(v,i,vp)	(*((v)->v_op->vop_vget))((v)->v_mount,i,vp)
	int	(*vop_vget)
		    __P((struct mount *mp, ino_t ino, struct vnode **vpp));
#define	VOP_VALLOC(v,m,c,vp)	(*((v)->v_op->vop_valloc))(v,m,c,vp)
	int	(*vop_valloc) __P((struct vnode *pvp,
		    int mode, struct ucred *cred, struct vnode **vpp));
#define	VOP_VFREE(v,i,m)	(*((v)->v_op->vop_vfree))(v,i,m)
	void	(*vop_vfree) __P((struct vnode *pvp, ino_t ino, int mode));
#define	VOP_TRUNCATE(v,l,f)	(*((v)->v_op->vop_truncate))(v,l,f)
	int	(*vop_truncate)
		    __P((struct vnode *vp, u_long length, int flags));
#define	VOP_UPDATE(v,ta,tm,w)	(*((v)->v_op->vop_update))(v,ta,tm,w)
	int	(*vop_update) __P((struct vnode *vp,
		    struct timeval *ta, struct timeval *tm, int waitfor));
#define	VOP_BWRITE(b)		(*((b)->b_vp->v_op->vop_bwrite))(b)
	int	(*vop_bwrite) __P((struct buf *bp));
};

/*
 * flags for ioflag
 */
#define	IO_UNIT		0x01		/* do I/O as atomic unit */
#define	IO_APPEND	0x02		/* append write to end */
#define	IO_SYNC		0x04		/* do I/O synchronously */
#define	IO_NODELOCKED	0x08		/* underlying node already locked */
#define	IO_NDELAY	0x10		/* FNDELAY flag set in file table */

/*
 *  Modes. Some values same as Ixxx entries from inode.h for now
 */
#define	VSUID	04000		/* set user id on execution */
#define	VSGID	02000		/* set group id on execution */
#define	VSVTX	01000		/* save swapped text even after use */
#define	VREAD	0400		/* read, write, execute permissions */
#define	VWRITE	0200
#define	VEXEC	0100

/*
 * Token indicating no attribute value yet assigned
 */
#define	VNOVAL	((unsigned)0xffffffff)

#ifdef KERNEL
/*
 * public vnode manipulation functions
 */
int 	vn_open __P((struct nameidata *ndp, struct proc *p, int fmode,
	    int cmode));
int 	vn_close __P((struct vnode *vp, int flags, struct ucred *cred,
	    struct proc *p));
int 	vn_rdwr __P((enum uio_rw rw, struct vnode *vp, caddr_t base,
	    int len, off_t offset, enum uio_seg segflg, int ioflg,
	    struct ucred *cred, int *aresid, struct proc *p));
int	vn_read __P((struct file *fp, struct uio *uio, struct ucred *cred));
int	vn_write __P((struct file *fp, struct uio *uio, struct ucred *cred));
int	vn_ioctl __P((struct file *fp, int com, caddr_t data, struct proc *p));
int	vn_select __P((struct file *fp, int which, struct proc *p));
int 	vn_closefile __P((struct file *fp, struct proc *p));
int 	getnewvnode __P((enum vtagtype tag, struct mount *mp,
	    struct vnodeops *vops, struct vnode **vpp));
int 	bdevvp __P((int dev, struct vnode **vpp));
	/* check for special device aliases */
	/* XXX nvp_rdev should be type dev_t, not int */
struct 	vnode *checkalias __P((struct vnode *vp, int nvp_rdev,
	    struct mount *mp));
void 	vattr_null __P((struct vattr *vap));
int 	vcount __P((struct vnode *vp));	/* total references to a device */
int 	vget __P((struct vnode *vp));	/* get first reference to a vnode */
void 	vref __P((struct vnode *vp));	/* increase reference to a vnode */
void 	vput __P((struct vnode *vp));	/* unlock and release vnode */
void 	vrele __P((struct vnode *vp));	/* release vnode */
void 	vgone __P((struct vnode *vp));	/* completely recycle vnode */
void 	vgoneall __P((struct vnode *vp));/* recycle vnode and all its aliases */

/*
 * Flags to various vnode functions.
 */
#define	SKIPSYSTEM	0x0001		/* vflush: skip vnodes marked VSYSTEM */
#define	FORCECLOSE	0x0002		/* vflush: force file closeure */
#define	DOCLOSE		0x0004		/* vclean: close active files */

#ifndef DIAGNOSTIC
#define	VREF(vp)	(vp)->v_usecount++	/* increase reference */
#define	VHOLD(vp)	(vp)->v_holdcnt++	/* increase buf or page ref */
#define	HOLDRELE(vp)	(vp)->v_holdcnt--	/* decrease buf or page ref */
#define	VATTR_NULL(vap)	(*(vap) = va_null)	/* initialize a vattr */
#else
#define	VREF(vp)	vref(vp)
#define	VHOLD(vp)	vhold(vp)
#define	HOLDRELE(vp)	holdrele(vp)
#define	VATTR_NULL(vap)	vattr_null(vap)
#endif

#define	NULLVP	((struct vnode *)NULL)

/*
 * Global vnode data.
 */
extern	struct vnode *rootdir;		/* root (i.e. "/") vnode */
extern	int desiredvnodes;		/* number of vnodes desired */
extern	struct vattr va_null;		/* predefined null vattr structure */
#endif
