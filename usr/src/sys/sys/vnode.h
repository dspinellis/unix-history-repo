/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vnode.h	7.23 (Berkeley) %G%
 */

/*
 * The vnode is the focus of all file activity in UNIX.
 * There is a unique vnode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
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
enum vtagtype	{ VT_NON, VT_UFS, VT_NFS, VT_MFS };

/*
 * This defines the maximum size of the private data area
 * permitted for any file system type. A defined constant 
 * is used rather than a union structure to cut down on the
 * number of header files that must be included.
 */
#define VN_MAXPRIVATE	184

struct vnode {
	u_long		v_flag;			/* vnode flags (see below) */
	long		v_usecount;		/* reference count of users */
	long		v_holdcnt;		/* page & buffer references */
	u_short		v_shlockc;		/* count of shared locks */
	u_short		v_exlockc;		/* count of exclusive locks */
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
		struct text	*vu_text;	/* text/mapped region (VREG) */
		struct specinfo	*vu_specinfo;	/* device (VCHR, VBLK) */
		struct fifoinfo	*vu_fifoinfo;	/* fifo (VFIFO) */
	} v_un;
	enum vtagtype	v_tag;			/* type of underlying data */
	char v_data[VN_MAXPRIVATE];		/* private data for fs */
};
#define v_mountedhere v_un.vu_mountedhere
#define v_socket v_un.vu_socket
#define v_text v_un.vu_text
#define v_specinfo v_un.vu_specinfo
#define v_fifoinfo v_un.vu_fifoinfo

/*
 * vnode flags.
 */
#define	VROOT		0x0001	/* root of its file system */
#define	VTEXT		0x0002	/* vnode is a pure text prototype */
#define	VXLOCK		0x0004	/* vnode is locked to change underlying type */
#define	VXWANT		0x0008	/* process is waiting for vnode */
#define	VEXLOCK		0x0010	/* exclusive lock */
#define	VSHLOCK		0x0020	/* shared lock */
#define	VLWAIT		0x0040	/* proc is waiting on shared or excl. lock */
#define	VALIASED	0x0080	/* vnode has an alias */
#define	VBWAIT		0x0100	/* waiting for output to complete */

/*
 * Operations on vnodes.
 */
struct vnodeops {
	int	(*vn_lookup)(		/* ndp */ );
	int	(*vn_create)(		/* ndp, fflags, vap, cred */ );
	int	(*vn_mknod)(		/* ndp, vap, cred */ );
	int	(*vn_open)(		/* vp, fflags, cred */ );
	int	(*vn_close)(		/* vp, fflags, cred */ );
	int	(*vn_access)(		/* vp, fflags, cred */ );
	int	(*vn_getattr)(		/* vp, vap, cred */ );
	int	(*vn_setattr)(		/* vp, vap, cred */ );

	int	(*vn_read)(		/* vp, uiop, offp, ioflag, cred */ );
	int	(*vn_write)(		/* vp, uiop, offp, ioflag, cred */ );
	int	(*vn_ioctl)(		/* vp, com, data, fflag, cred */ );
	int	(*vn_select)(		/* vp, which, cred */ );
	int	(*vn_mmap)(		/* vp, ..., cred */ );
	int	(*vn_fsync)(		/* vp, fflags, cred */ );
	int	(*vn_seek)(		/* vp, (old)offp, off, whence */ );

	int	(*vn_remove)(		/* ndp */ );
	int	(*vn_link)(		/* vp, ndp */ );
	int	(*vn_rename)(		/* ndp, ndp */ );
	int	(*vn_mkdir)(		/* ndp, vap */ );
	int	(*vn_rmdir)(		/* ndp */ );
	int	(*vn_symlink)(		/* ndp, vap, nm */ );
	int	(*vn_readdir)(		/* vp, uiop, cred, eofflagp */ );
	int	(*vn_readlink)(		/* vp, uiop, cred */ );

	int	(*vn_abortop)(		/* ndp */ );
	int	(*vn_inactive)(		/* vp */ );
	int	(*vn_reclaim)(		/* vp */ );
	int	(*vn_lock)(		/* vp */ );
	int	(*vn_unlock)(		/* vp */ );

	int	(*vn_bmap)(		/* vp, bn, vpp, bnp */ );
	int	(*vn_strategy)(		/* bp */ );

	int	(*vn_print)(		/* vp */ );
	int	(*vn_islocked)(		/* vp */ );
};

/* Macros to call the vnode ops */
#define	VOP_LOOKUP(v,n)		(*((v)->v_op->vn_lookup))((v),(n))
#define	VOP_CREATE(n,a)		(*((n)->ni_dvp->v_op->vn_create))((n),(a))
#define	VOP_MKNOD(n,a,c)	(*((n)->ni_dvp->v_op->vn_mknod))((n),(a),(c))
#define	VOP_OPEN(v,f,c)		(*((v)->v_op->vn_open))((v),(f),(c))
#define	VOP_CLOSE(v,f,c)	(*((v)->v_op->vn_close))((v),(f),(c))
#define	VOP_ACCESS(v,f,c)	(*((v)->v_op->vn_access))((v),(f),(c))
#define	VOP_GETATTR(v,a,c)	(*((v)->v_op->vn_getattr))((v),(a),(c))
#define	VOP_SETATTR(v,a,c)	(*((v)->v_op->vn_setattr))((v),(a),(c))
#define	VOP_READ(v,u,i,c)	(*((v)->v_op->vn_read))((v),(u),(i),(c))
#define	VOP_WRITE(v,u,i,c)	(*((v)->v_op->vn_write))((v),(u),(i),(c))
#define	VOP_IOCTL(v,o,d,f,c)	(*((v)->v_op->vn_ioctl))((v),(o),(d),(f),(c))
#define	VOP_SELECT(v,w,f,c)	(*((v)->v_op->vn_select))((v),(w),(f),(c))
#define	VOP_MMAP(v,c)		(*((v)->v_op->vn_mmap))((v),(c))
#define	VOP_FSYNC(v,f,c,w)	(*((v)->v_op->vn_fsync))((v),(f),(c),(w))
#define	VOP_SEEK(v,p,o,w)	(*((v)->v_op->vn_seek))((v),(p),(o),(w))
#define	VOP_REMOVE(n)		(*((n)->ni_dvp->v_op->vn_remove))(n)
#define	VOP_LINK(v,n)		(*((n)->ni_dvp->v_op->vn_link))((v),(n))
#define	VOP_RENAME(s,t)		(*((s)->ni_dvp->v_op->vn_rename))((s),(t))
#define	VOP_MKDIR(n,a)		(*((n)->ni_dvp->v_op->vn_mkdir))((n),(a))
#define	VOP_RMDIR(n)		(*((n)->ni_dvp->v_op->vn_rmdir))(n)
#define	VOP_SYMLINK(n,a,m)	(*((n)->ni_dvp->v_op->vn_symlink))((n),(a),(m))
#define	VOP_READDIR(v,u,c,e)	(*((v)->v_op->vn_readdir))((v),(u),(c),(e))
#define	VOP_READLINK(v,u,c)	(*((v)->v_op->vn_readlink))((v),(u),(c))
#define	VOP_ABORTOP(n)		(*((n)->ni_dvp->v_op->vn_abortop))(n)
#define	VOP_INACTIVE(v)		(*((v)->v_op->vn_inactive))(v)
#define	VOP_RECLAIM(v)		(*((v)->v_op->vn_reclaim))(v)
#define	VOP_LOCK(v)		(*((v)->v_op->vn_lock))(v)
#define	VOP_UNLOCK(v)		(*((v)->v_op->vn_unlock))(v)
#define	VOP_BMAP(v,s,p,n)	(*((v)->v_op->vn_bmap))((v),(s),(p),(n))
#define	VOP_STRATEGY(b)		(*((b)->b_vp->v_op->vn_strategy))(b)
#define	VOP_PRINT(v)		(*((v)->v_op->vn_print))(v)
#define	VOP_ISLOCKED(v)		(*((v)->v_op->vn_islocked))(v)

/*
 * This structure defines the information maintained about
 * special devices. It is allocated in checkalias and freed
 * in vgone.
 */
struct specinfo {
	struct	vnode **si_hashchain;
	struct	vnode *si_specnext;
	long	si_flags;
	dev_t	si_rdev;
};
/*
 * Exported shorthand
 */
#define v_rdev v_specinfo->si_rdev
/*
 * Flags for specinfo
 */
#define	SI_MOUNTEDON	0x0001	/* block special device is mounted on */

/*
 * flags for ioflag
 */
#define IO_UNIT		0x01		/* do I/O as atomic unit */
#define IO_APPEND	0x02		/* append write to end */
#define IO_SYNC		0x04		/* do I/O synchronously */
#define	IO_NODELOCKED	0x08		/* underlying node already locked */
#define	IO_NDELAY	0x10		/* FNDELAY flag set in file table */

/*
 * Vnode attributes.  A field value of VNOVAL
 * represents a field whose value is unavailable
 * (getattr) or which is not to be changed (setattr).
 */
struct vattr {
	enum vtype	va_type;	/* vnode type (for create) */
	u_short		va_mode;	/* files access mode and type */
	short		va_nlink;	/* number of references to file */
	uid_t		va_uid;		/* owner user id */
	gid_t		va_gid;		/* owner group id */
	long		va_fsid;	/* file system id (dev for now) */
	long		va_fileid;	/* file id */
	u_long		va_size;	/* file size in bytes (quad?) */
	u_long		va_size1;	/* reserved if not quad */
	long		va_blocksize;	/* blocksize preferred for i/o */
	struct timeval	va_atime;	/* time of last access */
	struct timeval	va_mtime;	/* time of last modification */
	struct timeval	va_ctime;	/* time file changed */
	u_long		va_gen;		/* generation number of file */
	u_long		va_flags;	/* flags defined for file */
	dev_t		va_rdev;	/* device the special file represents */
	u_long		va_bytes;	/* bytes of disk space held by file */
	u_long		va_bytes1;	/* reserved if va_bytes not a quad */
};

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
#define VNOVAL	((unsigned)0xffffffff)

#ifdef KERNEL
/*
 * public vnode manipulation functions
 */
extern int vn_open();			/* open vnode */
extern int vn_rdwr();			/* read or write vnode */
extern int vn_close();			/* close vnode */
extern void vattr_null();		/* set attributes to null */
extern int getnewvnode();		/* allocate a new vnode */
extern int bdevvp();			/* allocate a new special dev vnode */
extern struct vnode *checkalias();	/* check for special device aliases */
extern int vcount();			/* total references to a device */
extern int vget();			/* get first reference to a vnode */
extern void vref();			/* increase reference to a vnode */
extern void vput();			/* unlock and release vnode */
extern void vrele();			/* release vnode */
extern void vgone();			/* completely recycle vnode */
extern void vgoneall();			/* recycle vnode and all its aliases */

#ifdef notdef
#define VREF(vp)    (vp)->v_usecount++;	/* increase reference to a vnode */
#define VHOLD(vp)   (vp)->v_holdcnt++;	/* increase buf or page ref to vnode */
#define HOLDRELE(vp) (vp)->v_holdcnt--;	/* decrease buf or page ref to vnode */
#else
#define VREF(vp)    vref(vp)
#define VHOLD(vp)   vhold(vp)
#define HOLDRELE(vp) holdrele(vp)
#endif

/*
 * Global vnode data.
 */
extern	struct vnode *rootdir;		/* root (i.e. "/") vnode */

extern	struct vnode *vnode;		/* The vnode table itself */
extern	struct vnode *vnodeNVNODE;	/* The end of the vnode table */
extern	int nvnode;			/* number of slots in the table */
#endif
