/*	@(#)vnode.h 2.10 85/03/27 SMI	*/

/*
 * The vnode is the focus of all file activity in UNIX.
 * There is a unique vnode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
 */

/*
 * vnode types. VNON means no type.
 */
enum vtype 	{ VNON, VREG, VDIR, VBLK, VCHR, VLNK, VSOCK };

struct vnode {
	u_short		v_flag;			/* vnode flags (see below) */
	u_short		v_count;		/* reference count */
	u_short		v_shlockc;		/* count of shared locks */
	u_short		v_exlockc;		/* count of exclusive locks */
	struct vfs	*v_vfsmountedhere; 	/* ptr to vfs mounted here */
	struct vfs	*v_vfsp;		/* ptr to vfs we are in */
	struct vnodeops	*v_op;			/* vnode operations */
**	struct text	*v_text;		/* text/mapped region */
	enum vtype	v_type;			/* vnode type */
	caddr_t		v_data;			/* private data for fs */
};

/*
 * vnode flags.
 */
#define	VROOT		0x01	/* root of its file system */
#define	VTEXT		0x02	/* vnode is a pure text prototype */
#define	VEXLOCK		0x10	/* exclusive lock */
#define	VSHLOCK		0x20	/* shared lock */
#define	VLWAIT		0x40	/* proc is waiting on shared or excl. lock */

/*
 * Operations on vnodes.
 */
struct vnodeops {
*	int	(*vn_lookup)(		/* ndp */ );
*	int	(*vn_create)(		/* ndp, vap, fflags */ );
*	int	(*vn_open)(		/* vp, fflags, cred */ );
	int	(*vn_close)(		/* vp, fflags, cred */ );
	int	(*vn_access)(		/* vp, mode, cred */ );
	int	(*vn_getattr)(		/* vp, vap, cred */ );
	int	(*vn_setattr)(		/* vp, vap, cred */ );

**	int	(*vn_read)(		/* vp, uiop, ioflag, cred */ );
**	int	(*vn_write)(		/* vp, uiop, ioflag, cred */ );
*	int	(*vn_ioctl)(		/* vp, com, data, fflag, cred */ );
	int	(*vn_select)(		/* vp, which, cred */ );
**	int	(*vn_mmap)(		/* vp, ..., cred */ );
	int	(*vn_fsync)(		/* vp, cred */ );
**	int	(*vn_seek)(		/* vp, off, whence */

*	int	(*vn_remove)(		/* ndp */ );
*	int	(*vn_link)(		/* vp, ndp */ );
*	int	(*vn_rename)(		/* ndp, ndp */ );
*	int	(*vn_mkdir)(		/* ndp, vap */ );
*	int	(*vn_rmdir)(		/* ndp */ );
*	int	(*vn_symlink)(		/* ndp, vap, nm */ );
	int	(*vn_readdir)(		/* vp, uiop, ioflag, cred */ );
	int	(*vn_readlink)(		/* vp, uiop, ioflag, cred */ );

**	int	(*vn_abortop)(		/* ndp */ );
*	int	(*vn_inactive)(		/* vp */ );
**	int	(*vn_vptofh)(		/* vp, fhp */
};

/*
 * flags for ioflag
 */
#define IO_ATOMIC	0x01		/* do io as atomic unit for VOP_RDWR */
#define IO_UNIT		IO_ATOMIC	/* compat */
#define IO_APPEND	0x02		/* append write for VOP_RDWR */
#define IO_SYNC		0x04		/* sync io for VOP_RDWR */

/*
 * Vnode attributes.  A field value of -1
 * represents a field whose value is unavailable
 * (getattr) or which is not to be changed (setattr).
 */
struct vattr {
	enum vtype	va_type;	/* vnode type (for create) */
	u_short		va_mode;	/* files access mode and type */
*	uid_t		va_uid;		/* owner user id */
*	gid_t		va_gid;		/* owner group id */
	long		va_fsid;	/* file system id (dev for now) */
*	long		va_fileid;	/* file id */
	short		va_nlink;	/* number of references to file */
	u_long		va_size;	/* file size in bytes (quad?) */
*	u_long		va_size1;	/* reserved if not quad */
	long		va_blocksize;	/* blocksize preferred for i/o */
	struct timeval	va_atime;	/* time of last access */
	struct timeval	va_mtime;	/* time of last modification */
	struct timeval	va_ctime;	/* time file changed */
	dev_t		va_rdev;	/* device the file represents */
	u_long		va_blocks;	/* bytes of disk space held by file */
*	u_long		va_blocks1;	/* reserved if va_blocks not a quad */
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

#ifdef KERNEL
/*
 * public vnode manipulation functions
 */
extern int vn_open();			/* open vnode */
extern int vn_create();			/* creat/mkdir vnode */
extern int vn_rdwr();			/* read or write vnode */
extern int vn_close();			/* close vnode */
extern void vn_rele();			/* release vnode */
extern int vn_link();			/* make hard link */
extern int vn_rename();			/* rename (move) */
extern int vn_remove();			/* remove/rmdir */
extern void vattr_null();		/* set attributes to null */
extern int getvnodefp();		/* get fp from vnode fd */

#define VN_HOLD(VP)	{ \
	(VP)->v_count++; \
}

#define VN_RELE(VP)	{ \
	vn_rele(VP); \
}

#define VN_INIT(VP, VFSP, TYPE, DEV)	{ \
	(VP)->v_flag = 0; \
	(VP)->v_count = 1; \
	(VP)->v_shlockc = (VP)->v_exlockc = 0; \
	(VP)->v_vfsp = (VFSP); \
	(VP)->v_type = (TYPE); \
	(VP)->v_rdev = (DEV); \
}

/*
 * flags for vn_remove
 */
enum rm		{ FILE, DIRECTORY };		/* rmdir or rm (remove) */

/*
 * Global vnode data.
 */
extern struct vnode	*rootdir;		/* root (i.e. "/") vnode */

#endif
