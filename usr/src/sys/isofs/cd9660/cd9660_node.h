/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cd9660_node.h	8.5 (Berkeley) %G%
 */

/*
 * Theoretically, directories can be more than 2Gb in length,
 * however, in practice this seems unlikely. So, we define
 * the type doff_t as a long to keep down the cost of doing
 * lookup on a 32-bit machine. If you are porting to a 64-bit
 * architecture, you should make doff_t the same as off_t.
 */
#define doff_t	long

typedef	struct	{
	struct timespec	iso_atime;	/* time of last access */
	struct timespec	iso_mtime;	/* time of last modification */
	struct timespec	iso_ctime;	/* time file changed */
	u_short		iso_mode;	/* files access mode and type */
	uid_t		iso_uid;	/* owner user id */
	gid_t		iso_gid;	/* owner group id */
	short		iso_links;	/* links of file */
	dev_t		iso_rdev;	/* Major/Minor number for special */
} ISO_RRIP_INODE;

#ifdef ISODEVMAP
/*
 * FOr device# (major,minor) translation table
 */
struct iso_dnode {
	struct iso_dnode *d_next, **d_prev;	/* hash chain */
	dev_t		i_dev;		/* device where dnode resides */
	ino_t		i_number;	/* the identity of the inode */
	dev_t		d_dev;		/* device # for translation */
};
#endif

struct iso_node {
	struct	iso_node *i_next, **i_prev;	/* hash chain */
	struct	vnode *i_vnode;	/* vnode associated with this inode */
	struct	vnode *i_devvp;	/* vnode for block I/O */
	u_long	i_flag;		/* see below */
	dev_t	i_dev;		/* device where inode resides */
	ino_t	i_number;	/* the identity of the inode */
				/* we use the actual starting block of the file */
	struct	iso_mnt *i_mnt;	/* filesystem associated with this inode */
	struct	lockf *i_lockf;	/* head of byte-level lock list */
	doff_t	i_endoff;	/* end of useful stuff in directory */
	doff_t	i_diroff;	/* offset in dir, where we found last entry */
	doff_t	i_offset;	/* offset of free space in directory */
	ino_t	i_ino;		/* inode number of found directory */
	pid_t	i_lockholder, i_lockwaiter;

	long iso_extent;	/* extent of file */
	long i_size;
	long iso_start;		/* actual start of data of file (may be different */
				/* from iso_extent, if file has extended attributes) */
	ISO_RRIP_INODE  inode;
};

#define	i_forw		i_chain[0]
#define	i_back		i_chain[1]

/* flags */
#define	IN_LOCKED	0x0001		/* inode is locked */
#define	IN_WANTED	0x0002		/* some process waiting on lock */
#define	IN_ACCESS	0x0020		/* inode access time to be updated */

#define VTOI(vp) ((struct iso_node *)(vp)->v_data)
#define ITOV(ip) ((ip)->i_vnode)

/*
 * Prototypes for ISOFS vnode operations
 */
int cd9660_lookup __P((struct vop_lookup_args *));
int cd9660_open __P((struct vop_open_args *));
int cd9660_close __P((struct vop_close_args *));
int cd9660_access __P((struct vop_access_args *));
int cd9660_getattr __P((struct vop_getattr_args *));
int cd9660_read __P((struct vop_read_args *));
int cd9660_ioctl __P((struct vop_ioctl_args *));
int cd9660_select __P((struct vop_select_args *));
int cd9660_mmap __P((struct vop_mmap_args *));
int cd9660_seek __P((struct vop_seek_args *));
int cd9660_readdir __P((struct vop_readdir_args *));
int cd9660_abortop __P((struct vop_abortop_args *));
int cd9660_inactive __P((struct vop_inactive_args *));
int cd9660_reclaim __P((struct vop_reclaim_args *));
int cd9660_bmap __P((struct vop_bmap_args *));
int cd9660_lock __P((struct vop_lock_args *));
int cd9660_unlock __P((struct vop_unlock_args *));
int cd9660_strategy __P((struct vop_strategy_args *));
int cd9660_print __P((struct vop_print_args *));
int cd9660_islocked __P((struct vop_islocked_args *));
int cd9660_pathconf __P((struct vop_pathconf_args *));
int cd9660_blkatoff __P((struct vop_blkatoff_args *));
#define cd9660_revoke vop_revoke

void cd9660_defattr __P((struct iso_directory_record *,
			struct iso_node *, struct buf *));
void cd9660_deftstamp __P((struct iso_directory_record *,
			struct iso_node *, struct buf *));
struct vnode *cd9660_ihashget __P((dev_t, ino_t));
void cd9660_ihashins __P((struct iso_node *));
void cd9660_ihashrem __P((struct iso_node *));
int cd9660_tstamp_conv7 __P((u_char *, struct timespec *));
int cd9660_tstamp_conv17 __P((u_char *, struct timespec *));
ino_t isodirino __P((struct iso_directory_record *, struct iso_mnt *));
#ifdef	ISODEVMAP
struct iso_dnode *iso_dmap __P((dev_t, ino_t, int));
void iso_dunmap __P((dev_t));
#endif
