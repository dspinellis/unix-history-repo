/*
 * Copyright (c) 1985, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)namei.h	7.17 (Berkeley) %G%
 */

/* NEEDSWORK: function defns need update */

#ifndef _NAMEI_H_
#define	_NAMEI_H_


struct componentname {
	u_long cn_nameiop;	/* in */
	u_long cn_flags;	/* in */
	long cn_namelen;	/* in */
	char *cn_nameptr;	/* in */
	u_long cn_hash;		/* in */
	char *cn_pnbuf;		/* in */
	struct ucred *cn_cred;	/* in */
	struct proc *cn_proc;	/* in */
	/*
	 * Side effects.
	 */
	struct ufs_specific {		/* saved info for new dir entry */
		off_t	ufs_endoff;	/* end of useful directory contents */
		long	ufs_offset;	/* offset of free space in directory */
		long	ufs_count;	/* size of free slot in directory */
		ino_t	ufs_ino;	/* inode number of found directory */
		u_long	ufs_reclen;	/* size of found directory entry */
	} cn_ufs;
};

/*
 * Encapsulation of namei parameters.
 */
struct nameidata {
	/*
	 * Arguments to namei.
	 */
	caddr_t	ni_dirp;		/* pathname pointer */
	enum	uio_seg ni_segflg;	/* location of pathname */
        u_long	ni_nameiop;		/* see below.  NEEDSWORK: here for compatibility */
	/*
	 * Arguments to lookup.
	 */
     /* struct	ucred *ni_cred;		/* credentials */
	struct	vnode *ni_startdir;	/* starting directory */
	struct	vnode *ni_rootdir;	/* logical root directory */
	/*
	 * Results: returned from/manipulated by lookup
	 */
	struct	vnode *ni_vp;		/* vnode of result */
	struct	vnode *ni_dvp;		/* vnode of intermediate directory */
	/*
	 * Shared between namei, lookup routines, and commit routines.
	 */
     /* char	*ni_pnbuf;		/* pathname buffer */
	long	ni_pathlen;		/* remaining chars in path */
     /* char	*ni_ptr;		/* current location in pathname */
     /* long	ni_namelen;		/* length of current component */
	char	*ni_next;		/* next location in pathname */
     /* u_long	ni_hash;		/* hash value of current component */
	u_char	ni_loopcnt;		/* count of symlinks encountered */
     /* u_char	ni_makeentry;		/* 1 => add entry to name cache */
     /* u_char	ni_isdotdot;		/* 1 => current component name is .. */
     /* u_char	ni_more;		/* 1 => symlink needs interpretation */
	/*
	 * Lookup params.
	 */
	struct componentname ni_cnd;
};
/*
 * Backwards compatibility.
 */
/* #define ni_nameiop	ni_cnd.cn_nameiop */
#define ni_cred		ni_cnd.cn_cred
#define ni_pnbuf	ni_cnd.cn_pnbuf
#define ni_namelen	ni_cnd.cn_namelen
#define ni_ptr		ni_cnd.cn_nameptr
#define ni_hash		ni_cnd.cn_hash
#define ni_flags	ni_cnd.cn_flags
#define ni_ufs		ni_cnd.cn_ufs

#ifdef KERNEL
/*
 * namei operations
 */
#define	LOOKUP		0	/* perform name lookup only */
#define	CREATE		1	/* setup for file creation */
#define	DELETE		2	/* setup for file deletion */
#define	RENAME		3	/* setup for file renaming */
#define	OPMASK		3	/* mask for operation */
/*
 * namei operational modifier flags, stored in ni_cnd.flags
 */
#define	LOCKLEAF	0x0004	/* lock inode on return */
#define	LOCKPARENT	0x0008	/* want parent vnode returned locked */
#define	WANTPARENT	0x0010	/* want parent vnode returned unlocked */
#define	NOCACHE		0x0020	/* name must not be left in cache */
#define	FOLLOW		0x0040	/* follow symbolic links */
#define	NOFOLLOW	0x0000	/* do not follow symbolic links (pseudo) */
#define	MODMASK		0x00fc	/* mask of operational modifiers */
/*
 * Namei parameter descriptors.
 *
 * SAVENAME may be set by either the callers of namei or by VOP_LOOKUP.
 * If the caller of namei sets the flag (for example execve wants to
 * know the name of the program that is being executed), then it must
 * free the buffer. If VOP_LOOKUP sets the flag, then the buffer must
 * be freed by either the commit routine or the VOP_ABORT routine.
 * SAVESTART is set only by the callers of namei. It implies SAVENAME
 * plus the addition of saving the parent directory that contains the
 * name in ni_startdir. It allows repeated calls to lookup for the
 * name being sought. The caller is responsible for releasing the
 * buffer and for vrele'ing ni_startdir.
 */
#define	NOCROSSMOUNT	0x00100	/* do not cross mount points */
#define	RDONLY		0x00200	/* lookup with read-only semantics */
#define	HASBUF		0x00400	/* has allocated pathname buffer */
#define	SAVENAME	0x00800	/* save pathanme buffer */
#define	SAVESTART	0x01000	/* save starting directory */
/* new: */
#define ISDOTDOT	0x02000	/* current component name is .. */
#define MAKEENTRY	0x04000	/* entry is to be added to name cache */
#define ISLASTCN	0x08000	/* this is last component of pathname */
#define ISSYMLINK	0x10000	/* symlink needs interpretation */
#define PARAMASK	0xfff00	/* mask of parameter descriptors */
#endif

/*
 * This structure describes the elements in the cache of recent
 * names looked up by namei. NCHNAMLEN is sized to make structure
 * size a power of two to optimize malloc's. Minimum reasonable
 * size is 15.
 */

#define	NCHNAMLEN	31	/* maximum name segment length we bother with */

struct	namecache {
	struct	namecache *nc_forw;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_back;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_nxt;	/* LRU chain */
	struct	namecache **nc_prev;	/* LRU chain */
	struct	vnode *nc_dvp;		/* vnode of parent of name */
	u_long	nc_dvpid;		/* capability number of nc_dvp */
	struct	vnode *nc_vp;		/* vnode the name refers to */
	u_long	nc_vpid;		/* capability number of nc_vp */
	char	nc_nlen;		/* length of name */
	char	nc_name[NCHNAMLEN];	/* segment name */
};

#ifdef KERNEL
u_long	nextvnodeid;
int	namei __P((struct nameidata *ndp, struct proc *p));
int	lookup __P((struct nameidata *ndp, struct proc *p));
#endif

/*
 * Stats on usefulness of namei caches.
 */
struct	nchstats {
	long	ncs_goodhits;		/* hits that we can really use */
	long	ncs_neghits;		/* negative hits that we can use */
	long	ncs_badhits;		/* hits we must drop */
	long	ncs_falsehits;		/* hits with id mismatch */
	long	ncs_miss;		/* misses */
	long	ncs_long;		/* long names that ignore cache */
	long	ncs_pass2;		/* names found with passes == 2 */
	long	ncs_2passes;		/* number of times we attempt it */
};
#endif /* !_NAMEI_H_ */
