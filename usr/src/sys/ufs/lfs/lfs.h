/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs.h	5.1 (Berkeley) %G%
 */

#define	MAXMNTLEN	512		/* XXX move from fs.h to mount.h */
#define	LFSBLKSIZE	(4*1024)

/* On-disk super block. */
typedef struct lfs_super {
#define	LFS_MAGIC	0xabababab
	u_long	lfs_magic;		/* magic number */
	u_long	lfs_version;		/* version number */

	u_long	lfs_size;		/* number of blocks in fs */
	u_long	lfs_ssize;		/* number of blocks per segment */
	u_long	lfs_dsize;		/* number of data blocks in fs */
	u_long	lfs_bsize;		/* size of basic blocks in fs */
	u_long	lfs_fsize;		/* size of frag blocks in fs */
	u_long	lfs_frag;		/* number of frags in a block in fs */
	u_long	lfs_sbsize;		/* actual size of super block */

/* Checkpoint region. */
	ino_t	lfs_free;		/* start of the free list */
	daddr_t	lfs_idaddr;		/* inode file disk address */
	ino_t	lfs_ifile;		/* inode file inode number */
	daddr_t	lfs_lastseg;		/* last segment written */
	u_long	lfs_tstamp;		/* time stamp */

/* These are configuration parameters. */
	u_long	lfs_minfree;		/* minimum percentage of free blocks */

/* These fields can be computed from the others. */
	u_long	lfs_inopb;		/* inodes per block */
	u_long	lfs_ifpb;		/* IFILE entries per block */
	u_long	lfs_nindir;		/* indirect pointers per block */
	u_long	lfs_nseg;		/* number of segments */
	u_long	lfs_nspf;		/* number of sectors per fragment */
	u_long	lfs_segtabsz;		/* segment table size in blocks */

	u_long	lfs_segmask;		/* calculate offset within a segment */
	u_long	lfs_segshift;		/* fast mult/div for segments */
	u_long	lfs_bmask;		/* calc block offset from file offset */
	u_long	lfs_bshift;		/* calc block number from file offset */
	u_long	lfs_ffmask;		/* calc frag offset from file offset */
	u_long	lfs_ffshift;		/* fast mult/div for frag from file */
	u_long	lfs_fbmask;		/* calc frag offset from block offset */
	u_long	lfs_fbshift;		/* fast mult/div for frag from block */
	u_long	lfs_fsbtodb;		/* fsbtodb and dbtofsb shift constant */

#define	MAXNUMSB	10
	daddr_t	lfs_sboffs[MAXNUMSB];	/* super-block disk offsets */
} LFS_SUPER;

#define	blksize(fs, ip, lbn)	LFSBLKSIZE
#define	blkoff(fs, loc)		/* calculates (loc % fs->fs_bsize) */ \
	((loc) & ~(fs)->fs_bmask)
#define	fsbtodb(fs, b)		((b) << (fs)->fs_fsbtodb)
#define	lblkno(fs, loc)		/* calculates (loc / fs->fs_bsize) */ \
	((loc) >> (fs)->fs_bshift)
#define	itoo(fs, x)		((x) % INOPB(fs))
#define	itod(fs, x)		LFS -- IMPLEMENT

/* In-memory super block. */
typedef struct lfs {
	struct	fs *fs_link;		/* linked list of file systems */
	struct	fs *fs_rlink;		/*     used for incore super blocks */
	time_t	fs_time;		/* last time written */

/* These fields are cleared at mount time. */
	u_char	fs_fmod;		/* super block modified flag */
	u_char	fs_clean;		/* file system is clean flag */
	u_char	fs_ronly;		/* mounted read-only flag */
	u_char	fs_flags;		/* currently unused flag */
	u_char	fs_fsmnt[MAXMNTLEN];	/* name mounted on */

/* On-disk structure. */
	LFS_SUPER fs_super;
} LFS;

#define	fs_bmask	fs_super.lfs_bmask
#define	fs_bshift	fs_super.lfs_bshift
#define	fs_bsize	fs_super.lfs_bsize
#define	fs_dsize	fs_super.lfs_dsize
#define	fs_fbmask	fs_super.lfs_fbmask
#define	fs_fbshift	fs_super.lfs_fbshift
#define	fs_ffmask	fs_super.lfs_ffmask
#define	fs_ffshift	fs_super.lfs_ffshift
#define	fs_frag		fs_super.lfs_frag
#define	fs_free		fs_super.lfs_free
#define	fs_fsbtodb	fs_super.lfs_fsbtodb
#define	fs_fsize	fs_super.lfs_fsize
#define	fs_idaddr	fs_super.lfs_idaddr
#define	fs_ifile	fs_super.lfs_ifile
#define	fs_ifpb		fs_super.lfs_ifpb
#define	fs_inopb	fs_super.lfs_inopb
#define	fs_lastseg	fs_super.lfs_lastseg
#define	fs_magic	fs_super.lfs_magic
#define	fs_minfree	fs_super.lfs_minfree
#define	fs_nindir	fs_super.lfs_nindir
#define	fs_nseg		fs_super.lfs_nseg
#define	fs_nspf		fs_super.lfs_nspf
#define	fs_sboffs	fs_super.lfs_sboffs
#define	fs_sbsize	fs_super.lfs_sbsize
#define	fs_segmask	fs_super.lfs_segmask
#define	fs_segshift	fs_super.lfs_segshift
#define	fs_segtabsz	fs_super.lfs_segtabsz
#define	fs_size		fs_super.lfs_size
#define	fs_ssize	fs_super.lfs_ssize
#define	fs_tstamp	fs_super.lfs_tstamp
#define	fs_version	fs_super.lfs_version

/* Data structures in the ifile */
#define	IFILE_NUM	1		/* inode number of the ifile */

typedef struct ifile {
	u_long	if_version;		/* inode version number */
#define	UNUSED_DADDR	0		/* out-of-band daddr */
	daddr_t	if_daddr;		/* inode disk address */
	union {
		ino_t	nextfree;	/* next-unallocated inode */
		time_t	st_atime;	/* access time */
	} __ifile_u;
#define	if_st_atime	__ifile_u.st_atime;
#define	if_nextfree	__ifile_u.nextfree;
} IFILE;

/* Segment table size, in blocks. */
#define	SEGTABSIZE(fs) \
	(((fs)->fs_nseg * sizeof(SEGUSAGE) + \
	    ((fs)->fs_bsize - 1)) << (fs)->fs_bshift)

/* In-memory and on-disk checkpoint segment usage structure. */
typedef struct segusage {
	u_long	su_nbytes;		/* number of live bytes */
	u_long	su_lastmod;		/* last modified timestamp */
} SEGUSAGE;

/* On-disk segment summary information */
typedef struct segsum {
	daddr_t	ss_next;		/* next segment */
	daddr_t	ss_prev;		/* next segment */
	daddr_t	ss_nextsum;		/* next summary block */
	u_long	ss_create;		/* creation time stamp */
	u_long	ss_nfinfo;		/* number of file info structures */
	u_long	ss_niinfo;		/* number of inode info structures */
	u_long	ss_cksum;		/* check sum */
} SEGSUM;

/* On-disk file information.  One per file with data blocks in the segment. */
typedef struct finfo {
	u_long	fi_nblocks;		/* number of blocks */
	u_long	fi_version;		/* version number */
	ino_t	fi_ino;			/* inode number */
	u_long	fi_blocks[1];		/* array of logical block numbers */
} FINFO;

/* On-disk inode information.  One per block of inodes in the segment. */
typedef struct iinfo {
	u_long	ii_ninodes;		/* number of inodes */
	ino_t	ii_inodes;		/* array of inode numbers */
} IINFO;
