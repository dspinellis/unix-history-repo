/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs.h	7.6 (Berkeley) %G%
 */

typedef struct buf	BUF;
typedef struct dinode	DINODE;
typedef struct inode	INODE;
typedef struct mount	MOUNT;
typedef struct ucred	UCRED;
typedef struct ufsmount	UFSMOUNT;
typedef struct vnode	VNODE;

#define	LFS_LABELPAD	8192		/* LFS label size */
#define	LFS_SBPAD	8192		/* LFS superblock size */
#define MAXMNTLEN	512		/* XXX move from fs.h to mount.h */

/* On-disk and in-memory checkpoint segment usage structure. */
typedef struct segusage SEGUSE;
struct segusage {
	u_long	su_nbytes;		/* number of live bytes */
	u_long	su_lastmod;		/* SEGUSE last modified timestamp */
#define	SEGUSE_ACTIVE		0x1	/* segment is currently being written */
#define	SEGUSE_DIRTY		0x2	/* segment has data in it */
#define	SEGUSE_SUPERBLOCK	0x4	/* segment contains a superblock */
	u_long	su_flags;
};

#define	SEGTABSIZE_SU(fs) \
	(((fs)->lfs_nseg * sizeof(SEGUSE) + \
	((fs)->lfs_bsize - 1)) >> (fs)->lfs_bshift)

/* On-disk file information.  One per file with data blocks in the segment. */
typedef struct finfo FINFO;
struct finfo {
	u_long	fi_nblocks;		/* number of blocks */
	u_long	fi_version;		/* version number */
	u_long	fi_ino;			/* inode number */
	long	fi_blocks[1];		/* array of logical block numbers */
};

/* On-disk and in-memory super block. */
struct lfs {
#define	LFS_MAGIC	0xbedead
	u_long	lfs_magic;		/* magic number */
#define	LFS_VERSION	1
	u_long	lfs_version;		/* version number */

	u_long	lfs_size;		/* number of blocks in fs */
	u_long	lfs_ssize;		/* number of blocks per segment */
	u_long	lfs_dsize;		/* number of data blocks in fs */
	u_long	lfs_bsize;		/* size of basic blocks in fs */
	u_long	lfs_fsize;		/* size of frag blocks in fs */
	u_long	lfs_frag;		/* number of frags in a block in fs */

/* Checkpoint region. */
	ino_t	lfs_free;		/* start of the free list */
	u_long	lfs_bfree;		/* number of free blocks */
	u_long	lfs_nfiles;		/* number of allocated inodes */
	daddr_t	lfs_idaddr;		/* inode file disk address */
	ino_t	lfs_ifile;		/* inode file inode number */
	daddr_t	lfs_lastseg;		/* address of last segment written */
	daddr_t	lfs_nextseg;		/* address of next segment to write */
	daddr_t	lfs_curseg;		/* current segment being written */
	daddr_t	lfs_offset;		/* offset in curseg for next partial */
	u_long	lfs_tstamp;		/* time stamp */

/* These are configuration parameters. */
	u_long	lfs_minfree;		/* minimum percentage of free blocks */

/* These fields can be computed from the others. */
	u_long	lfs_dbpseg;		/* disk blocks per segment */
	u_long	lfs_inopb;		/* inodes per block */
	u_long	lfs_ifpb;		/* IFILE entries per block */
	u_long	lfs_sepb;		/* SEGUSE entries per block */
	u_long	lfs_nindir;		/* indirect pointers per block */
	u_long	lfs_nseg;		/* number of segments */
	u_long	lfs_nspf;		/* number of sectors per fragment */
	u_long	lfs_cleansz;		/* cleaner info size in blocks */
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

#define	LFS_MIN_SBINTERVAL	5	/* minimum superblock segment spacing */
#define	LFS_MAXNUMSB		10	/* superblock disk offsets */
	daddr_t	lfs_sboffs[LFS_MAXNUMSB];

/* These fields are set at mount time and are meaningless on disk. */
	VNODE	*lfs_ivnode;		/* vnode for the ifile */
	SEGUSE	*lfs_segtab;		/* in-memory segment usage table */
					/* XXX NOT USED */
	void	*XXXlfs_seglist;	/* list of segments being written */
	u_long	lfs_iocount;		/* number of ios pending */
	u_char	lfs_fmod;		/* super block modified flag */
	u_char	lfs_clean;		/* file system is clean flag */
	u_char	lfs_ronly;		/* mounted read-only flag */
	u_char	lfs_flags;		/* currently unused flag */
	u_char	lfs_fsmnt[MAXMNTLEN];	/* name mounted on */
	u_char	pad[3];			/* long-align */

/* Checksum; valid on disk. */
	u_long	lfs_cksum;		/* checksum for superblock checking */
};

/*
 * Inode 0 is the out-of-band inode, and inode 1 is the inode number for the
 * ifile.  Thus the root inode is 2, and the lost+found inode is 3.
 */
#define	LOSTFOUNDINO	((ino_t)3)

/* Fixed inode numbers. */
#define	LFS_UNUSED_INUM	0		/* out of band inode number */
#define	LFS_IFILE_INUM	1		/* inode number of the ifile */
					/* first free inode number */
#define	LFS_FIRST_INUM	(LOSTFOUNDINO + 1)

/*
 * Used to access the first spare of the dinode which we use to store
 * the ifile number so we can identify them
 */
#define	di_inum	di_spare[0]

/* Address calculations for metadata located in the inode */
#define	S_INDIR(fs)	-NDADDR
#define	D_INDIR(fs)	(S_INDIR - NINDIR(fs) - 1)
#define	T_INDIR(fs)	(D_INDIR - NINDIR(fs) * NINDIR(fs) - 1)

/* Structure used to pass around logical block paths. */
typedef struct _indir {
	long	in_lbn;			/* logical block number */
	int	in_off;			/* offset in buffer */
} INDIR;

/* Unassigned disk address. */
#define	UNASSIGNED	-1

typedef struct ifile IFILE;
struct ifile {
	u_long	if_version;		/* inode version number */
#define	LFS_UNUSED_DADDR	0	/* out-of-band daddr */
	daddr_t	if_daddr;		/* inode disk address */
	union {
		ino_t	nextfree;	/* next-unallocated inode */
		time_t	st_atime;	/* access time */
	} __ifile_u;
#define	if_st_atime	__ifile_u.st_atime
#define	if_nextfree	__ifile_u.nextfree
};

/*
 * Cleaner information structure.  This resides in the ifile and is used
 * to pass information between the cleaner and the kernel.
 */
typedef struct _cleanerinfo {
	u_long	clean;			/* K: number of clean segments */
	u_long	dirty;			/* K: number of dirty segments */
} CLEANERINFO;

#define	CLEANSIZE_SU(fs) \
	((sizeof(CLEANERINFO) + (fs)->lfs_bsize - 1) >> (fs)->lfs_bshift)

/*
 * All summary blocks are the same size, so we can always read a summary
 * block easily from a segment.
 */
#define	LFS_SUMMARY_SIZE	512

/* On-disk segment summary information */
typedef struct segsum SEGSUM;
struct segsum {
	u_long	ss_sumsum;		/* check sum of summary block */
	u_long	ss_datasum;		/* check sum of data */
	daddr_t	ss_next;		/* next segment */
	u_long	ss_create;		/* creation time stamp */
	u_long	ss_nfinfo;		/* number of file info structures */
	u_long	ss_ninos;		/* number of inodes in summary */
	/* FINFO's... */
};

/* NINDIR is the number of indirects in a file system block. */
#define	NINDIR(fs)	((fs)->lfs_nindir)

/* INOPB is the number of inodes in a secondary storage block. */
#define	INOPB(fs)	((fs)->lfs_inopb)

#define	blksize(fs)		((fs)->lfs_bsize)
#define	blkoff(fs, loc)		((loc) & (fs)->lfs_bmask)
#define	fsbtodb(fs, b)		((b) << (fs)->lfs_fsbtodb)
#define	lblkno(fs, loc)		((loc) >> (fs)->lfs_bshift)
#define	lblktosize(fs, blk)	((blk) << (fs)->lfs_bshift)
#define numfrags(fs, loc)	/* calculates (loc / fs->fs_fsize) */ \
	((loc) >> (fs)->lfs_bshift)

/* Read in the block with the cleaner info from the ifile. */
#define LFS_CLEANERINFO(CP, F, BP) { \
	if (bread((F)->lfs_ivnode, (daddr_t)0, (F)->lfs_bsize, NOCRED, &BP)) \
		panic("lfs: ifile read"); \
	(CP) = (CLEANERINFO *)BP->b_un.b_addr; \
}

/* Read in the block with a specific inode from the ifile. */
#define	LFS_IENTRY(IP, F, IN, BP) { \
	if (bread((F)->lfs_ivnode, \
	    (IN) / (F)->lfs_ifpb + (F)->lfs_cleansz + (F)->lfs_segtabsz, \
	    (F)->lfs_bsize, NOCRED, &BP)) \
		panic("lfs: ifile read"); \
	(IP) = (IFILE *)BP->b_un.b_addr + IN % (F)->lfs_ifpb; \
}

/* Read in the block with a specific segment usage entry from the ifile. */
#define	LFS_SEGENTRY(SP, F, IN, BP) { \
	if (bread((F)->lfs_ivnode, (IN) / (F)->lfs_sepb + (F)->lfs_cleansz, \
	    (F)->lfs_bsize, NOCRED, &BP)) \
		panic("lfs: ifile read"); \
	(SP) = (SEGUSE *)BP->b_un.b_addr + IN % (F)->lfs_sepb; \
}

/*
 * Structures used by lfs_bmapv and lfs_markv to communicate information
 * about inodes and data blocks.
 */
typedef struct block_info {
	ino_t	bi_inode;		/* inode # */
	off_t	bi_lbn;			/* logical block w/in file */
	daddr_t	bi_daddr;		/* disk address of block */
	time_t	bi_segcreate;		/* origin segment create time */
	void	*bi_bp;			/* data buffer */
} BLOCK_INFO;

typedef struct inode_info {
	ino_t	ii_inode;		/* inode # */
	daddr_t	ii_daddr;		/* disk address of block */
	time_t	ii_segcreate;		/* origin segment create time */
	struct dinode *ii_dinode;	/* data buffer */
} INODE_INFO;
