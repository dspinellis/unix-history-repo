/* Copyright (c) 1981 Regents of the University of California */

/*	fs.h	1.10	%G%	*/

/*
 * Each disk drive contains some number of file systems.
 * A file system consists of a number of cylinder groups.
 * Each cylinder group has inodes and data.
 *
 * A file system is described by its super-block, which in turn
 * describes the cylinder groups.  The super-block is critical
 * data and is replicated in each cylinder group to protect against
 * catastrophic loss.  This is done at mkfs time and the critical
 * super-block data does not change, so the copies need not be
 * referenced further unless disaster strikes.
 *
 * For file system fs and a cylinder group number cg:
 *	[BBLOCK]	Boot sector
 *	[SBLOCK]	Super-block
 *	[CBLOCK(fs)]	Cylinder group block
 *	[IBLOCK(fs)..IBLOCK(fs)+fs.fs_ipg/INOPB(fs))
 *			Inode blocks
 *	[IBLOCK(fs)+fs.fs_ipg/INOPB(fs)..fs.fs_fpg/fs.fs_frag)
 *			Data blocks
 * The beginning of data blocks for cg in fs is also given by
 * the ``cgdmin(cg,fs)'' macro.
 *
 * The boot and super blocks are given in absolute disk addresses.
 */
#define BBSIZE		1024
#define SBSIZE		8192
#define	BBLOCK		((daddr_t)(0))
#define	SBLOCK		((daddr_t)(BBLOCK + BBSIZE / DEV_BSIZE))
/*
 * The cylinder group and inode blocks are given in file system
 * addresses, and hence must be converted to disk addresses by
 * the ``fsbtodb(fs, bno)'' macro.
 */
#define	CBLOCK(fs) \
  ((daddr_t)(roundup(howmany(BBSIZE + SBSIZE, (fs)->fs_fsize), (fs)->fs_frag)))
#define	IBLOCK(fs)	((daddr_t)(CBLOCK(fs) + (fs)->fs_frag))

/*
 * Addresses stored in inodes are capable of addressing fragments
 * of `blocks'. File system blocks of at most size MAXBSIZE can 
 * be optionally broken into 2, 4, or 8 pieces, each of which is
 * addressible; these pieces may be DEV_BSIZE, or some multiple of
 * a DEV_BSIZE unit.
 *
 * Large files consist of exclusively large data blocks.  To avoid
 * undue wasted disk space, the last data block of a small file may be
 * allocated as only as many fragments of a large block as are
 * necessary.  The file system format retains only a single pointer
 * to such a fragment, which is a piece of a single large block that
 * has been divided.  The size of such a fragment is determinable from
 * information in the inode, using the ``blksize(fs, ip, lbn)'' macro.
 *
 * The file system records space availability at the fragment level;
 * to determine block availability, aligned fragments are examined.
 *
 * The root inode is the root of the file system.
 * Inode 0 can't be used for normal purposes and
 * historically bad blocks were linked to inode 1,
 * thus the root inode is 2. (inode 1 is no longer used for
 * this purpose, however numerous dump tapes make this
 * assumption, so we are stuck with it)
 * The lost+found directory is given the next available
 * inode when it is created by ``mkfs''.
 */
#define	ROOTINO		((ino_t)2)	/* i number of all roots */
#define LOSTFOUNDINO	(ROOTINO + 1)

/*
 * MINFREE gives the minimum acceptable percentage of file system
 * blocks which may be free. If the freelist drops below this level
 * only the superuser may continue to allocate blocks. This may
 * be set to 0 if no reserve of free blocks is deemed necessary,
 * however severe performance degredations will be observed if the
 * file system is run at greater than 90% full; thus the default
 * value of fs_minfree is 10%.
 *
 * Empirically the best trade-off between block fragmentation and
 * overall disk utilization at a loading of 90% comes with a
 * fragmentation of 4, thus the default fragment size is a fourth
 * of the block size.
 */
#define MINFREE		10
#define DESFRAG		4

/*
 * Under current technology, most 300MB disks have 32 sectors and
 * 19 tracks, thus these are the defaults used for fs_nsect and 
 * fs_ntrak respectively.
 */
#define DFLNSECT	32
#define DFLNTRAK	19

/*
 * Cylinder group related limits.
 *
 * For each cylinder we keep track of the availability of blocks at different
 * rotational positions, so that we can lay out the data to be picked
 * up with minimum rotational latency.  NRPOS is the number of rotational
 * positions which we distinguish.  With NRPOS 8 the resolution of our
 * summary information is 2ms for a typical 3600 rpm drive.
 *
 * ROTDELAY gives the minimum number of milliseconds to initiate
 * another disk transfer on the same cylinder. It is used in
 * determining the rotationally optimal layout for disk blocks
 * within a file; the default of fs_rotdelay is 2ms.
 */
#define	NRPOS		8	/* number distinct rotational positions */
#define ROTDELAY	2

/*
 * Each file system has a number of inodes statically allocated.
 * We allocate one inode slot per NBPI bytes, expecting this
 * to be far more than we will ever need.
 *
 * MAXIPG bounds the number of inodes per cylinder group, and
 * is needed only to keep the structure simpler by having the
 * only a single variable size element (the free bit map).
 *
 * N.B.: MAXIPG must be a multiple of INOPB(fs).
 */
#define	NBPI		2048
#define	MAXIPG		2048	/* max number inodes/cyl group */

/*
 * MINBSIZE is the smallest allowable block size.
 * In order to insure that it is possible to create files of size
 * 2^32 with only two levels of indirection, MINBSIZE is set to 4096.
 * MINBSIZE must be big enough to hold a cylinder group block,
 * thus changes to (struct cg) must keep its size within MINBSIZE.
 * MAXCPG is limited only to dimension an array in (struct cg);
 * it can be made larger as long as that structures size remains
 * within the bounds dictated by MINBSIZE.
 * Note that super blocks are always of size MAXBSIZE,
 * and that MAXBSIZE must be >= MINBSIZE.
 */
#define MINBSIZE	4096
#define	DESCPG		16	/* desired fs_cpg */
#define	MAXCPG		32	/* maximum fs_cpg */

/*
 * Per cylinder group information; summarized in blocks allocated
 * from first cylinder group data blocks.  These blocks have to be
 * read in from fs_csaddr (size fs_cssize) in addition to the
 * super block.
 *
 * N.B. sizeof(struct csum) must be a power of two in order for
 * the ``fs_cs'' macro to work (see below).
 */
struct csum {
	long	cs_ndir;	/* number of directories */
	long	cs_nbfree;	/* number of free blocks */
	long	cs_nifree;	/* number of free inodes */
	long	cs_nffree;	/* number of free frags */
};

/*
 * Super block for a file system.
 */
#define	FS_MAGIC	0x110854
struct	fs
{
	long	fs_magic;		/* magic number */
	daddr_t	fs_sblkno;		/* offset of super-block in filesys */
	time_t 	fs_time;    		/* last time written */
	long	fs_size;		/* number of blocks in fs */
	long	fs_dsize;		/* number of data blocks in fs */
	long	fs_ncg;			/* number of cylinder groups */
	long	fs_bsize;		/* size of basic blocks in fs */
	long	fs_fsize;		/* size of frag blocks in fs */
	long	fs_frag;		/* number of frags in a block in fs */
	short	fs_minfree;		/* minimum percentage of free blocks */
	short	fs_rotdelay;		/* num of ms for optimal next block */
/* sizes determined by number of cylinder groups and their sizes */
	daddr_t fs_csaddr;		/* blk addr of cyl grp summary area */
	long	fs_cssize;		/* size of cyl grp summary area */
	long	fs_cgsize;		/* cylinder group size */
/* these fields should be derived from the hardware */
	short	fs_ntrak;		/* tracks per cylinder */
	short	fs_nsect;		/* sectors per track */
	long  	fs_spc;   		/* sectors per cylinder */
/* this comes from the disk driver partitioning */
	long	fs_ncyl;   		/* cylinders in file system */
/* these fields can be computed from the others */
	short	fs_cpg;			/* cylinders per group */
	short	fs_ipg;			/* inodes per group */
	long	fs_fpg;			/* blocks per group * fs_frag */
/* this data must be re-computed after crashes */
	struct	csum fs_cstotal;	/* cylinder summary information */
/* these fields are cleared at mount time */
	char   	fs_fmod;    		/* super block modified flag */
	char   	fs_ronly;   		/* mounted read-only flag */
	char	fs_fsmnt[34];		/* name mounted on */
/* these fields retain the current block allocation info */
	long	fs_cgrotor;		/* last cg searched */
	struct	csum *fs_csp[NBUF];	/* list of fs_cs info buffers */
	short	fs_postbl[NRPOS];	/* head of blocks for each rotation */
	short	fs_rotbl[1];		/* list of blocks for each rotation */
/* actually longer */
};

/*
 * convert cylinder group to base address of its global summary info.
 *
 * N.B. This macro assumes that sizeof(struct csum) is a power of two.
 */
#define fs_cs(fs, indx) \
	fs_csp[(indx) / ((fs)->fs_bsize / sizeof(struct csum))] \
	[(indx) % ((fs)->fs_bsize / sizeof(struct csum))]

/*
 * MAXBPC bounds the number of blocks of data per cylinder,
 * and is limited by the fact that the super block is of size SBSIZE.
 * Its size is derived from the size of blocks and the (struct fs) size,
 * by the number of remaining bits.
 */
#define	MAXBPC	((SBSIZE - sizeof (struct fs)) / sizeof(short))

/*
 * Cylinder group block for a file system.
 */
#define	CG_MAGIC	0x092752
struct	cg {
	long	cg_magic;		/* magic number */
	time_t	cg_time;		/* time last written */
	long	cg_cgx;			/* we are the cgx'th cylinder group */
	short	cg_ncyl;		/* number of cyl's this cg */
	short	cg_niblk;		/* number of inode blocks this cg */
	long	cg_ndblk;		/* number of data blocks this cg */
	struct	csum cg_cs;		/* cylinder summary information */
	long	cg_rotor;		/* position of last used block */
	long	cg_frotor;		/* position of last used frag */
	long	cg_irotor;		/* position of last used inode */
	long	cg_frsum[MAXFRAG];	/* counts of available frags */
	short	cg_b[MAXCPG][NRPOS];	/* positions of free blocks */
	char	cg_iused[MAXIPG/NBBY];	/* used inode map */
	char	cg_free[1];		/* free block map */
/* actually longer */
};

/*
 * MAXBPG bounds the number of blocks of data per cylinder group,
 * and is limited by the fact that cylinder groups are at most one block.
 * Its size is derived from the size of blocks and the (struct cg) size,
 * by the number of remaining bits.
 */
#define	MAXBPG(fs) \
	(NBBY * ((fs)->fs_bsize - (sizeof (struct cg))) / (fs)->fs_frag)

/*
 * Turn file system block numbers into disk block addresses.
 * This maps file system blocks to device size blocks.
 */
#define fsbtodb(fs, b)	((b) * ((fs)->fs_fsize / DEV_BSIZE))
#define	dbtofsb(fs, b)	((b) / ((fs)->fs_fsize / DEV_BSIZE))

/*
 * Cylinder group macros to locate things in cylinder groups.
 *
 * cylinder group to disk block at very beginning
 */
#define	cgbase(c,fs)	((daddr_t)((fs)->fs_fpg * (c)))

/*
 * cylinder group to spare super block address
 */
#define	cgsblock(c,fs)	(cgbase(c,fs) + dbtofsb(fs, SBLOCK))

/*
 * convert cylinder group to index of its cg block
 */
#define	cgtod(c,fs)	(cgbase(c,fs) + CBLOCK(fs))

/*
 * give address of first inode block in cylinder group
 */
#define	cgimin(c,fs)	(cgbase(c,fs) + IBLOCK(fs))

/*
 * give address of first data block in cylinder group
 */
#define	cgdmin(c,fs)	(cgimin(c,fs) + (fs)->fs_ipg / INOPF(fs))

/*
 * turn inode number into cylinder group number
 */
#define	itog(x,fs)	((x) / (fs)->fs_ipg)

/*
 * turn inode number into file system block address
 */
#define	itod(x,fs) \
	((daddr_t)(cgimin(itog(x,fs),fs) + (fs)->fs_frag * \
	((x) % (fs)->fs_ipg / INOPB(fs))))

/*
 * turn inode number into file system block offset
 */
#define	itoo(x,fs)	((x) % INOPB(fs))

/*
 * give cylinder group number for a file system block
 */
#define	dtog(d,fs)	((d) / (fs)->fs_fpg)

/*
 * give cylinder group block number for a file system block
 */
#define	dtogd(d,fs)	((d) % (fs)->fs_fpg)

/*
 * determining the size of a file block in the file system
 */
#define blksize(fs, ip, lbn) \
	(((lbn) >= NDADDR || (ip)->i_size >= ((lbn) + 1) * (fs)->fs_bsize) \
		? (fs)->fs_bsize \
		: (roundup((ip)->i_size % (fs)->fs_bsize, (fs)->fs_fsize)))
#define dblksize(fs, dip, lbn) \
	(((lbn) >= NDADDR || (dip)->di_size >= ((lbn) + 1) * (fs)->fs_bsize) \
		? (fs)->fs_bsize \
		: (roundup((dip)->di_size % (fs)->fs_bsize, (fs)->fs_fsize)))

/*
 * number of disk sectors per block; assumes DEV_BSIZE byte sector size
 */
#define	NSPB(fs)	((fs)->fs_bsize / DEV_BSIZE)
#define	NSPF(fs)	((fs)->fs_fsize / DEV_BSIZE)

/*
 * INOPB is the number of inodes in a secondary storage block
 */
#define	INOPB(fs)	((fs)->fs_bsize / sizeof (struct dinode))
#define	INOPF(fs)	((fs)->fs_fsize / sizeof (struct dinode))

/*
 * NINDIR is the number of indirects in a file system block
 */
#define	NINDIR(fs)	((fs)->fs_bsize / sizeof (daddr_t))

#ifdef KERNEL
struct	fs *getfs();
#endif
