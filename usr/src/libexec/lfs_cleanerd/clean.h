/*
 * The LFS user-level library will be used when writing cleaners and
 * checkers for LFS file systems.  It will have facilities
 * for finding and parsing LFS segments.
 */

#define IFILE_NAME "ifile"

#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

typedef struct fs_info {
	struct	statfs	*fi_statfsp;	/* fsstat info from getfsstat */
	struct	lfs	*fi_lfsp;	/* superblock */
					/*
					 * shared cleaner info data 
					 * (from top of ifile) 
					 */
	CLEANERINFO	*fi_cip;
	SEGUSE	*fi_segusep;		/* segment usage table (from ifile) */
	IFILE	*fi_ifilep;		/* ifile table (from ifile) */
	u_long	fi_daddr_shift;		/* shift to get byte offset of daddr */
	u_long	fi_ifile_count;		/* # entries in the ifile table */
	u_long	fi_ifile_length;	/* length of the ifile */
} FS_INFO;


#define fsid		(fsp->fi_statfsp->f_fsid)
#define statfsp		(fsp->fi_statfsp)
#define lfsp		(fsp->fi_lfsp)
#define cip		(fsp->fi_cip)
#define segusep		(fsp->fi_segusep)
#define ifilep		(fsp->fi_ifilep)
#define ifile_count	(fsp->fi_ifile_count)
#define ifile_length	(fsp->fi_ifile_length)

/* 
 * XXX: size (in bytes) of a segment
 *	should lfs_bsize be fsbtodb(fs,1), blksize(fs), or lfs_dsize? 
 */
#define seg_size(fs) ((fs)->lfs_ssize<<(fs)->lfs_bshift)

/* daddr -> byte offset */
#define datobyte(fs, da) ((da)<<(fs)->fi_daddr_shift)
#define bytetoda(fs, byte) ((byte)>>(fs)->fi_daddr_shift)

#define CLEANSIZE(fs)	(CLEANSIZE_SU(fs) << fs->lfs_bshift)
#define SEGTABSIZE(fs)	(SEGTABSIZE_SU(fs) << fs->lfs_bshift)

#define IFILE_ENTRY(fs, if, i)	((IFILE*)((caddr_t)(if) + \
	(fs)->lfs_bsize*((i)/(fs)->lfs_ifpb) + \
	sizeof(IFILE)*((i)%(fs)->lfs_ifpb)))
#define SEGUSE_ENTRY(fs, su, i) ((SEGUSE*)((caddr_t)(su) + \
	(fs)->lfs_bsize*((i)/(fs)->lfs_sepb) + \
	sizeof(IFILE)*((i)%(fs)->lfs_sepb)))

/*
 * fs_getmntinfo:
 *
 *    This function will get information on all mounted file systems
 * with the given type.  It will return the number of mounted file
 * systems with the right type.  It will return in *buf a pointer to
 * the array of statfs structures.
 */
extern int
fs_getmntinfo __P((struct statfs **buf, int type));

/*
 * get_fs_info:
 *
 * get all the information available on a file system
 */
extern int
get_fs_info __P((struct statfs *lstatfsp, FS_INFO **fspp, int count));

extern void
free_fs_info __P((FS_INFO *fsp, int count));

/* 
 * get_superblock: 
 *    gets the superblock from disk (possibly in face of errors) 
 */
extern int
get_superblock __P((FS_INFO *fsp, struct lfs *sbp));


/* 
 * get_ifile: 
 *    This function will map the ifile into memory.  It returns
 * NULL on failure.
 */
extern int
get_ifile __P((FS_INFO *fsp));

/*
 * segmapv:
 *
 *   This function will scan a segment and return a list of
 * <inode, blocknum> pairs which indicate which blocks were
 * contained as live data within the segment at some point
 * (it may have "died" since then).  Any given pair will be 
 * listed at most once.
 */
extern int 
lfs_segmapv __P((FS_INFO *fsp, int seg, caddr_t seg_buf, 
		BLOCK_INFO **blocks, int *bcount, 
		INODE_INFO **inodes, int *icount));

/* 
 * this will parse a partial segment and create a vector of block_info's
 * for live data blocks for live inodes.  It will not include blocks or 
 * inodes from files with new version numbers.  
 */
extern void
pseg_blocks __P((FS_INFO *fsp, int seg, SEGSUM *s, caddr_t seg_buf, 
		BLOCK_INFO **blocks, int *count));

/* 
 * this will parse a partial segment and create a vector of inode_info's
 * for live inodes.  It will not include blocks or inodes from files 
 * with new version numbers.  
 */
extern void
pseg_inodes __P((FS_INFO *fsp, int seg, SEGSUM *s, caddr_t seg_buf, 
		INODE_INFO **inodes, int *count));

/* 
 * return the size of the partial segment in bytes. 
 */
extern u_long
pseg_size __P((FS_INFO *fsp, SEGSUM *s));


/* 
 * join block list b with list a (eliminating duplicates), leaving result
 * in list a.
 */
extern void
pseg_bjoin __P((FS_INFO *fsp, BLOCK_INFO **ablocks, int *acount, 
		BLOCK_INFO *bblocks, int bcount));


/* 
 * join inode list b with list a (eliminating duplicates), leaving result
 * in list a.
 */
extern void
pseg_ijoin __P((FS_INFO *fsp, INODE_INFO **ainodes, int *acount, 
		INODE_INFO *binodes, int bcount));


/* is the segsum block valid? return TRUE if it is, FALSE otherwise */
extern int 
segsum_valid __P((FS_INFO *fsp, SEGSUM *ssp));


/*
 * pseg_valid:
 *
 * returns 1 if the partial segment is valid, and 0 if it is invalid.
 * it uses the checksums to verify validity.
 */	 
extern int
pseg_valid __P((FS_INFO *fsp, SEGSUM *ssp));


/* 
 * pseg_finfos:
 * 
 * get array of FINFO pointers for partial segment
 * return the array in finfos, and the size of the array in count
 */
extern void
pseg_finfos __P((FS_INFO *fsp, SEGSUM *ssp, FINFO ***finfos, int *count));

/*
 * blocksize:
 *
 * returns the size (in bytes) of a (logical) block.
 * this is used because lfs uses different block sizes, depending
 * on the logical # of the block.  Lfs uses various sizes so
 * it doesn't need fragments.
 */ 
extern u_long
blocksize __P((FS_INFO *fsp, int index));

/*
 * finfo_size:
 *
 * returns the size in bytes of an FINFO structure 
 */
extern u_long
finfo_size __P((FINFO *finfop));
	
/*
 * Simple, general purpose, fast checksum.  Data must be short-aligned.
 * Returns a u_long in case we ever want to do something more rigorous.
 *
 * XXX
 * Use the TCP/IP checksum instead.
 */
extern u_long
cksum __P((register void *str, register size_t len));

/* 
 * read a segment into a memory buffer
 */
extern int
mmap_segment __P((FS_INFO *fsp, int segment, caddr_t *seg_buf));

extern void
munmap_segment __P((FS_INFO *fsp, caddr_t seg_buf));


/*
 * USEFUL DEBUGGING TOOLS:
 */

extern void
print_IFILE __P((IFILE *p));

extern void
print_SEGUSE __P((SEGUSE *p));

extern void
print_CLEANERINFO __P((CLEANERINFO *p));

extern void
print_SEGSUM __P((SEGSUM *p));

extern void
print_time_t __P((time_t t));

extern void
print_BLOCK_INFO __P((BLOCK_INFO *p));

extern void
print_INODE_INFO __P((INODE_INFO *p));

extern void
print_FINFO __P((FINFO *p));

extern void
print_lfs __P((struct lfs *p));

