
#include <sys/param.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/vnode.h>
#include <sys/stat.h>
#include <sys/mount.h>

#include <ufs/ufs/dinode.h>
#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include "clean.h"

/*
 * fs_getmntinfo:
 *
 *    This function will get information on all mounted file systems
 * of a given type.
 */
int
fs_getmntinfo(buf, type)
	struct	statfs	**buf;
	int	type;
{
	int	i;
	int	count;
	int	tcount;
	struct	statfs	*tstatfsp;

	tcount = getmntinfo(&tstatfsp, 0);

	if (tcount < 0) {
		perror ("fs_getmntinfo: getmntinfo failed");
		return -1;
	}

	for (count = 0, i = 0 ; i < tcount ; i ++)
		if (type == 0 || tstatfsp[i].f_type == type)
			++ count;

	if (count > 0) {
		*buf = (struct statfs *)
			malloc(count*sizeof(struct statfs));
		if (*buf == 0) { perror ("fs_getmntinfo: out of space"); exit (1); }
		for (i = 0, count = 0 ; i < tcount ; i ++) {
			if (type == 0 || tstatfsp[i].f_type == type) {
				(*buf)[count] = tstatfsp[i];
				++count;
			}
		}
		return count;
	}
	return 0;
}

/*
 * get_fs_info:
 *
 * get all the information available on a file system
 */
int
get_fs_info (lstatfsp, fspp, count)
	struct  statfs  *lstatfsp;	/* IN: array of statfs structs */
	FS_INFO **fspp;			/* OUT: resulting array of FS_INFOs */
	int	count;			/* IN: number of file systems */
{
	int	i;
	caddr_t	ifp;
	FS_INFO	*fsp;
	
	*fspp = (FS_INFO *)malloc(count * sizeof(FS_INFO));

	for (i = 0 ; i < count ; i++) {
		fsp = *fspp + i;
		statfsp = lstatfsp + i;
		lfsp = (struct lfs *)malloc (LFS_SBPAD);
		if (get_superblock (fsp, lfsp) < 0) {
			perror("get_fs_info: get_superblock failed");
			return -1;
		}
		fsp->fi_daddr_shift = lfsp->lfs_bshift - lfsp->lfs_fsbtodb;
		if (get_ifile (fsp) < 0) {
			perror("get_fs_info: get_ifile failed");
			return -1;
		}
	}
	return 0;
}

/* this is needed temporarily, because of the bug in mmap'ed files */
void
free_fs_info (fsp, count)
	FS_INFO *fsp;	/* IN: array of fs_infos we will dispose of */
	int	count;	/* IN: number of file systems */
{
	int	i;
	caddr_t	fsp_base = (caddr_t)fsp;
	
	for (i = 0 ; i < count ; i++, fsp++) {
		/* free superblock */
		free (lfsp);
		/* sdp points to the beginning of the ifile area */
#ifndef MMAP_WORKS
		free (cip);
#else
		if (munmap (cip, ifile_length) < 0) {
			perror("free_fs_info: munmap failed\n");
		}
#endif /* MMAP_WORKS */
	}
	
	free (fsp_base);
}

/* 
 * get_superblock: 
 *    gets the superblock from disk (possibly in face of errors) 
 */
int
get_superblock (fsp, sbp)
	FS_INFO *fsp;	/* IN: array of fs_infos we will dispose of */
	struct	lfs	*sbp;
{
        int 	fid;
	char	mntfromname[MNAMELEN+1];

	strcpy(mntfromname, "/dev/r");
	strcat(mntfromname, statfsp->f_mntfromname+5);

	if ((fid = open(mntfromname, O_RDONLY, (mode_t)0)) < 0) {
		perror("get_superblock: bad open");
		return -1;
	}

	if(lseek (fid, LFS_LABELPAD, SEEK_SET) != LFS_LABELPAD) {
		perror("get_superblock: bad seek");
		return -1;
	}
	if(read (fid, (char *)sbp, LFS_SBPAD) != LFS_SBPAD) {
		perror("get_superblock: bad read");
		return -1;
	}
	close (fid);
	
	return 0;
}

/* 
 * get_ifile: 
 *    This function will map the ifile into memory.  It returns
 * NULL on failure.
 */
int
get_ifile (fsp)
	FS_INFO	*fsp;
{
	int	fid;
	int	count;
	caddr_t	ifp = NULL;
	char    *ifile_name;
	struct	stat file_stat;

	ifile_name = (char *)
		malloc(strlen(statfsp->f_mntonname)+strlen(IFILE_NAME)+2);
	strcpy(ifile_name, statfsp->f_mntonname);
	strcat(ifile_name, "/");
	strcat(ifile_name, IFILE_NAME);

	if ((fid = open(ifile_name, O_RDWR, (mode_t)0)) < 0) {
		perror("get_ifile: bad open");
		return -1;
	}

	if(fstat (fid, &file_stat)) {
		perror("get_ifile: fstat failed");
		return -1;
	}
	ifile_length = file_stat.st_size;

	/* get the ifile */
#ifndef MMAP_WORKS
	ifp = (caddr_t)malloc (ifile_length);
	if (ifp == 0) {
		perror ("get_ifile: malloc failed, out of memory?"); 
		return -1;
	}
	count = read (fid, ifp, ifile_length);

	if (count != ifile_length) {
		perror("get_ifile: bad ifile read"); 
		return -1;
	}
#else	/* MMAP_WORKS */
	ifp = mmap ((caddr_t)0, ifile_length, PROT_READ|PROT_WRITE,
		MAP_FILE|MAP_SHARED, fid, (off_t)0);
	if (ifp < 0) {
		perror("get_ifile: mmap failed");
		return NULL;
	}
#endif	/* MMAP_WORKS */

	close (fid);

	cip = (CLEANERINFO*)ifp;
	segusep = (SEGUSE*)(ifp + CLEANSIZE(lfsp));
	ifilep  = (IFILE*)(ifp + CLEANSIZE(lfsp) + SEGTABSIZE(lfsp));
	/* # of bytes in ifile table */
	ifile_count = ifile_length - (CLEANSIZE(lfsp) + SEGTABSIZE(lfsp));
	/* # of ifile entries in ifile table */
	ifile_count = (ifile_count / lfsp->lfs_bsize) * lfsp->lfs_ifpb;
	free (ifile_name);
	return 0;
}


/*
 * segmapv:
 *
 *   This function will scan a segment and return a list of
 * <inode, blocknum> pairs which indicate which blocks were
 * contained as live data within the segment at some point
 * (it may have "died" since then).  Any given pair will be 
 * listed at most once.
 */
int 
lfs_segmapv(fsp, seg, seg_buf, blocks, bcount, inodes, icount)
	FS_INFO *fsp;		/* pointer to super block */
	int	seg;		/* the segment id */
	caddr_t	seg_buf;	/* the buffer containing the segment's data */
				/* OUT: array of block_info for live blocks */
	BLOCK_INFO	**blocks;
	int	*bcount;	/* OUT: number of active blocks in segment */
				/* OUT: array of inode_info for live inodes */
	INODE_INFO	**inodes;
	int	*icount;	/* OUT: number of active inodes in segment */
{
	caddr_t	s;
	caddr_t	endofseg;
	int	nextsum;
	u_long	sb_off;
	time_t	timestamp;
	
	*bcount = 0;
	*blocks = (BLOCK_INFO *)malloc (sizeof(BLOCK_INFO));
	
	*icount = 0;
	*inodes = (INODE_INFO *)malloc(sizeof(INODE_INFO));

	sb_off = (SEGUSE_ENTRY(lfsp, segusep, seg)->su_flags & SEGUSE_SUPERBLOCK) ?
		LFS_SBPAD : 0;

	for (s = seg_buf + sb_off, endofseg = seg_buf + seg_size(lfsp), 
	     timestamp = 0 ; 
	     s < endofseg ; 
	     s += pseg_size (fsp, (SEGSUM*)s)) {
		BLOCK_INFO	*pblocks;
		int		pbcount;
		INODE_INFO	*pinodes;
		int		picount;

#ifdef VERBOSE
		printf("lfs_segmapv: seg_buf = 0x%x, pseg_buf = 0x%x, offset = %lu (0x%x), pseg = \n\t",
			(u_int)seg_buf, (u_int)s, 
			(u_int)s - (u_int)seg_buf - (u_int)sb_off,
			(u_int)s - (u_int)seg_buf - (u_int)sb_off);
/* this can cause core dumps when printing an invalid segsum
 *		print_SEGSUM ((SEGSUM*)s);
 *		printf("\n");
 *		printf("pseg_size = %lu\n", pseg_size(fsp, (SEGSUM*)s));
 */
		fflush(stdout);
#endif /* VERBOSE */

		/* we have hit the end of the valid data */
		if (! pseg_valid (fsp, (SEGSUM*)s)) break;

		/* we have gone back in time and hit old data */
		if (timestamp > ((SEGSUM*)s)->ss_create) break;

		timestamp = ((SEGSUM*)s)->ss_create;

		/* get the block and inode list */
		pseg_blocks (fsp, seg, (SEGSUM*)s, seg_buf, 
			&pblocks, &pbcount);
		pseg_bjoin  (fsp, blocks, bcount, pblocks, pbcount);

		pseg_inodes (fsp, seg, (SEGSUM*)s, seg_buf, 
			&pinodes, &picount);
		pseg_ijoin  (fsp, inodes, icount, pinodes, picount);
		
		/* free the temporary tables */
		free (pblocks);
		free (pinodes);
	}
	
}

/* 
 * this will parse a partial segment and create a vector of block_info's
 * for live data and a vector of inode_info's for live inodes.  It will 
 * not include blocks or inodes from files with new version numbers.  
 */
void
pseg_blocks (fsp, seg, s, seg_buf, blocks, count)
	FS_INFO *fsp;		/* pointer to super block */
	int	seg;		/* the segment id */
	SEGSUM	*s;		/* (unvalidated) segsum pointer */
	caddr_t	seg_buf;	/* the buffer containing the segment's data */
				/* OUT: array of block_info for live blocks */
	BLOCK_INFO	**blocks;
	int	*count;		/* OUT: number of active blocks in segment */
{
	FINFO	**finfos;
	int	finfoc;
	int	blockc;
	int	i;
	int	j;
	int	ninob;		/* number of inode blocks passed */
	daddr_t	seg_daddr;
	daddr_t	*cur_iaddrp;	/* pointer to current inode block */
	u_long	offset;		/* the offset (in bytes) within the segment */

	*count = 0;
	*blocks = NULL;
	
	pseg_finfos (fsp, s, &finfos, &finfoc);

#ifdef VERBOSE
	for(i=0;i<finfoc;i++){print_FINFO(finfos[i]);printf("\n");fflush(stdout);}
	printf("pseg_blocks: finfoc = %d\n", finfoc);fflush(stdout);
#endif

	/* count how many blocks are held by live FINFO's */
	for (i = 0, blockc = 0 ; i < finfoc ; ++i)
		if (finfos[i]->fi_version == 
		    IFILE_ENTRY(lfsp, ifilep, finfos[i]->fi_ino)->if_version) 
			blockc += finfos[i]->fi_nblocks;

	if (finfoc == 0 || blockc == 0) return;
	
	ninob = 0;
	offset = LFS_SUMMARY_SIZE + ((u_int)s - (u_int)seg_buf) + 
		s->ss_next * datobyte(fsp, 1<<lfsp->lfs_bshift);
	cur_iaddrp = (daddr_t*)(s->ss_ninos == 0 ? 0 :
	    (char *)s + LFS_SUMMARY_SIZE - sizeof(daddr_t));
	seg_daddr = sntoda(lfsp, seg);
	*blocks = (BLOCK_INFO *)malloc (blockc*sizeof(BLOCK_INFO));

	for (i = 0 ; i < finfoc ; i ++) {
		FINFO		*f = finfos[i];

		if (f->fi_version != IFILE_ENTRY(lfsp, ifilep, f->fi_ino)->if_version) 
			continue;

#ifdef VERBOSE
		printf("finfo %d = ", i);
		print_FINFO(f);
		printf("\n");
		fflush(stdout);
		printf("IFILE entry for file %d = ", f->fi_ino);
		print_IFILE (IFILE_ENTRY(lfsp, ifilep, f->fi_ino));
		printf("\n");
		fflush(stdout);
#endif
		for (j = 0 ; j < finfos[i]->fi_nblocks ; j ++) {
			BLOCK_INFO	*b = &(*blocks)[*count];
		
			/*
			 * XXX: 
			 * this changes if we have variable size blocks
			 */
			for (;cur_iaddrp && 
			    seg_daddr + bytetoda(fsp, offset) == *cur_iaddrp; 
			    offset += datobyte(fsp, 1<<lfsp->lfs_bshift)) {
				if (ninob <= (s->ss_ninos + INOPB(lfsp) - 1) 
				    / INOPB(lfsp)) {
					++ninob;
					--cur_iaddrp;
				} else
					cur_iaddrp = NULL;
			}
			b->bi_inode = f->fi_ino;
			b->bi_lbn = f->fi_blocks[j];
			b->bi_daddr = seg_daddr + bytetoda(fsp, offset);
			b->bi_segcreate = s->ss_create;
			b->bi_bp = seg_buf + offset;
			
			(*count) ++;
			offset += blocksize(fsp, b->bi_lbn);
#ifdef VERBOSE
			printf("\tb[%d] = ", j);
			print_BLOCK_INFO(b);
			printf("\n");
			fflush(stdout);
#endif
		}
	}
	free (finfos);
}

void
pseg_inodes (fsp, seg, s, seg_buf, inodes, count)
	FS_INFO *fsp;		/* pointer to super block */
	int	seg;		/* the segment id */
	SEGSUM	*s;		/* (unvalidated) segsum pointer */
	caddr_t	seg_buf;	/* the buffer containing the segment's data */
				/* OUT: array of inode_info for live inodes */
	INODE_INFO	**inodes;
	int	*count;		/* OUT: number of active inodes in segment */
{
	int	i;
	ino_t	inum;
	daddr_t	*daddrp, i_daddr, seg_daddr;
	struct	dinode	*di;
	
	*count = 0;
	*inodes = NULL;

	if (s->ss_ninos <= 0) return;
	
	*inodes = (INODE_INFO *)malloc (s->ss_ninos * sizeof(INODE_INFO));

	seg_daddr = sntoda(lfsp, seg);

#ifdef VERBOSE
	printf("pseg_inodes:\n");
	print_SEGSUM(s);
	printf("\n");
	fflush(stdout);
#endif

	daddrp = (daddr_t *)((caddr_t)s + LFS_SUMMARY_SIZE);

	for (i = 0 ; i < s->ss_ninos ; ++i) {

		if (i % INOPB(lfsp) == 0) {
			i_daddr = *--daddrp;
			if (datosn(lfsp, i_daddr) != seg ||
			    datobyte(fsp, i_daddr - seg_daddr) > seg_size(lfsp)) {
				printf("pseg_inodes: bad i_daddr\n");
				print_SEGSUM(s);
				printf("\n");
				fflush(stdout);
				printf("i_daddr = %d, seg_daddr = %d, offset = %d, pseg_size = %d\n",
				    i_daddr, seg_daddr, i_daddr - seg_daddr, 
				    pseg_size(fsp, (SEGSUM*)s));
				fflush(stdout);
			}
			di = (struct dinode *)
				(seg_buf + datobyte(fsp, i_daddr - seg_daddr));
		} else 
			++di;
		
		inum = di->di_inum;

		if (IFILE_ENTRY(lfsp, ifilep, inum)->if_daddr == i_daddr) {
			(*inodes)[*count].ii_inode = inum;
			(*inodes)[*count].ii_daddr = i_daddr;
			(*inodes)[*count].ii_segcreate = s->ss_create;
			(*inodes)[*count].ii_dinode = di;
		
			(*count) ++;
		} 
	}
}

/* return the size of the partial segment in bytes. */
u_long
pseg_size (fsp, s)
	FS_INFO *fsp;   /* pointer to super block */
	SEGSUM	*s;	/* segsum pointer */
{
	int	i;
	int	j;
	FINFO	**finfos;
	int	finfoc;
	u_long	size = LFS_SUMMARY_SIZE;
	
	pseg_finfos (fsp, s, &finfos, &finfoc);
	for (i = 0 ; i < finfoc ; i ++) 
	for (j = 0 ; j < finfos[i]->fi_nblocks ; j ++) 
		size += blocksize(fsp, finfos[i]->fi_blocks[j]);

	/* inodes are packed INOPB inodes per block */
	/* there can be unused space in an inode block */
	size += datobyte(fsp, fsbtodb(lfsp,1)*((s->ss_ninos+INOPB(lfsp)-1)/INOPB(lfsp)));

	return size;
}

/* 
 * join block list b with list a (eliminating duplicates), leaving result
 * in list a.
 */
void
pseg_bjoin (fsp, ablocks, acount, bblocks, bcount)
	FS_INFO *fsp;   /* pointer to file system info */
				/* INOUT: array of live blocks block_info */
	BLOCK_INFO	**ablocks;
	int	*acount;	/* INOUT: number of active blocks */
				/* IN: array of live blocks block_info */
	BLOCK_INFO	*bblocks;
	int	bcount;	/* IN: number of active blocks */
{
	int	i;
	int	j;
	BLOCK_INFO	*abp;
	BLOCK_INFO	*bbp;

#ifdef VERBOSE
	printf("pseg_bjoin: *acount = %d, bcount = %d\n", *acount, bcount);
/**/
	printf("ablocks = \n");
	for(i=0;i<*acount;i++){print_BLOCK_INFO((*ablocks)+i); printf("\n");}
/**/
	printf("bblocks = \n");
	for(i=0;i<bcount;i++){print_BLOCK_INFO(bblocks+i); printf("\n");}
/**/
	fflush(stdout);
/**/
#endif

	for (i = 0, bbp = bblocks ; i < bcount ; ++i, ++bbp) {
		for (j = 0, abp = *ablocks ; j < *acount ; ++j, ++abp) {
			if (abp->bi_inode == bbp->bi_inode
				&& abp->bi_lbn == bbp->bi_lbn) {
				/* the data is for the same file and logical block */
				if (abp->bi_segcreate < bbp->bi_segcreate)
					*abp = *bbp;
				break;
			}
		}
		if (j == *acount) {
			/* this is a block we haven't seen before */
			*ablocks = (BLOCK_INFO*)
				realloc (*ablocks, sizeof(BLOCK_INFO)*(*acount + 1));
			(*ablocks)[*acount] = *bbp;
			(*acount) ++;
		}
	}
}

/* 
 * join block list b with list a (eliminating duplicates), leaving result
 * in list a.
 */
void
pseg_ijoin (fsp, ainodes, acount, binodes, bcount)
	FS_INFO *fsp;   /* pointer to file system info */
				/* INOUT: array of live inodes inode_info */
	INODE_INFO	**ainodes;
	int	*acount;	/* INOUT: number of active inodes */
				/* IN: array of live inodes inode_info */
	INODE_INFO	*binodes;
	int	bcount;		/* IN: number of active inodes */
{
	int	i;
	int	j;
	daddr_t	daddr;
	INODE_INFO	*aip;
	INODE_INFO	*bip;

	/* we assume that we have no duplicate live inodes on "a" and "b" */
	
	/* eliminate dead inodes from "a" */
	for (i = 0, aip = *ainodes ; i < *acount ; ++aip ) {
		daddr = IFILE_ENTRY(lfsp, ifilep, aip->ii_inode)->if_daddr;
		if (daddr != aip->ii_daddr) 
			*aip = (*ainodes)[--(*acount)];
		else 	i++;
	}

	/* eliminate dead inodes from "b" */
	for (i = 0, bip = binodes ; i < bcount ; ++bip) {
		daddr = IFILE_ENTRY(lfsp, ifilep, bip->ii_inode)->if_daddr;
		if (daddr != bip->ii_daddr) {
			/* don't really need to do this, only we don't want
			   to lose any inodes, just in case */
			INODE_INFO	tmp;
			tmp = *bip;
			*bip = binodes[bcount];
			binodes[bcount] = tmp;
			bcount --;
		}
		else	i++;
	}
	/* append "b" to "a" */
	if (bcount > 0) {
		*ainodes = (INODE_INFO *)realloc ((void *)*ainodes,
			(*acount + bcount + 1)*sizeof(INODE_INFO));
		for (i = 0 ; i < bcount ; i ++)
			(*ainodes)[(*acount)++] = binodes[i];
	}
}

/* is the segsum block valid? return TRUE if it is, FALSE otherwise */
int 
segsum_valid (fsp, ssp)
	FS_INFO *fsp;   /* pointer to file system info */
	SEGSUM	*ssp;	/* pointer to segment summary block */
{
	u_long	sumsum;

	/* check segsum block checksum */
	sumsum = cksum(&ssp->ss_datasum, 
	    LFS_SUMMARY_SIZE - sizeof(ssp->ss_sumsum));

	if (sumsum != ssp->ss_sumsum) return FALSE;
	
	return TRUE;
}

/*
 * pseg_valid:
 *
 * returns 1 if the partial segment is valid, and 0 if it is invalid.
 * it uses the checksums to verify validity.
 */	 
int
pseg_valid (fsp, ssp)
	FS_INFO *fsp;   /* pointer to file system info */
	SEGSUM	*ssp;	/* pointer to segment summary block */
{
	u_long	datasum;
	u_long	size;
	int	nblocks;
	int	i;
	u_long	*datap;
	caddr_t	p;

	/* check segsum block checksum */
	if (segsum_valid (fsp, ssp) == FALSE) return FALSE;

	return TRUE;
		
	/* check data/inode block(s) checksum too... */
	size = pseg_size (fsp, ssp);
	nblocks = size/fsbtodb(lfsp, 1);
	datap = (u_long*)malloc(sizeof(u_long)*nblocks);
	p = (caddr_t)ssp + LFS_SUMMARY_SIZE;
	for (i = 0 ; i < nblocks ; i ++) {
		datap[i] = *((u_long *)p);
		p += lfsp->lfs_bsize;
	}
	datasum = cksum ((void *)datap, nblocks*sizeof(u_long));
	if (datasum != ssp->ss_datasum) return FALSE;
	
	return TRUE;
}

/* get array of FINFO pointers for partial segment */
void
pseg_finfos (fsp, ssp, finfos, count)
	FS_INFO	*fsp;   /* pointer to file system info */
	SEGSUM	*ssp;	/* pointer to segment summary block */
	FINFO	***finfos;	/* OUT: return an array of FINFO pointers */
	int	*count;		/* OUT: return size of array */
{
	caddr_t	p = (caddr_t)ssp + sizeof(SEGSUM);
	int	i;
	FINFO	*fip;
	
	*count = 0;
	*finfos = NULL;

	if (ssp->ss_nfinfo > 0)
		*finfos = (FINFO**)malloc (ssp->ss_nfinfo*sizeof(FINFO*));

	for (i = 0 ; i < ssp->ss_nfinfo ; i ++) {
		fip = (FINFO *)p;
		(*finfos)[*count] = fip;
		(*count) ++;
		p += finfo_size (fip);
	}
}

/*
 * blocksize:
 *
 * returns the size (in bytes) of a (logical) block.
 * this is used because lfs uses different block sizes, depending
 * on the logical # of the block.  Lfs uses various sizes so
 * it doesn't need fragments.
 */ 
u_long
blocksize (fsp, index)
	FS_INFO *fsp;   /* pointer to file system info */
	int	index;	/* logical block # w/in file */
{
	return lfsp->lfs_bsize;	/* XXX: blocksize might depend on
					the logical block number */
}

/*
 * finfo_size
 *
 * returns the size in bytes of an FINFO structure 
 */
u_long
finfo_size (finfop)
	FINFO	*finfop;
{
	return sizeof(FINFO) + sizeof(long)*(finfop->fi_nblocks-1);
}
	

/* #define MMAP_SEGMENT */
/* 
 * read a segment into a memory buffer
 */
int
mmap_segment (fsp, segment, seg_buf)
	FS_INFO	*fsp;		/* file system information */
	int	segment;	/* the index of the segment to be cleaned */
	caddr_t	*seg_buf;	/* pointer to buffer area */
{
	off_t	seg_daddr;	/* base disk address of segment */
	int	fid;		/* fildes for file system device */
	char	mntfromname[MNAMELEN+2];

	/* get the disk address of the beginning of the segment */
	seg_daddr = sntoda(lfsp, segment);

	strcpy(mntfromname, "/dev/r");
	strcat(mntfromname, statfsp->f_mntfromname+5);

	if ((fid = open(mntfromname, O_RDONLY, (mode_t)0)) < 0) {
		perror("mmap_segment: bad open");
		return -1;
	}

#ifdef MMAP_SEGMENT
	*seg_buf = mmap ((caddr_t)0, seg_size(lfsp), PROT_READ,
		MAP_FILE, fid, (off_t)datobyte(fsp, seg_daddr));
	if ((long)*seg_buf < 0) {
		perror("mmap_segment: mmap failed");
		return NULL;
	}
#else /* MMAP_SEGMENT */
	printf("mmap_segment: seg_daddr = %lu, seg_size = %lu, seg_offset = %lu\n", 
		seg_daddr, seg_size(lfsp), datobyte(fsp, seg_daddr));
	/* malloc the space for the buffer */
	*seg_buf = (caddr_t)malloc(seg_size(lfsp));

	/* read the segment data into the buffer */
	if (datobyte(fsp, seg_daddr) != lseek (fid, datobyte(fsp, seg_daddr), SEEK_SET)) {
		perror ("mmap_segment: bad lseek");
		return -1;
	}
	
	if (seg_size(lfsp) != read (fid, *seg_buf, seg_size(lfsp))) {
		perror ("mmap_segment: bad read");
		return -1;
	}
#endif /* MMAP_SEGMENT */
	close (fid);

	return 0;
}

void
munmap_segment (fsp, seg_buf)
	FS_INFO	*fsp;		/* file system information */
	caddr_t	seg_buf;	/* pointer to buffer area */
{
#ifdef MMAP_SEGMENT
	munmap (seg_buf, seg_size(lfsp));
#else /* MMAP_SEGMENT */
	free (seg_buf);
#endif /* MMAP_SEGMENT */
}


/*
 * USEFUL DEBUGGING TOOLS:
 */

void
print_IFILE (p)
	IFILE	*p;
{
	if (p) {
		if (p->if_daddr == 0) 
			printf("{free, if_version=%lu, if_nextfree=%lu}",
				p->if_version, p->if_nextfree);
		else
			printf("{if_version=%lu, if_daddr=%lu}", 
				p->if_version, p->if_daddr);
	}
	else printf("0x0");
	fflush(stdout);
}

void
print_SEGUSE (p)
	SEGUSE	*p;
{
	if (p) {
		printf("{su_nbytes=%lu, su_flags=%c%c%c, su_lastmod=",
			p->su_nbytes, 
			((p->su_flags & SEGUSE_DIRTY) ? 'D' : 'C'),
			((p->su_flags & SEGUSE_ACTIVE) ? 'A' : ' '),
			((p->su_flags & SEGUSE_SUPERBLOCK) ? 'S' : ' '));
			print_time_t(p->su_lastmod);
			printf("}");
	}
	else 
		printf("0x0");
	fflush(stdout);
}

void
print_CLEANERINFO (p)
	CLEANERINFO	*p;
{
	if (p) printf("{clean=%lu, dirty=%lu}", p->clean, p->dirty);
	else printf("0x0");
	fflush(stdout);
}

void
print_SEGSUM (p)
	SEGSUM	*p;
{
	if (p) {
		printf("{ss_sumsum=%lu, ss_datasum=%lu, ss_next=%lu, ",
			p->ss_sumsum, p->ss_datasum, p->ss_next);
		printf("ss_create=%lu, ss_nfinfo=%lu, ss_ninos=%lu",
			p->ss_create, p->ss_nfinfo, p->ss_ninos);
		printf("}");
	}
	else printf("0x0");
	fflush(stdout);
}

void
print_time_t (t)
	time_t	t;
{
	char temp[128];
	int len;

	strcpy (temp, ctime(&t));
	len = strlen(temp);
	if (temp[len-1] == '\n') temp[len-1] = 0;
	printf("%s", temp);
	fflush(stdout);
}

void
print_FINFO (p)
	FINFO	*p;
{
	int i;

	if (p) {
		printf("{fi_nblocks=%lu, fi_version=%lu, fi_ino=%lu, fi_blocks={",
			p->fi_nblocks, p->fi_version, p->fi_ino);
		for (i = 0 ; i < p->fi_nblocks ; i ++) {
			if (i > 0) printf(", ");
			printf("%ld", p->fi_blocks[i]);
		}
		printf("}}");
	} else printf("0x0");
	fflush(stdout);
}

void
print_BLOCK_INFO (p)
	BLOCK_INFO	*p;
{
	if (p) {
		printf("{bi_inode=%lu, bi_lbn=%ld, bi_daddr=%lu, bi_segcreate=",
			p->bi_inode, p->bi_lbn, p->bi_daddr);
		print_time_t(p->bi_segcreate);
		printf(", bi_bp = 0x%x}", p->bi_bp);
	}
	else
		printf("0x0");
	fflush(stdout);
}

void
print_INODE_INFO (p)
	INODE_INFO	*p;
{
	if (p) {
		printf("{ii_inode=%lu, ii_daddr=%lu, ii_segcreate=",
			p->ii_inode, p->ii_daddr);
		print_time_t (p->ii_segcreate);
		printf(", ii_dinode=0x%x}", p->ii_dinode);
	}
	else
		printf("0x0");
	fflush(stdout);
}

void
print_lfs (p)
	struct	lfs	*p;
{
	int	i;
	
	if (p) {
		printf("{\n");
		printf("\tlfs_magic=0x%x\n", p->lfs_magic);
		printf("\tlfs_version=%lu\n", p->lfs_version);
		printf("\tlfs_size=%lu\n", p->lfs_size);
		printf("\tlfs_ssize=%lu\n", p->lfs_ssize);
		printf("\tlfs_dsize=%lu\n", p->lfs_dsize);
		printf("\tlfs_bsize=%lu\n", p->lfs_bsize);
		printf("\tlfs_fsize=%lu\n", p->lfs_fsize);
		printf("\tlfs_frag=%lu\n", p->lfs_frag);
		/* checkpoint region */
		printf("\tlfs_free=%lu\n", p->lfs_free);
		printf("\tlfs_bfree=%lu\n", p->lfs_bfree);
		printf("\tlfs_nfiles=%lu\n", p->lfs_nfiles);
		printf("\tlfs_idaddr=%lu\n", p->lfs_idaddr);
		printf("\tlfs_ifile=%lu\n", p->lfs_ifile);
		printf("\tlfs_lastseg=%lu\n", p->lfs_lastseg);
		printf("\tlfs_nextseg=%lu\n", p->lfs_nextseg);
		printf("\tlfs_curseg=%lu\n", p->lfs_curseg);
		printf("\tlfs_offset=%lu\n", p->lfs_offset);
		printf("\tlfs_tstamp=%lu\n", p->lfs_tstamp);
		/* configuration parameters */
		printf("\tlfs_minfree=%lu\n", p->lfs_minfree);
		/* these fields can be computed from the others */
		printf("\tlfs_dbpseg=%lu\n", p->lfs_dbpseg);
		printf("\tlfs_inopb=%lu\n", p->lfs_inopb);
		printf("\tlfs_ifpb=%lu\n", p->lfs_ifpb);
		printf("\tlfs_sepb=%lu\n", p->lfs_sepb);
		printf("\tlfs_nindir=%lu\n", p->lfs_nindir);
		printf("\tlfs_nseg=%lu\n", p->lfs_nseg);
		printf("\tlfs_nspf=%lu\n", p->lfs_nspf);
		printf("\tlfs_cleansz=%lu\n", p->lfs_cleansz);
		printf("\tlfs_segtabsz=%lu\n", p->lfs_segtabsz);

		printf("\tlfs_segmask=%lu\n", p->lfs_segmask);
		printf("\tlfs_segshift=%lu\n", p->lfs_segshift);
		printf("\tlfs_bmask=%lu\n", p->lfs_bmask);
		printf("\tlfs_bshift=%lu\n", p->lfs_bshift);
		printf("\tlfs_ffmask=%lu\n", p->lfs_ffmask);
		printf("\tlfs_ffshift=%lu\n", p->lfs_ffshift);
		printf("\tlfs_fbmask=%lu\n", p->lfs_fbmask);
		printf("\tlfs_fbshift=%lu\n", p->lfs_fbshift);
		printf("\tlfs_fsbtodb=%lu\n", p->lfs_fsbtodb);
		/* superblock offsets */
		printf("\tlfs_sboffs={");
		for (i = 0 ; i < LFS_MAXNUMSB ; i ++) {
			if (i > 0) printf(", ");
			printf("%lu", p->lfs_sboffs[i]);
		}
		printf("}\n");

		printf("}");
	}
	else
		printf("0x0");
	fflush(stdout);
}
