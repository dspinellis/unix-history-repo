/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dumplfs.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/file.h>
#include <ufs/dinode.h>
#include "lfs.h"				/* XXX fix this */
#include <time.h>
#include <fstab.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "extern.h"

static void	addseg __P((char *));
static void	dump_dinode __P((struct dinode *));
static void	dump_ifile __P((int, LFS *, int));
static int	dump_ipage_ifile __P((int, IFILE *, int));
static int	dump_ipage_segusage __P((LFS *, int, IFILE *, int));
static daddr_t	dump_segment __P((int, int, daddr_t, LFS *, int));
static int	dump_sum __P((SEGSUM *, int, daddr_t));
static void	dump_super __P((LFS *));
static void	usage __P((void));

typedef struct seglist SEGLIST;
struct seglist {
        SEGLIST *next;
	int num;
};
SEGLIST	*seglist;

int daddr_shift;
char *special;

/* Segment Usage formats */
#define print_suheader \
	(void)printf("segnum\tstatus\tnbytes\t\tlastmod\n")
#define print_suentry(i, sp) \
	(void)printf("%d\t%s\t%d\t%s", \
	    i, (((sp)->su_flags) ? "DIRTY" : "CLEAN"), \
	    (sp)->su_nbytes, ctime((time_t *)&(sp)->su_lastmod))

/* Ifile formats */
#define print_iheader \
	(void)printf("inum\tstatus\tversion\tdaddr\t\tatime\tfreeptr\n")
#define print_ientry(i, ip) \
	if (ip->if_daddr == LFS_UNUSED_DADDR) \
		(void)printf("%d\tFREE\t%d\t \t\t \t%d\n", \
		    i, ip->if_version, ip->if_nextfree); \
	else \
		(void)printf("%d\tINUSE\t%d\t%8X    \t%s\n", \
		    i, ip->if_version, ip->if_daddr, \
		    ctime((time_t *)&ip->if_st_atime))
int
main (argc, argv)
	int argc;
	char *argv[];
{
	LFS lfs_sb1, lfs_sb2, *lfs_master;
	daddr_t seg_addr;
	int ch, do_allsb, do_ientries, fd, segnum;

	do_allsb = 0;
	do_ientries = 0;
	while ((ch = getopt(argc, argv, "ais:")) != EOF)
		switch(ch) {
		case 'a':		/* Dump all superblocks */
			do_allsb = 1;
			break;
		case 'i':		/* Dump ifile entries */
			do_ientries = 1;
			break;
		case 's':		/* Dump out these segments */
			addseg(optarg);
			break;
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1)
		usage();

	special = argv[0];
	if ((fd = open(special, O_RDONLY, 0)) < 0)
		err("%s: %s", special, strerror(errno));

	/* Read the first superblock */
	get(fd, LFS_LABELPAD, &lfs_sb1, sizeof(LFS));
	daddr_shift = lfs_sb1.lfs_bshift - lfs_sb1.lfs_fsbtodb;

	/*
	 * Read the second superblock and figure out which check point is
	 * most up to date.
	 */
	get(fd, lfs_sb1.lfs_sboffs[1] << daddr_shift, &lfs_sb2, sizeof(LFS));

	lfs_master = &lfs_sb1;
	if (lfs_sb1.lfs_tstamp < lfs_sb2.lfs_tstamp)
		lfs_master = &lfs_sb2;

	(void)printf("Master Superblock:\n");
	dump_super(lfs_master);

	dump_ifile(fd, lfs_master, do_ientries);

	if (seglist != NULL)
		for (; seglist != NULL; seglist = seglist->next) {
			seg_addr = lfs_master->lfs_sboffs[0] + seglist->num *
			    (lfs_master->lfs_ssize << lfs_master->lfs_fsbtodb);
			dump_segment(fd,
			    seglist->num, seg_addr, lfs_master, do_allsb);
		}
	else
		for (segnum = 0, seg_addr = lfs_master->lfs_sboffs[0];
		    segnum < lfs_master->lfs_nseg; segnum++, seg_addr +=
		    lfs_master->lfs_ssize << lfs_master->lfs_fsbtodb)
			dump_segment(fd,
			    segnum, seg_addr, lfs_master, do_allsb);

	(void)close(fd);
	exit(0);
}

/*
 * We are reading all the blocks of an inode and dumping out the ifile table.
 * This code can be made tigher, but this is a first pass at getting the stuff
 * printed out rather than making this code incredibly efficient.
 */
static void
dump_ifile(fd, lfsp, do_ientries)
	int fd;
	LFS *lfsp;
	int do_ientries;
{
	IFILE *ipage;
	struct dinode *dip, *dpage;
	daddr_t addr, *addrp, *iaddrp;
	daddr_t	*dindir;
	daddr_t	*indir;
	int block_limit, nblocks;
	int i, j, inum, nsupb, psize;

	psize = lfsp->lfs_bsize;
	addr = lfsp->lfs_idaddr;
	nsupb = lfsp->lfs_bsize / sizeof(SEGUSE);

	if (!(dip = dpage = malloc(psize)))
		err("%s", strerror(errno));
	get(fd, addr << daddr_shift, dip, psize);

	for (i = 0; i < lfsp->lfs_inopb; i++, dip++)
		if (dip->di_inum == LFS_IFILE_INUM)
			break;

	if (i >= lfsp->lfs_inopb)
		err("unable to locate ifile inode");

	(void)printf("\nIFILE inode\n");
	dump_dinode(dip);

	(void)printf("\nIFILE contents\n");
	print_suheader;
	nblocks = dip->di_size >> lfsp->lfs_bshift;
	block_limit = MIN(nblocks, NDADDR);

	/* Get the direct block */
	if ((ipage = malloc(psize)) == NULL)
		err("%s", strerror(errno));
	for (inum = 0, addrp = dip->di_db, i = 0; i < block_limit;
	    i++, addrp++) {
		get(fd, *addrp << daddr_shift, ipage, psize);

		if (i < lfsp->lfs_segtabsz) {
			inum = dump_ipage_segusage(lfsp, inum, ipage, nsupb);
			if (!inum)
				if(!do_ientries)
					goto e0;
				else
					print_iheader;
		} else
			inum = dump_ipage_ifile(inum, ipage, lfsp->lfs_ifpb);
	}

	if (nblocks <= NDADDR)
		goto e0;

	/* Dump out blocks off of single indirect block */
	if (!(indir = malloc(psize)))
		err("%s", strerror(errno));
	get(fd, dip->di_ib[0] << daddr_shift, indir, psize);
	block_limit = MIN(i + lfsp->lfs_nindir, nblocks);
	for (addrp = indir; i < block_limit; i++, addrp++) {
		if (*addrp == LFS_UNUSED_DADDR)
			break;
		get(fd, *addrp << daddr_shift,ipage, psize);
		if (i < lfsp->lfs_segtabsz) {
			inum = dump_ipage_segusage(lfsp, inum, ipage, nsupb);
			if (!inum)
				if(!do_ientries)
					goto e1;
				else
					print_iheader;
		} else
			inum = dump_ipage_ifile(inum, ipage, lfsp->lfs_ifpb);
	}

	if (nblocks <= lfsp->lfs_nindir * lfsp->lfs_ifpb)
		goto e1;

	/* Get the double indirect block */
	if (!(dindir = malloc(psize)))
		err("%s", strerror(errno));
	get(fd, dip->di_ib[1] << daddr_shift, dindir, psize);
	for (iaddrp = dindir, j = 0; j < lfsp->lfs_nindir; j++, iaddrp++) {
		if (*iaddrp == LFS_UNUSED_DADDR)
			break;
		get(fd, *iaddrp << daddr_shift, indir, psize);
		block_limit = MIN(i + lfsp->lfs_nindir, nblocks);
		for (addrp = indir; i < block_limit; i++, addrp++) {
			if (*addrp == LFS_UNUSED_DADDR)
				break;
			get(fd, *addrp << daddr_shift, ipage, psize);
			if (i < lfsp->lfs_segtabsz) {
				inum = dump_ipage_segusage(lfsp,
				    inum, ipage, nsupb);
				if (!inum)
					if(!do_ientries)
						goto e2;
					else
						print_iheader;
			} else
				inum = dump_ipage_ifile(inum,
				    ipage, lfsp->lfs_ifpb);
		}
	}
e2:	free(dindir);
e1:	free(indir);
e0:	free(dpage);
	free(ipage);
}

static int
dump_ipage_ifile(i, pp, tot)
	int i;
	IFILE *pp;
	int tot;
{
	IFILE *ip;
	int cnt, max;

	max = i + tot;

	for (ip = pp, cnt = i; cnt < max; cnt++, ip++)
		print_ientry(cnt, ip);
	return (max);
}

static int
dump_ipage_segusage(lfsp, i, pp, tot)
	LFS *lfsp;
	int i;
	IFILE *pp;
	int tot;
{
	SEGUSE *sp;
	int cnt, max;

	max = i + tot;
	for (sp = (SEGUSE *)pp, cnt = i;
	     cnt < lfsp->lfs_nseg && cnt < max; cnt++, sp++)
		print_suentry(cnt, sp);
	if (max >= lfsp->lfs_nseg)
		return (0);
	else
		return (max);
}

static void
dump_dinode(dip)
	struct dinode *dip;
{
	int i;

	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\t%s%d\n",
		"mode  ", dip->di_mode,
		"nlink ", dip->di_nlink,
		"uid   ", dip->di_uid,
		"gid   ", dip->di_gid,
		"size  ", dip->di_size);
	(void)printf("%s%s%s%s%s%s",
		"atime ", ctime(&dip->di_atime),
		"mtime ", ctime(&dip->di_mtime),
		"ctime ", ctime(&dip->di_ctime));
	(void)printf("inum  %d\n", dip->di_inum);
	(void)printf("Direct Addresses\n");
	for (i = 0; i < NDADDR; i++) {
		(void)printf("\t%X", dip->di_db[i]);
		if ((i % 6) == 5)
			(void)printf("\n");
	}
	for (i = 0; i < NIADDR; i++)
		(void)printf("\t%X", dip->di_ib[i]);
	(void)printf("\n");
}

static int
dump_sum(sp, segnum, addr)
	SEGSUM *sp;
	int segnum;
	daddr_t addr;
{
	FINFO *fp;
	long *dp;
	int i, j, sb;
	int ck;

	if (sp->ss_cksum != (ck = cksum(&sp->ss_next, 
	    LFS_SUMMARY_SIZE - sizeof(sp->ss_cksum))))
		(void)printf("dumplfs: %s %d address %lx\n",
		    "corrupt summary block; segment", segnum, addr);

	(void)printf("Segment Summary Info\n");
	(void)printf("\t%s%X   \t%s%X   \t%s%X   \n",
		"next     ", sp->ss_next,
		"prev     ", sp->ss_prev,
		"nextsum  ", sp->ss_nextsum );
	(void)printf("\t%s%d\t%s%d\t%s%X\n",
		"nfinfo   ", sp->ss_nfinfo,
		"ninos    ", sp->ss_ninos,
		"cksum    ", sp->ss_cksum );
	(void)printf("\tcreate   %s", ctime((time_t *)&sp->ss_create));

	sb = 0;
	for (fp = (FINFO *)(sp + 1), i = 0; i < sp->ss_nfinfo; i++) {
		if (fp->fi_ino == LFS_UNUSED_INUM)
			sb = 1;
		(void)printf("File Info for file: %d version %d nblocks %d\n",
		    fp->fi_ino, fp->fi_version, fp->fi_nblocks);
		dp = &(fp->fi_blocks[0]);
		for (j = 0; j < fp->fi_nblocks; j++, dp++) {
			(void)printf("\t%d", *dp);
			if ((j % 8) == 7)
				(void)printf("\n");
		}
		if ((j % 8) != 0)
			(void)printf("\n");
		fp = (FINFO *)dp;
	}
	return (sb);
}

static daddr_t
dump_segment(fd, segnum, addr, lfsp, dump_sb)
	int fd, segnum;
	daddr_t addr;
	LFS *lfsp;
	int dump_sb;
{
	LFS lfs_sb;
	SEGSUM *sump;
	char sumblock[LFS_SUMMARY_SIZE];
	int sum_offset;
	int sb;

	(void)printf("\nSegment Number %d (Disk Address %X)\n",
	    addr >> (lfsp->lfs_segshift - daddr_shift), addr);
	sum_offset = (addr << (lfsp->lfs_bshift - lfsp->lfs_fsbtodb)) +
	    (lfsp->lfs_ssize << lfsp->lfs_bshift) - LFS_SUMMARY_SIZE;

	sb = 0;
	do {
		get(fd, sum_offset, sumblock, LFS_SUMMARY_SIZE);
		sump = (SEGSUM *)sumblock;
		sb = sb || dump_sum(sump, segnum, addr);
		sum_offset = (sump->ss_nextsum == -1 ? 0 :
		    sump->ss_nextsum << daddr_shift);
	} while (sum_offset);
	if (dump_sb && sb)  {
		get(fd, addr << daddr_shift, &lfs_sb, sizeof(LFS));
		dump_super(&lfs_sb);
	}
	return (sump->ss_prev);
}

static void
dump_super(lfsp)
	LFS *lfsp;
{
	int i;

	(void)printf("%s%X\t%s%X\t%s%d\t%s%d\n",
		"magic    ", lfsp->lfs_magic,
		"version  ", lfsp->lfs_version,
		"size     ", lfsp->lfs_size,
		"ssize    ", lfsp->lfs_ssize);
	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\n",
		"dsize    ", lfsp->lfs_dsize,
		"bsize    ", lfsp->lfs_bsize,
		"fsize    ", lfsp->lfs_fsize,
		"frag     ", lfsp->lfs_frag);

	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\n",
		"minfree  ", lfsp->lfs_minfree,
		"inopb    ", lfsp->lfs_inopb,
		"ifpb     ", lfsp->lfs_ifpb,
		"nindir   ", lfsp->lfs_nindir);

	(void)printf("%s%d\t%s%d\t%s%d\n",
		"nseg     ", lfsp->lfs_nseg,
		"nspf     ", lfsp->lfs_nspf,
		"segtabsz ", lfsp->lfs_segtabsz);

	(void)printf("%s%X\t%s%d\t%s%X\t%s%d\n",
		"segmask  ", lfsp->lfs_segmask,
		"segshift ", lfsp->lfs_segshift,
		"bmask    ", lfsp->lfs_bmask,
		"bshift   ", lfsp->lfs_bshift);

	(void)printf("%s%X\t%s%d\t%s%X\t%s%d\n",
		"ffmask   ", lfsp->lfs_ffmask,
		"ffshift  ", lfsp->lfs_ffshift,
		"fbmask   ", lfsp->lfs_fbmask,
		"fbshift  ", lfsp->lfs_fbshift);

	(void)printf("%s%d\t%s%X\n",
		"fsbtodb  ", lfsp->lfs_fsbtodb,
		"cksum    ", lfsp->lfs_cksum);

	(void)printf("Superblock disk addresses:");
	for (i = 0; i < LFS_MAXNUMSB; i++)
		(void)printf(" %X", lfsp->lfs_sboffs[i]);
	(void)printf("\n");

	(void)printf("Checkpoint Info\n");
	(void)printf("%s%d\t%s%X\t%s%d\n",
		"free     ", lfsp->lfs_free,
		"idaddr   ", lfsp->lfs_idaddr,
		"ifile    ", lfsp->lfs_ifile);
	(void)printf("%s%X\t%s%d\t%s%X\t%s%X\n",
		"bfree    ", lfsp->lfs_bfree,
		"nfiles   ", lfsp->lfs_nfiles,
		"lastseg  ", lfsp->lfs_lastseg,
		"nextseg  ", lfsp->lfs_nextseg);
	(void)printf("tstamp   %s", ctime((time_t *)&lfsp->lfs_tstamp));
}

static void
addseg(arg)
	char *arg;
{
	SEGLIST *p;

	if ((p = malloc(sizeof(SEGLIST))) == NULL)
		err("%s", strerror(errno));
	p->next = seglist;
	p->num = atoi(arg);
	seglist = p;
}

static void
usage()
{
	(void)fprintf(stderr, "usage: dumplfs [-ai] [-s segnum] file\n");
	exit(1);
}
