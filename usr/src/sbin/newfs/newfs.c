/*
 * Copyright (c) 1983, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)newfs.c	6.22.1.1 (Berkeley) 12/18/90";
#endif /* not lint */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

/*
 * newfs: friendly front end to mkfs
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <ufs/fs.h>
#include <ufs/dir.h>
#include <sys/ioctl.h>
#include <sys/disklabel.h>
#include <sys/file.h>
#include <sys/mount.h>

#include <stdio.h>
#include <ctype.h>
#include <paths.h>

#define COMPAT			/* allow non-labeled disks */

/*
 * The following two constants set the default block and fragment sizes.
 * Both constants must be a power of 2 and meet the following constraints:
 *	MINBSIZE <= DESBLKSIZE <= MAXBSIZE
 *	sectorsize <= DESFRAGSIZE <= DESBLKSIZE
 *	DESBLKSIZE / DESFRAGSIZE <= 8
 */
#define	DFL_FRAGSIZE	1024
#define	DFL_BLKSIZE	8192

/*
 * Cylinder groups may have up to many cylinders. The actual
 * number used depends upon how much information can be stored
 * on a single cylinder. The default is to use 16 cylinders
 * per group.
 */
#define	DESCPG		16	/* desired fs_cpg */

/*
 * MINFREE gives the minimum acceptable percentage of file system
 * blocks which may be free. If the freelist drops below this level
 * only the superuser may continue to allocate blocks. This may
 * be set to 0 if no reserve of free blocks is deemed necessary,
 * however throughput drops by fifty percent if the file system
 * is run at between 90% and 100% full; thus the default value of
 * fs_minfree is 10%. With 10% free space, fragmentation is not a
 * problem, so we choose to optimize for time.
 */
#define MINFREE		10
#define DEFAULTOPT	FS_OPTTIME

/*
 * ROTDELAY gives the minimum number of milliseconds to initiate
 * another disk transfer on the same cylinder. It is used in
 * determining the rotationally optimal layout for disk blocks
 * within a file; the default of fs_rotdelay is 4ms.
 */
#define ROTDELAY	4

/*
 * MAXCONTIG sets the default for the maximum number of blocks
 * that may be allocated sequentially. Since UNIX drivers are
 * not capable of scheduling multi-block transfers, this defaults
 * to 1 (ie no contiguous blocks are allocated).
 */
#define MAXCONTIG	1

/*
 * MAXBLKPG determines the maximum number of data blocks which are
 * placed in a single cylinder group. The default is one indirect
 * block worth of data blocks.
 */
#define MAXBLKPG(bsize)	((bsize) / sizeof(daddr_t))

/*
 * Each file system has a number of inodes statically allocated.
 * We allocate one inode slot per NFPI fragments, expecting this
 * to be far more than we will ever need.
 */
#define	NFPI		4

/*
 * For each cylinder we keep track of the availability of blocks at different
 * rotational positions, so that we can lay out the data to be picked
 * up with minimum rotational latency.  NRPOS is the default number of
 * rotational positions that we distinguish.  With NRPOS of 8 the resolution
 * of our summary information is 2ms for a typical 3600 rpm drive.
 */
#define	NRPOS		8	/* number distinct rotational positions */


int	mfs;			/* run as the memory based filesystem */
int	Nflag;			/* run without writing file system */
int	fssize;			/* file system size */
int	ntracks;		/* # tracks/cylinder */
int	nsectors;		/* # sectors/track */
int	nphyssectors;		/* # sectors/track including spares */
int	secpercyl;		/* sectors per cylinder */
int	trackspares = -1;	/* spare sectors per track */
int	cylspares = -1;		/* spare sectors per cylinder */
int	sectorsize;		/* bytes/sector */
#ifdef tahoe
int	realsectorsize;		/* bytes/sector in hardware */
#endif
int	rpm;			/* revolutions/minute of drive */
int	interleave;		/* hardware sector interleave */
int	trackskew = -1;		/* sector 0 skew, per track */
int	headswitch;		/* head switch time, usec */
int	trackseek;		/* track-to-track seek, usec */
int	fsize = 0;		/* fragment size */
int	bsize = 0;		/* block size */
int	cpg = DESCPG;		/* cylinders/cylinder group */
int	cpgflg;			/* cylinders/cylinder group flag was given */
int	minfree = MINFREE;	/* free space threshold */
int	opt = DEFAULTOPT;	/* optimization preference (space or time) */
int	density;		/* number of bytes per inode */
int	maxcontig = MAXCONTIG;	/* max contiguous blocks to allocate */
int	rotdelay = ROTDELAY;	/* rotational delay between blocks */
int	maxbpg;			/* maximum blocks per file in a cyl group */
int	nrpos = NRPOS;		/* # of distinguished rotational positions */
int	bbsize = BBSIZE;	/* boot block size */
int	sbsize = SBSIZE;	/* superblock size */
int	mntflags;		/* flags to be passed to mount */
u_long	memleft;		/* virtual memory available */
caddr_t	membase;		/* start address of memory based filesystem */
#ifdef COMPAT
char	*disktype;
int	unlabelled;
#endif

char	device[MAXPATHLEN];
char	*progname;

extern	int errno;
char	*index();
char	*rindex();

main(argc, argv)
	int argc;
	char *argv[];
{
	char *cp, *special, *rindex();
	register struct partition *pp;
	register struct disklabel *lp;
	struct disklabel *getdisklabel();
	struct partition oldpartition;
	struct mfs_args args;
	struct stat st;
	int fsi, fso;
	register int i;
	int status;
	char buf[BUFSIZ];

	if ((progname = rindex(*argv, '/') + 1) == (char *)1)
		progname = *argv;
	if (!strcmp(progname, "mfs")) {
		Nflag++;
		mfs++;
	}
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++)
			switch (*cp) {

			case 'F':
				if (!mfs)
					fatal("-F: unknown flag");
				if (argc < 1)
					fatal("-F: mount flags");
				argc--, argv++;
				mntflags = atoi(*argv);
				if (mntflags == 0)
					fatal("%s: bad mount flags", *argv);
				goto next;

			case 'N':
				Nflag++;
				break;

			case 'S':
				if (argc < 1)
					fatal("-S: missing sector size");
				argc--, argv++;
				sectorsize = atoi(*argv);
				if (sectorsize <= 0)
					fatal("%s: bad sector size", *argv);
				goto next;

#ifdef COMPAT
			case 'T':
				if (argc < 1)
					fatal("-T: missing disk type");
				argc--, argv++;
				disktype = *argv;
				goto next;
#endif

			case 'a':
				if (argc < 1)
					fatal("-a: missing max contiguous blocks\n");
				argc--, argv++;
				maxcontig = atoi(*argv);
				if (maxcontig <= 0)
					fatal("%s: bad max contiguous blocks\n",
						*argv);
				goto next;

			case 'b':
				if (argc < 1)
					fatal("-b: missing block size");
				argc--, argv++;
				bsize = atoi(*argv);
				if (bsize < MINBSIZE)
					fatal("%s: bad block size", *argv);
				goto next;

			case 'c':
				if (argc < 1)
					fatal("-c: missing cylinders/group");
				argc--, argv++;
				cpg = atoi(*argv);
				if (cpg <= 0)
					fatal("%s: bad cylinders/group", *argv);
				cpgflg++;
				goto next;

			case 'd':
				if (argc < 1)
					fatal("-d: missing rotational delay\n");
				argc--, argv++;
				rotdelay = atoi(*argv);
				if (rotdelay < 0)
					fatal("%s: bad rotational delay\n",
						*argv);
				goto next;

			case 'e':
				if (argc < 1)
					fatal("-e: missing blocks pre file in a cyl group\n");
				argc--, argv++;
				maxbpg = atoi(*argv);
				if (maxbpg <= 0)
					fatal("%s: bad blocks per file in a cyl group\n",
						*argv);
				goto next;

			case 'f':
				if (argc < 1)
					fatal("-f: missing frag size");
				argc--, argv++;
				fsize = atoi(*argv);
				if (fsize <= 0)
					fatal("%s: bad frag size", *argv);
				goto next;

			case 'i':
				if (argc < 1)
					fatal("-i: missing bytes per inode\n");
				argc--, argv++;
				density = atoi(*argv);
				if (density <= 0)
					fatal("%s: bad bytes per inode\n",
						*argv);
				goto next;

			case 'k':
				if (argc < 1)
					fatal("-k: track skew");
				argc--, argv++;
				trackskew = atoi(*argv);
				if (trackskew < 0)
					fatal("%s: bad track skew", *argv);
				goto next;

			case 'l':
				if (argc < 1)
					fatal("-l: interleave");
				argc--, argv++;
				interleave = atoi(*argv);
				if (interleave <= 0)
					fatal("%s: bad interleave", *argv);
				goto next;

			case 'm':
				if (argc < 1)
					fatal("-m: missing free space %%\n");
				argc--, argv++;
				minfree = atoi(*argv);
				if (minfree < 0 || minfree > 99)
					fatal("%s: bad free space %%\n",
						*argv);
				goto next;

			case 'n':
				if (argc < 1)
					fatal("-n: missing rotational layout count\n");
				argc--, argv++;
				nrpos = atoi(*argv);
				if (nrpos <= 0)
					fatal("%s: bad rotational layout count\n",
						*argv);
				goto next;

			case 'o':
				if (argc < 1)
					fatal("-o: missing optimization preference");
				argc--, argv++;
				if (strcmp(*argv, "space") == 0)
					opt = FS_OPTSPACE;
				else if (strcmp(*argv, "time") == 0)
					opt = FS_OPTTIME;
				else
					fatal("%s: bad optimization preference %s",
					    *argv,
					    "(options are `space' or `time')");
				goto next;

			case 'p':
				if (argc < 1)
					fatal("-p: spare sectors per track");
				argc--, argv++;
				trackspares = atoi(*argv);
				if (trackspares < 0)
					fatal("%s: bad spare sectors per track", *argv);
				goto next;

			case 'r':
				if (argc < 1)
					fatal("-r: missing revs/minute\n");
				argc--, argv++;
				rpm = atoi(*argv);
				if (rpm <= 0)
					fatal("%s: bad revs/minute\n", *argv);
				goto next;

			case 's':
				if (argc < 1)
					fatal("-s: missing file system size");
				argc--, argv++;
				fssize = atoi(*argv);
				if (fssize <= 0)
					fatal("%s: bad file system size",
						*argv);
				goto next;

			case 't':
				if (argc < 1)
					fatal("-t: missing track total");
				argc--, argv++;
				ntracks = atoi(*argv);
				if (ntracks <= 0)
					fatal("%s: bad total tracks", *argv);
				goto next;

			case 'u':
				if (argc < 1)
					fatal("-u: missing sectors/track");
				argc--, argv++;
				nsectors = atoi(*argv);
				if (nsectors <= 0)
					fatal("%s: bad sectors/track", *argv);
				goto next;

			case 'x':
				if (argc < 1)
					fatal("-x: spare sectors per cylinder");
				argc--, argv++;
				cylspares = atoi(*argv);
				if (cylspares < 0)
					fatal("%s: bad spare sectors per cylinder", *argv);
				goto next;

			default:
				fatal("-%c: unknown flag", *cp);
			}
next:
		argc--, argv++;
	}
	if (argc < 1) {
		if (mfs)
			fprintf(stderr,
			    "usage: mfs [ fsoptions ] special-device %s\n",
			    "mount-point");
		else
#ifdef COMPAT
			fprintf(stderr, "usage: %s\n",
			    "newfs [ fsoptions ] special-device [device-type]");
#else
			fprintf(stderr,
			    "usage: newfs [ fsoptions ] special-device\n");
#endif
		fprintf(stderr, "where fsoptions are:\n");
		fprintf(stderr, "\t-N do not create file system, %s\n",
			"just print out parameters");
#ifdef COMPAT
		fprintf(stderr, "\t-T disktype\n");
#endif
		fprintf(stderr, "\t-b block size\n");
		fprintf(stderr, "\t-f frag size\n");
		fprintf(stderr, "\t-m minimum free space %%\n");
		fprintf(stderr, "\t-o optimization preference %s\n",
			"(`space' or `time')");
		fprintf(stderr, "\t-a maximum contiguous blocks\n");
		fprintf(stderr, "\t-d rotational delay between %s\n",
			"contiguous blocks");
		fprintf(stderr, "\t-e maximum blocks per file in a %s\n",
			"cylinder group");
		fprintf(stderr, "\t-i number of bytes per inode\n");
		fprintf(stderr, "\t-c cylinders/group\n");
		fprintf(stderr, "\t-n number of distinguished %s\n",
			"rotational positions");
		fprintf(stderr, "\t-s file system size (sectors)\n");
		fprintf(stderr, "\t-r revolutions/minute\n");
		fprintf(stderr, "\t-S sector size\n");
		fprintf(stderr, "\t-u sectors/track\n");
		fprintf(stderr, "\t-t tracks/cylinder\n");
		fprintf(stderr, "\t-p spare sectors per track\n");
		fprintf(stderr, "\t-x spare sectors per cylinder\n");
		fprintf(stderr, "\t-l hardware sector interleave\n");
		fprintf(stderr, "\t-k sector 0 skew, per track\n");
		exit(1);
	}
	special = argv[0];
	cp = rindex(special, '/');
	if (cp != 0)
		special = cp + 1;
	if (*special == 'r'
#if defined(vax) || defined(tahoe)
	    && special[1] != 'a' && special[1] != 'b'
#endif
#if defined(hp300)
	    && special[1] != 'd'
#endif
	   )
		special++;
	(void)sprintf(device, "%s/r%s", _PATH_DEV, special);
	special = device;
	if (!Nflag) {
		fso = open(special, O_WRONLY);
		if (fso < 0) {
			perror(special);
			exit(2);
		}
	} else
		fso = -1;
	fsi = open(special, O_RDONLY);
	if (fsi < 0) {
		perror(special);
		exit(3);
	}
	if (fstat(fsi, &st) < 0) {
		fprintf(stderr, "%s: ", progname); perror(special);
		exit(4);
	}
	if ((st.st_mode & S_IFMT) != S_IFCHR)
		fatal("%s: not a character device", special);
	cp = index(argv[0], '\0') - 1;
	if (cp == 0 || (*cp < 'a' || *cp > 'h') && !isdigit(*cp))
		fatal("%s: can't figure out file system partition", argv[0]);
#ifdef COMPAT
	if (!mfs && disktype == NULL)
		disktype = argv[1];
#endif
	lp = getdisklabel(special, fsi);
	if (isdigit(*cp))
		pp = &lp->d_partitions[0];
	else
		pp = &lp->d_partitions[*cp - 'a'];
	if (pp->p_size == 0)
		fatal("%s: `%c' partition is unavailable", argv[0], *cp);
	if (fssize == 0)
		fssize = pp->p_size;
	if (fssize > pp->p_size && !mfs)
	       fatal("%s: maximum file system size on the `%c' partition is %d",
			argv[0], *cp, pp->p_size);
	if (rpm == 0) {
		rpm = lp->d_rpm;
		if (rpm <= 0)
			rpm = 3600;
	}
	if (ntracks == 0) {
		ntracks = lp->d_ntracks;
		if (ntracks <= 0)
			fatal("%s: no default #tracks", argv[0]);
	}
	if (nsectors == 0) {
		nsectors = lp->d_nsectors;
		if (nsectors <= 0)
			fatal("%s: no default #sectors/track", argv[0]);
	}
	if (sectorsize == 0) {
		sectorsize = lp->d_secsize;
		if (sectorsize <= 0)
			fatal("%s: no default sector size", argv[0]);
	}
	if (trackskew == -1) {
		trackskew = lp->d_trackskew;
		if (trackskew < 0)
			trackskew = 0;
	}
	if (interleave == 0) {
		interleave = lp->d_interleave;
		if (interleave <= 0)
			interleave = 1;
	}
	if (fsize == 0) {
		fsize = pp->p_fsize;
		if (fsize <= 0)
			fsize = MAX(DFL_FRAGSIZE, lp->d_secsize);
	}
	if (bsize == 0) {
		bsize = pp->p_frag * pp->p_fsize;
		if (bsize <= 0)
			bsize = MIN(DFL_BLKSIZE, 8 * fsize);
	}
	if (density == 0)
		density = NFPI * fsize;
	if (minfree < 10 && opt != FS_OPTSPACE) {
		fprintf(stderr, "Warning: changing optimization to space ");
		fprintf(stderr, "because minfree is less than 10%%\n");
		opt = FS_OPTSPACE;
	}
	if (trackspares == -1) {
		trackspares = lp->d_sparespertrack;
		if (trackspares < 0)
			trackspares = 0;
	}
	nphyssectors = nsectors + trackspares;
	if (cylspares == -1) {
		cylspares = lp->d_sparespercyl;
		if (cylspares < 0)
			cylspares = 0;
	}
	secpercyl = nsectors * ntracks - cylspares;
	if (secpercyl != lp->d_secpercyl)
		fprintf(stderr, "%s (%d) %s (%d)\n",
			"Warning: calculated sectors per cylinder", secpercyl,
			"disagrees with disk label", lp->d_secpercyl);
	if (maxbpg == 0)
		maxbpg = MAXBLKPG(bsize);
	headswitch = lp->d_headswitch;
	trackseek = lp->d_trkseek;
	/* Reno fix: label may be 0 if faked up by kernel */
#ifdef notdef
	bbsize = lp->d_bbsize;
	sbsize = lp->d_sbsize;
#endif
	oldpartition = *pp;
#ifdef tahoe
	realsectorsize = sectorsize;
	if (sectorsize != DEV_BSIZE) {		/* XXX */
		int secperblk = DEV_BSIZE / sectorsize;

		sectorsize = DEV_BSIZE;
		nsectors /= secperblk;
		nphyssectors /= secperblk;
		secpercyl /= secperblk;
		fssize /= secperblk;
		pp->p_size /= secperblk;
	}
#endif
	mkfs(pp, special, fsi, fso);
#ifdef tahoe
	if (realsectorsize != DEV_BSIZE)
		pp->p_size *= DEV_BSIZE / realsectorsize;
#endif
	if (!Nflag && bcmp(pp, &oldpartition, sizeof(oldpartition)))
		rewritelabel(special, fso, lp);
	if (!Nflag)
		close(fso);
	close(fsi);
	if (mfs) {
		sprintf(buf, "mfs:%d", getpid());
		args.name = buf;
		args.base = membase;
		args.size = fssize * sectorsize;
		if (mount(MOUNT_MFS, argv[1], mntflags, &args) < 0) {
			perror("mfs: mount");
			exit(5);
		}
	}
	exit(0);
}

#ifdef COMPAT
char lmsg[] = "%s: can't read disk label; disk type must be specified";
#else
char lmsg[] = "%s: can't read disk label";
#endif

struct disklabel *
getdisklabel(s, fd)
	char *s;
	int fd;
{
	static struct disklabel lab;

	if (ioctl(fd, DIOCGDINFO, (char *)&lab) < 0) {
#ifdef COMPAT
		if (disktype) {
			struct disklabel *getdiskbyname();

			unlabelled++;
			return (getdiskbyname(disktype));
		}
#endif
		perror("ioctl (GDINFO)");
		fatal(lmsg, s);
	}
	return (&lab);
}

rewritelabel(s, fd, lp)
	char *s;
	int fd;
	register struct disklabel *lp;
{

#ifdef COMPAT
	if (unlabelled)
		return;
#endif
	lp->d_checksum = 0;
	lp->d_checksum = dkcksum(lp);
	if (ioctl(fd, DIOCWDINFO, (char *)lp) < 0) {
		perror("ioctl (WDINFO)");
		fatal("%s: can't rewrite disk label", s);
	}
#if vax
	if (lp->d_type == DTYPE_SMD && lp->d_flags & D_BADSECT) {
		register i;
		int cfd;
		daddr_t alt;
		char specname[64];
		char blk[1024];
		char *cp;

		/*
		 * Make name for 'c' partition.
		 */
		strcpy(specname, s);
		cp = specname + strlen(specname) - 1;
		if (!isdigit(*cp))
			*cp = 'c';
		cfd = open(specname, O_WRONLY);
		if (cfd < 0) {
			perror(specname);
			exit(6);
		}
		bzero(blk, sizeof(blk));
		*(struct disklabel *)(blk + LABELOFFSET) = *lp;
		alt = lp->d_ncylinders * lp->d_secpercyl - lp->d_nsectors;
		for (i = 1; i < 11 && i < lp->d_nsectors; i += 2) {
			if (lseek(cfd, (off_t)(alt + i) * lp->d_secsize, L_SET) == -1) {
				perror("lseek to badsector area");
				exit(7);
			}
			if (write(cfd, blk, lp->d_secsize) < lp->d_secsize) {
				int oerrno = errno;
				fprintf(stderr, "alternate label %d ", i/2);
				errno = oerrno;
				perror("write");
			}
		}
		close(cfd);
	}
#endif
}

/*VARARGS*/
fatal(fmt, arg1, arg2)
	char *fmt;
{

	fprintf(stderr, "%s: ", progname);
	fprintf(stderr, fmt, arg1, arg2);
	putc('\n', stderr);
	exit(8);
}
