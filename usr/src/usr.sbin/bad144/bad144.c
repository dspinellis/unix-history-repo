/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)bad144.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * bad144
 *
 * This program prints and/or initializes a bad block record for a pack,
 * in the format used by the DEC standard 144.
 * It can also add bad sector(s) to the record, moving the sector
 * replacements as necessary.
 *
 * It is preferable to write the bad information with a standard formatter,
 * but this program will do.
 * 
 * RP06 sectors are marked as bad by inverting the format bit in the
 * header; on other drives the valid-sector bit is cleared.
 */
#include <sys/types.h>
#include <sys/dkbad.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <machine/dkio.h>

#include <stdio.h>
#include <disktab.h>

int	fflag, add, copy, verbose;
int	compare();
struct	dkbad dkbad, oldbad;
daddr_t	size, getold(), badsn();
struct	disktab *dp;
char	name[BUFSIZ];

main(argc, argv)
	int argc;
	char *argv[];
{
	register struct bt_bad *bt;
	daddr_t	sn, bn[126];
	int i, f, nbad, new, bad, errs;

	argc--, argv++;
	while (argc > 0 && **argv == '-') {
		(*argv)++;
		while (**argv) {
			switch (**argv) {
			    case 'f':
				fflag++;
				break;
			    case 'a':
				add++;
				break;
			    case 'c':
				copy++;
				break;
			    case 'v':
				verbose++;
				break;
			}
			(*argv)++;
		}
		argc--, argv++;
	}
	if (argc < 2) {
		fprintf(stderr,
		  "usage: bad144 [ -f ] type disk [ snum [ bn ... ] ]\n");
		fprintf(stderr,
	      "to read or overwrite bad-sector table, e.g.: bad144 rk07 hk0\n");
		fprintf(stderr,
		  "or bad144 -a [ -f ] [ -c ] type disk  bn ...\n");
		fprintf(stderr, "where options are:\n");
		fprintf(stderr, "\t-a  add new bad sectors to the table\n");
		fprintf(stderr, "\t-f  reformat listed sectors as bad\n");
		fprintf(stderr, "\t-c  copy original sector to replacement\n");
		exit(1);
	}
	dp = getdiskbyname(argv[0]);
	if (dp == NULL) {
		fprintf(stderr, "%s: unknown disk type\n", argv[0]);
		exit(1);
	}
	if (argv[1][0] != '/')
		sprintf(name, "/dev/r%sc", argv[1]);
	else
		strcpy(name, argv[1]);
	argc -= 2;
	argv += 2;
	size = dp->d_nsectors * dp->d_ntracks * dp->d_ncylinders; 
	if (argc == 0) {
		f = open(name, O_RDONLY);
		if (f < 0)
			Perror(name);
		sn = getold(f, &dkbad);
		printf("bad block information at sector %d in %s:\n",
		    sn, name);
		printf("cartridge serial number: %d(10)\n", dkbad.bt_csn);
		switch (dkbad.bt_flag) {

		case -1:
			printf("alignment cartridge\n");
			break;

		case 0:
			break;

		default:
			printf("bt_flag=%x(16)?\n", dkbad.bt_flag);
			break;
		}
		bt = dkbad.bt_bad;
		for (i = 0; i < 126; i++) {
			bad = (bt->bt_cyl<<16) + bt->bt_trksec;
			if (bad < 0)
				break;
			printf("sn=%d, cn=%d, tn=%d, sn=%d\n", badsn(bt),
			    bt->bt_cyl, bt->bt_trksec>>8, bt->bt_trksec&0xff);
			bt++;
		}
		exit(0);
	}
	f = open(name, (fflag || add)? O_RDWR: O_WRONLY);
	if (f < 0)
		Perror(name);
	if (add) {
		/*
		 * Read in the old badsector table.
		 * Verify that it makes sense, and the bad sectors
		 * are in order.  Copy the old table to the new one.
		 */
		(void) getold(f, &oldbad);
		i = checkold();
		if (verbose)
			printf("Had %d bad sectors\n", i);
		if (i + argc > 126) {
			printf("bad144: not enough room for %d more sectors\n",
				argc);
			printf("limited to 126 by information format\n");
			exit(1);
		}
		dkbad = oldbad;
	} else {
		dkbad.bt_csn = atoi(*argv++);
		argc--;
		dkbad.bt_mbz = 0;
		if (argc > 126) {
			printf("bad144: too many bad sectors specified\n");
			printf("limited to 126 by information format\n");
			exit(1);
		}
		i = 0;
	}
	errs = 0;
	new = argc;
	while (argc > 0) {
		daddr_t sn = atoi(*argv++);
		argc--;
		if (sn < 0 || sn >= size) {
			printf("%d: out of range [0,%d) for %s\n",
			    sn, size, dp->d_name);
			errs++;
			continue;
		}
		bn[i] = sn;
		dkbad.bt_bad[i].bt_cyl = sn / (dp->d_nsectors*dp->d_ntracks);
		sn %= (dp->d_nsectors*dp->d_ntracks);
		dkbad.bt_bad[i].bt_trksec =
		    ((sn/dp->d_nsectors) << 8) + (sn%dp->d_nsectors);
		i++;
	}
	if (errs)
		exit(1);
	nbad = i;
	while (i < 126) {
		dkbad.bt_bad[i].bt_trksec = -1;
		dkbad.bt_bad[i].bt_cyl = -1;
		i++;
	}
	if (add) {
		/*
		 * Sort the new bad sectors into the list.
		 * Then shuffle the replacement sectors so that
		 * the previous bad sectors get the same replacement data.
		 */
		qsort(dkbad.bt_bad, nbad, sizeof (struct bt_bad), compare);
		shift(f, nbad, nbad-new);
	}
	for (i = 0; i < 10; i += 2) {
		if (lseek(f, dp->d_secsize * (size - dp->d_nsectors + i),
		    L_SET) < 0)
			Perror("lseek");
		if (verbose)
			printf("write badsect file at %d\n",
				size - dp->d_nsectors + i);
		if (write(f, (caddr_t)&dkbad, sizeof dkbad) != sizeof dkbad) {
			char msg[80];
			sprintf(msg, "bad144: write bad sector file %d", i/2);
			perror(msg);
		}
	}
	if (fflag)
		for (i = 0; i < new; i++)
			format(f, bn[i]);
	exit(0);
}

daddr_t
getold(f, bad)
struct dkbad *bad;
{
	register int i;
	daddr_t sn;
	char msg[80];

	for (i = 0; i < 10; i += 2) {
		sn = size - dp->d_nsectors + i;
		if (lseek(f, sn * dp->d_secsize, L_SET) < 0)
			Perror("lseek");
		if (read(f, bad, sizeof (*bad)) == sizeof (*bad)) {
			if (i > 0)
				printf("Using bad-sector file %d\n", i/2);
			return(sn);
		}
		sprintf(msg, "bad144: read bad sector file at sn %d", sn);
		perror(msg);
	}
	fprintf(stderr,
	    "bad144: %s: can't read bad block info\n", name);
	exit(1);
}

checkold()
{
	register int i;
	register struct bt_bad *bt;
	daddr_t sn, lsn;

	if (oldbad.bt_flag != 0) {
		fprintf(stderr, "bad144: %s: bad flag in bad-sector table\n",
			name);
		exit(1);
	}
	if (oldbad.bt_mbz != 0) {
		fprintf(stderr, "bad144: %s: bad magic number\n", name);
		exit(1);
	}
	lsn = 0;
	bt = oldbad.bt_bad;
	for (i = 0; i < 126; i++, bt++) {
		if (bt->bt_cyl == -1 && bt->bt_trksec == -1)
			break;
		if ((bt->bt_cyl >= dp->d_ncylinders) ||
		    ((bt->bt_trksec >> 8) >= dp->d_ntracks) ||
		    ((bt->bt_trksec & 0xff) >= dp->d_nsectors)) {
			fprintf(stderr, "bad144: cyl/sect/trk out of range\n");
			exit(1);
		}
		sn = (bt->bt_cyl * dp->d_ntracks +
		    (bt->bt_trksec >> 8)) *
		    dp->d_nsectors + (bt->bt_trksec & 0xff);
		if (sn < lsn) {
		    fprintf(stderr, "bad144: bad sector file out of order\n");
		    exit(1);
		}
		lsn = sn;
	}
	return i;
}

/*
 * Move the bad sector replacements
 * to make room for the new bad sectors.
 * new is the new number of bad sectors, old is the previous count.
 */
shift(f, new, old)
{
	daddr_t repl;

	/*
	 * First replacement is last sector of second-to-last track.
	 */
	repl = size - dp->d_nsectors - 1;
	new--; old--;
	while (new >= 0 && new != old) {
		if (old < 0 ||
		    compare(&dkbad.bt_bad[new], &oldbad.bt_bad[old]) > 0) {
			/*
			 * Insert new replacement here-- copy original
			 * sector if requested and possible,
			 * otherwise write a zero block.
			 */
			if (!copy ||
			    !blkcopy(f, badsn(&dkbad.bt_bad[new]), repl - new))
				blkzero(f, repl - new);
		} else {
			if (blkcopy(f, repl - old, repl - new) == 0)
			    fprintf(stderr,
				"Can't copy replacement sector %d to %d\n",
				repl-old, repl-new);
			old--;
		}
		new--;
	}
}

/*
 *  Copy disk sector s1 to s2.
 */
blkcopy(f, s1, s2)
daddr_t s1, s2;
{
	char buf[512];

	if (lseek(f, dp->d_secsize * s1, L_SET) < 0)
		Perror("lseek");
	if (read(f, buf, sizeof (buf)) != sizeof (buf)) {
		if (verbose)
			fprintf(stderr, "bad144: can't read sector, %d\n", s1);
		return(0);
	}
	if (lseek(f, dp->d_secsize * s2, L_SET) < 0)
		Perror("lseek");
	if (verbose)
		printf("copying %d to %d\n", s1, s2);
	if (write(f, buf, sizeof (buf)) != sizeof (buf)) {
		fprintf(stderr,
		    "bad144: can't write replacement sector, %d\n", s2);
		return(0);
	}
	return(1);
}

char zbuf[512];

blkzero(f, sn)
daddr_t sn;
{

	if (lseek(f, dp->d_secsize * sn, L_SET) < 0)
		Perror("lseek");
	if (verbose)
		printf("zeroing %d\n", sn);
	if (write(f, zbuf, sizeof (zbuf)) != sizeof (zbuf)) {
		fprintf(stderr,
		    "bad144: can't write replacement sector, %d\n", sn);
		exit(1);
	}
}

compare(b1, b2)
register struct bt_bad *b1, *b2;
{
	if (b1->bt_cyl > b2->bt_cyl)
		return(1);
	if (b1->bt_cyl < b2->bt_cyl)
		return(-1);
	return (b2->bt_trksec - b1->bt_trksec);
}

daddr_t
badsn(bt)
register struct bt_bad *bt;
{
	return ((bt->bt_cyl*dp->d_ntracks + (bt->bt_trksec>>8)) * dp->d_nsectors
		+ (bt->bt_trksec&0xff));
}

struct rp06hdr {
	short	h_cyl;
	short	h_trksec;
	short	h_key1;
	short	h_key2;
	char	h_data[512];
#define	RP06_FMT	010000		/* 1 == 16 bit, 0 == 18 bit */
};

/*
 * Most massbus and unibus drives
 * have headers of this form
 */
struct hpuphdr {
	u_short	hpup_cyl;
	u_short hpup_trksec;
	char	hpup_data[512];
#define	HPUP_OKSECT	0xc000		/* this normally means sector is good */
};

struct	formats {
	char	*f_name;		/* disk name */
	int	f_bufsize;		/* size of sector + header */
	int	f_bic;			/* value to bic in hpup_cyl */
	int	(*f_routine)();		/* routine for special handling */
} formats[] = {
	{ "rp06",	sizeof (struct rp06hdr),	RP06_FMT,	0 },
	{ "eagle",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ "capricorn",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ "rm03",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ "rm05",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ "9300",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ "9766",	sizeof (struct hpuphdr),	HPUP_OKSECT,	0 },
	{ 0, 0, 0, 0 }
};

format(fd, blk)
	int fd;
	daddr_t blk;
{
	register struct formats *fp;
	char *buf, *malloc();

	for (fp = formats; fp->f_name; fp++)
		if (strcmp(dp->d_name, fp->f_name) == 0)
			break;
	if (fp->f_name == 0) {
		fprintf(stderr, "bad144: don't know how to format %s disks\n",
			dp->d_name);
		exit(2);
	}
	buf = malloc(fp->f_bufsize);
	if (buf == NULL) {
		fprintf(stderr, "bad144: can't allocate sector buffer\n");
		exit(3);
	}
	/*
	 * Here we do the actual formatting.  All we really
	 * do is rewrite the sector header and flag the bad sector
	 * according to the format table description.  If a special
	 * purpose format routine is specified, we allow it to
	 * process the sector as well.
	 */
	if (lseek(fd, (long)blk * 512, L_SET) < 0)
		Perror("lseek");
	if (verbose)
		printf("format blk %d\n", blk);
	if (ioctl(fd, DKIOCHDR, 0) < 0)
		Perror("ioctl");
	read(fd, buf, fp->f_bufsize);
	if (fp->f_bic) {
		struct hpuphdr *xp = (struct hpuphdr *)buf;

		xp->hpup_cyl &= ~fp->f_bic;
	}
	if (fp->f_routine)
		(*fp->f_routine)(fp, dp, blk, buf);
	if (lseek(fd, (long)blk * 512, L_SET) < 0)
		Perror("lseek");
	if (ioctl(fd, DKIOCHDR, 0) < 0)
		Perror("ioctl");
	if (write(fd, buf, fp->f_bufsize) != fp->f_bufsize) {
		char msg[80];
		sprintf(msg, "bad144: write format %d", blk);
		perror(msg);
	}
}

Perror(op)
	char *op;
{

	fprintf(stderr, "bad144: "); perror(op);
	exit(4);
}
