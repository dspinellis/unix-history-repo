static	char *sccsid = "@(#)bad144.c	4.4 (Berkeley) 83/02/23";

/*
 * bad144
 *
 * This program prints and/or initializes a bad block record for a pack,
 * in the format used by the DEC standard 144.
 *
 * BUGS:
 *	Only reads/writes the first of the bad block record (sector 0
 *	of the last track of the disk); in fact, there are copies
 *	of the information in the first 5 even numbered sectors of this
 *	track, but UNIX uses only the first, and we don't bother with the
 *	others.
 *
 * It is preferable to write the bad information with a standard formatter,
 * but this program will do in a pinch, e.g. if the bad information is
 * accidentally wiped out this is a much faster way of restoring it than
 * reformatting. 
 * 
 * For the RP06 the -f flag may be used to mark a sector as bad by
 * inverting the format bit in the header and writing the sector header.
 * One should be able to do this on all drives ... as soon as someone
 * puts the time into it.
 */
#include <sys/types.h>
#include <sys/dkbad.h>
#include <sys/ioctl.h>
#include <machine/dkio.h>

#include <stdio.h>
#include <disktab.h>

int	fflag;
struct	dkbad dkbad;

main(argc, argv)
	int argc;
	char **argv;
{
	register struct bt_bad *bt;
	register struct disktab *dp;
	char name[BUFSIZ];
	int size, i, f, bad, oldbad, errs;

	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-f") == 0) {
		argc--, argv++;
		fflag++;
	}
	if (argc < 2) {
		fprintf(stderr,
		  "usage: bad144 [ -f ] type disk [ snum [ bn ... ] ]\n");
		fprintf(stderr, "e.g.: bad144 rk07 hk0\n");
		exit(1);
	}
	dp = getdiskbyname(argv[0]);
	if (dp == NULL) {
		fprintf(stderr, "%s: unknown disk type\n", argv[0]);
		exit(1);
	}
	sprintf(name, "/dev/r%sc", argv[1]);
	argc -= 2;
	argv += 2;
	size = dp->d_nsectors * dp->d_ntracks * dp->d_ncylinders; 
	if (argc == 0) {
		f = open(name, 0);
		if (f < 0) {
			perror(name);
			exit(1);
		}
		lseek(f, dp->d_secsize * (size - dp->d_nsectors), 0);
		printf("bad block information at 0x%x in %s:\n",
		    tell(f), name);
		if (read(f, &dkbad, sizeof (struct dkbad)) !=
		    sizeof (struct dkbad)) {
			fprintf("%s: can't read bad block info (wrong type disk?)\n");
			exit(1);
		}
		printf("cartidge serial number: %d(10)\n", dkbad.bt_csn);
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
		oldbad = 0;
		bt = dkbad.bt_bad;
		for (i = 0; i < 128; i++) {
			bad = (bt->bt_cyl<<16) + bt->bt_trksec;
			if (bad < 0)
				break;
			printf("sn=%d, cn=%d, tn=%d, sn=%d\n",
			    (bt->bt_cyl*dp->d_ntracks + (bt->bt_trksec>>8)) *
				dp->d_nsectors + (bt->bt_trksec&0xff),
			    bt->bt_cyl, bt->bt_trksec>>8, bt->bt_trksec&0xff);
			bt++;
		}
		exit (0);
	}
	f = open(name, 1 + fflag);
	if (f < 0) {
		perror(name);
		exit(1);
	}
	dkbad.bt_csn = atoi(*argv++);
	argc--;
	dkbad.bt_mbz = 0;
	if (argc > 2 * dp->d_nsectors || argc > 126) {
		printf("bad144: too many bad sectors specified\n");
		if (2 * dp->d_nsectors > 126)
			printf("limited to 126 by information format\n");
		else
			printf("limited to %d (only 2 tracks of sectors)\n",
			    2 * dp->d_nsectors);
		exit(1);
	}
	errs = 0;
	i = 0;
	while (argc > 0) {
		int sn = atoi(*argv++);

		argc--;
		if (sn < 0 || sn >= size) {
			printf("%d: out of range [0,%d) for %s\n",
			    sn, size, dp->d_name);
			errs++;
		}
		dkbad.bt_bad[i].bt_cyl = sn / (dp->d_nsectors*dp->d_ntracks);
		sn %= (dp->d_nsectors*dp->d_ntracks);
		dkbad.bt_bad[i].bt_trksec =
		    ((sn/dp->d_nsectors) << 8) + (sn%dp->d_nsectors);
		i++;
	}
	while (i < 126) {
		dkbad.bt_bad[i].bt_trksec = -1;
		dkbad.bt_bad[i].bt_cyl = -1;
		i++;
	}
	if (errs)
		exit(1);
	lseek(f, dp->d_secsize * (size - dp->d_nsectors), 0);
	if (write(f, (caddr_t)&dkbad, sizeof (dkbad)) != sizeof (dkbad)) {
		perror(name);
		exit(1);
	}
	if (fflag)
		for (i = 0, bt = dkbad.bt_bad; i < 128; i++, bt++) {
			daddr_t bn;

			bad = (bt->bt_cyl<<16) + bt->bt_trksec;
			if (bad < 0)
				break;
			bn = (bt->bt_cyl * dp->d_ntracks +
			    (bt->bt_trksec >> 8)) *
			    dp->d_nsectors + (bt->bt_trksec & 0xff);
			format(f, dp, bn);
		}
	exit(0);
}

struct rp06hdr {
	short	h_cyl;
	short	h_trksec;
	short	h_key1;
	short	h_key2;
	char	h_data[512];
#define	RP06_FMT	010000		/* 1 == 16 bit, 0 == 18 bit */
};
int	rp06fmt();

struct	formats {
	char	*f_name;		/* disk name */
	int	f_bufsize;		/* size of sector + header */
	int	(*f_routine)();		/* routine for special handling */
} formats[] = {
	{ "rp06",	sizeof (struct rp06hdr),	rp06fmt },
	{ 0, 0, 0 }
};

format(fd, dp, blk)
	int fd;
	struct disktab *dp;
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
	lseek(fd, (long)blk * 512, 0);
	ioctl(fd, DKIOCHDR, 0);
	read(fd, buf, fp->f_bufsize);
	if (fp->f_routine)
		(*fp->f_routine)(fp, dp, blk, buf);
	lseek(fd, (long)blk * 512, 0);
	ioctl(fd, DKIOCHDR, 0);
	write(fd, buf, fp->f_bufsize);
}

rp06fmt(fp, dp, bn, hp)
	struct format *fp;
	struct disktab *dp;
	daddr_t bn;
	struct rp06hdr *hp;
{

	hp->h_cyl &= ~RP06_FMT;
}
