#ifndef lint
static	char *sccsid = "@(#)bad144.c	4.7 (Berkeley) 83/07/27";
#endif

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
 * RP06 sectors are marked as bad by inverting the format bit in the
 * header; on other drives the BSE bit is set.
 */
#include <sys/types.h>
#include <sys/dkbad.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <machine/dkio.h>

#include <stdio.h>
#include <disktab.h>

int	fflag;
struct	dkbad dkbad;

main(argc, argv)
	int argc;
	char *argv[];
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
		f = open(name, O_RDONLY);
		if (f < 0)
			Perror(name);
		if (lseek(f, dp->d_secsize*(size-dp->d_nsectors), L_SET) < 0)
			Perror("lseek");
		printf("bad block information at sector %d in %s:\n",
		    tell(f)/512, name);
		if (read(f, &dkbad, sizeof (struct dkbad)) !=
		    sizeof (struct dkbad)) {
			fprintf("bad144: %s: can't read bad block info\n");
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
		exit(0);
	}
	f = open(name, 1 + fflag);
	if (f < 0)
		Perror(name);
	dkbad.bt_csn = atoi(*argv++);
	argc--;
	dkbad.bt_mbz = 0;
	if (argc > 126) {
		printf("bad144: too many bad sectors specified\n");
		printf("limited to 126 by information format\n");
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
	if (lseek(f, dp->d_secsize * (size - dp->d_nsectors), L_SET) < 0)
		Perror("lseek");
	if (write(f, (caddr_t)&dkbad, sizeof (dkbad)) != sizeof (dkbad))
		Perror(name);
	if (fflag)
		for (i = 0, bt = dkbad.bt_bad; i < 126; i++, bt++) {
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
	{ 0, 0, 0, 0 }
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
	if (lseek(fd, (long)blk * 512, L_SET) < 0)
		Perror("lseek");
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
	if (write(fd, buf, fp->f_bufsize) != fp->f_bufsize)
		Perror("write");
}

Perror(op)
	char *op;
{

	fprintf(stderr, "bad144: "); perror(op);
	exit(4);
}
