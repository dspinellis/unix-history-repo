static	char *sccsid = "@(#)bad144.c	4.3 (Berkeley) 83/02/20";

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
 * reformatting.  To add a new bad sector the formatter must be used in
 * general since UNIX doesn't have on-line formatters to write the BSE
 * error in the header.  The
 */
#include <sys/types.h>
#include <sys/dkbad.h>

#include <stdio.h>
#include <disktab.h>

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
	if (argc < 2) {
		fprintf(stderr, "usage: bad144 type disk [ snum [ bn ... ] ]\n");
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
	f = open(name, 1);
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
	exit(0);
}
