#ifndef lint
static char sccsid[] = "@(#)disklabel.c	5.3 (Berkeley) %G%";
/* from static char sccsid[] = "@(#)disklabel.c	1.2 (Symmetric) 11/28/85"; */
#endif

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/file.h>
#include <sys/fs.h>
#include <sys/ioctl.h>
#define DKTYPENAMES
#include <sys/disklabel.h>

/*
 * Disklabel: read and write disklabels.
 * The label is usually placed on one of the first sectors of the disk.
 * Many machines (VAX 11/750) also place a bootstrap in the same area,
 * in which case the label is embedded in the bootstrap.
 * The bootstrap source must leave space at the proper offset
 * for the label on such machines.
 */

#ifdef vax
#define RAWPARTITION	'c'
#else
#define RAWPARTITION	'a'
#endif

#ifndef BBSIZE
#define	BBSIZE	8192			/* size of boot area, with label */
#endif

#ifdef vax
#define	BOOT				/* also have bootstrap in "boot area" */
#define	BOOTDIR	"/usr/mdec"		/* source of boot binaries */
#else
#ifdef lint
#define	BOOT
#endif
#endif

char	*dkname;
char	*dkbasename;
char	*xxboot;
char	*bootxx;
char	*specname;
char	*sprintf();
char	*rindex();

extern	int errno;
char	namebuf[BBSIZE], *np = namebuf;
char	bootarea[BBSIZE];
struct	disklabel lab;
struct	disklabel *readlabel(), *getbootarea();

int	op;			/* one of: */
#define	READ	1
#define	WRITE	2
#define	EDIT	3
#define	RESTORE	4

int	rflag;

main(argc, argv)
	int argc;
	char *argv[];
{
	register struct disklabel *lp;
	int f, t;
	char *name = 0, *asciifile, *type;

	while (argc > 1 && argv[1][0] == '-') {
		if (strcmp(argv[1], "-e") == 0) {
			if (argc != 3)
				goto usage;
			op = EDIT;
		} else if (strcmp(argv[1], "-w") == 0) {
			if (argc != 4)
				goto usage;
			op = RESTORE;
		} else if (strcmp(argv[1], "-r") == 0)
			rflag++;
		else
			goto usage;
		argv++;
		argc--;
	}
	if (argc < 2 || argc > 6) {
usage:
		fprintf(stderr, "%s%s%s%s",
#ifdef BOOT
"usage: disklabel disk    				(to read label)\n",
"or disklabel disk type [ packid ] [ xxboot bootxx ]    (to write label)\n",
"or disklabel -e disk    				(to edit label)\n",
"or disklabel -w disk protofile [ xxboot bootxx ]	(to restore label)\n"
#else
"usage: disklabel disk    			(to read label)\n",
"or disklabel disk type [ packid ]		(to write label)\n",
"or disklabel -e disk    			(to edit label)\n",
"or disklabel -w disk protofile			(to restore label)\n"
#endif
			);
		exit(1);
	}
	if (op == 0)
		if (argc == 2)
			op = READ;
		else
			op = WRITE;
	dkname = argv[1];
	if (dkname[0] != '/') {
		sprintf(np, "/dev/r%s%c", dkname, RAWPARTITION);
		specname = np;
		np += strlen(specname) + 1;
	} else
		specname = dkname;
	f = open(specname, op == READ ? O_RDONLY : O_RDWR);
	if (f < 0 && errno == ENOENT && dkname[0] != '/') {
		sprintf(specname, "/dev/r%s", dkname);
		np = namebuf + strlen(specname) + 1;
		f = open(specname, op == READ ? O_RDONLY : O_RDWR);
	}
	if (f < 0)
		Perror(specname);

	if (op == WRITE) {
		type = argv[2];
		if (argc == 4 || argc == 6) {
			name = argv[3];
			argv++;
			argc--;
		}
	}
	if (op == RESTORE) {
		asciifile = argv[2];
		argv++;
		argc--;
	}
#ifdef BOOT
	if (argc == 5) {
		xxboot = argv[3];
		bootxx = argv[4];
	}
#endif


	switch (op) {

	case READ:
		lp = readlabel(f, 0);
		display(stdout, lp);
		break;

	case EDIT:
		lp = readlabel(f, bootarea);
		edit(f, lp);
		writelabel(f, bootarea, lp);
		break;

	case WRITE:
		makelabel(type, name, &lab);
		lp = getbootarea(bootarea, &lab);
		*lp = lab;
		writelabel(f, bootarea, lp);
		break;

	case RESTORE:
		lab.d_secsize = DEV_BSIZE;			/* XXX */
		lab.d_bbsize = BBSIZE;				/* XXX */
		lp = getbootarea(bootarea, &lab);
		t = open(asciifile, O_RDONLY);
		if (t < 0)
			Perror(asciifile);
		getasciilabel(t, lp);
		writelabel(f, bootarea, lp);
		break;
	}
	exit(0);
}

makelabel(type, name, lp)
	char *type, *name;
	register struct disklabel *lp;
{
	register struct disklabel *dp;
	register char *p;

	dp = getdiskbyname(type);
	if (dp == NULL) {
		fprintf(stderr, "%s: unknown disk type\n", type);
		exit(1);
	}
	*lp = *dp;
	if (name)
		strncpy(lp->d_name, name, sizeof(lp->d_name));
}

writelabel(f, boot, lp)
	int f;
	char *boot;
	register struct disklabel *lp;
{
	register i;

	lp->d_magic = DISKMAGIC;
	lp->d_magic2 = DISKMAGIC;
	lp->d_checksum = 0;
	lp->d_checksum = dkcksum(lp);
	lseek(f, (off_t)0, L_SET);
	if (rflag) {
		if (write(f, boot, lp->d_bbsize) < lp->d_bbsize)
			Perror("write");
		if (ioctl(f, DIOCSDINFO, lp) < 0)
			Perror("ioctl DIOCSDINFO");
	} else if (ioctl(f, DIOCWDINFO, lp) < 0)
		Perror("ioctl DIOCWDINFO");
#if vax
	if (lp->d_type == DTYPE_SMD && lp->d_flags & D_BADSECT) {
		daddr_t alt;

		alt = lp->d_ncylinders * lp->d_secpercyl - lp->d_nsectors;
		for (i = 1; i < 11 && i < lp->d_nsectors; i += 2) {
			lseek(f, (off_t)(alt + i) * lp->d_secsize, L_SET);
			if (write(f, boot, lp->d_secsize) < lp->d_secsize) {
				int oerrno = errno;
				fprintf(stderr, "alternate label %d ", i/2);
				errno = oerrno;
				perror("write");
			}
		}
	}
#endif
}

/*
 * Read disklabel from disk.
 * If boot is given, need bootstrap too.
 * If boot not needed, use ioctl to get label
 * unless -r flag is given.
 */
struct disklabel *
readlabel(f, boot)
	int f;
	char *boot;
{
	register struct disklabel *lp;
	register char *buf;

	if (boot)
		buf = boot;
	else
		buf = bootarea;
	lp = (struct disklabel *)(buf + LABELOFFSET);
	if (rflag == 0 && boot == 0) {
		if (ioctl(f, DIOCGDINFO, lp) < 0)
			Perror("ioctl DIOCGDINFO");
	} else {
		if (read(f, buf, BBSIZE) < BBSIZE)
			Perror(specname);
		for (lp = (struct disklabel *)buf;
		    lp <= (struct disklabel *)(buf + BBSIZE - sizeof(*lp));
		    lp = (struct disklabel *)((char *)lp + 16))
			if (lp->d_magic == DISKMAGIC &&
			    lp->d_magic2 == DISKMAGIC)
				break;
		if (lp > (struct disklabel *)(buf + BBSIZE - sizeof(*lp)) ||
		    lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC ||
		    dkcksum(lp) != 0) {
			fprintf(stderr,
	"Bad pack magic number (label is damaged, or pack is unlabeled)\n");
			exit(1);
		}
	}
	return (lp);
}

struct disklabel *
getbootarea(boot, dp)
	char *boot;
	register struct disklabel *dp;
{
	struct disklabel *lp;
	register char *p;
	int b;

#ifdef BOOT
	if (xxboot == NULL) {
		dkbasename = np;
		if ((p = rindex(dkname, '/')) == NULL)
			p = dkname;
		else
			p++;
		while (*p && !isdigit(*p))
			*np++ = *p++;
		*np++ = '\0';

		sprintf(np, "%s/%sboot", BOOTDIR, dkbasename);
		if (access(np, F_OK) < 0 && dkbasename[0] == 'r')
			dkbasename++;
		xxboot = np;
		sprintf(xxboot, "%s/%sboot", BOOTDIR, dkbasename);
		np += strlen(xxboot) + 1;

		bootxx = np;
		sprintf(bootxx, "%s/boot%s", BOOTDIR, dkbasename);
		np += strlen(bootxx) + 1;
	}

	b = open(xxboot, O_RDONLY);
	if (b < 0)
		Perror(xxboot);
	if (read(b, boot, dp->d_secsize) < 0)
		Perror(xxboot);
	close(b);
	b = open(bootxx, O_RDONLY);
	if (b < 0)
		Perror(bootxx);
	if (read(b, &boot[dp->d_secsize], dp->d_bbsize-dp->d_secsize) < 0)
		Perror(bootxx);
	close(b);
#endif

	lp = (struct disklabel *)(boot + (LABELSECTOR * dp->d_secsize) +
	    LABELOFFSET);
	for (p = (char *)lp; p < (char *)lp + sizeof(struct disklabel); p++)
		if (*p) {
			fprintf(stderr,
			    "Bootstrap doesn't leave room for disk label\n");
			exit(2);
		}
	return (lp);
}

display(f, lp)
	FILE *f;
	register struct disklabel *lp;
{
	register i, j;
	register struct partition *pp;

	fprintf(f, "# %s:\n", specname);
	if ((unsigned) lp->d_type < DKMAXTYPES)
		fprintf(f, "type: %s\n", dktypenames[lp->d_type]);
	else
		fprintf(f, "type: %d\n", lp->d_type);
	fprintf(f, "disk: %.*s\n", sizeof(lp->d_typename), lp->d_typename);
	fprintf(f, "label: %.*s\n", sizeof(lp->d_name), lp->d_name);
	fprintf(f, "flags: ");
	if (lp->d_flags & D_REMOVABLE)
		fprintf(f, "removeable ");
	if (lp->d_flags & D_ECC)
		fprintf(f, "ecc ");
	if (lp->d_flags & D_BADSECT)
		fprintf(f, "badsect ");
	fprintf(f, "\n");
	fprintf(f, "bytes/sector: %d\n", lp->d_secsize);
	fprintf(f, "sectors/track: %d\n", lp->d_nsectors);
	fprintf(f, "tracks/cylinder: %d\n", lp->d_ntracks);
	fprintf(f, "cylinders: %d\n", lp->d_ncylinders);
	fprintf(f, "interleave: %d\n", lp->d_interleave);
	fprintf(f, "trackskew: %d\n", lp->d_trackskew);
	fprintf(f, "cylinderskew: %d\n", lp->d_cylskew);
	fprintf(f, "headswitch: %d\t\t# milliseconds\n", lp->d_headswitch);
	fprintf(f, "track-to-track seek: %d\t# milliseconds\n", lp->d_trkseek);
	fprintf(f, "drivedata: ");
	for (i = NDDATA - 1; i >= 0; i--)
		if (lp->d_drivedata[i])
			break;
	if (i < 0)
		i = 0;
	for (j = 0; j <= i; j++)
		fprintf(f, "%d ", lp->d_drivedata[j]);
	fprintf(f, "\n\n%d partitions:\n", lp->d_npartitions);
	fprintf(f, "#\t       size    offset     fstype\n");
	pp = lp->d_partitions;
	for (i = 0; i < lp->d_npartitions; i++, pp++) {
		fprintf(f, "\t%c: %8d %8d    ", 'a' + i,
		   pp->p_size, pp->p_offset);
		if (pp->p_size) {
			if ((unsigned) pp->p_fstype < FSMAXTYPES)
				fprintf(f, "%8.8s", fstypenames[pp->p_fstype]);
			else
				fprintf(f, "%8d", pp->p_fstype);
			fprintf(f, "\t# (Cyl. %4d",
			    pp->p_offset / lp->d_secpercyl);
			if (pp->p_offset % lp->d_secpercyl)
			    putc('*', f);
			else
			    putc(' ', f);
			fprintf(f, "- %d",
			    (pp->p_offset + 
			    pp->p_size + lp->d_secpercyl - 1) /
			    lp->d_secpercyl - 1);
			if (pp->p_size % lp->d_secpercyl)
			    putc('*', f);
			putc(')', f);
		}
		fprintf(f, "\n");
	}
}

edit(f)
	int f;
{
	fprintf(stderr, "sorry, not yet\n");
	exit(1);
}


/*
 * Read an ascii label in from fd f,
 * in the same format as that put out by display(),
 * and fill in lp.
 */
getasciilabel(f, lp)
	int f;
	register struct disklabel *lp;
{
	fprintf(stderr, "sorry, not yet\n");
	exit(1);
}

Perror(op)
	char *op;
{

	fprintf(stderr, "disklabel: "); perror(op);
	exit(4);
}
