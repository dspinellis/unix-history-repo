/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)disklabel.c	7.1 (Berkeley) %G%
 */

#ifndef	STANDALONE
#include <stdio.h>
#else
#define	stderr	0
#define	NULL	0
#endif
#include <disktab.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include "disk.h"

#define	BOOTSIZE	(8*1024)		/* size of boot "block" */

#define O_RDONLY	0
#define O_WRONLY	1
#define O_RDWR		2
#define L_SET		0

#ifdef	STANDALONE
#ifdef	TP
char	*standdisk = "cst2d:" ;
char	*st506boot = "cst2e:";		/* ST506 boot block */
char	*scsiboot = "cst2f:";		/* SCSI boot block */
#else
char	*standdisk = "/etc/disktab" ;
char	*st506boot = "/stand/bootwd";		/* ST506 boot block */
char	*scsiboot = "/stand/bootswd";		/* SCSI boot block */
#endif
#else
char	*st506boot = "/stand/bootwd";		/* ST506 boot block */
char	*scsiboot = "/stand/bootswd";		/* SCSI boot block */
#endif

char	name[BOOTSIZE];
union {
	char bootstrap[BOOTSIZE];
	struct {
		char pad[LABELOFFSET];
		struct disklabel lab;
	} b;
} block0;

#define MAXTYPES	4
char *tnames[MAXTYPES] = {
	"type 0",
	"ST506",
	"floppy",
	"SCSI",
};

main(argc, argv)
char *argv[];
{
	register struct disklabel *lp = &block0.b.lab;
	register struct disktab *dp;
	register i;
	int f, b;
	char *boot ;
	char *p;
#ifndef	STANDALONE
	char *sprintf();

	if (argc < 2 || argc > 5) {
		fprintf(stderr, "usage: disklabel disk    (to read label)\n");
		fprintf(stderr,
	"or disklabel disk type [ packid ] [ bootblock ]    (to write label)\n");
		exit(1);
	}
	if (argv[1][0] != '/')
		sprintf(name, "/dev/r%sc", argv[1]);
	else
		strcpy(name, argv[1]);
	if (argc == 2) {
		f = open(name, O_RDONLY);
		if (f < 0 && argv[1][0] != '/') {
			sprintf(name, "/dev/r%s", argv[1]);
			f = open(name, O_RDONLY);
		}
		if (f < 0)
			Perror(name);
#else
	char buf[80],c ; 

new_file:
	f = getdev("File", name) ;
	for(;;)	{
		printf("R)ead, W)rite, F)ilename,  or E)xit [RWFE] ? ") ;
		c = getchar() ;
		printf("\n") ;
		if (c == 'E') break ;
		if (c == 'W') goto wr_lab ;
		if (c == 'F') { close(f) ; goto new_file ; }
		if (c != 'R') continue ;
#endif
		if (read(f, &block0, BOOTSIZE) < BOOTSIZE)
			Perror(name);
		if (lp->dk_magic != DISKMAGIC) {
			fprintf(stderr,
				"Bad pack magic number (pack is unlabeled)\n");
#ifndef	STANDALONE
			exit(1);
#else
			continue ;
#endif
		}
#ifndef	STANDALONE
		printf("%s (%.*s):\n", name, sizeof(lp->dk_name), lp->dk_name);
#else
		printf("%s (%s):\n", name, lp->dk_name);
#endif
		printf("%s, ", (unsigned) lp->dk_type < MAXTYPES?
			tnames[lp->dk_type] : "unknown type");
		if(lp->dk_type == DTYPE_SCSI) {
			printf("%d bytes/sector, %d sectors/drive, ",
				lp->dk_secsize, lp->dk_secperunit);
			printf("%d sectors/track,\n %d tracks/cylinder, ",
				lp->dk_secpercyl/lp->dk_ntracks,
				lp->dk_ntracks);
			printf ("%d sectors/cylinder, ", lp->dk_secpercyl) ;
			printf ("%s i/o mode\n", lp->dk_blind?"blind":"slow");
		} else {
			printf("%d bytes/sector, %d sectors/track, ",
				lp->dk_secsize, lp->dk_nsectors);
			printf("%d tracks/cylinder, %d cylinders\n",
				lp->dk_ntracks, lp->dk_ncylinders);
			if (lp->dk_secpercyl !=
				lp->dk_nsectors * lp->dk_ntracks)
				printf(
		"WARNING: sectors/cylinder field is wrong (%d instead of %d)\n",
					lp->dk_secpercyl,
					lp->dk_nsectors * lp->dk_ntracks);
			if (lp->dk_secperunit != lp->dk_nsectors *
		    		lp->dk_ntracks * lp->dk_ncylinders)
				printf(
		"WARNING: sectors/unit field is wrong (%d instead of %d)\n",
					lp->dk_secperunit,
					lp->dk_nsectors * lp->dk_ntracks *
					lp->dk_ncylinders);
		}
#ifndef	STANDALONE
		printf("partitions:\n");
		printf("\t       size    offset\n");
#else
		printf("partition table:\n");
#endif
		for (i = 0; i < 8; i++) {
#ifndef	STANDALONE
		    printf("\t%c: %8d %8d", 'a' + i,
#else
    printf("partition %c, size %d sectors, offset %d cylinders.", 'a' + i,
#endif
		       lp->dk_partition[i].nblocks, lp->dk_partition[i].cyloff);
		    if (lp->dk_partition[i].nblocks){
				if (lp->dk_type != DTYPE_SCSI) {
#ifndef	STANDALONE
			printf("\t(Cyl. %d - %d",
#else
			printf(" (from cyl %d to %d",
#endif
			    lp->dk_partition[i].cyloff,
			    lp->dk_partition[i].cyloff + 
			    (lp->dk_partition[i].nblocks + lp->dk_secpercyl
			        - 1) / lp->dk_secpercyl - 1);
			if (lp->dk_partition[i].nblocks % lp->dk_secpercyl)
			    putchar('*');
			putchar(')');
		    		} else {
				}
			}
		    printf("\n");
		}
#ifdef	STANDALONE
		continue ;
wr_lab:
	printf("Type (e.g. miniscribe85...): ") ;
	gets(buf) ;
	dp = getdiskbyname(buf);
	if (dp == NULL) {
		printf("%s: unknown disk type\n", buf);
#ifndef	STANDALONE
		exit(1);
#else
		continue ;
#endif
	}
#else
		exit(0);
	}
	dp = getdiskbyname(argv[2]);
	if (dp == NULL) {
		fprintf(stderr, "%s: unknown disk type\n", argv[2]);
		exit(1);
	}
	f = open(name, O_WRONLY);
	if (f < 0)
		Perror(name);
#endif
	if (strcmp(dp->d_type, "scsi") == 0 || strcmp(dp->d_type, "SCSI") == 0)
		boot = scsiboot ; else boot = st506boot ;
#ifndef	STANDALONE
	if (argc > 4)
		boot = argv[4];
#endif
	b = open(boot, O_RDONLY);
	if (b < 0)
		Perror(boot);
	if (read(b, &block0, BOOTSIZE) < 0)
		Perror(boot);
	close(b) ;
	for (p = (char *)lp; p < (char *)lp + sizeof(struct disklabel); p++)
		if (*p) {
			fprintf(stderr,
			    "Bootstrap doesn't leave room for disk label\n");
			exit(2);
		}
	lp->dk_magic = DISKMAGIC;
	if (strcmp(dp->d_type, "st506") == 0 ||
	    strcmp(dp->d_type, "ST506") == 0) {
		lp->dk_type = DTYPE_ST506;
		lp->dk_precompcyl = dp->d_precomp;
	}
	if (strcmp(dp->d_type, "floppy") == 0)
		lp->dk_type = DTYPE_FLOPPY;
	if (strcmp(dp->d_type, "scsi") == 0)
		lp->dk_type = DTYPE_SCSI;

	if (strcmp(dp->d_type, "SCSI") == 0)
		lp->dk_type = DTYPE_SCSI;
	lp->dk_secsize = dp->d_secsize;
	lp->dk_nsectors = dp->d_nsectors;
	lp->dk_ntracks = dp->d_ntracks;
	lp->dk_ncylinders = dp->d_ncylinders;
	if (lp->dk_type == DTYPE_SCSI) {
		lp->dk_secpercyl = dp->d_secpercyl ;
		lp->dk_secperunit = dp->d_nsectors ;
		lp->dk_blind = dp->d_blind ;
	} else {
		lp->dk_secpercyl = dp->d_nsectors * dp->d_ntracks;
		lp->dk_secperunit = dp->d_nsectors * dp->d_ntracks
					* dp->d_ncylinders;
	}
	for (i = 0; i < 8; i++) {
		lp->dk_partition[i].nblocks = dp->d_partitions[i].p_size;
		if (lp->dk_partition[i].nblocks == -1)
			lp->dk_partition[i].nblocks = 0;
		lp->dk_partition[i].cyloff = dp->d_partitions[i].p_offset;
		if (lp->dk_partition[i].cyloff == -1)
			lp->dk_partition[i].cyloff = 0;
	}
#ifndef	STANDALONE
	if (argc > 3)
		strncpy(lp->dk_name, argv[3], sizeof(lp->dk_name));
	else
#endif
		strncpy(lp->dk_name, dp->d_name, sizeof(lp->dk_name));
	if (write(f, &block0, BOOTSIZE) < BOOTSIZE)
		Perror("write");
#ifdef	STANDALONE
	}
#endif
	exit(0);
}

Perror(op)
	char *op;
{

	fprintf(stderr, "disklabel: "); /*perror(op);*/
	exit(4);
}

#ifdef	STANDALONE
getdev(prompt, buf)
char *buf ;
{
	register int i;

	do {
		printf("%s: ", prompt);
		gets(buf);
		i = open(buf, 2);
	} while (i <= 0);
	return (i);
}

fprintf(a,b,c,d,e,f,g,h) {
	printf(b,c,d,e,f,g,h) ;
}
#endif
