/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)disklabel.c	7.1 (Berkeley) %G%
 */

/*
 * disklabel.c -- operate disklabel for BSD & OMRON
 * by A.Fujita, FEB-17-1992
 */

#include <sys/param.h>
#define DKTYPENAMES
#include <sys/disklabel.h>
#include <luna68k/stand/saio.h>
#include <luna68k/stand/status.h>
#include <luna68k/stand/omron_disklabel.h>

#define LABEL_SIZE BBSIZE

u_char lbl_buff[LABEL_SIZE];

u_short
dkcksum(lp)
	register struct disklabel *lp;
{
	register u_short *start, *end;
	register u_short sum = 0;

	start = (u_short *)lp;
	end = (u_short *)&lp->d_partitions[lp->d_npartitions];
	while (start < end)
		sum ^= *start++;
	return (sum);
}

int
disklabel(argc, argv)
	int   argc;
	char *argv[];
{
	register struct scd_dk_label *omp = (struct scd_dk_label *) lbl_buff;
	register struct disklabel    *bp  = (struct disklabel *)omp->dkl_pad;
	register struct fs *fp = (struct fs *) lbl_buff;
	register u_short *p;
	register u_long chksum, count;
	register char *q;
	register int i, j;

	if (argc < 2) {
		printf("This command is required sub command !!\n");
		return(ST_ERROR);
	}

	if (!strcmp(argv[1], "help")) {
		printf("Subcommand of disklabel\n\n");
		printf("\thelp:\t\tthis command\n");
		printf("\tread:\t\tread disklabel from scsi_device\n");
		printf("\twrite:\t\twrite disklabel to scsi_device\n");
		printf("\tomron:\t\tshow OMRON disklabel infomation\n");
		printf("\tbsd:\t\tshow BSD disklabel infomation\n");
		printf("\tcopy:\t\tcopy disklabel infomation from OMRON to BSD\n");
		printf("\tchecksum:\tdoing checksum\n");
		printf("\tset:\t\tchange BSD disklabel infomation\n");
		printf("\n\n");
	} else if (!strcmp(argv[1], "read")) {
		if (scsi_read( 0, lbl_buff, LABEL_SIZE)) {
			printf("Disk Label read done.\n");
		} else {
			printf("Disk Label read error !!\n");
		}
	} else if (!strcmp(argv[1], "omron")) {
		i  = (int) &omp->dkl_badchk;
		i -= (int) lbl_buff;
		printf("Offset = %d\n", i);
		printf("\n");
		printf("Checksum of Bad Track:\t0x%x\n",	omp->dkl_badchk);
		printf("Logical Block Total:\t%d(0x%x)\n",	omp->dkl_maxblk, omp->dkl_maxblk);
		printf("Disk Drive Type:\t0x%x\n",		omp->dkl_dtype);
		printf("Number of Disk Drives:\t%d(0x%x)\n",	omp->dkl_ndisk, omp->dkl_ndisk);
		printf("Number of Data Cylinders:\t%d(0x%x)\n",	omp->dkl_ncyl, omp->dkl_ncyl);
		printf("Number of Alternate Cylinders:\t%d(0x%x)\n",
		       omp->dkl_acyl,omp->dkl_acyl);
		printf("Number of Heads in This Partition:\t%d(0x%x)\n",
		       omp->dkl_nhead, omp->dkl_nhead);
		printf("Number of 512 byte Sectors per Track:\t%d(0x%x)\n",
		       omp->dkl_nsect, omp->dkl_nsect);
		printf("Identifies Proper Label Locations:\t0x%x\n",
		       omp->dkl_bhead);
		printf("Physical Partition Number:\t%d(0x%x)\n",
		       omp->dkl_ppart, omp->dkl_ppart);
		for (i = 0; i < NLPART; i++)
			printf("\t%d:\t%d\t%d\n", i,
			       omp->dkl_map[i].dkl_blkno, omp->dkl_map[i].dkl_nblk);
		printf("Identifies This Label Format:\t0x%x\n",	omp->dkl_magic);
		printf("XOR Checksum of Sector:\t0x%x\n",	omp->dkl_cksum);
	} else if (!strcmp(argv[1], "checksum")) {
		if (omp->dkl_magic == DKL_MAGIC){
							/* checksum of disk-label */
			chksum = 0;
			count = sizeof(struct scd_dk_label) / sizeof(short int);
			for (p= (u_short *) lbl_buff; count > 0; count--) {
				if (count == 1)
					printf("Check Sum: 0x%x\n", chksum);
				chksum ^= *p++;
			}

			printf("dkl_cksum: 0x%x\n", omp->dkl_cksum);

			if (chksum != 0) {
				printf("OMRON Disklabel check sum error.\n");
			}
		} else {
			printf("OMRON Disklabel not found.\n");
		}
	} else if (!strcmp(argv[1], "copy")) {
		bzero(bp, sizeof(struct disklabel));

		bcopy(lbl_buff, bp->d_typename, 16);

		bp->d_secsize    = DEV_BSIZE;
		bp->d_nsectors   = 38;
		bp->d_ntracks    = 12;
		bp->d_ncylinders = 1076;

		bp->d_type  = DTYPE_SCSI;

		bp->d_secpercyl  = bp->d_nsectors * bp->d_ntracks;
		bp->d_secperunit = bp->d_secpercyl * bp->d_ncylinders;
		bp->d_rpm        = 3600;
		bp->d_interleave = 1;
		bp->d_trackskew  = 0;
		bp->d_cylskew    = 0;
		bp->d_headswitch = 0;
		bp->d_trkseek    = 0;
		bp->d_bbsize     = BBSIZE;
		bp->d_sbsize     = SBSIZE;

		for (i = 0; i < MAXPARTITIONS; i++) {
			bp->d_partitions[i].p_size   = omp->dkl_map[i].dkl_nblk;
			bp->d_partitions[i].p_offset = omp->dkl_map[i].dkl_blkno;
			bp->d_partitions[i].p_fsize  = 1024;
			bp->d_partitions[i].p_frag   = 8192 / 1024;
			bp->d_partitions[i].p_fstype = FS_UNUSED;
		}

		bp->d_npartitions = MAXPARTITIONS;

		for (i = 0; i < NDDATA; i++) {
			bp->d_drivedata[i] = 0;
		}

		bzero(bp->d_packname, 16);

		bp->d_magic    = DISKMAGIC;
		bp->d_magic2   = DISKMAGIC;
		bp->d_checksum = 0;
		bp->d_checksum = dkcksum(bp);

		/* restump checksum of OMRON disklabel */
		chksum = 0;
		count = sizeof(struct scd_dk_label) / sizeof(short int);
		for (p= (u_short *) lbl_buff; count > 1; count--) {
			chksum ^= *p++;
		}
		printf("chksum: 0x%x\n", chksum);

		omp->dkl_cksum = chksum;
		printf("dkl_cksum: 0x%x\n", omp->dkl_cksum);
	} else if (!strcmp(argv[1], "bsd")) {
		display(bp);
	} else if (!strcmp(argv[1], "write")) {
		if (scsi_write( 0, lbl_buff, LABEL_SIZE)) {
			printf("Disk Label write done.\n");
		} else {
			printf("Disk Label write error !!\n");
		}
	} else if (!strcmp(argv[1], "set")) {
		i = (argv[2])[1] - 'a';
		for (q = argv[3], j = 0; *q != NULL; q++) {
			j = (j * 10) + (*q - '0');
		}
		switch (*argv[2]) {
		case 'b':
			bp->d_partitions[i].p_frag = j / bp->d_partitions[i].p_fsize;
			break;
		case 'f':	/* fragment size */
			bp->d_partitions[i].p_fsize = j;
			break;
		case 'o':	/* offset */
			bp->d_partitions[i].p_offset = j;
			omp->dkl_map[i].dkl_blkno = j;
			break;
		case 'p':	/* size */
			bp->d_partitions[i].p_size = j;
			omp->dkl_map[i].dkl_nblk = j;
			break;
		case 't':	/* FS type */
			bp->d_partitions[i].p_fstype = j;
			break;
		default:
			break;
		}

		/* restump checksum of BSD disklabel */
		bp->d_checksum = 0;
		bp->d_checksum = dkcksum(bp);

		/* restump checksum of OMRON disklabel */
		chksum = 0;
		count = sizeof(struct scd_dk_label) / sizeof(short int);
		for (p= (u_short *) lbl_buff; count > 1; count--) {
			chksum ^= *p++;
		}
		omp->dkl_cksum = chksum;

	} else if (!strcmp(argv[1], "sb")) {
#define BLOCK_SIZE	SBSIZE

		printf("checking Super Block: block size = %d bytes, seek amount = 1 blocks\n",
			BLOCK_SIZE);
		i = j = 0;
		while(1) {
			if (!scsi_read( i, lbl_buff, BLOCK_SIZE))
			break;

			if (fp->fs_magic == FS_MAGIC) {
				printf("%d, (%d)\n", i, i - j);
				j = i;
			}
			i++;
		}
	} else if (!strcmp(argv[1], "sbcopy")) {
		if (!scsi_read(32, lbl_buff, BLOCK_SIZE)) {
			printf("sbcopy: read failed\n");
			return(ST_ERROR);
		}
		if (scsi_write(16, lbl_buff, BLOCK_SIZE)) {
			printf("sbcopy: copy done\n");
		} else {
			printf("sbcopy: write failed\n");
		}
	}

	return(ST_NORMAL);
}

int
display(lp)
	register struct disklabel *lp;
{
	register int i, j;
	register struct partition *pp;

	if ((unsigned) lp->d_type < DKMAXTYPES)
		printf("type: %s\n", dktypenames[lp->d_type]);
	else
		printf("type: %d\n", lp->d_type);
	printf("disk: %s\n",  lp->d_typename);
	printf("label: %s\n", lp->d_packname);
	printf("flags:");
	if (lp->d_flags & D_REMOVABLE)
		printf(" removeable");
	if (lp->d_flags & D_ECC)
		printf(" ecc");
	if (lp->d_flags & D_BADSECT)
		printf(" badsect");
	printf("\n");
	printf("bytes/sector: %d\n", lp->d_secsize);
	printf("sectors/track: %d\n", lp->d_nsectors);
	printf("tracks/cylinder: %d\n", lp->d_ntracks);
	printf("sectors/cylinder: %d\n", lp->d_secpercyl);
	printf("cylinders: %d\n", lp->d_ncylinders);
	printf("rpm: %d\n", lp->d_rpm);
	printf("interleave: %d\n", lp->d_interleave);
	printf("trackskew: %d\n", lp->d_trackskew);
	printf("cylinderskew: %d\n", lp->d_cylskew);
	printf("headswitch: %d\t\t# milliseconds\n", lp->d_headswitch);
	printf("track-to-track seek: %d\t# milliseconds\n", lp->d_trkseek);
	printf("drivedata: ");
	for (i = NDDATA - 1; i >= 0; i--)
		if (lp->d_drivedata[i])
			break;
	if (i < 0)
		i = 0;
	for (j = 0; j <= i; j++)
		printf("%d ", lp->d_drivedata[j]);
	printf("\n\n%d partitions:\n", lp->d_npartitions);
	printf("#        size   offset    fstype   [fsize bsize   cpg]\n");
	pp = lp->d_partitions;
	for (i = 0; i < lp->d_npartitions; i++, pp++) {
		if (pp->p_size) {
			printf("  %c: %d %d  ", 'a' + i,
			   pp->p_size, pp->p_offset);
			if ((unsigned) pp->p_fstype < FSMAXTYPES)
				printf("%s", fstypenames[pp->p_fstype]);
			else
				printf("%d", pp->p_fstype);
			switch (pp->p_fstype) {

			case FS_UNUSED:				/* XXX */
				printf("    %d %d %s ",
				    pp->p_fsize, pp->p_fsize * pp->p_frag, "");
				break;

			case FS_BSDFFS:
				printf("    %d %d %d ",
				    pp->p_fsize, pp->p_fsize * pp->p_frag,
				    pp->p_cpg);
				break;

			default:
				printf("%s", "");
				break;
			}
			printf("\t# (Cyl. %d",
			    pp->p_offset / lp->d_secpercyl);
			if (pp->p_offset % lp->d_secpercyl)
			    cnputc('*');
			else
			    cnputc(' ');
			printf("- %d",
			    (pp->p_offset + 
			    pp->p_size + lp->d_secpercyl - 1) /
			    lp->d_secpercyl - 1);
			if (pp->p_size % lp->d_secpercyl)
			    cnputc('*');
			printf(")\n");
		}
	}
}
