/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)fsdump.c	8.1 (Berkeley) 6/10/93
 */

/*
 * fsdump.c -- dump and restore of File System
 * by A.Fujita, APR-26-1992
 */

#include <sys/param.h>
#include <sys/stat.h>
#define DKTYPENAMES
#include <sys/disklabel.h>
#include <luna68k/stand/status.h>
#include <luna68k/stand/omron_disklabel.h>

#define LABEL_SIZE 512

#define BUF_BLOCK	(20 * 12 * 38)			/* 20 Cylinder */
#define BUF_BYTES	BUF_BLOCK << DEV_BSHIFT

static u_char index[LABEL_SIZE];

struct disklabel *lp  = (struct disklabel *)((struct scd_dk_label *) index)->dkl_pad;

extern dev_t  rst0;
extern dev_t nrst0;

static u_char *dump_buf = (u_char *) 0x100000;

extern int scsi_device;
char cons_buf[100];


int
fsdump(argc, argv)
	int   argc;
	char *argv[];
{
	register int i, j, io;
	register char *p;
	register int status;
	register int block, bytes;
	int scsi_id, blk, nblks, size, mark;
	struct stat boot_stat;
	struct	partition *pp;
	scsi_id = scsi_device;

	printf("Current SCSI device = ID %d\n", scsi_id);
	getline("Is it sure ? (y/n) ", cons_buf);

	if ((cons_buf[0] != 'y') && (cons_buf[0] != 'Y'))
		return(ST_ERROR);

	scsi_read_raw(scsi_id, 0, 1, index, LABEL_SIZE);

	for (i = 0; i < MAXPARTITIONS; i++) {
		pp = &(lp->d_partitions[i]);
		if ((i != 0) &&
		    (i != 3) &&
		    (i != 4) &&
		    (i != 5)) {
			pp->p_size = 0;
		}
		if (i == 5 && argc > 1 && !strcmp(argv[1], "tailor"))
			pp->p_size = 0;
	}

	st_rewind(rst0);

	printf("Boot Program		");
	io = open("sd(0,0)boot", 0);
	if (io >= 0) {
		printf("read ... ");
		size = read(io, dump_buf, 1048576);
		close(io);
		printf("%d bytes ... ", size);
		if (size <= 0) {
			printf("failed\n");
			return(ST_ERROR);
		}
		boot_stat.st_size = size;
	}

	printf("write ... ");
	status = stwrite(rst0, dump_buf, size);
	st_write_EOF(rst0);

	if (status < size) {
		printf("failed\n");
		return(ST_ERROR);
	}

	printf("done\n");

	printf("disklabel (index)\t");

	printf("write ... ");
	status = stwrite(rst0, index, LABEL_SIZE);
	st_write_EOF(rst0);

	if (status < LABEL_SIZE) {
		printf("failed\n");
		return(ST_ERROR);
	}

	printf("done\n\n");

	for (i = 0; i < MAXPARTITIONS; i++) {
		pp = &(lp->d_partitions[i]);
		if (pp->p_size > 0) {
			printf("%c: ", i + 'A');
			printf("size = %d(0x%s), ", pp->p_size, hexstr(pp->p_size, 8));
			printf("offset = %d(0x%s)\n", pp->p_offset, hexstr(pp->p_offset, 8));
			
			blk   = pp->p_offset;
			nblks = pp->p_size;
			size  = nblks << DEV_BSHIFT;

			block = BUF_BLOCK;
			bytes = BUF_BYTES;

			mark = nblks / block;
			if (nblks % block)
				mark++;
			for (j = 0; j < mark; j++)
				printf("-");
			for (j = 0; j < mark; j++)
				printf("%c", '\x08');

			while (nblks > 0) {
				if (nblks < block) {
					block = nblks;
					bytes = nblks << DEV_BSHIFT;
				}

				if (!scsi_read_raw(scsi_id, blk, block, dump_buf, bytes)) {
					printf("disk read failed !!!\n");
					return(ST_ERROR);
				}

				if (stwrite(rst0, dump_buf, bytes) < bytes) {
					printf("tape write failed !!!\n");
					return(ST_ERROR);
				}

				blk   += block;
				nblks -= block;
				size  -= bytes;

				printf("#");
			}

			st_write_EOF(rst0);
			printf("\n\n");
		}
	}
}

int
fsrestore(argc, argv)
	int   argc;
	char *argv[];
{
	register int i, j, status;
	register int block, bytes;
	int blk, nblks, size, mark;
	struct	partition *pp;

	printf("Current SCSI device = ID %d\n", scsi_device);
	getline("Is it sure ? (y/n) ", cons_buf);

	if ((cons_buf[0] != 'y') && (cons_buf[0] != 'Y'))
		return(ST_ERROR);

	st_rewind(rst0);

	st_skip(rst0);

	status = stread(rst0, index, LABEL_SIZE);

	st_skip(rst0);

	for (i = 0; i < MAXPARTITIONS; i++) {
		pp = &(lp->d_partitions[i]);
		if (pp->p_size > 0) {
			printf("%c: ", i + 'A');
			printf("size = %d(0x%s), ", pp->p_size, hexstr(pp->p_size, 8));
			printf("offset = %d(0x%s)\n", pp->p_offset, hexstr(pp->p_offset, 8));
			
			blk   = pp->p_offset;
			nblks = pp->p_size;
			size  = nblks << DEV_BSHIFT;

			block = BUF_BLOCK;
			bytes = BUF_BYTES;

			mark = nblks / block;
			if (nblks % block)
				mark++;
			for (j = 0; j < mark; j++)
				printf("-");
			for (j = 0; j < mark; j++)
				printf("%c", '\x08');

			while (nblks > 0) {
				if (nblks < block) {
					block = nblks;
					bytes = nblks << DEV_BSHIFT;
				}

				if (stread(rst0, dump_buf, bytes) != bytes) {
					printf("tape read failed !!!\n");
					return(ST_ERROR);
				}

				if (!scsi_write(blk, dump_buf, bytes)) {
					printf("disk write failed !!!\n");
					return(ST_ERROR);
				}

				blk   += block;
				nblks -= block;
				size  -= bytes;

				printf("#");
			}
			st_skip(rst0);
			printf("\n\n");
		}
	}
}
