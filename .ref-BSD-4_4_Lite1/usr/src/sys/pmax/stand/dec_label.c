/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
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
 *	@(#)dec_label.c	8.1 (Berkeley) 6/10/93
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/disklabel.h>

#include <pmax/stand/dec_boot.h>

struct	disklabel label;
struct	Dec_DiskLabel dec_label;

/*
 * This program creates or updates the DEC label after 'disklabel'
 * has initialized the Berkeley disk label.
 * This program may be useful in sharing ULTRIX disks with 4.4 BSD but it
 * hasn't been tested much. Use at your own risk. 
 *
 * Usage: dec_label <disk>
 */

main(argc, argv)
	int argc;
	char *argv[];
{
	register int i;
	int fd;

	if (argc != 2) {
		fprintf(stderr, "Usage: dec_label <disk>\n");
		exit(1);
	}
	if ((fd = open(argv[1], O_RDWR, 0)) < 0)
		err(1, "%s", argv[1]);
	if (ioctl(fd, DIOCGDINFO, &label) < 0)
		err(1, "ioctl DIOCGDINFO");

	/* fill in DEC label */
	dec_label.magic = DEC_LABEL_MAGIC;
	dec_label.isPartitioned = 1;
	for (i = 0; i < DEC_NUM_DISK_PARTS; i++) {
		dec_label.map[i].numBlocks = label.d_partitions[i].p_size;
		dec_label.map[i].startBlock = label.d_partitions[i].p_offset;
		printf("%d: start %d size %d\n", i, dec_label.map[i].startBlock,
			dec_label.map[i].numBlocks);
	}
	if (lseek(fd, (off_t)DEC_LABEL_SECTOR * label.d_secsize, SEEK_SET) == -1)
		err(1, "lseek");
	if (write(fd, &dec_label, sizeof(dec_label)) != sizeof(dec_label))
		err(1, "write label");
	return (0);
}
