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
 *	@(#)ufs_disksubr.c	8.1 (Berkeley) 6/10/93
 */

/*
 * ufs_disksubr.c -- disk utility routines
 * by A.Fujita, FEB-26-1992
 */

#include <sys/param.h>
#include <sys/disklabel.h>
#include <luna68k/dev/scsireg.h>

extern u_char lbl_buff[];

/*
 * Attempt to read a disk label from a device
 * using the indicated stategy routine.
 * The label must be partly set up before this:
 * secpercyl and anything required in the strategy routine
 * (e.g., sector size) must be filled in before calling us.
 * Returns null on success and an error string on failure.
 */
char *
readdisklabel(dev, strat, lp)
	int dev;
	int (*strat)();
	register struct disklabel *lp;
{
	register u_char *bp = lbl_buff;
	struct disklabel *dlp;
	char *msg = NULL;
	static struct scsi_fmt_cdb cdb = {
		6,
		CMD_READ, 0, 0, 0, 1, 0
	};

	if (lp->d_secperunit == 0)
		lp->d_secperunit = 0x1fffffff;
	lp->d_npartitions = 1;
	if (lp->d_partitions[0].p_size == 0)
		lp->d_partitions[0].p_size = 0x1fffffff;
	lp->d_partitions[0].p_offset = 0;

	if (scsi_immed_command(0, dev, 0, &cdb, bp, DEV_BSIZE) != 0) {
		msg = "I/O error";
	} else {
		for (dlp = (struct disklabel *)bp;
		     dlp <= (struct disklabel *)(bp + DEV_BSIZE - sizeof(*dlp));
		     dlp = (struct disklabel *)((char *)dlp + sizeof(long))) {
			if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC) {
				if (msg == NULL)
					msg = "no disk label";
			} else if (dlp->d_npartitions > MAXPARTITIONS ||
				   dkcksum(dlp) != 0)
				msg = "disk label corrupted";
			else {
				*lp = *dlp;
				msg = NULL;
				break;
			}
		}
	}

	return (msg);
}
