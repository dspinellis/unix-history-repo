/*
 * Copyright (c) 1992 University of Utah.
 * Copyright (c) 1982, 1986, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: label.c 1.1 92/12/05$
 *
 *	@(#)label.c	8.1 (Berkeley) 6/10/93
 */

/*
 * Derived from routines in ufs/ufs/ufs_disksubr.c.
 */

#include <sys/param.h>
#include <sys/disklabel.h>
#include <stand.att/saio.h>

/*
 * Attempt to read a disk label from a device using the indicated stategy
 * routine.  Returns NULL on success and an error string on failure.
 */
char *
readdisklabel(io, strat, lp)
	struct iob *io;
	int (*strat)();
	register struct disklabel *lp;
{
	struct iob liob;
	struct disklabel *dlp;
	char *msg = NULL;

	liob.i_adapt = io->i_adapt;
	liob.i_ctlr = io->i_ctlr;
	liob.i_unit = io->i_unit;
	liob.i_part = 2;
	liob.i_boff = 0;
	liob.i_cyloff = 0;
	liob.i_bn = LABELSECTOR;
	liob.i_ma = liob.i_buf;
	liob.i_cc = lp->d_secsize ? lp->d_secsize : DEV_BSIZE;
	if ((*strat)(&liob, F_READ) == -1)
		return ("I/O error");

	for (dlp = (struct disklabel *)liob.i_buf;
	     dlp <= (struct disklabel *)(liob.i_buf+DEV_BSIZE-sizeof(*dlp));
	     dlp = (struct disklabel *)((char *)dlp + sizeof(long))) {
		if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC) {
			if (msg == NULL)
				msg = "no disk label";
		} else if (dlp->d_npartitions > MAXPARTITIONS || dkcksum(dlp))
			msg = "disk label corrupted";
		else {
			*lp = *dlp;
			msg = NULL;
			break;
		}
	}
	return (msg);
}

/*
 * Compute checksum for disk label.
 */
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
