/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
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
 *	@(#)scsi_ioctl.h	8.1 (Berkeley) 6/10/93
 *
 * from: $Header: scsi_ioctl.h,v 1.3 92/12/02 03:54:19 torek Exp $ (LBL)
 */

/*
 * SCSI ioctls (`format' mode).
 *
 * Format mode allows a privileged process to issue direct SCSI commands
 * to a drive (it is intended primarily to allow on-line formatting).
 * SDIOCSFORMAT sets format mode (nonzero arg => on, zero arg => off).
 * When in format mode, only the process that issued the SDIOCSFORMAT
 * can read or write the drive.
 *
 * In format mode, the process is expected to
 *	- do SDIOCSCSICOMMAND to supply cdb for next SCSI op
 *	- do read or write as appropriate for cdb
 *	- if I/O error, optionally do SDIOCSENSE to get completion
 *	  status and sense data from last SCSI operation.
 */

struct scsi_fmt_sense {
	u_int	status;		/* completion status of last op */
	u_char	sense[28];	/* sense data (if any) from last op */
};

#define	SDIOCSFORMAT		_IOW('S', 1, int)
#define	SDIOCGFORMAT		_IOR('S', 2, int)
#define	SDIOCSCSICOMMAND	_IOW('S', 3, struct scsi_cdb)
#define	SDIOCSENSE		_IOR('S', 4, struct scsi_fmt_sense)
