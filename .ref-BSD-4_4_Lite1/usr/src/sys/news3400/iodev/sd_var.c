/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: sd_var.c,v 4.300 91/06/09 06:38:23 root Rel41 $ SONY
 *
 *	@(#)sd_var.c	8.1 (Berkeley) 6/11/93
 */

/*
 * Copyright (c) 1989 by SONY Corporation.
 */
/*
 *	sd_var.c - SCSI disk device driver
 *			variable definition file.
 */

#include "sd.h"
#if NSD > 0

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/disklabel.h>

#include <news3400/iodev/scsireg.h>
#include <news3400/iodev/sdreg.h>

struct iop/**/_ctlr *sdminfo[NSDC];
struct iop/**/_device *sddinfo[NSD];
struct iop/**/_device *sdip[NSDC][MAXSLAVE];

struct buf rsdbuf[NSD];			/* buffer for raw I/O */
struct buf csdbuf[NSD];			/* buffer for controll */
struct buf sdutab[NSD];			/* per drive buffers */

struct sdc_softc sdc_softc[NSDC];
struct sdd_softc sdd_softc[NSD];
u_char sd_b_openf[NSD][PNUM];
u_char sd_c_openf[NSD][PNUM];

struct scsi kernscsi[NSD];
struct sdst sdstdrv[NSD];
struct disklabel sdlabel[NSD];
struct size sdsizedrv[NSD][PNUM];

u_char sdc_rsense[NSDC][RSEN_CNT];

struct sync_param sd_sync_param[NSD];

int nsd = NSD;
int nsdc = NSDC;

#endif /* NSD > 0 */
