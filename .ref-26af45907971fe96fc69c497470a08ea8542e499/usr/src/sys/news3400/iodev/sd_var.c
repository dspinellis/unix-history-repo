/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: sd_var.c,v 4.300 91/06/09 06:38:23 root Rel41 $ SONY
 *
 *	@(#)sd_var.c	7.3 (Berkeley) %G%
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
