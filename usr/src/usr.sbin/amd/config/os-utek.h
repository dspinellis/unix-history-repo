/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)os-utek.h	5.4 (Berkeley) %G%
 *
 * $Id: os-utek.h,v 5.2.2.1 1992/02/09 15:10:56 jsp beta $
 *
 * Utek 4.0  definitions for Amd (automounter)
 *	from Bill Trost <trost%reed@cse.ogi.edu>
 */

#define	UTEK
#define	__NFS_HEADER__		/* prevent re-inclusion of <sys/nfs.h> */
/* ... and fake the rest */
#include "os-sos3.h"
