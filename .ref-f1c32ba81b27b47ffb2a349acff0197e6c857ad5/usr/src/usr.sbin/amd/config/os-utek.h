/* $Id: os-utek.h,v 5.2 91/03/03 20:22:54 jsp Alpha $ */

/*
 * Utek 4.0  definitions for Amd (automounter)
 *	from Bill Trost <trost%reed@cse.ogi.edu>
 *
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
 *	@(#)os-utek.h	5.2 (Berkeley) %G%
 */

#define	UTEK
#define	__NFS_HEADER__		/* prevent re-inclusion of <sys/nfs.h> */
/* ... and fake the rest */
#include "os-sos3.h"
