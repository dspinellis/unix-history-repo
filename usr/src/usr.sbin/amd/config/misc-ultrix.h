/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)misc-ultrix.h	5.4 (Berkeley) %G%
 *
 * $Id: misc-ultrix.h,v 5.2.2.1 1992/02/09 15:10:49 jsp beta $
 *
 */

#include        <nfs/nfs_gfs.h>
#define KERNEL
#include        <sys/fs_types.h>
#undef  KERNEL

#ifndef HOSTNAMESZ
#include <nfs/nfs_clnt.h>
#endif

#include <ufs/ufs_mount.h>

#define	ufs_args ufs_specific
