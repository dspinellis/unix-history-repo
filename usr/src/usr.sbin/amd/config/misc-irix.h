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
 *	@(#)misc-irix.h	5.4 (Berkeley) %G%
 *
 * $Id: misc-irix.h,v 5.2.2.1 1992/02/09 15:10:30 jsp beta $
 *
 */

#include <sys/fs/nfs_clnt.h>
#include <sys/fsid.h>
#include <sys/fstyp.h>

struct ufs_args {
	char *fspec;
};
