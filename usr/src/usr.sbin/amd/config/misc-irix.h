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
 *	@(#)misc-irix.h	5.3 (Berkeley) %G%
 *
 * $Id: misc-irix.h,v 5.2.1.2 91/05/07 22:19:48 jsp Alpha $
 *
 */

#include <sys/fs/nfs_clnt.h>
#include <sys/fsid.h>
#include <sys/fstyp.h>

struct ufs_args {
	char *fspec;
};
