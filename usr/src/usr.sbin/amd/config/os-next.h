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
 *	@(#)os-next.h	5.5 (Berkeley) %G%
 *
 * $Id: os-next.h,v 5.2.2.1 1992/02/09 15:10:33 jsp beta $
 *
 * NeXT OS definitions for Amd (automounter)
 * By Bill Trost, Reed College
 * trost%reed@cse.ogi.edu,
 *
 * Derived from the Sun 3.2 definitions for Amd (os-sos3.h).
 */

/*
 * Does the compiler grok void *	(NeXT uses gcc)
 */
#define VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define RPC_3

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define NFS_3

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_UFS	MOUNT_UFS
#define MOUNT_TYPE_NFS	MOUNT_NFS
#undef MTAB_TYPE_UFS
#define MTAB_TYPE_UFS	"4.3"

/*
 * Where to get NFS definitions
 */
#define NFS_HDR "misc-next.h"
