/* $Id: os-fpx4.h,v 5.2 90/06/23 22:20:45 jsp Rel $ */

/*
 * Celerity FPX 4.1/2 definitions for Amd (automounter)
 *      from Stephen Pope <scp@grizzly.acl.lanl.gov>
 *
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
 *	@(#)os-fpx4.h	5.1 (Berkeley) %G%
 */

/*
 * FPX wants to include sys headers multiple times
 */
#define INCLUDE_HEADERS

/*
 * FPX sys/mount.h includes sys/nfs.h; prevent this
 */
#define INCLUDED_nfs

/*
 * FPX doesn't define NMOUNT anywhere
 */
#define NMOUNT 40

/*
 * Does the compiler grok void *
 */
/* #define VOIDP */

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
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define ARCH_ENDIAN     "big"

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_NFS MOUNT_NFS
#define MOUNT_TYPE_UFS MOUNT_UFS
