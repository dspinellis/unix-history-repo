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
 *	@(#)os-concentrix.h	5.4 (Berkeley) %G%
 *
 * $Id: os-concentrix.h,v 5.2.2.1 1992/02/09 15:10:14 jsp beta $
 *
 * Alliant Concentrix 5.0.0 definitions for Amd (automounter)
 */

/*
 * Does the compiler grok void *
 */
#undef	VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_3

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	NFS_3

/*
 * Does this OS have NDBM support?
 */
#define OS_HAS_NDBM

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define	ARCH_ENDIAN	"big"

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_NFS	MOUNT_NFS
#define MOUNT_TYPE_UFS	MOUNT_UFS
