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
 *	@(#)os-riscix.h	5.4 (Berkeley) %G%
 *
 * $Id: os-riscix.h,v 5.2.2.1 1992/02/09 15:10:38 jsp beta $
 *
 * Acorn Archimedes RISC iX definitions for Amd (automounter)
 * Contributed by Piete Brooks.
 */

/*
 * Does the compiler grok void *
 */
#define	VOIDP

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
#undef	ARCH_ENDIAN
#define	ARCH_ENDIAN "little"

/*
 * Is the mount table mirrored in software
 */
#define	UPDATE_MTAB

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_NFS	MOUNT_NFS
#define	MOUNT_TYPE_UFS	MOUNT_UFS

#undef	MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	MNTTYPE_43
