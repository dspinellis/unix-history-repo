/* $Id: os-hlh42.h,v 5.2 90/06/23 22:20:46 jsp Rel $ */

/*
 * HLH OTS definitions for Amd (automounter)
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
 *	@(#)os-hlh42.h	5.1 (Berkeley) %G%
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
 * Byte ordering
 */
#undef ARCH_ENDIAN
#if defined(hlh)
#define	ARCH_ENDIAN	"little"
#endif

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_NFS	MOUNT_NFS
#define	MOUNT_TYPE_UFS	MOUNT_UFS

/*
 * Miscellaneous HLH 4.2 incantations
 */
#define	strchr	index
#define strrchr	rindex
#define sigmask(x)	(1 << ((x)-1))

/*
 * HLH's 4.2 needs the extra RPC definitions.
 */
#define MISC_RPC
