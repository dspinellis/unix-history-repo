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
 *	@(#)os-hcx.h	5.4 (Berkeley) %G%
 *
 * $Id: os-hcx.h,v 5.2.2.1 1992/02/09 15:10:20 jsp beta $
 *
 * Harris HCX/UX Release 3.0 definitions for Amd (automounter)
 */

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
 * Deviant call necessary.  The mount() routine in libc only works for UFS
 * (it's a backward-compatible piece of C code which traps to mountsyscall).
 */
#undef MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flags, mnt_data) \
	mountsyscall(type, mnt->mnt_dir, flags, mnt_data)

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_NFS	MOUNT_NFS
#define	MOUNT_TYPE_UFS	MOUNT_UFS

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#ifdef _hcx
#define ARCH_ENDIAN "big"
#else
XXX - bizarre!
#endif
