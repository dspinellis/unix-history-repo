/* $Id: os-sos4.h,v 5.2 90/06/23 22:20:52 jsp Rel $ */

/*
 * SunOS 4.0 definitions for Amd (automounter)
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)os-sos4.h	5.1 (Berkeley) 6/29/90
 */

/*
 * Does the compiler grok void *
 */
#define	VOIDP

/*
 * What type is free(void*) returning?
 */
#undef FREE_RETURN_TYPE
#define FREE_RETURN_TYPE	int

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_4

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	NFS_4

/*
 * Does this OS have NDBM support?
 */
#define OS_HAS_NDBM

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#if defined(mc68010) || defined(mc68020) || defined(sparc)
#define	ARCH_ENDIAN	"big"
#endif
#if defined(i386)
#define ARCH_ENDIAN	"little"
#endif

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_NFS	"nfs"
#define MOUNT_TYPE_UFS	"4.2"

/*
 * Type of a file handle
 */
#undef NFS_FH_TYPE
#define	NFS_FH_TYPE	caddr_t

/*
 * Type of filesystem type
 */
#undef MTYPE_TYPE
#define	MTYPE_TYPE	char *

/*
 * Add support for SunOS 4 automounter files
 */
#define	SUNOS4_COMPAT
