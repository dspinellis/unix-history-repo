/* $Id: os-convex.h,v 5.2 90/06/23 22:20:42 jsp Rel $ */

/*
 * Convex C220, version 7.1 definitions for Amd (automounter)
 *         from Eitan Mizrotsky <eitan@shum.huji.ac.il>
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
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
 *	@(#)os-convex.h	5.1 (Berkeley) 6/29/90
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
#define	ARCH_ENDIAN	"big"
 
/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_UFS	MOUNT_UFS
#define MOUNT_TYPE_NFS	MOUNT_NFS
 
 
#define strrchr rindex
#define strchr  index
