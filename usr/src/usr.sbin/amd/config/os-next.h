/* $Id: os-next.h,v 5.2.1.1 90/10/21 22:31:00 jsp Exp $ */

/*
 * NeXT OS definitions for Amd (automounter)
 *
 * By Bill Trost, Reed College
 * trost%reed@cse.ogi.edu,
 *
 * Derived from the Sun 3.2 definitions for Amd (os-sos3.h).
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Imperial College of Science, Technology and Medicine, London, UK.
 * The names of the College and University may not be used to endorse
 * or promote products derived from this software without specific
 * prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)os-next.h	5.1 (Berkeley) %G%
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
