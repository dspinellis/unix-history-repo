/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)register_proto.h	8.1 (Berkeley) %G%
 */

#define	APPEND_DB	0x01
#define	ABORT		0x02

#define	GOTKEY_MSG	"GOTKEY"

struct	keyfile_data {
	C_Block		kf_key;
};
