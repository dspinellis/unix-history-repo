/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Steven McCanne of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kgdb_proto.h	7.1 (Berkeley) %G%
 *
 * $Header: kgdb_proto.h,v 1.3 91/03/12 22:06:43 mccanne Exp $ (LBL)
 */

/*
 * Message types.
 */
#define KGDB_MEM_R	0x01
#define KGDB_MEM_W	0x02
#define KGDB_REG_R	0x03
#define KGDB_REG_W	0x04
#define KGDB_CONT	0x05
#define KGDB_STEP	0x06
#define KGDB_KILL	0x07
#define KGDB_SIGNAL	0x08
#define KGDB_EXEC	0x09

#define KGDB_CMD(x) ((x) & 0x0f)

/*
 * Message flags.
 */
#define KGDB_ACK	0x80
#define KGDB_DELTA	0x40
#define KGDB_MORE	0x20
#define KGDB_SEQ	0x10
