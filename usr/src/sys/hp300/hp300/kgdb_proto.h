/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * @(#) $Header: kgdb_proto.h,v 1.3 91/03/12 22:06:43 mccanne Exp $ (LBL)
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
