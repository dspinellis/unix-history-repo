/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Steven McCanne of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)remote-sl.h	7.4 (Berkeley) %G%
 *
 * $Header: remote-sl.h,v 1.2 92/07/23 19:38:20 mccanne Exp $ (LBL)
 */

#define FRAME_START		0xc1		/* Frame End */
#define FRAME_END		0xc0		/* Frame End */
#define FRAME_ESCAPE		0xdb		/* Frame Esc */
#define TRANS_FRAME_START	0xde		/* transposed frame start */
#define TRANS_FRAME_END		0xdc		/* transposed frame esc */
#define TRANS_FRAME_ESCAPE	0xdd		/* transposed frame esc */

/*
 * Message limits.  SL_MAXDATA is the maximum number of bytes that can
 * be read or written.  SL_RPCSIZE is the maximum message size for
 * the serial link.  The actual MTU is two times the max message (since
 * each byte might be escaped), plus the two framing bytes.  We add two 
 * to the message length to account for the type byte and checksum.
 */
#define SL_MAXDATA 62			/* max data that can be read */
#define SL_RPCSIZE (1 + SL_MAXDATA)	/* errno byte + data */
#define SL_MTU ((2 * (SL_RPCSIZE + 2) + 2))

