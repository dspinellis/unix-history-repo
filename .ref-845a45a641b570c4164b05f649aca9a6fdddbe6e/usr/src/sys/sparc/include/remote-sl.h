/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)remote-sl.h	7.3 (Berkeley) %G%
 *
 * from: $Header: remote-sl.h,v 1.6 92/11/26 02:04:45 torek Exp $ (LBL)
 */

/*
 * These definitions are factored out into an include file so
 * the kernel stub has access to them.
 */
#define FRAME_START		0xc1		/* Frame End */
#define FRAME_END		0xc0		/* Frame End */
#define FRAME_ESCAPE		0xdb		/* Frame Esc */
#define TRANS_FRAME_START	0xde		/* transposed frame start */
#define TRANS_FRAME_END		0xdc		/* transposed frame esc */
#define TRANS_FRAME_ESCAPE	0xdd		/* transposed frame esc */

/*
 * Message limits. SL_MAXDATA is the maximum number of bytes that can
 * be read or written. SL_BUFSIZE is the maximum amount of data that
 * can be passed across the serial link. The actual MTU is two times
 * the max message (since each byte might be escaped), plus the two
 * framing bytes. We add two to the message length to account for the
 * type byte and checksum.
 */
#define SL_MAXDATA 62			/* max data that can be read */
#define SL_RPCSIZE (1 + SL_MAXDATA)	/* errno byte + data */
#define SL_MTU ((2 * (SL_RPCSIZE + 2) + 2))
