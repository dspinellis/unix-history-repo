/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)remote-sl.h	8.1 (Berkeley) 6/11/93
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
