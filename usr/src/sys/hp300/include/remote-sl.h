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
 * @(#) $Header: remote-sl.h,v 1.3 91/03/22 15:34:27 mccanne Exp $ (LBL)
 */

#define FRAME_END		0xc0		/* Frame End */
#define FRAME_ESCAPE		0xdb		/* Frame Esc */
#define TRANS_FRAME_END		0xdc		/* transposed frame end */
#define TRANS_FRAME_ESCAPE	0xdd		/* transposed frame esc */

/*
 * Error codes.
 */
#define EKGDB_CSUM	1
#define EKGDB_2BIG	2
#define EKGDB_RUNT	3

/*
 * Message limits.  SL_MAXMSG is the longest message that can be passed
 * down to the serial link.  The actual MTU is two times the max message
 * (since each byte might be escaped), plus the two framing bytes.  We add 
 * two to the message length to account for the type byte and check sum.
 * SL_BUFSIZE is one character larger than SL_MAXMSG so we can stuff
 * a checksum into the input buffer without special casing.
 */
#define SL_MAXMSG 64
#define SL_BUFSIZE (SL_MAXMSG + 1)
#define SL_MTU ((2 * (SL_MAXMSG + 2) + 2))

