/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vis.h	5.1 (Berkeley) %G%
 */

/*
 * cencode flags
 */
#define	CENC_CSTYLE	1	/* use C-style backslash sequences */
#define	CENC_GRAPH	2	/* encode as ^? and M- sequences */
#define	CENC_OCTAL	4	/* encode as octal sequences */
#define	CENC_ALL	(CENC_CSTYLE|CENC_GRAPH|CENC_OCTAL)
#define	CENC_WHITE	8	/* encode white-space chars (sp-tb-nl) */
#define	CENC_RACHAR	0x10	/* look at read-ahead char */

/*
 * cdecode flags
 */
#define	CDEC_HAT	1	/* treat ^ as special */
#define	CDEC_END	2	/* reset decode machine */

/*
 * cdecode return codes
 */
#define	CDEC_NEEDMORE	0	/* not done, feed another char */
#define	CDEC_OK		1	/* done, character ready */
#define	CDEC_OKPUSH	2	/* done, push back passed char */
#define	CDEC_NOCHAR	3	/* done, no character ready (hidden newline) */
#define	CDEC_SYNBAD	-1	/* unrecognized escape sequence */

extern char *cencode();
