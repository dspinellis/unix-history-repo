/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: dkio.h,v 4.300 91/06/09 06:38:02 root Rel41 $ SONY
 *
 *	@(#)dkio.h	8.1 (Berkeley) 6/11/93
 */

#ifndef __DKIO__
#define __DKIO__ 1

#define	DKIOCGGEOM	_IOR('d', 0, struct dkst)	/* get geometry info */
#define	DKIOCSGEOM	_IOW('d', 1, struct dkst)	/* set geometry info */
#define	DKIOCGPART	_IOR('d', 2, struct Partinfo)	/* get partition info */
#define	DKIOCSPART	_IOW('d', 3, struct Partinfo)	/* set partition info */
#define	DKIOCGCHAN	_IOR('d', 4, int)		/* get drive channel# */
#define	DKIOCGUNIT	_IOR('d', 5, int)		/* get drive unit# */
#define	DKIOCSEEK	_IOW('d', 6, int)		/* seek logical block */
#define	DKIOCRGEOM	_IOW('d', 7, int)		/* reset geom info */
#define		RGEOM_SDINFO	0	/* reset geom info */
#define		RGEOM_WDINFO	1	/* reset geom info & write to disk */

#define	DKIOCRSEC0	_IOW('d', 8, char *)	/* read sector #0 */
#define	DKIOCWSEC0	_IOW('d', 9, char *)	/* write sector #0 */
#define	DKIOCRBOOT0	DKIOCRSEC0		/* read sector #0 */
#define	DKIOCWBOOT0	DKIOCWSEC0		/* write sector #0 */
#define	DKIOCRBOOT1	_IOW('d',10, char *)	/* read sector #1~#15 */
#define	DKIOCWBOOT1	_IOW('d',11, char *)	/* write sector #1~#15 */
#define	DKIOCRBOOT	_IOW('d',12, char *)	/* read sector #0~#15 */
#define	DKIOCWBOOT	_IOW('d',13, char *)	/* write sector #0~#15 */

#endif /* !__DKIO__ */
