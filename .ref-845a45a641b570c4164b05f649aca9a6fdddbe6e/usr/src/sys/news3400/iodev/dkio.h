/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: dkio.h,v 4.300 91/06/09 06:38:02 root Rel41 $ SONY
 *
 *	@(#)dkio.h	7.1 (Berkeley) %G%
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
