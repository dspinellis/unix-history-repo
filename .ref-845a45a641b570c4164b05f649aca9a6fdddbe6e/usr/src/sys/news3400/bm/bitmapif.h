/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: bitmapif.h,v 4.300 91/06/09 06:14:42 root Rel41 $ SONY
 *
 *	@(#)bitmapif.h	7.1 (Berkeley) %G%
 */

/*
 *  bitmap interface header
 */
#define	C_ON	1
#define	C_OFF	0

struct	csr_buf {
	int	csr_x;			/*XXX cursor position x	*/
	int	csr_y;			/*XXX cursor position y	*/
	lPoint	csr_p;			/*  cursor point	*/
	int	csr_sw;			/*  cursor activity	*/
	int	csr_number;		/*  which cursor use	*/
}; 
