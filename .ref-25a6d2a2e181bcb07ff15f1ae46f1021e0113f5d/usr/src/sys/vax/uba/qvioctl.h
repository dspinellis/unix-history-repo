/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * 		@(#)qvioctl.h	1.4  Berkeley  %G%
 * 
 *	derived from: @(#)qvioctl.h	1.7 (ULTRIX) 6/11/85
 */
/*
 * Ioctl definitions for the qvss.
 */
#ifdef KERNEL
#include "ioctl.h"
#include "qvreg.h"
#include "qevent.h"
#else
#include <sys/ioctl.h>
#include <vaxuba/qvreg.h>
#include <vaxuba/qevent.h>
#endif

struct qv_kpcmd {
	char nbytes;		/* number of bytes in parameter */
	unsigned char cmd;	/* command to be sent, peripheral bit will */
				/* be forced by driver */
	unsigned char par[2];	/* bytes of parameters to be sent */
};
/*
 * qvss information block
 */

struct qv_info {
	short	mswitches;		/* current value of mouse buttons */
	vsCursor tablet;		/* current tablet position	*/
	short	tswitches;		/* current tablet buttons NI!	*/
	vsCursor cursor;		/* current cursor position	*/
	short	row;			/* screen row			*/
	short	col;			/* screen col			*/
	short	max_row;		/* max character row		*/
	short	max_col;		/* max character col		*/
	short	max_x;			/* max x position		*/
	short	max_y;			/* max y position		*/
	short	max_cur_x;		/* max cursor y position 	*/
	short	max_cur_y;		/* max cursor y position	*/
	char	*bitmap;		/* bit map position		*/
	short	*scanmap;		/* scanline map position	*/
	short	*cursorbits;		/* cursor bit position		*/
	struct	qvdevice *qvaddr;	/* virtual address of the csr	*/
	vsEvent *ibuff;			/* pointer to event queue	*/
	int 	iqsize;			/* may assume power of two 	*/
	int 	ihead;			/* atomic write			*/
	int 	itail;			/* atomic read			*/
	vsCursor mouse;			/* atomic read/write		*/
	vsBox	mbox;			/* atomic read/write		*/
	short	mthreshold;		/* mouse motion parameter	*/
	short	mscale;			/* mouse scale factor (if 
					   negative, then do square).	*/
};
typedef struct qv_info vsIoAddr;

#define QIOCGINFO 	_IOR('q', 1, struct qv_info)	/* get the info	 */
#define QIOCSMSTATE	_IOW('q', 2, vsCursor)		/* set mouse pos */
#define QIOCINIT	_IO('q', 4)			/* init screen   */
#define QIOCKPCMD	_IOW('q', 5, struct qv_kpcmd)	/* keybd. per. cmd */
#define QIOCADDR	_IOR('q', 6, struct qv_info *)	/* get address */

