/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pmioctl.h	7.1 (Berkeley) %G%
 *
 * graphics.h --
 *
 *     	Defines for the graphics device.
 *
 * Copyright (C) 1989 by Digital Equipment Corporation, Maynard MA
 *
 *			All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in 
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  
 *
 * Digitial disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event shall
 * Digital be liable for any special, indirect or consequential damages or
 * any damages whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious action,
 * arising out of or in connection with the use or performance of this
 * software.
 *
 * from: $Header: devSerialPmax.c,
 *	v 1.4 89/05/22 13:31:07 mnelson Exp $ SPRITE (DECWRL)
 */

#include <sys/ioctl.h>

/*
 * Events.
 */
typedef struct {
        short	        x;		/* x position */
        short 	        y;		/* y position */
        unsigned int    time;		/* 1 millisecond units */
        unsigned char   type;		/* button up/down/raw or motion */
        unsigned char   key;		/* the key (button only) */
        unsigned char   index;		/* which instance of device */
        unsigned char   device;		/* which device */
} pmEvent;

/*
 * type field
 */
#define BUTTON_UP_TYPE          0
#define BUTTON_DOWN_TYPE        1
#define BUTTON_RAW_TYPE         2
#define MOTION_TYPE             3

/*
 * device field
 */
#define NULL_DEVICE		0	/* NULL event (for QD_GETEVENT ret) */
#define MOUSE_DEVICE		1	/* mouse */
#define KEYBOARD_DEVICE		2	/* main keyboard */
#define TABLET_DEVICE		3	/* graphics tablet */
#define AUX_DEVICE		4	/* auxiliary */
#define CONSOLE_DEVICE		5	/* console */
#define KNOB_DEVICE		8
#define JOYSTICK_DEVICE		9

#define PM_MAXEVQ		64	/* must be power of 2 */
#define PM_EVROUND(x)		((x) & (PM_MAXEVQ - 1))
#define MOTION_BUFFER_SIZE	100

typedef struct {
	unsigned int	time;
	short		x, y;
} pmTimeCoord;

/*
 * The event queue. This structure is normally included in the info
 * returned by the device driver.
 */
typedef struct {
	pmEvent 	*events;
	unsigned int 	eSize;
        unsigned int    eHead;
        unsigned int    eTail;
	unsigned	long	timestamp_ms;
	pmTimeCoord	*tcs;	/* history of pointer motions */
	unsigned int	tcSize;
	unsigned int	tcNext;	/* simple ring buffer, old events are tossed */
} pmEventQueue;

/*
 * mouse cursor position
 */
typedef struct {
        short	x;
        short	y;
} pmCursor;

/*
 * mouse motion rectangle
 */
typedef struct {
        short	bottom;
        short	right;
        short	left;
        short	top;
} pmBox;

/*
 * Structures used by ioctl's.
 */
typedef struct pm_kpcmd {
	char nbytes;		/* number of bytes in parameter */
	unsigned char cmd;	/* command to be sent, peripheral bit will */
				/* be forced by driver */
	unsigned char par[2];	/* bytes of parameters to be sent */
} pmKpCmd;

typedef struct pm_info {
	pmEventQueue qe;		/* event & motion queues	*/
	short	mswitches;		/* current value of mouse buttons */
	pmCursor tablet;		/* current tablet position	*/
	short	tswitches;		/* current tablet buttons NI!	*/
	pmCursor cursor;		/* current cursor position	*/
	short	row;			/* screen row			*/
	short	col;			/* screen col			*/
	short	max_row;		/* max character row		*/
	short	max_col;		/* max character col		*/
	short	max_x;			/* max x position		*/
	short	max_y;			/* max y position		*/
	short	max_cur_x;		/* max cursor x position 	*/
	short	max_cur_y;		/* max cursor y position	*/
	int	version;		/* version of driver		*/
	char	*bitmap;		/* bit map position		*/
        short   *scanmap;               /* scanline map position        */
	short	*cursorbits;		/* cursor bit position		*/
	short	*pmaddr;		/* virtual address           	*/
	char    *planemask;		/* plane mask virtual location  */
	pmCursor mouse;			/* atomic read/write		*/
	pmBox	mbox;			/* atomic read/write		*/
	short	mthreshold;		/* mouse motion parameter	*/
	short	mscale;			/* mouse scale factor (if 
					   negative, then do square).	*/
	short	min_cur_x;		/* min cursor x position	*/
	short	min_cur_y;		/* min cursor y position	*/
} PM_Info;

typedef struct {
	short		Map;
	unsigned short	index;
	struct {
		unsigned short red;
		unsigned short green;
		unsigned short blue;
	} Entry;
} ColorMap;

/*
 * CAUTION:
 *	The numbers of these ioctls must match
 *	the ioctls in qvioctl.h
 */
#define QIOCGINFO 	_IOR('q', 1, struct pm_info *)	/* get the info	 */
#define QIOCPMSTATE	_IOW('q', 2, pmCursor)		/* set mouse pos */
#define	QIOWCURSORCOLOR	_IOW('q', 3, unsigned int [6])	/* bg/fg r/g/b */
#define QIOCINIT	_IO('q', 4)			/* init screen   */
#define QIOCKPCMD	_IOW('q', 5, struct pm_kpcmd)	/* keybd. per. cmd */
#define QIOCADDR	_IOR('q', 6, struct pm_info *)	/* get address */
#define	QIOWCURSOR	_IOW('q', 7, short [32])	/* write cursor bit map */
#define QIOKERNLOOP	_IO('q', 8)   /* re-route kernel console output */
#define QIOKERNUNLOOP	_IO('q', 9)   /* don't re-route kernel console output */
#define QIOVIDEOON	_IO('q', 10)			/* turn on the video */
#define	QIOVIDEOOFF	_IO('q', 11)			/* turn off the video */
#define QIOSETCMAP      _IOW('q', 12, ColorMap)
