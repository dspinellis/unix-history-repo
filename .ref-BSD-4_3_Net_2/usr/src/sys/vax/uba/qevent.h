/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)qevent.h	7.1 (Berkeley) 5/9/91
 */

/************************************************************************
*									*
*			Copyright (c) 1985 by				*
*		Digital Equipment Corporation, Maynard, MA		*
*			All rights reserved.				*
*									*
*   This software is furnished under a license and may be used and	*
*   copied  only  in accordance with the terms of such license and	*
*   with the  inclusion  of  the  above  copyright  notice.   This	*
*   software  or  any  other copies thereof may not be provided or	*
*   otherwise made available to any other person.  No title to and	*
*   ownership of the software is hereby transferred.			*
*									*
*   The information in this software is subject to change  without	*
*   notice  and should not be construed as a commitment by Digital	*
*   Equipment Corporation.						*
*									*
*   Digital assumes no responsibility for the use  or  reliability	*
*   of its software on equipment which is not supplied by Digital.	*
*									*
************************************************************************/

/*
 * Event queue entries
 */

#ifndef _QEVENT_
#define _QEVENT_

typedef struct  _vs_event {
        unsigned short vse_x;	/* x position */
        unsigned short vse_y;	/* y position */
        unsigned short vse_time;/* 10 millisecond units (button only) */
        char    vse_type;       /* button or motion? */
        unsigned char  vse_key;	/* the key (button only) */
        char    vse_direction;	/* which direction (button only) */
        char    vse_device;	/* which device (button only) */
} vsEvent;

/* vse_type field */
#define VSE_BUTTON      0               /* button moved */
#define VSE_MMOTION     1               /* mouse moved */
#define VSE_TMOTION     2               /* tablet moved */

/* vse_direction field */
#define VSE_KBTUP       0               /* up */
#define VSE_KBTDOWN     1               /* down */
#define VSE_KBTRAW	2		/* undetermined */

/* vse_device field */
#define VSE_NULL	0		/* NULL event (for QD_GETEVENT ret) */
#define VSE_MOUSE       1               /* mouse */
#define VSE_DKB         2               /* main keyboard */
#define VSE_TABLET      3               /* graphics tablet */
#define VSE_AUX         4               /* auxiliary */
#define VSE_CONSOLE     5               /* console */

/* The event queue */

typedef struct _vs_eventqueue {
	vsEvent *events;	/* input event buffer */
	int size;		/* size of event buffer */
	int head;		/* index into events */
	int tail;		/* index into events */
} vsEventQueue;

/* mouse cursor position */

typedef struct _vs_cursor {
        short x;
        short y;
} vsCursor;

/* mouse motion rectangle */

typedef struct _vs_box {
        short bottom;
        short right;
        short left;
        short top;
} vsBox;

#endif /*_QEVENT_*/
