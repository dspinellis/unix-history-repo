#ifndef lint
static char *rcsid_display_c = "$Header: display.c,v 10.2 86/12/17 18:04:40 swick Exp $";
#endif	lint
/* display.c - Routines to initialize screen, and allocate space??
 *
 *	OpenDisplay		Open it
 *	InitDisplay		Do display initialization
 *	DisplayDead		Check if dead
 *	AllocateSpace		Allocate some temporary storage
 *
 *  	Author:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"

#ifdef APA16
#include "apa16.h"
#endif APA16

#ifdef AED
#include "aed.h"
#endif AED

#ifdef APA8
#include "apa8.h"
#endif APA8

#ifdef APA8C
#include "apa8c.h"
#endif APA8C

#ifdef PQD
#include "pqd.h"
#endif PQD

/*
 * Open the display
 */

/*ARGSUSED*/
OpenDisplay (xNumber)
	char *xNumber;
{
	char *MouseType = getenv("MOUSETYPE");
	char *MouseName = getenv("MOUSENAME");
	extern int mdev; 
	int emul;

#ifdef TRACE_X
	fprintf(stderr, "In OpenDisplay\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Open Minor device for correct screen and switch
	 * to that screen
	 */

	if ((xdev = open (SCREEN_DEVICE, O_RDWR|O_NDELAY)) < 0) {
		fprintf(stderr, "Unable to open device (%s)\n", SCREEN_DEVICE);
		fflush(stderr);
		exit(2);
	}


	/*
	 * Get mouse device name
	 */

	if (!MouseName)
		MouseName = MOUSE_DEVICE;

	/*
	 * Open mouse 
	 */


	if ((mdev = open (MouseName, O_RDWR|O_NDELAY)) < 0) {
		fprintf  (stderr, "Error in open of (%s)\n", MouseName);
		fflush(stderr);
		exit (2);
	}

	/*
	 * Set X Input and Output Emulators for this display
	 */

	emul = E_XINPUT;
	if (ioctl (xdev, EISETD, (caddr_t)&emul) < 0) {
		fprintf(stderr, "Unable to set E_XINPUT (%s)\n",SCREEN_DEVICE);
		fflush(stderr);
		exit(2);
	}
	emul = E_XOUTPUT;
	if (ioctl (xdev, EOSETD, (caddr_t)&emul) < 0) {
		fprintf(stderr, "Unable to set E_XOUTPUT (%s)\n",SCREEN_DEVICE);
		fflush(stderr);
		exit(2);
	}

	/*
	 * Gain access to I/O address space
	 */

	if (open ("/dev/bus", O_RDONLY|O_NDELAY) < 0) {
                DeviceError ("Unable to open /dev/bus\n");
		exit(2);
	}

	/*
	 * Return displays file descriptor
	 */

	return(xdev);
}

/*
 * Do display specific initialization
 */

InitDisplay (info)
	register DEVICE *info;
{
#ifdef TRACE_X
	fprintf(stderr, "In InitDisplay\n");
	fflush(stderr);
#endif TRACE_X

	/*
	 * Get address of shared memory
	 */

	if (ioctl(xdev, QIOCADDR, (caddr_t)&XAddr) < 0) { 
		DeviceError("Ioctl to set XAddr failed");
		exit(2);
	}

        /*
         * Initialize (reset) the display
         */

	DISPLAY_INIT();
 
	/*
	 * Initialize bitblt() 
	 */

	bitblt_init(SCREEN_BASE, REAL_SCREEN_WIDTH,
		 REAL_SCREEN_HEIGHT, CURSOR_TYPE);

        /*
         * Set up the physical bitmap structure. This is used
	 * by device dependent code to talk to bitblt().
         */
 
        pbm.width = REAL_SCREEN_WIDTH;
        pbm.height = REAL_SCREEN_HEIGHT;
        pbm.refcnt = 1;
        pbm.data = (caddr_t) SCREEN_BASE;
 
	/*
	 * Fill in device and queue information
	 */

	info->id = XDEV_ID;
	info->width = X_SCREEN_WIDTH;
	info->height = X_SCREEN_HEIGHT;
	info->planes = 1;
	info->entries = 0;
	info->mouse = (vsCursor *)(&(XAddr->mouse));
	info->mbox  = (vsBox *) (&(XAddr->mbox));
	info->queue = (vsEventQueue *) (&XAddr->ibuff);

	return (0);
}

/*
 * Check if display is dead
 */

DisplayDead ()
{
#ifdef TRACE_X
	fprintf(stderr, "In DisplayDead\n");
	fflush(stderr);
#endif TRACE_X

	return(0);
}

/*
 * The presumption here is that only one AllocateSpace
 * call is made/request
 */

#define ABUFSIZE 3072
static char ABuffer[3072];	/* random size buffer for allocate space */

caddr_t AllocateSpace (size)
	register int size;
{
#ifdef TRACE_X
	fprintf(stderr, "In AllocateSpace\n");
	fflush(stderr);
#endif TRACE_X

	if (size < ABUFSIZE)
		return(ABuffer);
	errno = ENOMEM;
	return (NULL);
}
