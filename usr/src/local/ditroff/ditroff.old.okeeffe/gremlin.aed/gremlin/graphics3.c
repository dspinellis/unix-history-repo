/* @(#)graphics3.c	1.4	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains additional routines for implementing graphics
 * primitives for the gremlin picture editor
 */

#include "gremlin.h"
#include <sys/types.h>
#include <sys/timeb.h>
#include <signal.h>
#include <sys/time.h>

/* imports from graphics1.c */

extern GRsetwmask();
extern int curx, cury, rmask;
extern FILE *display;
extern GRClear();

/* library routines */

extern ftime();

GREnableTablet()

/*---------------------------------------------------------
 *	This routine enables the graphics tablet.
 *
 *	Results: None.
 *
 *	Side Effects:
 *	The tablet cursor is enabled on the AED.  This will cause characters
 *	to be sent over the serial line whenever a button on the cursor is
 *	pushed.  After calling this routine, the routine GrGetButton may
 *	be called to wait for the next button to be pushed.
 *
 *	Design:
 *	Note:  the AED really messes up with the cursor, because it
 *	continually overwrites CAP with cursor coordinates.  Thus the
 *	cursor must disabled before doing ANYTHING to the AED.
 *---------------------------------------------------------
 */

{
    GRsetwmask(127);   /* don't write cursor on grid layer */
    fputs("3:", display);
    (void) fflush(display);
}

GRDisableTablet()

/*---------------------------------------------------------
 *	This routine disables the graphics tablet so that other things may
 *	be done with the AED.
 *
 *	Results:	None.
 *	Side Effects:	The tablet is disabled.
 *---------------------------------------------------------
 */

{
    putc('3', display);
    putc('\0', display);
    (void) fflush(display);
    curx = cury = -1;
}


#define MINTIME 100        /* 100 milliseconds */

int
GRGetButton(stream, px, py)
FILE *stream;			/* File from which AED chars. can be read */
int *px;			/* Address of a word to hold x-coordinate */
int *py;			/* Address of word to hold y-coorinate */

/*---------------------------------------------------------
 *	This routine waits for cursor input.
 *
 *	Results:
 *	The return value is the number of the button that was pushed.
 *	If an error occurs while reading from the stream, -1 is returned.
 *
 *	Side Effects:
 *	The integers pointed to by px and py are filled in with the
 *	x- and y-coordinates of the cursor at the time the button
 *	was pushed.
 *
 *	Design:
 *	This is tricky.  The AED apparently sends bogus button data at
 *	random times.  It can usually be distinguished by the presence
 *	of an illegal button number.  We must pick out such data and throw
 *	it away.
 *
 *	For systems up to 4.1BSD (characterized by the presence of the
 *	SIGTINT signal) the routine waits for cursor input.
 *	For 4.2BSD and beyond, the routine gives an error of -4 if input
 *	is not immediately available (it expects to be called only when
 *	input is available).
 *---------------------------------------------------------
 */

{
    int button;
    struct timeval selectpoll;
    static unsigned long time, lastime = 0;
    struct timeb tp;
    static char line1[100];

    selectpoll.tv_sec = 0l;
    selectpoll.tv_usec = 0l;
    while (TRUE)
    {

#ifndef SIGTINT
        button = 1 << fileno(stream);
        if (select(20, &button, 0, 0, &selectpoll) <= 0) return -4;
#endif

	if (fgets(line1, 99, stream) == NULL) return -1;
	if (line1[0] != ':') continue;
	(void) sscanf(&(line1[1]), "%d", &button);
	if (fgets(line1, 99, stream) == NULL) return -2;
	(void) sscanf (line1, "%d", px);
	if (fgets(line1, 99, stream) == NULL) return -3;
	(void) sscanf (line1, "%d", py);

            /* wait for minimum duration before accepting   */
            /* new button to debounce the cursor            */

        ftime(&tp);
        time = 1000 * tp.time + tp.millitm;
        if ((time - lastime) < MINTIME ) continue;
        lastime = time;

	if (button == 2) return 1;
	if (button == 4) return 2;
	if (button == 8) return 3;
	if (button == 1) return 0;
    }
}



GRSetGrid(x1, y1, x2, y2, size)
int x1, y1, x2, y2, size;
/*
 *      This routine sets the grid  according to the paramaters
 * (lower left and upper right corners, and spacing) supplied.  The
 * grid is written to the appropriate memory plane but will not be 
 * displayed unless that plane has been (Read) enabled.
 */

{
	int i, start;
	POINT end1, end2;

	GRClear(gridmask);
	GRsetwmask(gridmask);
	end1.y = (float) y1;
	end2.y = (float) y2;
	start  = ((x1 + size - 1) - ((x1 + size - 1) % size));

	     /* start forces the lines to be drawn on pixel
                boundaries evenly divisible by size           */

	for (i=start; i<=x2; i+=size)
	{
		end1.x = end2.x = (float) i;
		GRVector(&end1, &end2, gridstyle);
	}  /* end for */;
	end1.x = (float) x1;
	end2.x = (float) x2;
	start  = ((y1 + size - 1) - ((y1 + size - 1) % size));
	for (i=start; i<=y2; i+=size)
	{
		end1.y = end2.y = (float) i;
		GRVector(&end1, &end2, gridstyle);
	}  /* end for */;
}  /* end SetGrid */;


GRSetRead(mask)
int mask;
/*
 *      This routine sets the read mask.
 */

{
#ifndef FASTIO
	char s1[3], s2[3], s3[3], s4[3];
#endif

	rmask = mask;

#ifndef FASTIO
	GRchex(rmask,s1,2);
	GRchex(rmask1,s2,2);
	GRchex(rmask2,s3,2);
	GRchex(rmask3,s4,2);
	fprintf(display,"M%s%s%s%s", s1, s2, s3, s4);
#else
	putc('M',display);
	putc(rmask,display);
	putc(rmask1,display);
	putc(rmask2,display);
	putc(rmask3,display);
#endif

	(void) fflush(display);
}  /* end SetRead */


GRDisplayGrid()
/*
 *      This routine displays the grid created by setgrid by enabling
 * the appropriate read layer.
 */

{
	GRSetRead(rmask | gridmask);
}  /* end DisplayGrid */

GRBlankGrid()
/*
 *      This routine assures that the grid is not displayed by
 * setting the read mask to disable the grid layer.
 */

{

	GRSetRead(rmask & ~gridmask);
}  /* end BlankGrid */

