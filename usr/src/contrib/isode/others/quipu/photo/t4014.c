/* t4014.c - display on tetronix 4014 terminals */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/t4014.c,v 7.1 91/02/22 09:29:26 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/t4014.c,v 7.1 91/02/22 09:29:26 mrose Interim $
 *
 *
 * $Log:	t4014.c,v $
 * Revision 7.1  91/02/22  09:29:26  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:01:47  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */



#include "stdio.h"
#include "quipu/photo.h"
#include "signal.h"
extern int NUMLINES,PIC_LINESIZE;
extern unsigned position;

/* Any errors should be written to *stdout* */
/* if the process exits, with out giving an error message, quipu may hang */

#define SCALE 5
#define Y_OFFSET 2700
#define X_OFFSET 3000
#define X_SKIP 8

int y = Y_OFFSET;

photo_quit ()
{
	putch (030);	/* Return to non-graphic mode */
	exit (0);
}

photo_start (name)
char * name;
{
	putch (035);	/* Enter graphic mode */

	openpl ();
	erase ();
	linemod ("solid");

	signal (SIGTERM,photo_quit);
	/* return 0 if sucessful -1 if not */

	return (0);
}


photo_end (name)
char * name;
{
	/* Decoding has finished - display the photo */
	move (0,Y_OFFSET - 100);
	closepl();

	(void) printf ("\n");
	(void) fflush (stdout);
	(void) close (1);	/* this is needed for QUIPU */
	/* wait until signalled to Terminate */
	for (;;)
		;
}

photo_black (length)
int length;
{
}

photo_white (length)
int length;
{
	line ((position*SCALE)+X_OFFSET,y,((length+position-1)*SCALE)+X_OFFSET,y);
}


photo_line_end (line)
bit_string * line;
{
	y -= SCALE;
}
