/* template.c - template for display processes */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/template.c,v 7.1 91/02/22 09:29:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/template.c,v 7.1 91/02/22 09:29:27 mrose Interim $
 *
 *
 * $Log:	template.c,v $
 * Revision 7.1  91/02/22  09:29:27  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:01:48  mrose
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

/* Any errors should be written to *stdout* */
/* if the process exits, with out giving an error message, quipu may hang */

photo_start (name)
char * name;
{
	/* Initialise a window to recieve a photo of 'name' */

	/* return 0 if sucessful -1 if not */
        (void) fprintf (stderr,"PHOTO: not implemented yet");
	return (-1);
}


photo_end (name)
char * name;
{
	/* Decoding has finished - display the photo */
	(void) printf ("done it");
	(void) fflush (stdout);
	(void) close (1);	/* this is needed for QUIPU if the process does not exit */

	/* return 0 if sucessful -1 if not */
	return (-1);
}

photo_black (length)
int length;
{
	/* draw a black line of 'length' pixels */
}

photo_white (length)
int length;
{
	/* draw a white line of 'length' pixels */
}


photo_line_end (line)
bit_string * line;
{
	/* the end of a line has been reached */
	/* A bit string is stored in line->dbuf_top */
}
