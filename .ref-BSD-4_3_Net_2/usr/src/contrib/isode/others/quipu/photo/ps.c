/* ps.c - fax to postscript */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/ps.c,v 7.1 91/02/22 09:29:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/ps.c,v 7.1 91/02/22 09:29:24 mrose Interim $
 *
 *
 * $Log:	ps.c,v $
 * Revision 7.1  91/02/22  09:29:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/25  11:47:45  mrose
 * *** empty log message ***
 * 
 * Revision 1.1  91/01/24  17:03:35  kej
 * Initial revision
 * 
 * Revision 1.1  91/01/02  21:36:47  kej
 * Initial revision
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

#include <stdio.h>
#include <quipu/photo.h>

#define HEIGHT 2200

static int x, y;

extern int two_passes;

photo_start (name)
char * name;
{
	x = 0;
	y = HEIGHT;
	two_passes = 0;
	puts ("%!\n0 setlinewidth 72 200 div 72 200 div scale");
	return 0;
}


photo_end (name)
char * name;
{
	/* Decoding has finished - display the image */

	if (y < HEIGHT) puts ("showpage");
	return 0;
}

photo_black (length)
int length;
{
    if (length > 0)
        printf ("%d %d moveto %d %d lineto stroke\n", x, y, x + length - 1, y);
    x += length;
    return 0;
}

photo_white (length)
int length;
{
    x += length;
    return 0;
}


photo_line_end (line)
bit_string * line;
{
    x = 0;
    --y;
    if (y < 0) {
	puts ("showpage");
	y = HEIGHT;
    }
    return 0;
}

