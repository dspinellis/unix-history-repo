/*
 * Copyright 1985, Cognition Inc.
 * This software is not supported by Cognition Inc. and is
 * provided "as is".  To keep the X revolution growing, please forward 
 * enhancements and bug fixes to anyone who is interested.
 */
#ifndef lint
static char *rcsid_xshell_c = "$Header: xutils.c,v 10.3 86/02/01 16:19:44 tony Rel $";
#endif

#include <stdio.h>
#include <strings.h>
#include <ctype.h>

/****************************************************************************
 * XParse_Window_Geometry - parses a standard X dimension string like
 *
 *			=WIDTHxHEIGHT+XOFF+YOFF
 *			=WIDTHxHEIGHT-XOFF-YOFF
 *
 * and returns 1 if everything goes okay, else 0 so that you can use the
 * routine in an if statement.  Note that it will not touch the integer input
 * arguments but does mung the geometry.
 */


#define bomb(msg) return(0)

int XParse_Window_Geometry (geometry, widthp, heightp, 
				xsignp, xoffp, ysignp, yoffp)
	char *geometry;
	int *widthp, *heightp, *xsignp, *xoffp, *ysignp, *yoffp;
{
	register char *cp;
	int xoff, yoff, xsign, ysign, width, height;

	xoff = yoff = -1;	/* put it in the lower right hand corner */
	xsign = ysign = 0;	/* so that we can see if it is set or not */
	width = height = -1;	/* ditto */

	if (geometry != (char *) NULL) {/* normal =XxY+w+h or =XxY-w-h etc. */
	    if (*geometry == '=') geometry++;	/* skip = sign */
	    switch (*geometry) {	/* what is the first char we see? */
		case '+':		/* it is the positive XOFF */
		    xsign = 1;
		    goto get_xoff;
		case '-':		/* it is the negative XOFF */
		    xsign = -1;
		    goto get_xoff;
		case 'x':		/* it is the HEIGHT */
		    goto get_height;
		default:		/* it is the WIDTH */
		    break;
	    } /*end switch*/
	    cp = index (geometry, 'x');			/* find the x */
	    if (cp == (char *) NULL) bomb ("width");
	    *cp = '\0';
	    width = atoi (geometry);
	    geometry = cp;		/* skip to WIDTH */
    get_height:
	    for (cp = ++geometry; isdigit (*cp); cp++) ;
	    switch (*cp) {
		case '\0':
		    height = atoi (geometry);
		    goto done;
		case '+':
		    xsign = 1;
		    break;
		case '-':
		    xsign = -1;
		    break;
		default:
		    bomb ("height");
	    } /*end switch*/
	    *cp = '\0';
	    height = atoi (geometry);
	    geometry = cp;
    get_xoff:
	    for (cp = ++geometry; isdigit (*cp); cp++) ;
	    switch (*cp) {
		case '\0':
		    xoff = atoi (geometry);
		    goto done;
		case '+':
		    ysign = 1;
		    break;
		case '-':
		    ysign = -1;
		    break;
		default:
		    bomb ("X position");
	    } /*end switch*/
	    *cp = '\0';
	    xoff = atoi (geometry);
	    geometry = cp;
	    for (cp = ++geometry; isdigit (*cp); cp++) ;
	    if (*cp != '\0') bomb ("Y position");
	    yoff = atoi (geometry);
	} /*end if*/
	    
    done:
	if (width != -1) *widthp = width;
	if (height != -1) *heightp = height;

	if (xsign != 0) *xsignp = xsign;
	if (ysign != 0) *ysignp = ysign;

	if (xoff != -1) *xoffp = xoff;
	if (yoff != -1) *yoffp = yoff;

	return (1);

} /*end XParse_Window_Geometry*/

