/*
 * @(#)gremlinlib.c	1.1  %G% 
 * 
 * Programmer's interface to Gremlin file creation routines.
 *
 * Mark Opperman (opcode@monet.BERKELEY) 
 *
 */

#include <stdio.h>
#include <math.h>
#include "gremlin.h"

#define SQRT2 0.707107		/* sqrt(2) / 2.0 */
#define TWOPI 6.283185

#define NFONTS 4
#define NBRUSHES 6
#define NSIZES 4
#define NSTIPPLES 8

static version;			/* AED_GREMLIN or SUN_GREMLIN */

static char *justify_names[] = { "BOTLEFT", "BOTRIGHT", "CENTCENT", 
				 "", "", "", "", "", "", "", 
				 "TOPLEFT", "TOPCENT", "TOPRIGHT", 
				 "CENTLEFT", "CENTRIGHT", "BOTCENT" };


/*
 * Return file pointer for open gremlin file.
 * Return NULL if bad arguments passed or unable to open file.
 * Write gremlin file header info to file.
 * Because there is only one global indicator of the device (version),
 * elements written to gremlinfile will be of the type of the device
 * in the most recent call to gr_open().
 */
FILE *
gr_open(gremlinfile, device, orientation, x, y)
char *gremlinfile;		/* file path name */
int device;			/* SUN_GREMLIN or AED_GREMLIN */
int orientation;		/* meaningful for AED only */
float x, y;			/* picture reference point */
{
    FILE *fp, *fopen();

    if ((device != SUN_GREMLIN) && (device != AED_GREMLIN)) {
	fprintf(stderr, "gr_open: bad device %d\n", device);
	return((FILE *) NULL);
    }
    else if ((orientation != VERT_ORIENT) && (orientation != HORZ_ORIENT)) {
	fprintf(stderr, "gr_open: bad orientation %d\n", orientation);
	return((FILE *) NULL);
    }

    if ((fp = fopen(gremlinfile, "w")) != NULL) {
	if ((version = device) == SUN_GREMLIN)
	    fprintf(fp, "sungremlinfile\n");
	else
	    fprintf(fp, "gremlinfile\n");
	
	fprintf(fp, "%d %3.2f %3.2f\n", orientation, x, y);
    }

    return(fp);
}


/*
 * Write terminator element to gremlin file and close the file.
 */
gr_close(fp)
FILE *fp;
{
    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_close: NULL file pointer\n");
	return(GR_ERROR);
    }
    fprintf(fp, "-1\n");
    return(fclose(fp));
}


/*
 * Write vector element to gremlin file.
 */
gr_vector(fp, npoints, list, brush)
FILE *fp;			/* gremlin file pointer */
int npoints;			/* number of points */
float list[][2];		/* coordinate point list */
int brush;			/* line style */
{
    register i;

    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_vector: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if (npoints < 2) {
	fprintf(stderr, "gr_vector: too few points %d\n", npoints);
	return(GR_ERROR);
    }
    else if ((brush < 1) || (brush > NBRUSHES)) {
	fprintf(stderr, "gr_vector: bad brush %d\n", brush);
	return(GR_ERROR);
    }

    if (version == SUN_GREMLIN)
	fprintf(fp, "VECTOR\n");
    else
	fprintf(fp, "%d\n", VECTOR);

    for (i=0; i<npoints; i++)
	fprintf(fp, "%3.2f %3.2f\n", list[i][0], list[i][1]);

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d 0\n0\n", brush);
    return(GR_OK);
}


/*
 * Write curve element to gremlin file.
 */
gr_curve(fp, npoints, list, brush)
FILE *fp;			/* gremlin file pointer */
int npoints;			/* number of points */
float list[][2];		/* coordinate point list */
int brush;			/* line style */
{
    register i;

    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_curve: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if (npoints < 2) {
	fprintf(stderr, "gr_curve: too few points %d\n", npoints);
	return(GR_ERROR);
    }
    else if ((brush < 1) || (brush > NBRUSHES)) {
	fprintf(stderr, "gr_curve: bad brush %d\n", brush);
	return(GR_ERROR);
    }

    if (version == SUN_GREMLIN)
	fprintf(fp, "CURVE\n");
    else
	fprintf(fp, "%d\n", CURVE);

    for (i=0; i<npoints; i++)
	fprintf(fp, "%3.2f %3.2f\n", list[i][0], list[i][1]);

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d 0\n0\n", brush);
    return(GR_OK);
}


/*
 * Write circle element (360 degree arc) to gremlin file.
 */
gr_circle(fp, cx, cy, radius, brush)
FILE *fp;			/* gremlin file pointer */
float cx, cy;			/* center of the circle */
float radius;			/* circle radius */
int brush;			/* line style */
{
    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_circle: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if (radius <= 0.0) {
	fprintf(stderr, "gr_circle: non-positive radius %f\n", radius);
	return(GR_ERROR);
    }
    else if ((brush < 1) || (brush > NBRUSHES)) {
	fprintf(stderr, "gr_circle: bad brush %d\n", brush);
	return(GR_ERROR);
    }

    if (version == SUN_GREMLIN)
	fprintf(fp, "ARC\n");
    else
	fprintf(fp, "%d\n", ARC);

    fprintf(fp, "%3.2f %3.2f\n", cx, cy);
    fprintf(fp, "%3.2f %3.2f\n", cx + (SQRT2 * radius), 
				 cy + (SQRT2 * radius));
    fprintf(fp, "%3.2f %3.2f\n", cx, cy + radius);
    fprintf(fp, "%3.2f %3.2f\n", cx, cy - radius);
    fprintf(fp, "%3.2f %3.2f\n", cx + radius, cy);
    fprintf(fp, "%3.2f %3.2f\n", cx - radius, cy);

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d 0\n0\n", brush);
    return(GR_OK);
}


/*
 * Write arc element to gremlin file.
 */
gr_arc(fp, cx, cy, sx, sy, angle, brush)
FILE *fp;			/* gremlin file pointer */
float cx, cy;			/* center of the circle */
float sx, sy;			/* point at start of arc */
float angle;			/* counter-clockwise angle of arc (degrees) */
int brush;			/* line style */
{
    float radius;		/* radius of arc */
    float start_angle;		/* angle of starting point (radians) */
    float stop_angle;		/* angle stopping point (radians) */
    double dx, dy;		/* delta x and y (center to start) */

    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_arc: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if ((angle <= 0.0) || (angle > 360.0)) {
	fprintf(stderr, "gr_arc: bad angle %f\n", angle);
	return(GR_ERROR);
    }
    else if ((brush < 1) || (brush > NBRUSHES)) {
	fprintf(stderr, "gr_arc: bad brush %d\n", brush);
	return(GR_ERROR);
    }

    dx = (double) (sx - cx);
    dy = (double) (sy - cy);

    if ((radius = (float) sqrt(dx * dx + dy * dy)) == 0.0) {
	fprintf(stderr, "gr_arc: zero radius\n");
	return(GR_ERROR);
    }

    start_angle = (float) atan2(dx, dy);
    stop_angle = start_angle + (angle * (TWOPI / 360.0));

    if (version == SUN_GREMLIN)
	fprintf(fp, "ARC\n");
    else
	fprintf(fp, "%d\n", ARC);

    fprintf(fp, "%3.2f %3.2f\n", cx, cy);
    fprintf(fp, "%3.2f %3.2f\n", sx, sy);
    fprintf(fp, "%3.2f %3.2f\n", cx + (radius * sin(stop_angle)),
				 cy + (radius * cos(stop_angle)));

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d %d\n0\n", brush, (int) (angle + 0.5));
    return(GR_OK);
}


/*
 * Write polygon element to gremlin file.
 */
gr_polygon(fp, npoints, list, border, stipple)
FILE *fp;			/* gremlin file pointer */
int npoints;			/* number of points */
float list[][2];		/* coordinate point list */
int border;			/* border style (0 is unbordered) */
int stipple;			/* stipple pattern */
{
    register i;

    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_polygon: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if (npoints < 3) {
	fprintf(stderr, "gr_polygon: too few points %d\n", npoints);
	return(GR_ERROR);
    }
    else if ((border < 0) || (border > NBRUSHES)) {
	fprintf(stderr, "gr_polygon: bad border %d\n", border);
	return(GR_ERROR);
    }
    else if ((stipple < 1) || (stipple > NSTIPPLES)) {
	fprintf(stderr, "gr_polygon: bad stipple %d\n", stipple);
	return(GR_ERROR);
    }

    if (version == SUN_GREMLIN)
	fprintf(fp, "POLYGON\n");
    else
	fprintf(fp, "%d\n", POLYGON);

    for (i=0; i<npoints; i++)
	fprintf(fp, "%3.2f %3.2f\n", list[i][0], list[i][1]);

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d %d\n0\n", border, stipple);
    return(GR_OK);
}


/*
 * Write text element to gremlin file.
 * The SUN version of Gremlin recalculates the lower left coordinate
 * text position each time a file is read.  Therefore, coordinates
 * are computed as if the text were being displayed on the AED.
 */
gr_text(fp, text, x, y, font, size, justification)
FILE *fp;			/* gremlin file pointer */
char *text;			/* text string */
float x, y;			/* justification relative to this point */
int font, size;			/* text font, text size */
int justification;		/* type of justification */
{
    int nchars;			/* number of characters in text string */
    int length;			/* (AED) length based on size */
    int charxsize;		/* (AED) character width */
    int charysize;		/* (AED) character height */
    int descenders;		/* (AED) character descender height */
    float pos_x, pos_y;		/* lower left coordinate position of text */

    if (fp == (FILE *) NULL) {
	fprintf(stderr, "gr_text: NULL file pointer\n");
	return(GR_ERROR);
    }
    else if ((nchars = strlen(text)) == 0) {
	fprintf(stderr, "gr_text: null text string\n");
	return(GR_ERROR);
    }
    else if ((font < 1) || (font > NFONTS)) {
	fprintf(stderr, "gr_text: bad font %d\n", font);
	return(GR_ERROR);
    }
    else if ((size < 1) || (size > NSIZES)) {
	fprintf(stderr, "gr_text: bad size %d\n", size);
	return(GR_ERROR);
    }
    else if ((justification < BOTLEFT) || (justification > BOTCENT) ||
	    ((justification > CENTCENT) && (justification < TOPLEFT))) {
	fprintf(stderr, "gr_text: bad justification %d\n", justification);
	return(GR_ERROR);
    }

    switch (size) {		/* set lower left position a la AED */
	case 1:
	    charxsize = 6;
	    charysize = 7;
	    descenders = 1;
	    break;
	case 2:
	    charxsize = 8;
	    charysize = 12;
	    descenders = 3;
	    break;
	case 3:
	    charxsize = 10;
	    charysize = 14;
	    descenders = 2;
	    break;
	case 4:
	    charxsize = 15;
	    charysize = 24;
	    descenders = 6;
	    break;
    }

    length = nchars * charxsize;

    switch (justification) {
	case BOTLEFT:
	    pos_x = x;
	    pos_y = y;
	case BOTCENT:
	    pos_x = x - (length >> 1);
	    pos_y = y;
	    break;
	case BOTRIGHT:
	    pos_x = x - length;
	    pos_y = y;
	    break;
	case CENTLEFT:
	    pos_x = x;
	    pos_y = y - (charysize >> 1);
	    break;
	case CENTCENT:
	    pos_x = x - (length >> 1);
	    pos_y = y - (charysize >> 1);
	    break;
	case CENTRIGHT:
	    pos_x = x - length;
	    pos_y = y - (charysize >> 1);
	    break;
	case TOPLEFT:
	    pos_x = x;
	    pos_y = y + descenders - charysize;
	    break;
	case TOPCENT:
	    pos_x = x - (length >> 1);
	    pos_y = y + descenders - charysize;
	    break;
	case TOPRIGHT:
	    pos_x = x - length;
	    pos_y = y + descenders - charysize;
	    break;
    }

    if (version == SUN_GREMLIN)
	fprintf(fp, "%s\n", justify_names[justification]);
    else
	fprintf(fp, "%d\n", justification);

    fprintf(fp, "%3.2f %3.2f\n", x, y);
    fprintf(fp, "%3.2f %3.2f\n", pos_x, pos_y);
    fprintf(fp, "%3.2f %3.2f\n", pos_x + (length >> 1), pos_y);
    fprintf(fp, "%3.2f %3.2f\n", pos_x + length, pos_y);

    fprintf(fp, (version == SUN_GREMLIN) ? "*\n" : "-1.00 -1.00\n");

    fprintf(fp, "%d %d\n", font, size);
    fprintf(fp, "%d %s\n", nchars, text);
    return(GR_OK);
}
