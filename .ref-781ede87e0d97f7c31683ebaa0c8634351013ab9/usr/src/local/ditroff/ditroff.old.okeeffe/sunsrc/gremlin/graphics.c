/*
 * @(#)graphics.c	1.2	%G%
 *
 * Graphics routines for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include <vfont.h>
#include "icondata.h"
#include "gremlin.h"

/* imports from main.c */

extern error();
extern struct pixwin *pix_pw;
extern struct rect pix_size;
extern struct pixrect *cset_pr;
extern struct pixrect *scratch_pr;
extern ELT *cset;
extern Artmode;
extern CSIZE;
extern CFONT;
extern CsetOn;
extern SUN_XORIGIN; 
extern SUN_YORIGIN; 

/* imports from display.c */

extern minsunx, maxsunx, minsuny, maxsuny;

/* imports from C */

extern char *malloc();

/* forward references */

extern char *GRReadFontFile();

/* symbolic font from text.c */

extern struct pixfont *text_pf;

/* locally defined variables */

int charysizes[NFONTS][NSIZES];	/* Character y dimensions for each size */
int curve_set;			/* TRUE if spline points pre-computed */

int linestyle;			/* Current line style */
int linemod;			/* Type of line (SOLID, DOTTED, ...) */
int linethickness;		/* 1, 2, 3 */

char fontdir[128] = "/usr/lib/font/devsun/";
char stippledir[128] = "/usr/lib/font/devsun/";
char stippletype[32] = "cf";

char *font_types[NFONTS] = { "R", "I", "B", "S" };
int font_sizes[NSIZES] = { 7, 10, 14, 24 };
int stipple_index[NSTIPPLES] = { 1, 3, 12, 14, 16, 19, 21, 23 };

/* NOTE: all stipple fonts are expected to be 32 x 32 bit rasters */

/* pointers to the stipple pixrects (16 x 16 bits) in the menu ... */
struct pixrect *stipple_prs[NSTIPPLES] = {
    &stipple1_pr, &stipple2_pr, &stipple3_pr, &stipple4_pr, 
    &stipple5_pr, &stipple6_pr, &stipple7_pr, &stipple8_pr 
};

/* ... and the corresponding images (32 x 32 bits) from the vfont file */
char stipple_patterns[NSTIPPLES][128];

/* data used in graphics2.c for drawing polygons */
int rasterlength;		/* real # horizontal bits in scratch_pr */
int bytesperline;		/* rasterlength / 8 */
int nlines;			/* # horizontal bits defined by scratch_pr */
char *fill;			/* pointer to scratch_pr image */

/* 
 *  This matrix points to the DISPATCH data for each font/size pair
 *  if an unsuccesful attempt is made to open a particular font/size pair,
 *  its entry in this table is marked as -1.
 */
char *font_info[NFONTS][NSIZES] = {
    { NULL, NULL, NULL, NULL },
    { NULL, NULL, NULL, NULL },
    { NULL, NULL, NULL, NULL },
    { NULL, NULL, NULL, NULL },
};
struct pixrect *char_pr;

/* Splines use these global arrays */

static float h[MAXPOINTS];
static float x[MAXPOINTS], dx[MAXPOINTS], d2x[MAXPOINTS], d3x[MAXPOINTS];
static float y[MAXPOINTS], dy[MAXPOINTS], d2y[MAXPOINTS], d3y[MAXPOINTS];
static numpoints;

/* These are used as bit masks to create the right style lines. */
#define SOLID -1
#define DOTTED 002
#define DASHED 004
#define DOTDASHED 012


/*
 * This routine sets the current line style.
 */
GRSetLineStyle(style)
int style;			/* new stipple pattern for lines */
{
    switch (linestyle = style) {
	case 1:			/* dotted */
	    linemod = DOTTED;
	    linethickness = 1;
	    break;
	case 2:			/* broken */
	    linemod = DOTDASHED;
	    linethickness = 1;
	    break;
	case 3:			/* thick */
	    linemod = SOLID;
	    linethickness = 3;
	    break;
	case 4:			/* dashed */
	    linemod = DASHED;
	    linethickness = 1;
	    break;
	case 5:			/* narrow */
	    linemod = SOLID;
	    linethickness = 1;
	    break;
	case 6:			/* medium */
	    linemod = SOLID;
	    linethickness = 2;
	    break;
    }
}


/*
 * This routine returns the maximum vertical size (in bits) of a character
 * of the specified font/size.
 */
GRGetCharYSize(font, size)
register font;			/* character font (1 - 4) */
register size;			/* character size (1 - 4) */
{
    return(charysizes[--font][--size]);
}


#define pi 3.14159265359
#define twopi 6.28318530718
#define log2_10 3.321915


/* 
 * Draw arc - always to scratch_pr.
 * Note: must check for zero radius before calling.
 */
GRArc(center, cpoint, angle, style)
register POINT *center, *cpoint;
float angle;
register style;
{
    double radius, resolution, t1, fullcircle;
    double degreesperpoint;
    float xs, ys, epsilon;
    float x1, y1, x2, y2;
    register i, extent;

    xs = cpoint->x - center->x;
    ys = cpoint->y - center->y;

    /* calculate drawing parameters */

    radius = sqrt((double) (xs * xs + ys * ys));
    t1 = floor(log10(radius) * log2_10);
    resolution = pow(2.0, t1);
    epsilon = (float) 1.0 / resolution;
    fullcircle = ceil(twopi * resolution);
    degreesperpoint = 360.0 / fullcircle;

    extent = (angle == 0) ? fullcircle : angle/degreesperpoint;

    GRSetLineStyle(style);

    x1 = cpoint->x;
    y1 = cpoint->y;

    for (i=0; i<extent; ++i) {
	xs -= epsilon * ys;
	x2 = xs + center->x;
	ys += epsilon * xs;
	y2 = ys + center->y;

	GRVector(x1, y1, x2, y2);

	x1 = x2;
	y1 = y2;
    }
}  /* end GRArc */;


/* This routine calculates parameteric values for use in calculating
 * curves.  The values are an approximation of cumulative arc lengths 
 * of the curve (uses cord * length).  For additional information, 
 * see paper cited below.
 */
static
Paramaterize(x, y, h, n)
float x[MAXPOINTS];
float y[MAXPOINTS];
float h[MAXPOINTS];
register n;
{
    register i, j;
    float t1, t2;
    float u[MAXPOINTS];

    n = numpoints;

    for (i=1; i<=n; ++i) {
	u[i] = 0.0;
	for (j=1; j<i; ++j) {
	    t1 = x[j+1] - x[j];
	    t2 = y[j+1] - y[j];
	    u[i] += (float) sqrt((double) ((t1 * t1) + (t2 * t2)));
	}
    }

    for (i=1; i<n; ++i)
	h[i] = u[i+1] - u[i];
}  /* end Paramaterize */


/*
 * This routine solves for the cubic polynomial to fit a spline
 * curve to the the points  specified by the list of values.
 * The curve generated is periodic.  The alogrithms for this 
 * curve are from the "Spline Curve Techniques" paper cited below.
 */
static
PeriodicSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* paramaterization */
float z[MAXPOINTS];		/* point list */
float dz[MAXPOINTS];		/* to return the 1st derivative */
float d2z[MAXPOINTS];		/* 2nd derivative */
float d3z[MAXPOINTS];		/* and 3rd derivative */
register npoints;		/* number of valid points */
{
    float a[MAXPOINTS]; 
    float b[MAXPOINTS]; 
    float c[MAXPOINTS]; 
    float d[MAXPOINTS]; 
    float deltaz[MAXPOINTS];
    float r[MAXPOINTS];
    float s[MAXPOINTS];
    float ftmp;
    register i;

    /* step 1 */
    for (i=1; i<npoints; ++i) {
	if (h[i] != 0)
	    deltaz[i] = (z[i+1] - z[i]) / h[i];
	else
	    deltaz[i] = 0;
    }
    h[0] = h[npoints-1];
    deltaz[0] = deltaz[npoints-1];

    /* step 2 */
    for (i=1; i<npoints-1; ++i) {
	d[i] = deltaz[i+1] - deltaz[i];
    }
    d[0] = deltaz[1] - deltaz[0];

    /* step 3a */
    a[1] = 2 * (h[0] + h[1]);
    if (a[1] == 0) 
	return(-1);  /* 3 consecutive knots at same point */
    b[1] = d[0];
    c[1] = h[0];

    for (i=2; i<npoints-1; ++i) {
	ftmp = h[i-1];
	a[i] = ftmp + ftmp + h[i] + h[i] - (ftmp * ftmp)/a[i-1];
	    if (a[i] == 0) 
		return(-1);  /* 3 consec knots at same point */
	b[i] = d[i-1] - ftmp * b[i-1]/a[i-1];
	c[i] = -ftmp * c[i-1]/a[i-1];
    }

    /* step 3b */
    r[npoints-1] = 1;
    s[npoints-1] = 0;
    for (i=npoints-2; i>0; --i) {
	r[i] = -(h[i] * r[i+1] + c[i])/a[i];
	s[i] = (6 * b[i] - h[i] * s[i+1])/a[i];
    }

    /* step 4 */
    d2z[npoints-1] = (6 * d[npoints-2] - h[0] * s[1] 
		       - h[npoints-1] * s[npoints-2]) 
		     / (h[0] * r[1] + h[npoints-1] * r[npoints-2] 
			+ 2 * (h[npoints-2] + h[0]));
    for (i=1; i<npoints-1; ++i) {
	d2z[i] = r[i] * d2z[npoints-1] + s[i];
    }
    d2z[npoints] = d2z[1];

    /* step 5 */
    for (i=1; i<npoints; ++i) {
	dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
	if (h[i] != 0)
	    d3z[i] = (d2z[i+1] - d2z[i])/h[i];
	else
	    d3z[i] = 0;
    }

    return(0);
}  /* end PeriodicSpline */


/*
 * This routine solves for the cubic polynomial to fit a spline
 * curve from the points specified by the list of values.  The alogrithms for
 * this curve are from the "Spline Curve Techniques" paper cited below.
 */
static
NaturalEndSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* paramaterization */
float z[MAXPOINTS];		/* point list */
float dz[MAXPOINTS];		/* to return the 1st derivative */
float d2z[MAXPOINTS];		/* 2nd derivative */
float d3z[MAXPOINTS];		/* and 3rd derivative */
register npoints;		/* number of valid points */
{
    float a[MAXPOINTS]; 
    float b[MAXPOINTS]; 
    float d[MAXPOINTS]; 
    float deltaz[MAXPOINTS];
    float ftmp;
    register i;

    /* step 1 */
    for (i=1; i<npoints; ++i) {
	if (h[i] != 0)
	    deltaz[i] = (z[i+1] - z[i]) / h[i];
	else
	    deltaz[i] = 0;
    }
    deltaz[0] = deltaz[npoints-1];

    /* step 2 */
    for (i=1; i<npoints-1; ++i) {
	d[i] = deltaz[i+1] - deltaz[i];
    }
    d[0] = deltaz[1] - deltaz[0];

    /* step 3 */
    a[0] = 2 * (h[2] + h[1]);
    if (a[0] == 0)		/* 3 consec knots at same point */
	return(-1);
    b[0] = d[1];

    for (i=1; i<npoints-2; ++i) {
	ftmp = h[i+1];
	a[i] = ftmp + ftmp + h[i+2] + h[i+2] - (ftmp * ftmp) / a[i-1];
	if (a[i] == 0)		/* 3 consec knots at same point */
	    return(-1);
	b[i] = d[i+1] - ftmp * b[i-1]/a[i-1];
    }

    /* step 4 */
    d2z[npoints] = d2z[1] = 0;
    for (i=npoints-1; i>1; --i) {
	d2z[i] = (6 * b[i-2] - h[i] *d2z[i+1])/a[i-2];
    }

    /* step 5 */
    for (i=1; i<npoints; ++i) {
	dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
	if (h[i] != 0)
	    d3z[i] = (d2z[i+1] - d2z[i])/h[i];
	else
	    d3z[i] = 0;
    }

    return(0);
}  /* end NaturalEndSpline */


#define PointsPerInterval 16


/*
 * This routine computes a smooth curve through a set of points.
 * Returns -1 if there are too many knots to draw the curve.
 * Use GRCurve AFTER this routine to actually draw the curve.
 * [Formerly the first half of GRCurve()]
 *
 * The method used is the parametric spline curve on unit knot mesh described
 * in "Spline Curve Techniques" by Patrick Baudelaire, Robert Flegal, and 
 * Robert Sproull -- Xerox Parc.
 */
GRSetCurve(pointlist)
POINT *pointlist;
{
    register POINT *ptr;
    register i, stat;

    /* Copy point list to array for easier access */
    ptr = pointlist;
    for (i=1; (!Nullpoint(ptr)); ++i) {
	x[i] = ptr->x;
	y[i] = ptr->y;
	ptr = PTNextPoint(ptr);
    }

    /* Solve for derivatives of the curve at each point 
       separately for x and y (parametric). */

    numpoints = i - 1;		/* set global numpoints */

    Paramaterize(x, y, h, numpoints);

    stat = 0;
    if ((x[1] == x[numpoints]) && (y[1] == y[numpoints])) { /* closed curve */
	stat |= PeriodicSpline(h, x, dx, d2x, d3x, numpoints);
	stat |= PeriodicSpline(h, y, dy, d2y, d3y, numpoints);
    }
    else {
	stat |= NaturalEndSpline(h, x, dx, d2x, d3x, numpoints);
	stat |= NaturalEndSpline(h, y, dy, d2y, d3y, numpoints);
    }

    curve_set = 1;		/* indicates that paramterization is done */
    return(stat);
}


/*
 * This routine displays a smooth curve through a set of points.  The 
 * method used is the parametric spline curve on unit knot mesh described
 * in "Spline Curve Techniques" by Patrick Baudelaire, Robert Flegal, and 
 * Robert Sproull -- Xerox Parc.
 * [formerly the second half of GRCurve()]
 *
 * Uses the data computed first by GRSetCurve().
 * GRSetCurve() MUST be called before this routine and have returned a ZERO.
 */
GRCurve(style)
int style;
{
    float t, t2, t3, xinter, yinter;
    float x1, y1, x2, y2;
    register j, k;

    GRSetLineStyle(style);

    x1 = x[1];
    y1 = y[1];

    /* generate the curve using the information from GRSetCurve() and 
       PointsPerInterval vectors between each specified knot. */

    for (j=1; j<numpoints; ++j) {
	for (k=0; k<=PointsPerInterval; ++k) {
	    t = (float) k * h[j] / (float) PointsPerInterval;
	    t2 = t * t;
	    t3 = t2 * t;
	    x2 = x[j] + t * dx[j] + t2 * d2x[j]/2.0 + t3 * d3x[j]/6.0;
	    y2 = y[j] + t * dy[j] + t2 * d2y[j]/2.0 + t3 * d3y[j]/6.0;

	    GRVector(x1, y1, x2, y2);

	    x1 = x2;
	    y1 = y2;
	}
    }
}  /* end GRCurve */


/*
 * This routine clears the Gremlin pix subwindow or current set
 * pixrect image as specified in the mask.
 */
GRClear(mask)
register mask;
{
    if (mask & pixmask)
	pw_writebackground(pix_pw, 0, 0, 2000, 2000, PIX_SRC);

    if (mask & csetmask)
	pr_rop(cset_pr, 0, 0, 2000, 2000, PIX_SRC, NULL, 0, 0);
}  /* end GRClear */


/*
 *  Display justification of TEXT element.
 */
GRDisplayJustify(elt)
register ELT *elt;
{
    register POINT *point;
    register x, y, length, ysize;

    ysize = GRGetCharYSize(elt->brushf, elt->size);
    length = GRFontStrlen(elt->textpt, elt->brushf, elt->size);
    point = PTNextPoint(elt->ptlist);	/* lower left corner of text */
    x = dbx_to_win(point->x);
    y = dby_to_win(point->y);

    switch (elt->type) {
	case TOPLEFT:
	    y -= ysize;
	    break;
	case TOPCENT:
	    y -= ysize;
	    x += (length >> 1);
	    break;
	case TOPRIGHT:
	    y -= ysize;
	    x += length;
	    break;
	case CENTLEFT:
	    y -= (ysize >> 1);
	    break;
	case CENTCENT:
	    y -= (ysize >> 1);
	    x += (length >> 1);
	    break;
	case CENTRIGHT:
	    y -= (ysize >> 1);
	    x += length;
	    break;
	case BOTLEFT:
	    break;
	case BOTCENT:
	    x += (length >> 1);
	    break;
	case BOTRIGHT:
	    x += length;
	    break;
    }

    pw_write(pix_pw, x - 2, y - 2, 5, 5, PIX_SRC ^ PIX_DST, &dot_pr, 0, 0);
    pr_rop(cset_pr, x - 2, y - 2, 5, 5, PIX_SRC ^ PIX_DST, &dot_pr, 0, 0);
}


/*
 * This routine displays a point (layed down by the user) in the
 * pix subwindow.  
 */
GRDisplayPoint(dbx, dby, number)
float dbx, dby;			/* data base coordinates */
register number;		/* point number */
{
    register x, y;
    char numbuf[5];

    x = dbx_to_win(dbx);
    y = dby_to_win(dby);

    if (Artmode)
	pw_write(pix_pw, x-1, y-1, 3, 3, PIX_SRC ^ PIX_DST, 
				    &littlepoint_pr, 3, 2);
    else {
	pw_write(pix_pw, x-3, y-3, 7, 7, PIX_SRC ^ PIX_DST, 
				    &littlepoint_pr, 1, 7);
	(void) sprintf(numbuf, "%d", number+1);
	pw_text(pix_pw, x+5, y+3, PIX_SRC^PIX_DST, text_pf, numbuf);
    }
}  /* end GRDisplayPoint */


/*
 * This routine erases the specified point.
 */
GRErasePoint(dbx, dby, number)
float dbx, dby;
register number;
{
    GRDisplayPoint(dbx, dby, number);
}  /* end GRErasePoint */


/*
 * This routine clears all points in plist.
 */
GRBlankPoints(plist)
register POINT *plist;
{
    register i = 0;

    while (!Nullpoint(plist)) {
	GRErasePoint(plist->x, plist->y, i++);
	plist = PTNextPoint(plist);
    }   
}  /* end GRBlankPoints */


/*
 * This routine displays the grid.
 */
GRDisplayGrid()
{
    pw_replrop(pix_pw, 0, 0, 2000, 2000, PIX_SRC ^ PIX_DST, 
						&replgrid32_pr, 0, 0);
}  /* end GRDisplayGrid */


/*
 * This routine erases the grid.
 */
GRBlankGrid()
{
    GRDisplayGrid();
}  /* end GRBlankGrid */


/*
 * Flash current set display.
 */
GRCurrentSet()
{
    if (DBNullelt(cset))
	return;

    pw_write(pix_pw, 0, 0, pix_size.r_width, pix_size.r_height,
	PIX_SRC ^ PIX_DST, cset_pr, 0, 0);

    CsetOn = !CsetOn;
}


/*
 * Make current set on.
 */
GRCurrentSetOn()
{
    if (!CsetOn)
	GRCurrentSet();
}


/*
 * Make current set off.
 */
GRCurrentSetOff()
{
    if (CsetOn)
	GRCurrentSet();
}


/*
 * Return TRUE if font file exists and is readable.
 */
GRfontfound(font, size)
register font, size;
{
    return(font_info[font-1][size-1] != (char *) -1);
}
 

/* 
 * Open the default font file on startup.
 */
GRFontInit()
{
    /* create memory pixrect template for displaying text with GRPutText() */
    if ((char_pr = mem_create(1, 1, 1)) == NULL) {
	printf("GRFontInit: can't create char_pr\n");
	exit(1);
    }

    GROpenFont(CFONT, CSIZE);
    GRStippleInit();
}  /* end GRFontInit */


/*
 * Initialize stipple patterns from font file.
 * Big assumption: all stipples are defined by 32 x 32 bit patterns.
 * All fonts do not contain exactly 32 rows of 4 bytes - this is ok -
 * Fonts wider than 32 bits will be clipped.
 */
GRStippleInit()
{
    register struct mpr_data *mpr_data;
    register char *from, *to;
    register char *fbase;
    register i, j, k;
    struct dispatch *dispatch, *dstart;
    int width, height, bytewidth;
    char *stipple_info;
    char name[128];

    (void) sprintf(name, "%s%s.0", stippledir, stippletype);

    if ((stipple_info = GRReadFontFile(name)) == (char *) -1) {
	/* 
	 * use default stipple pixrects since we can't read the
	 * user specified stipple font file.
	 * copy stipple pixrects to stipple_patterns for display
	 */
	for (i=0; i<NSTIPPLES; i++)
	    GRCopyStipple(i);

	return;
    }

    dstart = (struct dispatch *) (stipple_info + sizeof(struct header));
    fbase = (char *) ((char *) dstart + NUM_DISPATCH * sizeof(struct dispatch));

    for (i=0; i<NSTIPPLES; i++) {
	mpr_data = (struct mpr_data *) stipple_prs[i]->pr_data;
	dispatch = dstart + stipple_index[i];
	if (dispatch->nbytes != 0) {
	    width = dispatch->left + dispatch->right;
	    height = dispatch->up + dispatch->down;
	    bytewidth = (width + 7) >> 3;
	    if (bytewidth > 4)		/* force size constraint */
		bytewidth = 4;		/* pattern screwed up if ever > 4 */

	    from = (char *) ((char *) fbase + dispatch->addr);
	    to = stipple_patterns[i];

	    for (j=1; j<=height; j++) {	/* copy font entry to known location */
		for (k=1; k<=bytewidth; k++)
		    *to++ = *from++;
		for ( ;k<=4; k++)
		    *to++ = '\0';
	    }

	    for ( ; j<=32; j++)		/* fix up any non- 32 x 32 font */
		for (k=1; k<=4; k++)
		    *to++ = '\0';
		    
	    /* copy vfont stipple to stipple pixrect for menu display */
	    /* can only display a 16 x 16 menu icon */
	    from = stipple_patterns[i];
	    to = (char *) mpr_data->md_image;
	    for (j=0; j<16; j++) {
		*to++ = *from++;
		*to++ = *from++;
		from += 2;
	    }
	}
	else {
	    (void) sprintf(name, "stipple index=%d not defined", 
							    stipple_index[i]);
	    error(name);
	    
	    /* copy stipple pixrect to stipple_patterns for display */
	    GRCopyStipple(i);
	}
    }

    /* bit maps are all in core now */
    free(stipple_info);

    /* set up parameters for drawing polygons */
    mpr_data = (struct mpr_data *) scratch_pr->pr_data;
    bytesperline = mpr_data->md_linebytes;
    fill = (char *) mpr_data->md_image;
    rasterlength = bytesperline << 3;
    nlines = scratch_pr->pr_size.x;
} /* end GRStippleInit */;


/*
 * Copy the stipple bit map image as defined in the menu pixrect
 * to the bit maps used for drawing polygons.  The pixrect bit map
 * is 16 x 16 bits and the target bit map is 32 x 32 bits.  The
 * image is expanded appropriately.
 */
GRCopyStipple(index)
int index;
{
    register struct mpr_data *mpr_data;
    register char *from, *to;
    register i, j;

    mpr_data = (struct mpr_data *) stipple_prs[index]->pr_data;
    from = (char *) mpr_data->md_image;
    to = stipple_patterns[index];

    for (i=0; i<16; i++) {
	j = i << 2;
	to[j]   = to[j+2] = to[j+64] = to[j+66] = *from++;
	to[j+1] = to[j+3] = to[j+65] = to[j+67] = *from++;
    }
}


/* 
 * Open a font file for use by first reading it into memory.
 * If the file is read successfully, the appropriate entry in
 * font_info[] is set to point to its memory image.  If the
 * file cannot be opened or there is a error in reading the
 * file, set the font_info[] entry to -1.
 */
GROpenFont(font, size)
register font;		/* font is 1 to 4 */
register size;		/* size is 1 to 4 */
{
    char name[128];

    if (font_info[--font][--size] != NULL)	/* already tried to open */
	return;
    
    sprintf(name, "%s%s.%d", fontdir, font_types[font], font_sizes[size]);

    if ((font_info[font][size] = GRReadFontFile(name)) == (char *) -1)
	return;

    /* height of this font/size combination */
    charysizes[font][size] = ((struct header *) font_info[font][size])->maxy;
}  /* end GROpenFont */


/* 
 * Read a font file into memory and return a pointer to its
 * memory image, or -1 if any error occurs.
 */
char *
GRReadFontFile(file)
char *file;
{
    char *image;			/* pointer to font memory image */
    char msg[128];
    struct header header;
    int fd, filesize;

    if ((fd = open(file, 0)) < 0) {
	sprintf(msg, "can't open font file: %s", file);
	error(msg);
	return((char *) -1);
    }

    if (read(fd, &header, sizeof(struct header)) != sizeof(struct header)) {
	sprintf(msg, "can't read font header: %s\n", file);
	error(msg);
	return((char *) -1);
    }

    if (header.magic != VFONT_MAGIC) {
	sprintf(msg, "bad magic number %o in font file\n", header.magic);
	error(msg);
	return((char *) -1);
    }

    filesize = (sizeof(struct header) + 
		sizeof(struct dispatch) * NUM_DISPATCH + header.size);

    if ((image = malloc(filesize)) == NULL) {
	error("not enough memory for font file");
	return((char *) -1);
    }

    lseek(fd, (long) 0, 0);

    if (read(fd, image, filesize) != filesize) {
	error("can't read font file");
	return((char *) -1);
    }

    close(fd);
    return(image);
}  /* end GRReadFontFile */


/*
 * Determine pixel length of string s in font and size.
 */
GRFontStrlen(text, font, size)
char *text;
int font;		/* 1 - 4 */
int size;		/* 1 - 4 */
{
    register struct dispatch *dispatch, *start;
    register length, spacewidth;

    if (*text == '\0')
	return(0);

    if (font_info[font-1][size-1] == NULL)	/* not open yet */
	GROpenFont(font, size);

    if (!GRfontfound(font, size))		/* unreadable font */
	return(0);

    start = (struct dispatch *) (font_info[font-1][size-1] + 
						sizeof(struct header));
    spacewidth = font_sizes[size-1] * (120.0 / 216.0) + 0.5;
    length = 0;
    while (*text != '\0') {
	dispatch = start + (*text);

	if (*text == ' ')
	    length += spacewidth;
	else if (dispatch->nbytes != 0)
	    length += dispatch->width;

	text++;
    }
    
    return(length);
}

/*
 * Display text string of font/size at position pos.
 */
GRPutText(text, font, size, pos)
char *text;
int font, size; 
POINT *pos;
{
    register struct dispatch *dispatch, *dstart;
    register struct mpr_data *mpr_data;
    register char *fbase;
    register width, height, spacewidth;
    int x, y;

    if (font_info[font-1][size-1] == NULL)
	GROpenFont(font, size);
    
    if (!GRfontfound(font, size))
	return;

    dstart = (struct dispatch *) (font_info[font-1][size-1] +
						    sizeof(struct header));
    fbase = (char *) ((char *) dstart + NUM_DISPATCH * sizeof(struct dispatch));

    x = dbx_to_win(pos->x);
    y = dby_to_win(pos->y);

    /* define region of screen to be drawn with text */
    minsunx = x;
    maxsuny = y + 8;	/* catch descenders */
    minsuny = y - GRGetCharYSize(font, size);

    spacewidth = font_sizes[size-1] * (120.0 / 216.0) + 0.5;
    mpr_data = (struct mpr_data *) char_pr->pr_data;
    while (*text != '\0') {
	dispatch = dstart + (*text);

	if (*text == ' ')
	    x += spacewidth;
	else if (dispatch->nbytes != 0) {
	    mpr_data->md_image = (short *) ((char *) fbase + dispatch->addr);
	    char_pr->pr_size.x = width = dispatch->left + dispatch->right;
	    char_pr->pr_size.y = height = dispatch->up + dispatch->down;
	    mpr_data->md_linebytes = ((width + 15) >> 3) & ~1;

	    if (*text != ' ')
		pr_rop(scratch_pr, x - dispatch->left, y - dispatch->up, 
			    width, height, PIX_SRC ^ PIX_DST, char_pr, 0, 0);
	    x += dispatch->width;
	}

	text++;
    }

    maxsunx = x;
} /* end GRPutText */;


/* 
 * Set the actual positioning point for text with the justify, font, and
 * size attributes.  Point is a pointer to the POINT layed down by the user.
 * Pos is a pointer to the returned positioning POINT used in a subsequent
 * call to GRPutText(). 
 */
GRSetTextPos(text, justify, font, size, point, pos)
char *text;
int justify, font, size; 
POINT *point, *pos;
{
    register length;
    register charysize;

    charysize = GRGetCharYSize(font, size);
    length = GRFontStrlen(text, font, size);

    switch (justify) {
	case BOTLEFT:	
	    pos->x = point->x;
	    pos->y = point->y;
	    break;
	case BOTCENT:	
	    pos->x = point->x - (length / 2);
	    pos->y = point->y;
	    break;
	case BOTRIGHT:	
	    pos->x = point->x - length;
	    pos->y = point->y;
	    break;
	case CENTLEFT:	
	    pos->x = point->x;
	    pos->y = point->y - (charysize / 2);
	    break;
	case CENTCENT:	
	    pos->x = point->x - (length / 2);
	    pos->y = point->y - (charysize / 2);
	    break;
	case CENTRIGHT:	
	    pos->x = point->x - length;
	    pos->y = point->y - (charysize / 2);
	    break;
	case TOPLEFT:	
	    pos->x = point->x;
	    pos->y = point->y - charysize;
	    break;
	case TOPCENT:	
	    pos->x = point->x - (length / 2);
	    pos->y = point->y - charysize;
	    break;
	case TOPRIGHT:	
	    pos->x = point->x - length;
	    pos->y = point->y - charysize;
	    break;
    }
}
