/* $Header: makemask.h,v 10.1 86/11/19 10:45:51 jg Exp $ */
/* makemask.h -  Defines, constants, structures and globals
 *               use by MakeMask()
 *
 *  	Author:
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

/*
 * Edge table entry
 */

static struct edge {
    struct edge *NextEdge;	/* pointer to next edge */
    long      	Delta_X;        /* change in x per scan line */
    long      	Min_X;      	/* current x value */
    short	Max_Y;		/* last scan line of edge */
};

/*
 * Ploygon 
 */

static struct Polygon {
	Vertex	*PolyPoints;	/* pointer to list of points */
	int	PolyCount;	/* number of points in polygon */
};

/*
 * Left and right mask values used by FillMask()
 */

static long LeftMasks[] = {
	0x80000000, 0xC0000000, 0xE0000000, 0xF0000000,
	0xF8000000, 0xFC000000, 0xFE000000, 0xFF000000,
	0xFF800000, 0xFFC00000, 0xFFE00000, 0xFFF00000,
	0xFFF80000, 0xFFFC0000, 0xFFFE0000, 0xFFFF0000,
	0xFFFF8000, 0xFFFFC000, 0xFFFFE000, 0xFFFFF000,
	0xFFFFF800, 0xFFFFFC00, 0xFFFFFE00, 0xFFFFFF00,
	0xFFFFFF80, 0xFFFFFFC0, 0xFFFFFFE0, 0xFFFFFFF0,
	0xFFFFFFF8, 0xFFFFFFFC, 0xFFFFFFFE, 0xFFFFFFFF,
};

static long RightMasks[] = {
	0xFFFFFFFF, 0x7FFFFFFF, 0x3FFFFFFF, 0x1FFFFFFF,
	0x0FFFFFFF, 0x07FFFFFF, 0x03FFFFFF, 0x01FFFFFF,
	0x00FFFFFF, 0x007FFFFF, 0x003FFFFF, 0x001FFFFF,
	0x000FFFFF, 0x0007FFFF, 0x0003FFFF, 0x0001FFFF,
	0x0000FFFF, 0x00007FFF, 0x00003FFF, 0x00001FFF,
	0x00000FFF, 0x000007FF, 0x000003FF, 0x000001FF,
	0x000000FF, 0x0000007F, 0x0000003F, 0x0000001F,
	0x0000000F, 0x00000007, 0x00000003, 0x00000001,
};

/*
 * global variables
 */

static int EdgeCount;
static struct edge **EdgeTable;
static struct edge *Edges;
static int Direction;
static int ShortenStartOfEdge;
