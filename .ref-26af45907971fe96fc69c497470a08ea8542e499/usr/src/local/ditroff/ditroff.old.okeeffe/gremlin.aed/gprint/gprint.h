/* gprint.h	1.5	83/05/22
 *
 * This file contains standard definitions used by the gprint program.
 */

#define Vxlen		2048		/* varian dimensions */
#define Vylen		1600
#define Vbytperlin	264

#define Wxlen		2048		/* versatec dimensions */
#define Wylen		2048
#define Wbytperlin	880

#define	MenuSize	116		/* screen dimensions */
#define HXmin		0
#define HXmax		511
#define HYmin		0
#define HYmax		(511 - MenuSize)
#define VXmin		MenuSize
#define VXmax		511
#define VYmin		0
#define VYmax		511

						/* translation stuff */
#define xorn(x,y)	Orientation ? ((y) - VYmin) : ((x) - HXmin)
#define yorn(x,y)	Orientation ? ((x) - VXmin) : (HYmax - (y))
#define mapx(x)		(((x) * scale)-orgx)
#define mapy(y)		(((y) * scale)-orgy)

#define STYLES 6
#define SIZES 4
#define FONTS 4
#define SOLID -1
#define DOTTED 004	/* 014 */
#define DASHED 020	/* 034 */
#define DOTDASHED 024	/* 054 */
#define LONGDASHED 074

#include <stdio.h>
#include <math.h>

#define TRUE	1
#define FALSE	0

#define nullelt	-1
#define nullpt	-1
#define nullun	NULL

#define BOTLEFT	0
#define BOTRIGHT 1
#define CENTCENT 2
#define VECTOR 3
#define ARC 4
#define CURVE 5
#define TOPLEFT 10
#define TOPCENT 11
#define TOPRIGHT 12
#define CENTLEFT 13
#define CENTRIGHT 14
#define BOTCENT 15
#define TEXT(t) ( (t <= CENTCENT) || (t >= TOPLEFT) )
/* WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING 
 *    The above (TEXT) test is dependent on the relative values of the 
 *    constants and will have to change if these values change or if new
 *    commands are added with value greater than BOTCENT
 */

#define NUSER 4
#define NFONTS 4
#define NBRUSHES 6
#define NSIZES 4
#define NJUSTS 9

#define ADD 1
#define DELETE 2
#define MOD 3

typedef struct point {
	float x, y;
	struct point *nextpt;
} POINT;

typedef struct elmt {
	int type, brushf, size, textlength;
	char *textpt;
	POINT *ptlist;
	struct elmt *nextelt, *setnext;
} ELT;

typedef struct unlt {
	int action;
	ELT *(*dbase), *oldelt, *newelt;
	struct unlt *nextun;
} UNELT;

typedef struct {
	int tx_x;	/* First column of field */
	int tx_y;	/* Row containing field */
	int tx_size;	/* Size of field in characters */
} TXFIELD;

#define DBNextElt(elt) elt->nextelt
#define DBNextofSet(elt) elt->setnext
#define DBNullelt(elt) (elt == NULL)
#define Nullpoint(pt)  (pt->x == nullpt)
#define PTNextPoint(pt) pt->nextpt
