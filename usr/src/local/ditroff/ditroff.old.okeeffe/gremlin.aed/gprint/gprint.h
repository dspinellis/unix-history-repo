/*	gprint.h	1.2	83/03/02
 *
 * This file contains standard definitions used by the gprint program.
 */

#define poffset ( (device == 'V') ? 10 : 117 )
#define Ymax 483
#define Xmax 511
#define xorn(x,y) Orientation ? (y) : (x) 
#define yorn(x,y) Orientation ? (Ymax - ((x) - poffset)) : (y) 
#define mapx(x) ((DevRange*(((x) * scale)-botx)/del)+centx)
#define mapy(y) ((DevRange*(del-((y) * scale)+boty)/del)-centy)
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
