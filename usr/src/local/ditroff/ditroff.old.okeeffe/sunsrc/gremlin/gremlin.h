/*
 * @(#)gremlin.h	1.1	%G%
 *
 * Standard definitions used by the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <stdio.h>
#include <math.h>

#define nullpt  -1

#define MAXPOINTS 200

#define pixmask     1
#define csetmask    2
#define scratchmask 4

#define BOTLEFT 0
#define BOTRIGHT 1
#define CENTCENT 2
#define VECTOR 3
#define ARC 4
#define CURVE 5
#define POLYGON 6
#define TOPLEFT 10
#define TOPCENT 11
#define TOPRIGHT 12
#define CENTLEFT 13
#define CENTRIGHT 14
#define BOTCENT 15
#define TEXT(t) ( ((t) <= CENTCENT) || ((t) >= TOPLEFT) )
/* WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING 
 *    The above (TEXT) test is dependent on the relative values of the 
 *    constants and will have to change if these values change or if new
 *    commands are added with value greater than BOTCENT
 */


#define RFONT 0
#define IFONT 1
#define BFONT 2
#define SFONT 3

#define NOADJ 0
#define HORZ  1
#define VERT  2
#define MAN   3

#define NUSER 4
#define NFONTS 4
#define NBRUSHES 6
#define NSIZES 4
#define NSTIPPLES 8
#define NJUSTS 9
#define JUSTMODES 17

#define ADD 1
#define DELETE 2
#define MOD 3

#define Delimiter(c) ((c == '\0') || (c == ' ') || (c == '\t'))

#define MINMAX(min, max, val)   if (val < min) \
				    min = val; \
				else if (val > max) \
				    max = val;

/* The following represents the maximum distance a point may be from another
 * and still be affected by gravity.  The distance is represented as the
 * square of the number of pixels (32) of the actual distance.  
 */

#define MAXGDIST 1024	/* 32 * 32 */

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


/* 
 * database macros
 */

#define DBInit()	  ((ELT *) NULL)
#define DBNextElt(elt)    ((elt)->nextelt)
#define DBNextofSet(elt)  ((elt)->setnext)
#define DBNullelt(elt)    ((elt) == NULL)

#define PTInit()	  ((POINT *) NULL)
#define PTNextPoint(pt)   ((pt)->nextpt)
#define Nullpoint(pt)     ((pt) == (POINT *) NULL)
 

/* sun version parameters */

/* icon display parameters */

#define ICON_SIZE  16
#define ICON_BORDER  8

#define MENU_TOP  0	/* 17 */
#define MENU_LEFT  0
#define MENU_RIGHT  104
#define MENU_BOTTOM  1000
#define MENU_COLUMNS  4

#define MENU_X  (ICON_SIZE + ICON_BORDER)
#define MENU_Y  (ICON_SIZE + ICON_BORDER)

/* subwindow stuff */

#define TEXTSW_HEIGHT 33
#define MENUSW_WIDTH 104
#define PIXSW_WIDTH 512
#define PIXSW_HEIGHT 512

#define stipple1_pr  white_pr
#define stipple2_pr  gray_pr
#define stipple3_pr  _50_pr
#define stipple4_pr  black_pr

#define is_stipple(pr)  (((pr) == &stipple1_pr) || ((pr) == &stipple2_pr) || \
			 ((pr) == &stipple3_pr) || ((pr) == &stipple4_pr) || \
			 ((pr) == &stipple5_pr) || ((pr) == &stipple6_pr) || \
			 ((pr) == &stipple7_pr) || ((pr) == &stipple8_pr))

#define winx_to_db(x) ((float) ((x) + SUN_XORIGIN))
#define winy_to_db(y) ((float) (SUN_YORIGIN - (y)))
#define dbx_to_win(x) (((int) (x)) - SUN_XORIGIN)
#define dby_to_win(y) (SUN_YORIGIN  - ((int) (y)))

struct _menu {
	    int menu_x;				/* icon x coordinate */
	    int menu_y;				/* icon y coordinate */
	    struct pixrect *menu_icon;		/* icon pixrect pointer */
	    int (*menu_function)();		/* left button function */
	    int (*menu_modify)();		/* middle button function */
	    int (*menu_help)();			/* right button function */
	};

#define TEXT_BASELINE  10
#define TEXT_BUFMAX  128

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif
