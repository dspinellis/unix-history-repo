/*
 * @(#)gremlin.h	1.1	%G%
 *
 * Gremlin library routines header file - 
 * for use with /usr/local/lib/libgremlin.a
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

extern FILE *gr_open();
extern gr_close();
extern gr_vector();
extern gr_curve();
extern gr_arc();
extern gr_circle();
extern gr_polygon();
extern gr_text();

#define GR_ERROR -1
#define GR_OK 0

#define SUN_GREMLIN 0
#define AED_GREMLIN 1

#define HORZ_ORIENT 0
#define VERT_ORIENT 1

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

#define DOTTED 1
#define DOTDASHED 2
#define THICK 3
#define DASHED 4
#define NARROW 5
#define MEDIUM 6
