#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* $Header: gedit.h,v 10.4 86/02/01 16:19:03 tony Rel $ */

#include <stdio.h>

#define	HELPFILE	"GEDIT"		/* Name of .DEF file for help	*/

/* types of things we can edit */
#define SEGMENT	1	/* straight or curved line segments */
#define LABEL	2	/* text */
#define OBJECT	3	/* other files included in this one */

typedef union widget *gptr;	/* pointer to graphics object */

struct segment {
  short type;		/* type of this widget == SEGMENT */
  gptr selink;		/* select link */
  gptr next;		/* next widget */
  struct prototype *parent;  /* our ancestor */
  short x1,y1;		/* starting coord */
  short x2,y2;		/* ending coord */
  short angle;		/* angle subtended by chord, 0 => straight line */
  gptr cache;		/* list of line segments for displaying arc */
};

struct label {
  short type;		/* type of this widget == LABEL */
  gptr selink;		/* select link */
  gptr next;		/* next widget */
  struct prototype *parent;  /* our ancestor */
  short x,y;		/* label point */
  short orient;		/* position wrt to point */
  char *string;		/* the label itself */
};

struct object {
  short type;		/* type of this widget == OBJECT */
  gptr selink;		/* select link */
  gptr next;		/* next widget */
  struct prototype *parent;  /* our ancestor */
  short x,y;		/* coords of origin */
  short orient;		/* one of eight orientations */
  struct prototype *proto;  /* info about the object itself */
  short mscale,dscale;	/* local scaling parameters */
};

union widget {		/* used for pointing at things */
  struct segment s;
  struct label l;
  struct object o;
};

#define NORTH	0	/* normal orientations */
#define EAST	1
#define	SOUTH	2
#define	WEST	3
#define	RNORTH	4	/* reflected orientations */
#define	REAST	5
#define	RSOUTH	6
#define	RWEST	7

/* label orientations */
#define CC	0
#define TC	1
#define BC	2
#define CL	3
#define TL	4
#define BL	5
#define CR	6
#define TR	7
#define BR	8

/* line types */
#define NORMAL	0
#define HIGHLIGHT 1

extern char ocomp[8][8];	/* orientation composition matrix */
extern char lcomp[8][9];

struct state {
  struct prototype *curobj;  /* current prototype, if any */
  short mscale,dscale;	/* display scale */
  short worgx,worgy;	/* coords of lower left corner of window */
  short wmaxx,wmaxy;	/* coords of upper right corner of window */
  short curx,cury;	/* coords of cursor center */
  short oldx,oldy;	/* coords of cursor last time displayed */
  short csize;		/* size of cursor */
  short grid;		/* <>0 if we should display grid points */
  gptr editee;		/* currently selected object(s) */
  short xoff,yoff;	/* offset from cursor to selected object */
  short lxoff,lyoff;	/* offsets from last select/deselect operation */
  short whichend;	/* indicates which end of a selected line */  
};

struct prototype {
  struct prototype *next;	/* linked list of prototypes */
  char *name;			/* name */
  gptr body;			/* what's inside the object */
  struct state recent;		/* copy of most recent edit state, if any */
  char modified;		/* <>0 => modified defn not yet saved */
};

extern struct state cur_state;

extern struct prototype *directory,*read_def();

extern short grid;		/* <>0 display grid points */
extern short incol;		/* current text prompt column */
extern char *prompt;		/* what the current user prompt is */
extern char typein[100];	/* user input buffer */

typedef int (*fptr)();
extern fptr dispatch[];

/* various display parameters */
extern short wminx,wminy,wmaxx,wmaxy;
extern short chrhgt,chrwid;

/* command routine return codes */
#define MULTIPLIER	0x01	/* don't reset multipler to 1 */
#define RECENTER	0x02	/* recenter picture about cursor */
#define REDISPLAY	0x04	/* redraw the picture */
#define DONE		0x08	/* all done with current picture */
#define UPDATE		0x10	/* flags for deselect operation */
#define USEOFFSET	0x20

