#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* $Header: Xint.h,v 10.8 86/02/01 15:15:27 tony Rel $ */

/* Internal definitions for X server */

#include <sys/param.h>
#include <stdio.h>
#include "X.h"
#include "Xproto.h"
#include "vsinput.h"
#include "Xdev.h"

#define funclim 16			/* max GXfunction + 1 */

#define maxsocks (NOFILE - 1)		/* maximum open sockets */
#define mskcnt ((maxsocks + 31) / 32)	/* size of bit array */

#if (mskcnt==1)
#define copybits(src, dst) dst[0] = src[0]
#define clearbits(buf) buf[0] = 0
#endif
#if (mskcnt==2)
#define copybits(src, dst) dst[0] = src[0]; dst[1] = src[1]
#define clearbits(buf) buf[0] = 0; buf[1] = 0
#endif
#if (mskcnt==3)
#define copybits(src, dst) dst[0] = src[0]; dst[1] = src[1]; dst[2] = src[2];
#define clearbits(buf) buf[0] = 0; buf[1] = 0; buf[2] = 0
#endif
#if (mskcnt>3)
#define copybits(src, dst) bcopy((caddr_t) src, (caddr_t) dst, sizeof (src))
#define clearbits(buf) bzero((caddr_t) buf, sizeof (buf))
#endif

#if (mskcnt==1)
#define bitmask(i) (1 << (i))
#define maskidx(i) 0
#endif
#if (mskcnt>1)
#define bitmask(i) (1 << ((i) & 31))
#define maskidx(i) ((i) >> 5)
#endif

#define maskword(buf, i) buf[maskidx(i)]
#define bitset(buf, i) maskword(buf, i) |= bitmask(i)
#define bitclear(buf, i) maskword(buf, i) &= ~bitmask(i)
#define getbit(buf, i) (maskword(buf, i) & bitmask(i))

#if (mskcnt==1)
#define singlebit(buf, i) maskword(buf, i) = bitmask(i)
#endif
#if (mskcnt>1)
#define singlebit(buf, i) clearbits(buf); bitset(buf, i)
#endif

typedef struct _resource {
	struct _resource *next, *prev;		/* chain pointers */
	caddr_t	value;				/* the object */
	char type;				/* RT_* */
	long id;				/* resource id */
} RESOURCE;

#define RT_FREE		0
#define RT_WINDOW	1
#define RT_FONT		2
#define RT_BITMAP	3
#define RT_PIXMAP	4
#define RT_CURSOR	5

#define RESIDX(id) ((id) & 0xffff)		/* index in low 16 bits */

/* Now define the rectangle types and the rectangle itself */

#define contents_rec 0
#define new_rec 1
#define border_rec 2

/* The first 4 components must match RASTER */

typedef struct rec {
	short bottom;		/* not inclusive */
	short right;		/* not inclusive */
	short left;
	short top;
	short type;		/* one of *_rec types above */
	short internal;		/* 1: not head of malloc() area */
	struct rec *next;	/* chain pointer */
} RECTANGLE;

typedef struct {		/* components must match REGION */
	short bottom;		/* not inclusive */
	short right;		/* not inclusive */
	short left;
	short top;
} RASTER;

typedef struct {		/* component order dictated by protocol */
	short height;
	short width;
	short left;
	short top;
} REGION;

/* A window */
/*  If mapped is true, coords are absolute (i.e. relative only to the root
 *  window).  Otherwise, they are relative to the parent window's origin.
 */

typedef struct wnode {
	RASTER full;		/* Inside dimensions not clipped by parent */
	RASTER vs;		/* Inside dimensions clipped by parent */
	RASTER ovs;		/* Outside dimensions clipped by parent */
	CLIP clip;		/* vs as clipping rectangle */
	struct wnode *parent;	/* Who contains this window */
	struct wnode *next_sib;	/* Other windows it contains */
	struct wnode *prev_sib;	/* (linked two ways) */
	struct wnode *first_child;	/* Bottom-most window this contains */
	struct wnode *last_child;	/* Top-most window it contains */
	RECTANGLE *visible;	/* List of visible rectangles */
	RECTANGLE *cmvisible;	/* List of visible rectangles when clipmode */
	ushort level;		/* The level in the window hierarchy */
				/*	child.level = parent.level + 1 */
	char unobscured;	/* 0: obscured, 1: unobscured (3 temp), OB_* */
	char kind;		/* 0: transparent, 1: opaque, 2: icon */
	char clipmode;		/* 0: clipped, 1: draw-thru */
	char tilemode;		/* 0: absolute, 1: relative */
	char mapped;		/* 0: unmapped, 1: mapped */
	char should_be_mapped;	/* 0: unmapped, 1: should be mapped */
	CURSOR *cursor;		/* The cursor information */
	PIXMAP *tile;		/* The background tile */
	PIXMAP *border;		/* The border tile */
	short bwidth;		/* The border width */
	char bgrabs;		/* Button grab count */
	char internal;		/* 1: not head of malloc() area */
	long mask;		/* The input event mask */
	int client;		/* Client asking for events */
	long rid;		/* Resource identifier */
	short width0;		/* Minimum width */
	short widthinc;		/* Width increment */
	short height0;		/* Minimum height */
	short heightinc;	/* Height increment */
	char *name;		/* Window name */
	struct wnode *icon;	/* Icon or normal window */
	struct wnode *next;	/* mapped_list chain pointers */
	struct wnode *prev;
} WINDOW;

#define OB_NOT 0
#define OB_YES 1
#define OB_TMP 3		/* not 2, for lsb test */

#define max(x,y) ((x) >= (y) ? (x) : (y))
#define min(x,y) ((x) <= (y) ? (x) : (y))

/* create a new rectangle */
#define NEWRECT(r,lf,rt,tp,bt,ty)	if ((r = free_rectangles) == NULL)\
					    r = Alloc_rectangle ();\
					free_rectangles = r->next;\
					r->left = lf; r->right = rt;\
					r->top = tp; r->bottom = bt;\
					r->type = ty

/* create a new rectangle from a raster */
#define RASTRECT(r,rs,ty)		if ((r = free_rectangles) == NULL)\
					    r = Alloc_rectangle ();\
					free_rectangles = r->next;\
					*(RASTER *) r = rs;\
					r->type = ty

/* free a rectangle */
#define FREERECT(r) 	r->next = free_rectangles; free_rectangles = r

#define BytePad(n) (((n) + 3) & ~3)
#define WordPad(n) (((n) + 3) & ~3)

/* byte swap a long literal */
#define lswapl(x) ((((x) & 0xff) << 24) |\
		   (((x) & 0xff00) << 8) |\
		   (((x) & 0xff000) >> 8) |\
		   (((x) >> 24) & 0xff))
/* byte swap a short literal */
#define lswaps(x) ((((x) & 0xff) << 8) | (((x) >> 8) & 0xff))
/* byte swap a long */
#define swapl(x) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[3];\
		 ((char *) (x))[3] = n;\
		 n = ((char *) (x))[1];\
		 ((char *) (x))[1] = ((char *) (x))[2];\
		 ((char *) (x))[2] = n
/* byte swap a short */
#define swaps(x) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[1];\
		 ((char *) (x))[1] = n
/* byte swap a long parameter */
#define pswapl(x, i) n = (x)->param.b[4*(i)];\
		     (x)->param.b[4*(i)] = (x)->param.b[4*(i)+3];\
		     (x)->param.b[4*(i)+3] = n;\
		     n = (x)->param.b[4*(i)+1];\
		     (x)->param.b[4*(i)+1] = (x)->param.b[4*(i)+2];\
		     (x)->param.b[4*(i)+2] = n
/* byte swap a short parameter */
#define pswaps(x, i) n = (x)->param.b[2*(i)];\
		     (x)->param.b[2*(i)] = (x)->param.b[2*(i)+1];\
		     (x)->param.b[2*(i)+1] = n

#ifdef vax
#define swaptype int
#else
#define swaptype char
#endif

#ifdef vax
#define TRUE(b) ((b) & 1)
#define FALSE(b) (!((b) & 1))
#else
#define TRUE(b) (b)
#define FALSE(b) (!(b))
#endif
