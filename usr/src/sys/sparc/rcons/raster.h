/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to the Computer Systems
 * Engineering Group at Lawrence Berkeley Laboratory and to the University
 * of California at Berkeley by Jef Poskanzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)raster.h	7.1 (Berkeley) %G%
 *
 * from: $Header: raster.h,v 1.14 92/06/17 08:14:43 torek Exp $
 */

/*
 * Simple raster and frame buffer routines.
 *
 * Currently this set of routines is fairly minimal.  It's enough to
 * implement a console terminal emulator on monochrome and pseudocolor
 * screens, and that's about it.
 *
 * Future additions might be other kinds of frame buffers (direct color?),
 * lines, dashed lines, three-operand blits (stipples/stencils), etc.
 */

#ifndef _RASTER_H_
#define _RASTER_H_

/* Configurable definitions. */

/* CONFIGURE: define or undef for your machine's byte order */
#define MSBYTE_FIRST

/* CONFIGURE: define or under for your frame buffer's bit order */
#define MSBIT_FIRST

/* CONFIGURE: The text routines can optionally keep a cache of 8-bit
** characters.  This uses about 30K, but makes text on a color screen
** go 3.2 times faster.
*/
#undef COLORFONT_CACHE


/* Definitions. */

/* ANSI prototype conditionalizer.  */
#ifndef ARGS
#if __STDC__
#define ARGS(alist) alist
#else /*__STDC__*/
#define ARGS(alist) ()
#endif /*__STDC__*/
#endif /*ARGS*/

/* Raster struct. */
struct raster {
    int width, height;	/* size in pixels */
    int depth;		/* bits per pixel - 1 or 8 */
    int linelongs;	/* longs from one line to the next - for padding */
    u_long* pixels;	/* pointer to the actual bits */
    caddr_t data;	/* special pointer for frame buffers and subregions */
    };

/* Colormap struct. */
struct raster_colormap {
    int length;
    u_char* red;
    u_char* grn;
    u_char* blu;
    };

/* Font character struct. */
struct raster_char {
    struct raster* r;
    int homex, homey;
    int nextx, nexty;
    };

#ifdef COLORFONT_CACHE
struct raster_fontcache {
    struct raster* cr[256];
    u_char color[256];
    };
#endif /*COLORFONT_CACHE*/

/* Font struct. */
struct raster_font {
    int width, height;	/* nominal character size */
    int flags;
#define RASFONT_FIXEDWIDTH		0x1
#define RASFONT_NOVERTICALMOVEMENT	0x2
    struct raster_char chars[256];
#ifdef COLORFONT_CACHE
    struct raster_fontcache* cache;
#endif /*COLORFONT_CACHE*/
    };

/* Defines for the raster_op() and raster_text() rop parameter - the bitblit
** operation.  A rop can be some Boolean combination of RAS_SRC and
** RAS_DST.  For instance, just RAS_SRC means copy the source to the
** destination without modification.  RAS_SRC|RAS_DST means "or" the source
** and destination together, while "xor" would be RAS_SRC^RAS_DST.  The
** RAS_NOT macro should be used to express negation - RAS_NOT(RAS_SRC)&RAS_DST
** would "and" the complement of the source with the destination.
**
** Or, you can just use one of the pre-defined ops.  There are only 16
** possible combinations, so all 16 are defined here.
**
** For color rasters, you specify the color of the operation by simply
** oring RAS_COLOR(color) into the rop.
*/

#define RAS_NOT(op) ( 0xf & ( ~ (op) ) )

#define RAS_CLEAR		0x0	/* 0 */
#define RAS_NOTOR		0x1	/* !( src | dst ) */
#define RAS_NOTSRC_AND_DST	0x2	/* !src & dst */
#define RAS_INVERTSRC		0x3	/* !src */
#define RAS_SRC_AND_NOTDST	0x4	/* src & !dst */
#define RAS_INVERT		0x5	/* !dst */
#define RAS_XOR			0x6	/* src ^ dst */
#define RAS_NOTAND		0x7	/* !( src & dst ) */
#define RAS_AND			0x8	/* src & dst */
#define RAS_NOTXOR		0x9	/* !( src ^ dst ) */
#define RAS_DST			0xa	/* dst */
#define RAS_NOTSRC_OR_DST	0xb	/* !src | dst */
#define RAS_SRC			0xc	/* src */
#define RAS_SRC_OR_NOTDST	0xd	/* src | !dst */
#define RAS_OR			0xe	/* src | dst */
#define RAS_SET			0xf	/* 1 */

#define RAS_COLOR(color) ( ( (color) & 0xff ) << 4 )

/* Get the op from a rop. */
#define RAS_GETOP(op) ( (op) & 0xf )
/* Get the color from a rop. */
#define RAS_GETCOLOR(op) ( ( (op) >> 4 ) & 0xff )
/* Get the longword address of a pixel. */
#define RAS_ADDR( r, x, y ) \
    ( (r)->pixels + (y) * (r)->linelongs + (x) * (r)->depth / 32 )


/* Raster routines. */

extern struct raster* raster_alloc ARGS(( int width, int height, int depth ));
/* Allocates a raster.  Returns (struct raster*) 0 on failure. */

extern void raster_free ARGS(( struct raster* r ));
/* Frees/closes a raster. */

extern int raster_get ARGS(( struct raster* r, int x, int y ));
/* Gets a single pixel from a raster. */

extern void raster_put ARGS(( struct raster* r, int x, int y, int v ));
/* Puts a single pixel into a raster. */

extern struct raster* raster_subregion ARGS(( struct raster* r, int x, int y, int width, int height ));
/* Makes a raster that points to a region of another.  Returns
** (struct raster*) 0 on failure.
*/


/* Raster operations.  */

extern int raster_op ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Performs a bitblit.  Returns 0 on success, -1 on failure.  */

extern int raster_op_noclip ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Bitblit without clipping.  Returns 0 on success, -1 on failure. */

extern int raster_op_nosrc_noclip ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop ));
/* No-src bitblit without clipping.  Returns 0 on success, -1 on failure. */

extern int raster_replsrc ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Tiles the src to fit the dst.  Returns 0 on success, -1 on failure.  Only
** implements RAS_SRC.
*/


/* Raster text routines */

extern struct raster_font* raster_fontopen ARGS(( char* fontname ));
/* Opens a font. Returns (struct raster_font*) 0 on failure. */

extern int raster_text ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text ));
/* Draws text.  Returns 0 on success, -1 on failure. */

extern int raster_textn ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text, int len ));
/* Draws n characters of text.  Returns 0 on success, -1 on failure. */

extern void raster_fontclose ARGS(( struct raster_font* rf ));
/* Closes a font. */


/* Frame buffer routines. */

extern struct raster* raster_open ARGS(( char* fbname ));
/* Opens a frame buffer as a raster.  Returns (struct raster*) 0 on failure. */

extern struct raster* raster_coloropen ARGS(( void ));
/* Opens a color frame buffer if there is one.  Returns (struct raster*) 0 on
** failure.
*/

extern int raster_video_off ARGS(( struct raster* r ));
/* Blanks the screen.  Returns 0 on success, -1 on failure.  This might
** be implemented as actual video blanking, or it might just load black
** into all colormap entries (and disable further colormap changes).
*/

extern int raster_video_on ARGS(( struct raster* r ));
/* Re-enables video.  Returns 0 on success, -1 on failure. */

extern struct raster_colormap* raster_colormap_alloc ARGS(( int length ));
/* Allocates a colormap structure, returns 0 on failure. */

extern struct raster_colormap* raster_colormap_get ARGS(( struct raster* r ));
/* Allocates a colormap structure and returns the frame buffer's
** current colormap, or (struct raster_colormap*) 0 on failure.  The raster
** must be one returned by raster_open(), not raster_alloc().
*/

extern int raster_colormap_set ARGS(( struct raster* r, struct raster_colormap* cm ));
/* Sets a frame buffer's colormap.  The raster must be one returned
** by raster_open(), not raster_alloc().  Returns 0 on success, -1 on
** failure.
*/

extern void raster_colormap_free ARGS(( struct raster_colormap* cm ));
/* Frees a colormap. */

#endif /*_RASTER_H_*/
