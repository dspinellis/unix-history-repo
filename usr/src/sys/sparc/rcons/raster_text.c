/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to the Computer Systems
 * Engineering Group at Lawrence Berkeley Laboratory and to the University
 * of California at Berkeley by Jef Poskanzer.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)raster_text.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: raster_text.c,v 1.15 92/06/17 08:14:45 torek Exp $
 */

/*
 * Text routines for raster library.
 */

#ifdef KERNEL
#include <sys/param.h>
#include <sparc/rcons/raster.h>
#ifdef COLORFONT_CACHE
#include <sys/malloc.h>
#define NEW(size) malloc(size, M_DEVBUF, M_NOWAIT)
#endif
#else
#include <sys/types.h>
#include <sparc/rcons/raster.h>
#ifdef COLORFONT_CACHE
#include <malloc.h>
#define NEW(size) malloc(size)
#endif
#endif


/* Draws text.  Returns 0 on success, -1 on failure. */
int
raster_text( r, x, y, rop, rf, text )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    {
    return raster_textn( r, x, y, rop, rf, text, strlen( text ) );
    }

/* Draws n characters of text.  Returns 0 on success, -1 on failure. */
int
raster_textn( r, x, y, rop, rf, text, n )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    int n;
    {
    int clip;
    int x1, y1;
    struct raster_char* c;
    struct raster* charrast;
    int i;
    register char ch;
    int thisx, thisy;
    int phase;

    /* Check whether we can avoid clipping. */
    clip = 0;
    if ( rf->flags & RASFONT_FIXEDWIDTH &&
	 rf->flags & RASFONT_NOVERTICALMOVEMENT )
	{
	/* This font is well-behaved, we can compute the extent cheaply. */
	c = &(rf->chars['@']);
	charrast = c->r;
	if ( x + c->homex < 0 || y + c->homey < 0 ||
	     x + c->homex + n * c->nextx > r->width ||
	     y + c->homey + charrast->height > r->height )
	    clip = 1;
	}
    else
	{
	/* Got to step through the string to compute the extent. */
	for ( i = 0, x1 = x, y1 = y;
	      i < n;
	      ++i, x1 += c->nextx, y1 += c->nexty )
	    {
	    c = &(rf->chars[text[i]]);
	    charrast = c->r;
	    if ( charrast != (struct raster*) 0 )
		{
		if ( x1 + c->homex < 0 || y1 + c->homey < 0 ||
		     x1 + c->homex + charrast->width > r->width ||
		     y1 + c->homey + charrast->height > r->height )
		    {
		    clip = 1;
		    break;
		    }
		}
	    }
	}

    /* Now display the text. */
    for ( i = 0, x1 = x, y1 = y;
	  i < n;
	  ++i, x1 += c->nextx, y1 += c->nexty )
	{
	ch = text[i];
	c = &(rf->chars[ch]);
	charrast = c->r;
	if ( charrast != (struct raster*) 0 )
	    {
	    thisx = x1 + c->homex;
	    thisy = y1 + c->homey;

	    phase = 0;
#ifdef COLORFONT_CACHE
	    if ( r->depth == 8 )
		{
		/* Initialize color font cache if necessary. */
		if ( rf->cache == (struct raster_fontcache*) -1 )
		    {
		    int c;

		    rf->cache = (struct raster_fontcache*)
			NEW( sizeof(struct raster_fontcache) );
		    if ( rf->cache != (struct raster_fontcache*) 0 )
			for ( c = 0; c < 256; ++c )
			    rf->cache->cr[c] = (struct raster*) 0;
		    }

		if ( rf->cache != (struct raster_fontcache*) 0 )
		    {
		    int color;
		    struct raster* cr;

		    color = RAS_GETCOLOR( rop );
		    cr = rf->cache->cr[ch];
		    /* Is this character cached yet? */
		    if ( cr != (struct raster*) 0 )
			{
			/* Yes, but is it the right color? */
			if ( rf->cache->color[ch] == color )
			    {
			    /* Yes - switch rasters. */
			    charrast = cr;
			    }
			else
			    {
			    /* No, re-draw it. */
			    if ( raster_op_noclip(
				 cr, 0, 0, charrast->width,
				 charrast->height, rop, charrast, 0, 0 ) == 0 )
				{
				rf->cache->color[ch] = color;
				charrast = cr;
				}
			    }
			}
		    else
			{
			/* It's not cached, so cache it. */
			cr = raster_alloc(
			    charrast->width, charrast->height, 8 );
			if ( cr != (struct raster*) 0 )
			    if ( raster_op_noclip(
				 cr, 0, 0, charrast->width, charrast->height,
				 rop, charrast, 0, 0 ) == 0 )
				{
				rf->cache->color[ch] = color;
				charrast = rf->cache->cr[ch] = cr;
				}
			}
		    }
		}
#endif /*COLORFONT_CACHE*/

	    if ( clip )
		{
		if ( raster_op(
			 r, thisx, thisy, charrast->width, charrast->height,
			 rop, charrast, phase, 0 ) < 0 )
		    return -1;
		}
	    else
		{
		if ( raster_op_noclip(
			 r, thisx, thisy, charrast->width, charrast->height,
			 rop, charrast, phase, 0 ) < 0 )
		    return -1;
		}
	    }
	}

    return 0;
    }

#ifdef COLORFONT_CACHE
/* Allocates a raster.  Returns (struct raster*) 0 on failure. */
struct raster*
raster_alloc( width, height, depth )
    int width, height, depth;
    {
    struct raster* r;
    int linelongs;

    if ( width <= 0 || height <= 0 || ( depth != 1 && depth != 8 ) )
	return (struct raster*) 0;
    linelongs = ( ( width * depth + 31 ) >> 5 );
    r = (struct raster*)
	NEW( sizeof(struct raster) + height * linelongs * sizeof(u_long));
    if ( r == (struct raster*) 0 )
	return (struct raster*) 0;

    r->width = width;
    r->height = height;
    r->depth = depth;
    r->linelongs = linelongs;
    r->pixels = (u_long*) (r + 1);
    r->data = (caddr_t) 0;
    return r;
    }
#endif
