/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rcons_subr.c	7.4 (Berkeley) %G%
 *
 * from: $Header: rcons_subr.c,v 1.38 93/04/20 11:15:39 torek Exp $
 */

#ifdef KERNEL
#include <sys/param.h>
#include <sys/fbio.h>
#include <sys/device.h>
#include <machine/fbvar.h>
#else
#include <sys/types.h>
#include "myfbdevice.h"
#endif

#include <sparc/rcons/raster.h>

void rcons_text(struct fbdevice *, char *, int);
void rcons_pctrl(struct fbdevice *, int);
void rcons_esc(struct fbdevice *, int);
void rcons_doesc(struct fbdevice *, int);
void rcons_cursor(struct fbdevice *);
void rcons_invert(struct fbdevice *, int);
void rcons_clear2eop(struct fbdevice *);
void rcons_clear2eol(struct fbdevice *);
void rcons_scroll(struct fbdevice *, int);
void rcons_delchar(struct fbdevice *, int);
void rcons_delline(struct fbdevice *, int);
void rcons_insertchar(struct fbdevice *, int);
void rcons_insertline(struct fbdevice *, int);

extern void rcons_bell(struct fbdevice *);

#define RCONS_ISPRINT(c) ((c) >= ' ' && (c) <= '~')
#define RCONS_ISDIGIT(c) ((c) >= '0' && (c) <= '9')

/* Output (or at least handle) a string sent to the console */
void
rcons_puts(fb, str, n)
	register struct fbdevice *fb;
	register char *str;
	register int n;
{
	register int c, i, j;
	register char *cp;

	/* Jump scroll */
	/* XXX maybe this should be an option? */
	if ((fb->fb_bits & FB_INESC) == 0) {
		/* Count newlines up to an escape sequence */
		i = 0;
		j = 0;
		for (cp = str; j++ < n && *cp != '\033'; ++cp) {
			if (*cp == '\n')
				++i;
			else if (*cp == '\013')
				--i;
		}

		/* Only jump scroll two or more rows */
		if (*fb->fb_row + i >= fb->fb_maxrow + 1) {
			/* Erase the cursor (if necessary) */
			if (fb->fb_bits & FB_CURSOR)
				rcons_cursor(fb);

			rcons_scroll(fb, i);
		}
	}

	/* Process characters */
	while (--n >= 0) {
		c = *str;
		if (c == '\033') {
			/* Start an escape (perhaps aborting one in progress) */
			fb->fb_bits |= FB_INESC | FB_P0_DEFAULT | FB_P1_DEFAULT;
			fb->fb_bits &= ~(FB_P0 | FB_P1);

			/* Most parameters default to 1 */
			fb->fb_p0 = fb->fb_p1 = 1;
		} else if (fb->fb_bits & FB_INESC) {
			rcons_esc(fb, c);
		} else {
			/* Erase the cursor (if necessary) */
			if (fb->fb_bits & FB_CURSOR)
				rcons_cursor(fb);

			/* Display the character */
			if (RCONS_ISPRINT(c)) {
				/* Try to output as much as possible */
				j = fb->fb_maxcol - (*fb->fb_col + 1);
				if (j > n)
					j = n;
				for (i = 1; i < j && RCONS_ISPRINT(str[i]); ++i)
					continue;
				rcons_text(fb, str, i);
				--i;
				str += i;
				n -= i;
			} else
				rcons_pctrl(fb, c);
		}
		++str;
	}
	/* Redraw the cursor (if necessary) */
	if ((fb->fb_bits & FB_CURSOR) == 0)
		rcons_cursor(fb);
}

/* Actually write a string to the frame buffer */
void
rcons_text(fb, str, n)
	register struct fbdevice *fb;
	register char *str;
	register int n;
{
	register int x, y, op;

	x = *fb->fb_col * fb->fb_font->width + fb->fb_xorigin;
	y = *fb->fb_row * fb->fb_font->height +
	    fb->fb_font_ascent + fb->fb_yorigin;
	op = RAS_SRC;
	if (((fb->fb_bits & FB_STANDOUT) != 0) ^
	    ((fb->fb_bits & FB_INVERT) != 0))
		op = RAS_NOT(op);
	raster_textn(fb->fb_sp, x, y, op, fb->fb_font, str, n);
	*fb->fb_col += n;
	if (*fb->fb_col >= fb->fb_maxcol) {
		*fb->fb_col = 0;
		(*fb->fb_row)++;
	}
	if (*fb->fb_row >= fb->fb_maxrow)
		rcons_scroll(fb, 1);
}

/* Handle a control character sent to the console */
void
rcons_pctrl(fb, c)
	register struct fbdevice *fb;
	register int c;
{

	switch (c) {

	case '\r':	/* Carriage return */
		*fb->fb_col = 0;
		break;

	case '\b':	/* Backspace */
		if (*fb->fb_col > 0)
			(*fb->fb_col)--;
		break;

	case '\013':	/* Vertical tab */
		if (*fb->fb_row > 0)
			(*fb->fb_row)--;
		break;

	case '\f':	/* Formfeed */
		*fb->fb_row = *fb->fb_col = 0;
		rcons_clear2eop(fb);
		break;

	case '\n':	/* Linefeed */
		(*fb->fb_row)++;
		if (*fb->fb_row >= fb->fb_maxrow)
			rcons_scroll(fb, 1);
		break;

	case '\007':	/* Bell */
		rcons_bell(fb);
		break;

	case '\t':	/* Horizontal tab */
		*fb->fb_col = (*fb->fb_col + 8) & ~7;
		if (*fb->fb_col >= fb->fb_maxcol)
			*fb->fb_col = fb->fb_maxcol - 1;
		break;
	}
}

/* Handle the next character in an escape sequence */
void
rcons_esc(fb, c)
	register struct fbdevice *fb;
	register int c;
{

	if (c == '[') {
		/* Parameter 0 */
		fb->fb_bits &= ~FB_P1;
		fb->fb_bits |= FB_P0;
	} else if (c == ';') {
		/* Parameter 1 */
		fb->fb_bits &= ~FB_P0;
		fb->fb_bits |= FB_P1;
	} else if (RCONS_ISDIGIT(c)) {
		/* Add a digit to a parameter */
		if (fb->fb_bits & FB_P0) {
			/* Parameter 0 */
			if (fb->fb_bits & FB_P0_DEFAULT) {
				fb->fb_bits &= ~FB_P0_DEFAULT;
				fb->fb_p0 = 0;
			}
			fb->fb_p0 *= 10;
			fb->fb_p0 += c - '0';
		} else if (fb->fb_bits & FB_P1) {
			/* Parameter 1 */
			if (fb->fb_bits & FB_P1_DEFAULT) {
				fb->fb_bits &= ~FB_P1_DEFAULT;
				fb->fb_p1 = 0;
			}
			fb->fb_p1 *= 10;
			fb->fb_p1 += c - '0';
		}
	} else {
		/* Erase the cursor (if necessary) */
		if (fb->fb_bits & FB_CURSOR)
			rcons_cursor(fb);

		/* Process the completed escape sequence */
		rcons_doesc(fb, c);
		fb->fb_bits &= ~FB_INESC;
	}
}

/* Process a complete escape sequence */
void
rcons_doesc(fb, c)
	register struct fbdevice *fb;
	register int c;
{

#ifdef notdef
	/* XXX add escape sequence to enable visual (and audible) bell */
	fb->fb_bits = FB_VISBELL;
#endif

	switch (c) {

	case '@':
		/* Insert Character (ICH) */
		rcons_insertchar(fb, fb->fb_p0);
		break;

	case 'A':
		/* Cursor Up (CUU) */
		*fb->fb_row -= fb->fb_p0;
		if (*fb->fb_row < 0)
			*fb->fb_row = 0;
		break;

	case 'B':
		/* Cursor Down (CUD) */
		*fb->fb_row += fb->fb_p0;
		if (*fb->fb_row >= fb->fb_maxrow)
			*fb->fb_row = fb->fb_maxrow - 1;
		break;

	case 'C':
		/* Cursor Forward (CUF) */
		*fb->fb_col += fb->fb_p0;
		if (*fb->fb_col >= fb->fb_maxcol)
			*fb->fb_col = fb->fb_maxcol - 1;
		break;

	case 'D':
		/* Cursor Backward (CUB) */
		*fb->fb_col -= fb->fb_p0;
		if (*fb->fb_col < 0)
			*fb->fb_col = 0;
		break;

	case 'E':
		/* Cursor Next Line (CNL) */
		*fb->fb_col = 0;
		*fb->fb_row += fb->fb_p0;
		if (*fb->fb_row >= fb->fb_maxrow)
			*fb->fb_row = fb->fb_maxrow - 1;
		break;

	case 'f':
		/* Horizontal And Vertical Position (HVP) */
	case 'H':
		/* Cursor Position (CUP) */
		*fb->fb_col = fb->fb_p1 - 1;
		if (*fb->fb_col < 0)
			*fb->fb_col = 0;
		else if (*fb->fb_col >= fb->fb_maxcol)
			*fb->fb_col = fb->fb_maxcol - 1;

		*fb->fb_row = fb->fb_p0 - 1;
		if (*fb->fb_row < 0)
			*fb->fb_row = 0;
		else if (*fb->fb_row >= fb->fb_maxrow)
			*fb->fb_row = fb->fb_maxrow - 1;
		break;

	case 'J':
		/* Erase in Display (ED) */
		rcons_clear2eop(fb);
		break;

	case 'K':
		/* Erase in Line (EL) */
		rcons_clear2eol(fb);
		break;

	case 'L':
		/* Insert Line (IL) */
		rcons_insertline(fb, fb->fb_p0);
		break;

	case 'M':
		/* Delete Line (DL) */
		rcons_delline(fb, fb->fb_p0);
		break;

	case 'P':
		/* Delete Character (DCH) */
		rcons_delchar(fb, fb->fb_p0);
		break;

	case 'm':
		/* Select Graphic Rendition (SGR); */
		/* (defaults to zero) */
		if (fb->fb_bits & FB_P0_DEFAULT)
			fb->fb_p0 = 0;
		if (fb->fb_p0)
			fb->fb_bits |= FB_STANDOUT;
		else
			fb->fb_bits &= ~FB_STANDOUT;
		break;

	case 'p':
		/* Black On White (SUNBOW) */
		rcons_invert(fb, 0);
		break;

	case 'q':
		/* White On Black (SUNWOB) */
		rcons_invert(fb, 1);
		break;

	case 'r':
		/* Set scrolling (SUNSCRL) */
		/* (defaults to zero) */
		if (fb->fb_bits & FB_P0_DEFAULT)
			fb->fb_p0 = 0;
		/* XXX not implemented yet */
		fb->fb_scroll = fb->fb_p0;
		break;

	case 's':
		/* Reset terminal emulator (SUNRESET) */
		fb->fb_bits &= ~FB_STANDOUT;
		fb->fb_scroll = 0;
		if (fb->fb_bits & FB_INVERT)
			rcons_invert(fb, 0);
		break;
	}
}

/* Paint (or unpaint) the cursor */
void
rcons_cursor(fb)
	register struct fbdevice *fb;
{
	register int x, y;

	x = *fb->fb_col * fb->fb_font->width + fb->fb_xorigin;
	y = *fb->fb_row * fb->fb_font->height + fb->fb_yorigin;
	raster_op(fb->fb_sp, x, y,
#ifdef notdef
	    /* XXX This is the right way but too slow */
	    fb->fb_font->chars[(int)' '].r->width,
	    fb->fb_font->chars[(int)' '].r->height,
#else
	    fb->fb_font->width, fb->fb_font->height,
#endif
	    RAS_INVERT, (struct raster *) 0, 0, 0);
	fb->fb_bits ^= FB_CURSOR;
}

/* Possibly change to SUNWOB or SUNBOW mode */
void
rcons_invert(fb, wob)
	struct fbdevice *fb;
	int wob;
{
	if (((fb->fb_bits & FB_INVERT) != 0) ^ wob) {
		/* Invert the display */
		raster_op(fb->fb_sp, 0, 0, fb->fb_sp->width, fb->fb_sp->height,
		    RAS_INVERT, (struct raster *) 0, 0, 0);

		/* Swap things around */
		fb->fb_ras_blank = RAS_NOT(fb->fb_ras_blank);
		fb->fb_bits ^= FB_INVERT;
	}
}

/* Clear to the end of the page */
void
rcons_clear2eop(fb)
	register struct fbdevice *fb;
{
	register int y;

	if (*fb->fb_col == 0 && *fb->fb_row == 0) {
		/* Clear the entire frame buffer */
		raster_op(fb->fb_sp, 0, 0,
		    fb->fb_sp->width, fb->fb_sp->height,
		    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
	} else {
		/* Only clear what needs to be cleared */
		rcons_clear2eol(fb);
		y = (*fb->fb_row + 1) * fb->fb_font->height;

		raster_op(fb->fb_sp, fb->fb_xorigin, fb->fb_yorigin + y,
		    fb->fb_emuwidth, fb->fb_emuheight - y,
		    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
	}
}

/* Clear to the end of the line */
void
rcons_clear2eol(fb)
	register struct fbdevice *fb;
{
	register int x;

	x = *fb->fb_col * fb->fb_font->width;

	raster_op(fb->fb_sp,
	    fb->fb_xorigin + x,
	    *fb->fb_row * fb->fb_font->height + fb->fb_yorigin,
	    fb->fb_emuwidth - x, fb->fb_font->height,
	    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}

/* Scroll up one line */
void
rcons_scroll(fb, n)
	register struct fbdevice *fb;
	register int n;
{
	register int ydiv;

	/* Can't scroll more than the whole screen */
	if (n > fb->fb_maxrow)
		n = fb->fb_maxrow;

	/* Calculate new row */
	*fb->fb_row -= n;
	if (*fb->fb_row < 0)
		*fb->fb_row  = 0;

	/* Calculate number of pixels to scroll */
	ydiv = fb->fb_font->height * n;

	raster_op(fb->fb_sp, fb->fb_xorigin, fb->fb_yorigin,
	    fb->fb_emuwidth, fb->fb_emuheight - ydiv,
	    RAS_SRC, fb->fb_sp, fb->fb_xorigin, ydiv + fb->fb_yorigin);

	raster_op(fb->fb_sp,
	    fb->fb_xorigin, fb->fb_yorigin + fb->fb_emuheight - ydiv,
	    fb->fb_emuwidth, ydiv, fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}

/* Delete characters */
void
rcons_delchar(fb, n)
	register struct fbdevice *fb;
	register int n;
{
	register int tox, fromx, y, width;

	/* Can't delete more chars than there are */
	if (n > fb->fb_maxcol - *fb->fb_col)
		n = fb->fb_maxcol - *fb->fb_col;

	fromx = (*fb->fb_col + n) * fb->fb_font->width;
	tox = *fb->fb_col * fb->fb_font->width;
	y = *fb->fb_row * fb->fb_font->height;
	width = n * fb->fb_font->width;

	raster_op(fb->fb_sp, tox + fb->fb_xorigin, y + fb->fb_yorigin,
	    fb->fb_emuwidth - fromx, fb->fb_font->height,
	    RAS_SRC, fb->fb_sp, fromx + fb->fb_xorigin, y + fb->fb_yorigin);

	raster_op(fb->fb_sp,
	    fb->fb_emuwidth - width + fb->fb_xorigin, y + fb->fb_yorigin,
	    width, fb->fb_font->height,
	    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}

/* Delete a number of lines */
void
rcons_delline(fb, n)
	register struct fbdevice *fb;
	register int n;
{
	register int fromy, toy, height;

	/* Can't delete more lines than there are */
	if (n > fb->fb_maxrow - *fb->fb_row)
		n = fb->fb_maxrow - *fb->fb_row;

	fromy = (*fb->fb_row + n) * fb->fb_font->height;
	toy = *fb->fb_row * fb->fb_font->height;
	height = fb->fb_font->height * n;

	raster_op(fb->fb_sp, fb->fb_xorigin, toy + fb->fb_yorigin,
	    fb->fb_emuwidth, fb->fb_emuheight - fromy, RAS_SRC,
	    fb->fb_sp, fb->fb_xorigin, fromy + fb->fb_yorigin);

	raster_op(fb->fb_sp,
	    fb->fb_xorigin, fb->fb_emuheight - height + fb->fb_yorigin,
	    fb->fb_emuwidth, height,
	    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}

/* Insert some characters */
void
rcons_insertchar(fb, n)
	register struct fbdevice *fb;
	register int n;
{
	register int tox, fromx, y;

	/* Can't insert more chars than can fit */
	if (n > fb->fb_maxcol - *fb->fb_col)
		n = fb->fb_maxcol - *fb->fb_col;

	tox = (*fb->fb_col + n) * fb->fb_font->width;
	fromx = *fb->fb_col * fb->fb_font->width;
	y = *fb->fb_row * fb->fb_font->height;

	raster_op(fb->fb_sp, tox + fb->fb_xorigin, y + fb->fb_yorigin,
	    fb->fb_emuwidth - tox, fb->fb_font->height,
	    RAS_SRC, fb->fb_sp, fromx + fb->fb_xorigin, y + fb->fb_yorigin);

	raster_op(fb->fb_sp, fromx + fb->fb_xorigin, y + fb->fb_yorigin,
	    fb->fb_font->width * n, fb->fb_font->height,
	    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}

/* Insert some lines */
void
rcons_insertline(fb, n)
	register struct fbdevice *fb;
	register int n;
{
	register int fromy, toy;

	/* Can't insert more lines than can fit */
	if (n > fb->fb_maxrow - *fb->fb_row)
		n = fb->fb_maxrow - *fb->fb_row;

	toy = (*fb->fb_row + n) * fb->fb_font->height;
	fromy = *fb->fb_row * fb->fb_font->height;

	raster_op(fb->fb_sp, fb->fb_xorigin, toy + fb->fb_yorigin,
	    fb->fb_emuwidth, fb->fb_emuheight - toy,
	    RAS_SRC, fb->fb_sp, fb->fb_xorigin, fromy + fb->fb_yorigin);

	raster_op(fb->fb_sp, fb->fb_xorigin, fromy + fb->fb_yorigin,
	    fb->fb_emuwidth, fb->fb_font->height * n,
	    fb->fb_ras_blank, (struct raster *) 0, 0, 0);
}
