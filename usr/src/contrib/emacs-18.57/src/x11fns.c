/* Functions for the X window system.
   Copyright (C) 1988, 1990 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Yakim Martillo; rearranged by Richard Stallman.  */
/* Color and other features added by Robert Krawitz*/
/* Converted to X11 by Robert French */

#include <stdio.h>
#ifdef NULL
#undef NULL
#endif
#include <signal.h>
#include "config.h"

/* Get FIONREAD, if it is available.  */
#ifdef USG
#include <termio.h>
#include <fcntl.h>
#else /* not USG */
#ifndef VMS
#include <sys/ioctl.h>
#endif /* not VMS */
#endif /* not USG */

/* Allow m- file to inhibit use of interrupt-driven input.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try.  */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

#include "lisp.h"
#include "window.h"
#include "x11term.h"
#include "dispextern.h"
#include "termchar.h"
#ifdef USG
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <fcntl.h>
#include <setjmp.h>

#ifdef HAVE_X_WINDOWS

#define abs(x) ((x < 0) ? ((x)) : (x))
#define sgn(x) ((x < 0) ? (-1) : (1))
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))
  
/* Non-nil if Emacs is running with an X window for display.
   Nil if Emacs is run on an ordinary terminal.  */

Lisp_Object Vxterm;

Lisp_Object Vx_mouse_pos;
Lisp_Object Vx_mouse_abs_pos;

Lisp_Object Vx_mouse_item;

extern Lisp_Object MouseMap;

extern Lisp_Object minibuf_window;
extern int minibuf_prompt_width;

extern XEvent *XXm_queue[XMOUSEBUFSIZE];
extern int XXm_queue_num;
extern int XXm_queue_in;
extern int XXm_queue_out;
extern char *fore_color;
extern char *back_color;
extern char *brdr_color;
extern char *mous_color;
extern char *curs_color;

extern unsigned long fore;
extern unsigned long back;
extern unsigned long brdr;
extern unsigned long curs;

extern int XXborder;
extern int XXInternalBorder;

extern char *progname;

extern XFontStruct *fontinfo;
extern Font XXfid;
extern GC XXgc_norm,XXgc_rev,XXgc_curs,XXgc_temp;
extern XGCValues XXgcv;
extern int XXfontw,XXfonth,XXbase,XXisColor;
extern Colormap XXColorMap;

extern int PendingExposure;
extern char *default_window;
extern char *desiredwindow;

extern int XXscreen;
extern Window XXwindow;
extern Cursor EmacsCursor;
extern short MouseCursor[], MouseMask[];
extern char *XXcurrentfont;
extern int informflag;

extern int WindowMapped;
extern int CurHL;
extern int pixelwidth, pixelheight;
extern int XXpid;

extern char *XXidentity;

extern Display *XXdisplay;
extern int bitblt, CursorExists, VisibleX, VisibleY;

check_xterm ()
{
	if (NULL (Vxterm))
		error ("Terminal does not understand X protocol.");
}

DEFUN ("x-set-bell", Fx_set_bell, Sx_set_bell, 1, 1, "P",
  "For X window system, set audible vs visible bell.\n\
With non-nil argument (prefix arg), use visible bell; otherwise, audible bell.")
   (arg)
     Lisp_Object arg;
{
	BLOCK_INPUT_DECLARE ();

	check_xterm ();
	BLOCK_INPUT ();
	if (!NULL (arg))
		XSetFlash ();
	else
		XSetFeep ();
	UNBLOCK_INPUT ();
	return arg;
}

DEFUN ("x-flip-color", Fx_flip_color, Sx_flip_color, 0, 0, "",
  "Toggle the background and foreground colors")
  ()
{
	check_xterm ();
	XFlipColor ();
	return Qt;
}

DEFUN ("x-set-foreground-color", Fx_set_foreground_color,
       Sx_set_foreground_color, 1, 1, "sSet foregroud color:  ",
       "Set foreground (text) color to COLOR.")
  (arg)
     Lisp_Object arg;
{
	XColor cdef;
	BLOCK_INPUT_DECLARE ();
	char *save_color;

	save_color = fore_color;
	check_xterm ();
	CHECK_STRING (arg,1);
	fore_color = (char *) xmalloc (XSTRING (arg)->size + 1);
	bcopy (XSTRING (arg)->data, fore_color, XSTRING (arg)->size + 1);

	BLOCK_INPUT ();

	if (fore_color && XXisColor &&
	    XParseColor (XXdisplay, XXColorMap, fore_color, &cdef) &&
	    XAllocColor(XXdisplay, XXColorMap, &cdef))
		fore = cdef.pixel;
	else
		if (fore_color && !strcmp (fore_color, "black"))
			fore = BlackPixel (XXdisplay, XXscreen);
		else
			if (fore_color && !strcmp (fore_color, "white"))
				fore = WhitePixel (XXdisplay, XXscreen);
			else
				fore_color = save_color;

	XSetForeground(XXdisplay, XXgc_norm, fore);
	XSetBackground(XXdisplay, XXgc_rev, fore);
	
	Fredraw_display ();
	UNBLOCK_INPUT ();

	XFlush (XXdisplay);
	return Qt;
}

DEFUN ("x-set-background-color", Fx_set_background_color,
       Sx_set_background_color, 1, 1, "sSet background color: ",
       "Set background color to COLOR.")
  (arg)
     Lisp_Object arg;
{
	XColor cdef;
	BLOCK_INPUT_DECLARE ();
	char *save_color;

	check_xterm ();
	CHECK_STRING (arg,1);
	save_color = back_color;
	back_color = (char *) xmalloc (XSTRING (arg)->size + 1);
	bcopy (XSTRING (arg)->data, back_color, XSTRING (arg)->size + 1);

	BLOCK_INPUT ();

	if (back_color && XXisColor &&
	    XParseColor (XXdisplay, XXColorMap, back_color, &cdef) &&
	    XAllocColor(XXdisplay, XXColorMap, &cdef))
		back = cdef.pixel;
	else
		if (back_color && !strcmp (back_color, "white"))
			back = WhitePixel (XXdisplay, XXscreen);
		else
			if (back_color && !strcmp (back_color, "black"))
				back = BlackPixel (XXdisplay, XXscreen);
			else
				back_color = save_color;

	XSetBackground (XXdisplay, XXgc_norm, back);
	XSetForeground (XXdisplay, XXgc_rev, back);
	XSetForeground (XXdisplay, XXgc_curs, back);
	XSetWindowBackground(XXdisplay, XXwindow, back);
	XClearArea (XXdisplay, XXwindow, 0, 0,
		    screen_width*XXfontw+2*XXInternalBorder,
		    screen_height*XXfonth+2*XXInternalBorder, 0);
	
	UNBLOCK_INPUT ();
	Fredraw_display ();

	XFlush (XXdisplay);
	return Qt;
}

DEFUN ("x-set-border-color", Fx_set_border_color, Sx_set_border_color, 1, 1,
       "sSet border color: ",
       "Set border color to COLOR.")
  (arg)
     Lisp_Object arg;
{
	XColor cdef;
	BLOCK_INPUT_DECLARE ();

	check_xterm ();
	CHECK_STRING (arg,1);
	brdr_color= (char *) xmalloc (XSTRING (arg)->size + 1);
	bcopy (XSTRING (arg)->data, brdr_color, XSTRING (arg)->size + 1);

	BLOCK_INPUT ();

	if (brdr_color && XXisColor &&
	    XParseColor (XXdisplay, XXColorMap, brdr_color, &cdef) &&
	    XAllocColor(XXdisplay, XXColorMap, &cdef))
		brdr = cdef.pixel;
	else
		if (brdr_color && !strcmp (brdr_color, "black"))
			brdr = BlackPixel (XXdisplay, XXscreen);
		else
			if (brdr_color && !strcmp (brdr_color, "white"))
				brdr = WhitePixel (XXdisplay, XXscreen);
			else {
				brdr_color = "black";
				brdr = BlackPixel (XXdisplay, XXscreen);
			}

	if (XXborder) {
		XSetWindowBorder(XXdisplay, XXwindow, brdr);
		XFlush (XXdisplay);
	}
	
	UNBLOCK_INPUT ();

	return Qt;
}

DEFUN ("x-set-cursor-color", Fx_set_cursor_color, Sx_set_cursor_color, 1, 1,
       "sSet text cursor color: ",
       "Set text cursor color to COLOR.")
  (arg)
     Lisp_Object arg;
{
	XColor cdef;
	BLOCK_INPUT_DECLARE ();
	char *save_color;

	check_xterm ();
	CHECK_STRING (arg,1);
	save_color = curs_color;
	curs_color = (char *) xmalloc (XSTRING (arg)->size + 1);
	bcopy (XSTRING (arg)->data, curs_color, XSTRING (arg)->size + 1);

	BLOCK_INPUT ();

	if (curs_color && XXisColor &&
	    XParseColor (XXdisplay, XXColorMap, curs_color, &cdef) &&
	    XAllocColor(XXdisplay, XXColorMap, &cdef))
		curs = cdef.pixel;
	else
		if (curs_color && !strcmp (curs_color, "black"))
			curs = BlackPixel (XXdisplay, XXscreen);
		else
			if (curs_color && !strcmp (curs_color, "white"))
				curs = WhitePixel (XXdisplay, XXscreen);
			else
				curs_color = save_color;

	XSetBackground(XXdisplay, XXgc_curs, curs);
	
	CursorToggle ();
	CursorToggle ();

	UNBLOCK_INPUT ();
	return Qt;
}

DEFUN ("x-set-mouse-color", Fx_set_mouse_color, Sx_set_mouse_color, 1, 1,
       "sSet mouse cursor color: ",
       "Set mouse cursor color to COLOR.")
  (arg)
     Lisp_Object arg;
{
  BLOCK_INPUT_DECLARE ();
  char *save_color;

  check_xterm ();
  CHECK_STRING (arg,1);
  save_color = mous_color;
  mous_color = (char *) xmalloc (XSTRING (arg)->size + 1);
  bcopy (XSTRING (arg)->data, mous_color, XSTRING (arg)->size + 1);

  BLOCK_INPUT ();

  if (! x_set_cursor_colors ())
    mous_color = save_color;

  XFlush (XXdisplay);
	
  UNBLOCK_INPUT ();
  return Qt;
}   

/* Set the actual X cursor colors from `mous_color' and `back_color'.  */

int
x_set_cursor_colors ()
{
  XColor forec, backc;

  char	 *useback;

  /* USEBACK is the background color, but on monochrome screens
     changed if necessary not to match the mouse.  */

  useback = back_color;

  if (!XXisColor && !strcmp (mous_color, back_color))
    {
      if (strcmp (back_color, "white"))
	useback = "white";
      else
	useback = "black";
    }

  if (XXisColor && mous_color
      && XParseColor (XXdisplay, XXColorMap, mous_color, &forec)
      && XParseColor (XXdisplay, XXColorMap, useback, &backc))
    {
      XRecolorCursor (XXdisplay, EmacsCursor, &forec, &backc);
      return 1;
    }
  else return 0;
}

DEFUN ("x-color-p", Fx_color_p, Sx_color_p, 0, 0, 0,
       "Returns t if the display is a color X terminal.")
  ()
{
	check_xterm ();

	if (XXisColor)
		return Qt;
	else
		return Qnil;
}
	
DEFUN ("x-get-foreground-color", Fx_get_foreground_color,
       Sx_get_foreground_color, 0, 0, 0,
       "Returns the color of the foreground, as a string.")
  ()
{
	Lisp_Object string;

	check_xterm ();
	string = build_string (fore_color);
	return string;
}

DEFUN ("x-get-background-color", Fx_get_background_color,
       Sx_get_background_color, 0, 0, 0,
       "Returns the color of the background, as a string.")
  ()
{
	Lisp_Object string;

	check_xterm ();
	string = build_string (back_color);
	return string;
}

DEFUN ("x-get-border-color", Fx_get_border_color,
       Sx_get_border_color, 0, 0, 0,
       "Returns the color of the border, as a string.")
  ()
{
	Lisp_Object string;

	check_xterm ();
	string = build_string (brdr_color);
	return string;
}

DEFUN ("x-get-cursor-color", Fx_get_cursor_color,
       Sx_get_cursor_color, 0, 0, 0,
       "Returns the color of the cursor, as a string.")
  ()
{
	Lisp_Object string;

	check_xterm ();
	string = build_string (curs_color);
	return string;
}

DEFUN ("x-get-mouse-color", Fx_get_mouse_color,
       Sx_get_mouse_color, 0, 0, 0,
       "Returns the color of the mouse cursor, as a string.")
  ()
{
	Lisp_Object string;

	check_xterm ();
	string = build_string (mous_color);
	return string;
}

DEFUN ("x-get-default", Fx_get_default, Sx_get_default, 1, 1, 0,
       "Get default for X-window attribute ATTRIBUTE from the system.\n\
ATTRIBUTE must be a string.\n\
Returns nil if attribute default isn't specified.")
  (arg)
     Lisp_Object arg;
{
	char *default_name, *value;

	check_xterm ();
	CHECK_STRING (arg, 1);
	default_name = (char *) XSTRING (arg)->data;

#ifdef XBACKWARDS
	/* Some versions of X11R4, at least, have the args backwards.  */
	if (XXidentity && *XXidentity)
		value = XGetDefault (XXdisplay, default_name, XXidentity);
	else
		value = XGetDefault (XXdisplay, default_name, CLASS);
#else
	if (XXidentity && *XXidentity)
		value = XGetDefault (XXdisplay, XXidentity, default_name);
	else
		value = XGetDefault (XXdisplay, CLASS, default_name);
#endif
 	
	if (value)
		return build_string (value);
	return (Qnil);
}

DEFUN ("x-set-font", Fx_set_font, Sx_set_font, 1, 1, "sFont Name: ",
      "Sets the font to be used for the X window.")
  (arg)
     Lisp_Object arg;
{
	register char *newfontname;
	
	CHECK_STRING (arg, 1);
	check_xterm ();

	newfontname = (char *) xmalloc (XSTRING (arg)->size + 1);
	bcopy (XSTRING (arg)->data, newfontname, XSTRING (arg)->size + 1);
	if (XSTRING (arg)->size == 0)
		goto badfont;

	if (!XNewFont (newfontname)) {
		free (XXcurrentfont);
		XXcurrentfont = newfontname;
		return Qt;
	}
badfont:
	error ("Font \"%s\" is not defined", newfontname);
	free (newfontname);

	return Qnil;
}

DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
  Scoordinates_in_window_p, 2, 2, 0,
  "Return non-nil if POSITIONS (a list, (SCREEN-X SCREEN-Y)) is in WINDOW.\n\
Returned value is list of positions expressed\n\
relative to window upper left corner.")
  (coordinate, window)
     register Lisp_Object coordinate, window;
{
	register Lisp_Object xcoord, ycoord;
	int height;
	
	if (!CONSP (coordinate))
		wrong_type_argument (Qlistp, coordinate);

	CHECK_WINDOW (window, 2);
	xcoord = Fcar (coordinate);
	ycoord = Fcar (Fcdr (coordinate));
	CHECK_NUMBER (xcoord, 0);
	CHECK_NUMBER (ycoord, 1);
	if ((XINT (xcoord) < XINT (XWINDOW (window)->left)) ||
	    (XINT (xcoord) >= (XINT (XWINDOW (window)->left) +
			       XINT (XWINDOW (window)->width))))
		return Qnil;

	XFASTINT (xcoord) -= XFASTINT (XWINDOW (window)->left);

	height = XINT (XWINDOW (window)->height);

	if (window == minibuf_window)
	  {
	    XFASTINT (xcoord) -= minibuf_prompt_width;
	    if(XINT (xcoord) < 0)
		XFASTINT (xcoord) = 0;
	  }
	else
	  height --;

	if ((XINT (ycoord) < XINT (XWINDOW (window)->top)) ||
	    (XINT (ycoord) >= XINT (XWINDOW (window)->top) + height))
	  return Qnil;

	XFASTINT (ycoord) -= XFASTINT (XWINDOW (window)->top);
	return Fcons (xcoord, Fcons (ycoord, Qnil));
}

DEFUN ("x-mouse-events", Fx_mouse_events, Sx_mouse_events, 0, 0, 0,
  "Return number of pending mouse events from X window system.")
  ()
{
	register Lisp_Object tem;

	check_xterm ();

	XSET (tem, Lisp_Int, XXm_queue_num);
	
	return tem;
}

DEFUN ("x-proc-mouse-event", Fx_proc_mouse_event, Sx_proc_mouse_event,
  0, 0, 0,
  "Pulls a mouse event out of the mouse event buffer and dispatches\n\
the appropriate function to act upon this event.")
  ()
{
	XEvent event;
	register Lisp_Object mouse_cmd;
	register char com_letter;
	register char key_mask;
	register Lisp_Object tempx;
	register Lisp_Object tempy;
	extern Lisp_Object get_keyelt ();
	extern int meta_prefix_char;
	
	check_xterm ();

	if (XXm_queue_num) {
		event = *XXm_queue[XXm_queue_out];
		free (XXm_queue[XXm_queue_out]);
		XXm_queue_out = (XXm_queue_out + 1) % XMOUSEBUFSIZE;
		XXm_queue_num--;
		com_letter = 3-(event.xbutton.button & 3);
		key_mask = (event.xbutton.state & 15) << 4;
		/* Report meta in 2 bit, not in 8 bit.  */
		if (key_mask & 0x80)
		  {
		    key_mask |= 0x20;
		    key_mask &= ~0x80;
		  }
		com_letter |= key_mask;
		if (event.type == ButtonRelease)
			com_letter |= 0x04;
		XSET (tempx, Lisp_Int,
		      min (screen_width-1,
			   max (0, (event.xbutton.x-XXInternalBorder)/
				XXfontw)));
		XSET (tempy, Lisp_Int,
		      min (screen_height-1,
			   max (0, (event.xbutton.y-XXInternalBorder)/
				XXfonth)));
		Vx_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
		XSET (tempx, Lisp_Int, event.xbutton.x_root);
		XSET (tempy, Lisp_Int, event.xbutton.y_root);
		Vx_mouse_abs_pos = Fcons (tempx, Fcons (tempy, Qnil));
		Vx_mouse_item = make_number (com_letter);
		mouse_cmd
		  = get_keyelt (access_keymap (MouseMap, com_letter));
		if (NULL (mouse_cmd)) {
			if (event.type != ButtonRelease)
				bell ();
			Vx_mouse_pos = Qnil;
		}
		else
			return call1 (mouse_cmd, Vx_mouse_pos);
	}
	return Qnil;
}

DEFUN ("x-get-mouse-event", Fx_get_mouse_event, Sx_get_mouse_event,
  1, 1, 0,
  "Get next mouse event out of mouse event buffer (com-letter (x y)).\n\
ARG non-nil means return nil immediately if no pending event;\n\
otherwise, wait for an event.")
  (arg)
     Lisp_Object arg;
{
	XEvent event;
	register char com_letter;
	register char key_mask;

	register Lisp_Object tempx;
	register Lisp_Object tempy;
	
	check_xterm ();

	if (NULL (arg))
		while (!XXm_queue_num)
		  {
		    consume_available_input ();
		    Fsleep_for (make_number (1));
		  }
	/*** ??? Surely you don't mean to busy wait??? */

	if (XXm_queue_num) {
		event = *XXm_queue[XXm_queue_out];
		free (XXm_queue[XXm_queue_out]);
		XXm_queue_out = (XXm_queue_out + 1) % XMOUSEBUFSIZE;
		XXm_queue_num--;
		com_letter = 3-(event.xbutton.button & 3);
		key_mask = (event.xbutton.state & 15) << 4;
		/* Report meta in 2 bit, not in 8 bit.  */
		if (key_mask & 0x80)
		  {
		    key_mask |= 0x20;
		    key_mask &= ~0x80;
		  }
		com_letter |= key_mask;
		if (event.type == ButtonRelease)
			com_letter |= 0x04;
		XSET (tempx, Lisp_Int,
		      min (screen_width-1,
			   max (0, (event.xbutton.x-XXInternalBorder)/
				XXfontw)));
		XSET (tempy, Lisp_Int,
		      min (screen_height-1,
			   max (0, (event.xbutton.y-XXInternalBorder)/
				XXfonth)));
		Vx_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
		XSET (tempx, Lisp_Int, event.xbutton.x_root);
		XSET (tempy, Lisp_Int, event.xbutton.y_root);
		Vx_mouse_abs_pos = Fcons (tempx, Fcons (tempy, Qnil));
		Vx_mouse_item = make_number (com_letter);
		return Fcons (com_letter, Fcons (Vx_mouse_pos, Qnil));
	}
	return Qnil;
}

DEFUN ("x-store-cut-buffer", Fx_store_cut_buffer, Sx_store_cut_buffer,
  1, 1, "sSend string to X:",
  "Store contents of STRING into the cut buffer of the X window system.")
  (string)
     register Lisp_Object string;
{
	BLOCK_INPUT_DECLARE ();

	CHECK_STRING (string, 1);
	check_xterm ();

	BLOCK_INPUT ();
	XStoreBytes (XXdisplay, XSTRING (string)->data,
		     XSTRING (string)->size);
	/* Clear the selection owner, so that other applications
	   will use the cut buffer rather than a selection.  */
        XSetSelectionOwner (XXdisplay, XA_PRIMARY, None, CurrentTime);
	UNBLOCK_INPUT ();

	return Qnil;
}

DEFUN ("x-get-cut-buffer", Fx_get_cut_buffer, Sx_get_cut_buffer, 0, 0, 0,
  "Return contents of cut buffer of the X window system, as a string.")
  ()
{
	int len;
	register Lisp_Object string;
	BLOCK_INPUT_DECLARE ();
	register char *d;

	check_xterm ();
	BLOCK_INPUT ();
	d = XFetchBytes (XXdisplay, &len);
	string = make_string (d, len);
	UNBLOCK_INPUT ();

	return string;
}

DEFUN ("x-set-border-width", Fx_set_border_width, Sx_set_border_width,
  1, 1, "nBorder width: ",
  "Set width of border to WIDTH, in the X window system.")
  (borderwidth)
     register Lisp_Object borderwidth;
{
	BLOCK_INPUT_DECLARE ();

	CHECK_NUMBER (borderwidth, 0);

	check_xterm ();
  
	if (XINT (borderwidth) < 0)
		XSETINT (borderwidth, 0);
  
	BLOCK_INPUT ();
	XSetWindowBorderWidth(XXdisplay, XXwindow, XINT(borderwidth));
	XFlush(XXdisplay);
	UNBLOCK_INPUT ();

	return Qt;
}


DEFUN ("x-set-internal-border-width", Fx_set_internal_border_width,
       Sx_set_internal_border_width, 1, 1, "nInternal border width: ",
  "Set width of internal border to WIDTH, in the X window system.")
  (internalborderwidth)
     register Lisp_Object internalborderwidth;
{
	BLOCK_INPUT_DECLARE ();

	CHECK_NUMBER (internalborderwidth, 0);

	check_xterm ();
  
	if (XINT (internalborderwidth) < 0)
		XSETINT (internalborderwidth, 0);

	BLOCK_INPUT ();
	XXInternalBorder = XINT(internalborderwidth);
	XSetWindowSize(screen_height,screen_width);
	UNBLOCK_INPUT ();

	return Qt;
}

#ifdef foobar
DEFUN ("x-rebind-key", Fx_rebind_key, Sx_rebind_key, 3, 3, 0,
  "Rebind KEYCODE, with shift bits SHIFT-MASK, to new string NEWSTRING.\n\
KEYCODE and SHIFT-MASK should be numbers representing the X keyboard code\n\
and shift mask respectively.  NEWSTRING is an arbitrary string of keystrokes.\n\
If SHIFT-MASK is nil, then KEYCODE's key will be bound to NEWSTRING for\n\
all shift combinations.\n\
Shift Lock  1	   Shift    2\n\
Meta	    4	   Control  8\n\
\n\
For values of KEYCODE, see /usr/lib/Xkeymap.txt (remember that the codes\n\
in that file are in octal!)\n")

  (keycode, shift_mask, newstring)
     register Lisp_Object keycode;
     register Lisp_Object shift_mask;
     register Lisp_Object newstring;
{
#ifdef notdef
	char *rawstring;
	int rawkey, rawshift;
	int i;
	int strsize;

	CHECK_NUMBER (keycode, 1);
	if (!NULL (shift_mask))
		CHECK_NUMBER (shift_mask, 2);
	CHECK_STRING (newstring, 3);
	strsize = XSTRING (newstring) ->size;
	rawstring = (char *) xmalloc (strsize);
	bcopy (XSTRING (newstring)->data, rawstring, strsize);
	rawkey = ((unsigned) (XINT (keycode))) & 255;
	if (NULL (shift_mask))
		for (i = 0; i <= 15; i++)
			XRebindCode (rawkey, i<<11, rawstring, strsize);
	else
	{
		rawshift = (((unsigned) (XINT (shift_mask))) & 15) << 11;
		XRebindCode (rawkey, rawshift, rawstring, strsize);
	}
#endif notdef
	return Qnil;
}
  
DEFUN ("x-rebind-keys", Fx_rebind_keys, Sx_rebind_keys, 2, 2, 0,
  "Rebind KEYCODE to list of strings STRINGS.\n\
STRINGS should be a list of 16 elements, one for each all shift combination.\n\
nil as element means don't change.\n\
See the documentation of x-rebind-key for more information.")
  (keycode, strings)
     register Lisp_Object keycode;
     register Lisp_Object strings;
{
#ifdef notdef
	register Lisp_Object item;
	register char *rawstring;
	int rawkey, strsize;
	register unsigned i;

	CHECK_NUMBER (keycode, 1);
	CHECK_CONS (strings, 2);
	rawkey = ((unsigned) (XINT (keycode))) & 255;
	for (i = 0; i <= 15; strings = Fcdr (strings), i++)
	{
		item = Fcar (strings);
		if (!NULL (item))
		{
			CHECK_STRING (item, 2);
			strsize = XSTRING (item)->size;
			rawstring = (char *) xmalloc (strsize);
			bcopy (XSTRING (item)->data, rawstring, strsize);
			XRebindCode (rawkey, i << 11, rawstring, strsize);
		}
	}
#endif notdef
	return Qnil;
}

#endif foobar

XExitWithCoreDump ()
{
	XCleanUp ();
	abort ();
}

DEFUN ("x-debug", Fx_debug, Sx_debug, 1, 1, 0,
  "ARG non-nil means that X errors should generate a coredump.")
  (arg)
     register Lisp_Object arg;
{
	int (*handler)();

	check_xterm ();
	if (!NULL (arg))
		handler = XExitWithCoreDump;
	else
	{
		extern int XIgnoreError ();
		handler = XIgnoreError;
	}
	XSetErrorHandler(handler);
	XSetIOErrorHandler(handler);
	return (Qnil);
}

XRedrawDisplay ()
{
	Fredraw_display ();
}

XCleanUp ()
{
	Fdo_auto_save (Qt);

#ifdef subprocesses
	kill_buffer_processes (Qnil);
#endif				/* subprocesses */
}

syms_of_xfns ()
{
  /* If not dumping, init_display ran before us, so don't override it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    Vxterm = Qnil;

  DEFVAR_LISP ("x-mouse-item", &Vx_mouse_item,
	       "Encoded representation of last mouse click, corresponding to\n\
numerical entries in x-mouse-map.");
  Vx_mouse_item = Qnil;
  DEFVAR_LISP ("x-mouse-pos", &Vx_mouse_pos,
	       "Current x-y position of mouse by row, column as specified by font.");
  Vx_mouse_pos = Qnil;
  DEFVAR_LISP ("x-mouse-abs-pos", &Vx_mouse_abs_pos,
	       "Current x-y position of mouse relative to root window.");
  Vx_mouse_abs_pos = Qnil;

  defsubr (&Sx_set_bell);
  defsubr (&Sx_flip_color);
  defsubr (&Sx_set_font);
#ifdef notdef
  defsubr (&Sx_set_icon);
#endif notdef
  defsubr (&Scoordinates_in_window_p);
  defsubr (&Sx_mouse_events);
  defsubr (&Sx_proc_mouse_event);
  defsubr (&Sx_get_mouse_event);
  defsubr (&Sx_store_cut_buffer);
  defsubr (&Sx_get_cut_buffer);
  defsubr (&Sx_set_border_width);
  defsubr (&Sx_set_internal_border_width);
  defsubr (&Sx_set_foreground_color);
  defsubr (&Sx_set_background_color);
  defsubr (&Sx_set_border_color);
  defsubr (&Sx_set_cursor_color);
  defsubr (&Sx_set_mouse_color);
  defsubr (&Sx_get_foreground_color);
  defsubr (&Sx_get_background_color);
  defsubr (&Sx_get_border_color);
  defsubr (&Sx_get_cursor_color);
  defsubr (&Sx_get_mouse_color);
  defsubr (&Sx_color_p);
  defsubr (&Sx_get_default);
#ifdef notdef
  defsubr (&Sx_rebind_key);
  defsubr (&Sx_rebind_keys);
#endif notdef
  defsubr (&Sx_debug);
}

#endif /* HAVE_X_WINDOWS */
