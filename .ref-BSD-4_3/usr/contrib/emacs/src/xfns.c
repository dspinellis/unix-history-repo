/* Functions for the X window system.
   Copyright (C) 1985 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* Written by Yakim Martillo; rearranged by Richard Stallman.  */
/* Color added by Robert Krawitz*/

/*#include <stdio.h>*/
#include "config.h"
#include "lisp.h"
#include "window.h"
#include "xterm.h"
#include "dispextern.h"
#include "termchar.h"
#include <signal.h>
#include "sink.h"
#include "sinkmask.h"
#include <sys/time.h>
#include <fcntl.h>
#include <setjmp.h>

#define abs(x) ((x < 0) ? ((x)) : (x))
#define sgn(x) ((x < 0) ? (-1) : (1))
  
#define CROSS_WIDTH 16
#define CROSS_HEIGHT 16

static short cross_bits[] =
  {
    0x0000, 0x0180, 0x0180, 0x0180,
    0x0180, 0x0180, 0x0180, 0x7ffe,
    0x7ffe, 0x0180, 0x0180, 0x0180,
    0x0180, 0x0180, 0x0180, 0x0000,
  };

static short gray_bits[] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555};

#define CROSS_MASK_WIDTH 16
#define CROSS_MASK_HEIGHT 16
static short cross_mask_bits[] =
  {
    0x03c0, 0x03c0, 0x03c0, 0x03c0,
    0x03c0, 0x03c0, 0xffff, 0xffff,
    0xffff, 0xffff, 0x03c0, 0x03c0,
    0x03c0, 0x03c0, 0x03c0, 0x03c0,
  };

extern XREPBUFFER Xxrepbuffer;

/* Non-nil if Emacs is running with an X window for display.
   Nil if Emacs is run on an ordinary terminal.  */

Lisp_Object Vxterm;

Lisp_Object Vx_mouse_pos;

extern struct Lisp_Vector *MouseMap;

extern char *fore_color;
extern char *back_color;
extern char *brdr_color;
extern char *mous_color;
extern char *curs_color;

extern int fore;
extern int back;
extern int brdr;
extern int mous;
extern int curs;

/* Nonzero if x-set-window-edges has been called
   or x-rubber-band has been called.
   If it is zero when x-pop-up-window is called,
   x-rubber-band is called at that point.  */

int x_edges_specified;

check_xterm ()
{
  if (NULL (Vxterm))
    error ("Terminal does not understand X protocol.");
}

DEFUN ("x-pop-up-window", Fx_pop_up_window, Sx_pop_up_window, 0, 0, 0,
  "Make the X window appear on the screen.")
  ()
{
  check_xterm();
  XPopUpWindow();
  return Qnil;
}

DEFUN ("x-set-bell", Fx_set_bell, Sx_set_bell, 1, 1, "P",
  "For X window system, set audible vs visible bell.\n\
With non-nil argument (prefix arg), use visible bell; otherwise, audible bell.")
  (arg)
     Lisp_Object arg;
{
  check_xterm ();
  if (!NULL (arg))
    XSetFlash ();
  else
    XSetFeep ();
  return arg;
}

DEFUN ("x-flip-color", Fx_flip_color, Sx_flip_color, 0, 0, "",
  "Toggle the background and foreground colors (currently only black \n\
and white -- by default background is white -- Only effective at init")
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
    Color cdef;
    extern int PendingExposure;
    int (*func) ();
    extern Window XXwindow;
    extern FontInfo *fontinfo;
    char *save_color;
    save_color = fore_color;
    check_xterm ();
    CHECK_STRING (arg,1);
    fore_color = (char *) xmalloc (XSTRING (arg)->size + 1);
    func = signal (SIGIO, SIG_IGN);
    bcopy (XSTRING (arg)->data, fore_color, XSTRING (arg)->size + 1);
    if (fore_color && DisplayCells() > 2 &&
	XParseColor(fore_color, &cdef) && XGetHardwareColor(&cdef)) {
	fore = cdef.pixel;
    } else if (fore_color && strcmp(fore_color, "black") == 0) {
	fore = BlackPixel;
    } else if (fore_color && strcmp(fore_color, "white") == 0) {
	fore = WhitePixel;
    }
    else
      {
	  fore_color = save_color;
      }
/*    XPixFill (XXwindow, 0, 0, screen_width * fontinfo->width,
	      screen_height * fontinfo->height, back, ClipModeClipped,
	      GXcopy, AllPlanes);*/
    Fredraw_display ();
/*    dumprectangle(0, 0, screen_height * fontinfo->height,
		  screen_width * fontinfo -> width);*/
/*    PendingExposure = 1;
      xfixscreen ();*/
    (void) signal (SIGIO, func);
    XFlush();
    return Qt;
}

DEFUN ("x-set-background-color", Fx_set_background_color,
       Sx_set_background_color, 1, 1, "sSet background color:  ",
       "Set background color to COLOR.")
(arg)
Lisp_Object arg;
{
    Color cdef;
    extern int PendingExposure;
    Pixmap temp;
    int (*func) ();
    char *save_color;
    extern Window XXwindow;
    check_xterm ();
    CHECK_STRING (arg,1);
    back_color = (char *) xmalloc (XSTRING (arg)->size + 1);
    bcopy (XSTRING (arg)->data, back_color, XSTRING (arg)->size + 1);
    func = signal (SIGIO, SIG_IGN);
    if (back_color && DisplayCells() > 2 &&
	XParseColor(back_color, &cdef) && XGetHardwareColor(&cdef)) {
	back = cdef.pixel;
    } else if (back_color && strcmp(back_color, "white") == 0) {
	back = WhitePixel;
    } else if (back_color && strcmp(back_color, "black") == 0) {
	back = BlackPixel;
    }
    else
      {
	  back_color = save_color;
      }
    temp = XMakeTile(back);
    XChangeBackground (XXwindow, temp);
/*    XPixFill (XXwindow, 0, 0, screen_width * fontinfo->width,
	      screen_height * fontinfo->height, back, ClipModeClipped,
	      GXcopy, AllPlanes);*/
    (void) signal (SIGIO, func);
    Fredraw_display();
/*    dumprectangle(0, 0, screen_height * fontinfo->height,
		  screen_width * fontinfo -> width);*/
/*    PendingExposure = 1;
      xfixscreen ();*/
    XFlush();
    XFreePixmap (temp);
    return Qt;
}

DEFUN ("x-set-border-color", Fx_set_border_color, Sx_set_border_color, 1, 1,
       "sSet border color:  ",
       "Set border color to COLOR.")
(arg)
Lisp_Object arg;
{
    Color cdef;
    Pixmap temp;
    extern int XXborder;
    int (*func) ();
    extern Window XXwindow;
    check_xterm ();
    CHECK_STRING (arg,1);
    brdr_color= (char *) xmalloc (XSTRING (arg)->size + 1);
    bcopy (XSTRING (arg)->data, brdr_color, XSTRING (arg)->size + 1);
    func = signal (SIGIO, SIG_IGN);
    if (brdr_color && DisplayCells() > 2 &&
	XParseColor(brdr_color, &cdef) && XGetHardwareColor(&cdef))
      {
	  temp = XMakeTile(cdef.pixel);
	  brdr = cdef.pixel;
      }
    else if (brdr_color && strcmp(brdr_color, "black") == 0)
      {
	  temp = BlackPixmap;
	  brdr = BlackPixel;
      }
    else if (brdr_color && strcmp(brdr_color, "white") == 0)
      {
	  temp = WhitePixmap;
	  brdr = WhitePixel;
      }
    else
      {
	  temp = XMakePixmap ((Bitmap) XStoreBitmap (16, 16, gray_bits),
			      BlackPixel, WhitePixel);
	  brdr = BlackPixel;
	  brdr_color = "gray";
      }
    if (XXborder)
      XChangeBorder (XXwindow, temp);
    (void) signal (SIGIO, func);
    XFreePixmap (temp);
    return Qt;
}

DEFUN ("x-set-cursor-color", Fx_set_cursor_color, Sx_set_cursor_color, 1, 1,
       "sSet text cursor color:  ",
       "Set text cursor color to COLOR.")
(arg)
Lisp_Object arg;
{
    Color cdef;
    extern Window XXwindow;
    int (*func) ();
    char *save_color;
    check_xterm ();
    CHECK_STRING (arg,1);
    curs_color = (char *) xmalloc (XSTRING (arg)->size + 1);
    func = signal (SIGIO, SIG_IGN);
    bcopy (XSTRING (arg)->data, curs_color, XSTRING (arg)->size + 1);
    if (curs_color && DisplayCells() > 2 &&
	XParseColor(curs_color, &cdef) && XGetHardwareColor(&cdef)) {
	curs = cdef.pixel;
    } else if (curs_color && strcmp(curs_color, "black") == 0) {
	curs = BlackPixel;
    } else if (curs_color && strcmp(curs_color, "white") == 0) {
	curs = WhitePixel;
    }
    else
      {
	  curs_color = save_color;
      }
    (void) signal (SIGIO, func);
    CursorToggle();
    CursorToggle();
    return Qt;
}

DEFUN ("x-set-mouse-color", Fx_set_mouse_color, Sx_set_mouse_color, 1, 1,
       "sSet mouse cursor color:  ",
       "Set mouse cursor color to COLOR.")
(arg)
Lisp_Object arg;
{
    extern Cursor EmacsCursor;
    extern char MouseCursor[], MouseMask[];
    Cursor temp;
    int (*func) ();
    Color cdef;
    char *save_color;
    extern Window XXwindow;
    check_xterm ();
    CHECK_STRING (arg,1);
    mous_color = (char *) xmalloc (XSTRING (arg)->size + 1);
    func = signal (SIGIO, SIG_IGN);
    bcopy (XSTRING (arg)->data, mous_color, XSTRING (arg)->size + 1);
    if (mous_color && DisplayCells() > 2 &&
	XParseColor(mous_color, &cdef) && XGetHardwareColor(&cdef)) {
	mous = cdef.pixel;
    } else if (mous_color && strcmp(mous_color, "black") == 0) {
	mous = BlackPixel;
    } else if (mous_color && strcmp(mous_color, "white") == 0) {
	mous = WhitePixel;
    }
    else
      {
	  mous_color = save_color;
      }
    temp = XCreateCursor(16, 16, MouseCursor, MouseMask, 0, 0,
				mous, back, GXcopy);
    XDefineCursor (XXwindow, temp);
    XFreeCursor (EmacsCursor);
    (void) signal (SIGIO, func);
    bcopy(&temp, &EmacsCursor, sizeof(Cursor));
    return Qt;
}   

DEFUN ("x-color-p", Fx_color_p, Sx_color_p, 0, 0, "",
       "Returns t if the display is a color X terminal.")
()
{
    check_xterm ();
    if (DisplayCells() > 2)
      return Qt;
    else
      return Qnil;
}
	
DEFUN ("x-get-foreground-color", Fx_get_foreground_color,
       Sx_get_foreground_color, 0, 0, "",
       "Returns the color of the foreground, as a string.")
()
{
    Lisp_Object string;
    string = make_string(fore_color, strlen (fore_color));
    return string;
}

DEFUN ("x-get-background-color", Fx_get_background_color,
       Sx_get_background_color, 0, 0, "",
       "Returns the color of the background, as a string.")
()
{
    Lisp_Object string;
    string = make_string(back_color, strlen (back_color));
    return string;
}

DEFUN ("x-get-border-color", Fx_get_border_color,
       Sx_get_border_color, 0, 0, "",
       "Returns the color of the border, as a string.")
()
{
    Lisp_Object string;
    string = make_string(brdr_color, strlen (brdr_color));
    return string;
}

DEFUN ("x-get-cursor-color", Fx_get_cursor_color,
       Sx_get_cursor_color, 0, 0, "",
       "Returns the color of the cursor, as a string.")
()
{
    Lisp_Object string;
    string = make_string(curs_color, strlen (curs_color));
    return string;
}

DEFUN ("x-get-mouse-color", Fx_get_mouse_color,
       Sx_get_mouse_color, 0, 0, "",
       "Returns the color of the mouse cursor, as a string.")
()
{
    Lisp_Object string;
    string = make_string(mous_color, strlen (mous_color));
    return string;
}

DEFUN ("x-get-default", Fx_get_default, Sx_get_default, 1, 1,
       "sGet X default name:  ",
       "Get X default ATTRIBUTE from the system.  Returns nil if\n\
attribute does not exist.")
(arg)
Lisp_Object arg;
{
    char *default_name, *value;
    Lisp_Object return_string;
    extern char *malloc(), strcpy();
    check_xterm ();
    CHECK_STRING (arg,1);
    default_name = (char *) xmalloc (XSTRING (arg) -> size + 1);
    if (default_name == 0)
      {
	  return Qnil;
      }
    else
      {
	  bcopy (XSTRING (arg) -> data, default_name,
		 XSTRING (arg) -> size + 1);
	  value = XGetDefault("emacs", default_name);
	  if (value == 0)
	    value = XGetDefault("", default_name);
	  return make_string (value, value ? strlen (value) : 0);
      }
}    

DEFUN ("x-set-icon", Fx_set_icon, Sx_set_icon, 1, 1, "P",
  "Set type of icon used by X for Emacs's window.\n\
ARG non-nil means use kitchen-sink icon;\n\
nil means use generic window manager icon.")
  (arg)
     Lisp_Object arg;
{
  check_xterm ();
  if (NULL (arg))
    XTextIcon ();
  else
    XBitmapIcon ();
  return arg;
}

DEFUN ("x-set-font", Fx_set_font, Sx_set_font, 1, 1, "sFont Name: ",
      "At initialization sets the font to be used for the X window.")
  (arg)
     Lisp_Object arg;
{
  register char *newfontname;
  extern char *XXcurrentfont;
	
  CHECK_STRING (arg, 1);
  check_xterm ();

  newfontname = (char *) xmalloc (XSTRING (arg)->size + 1);
  bcopy (XSTRING (arg)->data, newfontname, XSTRING (arg)->size + 1);
  if (!XNewFont (newfontname))
    {
      free (XXcurrentfont);
      XXcurrentfont = newfontname;
      return Qt;
    }
  else
    {
      error ("Font %s is not defined", newfontname);
      free (newfontname);
    }

  return Qnil;
}

DEFUN ("x-set-window-edges", Fx_set_window_edges, Sx_set_window_edges, 4, 4,
  "nNumber of Columns: \nnNumber of Rows: \nnX Offset in Pixels: \n\
nY Offset in Pixels: ",
  "Sets X window size/position: size COLS by ROWS, positions XOFF and YOFF.\n\
To get \"minus zero\" for XOFF or YOFF, supply -1.")
  (cols, rows, xoffset, yoffset)
     Lisp_Object rows, cols, xoffset, yoffset;
{
  CHECK_NUMBER (rows, 1);
  CHECK_NUMBER (cols, 2);
  CHECK_NUMBER (xoffset, 3);
  CHECK_NUMBER (yoffset, 4);
  check_xterm ();

  x_edges_specified = 1;
  if (XINT (rows) != screen_width || XINT (cols) != screen_height) 
    {
      XSetWindowSize (XINT (rows), XINT (cols));
    }
  XSetOffset (XINT (xoffset), XINT (yoffset));
  XFlush ();
  return Qt;
}

DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
  Scoordinates_in_window_p, 2, 2,
  "xSpecify coordinate pair: \nXExpression which evals to window: ",
  "Return non-nil if POSITIONS (a list, (SCREEN-X SCREEN-Y)) is in WINDOW.\n\
Returned value is list of positions expressed\n\
relative to window upper left corner.")
  (coordinate, window)
     register Lisp_Object coordinate, window;
{
  register Lisp_Object xcoord, ycoord;
	
  if (!LISTP  (coordinate)) wrong_type_argument (Qlistp, coordinate);
  CHECK_WINDOW (window, 2);
  xcoord = Fcar (coordinate);
  ycoord = Fcar (Fcdr (coordinate));
  CHECK_NUMBER (xcoord, 0);
  CHECK_NUMBER (ycoord, 1);
  if ((XINT (xcoord) < XINT (XWINDOW (window)->left)) ||
      (XINT (xcoord) >= (XINT (XWINDOW (window)->left) +
			 XINT (XWINDOW (window)->width))))
    {
      return Qnil;
    } 
  XFASTINT (xcoord) -= XFASTINT (XWINDOW (window)->left);
  if (XINT (ycoord) == (screen_height - 1))
    return Qnil;
  if ((XINT (ycoord) < XINT (XWINDOW (window)->top)) ||
      (XINT (ycoord) >= (XINT (XWINDOW (window)->top) +
			 XINT (XWINDOW (window)->height)) - 1))
    {
      return Qnil;
    }
  XFASTINT (ycoord) -= XFASTINT (XWINDOW (window)->top);
  return (Fcons (xcoord, Fcons (ycoord, Qnil)));
}

DEFUN ("x-mouse-events", Fx_mouse_events, Sx_mouse_events, 0, 0, 0,
  "Return number of pending mouse events from X window system.")
  ()
{
  register Lisp_Object tem;
  register int windex, rindex, mindex;

  check_xterm ();
  windex = Xxrepbuffer.windex;
  rindex = Xxrepbuffer.rindex;
  mindex = Xxrepbuffer.mindex;

  if (windex >= rindex) 
    {
      XSET (tem, Lisp_Int, windex - rindex);
    }
  else
    {
      XSET (tem, Lisp_Int, mindex + 1 - (rindex - windex));
    }
  return tem;
}

DEFUN ("x-proc-mouse-event", Fx_proc_mouse_event, Sx_proc_mouse_event,
  0, 0, 0,
  "Pulls a mouse event out of the mouse event buffer and dispatches\n\
the appropriate function to act upon this event.")
  ()
{
  XButtonEvent xrep;
  extern FontInfo *fontinfo;
  register Lisp_Object Mouse_Cmd;
  register char com_letter;
  register char key_mask;
  register Lisp_Object tempx;
  register Lisp_Object tempy;
  extern Lisp_Object get_keyelt ();

  check_xterm ();
  if (unloadxrepbuffer (&xrep, &Xxrepbuffer) == 0) 
    {
      com_letter = xrep.detail & 3;
      key_mask = (xrep.detail >> 8) & 0xf0;
      com_letter |= key_mask;
      XSET (tempx, Lisp_Int, xrep.x/fontinfo->width);
      XSET (tempy, Lisp_Int, xrep.y/fontinfo->height);
      Vx_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
      Mouse_Cmd = get_keyelt (access_keymap (MouseMap, com_letter));
      if (NULL (Mouse_Cmd)) 
	{
	  Ding ();
	  Vx_mouse_pos = Qnil;
	}
      else
	{
	  return (call1 (Mouse_Cmd, Vx_mouse_pos));
	}
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
  XButtonEvent xrep;
  extern FontInfo *fontinfo;
  register Lisp_Object Mouse_Cmd;
  register char com_letter;
  register char key_mask;

  register Lisp_Object tempx;
  register Lisp_Object tempy;
  extern Lisp_Object get_keyelt ();

  check_xterm ();

  if (NULL (arg))
    while (Xxrepbuffer.windex == Xxrepbuffer.rindex);
/*** ??? Surely you don't mean to busy wait??? */
  if (unloadxrepbuffer (&xrep, &Xxrepbuffer) == 0) 
    {
      com_letter = *((char *)&xrep.detail);
      com_letter &= 3;
      key_mask = *((char *)&xrep.detail + 1);
      key_mask &= 0xf0;
      com_letter |= key_mask;
      XSET (tempx, Lisp_Int, xrep.x/fontinfo->width);
      XSET (tempy, Lisp_Int, xrep.y/fontinfo->height);
      Vx_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
      return (Fcons (com_letter, Fcons (Vx_mouse_pos, Qnil)));
    }
  return Qnil;
}

DEFUN ("x-set-keyboard-enable", Fx_set_keyboard_enable,
  Sx_set_keyboard_enable, 1, 1, 0,
  "In the X window system, set the flag that permite keyboard input.\n\
Permit input if ARG is non-nil.")
  (arg)
     Lisp_Object arg;
{
  extern Window XXwindow;
  check_xterm ();

  XSelectInput (XXwindow,
		ExposeWindow | ButtonPressed | ExposeRegion | ExposeCopy
		| (!NULL (arg) ? KeyPressed : 0));
  return arg;
}

DEFUN ("x-set-mouse-inform-flag", Fx_set_mouse_inform_flag,
  Sx_set_mouse_inform_flag, 1, 1, 0,
  "Set inform-of-mouse-events flag in X window system on if ARG is non-nil.")
  (arg)
     Lisp_Object arg;
{
  extern int informflag;
  informflag = !NULL (arg);
  return arg;
}

DEFUN ("x-store-cut-buffer", Fx_store_cut_buffer, Sx_store_cut_buffer,
  1, 1, "sSend string to X:",
  "Store contents of STRING into the cut buffer of the X window system.")
  (string)
     register Lisp_Object string;
{
  CHECK_STRING (string, 1);
  check_xterm ();

  XStoreBytes (XSTRING (string)->data, XSTRING (string)->size);

  return Qnil;
}

DEFUN ("x-get-cut-buffer", Fx_get_cut_buffer, Sx_get_cut_buffer, 0, 0, 0,
  "Return contents of cut buffer of the X window system, as a string.")
  ()
{
  int len;
  register Lisp_Object string;
  register int (*func) ();
  register char *d;

  func = (int (*)()) (signal (SIGIO, SIG_IGN));
  d = XFetchBytes (&len);
  string = make_string (d, len);
  signal (SIGIO, func);
  return string;
}

DEFUN ("x-rubber-band", Fx_rubber_band, Sx_rubber_band, 0, 0, 0,
  "Ask user to specify Emacs window position and size with mouse.\n\
This is done automatically if the data has not been specified\n\
when Emacs needs the window to be displayed.")
  ()
{
  int x, y, width, height;
  extern int XXborder;
  extern int PendingExposure;
  extern char *default_window;
  register int (*handle) ();
  x_edges_specified = 1;

  check_xterm ();
  handle = signal (SIGIO, SIG_IGN);
  window_fetch (fontinfo->id, &x, &y, &width, &height, "", default_window,
		XXborder, "Gnuemacs");
  (void) signal (SIGIO, handle);
  XSetWindowSize (height, width);
  XSetOffset (x, y);
  XFlush();
  return Qnil;
}

DEFUN ("x-create-x-window", Fx_create_x_window, Sx_create_x_window,
       1, 1, 0,
       "Create window for gnuemacs from a valid GEOMETRY specification.")
(arg)
Lisp_Object arg;
{
  int x, y, width, height;
  extern int XXborder;
  extern int PendingExposure;
  char *geometry;
  register int (*handle) ();
  x_edges_specified = 1;

  check_xterm ();
  CHECK_STRING(arg, 1);
  geometry= (char *) xmalloc (XSTRING (arg)->size + 1);
  bcopy (XSTRING (arg)->data, geometry, XSTRING (arg)->size + 1);
  handle = signal (SIGIO, SIG_IGN);
  window_fetch (fontinfo->id, &x, &y, &width, &height, geometry,
		default_window,	XXborder, "Gnuemacs");
  (void) signal (SIGIO, handle);
  XSetWindowSize (height, width);
/*  XSetWindowSize ((height - (2 * XXborder))/fontinfo -> height,
    (width - (2 * XXborder))/fontinfo -> width);*/
  XSetOffset (x, y);
  XMapWindow (XXwindow);
  XFlush();
  return Qnil;
}

DEFUN ("x-set-border-width", Fx_set_border_width, Sx_set_border_width,
  1, 1, 0,
  "Set width of border to WIDTH, in the X window system.\n\
Works only before the window has been mapped.")
  (borderwidth)
     register Lisp_Object borderwidth;
{
  extern int WindowMapped;
  extern int XXborder;
  WindowInfo WinInfo;
  extern Window XXwindow;
  extern FontInfo *fontinfo;
  extern Cursor EmacsCursor;
  extern char iconidentity[];
  register int (*func) ();
  extern int CurHL;
  Window tempwindow;
  extern int pixelwidth, pixelheight;
  register int temppixelwidth;
  register int temppixelheight;
  register int tempx;
  register int tempy;
  extern int XXxoffset, XXyoffset;
  extern int XXpid;
  Pixmap temp_brdr, temp_back;

  CHECK_NUMBER (borderwidth, 1);

  check_xterm ();
  
  if (XINT (borderwidth) < 0) XSETINT (borderwidth, 0);
  
  temppixelwidth = screen_width * fontinfo->width;
  temppixelheight = screen_height * fontinfo->height;
  func = signal (SIGIO, SIG_IGN);
  XQueryWindow (XXwindow, &WinInfo);
  tempx = WinInfo.x;
  tempy = WinInfo.y;
  if (strcmp (brdr_color, "gray") == 0)
    temp_brdr = XMakePixmap ((Bitmap) XStoreBitmap (16, 16, gray_bits),
			     BlackPixel, WhitePixel);
  else
    temp_brdr = XMakeTile(brdr);
  temp_back = XMakeTile(back);
  tempwindow = XCreateWindow (RootWindow,
			      tempx /* Absolute horizontal offset */,
			      tempy /* Absolute Vertical offset */,
			      temppixelwidth, temppixelheight,
			      XINT (borderwidth),
			      temp_brdr, temp_back);
  if (tempwindow) 
    {
      XDestroyWindow (XXwindow);
      XXwindow = tempwindow;
      pixelwidth = temppixelwidth;
      pixelheight = temppixelheight;
      XXborder = XINT (borderwidth);
      XSelectInput (XXwindow, NoEvent);
      XSetResizeHint (XXwindow, fontinfo->width * 10, fontinfo->height *5, 
		      fontinfo->width, fontinfo->height);
      XStoreName (XXwindow, &iconidentity[0]);
      XDefineCursor (XXwindow, EmacsCursor);
      XFreePixmap(temp_brdr);
      XFreePixmap(temp_back);
      (void)signal (SIGIO, func);
      if (QLength () > 0)
	{
	  kill (XXpid, SIGIO);
	}
      if (WindowMapped)
	{
	  XMapWindow (XXwindow);
	  XSelectInput (XXwindow, KeyPressed | ExposeWindow |
			ButtonPressed | ExposeRegion |
			  ExposeCopy);
	  ++screen_garbaged;
	  XFlush ();
	}
      return Qt;
    }
  else
    {
      (void) signal (SIGIO, func);
      if (QLength () > 0)
	{
	  kill (XXpid, SIGIO);
	}
      message ("Could not recreate window.");
      return Qnil;
    }
}

jmp_buf dispenv;
Display *OldDisplay;
FontInfo *OldFontInfo;
Window OldWindow;

XRestoreDisplay ()
{
  longjmp (dispenv, "Unable to access display (probably)");
}

DEFUN ("x-change-display", Fx_change_display, Sx_change_display, 1, 1,
  "sNew display name: ", 
  "This function takes one argument, the display where you wish to\n\
continue your editing session.  Your current window will be unmapped and\n\
the current display will be closed.  The new X display will be opened and\n\
the rubber-band outline of the new window will appear on the new X display.\n\
This function does not look at your .Xdefaults file, so you should use the\n\
function x-new-display instead.")
  (new_display)
     register Lisp_Object new_display;
{
  extern Cursor EmacsCursor;
  Cursor OldEmacsCursor;
  register int (*sigfunc) (), (*pipefunc) ();
  register char *newdisplayname = 0;
  extern char iconidentity[];
  extern Display *XXdisplay;
  extern Window XXwindow;
  extern Window XXIconWindow;
  extern int IconWindow;
  extern Bitmap XXIconMask;
  extern int pixelwidth, pixelheight, XXborder, CurHL;
  extern FontInfo *fontinfo;
  extern int bitblt, CursorExists, VisibleX, VisibleY;
  extern WindowInfo rootwindowinfo;
  extern char MouseCursor[], MouseMask[];
  int old_fcntl_flags, old_fcntl_owner;
  int x, y, width, height;
  int temp_icon;
  Pixmap temp_brdr, temp_back;
  register char *XXerrorcode;
  extern int XXxoffset, XXyoffset;	

  CHECK_STRING (new_display, 1);
  check_xterm ();

/*  newdisplayname = xmalloc (XSTRING (new_display)->size + 1); */
/*  bcopy (XSTRING (new_display)->data, newdisplayname, */
/*	 XSTRING (new_display)->size + 1);  */
  /* Since this was freed at the end, why not just use the original? */
  newdisplayname = (char *) XSTRING (new_display)->data;
  sigfunc = signal (SIGIO, SIG_IGN);
  XIOErrorHandler(XRestoreDisplay);
   if (XXerrorcode = (char *) setjmp (dispenv))
     {
 /*       free (&newdisplayname[0]); */
       if (fontinfo)
	 XCloseFont (fontinfo);
       if (XXwindow)
	 XDestroyWindow (XXwindow);
       if (XXdisplay)
	 XCloseDisplay (XXdisplay);
       XXdisplay = OldDisplay;
       fontinfo = OldFontInfo;
       XXwindow = OldWindow;
       EmacsCursor = OldEmacsCursor;
       XIOErrorHandler (0);
       XSetDisplay (XXdisplay);
       (void)signal (SIGIO, sigfunc);
       if (QLength () > 0)
	 {
	   kill (XXpid, SIGIO);
	 }
       error ("Display change problem: %s", XXerrorcode);
     }
   else
     {
       OldEmacsCursor = EmacsCursor;
       OldDisplay = XXdisplay;
       OldFontInfo = fontinfo;
       OldWindow = XXwindow;
       XXwindow = 0;
       fontinfo = 0;
       XXdisplay = 0;
     }
   XXdisplay = XOpenDisplay (newdisplayname);
   if (!XXdisplay)
     {
       longjmp (dispenv, "Probably nonexistant display");
     }
   XQueryWindow (RootWindow, &rootwindowinfo);
   fontinfo = XOpenFont (XXcurrentfont);
   if (!fontinfo)
     {
       longjmp (dispenv, "Bad font");
     }
   /* pixelwidth and pixelheight are correct*/
   XXwindow = XCreateWindow (RootWindow,
			     XXxoffset,
			     XXyoffset,
			     pixelwidth, pixelheight,
			     XXborder, BlackPixmap, WhitePixmap);
   if (!XXwindow)
     {
       longjmp (dispenv, "Could not create window");
     }
   fore = BlackPixel;
   back = WhitePixel;
   brdr = BlackPixel;
   mous = BlackPixel;
   curs = BlackPixel;

   fore_color = "black";
   back_color = "white";
   brdr_color = "black";
   mous_color = "black";
   curs_color = "black";

   XSelectInput (XXwindow, NoEvent);
   EmacsCursor = XCreateCursor (16, 16, MouseCursor, MouseMask,
				0, 0, mous, back, GXcopy);
   XDefineCursor (XXwindow, EmacsCursor);

   XSetResizeHint (XXwindow, fontinfo->width * 10, fontinfo->height * 5, 
		  fontinfo->width, fontinfo->height);
   XStoreName (XXwindow, iconidentity);
/*  WindowMapped = 0;*/
  x_edges_specified = 0;
  bitblt = 0;
  CursorExists = 0;
  VisibleX = 0;
  VisibleY = 0;
  XSetDisplay (XXdisplay);
/*  XQueryWindow (RootWindow, &rootwindowinfo);*/
/*  if (WindowMapped)
    {*/
  WindowMapped = 0;
  XPopUpWindow ();
/*  }*/
  WindowMapped = 1;
  XXIconWindow = XCreateWindow (RootWindow, 0, 0, sink_width, sink_height,
				2, WhitePixmap, BlackPixmap);
  XXIconMask = XStoreBitmap(sink_mask_width, sink_mask_height, sink_mask_bits);
  XSetDisplay (OldDisplay);
  XCloseFont (OldFontInfo);
  XFreeCursor (OldEmacsCursor);
  XDestroyWindow (OldWindow);
  XSetDisplay (XXdisplay);
  XCloseDisplay (OldDisplay);
  temp_icon = IconWindow;
  XBitmapIcon;
  XTextIcon;
  if (temp_icon)
    {
	IconWindow = 0;
	XBitmapIcon;
    }
  XErrorHandler (0);
  dup2 (dpyno (), 0);
  close (dpyno ());
  dpyno () = 0;		/* Looks a little strange? */
  /* check the def of the */
  /* macro, it is a genuine */
  /* lvalue */
  old_fcntl_flags = fcntl (0, F_GETFL, 0);
  fcntl (0, F_SETFL, old_fcntl_flags | FASYNC);
  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
  fcntl (0, F_SETOWN, getpid ());
  (void)signal (SIGIO, sigfunc);
  if (QLength () > 0)
    {
      kill (XXpid, SIGIO);
    }
/*  free (newdisplayname); */
/*  x_edges_specified = 0;*/
  ++screen_garbaged;
  Fredraw_display();
  return Qt;
}

/*
   Grabs mouse, outlines a window, etc.
   if left button pressed, sizes a wd x hd window (in characters)
   if right button pressed, sizes wd x what will fit window (in characters)
   if middle button pressed, allows user to size window in font increments
   	(+ border * 2 for inner border);
   While sizing, dimensions of window are displayed in upper left of root.
   str is also displayed there.
   In all cases, x and y are the desired coordinates for the upper lefthand
   	corner, *width = width desired, *height = height desired
	(min for both is 1 font char).

	*/
/*
  This routine is a total crock.  It makes a window using XCreateTerm
  purely for return value, destroying the temporary window created in
  the process.  If XCreateTerm were broken into smaller, more easily
  digestible pieces, it would be useful.  As such, the constraints of
  time, emacs, and X conventions force me into this crock. --rlk
  */

window_fetch(font, x, y, width, height, geo, deflt, border, str)
Font font;
int *x, *y, *width, *height;
char *geo, *deflt;
int border;
char *str;
{
  extern int WindowMapped;
  extern int XXborder;
  extern Window XXwindow;
  extern FontInfo *fontinfo;
  extern Cursor EmacsCursor;
  OpaqueFrame frame;
  extern char iconidentity[];
  register int (*func) ();
  Window tempwindow;
  WindowInfo WinInfo;
  extern int pixelwidth, pixelheight;
  register int temppixelwidth;
  register int temppixelheight;
  extern int XXxoffset, XXyoffset;
  extern int XXpid;
  Pixmap temp_brdr, temp_back;

  func = signal (SIGIO, SIG_IGN);
  temp_brdr = XMakeTile(brdr);
  temp_back = XMakeTile(back);
  frame.bdrwidth = border;
  frame.border = XMakeTile (brdr);
  frame.background = XMakeTile (back);
  tempwindow = XCreateTerm(str, "emacs", geo, deflt, &frame, 10, 5, 0, 0,
			   width, height, fontinfo, fontinfo->width,
			   fontinfo->height);
  if (tempwindow) 
    {
      XDestroyWindow (XXwindow);
      XXwindow = tempwindow;
      XSelectInput (XXwindow, NoEvent);
      XSetResizeHint (XXwindow, fontinfo->width * 10, fontinfo->height *5, 
		      fontinfo->width, fontinfo->height);
      XStoreName (XXwindow, &iconidentity[0]);
      XDefineCursor (XXwindow, EmacsCursor);
      XQueryWindow (XXwindow, &WinInfo);
      *x = WinInfo.x;
      *y = WinInfo.y;
      XFreePixmap(temp_brdr);
      XFreePixmap(temp_back);
      (void)signal (SIGIO, func);
      if (QLength () > 0)
	{
	  kill (XXpid, SIGIO);
	}
      if (WindowMapped)
	{
	  XMapWindow (XXwindow);
	  XSelectInput (XXwindow, KeyPressed | ExposeWindow |
			ButtonPressed | ExposeRegion |
			  ExposeCopy);
	  ++screen_garbaged;
	  XFlush ();
	}
      return Qt;
    }
  else
    {
      (void) signal (SIGIO, func);
      if (QLength () > 0)
	{
	  kill (XXpid, SIGIO);
	}
      message ("Could not recreate window.");
      return Qnil;
    }
}

setxterm ()
{
  Vxterm = Qt;
}

XRedrawDisplay()
{
    Fredraw_display();
}

XAutoSave()
{
  Fdo_auto_save();
}


syms_of_xfns ()
{
  x_edges_specified = 0;

  DefLispVar("xterm", &Vxterm,
	     "True if using xterm, nil otherwise.");
  Vxterm = Qnil;
  DefLispVar("x-mouse-pos", &Vx_mouse_pos,
     "Current x-y position of mouse by row, column as specified by font.");
  Vx_mouse_pos = Qnil;

  defsubr (&Sx_pop_up_window);
  defsubr (&Sx_set_bell);
  defsubr (&Sx_flip_color);
  defsubr (&Sx_set_icon);
  defsubr (&Sx_set_font);
  defsubr (&Sx_set_window_edges);
  defsubr (&Scoordinates_in_window_p);
  defsubr (&Sx_mouse_events);
  defsubr (&Sx_proc_mouse_event);
  defsubr (&Sx_get_mouse_event);
  defsubr (&Sx_set_keyboard_enable);
  defsubr (&Sx_set_mouse_inform_flag);
  defsubr (&Sx_store_cut_buffer);
  defsubr (&Sx_get_cut_buffer);
  defsubr (&Sx_rubber_band);
  defsubr (&Sx_create_x_window);
  defsubr (&Sx_set_border_width);
  defsubr (&Sx_change_display);
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
}
