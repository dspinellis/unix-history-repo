/* Functions for Sun Windows menus and selection buffer.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

Author: Jeff Peck, Sun Microsystems, Inc. <peck@sun.com>
Original ideas by David Kastan and Eric Negaard, SRI International
Major help from: Steve Greenbaum, Reasoning Systems, Inc.
 		    <froud@kestrel.arpa>
who first discovered the Menu_Base_Kludge.
*/

/*
 *	Emacs Lisp-Callable functions for sunwindows
 */
#include "config.h"

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <sunwindow/window_hs.h>
#include <suntool/selection.h>
#include <suntool/menu.h>
#include <suntool/walkmenu.h>
#include <suntool/frame.h>
#include <suntool/window.h>

#include <fcntl.h>
#undef NULL /* We don't need sunview's idea of NULL */
#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "termhooks.h"

/* conversion to/from character & screen coordinates */
/* From Gosling Emacs SunWindow driver by Chris Torek */

/* Chars to screen coords.  Note that we speak in zero origin. */
#define CtoSX(cx) ((cx) * Sun_Font_Xsize)
#define CtoSY(cy) ((cy) * Sun_Font_Ysize)

/* Screen coords to chars */
#define StoCX(sx) ((sx) / Sun_Font_Xsize)
#define StoCY(sy) ((sy) / Sun_Font_Ysize)

#define CHECK_GFX(x) if((win_fd<0)&&(Fsun_window_init(),(win_fd<0)))return(x)
int win_fd = -1;
struct pixfont *Sun_Font;	/* The font */
int Sun_Font_Xsize;		/* Width of font  */
int Sun_Font_Ysize;		/* Height of font */

#define Menu_Base_Kludge	/* until menu_show_using_fd gets fixed */
#ifdef  Menu_Base_Kludge
static Frame Menu_Base_Frame;
static int Menu_Base_fd;
static Lisp_Object sm_kludge_string;
#endif
struct cursor CurrentCursor;	/* The current cursor */

static short CursorData[16];	/* Build cursor here */
static mpr_static(CursorMpr, 16, 16, 1, CursorData);
static struct cursor NewCursor = {0, 0, PIX_SRC ^ PIX_DST, &CursorMpr};

#define RIGHT_ARROW_CURSOR	/* if you want the right arrow */
#ifdef RIGHT_ARROW_CURSOR
/* The default right-arrow cursor, with XOR drawing. */
static short ArrowCursorData[16] = {
  0x0001,0x0003,0x0007,0x000F,0x001F,0x003F,0x007F,0x000F,
  0x001B,0x0019,0x0030,0x0030,0x0060,0x0060,0x00C0,0x00C0};
static mpr_static(ArrowCursorMpr, 16, 16, 1, ArrowCursorData);
struct cursor DefaultCursor = {15, 0, PIX_SRC ^ PIX_DST, &ArrowCursorMpr};

#else
/* The default left-arror cursor, with XOR drawing. */
static short ArrowCursorData[16] = {
	0x8000,0xC000,0xE000,0xF000,0xF800,0xFC00,0xFE00,0xF000,
	0xD800,0x9800,0x0C00,0x0C00,0x0600,0x0600,0x0300,0x0300};
static mpr_static(ArrowCursorMpr, 16, 16, 1, ArrowCursorData);
struct cursor DefaultCursor = {0, 0, PIX_SRC ^ PIX_DST, &ArrowCursorMpr};
#endif

/*
 *	Initialize window
 */
DEFUN ("sun-window-init", Fsun_window_init, Ssun_window_init, 0, 1, 0,
       "One time setup for using Sun Windows with mouse.\n\
Unless optional argument FORCE is non-nil, is a noop after its first call.\n\
Returns a number representing the file descriptor of the open Sun Window,\n\
or -1 if can not open it.")
      (force)
      Lisp_Object force;
{
  char *cp;
  static int already_initialized = 0;

  if ((! already_initialized) || (!NULL(force))) {
    cp = getenv("WINDOW_GFX");
    if (cp != 0) win_fd = open(cp, 2);
    if (win_fd > 0)
      {
	Sun_Font = pf_default();
	Sun_Font_Xsize = Sun_Font->pf_defaultsize.x;
	Sun_Font_Ysize = Sun_Font->pf_defaultsize.y;
	Fsun_change_cursor_icon (Qnil);	/* set up the default cursor */
	already_initialized = 1;
#ifdef  Menu_Base_Kludge

	/* Make a frame to use for putting the menu on, and get its fd. */
	Menu_Base_Frame = window_create(0, FRAME,
					WIN_X, 0, WIN_Y, 0,
					WIN_ROWS, 1, WIN_COLUMNS, 1,
					WIN_SHOW, FALSE,
					FRAME_NO_CONFIRM, 1,
					0);
	Menu_Base_fd = (int) window_get(Menu_Base_Frame, WIN_FD);
#endif
      }
  }
  return(make_number(win_fd));
}

/*
 *	Mouse sit-for (allows a shorter interval than the regular sit-for
 *	and can be interrupted by the mouse)
 */
DEFUN ("sit-for-millisecs", Fsit_for_millisecs, Ssit_for_millisecs, 1, 1, 0,
   "Like sit-for, but ARG is milliseconds. \n\
Perform redisplay, then wait for ARG milliseconds or until\n\
input is available.  Returns t if wait completed with no input.\n\
Redisplay does not happen if input is available before it starts.")
	(n)
	Lisp_Object n;
{
  struct timeval Timeout;
  int waitmask = 1;
  
  CHECK_NUMBER (n, 0);
  Timeout.tv_sec = XINT(n) / 1000;
  Timeout.tv_usec = (XINT(n) - (Timeout.tv_sec * 1000)) * 1000;

  if (detect_input_pending()) return(Qnil);
  redisplay ();
  /*
   *	Check for queued keyboard input/mouse hits again
   *	(A bit screen update can take some time!)
   */
  if (detect_input_pending()) return(Qnil);
  select(1,&waitmask,0,0,&Timeout);
  if (detect_input_pending()) return(Qnil);
  return(Qt);
}

/*
 *   Sun sleep-for (allows a shorter interval than the regular sleep-for)
 */
DEFUN ("sleep-for-millisecs", 
	Fsleep_for_millisecs,
	Ssleep_for_millisecs, 1, 1, 0,
   "Pause, without updating display, for ARG milliseconds.")
	(n)
	Lisp_Object n;
{
  unsigned useconds;

  CHECK_NUMBER (n, 0);
  useconds = XINT(n) * 1000;
  usleep(useconds);
  return(Qt);
}

DEFUN ("update-display", Fupdate_display, Supdate_display, 0, 0, 0,
       "Perform redisplay.")
     ()
{
  redisplay_preserve_echo_area ();
  return(Qt);
}


/*
 *	Change the Sun mouse icon
 */
DEFUN ("sun-change-cursor-icon",
	Fsun_change_cursor_icon,
	Ssun_change_cursor_icon, 1, 1, 0,
  "Change the Sun mouse cursor icon.  ICON is a lisp vector whose 1st element\n\
is the X offset of the cursor hot-point, whose 2nd element is the Y offset\n\
of the cursor hot-point and whose 3rd element is the cursor pixel data\n\
expressed as a string.  If ICON is nil then the original arrow cursor is used")
     (Icon)
     Lisp_Object Icon;
{
  register unsigned char *cp;
  register short *p;
  register int i;
  Lisp_Object X_Hot, Y_Hot, Data;
  
  CHECK_GFX (Qnil);
  /*
   *	If the icon is null, we just restore the DefaultCursor
   */
  if (NULL(Icon)) 
    CurrentCursor = DefaultCursor;
  else {
    /*
     *	extract the data from the vector
     */
    CHECK_VECTOR (Icon, 0);
    if (XVECTOR(Icon)->size < 3) return(Qnil);
    X_Hot = XVECTOR(Icon)->contents[0];
    Y_Hot = XVECTOR(Icon)->contents[1];
    Data = XVECTOR(Icon)->contents[2];
    
    CHECK_NUMBER (X_Hot, 0);
    CHECK_NUMBER (Y_Hot, 0);
    CHECK_STRING (Data, 0);
    if (XSTRING(Data)->size != 32) return(Qnil);
    /*
     *	Setup the new cursor
     */
    NewCursor.cur_xhot = X_Hot;
    NewCursor.cur_yhot = Y_Hot;
    cp = XSTRING(Data)->data;
    p = CursorData;
    i = 16;
    while(--i >= 0)
      *p++ = (cp[0] << 8) | cp[1], cp += 2;
    CurrentCursor = NewCursor;
  }
  win_setcursor(win_fd, &CurrentCursor);
  return(Qt);
}

/*
 *	Interface for sunwindows selection
 */
static Lisp_Object Current_Selection;

static
sel_write (sel, file)
     struct selection *sel;
     FILE *file;
{
  fwrite (XSTRING (Current_Selection)->data, sizeof (char), 
	  sel->sel_items, file);
}

static
sel_clear (sel, windowfd)
     struct selection *sel;
     int windowfd;
{
}

static
sel_read (sel, file)
     struct selection *sel;
     FILE *file;
{
  register int i, n;
  register char *cp;
  
  Current_Selection = make_string ("", 0);
  if (sel->sel_items <= 0)
    return (0);
  cp = (char *) malloc(sel->sel_items);
  if (cp == (char *)0) {
    error("malloc failed in sel_read");
    return(-1);
  }
  n = fread(cp, sizeof(char), sel->sel_items, file);
  if (n > sel->sel_items) {
    error("fread botch in sel_read");
    return(-1);
  } else if (n < 0) {
    error("Error reading selection.");
    return(-1);
  }
  /*
   * The shelltool select saves newlines as carrige returns,
   * but emacs wants newlines.
   */
  for (i = 0; i < n; i++) 
    if (cp[i] == '\r') cp[i] = '\n';

  Current_Selection = make_string (cp, n);
  free (cp);
  return (0);
}

/*
 *	Set the window system "selection" to be the arg STRING
 */
DEFUN ("sun-set-selection", Fsun_set_selection, Ssun_set_selection, 1, 1,
       "sSet selection to: ",
  "Set the current sunwindow selection to STRING.")
     (str)
     Lisp_Object str;
{
  struct selection selection;

  CHECK_STRING (str, 0);
  Current_Selection = str;

  CHECK_GFX (Qnil);
  selection.sel_type = SELTYPE_CHAR;
  selection.sel_items = XSTRING (str)->size;
  selection.sel_itembytes = 1;
  selection.sel_pubflags = 1;
  selection_set(&selection, sel_write, sel_clear, win_fd);
  return (Qt);
}
/*
 *	Stuff the current window system selection into the current buffer
 */
DEFUN ("sun-get-selection", Fsun_get_selection, Ssun_get_selection, 0, 0, 0,
       "Return the current sunwindows selection as a string.")
     ()
{
  CHECK_GFX (Current_Selection);
  selection_get (sel_read, win_fd);
  return (Current_Selection);
}

Menu sun_menu_create();

Menu_item
sun_item_create (Pair)
     Lisp_Object Pair;
{
  /* In here, we depend on Lisp supplying zero terminated strings in the data*/
  /* so we can just pass the pointers, and not recopy anything */

  Menu_item menu_item;
  Menu submenu;
  Lisp_Object String;
  Lisp_Object Value;

  if (!CONSP(Pair)) wrong_type_argument(Qlistp, Pair);
  String = Fcar(Pair);
  CHECK_STRING(String, 0);
  Value = Fcdr(Pair);
  if(XTYPE(Value) == Lisp_Symbol)
    Value = XSYMBOL(Value)->value;
  if(XTYPE(Value) == Lisp_Vector) {
    submenu = sun_menu_create (Value);
    menu_item = menu_create_item
      (MENU_RELEASE, MENU_PULLRIGHT_ITEM, XSTRING(String)->data, submenu, 0);
  } else {
    menu_item = menu_create_item
      (MENU_RELEASE, MENU_STRING_ITEM, XSTRING(String)->data, Value, 0);
  }
  return menu_item;
}

Menu 
sun_menu_create (Vector)
     Lisp_Object Vector;
{
  Menu menu;
  int i;
  CHECK_VECTOR(Vector,0);
  menu=menu_create(0); 
  for(i = 0; i < XVECTOR(Vector)->size; i++) {
    menu_set (menu, MENU_APPEND_ITEM, 
	      sun_item_create(XVECTOR(Vector)->contents[i]), 0);
  }
  return menu;
}

/*
 *  If the first item of the menu has nil as its value, then make the
 *  item look like a label by inverting it and making it unselectable.
 *  Returns 1 if the label was made, 0 otherwise.
 */
int
make_menu_label (menu)
     Menu menu;
{
  int made_label_p = 0;

  if (( menu_get(menu, MENU_NITEMS) > 0 ) && /* At least one item */
      ((Lisp_Object) menu_get(menu_get(menu, MENU_NTH_ITEM, 1),
			      MENU_VALUE) == Qnil )) {
      menu_set(menu_get(menu, MENU_NTH_ITEM, 1),
	       MENU_INVERT, TRUE,
	       MENU_FEEDBACK, FALSE,
	       0);
      made_label_p = 1;
    }
  return made_label_p;
}

/*
 *	Do a pop-up menu and return the selected value
 */
DEFUN ("sun-menu-internal",
       Fsun_menu_internal,
       Ssun_menu_internal, 5, 5, 0,
       "Set up a SunView pop-up menu and return the user's choice.\n\
Arguments WINDOW, X, Y, BUTTON, and MENU.\n\
*** User code should generally use sun-menu-evaluate ***\n\
\n\
Arguments WINDOW, X, Y, BUTTON, and MENU.\n\
Put MENU up in WINDOW at position X, Y.\n\
The BUTTON argument specifies the button to be released that selects an item:\n\
   1 = LEFT BUTTON\n\
   2 = MIDDLE BUTTON\n\
   4 = RIGHT BUTTON\n\
The MENU argument is a vector containing (STRING . VALUE) pairs.\n\
The VALUE of the selected item is returned.\n\
If the VALUE of the first pair is nil, then the first STRING will be used\n\
as a menu label.")
      (window, X_Position, Y_Position, Button, MEnu)
      Lisp_Object window, X_Position, Y_Position, Button, MEnu;
{
  Menu menu;
  int button, xpos, ypos;
  Event event0;
  Event *event = &event0;
  Lisp_Object Value, Pair;
  
  CHECK_NUMBER(X_Position, 0);
  CHECK_NUMBER(Y_Position, 1);
  CHECK_WINDOW(window, 2);
  CHECK_NUMBER(Button, 3);
  CHECK_VECTOR(MEnu, 4);

  CHECK_GFX (Qnil);

  xpos = CtoSX (XWINDOW(window)->left + XINT(X_Position));
  ypos = CtoSY (XWINDOW(window)->top  + XINT(Y_Position));
#ifdef  Menu_Base_Kludge
  {static Lisp_Object symbol[2];
   symbol[0] = Fintern (sm_kludge_string, Qnil);
   Pair = Ffuncall (1, symbol);
   xpos += XINT (XCONS (Pair)->cdr);
   ypos += XINT (XCONS (Pair)->car);
 }
#endif

  button = XINT(Button);
  if(button == 4) button = 3;
  event_set_id   (event, BUT(button));
  event_set_down (event);
  event_set_x    (event, xpos);
  event_set_y    (event, ypos);

  menu = sun_menu_create(MEnu);
  make_menu_label(menu);

#ifdef  Menu_Base_Kludge
  Value = (Lisp_Object) menu_show(menu, Menu_Base_Frame, event, 0);
#else
/* This confuses the notifier or something: */
  Value = (Lisp_Object) menu_show_using_fd(menu, win_fd, event, 0);
/*
 * Right button gets lost, and event sequencing or delivery gets mixed up
 * So, until that gets fixed, we use this <Menu_Base_Frame> kludge:
 */
#endif
  menu_destroy (menu);

  return ((int)Value ? Value : Qnil);
}


/*
 *	Define everything
 */
syms_of_sunfns()
{
#ifdef  Menu_Base_Kludge
  /* i'm just too lazy to re-write this into C code */
  /* so we will call this elisp function from C */
  sm_kludge_string = make_pure_string ("sm::menu-kludge", 15);
#endif /* Menu_Base_Kludge */

  defsubr(&Ssun_window_init);
  defsubr(&Ssit_for_millisecs);
  defsubr(&Ssleep_for_millisecs);
  defsubr(&Supdate_display);
  defsubr(&Ssun_change_cursor_icon);
  defsubr(&Ssun_set_selection);
  defsubr(&Ssun_get_selection);
  defsubr(&Ssun_menu_internal);
}
