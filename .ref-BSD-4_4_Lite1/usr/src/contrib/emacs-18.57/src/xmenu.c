/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1986, 1988 Free Software Foundation, Inc.

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

 * X pop-up deck-of-cards menu facility for gnuemacs.
 *
 * Written by Jon Arnold and Roman Budzianowski
 * Mods and rewrite by Robert Krawitz
 *
 */

/* $Source: /u2/third_party/gnuemacs.chow/src/RCS/xmenu.c,v $
 * $Author: rlk $
 * $Locker:  $
 * $Header: xmenu.c,v 1.6 86/08/26 17:23:26 rlk Exp $
 *
 */

#ifndef lint
static char *rcsid_GXMenu_c = "$Header: xmenu.c,v 1.6 86/08/26 17:23:26 rlk Exp $";
#endif	lint

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>
#include "config.h"
#include "lisp.h"
#include "window.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#ifdef X11
#include "x11term.h"
#else
#include "xterm.h"
#endif

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#include "dispextern.h"

#ifdef X11
#include "../oldXMenu/XMenu.h"
#define X11ONLY(arg) arg,
#else
#include <X/XMenu.h>
#define X11ONLY(arg)
#endif

#include "termopts.h"

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

#define NUL 0

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif TRUE

#ifdef X11
extern XFontStruct *fontinfo;
extern Display *XXdisplay;
extern Window	XXwindow;
extern int	XXfontw;
extern int	XXfonth;
int	XXscreen;
#else
extern int XXxoffset,XXyoffset;
extern FontInfo *fontinfo;
#define	ButtonReleaseMask ButtonReleased
#endif /* not X11 */

extern int (*handler)();

/*************************************************************/

/* Ignoring the args is easiest.  */
XMenuQuit ()
{
  error("Unknown XMenu error");
}

DEFUN ("x-popup-menu",Fx_popup_menu, Sx_popup_menu, 1, 2, 0,
       "Interface to XMenu library; permits creation and use of\n\
deck-of-cards pop-up menus.\n\
\n\
ARG is a position specification; it is a list of (Xoffset, Yoffset)\n\
from the top-left corner of the editor window.  Note that this offset\n\
is relative to the center of the first line in the first pane of the\n\
menu, not the top left of the menu as a whole.\n\
\n\
MENU is a specifier for a menu.  It is a list of the form\n\
(title pane1 pane2...), and each pane is a list of form\n\
(title (line item)...).  Each line should be a string, and item should\n\
be the return value for that line (i. e. if it is selected.")
       (arg,menu)
     Lisp_Object arg,menu;
{
  int number_of_panes;
  Lisp_Object XMenu_return;
  int XMenu_xpos,XMenu_ypos;
  char **menus;
  char ***names;
  Lisp_Object **obj_list;
  int *items;
  char *title;
  char *error_name;
  Lisp_Object ltitle, selection;
  Lisp_Object XEmacsMenu();
  int i, j;
#ifdef X11
  Window root_window, wjunk;
  int ijunk;
  extern Lisp_Object Vx_mouse_abs_pos;
#endif
  BLOCK_INPUT_DECLARE ();

  check_xterm();
#ifdef X11
#if 0
  if (interrupt_input)
    unrequest_sigio ();
#endif
  BLOCK_INPUT ();
  root_window = RootWindow (XXdisplay, DefaultScreen(XXdisplay));
  UNBLOCK_INPUT ();
  XMenu_xpos = Fcar (Vx_mouse_abs_pos);
  XMenu_ypos = Fcar (Fcdr (Vx_mouse_abs_pos));
#if 0
  if (interrupt_input)
    request_sigio ();
#endif
#else
  XMenu_xpos = fontinfo->width * XINT(Fcar(arg));
  XMenu_ypos = fontinfo->height * XINT(Fcar(Fcdr (arg)));
  XMenu_xpos += XXxoffset;
  XMenu_ypos += XXyoffset;
#endif
  ltitle = Fcar(menu);
  CHECK_STRING(ltitle,1);
  title = (char *) XSTRING(ltitle)->data;
  number_of_panes=list_of_panes(&obj_list, &menus, &names, &items, Fcdr(menu));
#ifdef XDEBUG
  fprintf(stderr,"Panes= %d\n", number_of_panes);
  for(i=0; i < number_of_panes; i++)
    {
      fprintf (stderr,"Pane %d lines %d title %s\n", i, items[i], menus[i]);
      for (j=0; j < items[i]; j++)
	{
	  fprintf (stderr,"    Item %d %s\n", j, names[i][j]);
	}
    }
#endif
  BLOCK_INPUT ();
#ifdef X11
  XSetErrorHandler(XMenuQuit);
  selection = XEmacsMenu(root_window, XMenu_xpos, XMenu_ypos, names, menus,
			 items, number_of_panes, obj_list ,title, &error_name);
  XSetErrorHandler(handler);
#else
  XErrorHandler(XMenuQuit);
  selection = XEmacsMenu(RootWindow, XMenu_xpos, XMenu_ypos, names, menus,
			 items, number_of_panes, obj_list ,title, &error_name);
  XErrorHandler(handler);
#endif
  UNBLOCK_INPUT ();
  force_input_read ();
  /** fprintf(stderr,"selection = %x\n",selection);  **/
  if (selection != NUL)
    {				/* selected something */
      XMenu_return = selection;
    }
  else
    {				/* nothing selected */
      XMenu_return = Qnil;
    }
  /* now free up the strings */
  for (i=0; i < number_of_panes; i++)
    {
      free(names[i]);
      free(obj_list[i]);
    }
  free(menus);
  free(obj_list);
  free(names);
  free(items);
  /*   free(title); */
  if (error_name) error(error_name);
  return XMenu_return;
}

struct indices {
  int pane;
  int line;
};

Lisp_Object
XEmacsMenu(parent, startx, starty, line_list, pane_list, line_cnt,
	   pane_cnt, item_list, title, error)
     Window parent;		
     int startx, starty;	/* upper left corner position BROKEN */
     char **line_list[];   	/* list of strings for items */
     char *pane_list[];		/* list of pane titles */
     char *title;
     int pane_cnt;		/* total number of panes */
     Lisp_Object *item_list[];	/* All items */
     int line_cnt[];		/* Lines in each pane */
     char **error;		/* Error returned */
{
  XMenu *GXMenu;
  int last, panes, selidx, lpane, status;
  int lines, sofar;
  Lisp_Object entry;
  /* struct indices *datap, *datap_save; */
  char *datap;
  int ulx = 0, uly = 0, width, height;
  int dispwidth, dispheight;
  
  *error = (char *) 0;		/* Initialize error pointer to null */
  if ((GXMenu = XMenuCreate( X11ONLY (XXdisplay) parent, "emacs" )) == NUL )
    {
      *error = "Can't create menu";
      /* error("Can't create menu"); */
      return(0);
    }
  
  for (panes=0, lines=0; panes < pane_cnt; lines += line_cnt[panes], panes++ )
    ;
  /* datap = (struct indices *) xmalloc (lines * sizeof(struct indices)); */
  /*datap = (char *) xmalloc(lines * sizeof(char));
    datap_save = datap;*/
  
  for (panes = 0, sofar=0;panes < pane_cnt;sofar +=line_cnt[panes], panes++ )
    {
      /* create all the necessary panes */
      if ((lpane= XMenuAddPane( X11ONLY (XXdisplay) GXMenu, pane_list[panes] , TRUE ))
	  == XM_FAILURE)
	{
	  XMenuDestroy(X11ONLY (XXdisplay) GXMenu);
	  *error = "Can't create pane";
	  /* error("Can't create pane"); */
	  /* free(datap); */
	  return(0);
	}
      for ( selidx = 0; selidx < line_cnt[panes] ; selidx++ )
	{
	  /* add the selection stuff to the menus */
	  /* datap[selidx+sofar].pane = panes;
	     datap[selidx+sofar].line = selidx; */
	  if (XMenuAddSelection(X11ONLY (XXdisplay) GXMenu, lpane, 0, line_list[panes][selidx], TRUE)
	      == XM_FAILURE)
	    {
	      XMenuDestroy(X11ONLY (XXdisplay) GXMenu);
	      /* free(datap); */
	      *error = "Can't add selection to menu";
	      /* error ("Can't add selection to menu"); */
	      return(0);
	    }
	}
    }
  /* all set and ready to fly */
  XMenuRecompute(X11ONLY (XXdisplay) GXMenu);
#ifdef X11
  XXscreen = DefaultScreen(XXdisplay);
  dispwidth = DisplayWidth(XXdisplay, XXscreen);
  dispheight = DisplayHeight(XXdisplay, XXscreen);
#else
  dispwidth = DisplayWidth();
  dispheight = DisplayHeight();
#endif
  startx = min(startx, dispwidth);
  starty = min(starty, dispheight);
  startx = max(startx, 1);
  starty = max(starty, 1);
  XMenuLocate(X11ONLY (XXdisplay) GXMenu, 0, 0, startx, starty, &ulx, &uly, &width, &height);
  if (ulx+width > dispwidth)
    {
      startx -= (ulx + width) - dispwidth;
      ulx = dispwidth - width;
    }
  if (uly+height > dispheight)
    {
      starty -= (uly + height) - dispheight;
      uly = dispheight - height;
    }
  if (ulx < 0) startx -= ulx;
  if (uly < 0) starty -= uly;
    
  XMenuSetFreeze(GXMenu, TRUE);
  panes = selidx = 0;
  
  status = XMenuActivate(X11ONLY (XXdisplay) GXMenu, &panes, &selidx,
			 startx, starty, ButtonReleaseMask, &datap);
  switch (status ) {
  case XM_SUCCESS:
#ifdef XDEBUG
    fprintf (stderr, "pane= %d line = %d\n", panes, selidx);
#endif
    entry = item_list[panes][selidx];
    break;
  case XM_FAILURE:
    /*free (datap_save); */
    XMenuDestroy(X11ONLY (XXdisplay) GXMenu);
    *error = "Can't activate menu";
    /* error("Can't activate menu"); */
  case XM_IA_SELECT:
  case XM_NO_SELECT:
    entry = Qnil;
    break;
  }
  XMenuDestroy(X11ONLY (XXdisplay) GXMenu);
  /*free(datap_save);*/
  return(entry);
}

syms_of_xmenu ()
{
  defsubr (&Sx_popup_menu);
}

list_of_panes (vector, panes, names, items, menu)
     Lisp_Object ***vector;	/* RETURN all menu objects */
     char ***panes;		/* RETURN pane names */
     char ****names;		/* RETURN all line names */
     int **items;		/* RETURN number of items per pane */
     Lisp_Object menu;
{
  Lisp_Object tail, item, item1;
  int i;
  
  if (XTYPE (menu) != Lisp_Cons) menu = wrong_type_argument(Qlistp, menu);
  
  i= XFASTINT(Flength(menu,1));

  *vector = (Lisp_Object **) xmalloc(i * sizeof (Lisp_Object *));
  *panes = (char **) xmalloc(i * sizeof (char *));
  *items = (int *) xmalloc(i * sizeof (int));
  *names = (char ***) xmalloc(i * sizeof (char **));

  for (i=0,tail = menu; !NULL (tail); tail = Fcdr (tail), i++)
    {
       item = Fcdr(Fcar(tail));
       if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument(Qlistp, item);
#ifdef XDEBUG
       fprintf (stderr, "list_of_panes check tail, i=%d\n", i);
#endif
       item1 = Fcar(Fcar(tail));
       CHECK_STRING (item1, 1);
#ifdef XDEBUG
       fprintf (stderr, "list_of_panes check pane, i=%d%s\n", i,
		XSTRING(item1)->data);
#endif
       (*panes)[i] = (char *) XSTRING(item1)->data;
       (*items)[i] = list_of_items ((*vector)+i, (*names)+i, item);
       /* (*panes)[i] = (char *) xmalloc((XSTRING(item1)->size)+1);
	  bcopy (XSTRING (item1)->data, (*panes)[i], XSTRING(item1)->size + 1)
	  ; */
    }
  return i;
}
     

list_of_items (vector,names,pane)  /* get list from emacs and put to vector */
     Lisp_Object **vector;	/* RETURN menu "objects" */
     char ***names;		/* RETURN line names */
     Lisp_Object pane;
{
  Lisp_Object tail, item, item1;
  int i;

  if (XTYPE (pane) != Lisp_Cons) pane = wrong_type_argument(Qlistp, pane);
  
  i= XFASTINT(Flength(pane,1));

  *vector = (Lisp_Object *) xmalloc(i * sizeof (Lisp_Object));
  *names = (char **) xmalloc(i * sizeof (char *));

  for (i=0,tail = pane; !NULL (tail); tail = Fcdr (tail), i++)
    {
       item = Fcar(tail);
       if (XTYPE (item) != Lisp_Cons) (void) wrong_type_argument(Qlistp, item);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check tail, i=%d\n", i);
#endif
       (*vector)[i] =  Fcdr (item);
       item1 = Fcar(item);
       CHECK_STRING (item1, 1);
#ifdef XDEBUG
       fprintf (stderr, "list_of_items check item, i=%d%s\n", i,
		XSTRING(item1)->data);
#endif
       (*names)[i] = (char *) XSTRING(item1)->data;
       /* (*names)[i] = (char *) xmalloc((XSTRING(item1)->size)+1);
       bcopy (XSTRING (item1)->data, (*names)[i], XSTRING(item1)->size + 1); */
    }
  return i;
}

