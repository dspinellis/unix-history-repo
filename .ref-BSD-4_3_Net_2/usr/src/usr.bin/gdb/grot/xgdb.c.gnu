/* Interface from GDB to X windows.
   Copyright (C) 1987, 1989 Free Software Foundation, Inc.

This file is part of GDB.

GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Original version was contributed by Derek Beatty, 30 June 87.  */

#include <stdio.h>
#include "defs.h"
#include "param.h"
#include "symtab.h"
#include "frame.h"

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Label.h>
#include <X11/Command.h>
#include <X11/AsciiText.h>
#include <X11/Box.h>
#include <X11/VPaned.h>


/*#define	XtNfunction	"function"*/

/* Cursor used in GDB window.  */

#define gdb_width 16
#define gdb_height 16
#define gdb_x_hot 7
#define gdb_y_hot 0
static short gdb_bits[] = {
   0x0000, 0x0140, 0x0220, 0x0220,
   0x23e2, 0x13e4, 0x09c8, 0x0ff8,
   0x0220, 0x3ffe, 0x0630, 0x03e0,
   0x0220, 0x1ffc, 0x2632, 0x01c0};

#define gdb_mask_width 16
#define gdb_mask_height 16
#define gdb_mask_x_hot 7
#define gdb_mask_y_hot 0
static short gdb_mask_bits[] = {
   0x0360, 0x07f0, 0x07f0, 0x77f7,
   0x7fff, 0x7fff, 0x1ffc, 0x1ffc,
   0x7fff, 0x7fff, 0x7fff, 0x0ff8,
   0x3ffe, 0x7fff, 0x7fff, 0x7fff};

/* The X display on which the window appears.  */

Display *screen_display;

#if 0
/* The graphics context.  */
GC default_gc;
#endif

/* Windows manipulated by this package.  */

static Window icon_window;
static Widget main_widget;
static Widget containing_widget;
static Widget source_name_widget;
static Widget source_text_widget;
static Widget exec_name_widget;
static Widget button_box_widget;

/* Source text display.  */

static struct symtab *source_window_symtab = 0;

/* Forward declarations */

static Widget create_text_widget ();

/* Display an appropriate piece of source code in the source window.  */

xgdb_display_source ()
{
  char *filename;
  static Arg labelArgs[1];
  int linenumbers_changed = 0;
  static int new = 1;

  struct symtab_and_line get_selected_frame_sal ();
  struct symtab_and_line sal;
  struct frame_info *fi;

  /* Do nothing if called before we are initialized or when there
     is nothing to show.   */

  if (!containing_widget || !selected_frame) return;

  /* Get the symtab and line number of the selected frame.  */

  fi = get_frame_info (selected_frame);
  sal = find_pc_line (fi->pc, fi->next_frame);

  /* Strictly this is wrong, but better than a blank display */

  if (sal.symtab == NULL) 
    {
      sal.symtab = current_source_symtab;
      /* current_source_line may be off by a small number like 4 */
      sal.line = current_source_line;
    }

  /* Do a path search and get the exact filename of this source file.
     Also scan it and find its source lines if not already done.  */

  if (sal.symtab)
    linenumbers_changed = get_filename_and_charpos (sal.symtab, sal.line,
						    &filename);

  if (!filename) sal.symtab = NULL;

  /* If the source window may be wrong, destroy it (and make a new one).  */

  if (linenumbers_changed || source_window_symtab != sal.symtab)
    {
      static Arg fileArgs[1];
      XtTextSource	src;
      new = 1;
      source_window_symtab = sal.symtab;

      src = XtTextGetSource(source_text_widget);
      XtDiskSourceDestroy(src);
      
      XtSetArg (fileArgs[0], XtNfile, filename);
      src = XtDiskSourceCreate(source_text_widget->core.parent, fileArgs, 1);
      XtTextSetSource(source_text_widget, src, 0);

      XtSetArg (labelArgs[0], XtNlabel,
		filename ? filename : "No source displayed.");
      XtSetValues (source_name_widget, labelArgs, XtNumber (labelArgs));
      if (filename) free (filename);
    }

  /* Update display and cursor positions as necessary.
     Cursor should be placed on line sal.line.  */

  {
    static int top_line_number, bottom_line_number;
    int current_top;
    Arg textArgs[1];

    if (! new)
      {
	int new_top;

	/* Get positions of start of display, and caret */
	XtSetArg (textArgs[0], XtNdisplayPosition, NULL);
	XtGetValues (source_text_widget, textArgs, XtNumber (textArgs));
	new_top = source_charpos_line (source_window_symtab,
				       (int) textArgs[0].value);
	bottom_line_number += new_top - top_line_number;
	top_line_number = new_top;
      }

    /* If appropriate, scroll the text display.  */
    if (sal.line < top_line_number
	|| sal.line > bottom_line_number
	|| new)
      {
	/* yes, these magic numbers are ugly, but I don't know how
	 * to get the height of a text widget in a V11-portable way
	 */
	top_line_number = (sal.line > 15) ? sal.line - 15 : 0;
	bottom_line_number = top_line_number + 35;
      
	XtSetArg (textArgs[0], XtNdisplayPosition,
		  source_line_charpos (source_window_symtab, top_line_number));
	XtSetValues (source_text_widget, textArgs, XtNumber (textArgs));
      }

    /* Set the text display cursor position within the text.  */

    XtSetArg (textArgs[0], XtNinsertPosition, 
	      source_line_charpos (source_window_symtab, sal.line));
    XtSetValues (source_text_widget, textArgs, XtNumber (textArgs));
  }
}

/* Display FILENAME in the title bar at bottom of window.  */

xgdb_display_exec_file (filename)
     char *filename;
{
  static Arg labelArgs[1];

  XtSetArg (labelArgs[0], XtNlabel, filename);
  XtSetValues (exec_name_widget, labelArgs, XtNumber (labelArgs));
}

/* Do any necessary prompting, etc.  */

static char *prompt_string;

static void
print_prompt ()
{
  if (prompt_string)
    printf ("%s", prompt_string);
  fflush (stdout);
}

/* Handlers for buttons.  */

/* Subroutine used by "print" and "print*" buttons.
   STARFLAG is 1 for print*, 0 for print.
   Get the "selection" from X and use it as the operand of a print command.  */

static void
print_button(w, starflag, call_data)
Widget	w;
int	starflag;
caddr_t	call_data;
{
  int selected_length;
  char *selected_text;
  char *cmd = starflag ? "print * " : "print ";
  register int cmdlen = strlen (cmd);
  
  selected_text = XFetchBytes (screen_display, &selected_length);
  if (selected_length)
    {
      char *line = xmalloc (cmdlen + selected_length + 1);
      strcpy (line, cmd);
      strncpy (line + cmdlen, selected_text, selected_length);
      line[cmdlen + selected_length] = 0;

      execute_command (line, 0);

      free (selected_text);
      free (line);
    }

  print_prompt ();
}


/* Subroutine used by "stop at" and "go till" buttons.
   Set a breakpoint at the position indicated by the "selection"
   in the source window, and, if RUNFLAG is nonzero, continue.  */

static void
breakpoint_button(w, runflag, call_data)
Widget	w;
int	runflag;
caddr_t	call_data;
{
  XtTextPosition start, finish;
  
  XtTextGetSelectionPos (source_text_widget, &start, &finish);
  if (!source_window_symtab)
    printf ("No source file displayed.\n");
  else
    {
      set_breakpoint (source_window_symtab, 
		      source_charpos_line (source_window_symtab, start),
		      runflag);
      if (runflag)
	{
	  cont_command (0, 1);
	  xgdb_display_source ();
	}
    }
  print_prompt ();
}

/* decide if a character is trash */
static int
garbage (c)
     char c;
{
  if ('a' <= c && c <= 'z') return 0;
  if ('A' <= c && c <= 'Z') return 0;
  if ('0' <= c && c <= '9') return 0;
  if (c == '_') return 0;
  return 1;
}

/* Set a breakpoint at the place specified by the "selection" in X.  */

static void
explicit_breakpoint_button ()
{
  int selected_length;
  char *selected_text;

  selected_text = XFetchBytes (screen_display, &selected_length);
  if (selected_length)
    {
      char *line = (char *) xmalloc (selected_length + 6);
      register char *p, *sp, *end;

      strcpy (line, "break ");

      /* Copy selection but exclude "garbage" characters.  */

      p = selected_text;
      end = p + selected_length;
      sp = line + strlen (line);

      while (garbage (*p) && p != end) p++;
      while (!garbage (*p) && p != end)
	*sp++ = *p++;
      *sp = 0;

      execute_command (line, 0);
      free (selected_text);
      free (line);
    }
  print_prompt ();
}


static void
do_command(w, command, call_data)
Widget	w;
char	*command;
caddr_t	call_data;
{
  char *copy = (char *) xmalloc (strlen (command) + 1);
  strcpy (copy, command);
  execute_command (copy, 0);
  xgdb_display_source ();
  print_prompt ();
  free (copy);
}

static void
redisplay_button()
{
	xgdb_display_source();
}

/* Define and display all the buttons.  */

static void
addbutton (parent, name, function, closure)
Widget	parent;
char	*name;
void	(*function) ();
caddr_t	closure;
{
	static	XtCallbackRec	Callback[] = {
		{NULL, (caddr_t)NULL},
		{NULL, (caddr_t)NULL},
	};
	static	Arg	commandArgs[] = {
		{XtNlabel, (XtArgVal)NULL},
		{XtNcallback, (XtArgVal)Callback},
	};

	Callback[0].callback = (XtCallbackProc)function;
        Callback[0].closure = (caddr_t)closure;
	commandArgs[0].value = (XtArgVal)name;
	XtCreateManagedWidget (name, commandWidgetClass, parent,
			       commandArgs, XtNumber(commandArgs));
}

/* Create the button windows and store them in `buttons'.  */

static void
create_buttons (parent)
     Widget parent;
{
  addbutton (parent, "run", do_command, "run");
  addbutton (parent, "quit", do_command, "quit");

  addbutton (parent, "break in", explicit_breakpoint_button, NULL);
  addbutton (parent, "break at", breakpoint_button, 0);
  addbutton (parent, "go until", breakpoint_button, 1);

  addbutton (parent, "print", print_button, 0);
  addbutton (parent, "print*", print_button, 1);

  addbutton (parent, "next", do_command, "next");
  addbutton (parent, "step", do_command, "step");
  addbutton (parent, "cont", do_command, "cont");
  addbutton (parent, "finish", do_command, "finish");
  
  addbutton (parent, "up", do_command, "up");
  addbutton (parent, "down", do_command, "down");

  addbutton (parent, "redisplay", redisplay_button, NULL);
}

/* Create a "label window" that just displays the string LABEL.  */

static Widget
create_label (name, label)
     char *name, *label;
{
  static Arg labelArgs[2];
  
  XtSetArg (labelArgs[0], XtNname, name);

  XtSetArg (labelArgs[1], XtNlabel, label);
  return XtCreateManagedWidget ("label", labelWidgetClass, containing_widget,
			 labelArgs, XtNumber (labelArgs));
}

/* Create a subwindow of PARENT that displays and scrolls the contents
   of file FILENAME.  */

static Widget
create_text_widget (parent, filename)
     Widget parent;
     char *filename;
{
  static Arg fileArgs[3];
  XtTextSource src;
  XtTextSink   sink;
  Widget text_widget;

  text_widget = XtCreateManagedWidget ("disk", textWidgetClass,
				       parent, NULL, 0);

  XtSetArg (fileArgs[0], XtNfile, filename);
  src = XtDiskSourceCreate(parent, fileArgs, 1);
  sink = XtAsciiSinkCreate(parent, NULL, 0);
  
  XtSetArg (fileArgs[0], XtNtextOptions, scrollVertical);
  XtSetArg (fileArgs[1], XtNtextSource, src);
  XtSetArg (fileArgs[2], XtNtextSink, sink);
  XtSetValues (text_widget, fileArgs, XtNumber (fileArgs));
  return text_widget;
}

/* Entry point to create the widgets representing our display.  */

int
xgdb_create_window ()
{
  static Arg frameArgs[]= {
      {XtNwidth, (XtArgVal) 600},
      {XtNheight, (XtArgVal) 700},
  };
  {
    char *dummy1[2];
    int dummy2 = 1;
    
    dummy1[0] = "xgdb";
    dummy1[1] = NULL;
    main_widget = XtInitialize ("xgdb", "XGdb", 0, 0, &dummy2, dummy1);
  }

  screen_display = XtDisplay(main_widget);
  
  /* Create the containing_widget.  */

  containing_widget = XtCreateManagedWidget ("frame", vPanedWidgetClass, main_widget,
				      frameArgs, XtNumber (frameArgs));
  /* Create source file name window and add to containing_widget */
  source_name_widget
    = create_label ("Source File", "No source file yet.");

  /* Create exec file name window and add */
  exec_name_widget = create_label ("Executable", "No executable specified.");

  /* Create window full of buttons.  */
  button_box_widget = XtCreateManagedWidget ("buttonbox", boxWidgetClass,
				      containing_widget, NULL, 0);
  create_buttons (button_box_widget);

  /* Create an empty source-display window and add to containing_widget */
  source_text_widget = create_text_widget (containing_widget, "/dev/null");

  XSync(screen_display, 0);
  XtRealizeWidget(main_widget);
  
#if 0
  default_gc = XCreateGC (screen_display, XtWindow(containing_widget), 0, NULL);
  /* Create icon window.  */
  {
    static Arg iconArgs[2];
    void (*compiler_bug) () = deiconify_button;
    XtSetArg (iconArgs[0], XtNlabel, "(gdb)");
    XtSetArg (iconArgs[1], XtNfunction, compiler_bug);
    icon_window = XtCreateWidget ("Icon", commandWidgetClass, 
				   iconArgs, XtNumber (iconArgs));
    XMoveWindow (screen_display, icon_window, 100, 100);	/* HACK */
    XSetIconWindow (screen_display, containing_widget, icon_window);
  }

  /* Now make the whole thing appear on the display.  */
  {
    Pixmap pm1, pm2;
    XImage image;
    Cursor curse;

    image.width = gdb_width;
    image.height = gdb_height;
    image.xoffset = 0;
    image.format = XYBitmap;
    image.byte_order = LSBFirst;
    image.bitmap_unit = 16;
    image.bitmap_bit_order = LSBFirst;
    image.depth = 1;
    image.bytes_per_line = 2;
    image.bits_per_pixel = 1;

    pm1 = XCreatePixmap (screen_display, DefaultScreen (screen_display),
			 gdb_width, gdb_height, 1);
    pm2 = XCreatePixmap (screen_display, DefaultScreen (screen_display),
			 gdb_width, gdb_height, 1);

    image.data = (char *) gdb_bits;
    XPutImage (screen_display, pm1, default_gc, &image, 0, 0, 0, 0,
	       gdb_width, gdb_height);

    image.data = (char *) gdb_mask_bits;
    XPutImage (screen_display, pm2, default_gc, &image, 0, 0, 0, 0,
	       gdb_width, gdb_height);

    curse = XCreatePixmapCursor (screen_display, pm1, pm2,
				 BlackPixel (screen_display,
					     DefaultScreen (screen_display)),
				 WhitePixel (screen_display,
					     DefaultScreen (screen_display)),
				 gdb_x_hot, gdb_y_hot);

    XFreePixmap (screen_display, pm1);
    XFreePixmap (screen_display, pm2);

    XDefineCursor (screen_display, containing_widget, curse);
    XDefineCursor (screen_display, icon_window, curse);
  }
#endif 0

  XFlush (screen_display);

  return 1;
}

/* xgdb_dispatch -- Loop, dispatching on window events,
   until data is available on FP (which is normally stdin).
   Then return, so the data on FP can be processed.  */

void
xgdb_dispatch (fp)
     FILE *fp;
{
  int inmask = 1 << fileno (fp);
  int xmask = 1 << ConnectionNumber (screen_display);
  int rfds = 0;
  int nfds;
  XEvent ev;
  int pend;
  
  while (! (rfds & inmask))
    {
      pend = XPending (screen_display);
      if (!pend)
	{
	  rfds = inmask | xmask;
	  /* this isn't right for 4.3 but it works 'cuz of 4.2 compatibility */
	  nfds = select (32, &rfds, 0, 0, (struct timeval *) 0);
	}
      if (pend || rfds & xmask)
	{
	  XNextEvent (screen_display, &ev);
	  XtDispatchEvent (&ev);
	}
    }
}  

/* If we use an X window, the GDB command loop is told to call this function

   before reading a command from stdin.
   PROMPT is saved for later use so buttons can print a prompt-string.  */

void
xgdb_window_hook (infile, prompt)
     FILE *infile;
     char *prompt;
{
  prompt_string = prompt;
  xgdb_display_source ();
  xgdb_dispatch (infile);
}

_initialize_xgdb ()
{
  extern void (*window_hook) ();
  extern int inhibit_windows;

  if (getenv ("DISPLAY") && ! inhibit_windows)
    {
      if (xgdb_create_window ())
	window_hook = xgdb_window_hook;

      specify_exec_file_hook (xgdb_display_exec_file);
    }
}


