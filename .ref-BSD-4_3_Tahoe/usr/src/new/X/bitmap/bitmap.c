#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

#ifndef lint
static char *rcsid_bitmap_c = "$Header: bitmap.c,v 10.9 86/11/19 19:13:23 jg Rel $";
#endif

#include <errno.h>
#include <stdio.h>
#include <X/Xlib.h>
#include <sys/types.h>

#include "../cursors/cross.cursor"
#include "../cursors/cross_mask.cursor"
#include "../cursors/ul_angle.cursor"
#include "../cursors/ul_angle_mask.cursor"
#include "../cursors/lr_angle.cursor"
#include "../cursors/lr_angle_mask.cursor"
#include "../cursors/dot.cursor"
#include "../cursors/dot_mask.cursor"

#define TOP_MARGIN 10
#define LEFT_MARGIN 10
#define BOTTOM_MARGIN 10
#define AROUND_RASTER_MARGIN 20
#define GRID_TO_COMMAND_MARGIN 5
#define RIGHT_MARGIN 5

#define MIN_SQUARE_SIZE 3
#define DEFAULT_SQUARE_SIZE 13

#define bit int
#define boolean int
#define TRUE 1
#define FALSE 0
#define OUT_OF_RANGE 10000

#define COPY		0
#define MOVE		1
#define OVERLAY		2

#define min(x,y) ((x < y) ? x : y)
#define max(x,y) ((x < y) ? y : x)

/* error handling stuff */
extern int errno;
extern char *sys_errlist[];

/* global "constants" -- set once at startup time */
/* the first few variables are not static because they are shared
   with dialog.c */
int foreground = BlackPixel;
int background = WhitePixel;
Pixmap backmap;
Pixmap border;
int borderwidth = 3;
int invertplane = 1;
int highlightplane = 1;
int mousepix = BlackPixel;
static int squares_wide = OUT_OF_RANGE;
static int squares_high = OUT_OF_RANGE;
static short *raster;
static int raster_length; /* how many shorts in the raster[] array */
static Window outer_window, grid_window;
static Window raster_window, raster_invert_window;
static Font font;
static FontInfo fontInfo;
static Cursor cross, upper_left, lower_right, dot;
static char *filename = NULL; /* name of input file */
static char *backup_filename;
static char *stripped_name; 
  /* file name without directory path or extension */
static char *progname; /* name this program was invoked by */
static Pattern DottedPattern = XMakePattern (
    1 /* pattern */,
    2 /* length */,
    1); /* multiplier */
static Pattern InverseDottedPattern = XMakePattern (
    2 /* pattern */,
    2 /* length */,
    1); /* multiplier */

/* command-button data */
#define N_COMMANDS 	16
static struct command_data {
  Window window;
  char *name;
  int name_length;
  int x_offset;  /* so text is centered within command box */
  boolean inverted;
  int (*proc)(); 
     /* function to invoke when command button is "pressed" */
     /* actually no return value, but compiler doesn't like "void" here */
  int data;  /* arbitrary instance data to call procedure back with */
  } commands [N_COMMANDS];

/* global variables */
/* layout-related variables */
static int square_size;  /* length of square's side, in pixels */
static OpaqueFrame frames[N_COMMANDS+3], outer_frame;
     /* frames[0] throgh frames[N_COMMANDS-1] are the command windows;
        frames[N_COMMANDS] is the raster;
    	frames[N_COMMANDS+1] is the inverted raster;
	frames[N_COMMANDS+2] is the grid */

  /* location of x'd-through squares, if any */
static int x1_square_exed_through = OUT_OF_RANGE;
static int y1_square_exed_through = OUT_OF_RANGE;
static int x2_square_exed_through = OUT_OF_RANGE;
static int y2_square_exed_through = OUT_OF_RANGE;

  /* location of "plus'd through" squares, if any */
static int x1_square_plus_through = OUT_OF_RANGE;
static int y1_square_plus_through = OUT_OF_RANGE;
static int x2_square_plus_through = OUT_OF_RANGE;
static int y2_square_plus_through = OUT_OF_RANGE;

  /* location of hot spot, if any */
static int x_hot_spot = OUT_OF_RANGE;
static int y_hot_spot = OUT_OF_RANGE;

static boolean changed = FALSE;
   /* has user changed bitmap since starting program or last write? */

static enum RepaintGridType {e_AgainstBackground, e_AgainstForeground, e_Invert};

extern char *malloc();

main (argc, argv)
  int argc;
  char **argv;
  {
  SetUp (argc, argv);
  while (TRUE) {
    XEvent event;
    XNextEvent(&event);
    ProcessEvent(&event);
    }
  }   /* end of main procedure */


SetUp (argc, argv)
  int argc;
  char **argv;
  {
  char *StripName(), *BackupName(), *index();
  char *option;
  FILE *file;
  char *geometry = NULL, *host = NULL, *dimensions = NULL;
  int i;

  progname = argv[0];
  setlinebuf (stderr);

  /* Parse command line */
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '=')
	geometry = argv[i];

    else if (index (argv[i], ':') != NULL)
    	host = argv[i];
	
    else if (filename == NULL)
    	filename = argv[i];

    else 
    	dimensions = argv[i];
    }

  if (filename == NULL) {
    fprintf (stderr, "%s: no file name specified\n", progname);
    exit (1);
    }
  
  stripped_name = StripName (filename);
  backup_filename = BackupName (filename);

  file = fopen (filename, "r");
  if (!file && (errno != ENOENT)) {
    fprintf (stderr, "%s: could not open file '%s' for reading -- %s\n",
      progname, filename, sys_errlist[errno]);
    exit (1);
    }
      
  if (file)
    DimensionsFromFile(file);
  else {
    if (dimensions != NULL) {
      if (sscanf (dimensions, "%dx%d", &squares_wide, &squares_high) != 2) {
	fprintf (stderr, "%s: invalid dimensions '%s'\n",
	  progname, dimensions);
	exit (1);
	}
      if ((squares_wide <=0) || (squares_high <=0)) {
        fprintf (stderr, "%s: dimensions must be positive\n", progname);
        exit (1);
        }
      }
    else   /* dimensions not supplied on command line */
      squares_wide = squares_high = 16;
    }

  raster_length = ((squares_wide+15)/16)*squares_high;
  raster = (short *) malloc (raster_length*sizeof(short));
  
  if (file) {
    InitialValuesFromFile(file);
    fclose (file);
    }
  else {
    /* set raster to all 0's (background color) */
    register int i;
    for (i=0;i<raster_length;i++)
      raster[i] = 0;
    }

  if (!XOpenDisplay(host)) {
	fprintf(stderr, "%s: Can't open display '%s'\n",
		argv[0], XDisplayName(host));
	exit (1);
    }

  backmap = WhitePixmap;
  border = BlackPixmap;
  if ((option = XGetDefault(progname, "BorderWidth")) != NULL)
    borderwidth = atoi(option);
  if ((option = XGetDefault(progname, "BodyFont")) == NULL)
    option = "vtsingle";
  font = XGetFont (option);
  XQueryFont (font, &fontInfo);
  if (DisplayCells() > 2) {
    char *fore_color = XGetDefault(progname, "Foreground");
    char *back_color = XGetDefault(progname, "Background");
    char *high_color = XGetDefault(progname, "Highlight");
    char *brdr_color = XGetDefault(progname, "Border");
    char *mous_color = XGetDefault(progname, "Mouse");
    Color fdef, bdef, hdef;
    if (fore_color && XParseColor(fore_color, &fdef) &&
      back_color && XParseColor(back_color, &bdef) &&
      (high_color == NULL || XParseColor(high_color, &hdef)) &&
      XGetColorCells(0, 1, high_color ? 2 : 1, &invertplane, &bdef.pixel)) {
      background = bdef.pixel;
      backmap = XMakeTile(background);
      if (high_color) {
	hdef.pixel = bdef.pixel | invertplane;
	XStoreColor(&hdef);
	highlightplane = 1 << (ffs(invertplane) - 1);
	hdef.pixel = bdef.pixel | highlightplane;
	XStoreColor(&hdef);
	invertplane ^= highlightplane;
      } else
	highlightplane = invertplane;
      XStoreColor(&bdef);
      foreground = background | invertplane;
      fdef.pixel = foreground;
      XStoreColor(&fdef);
    }
    if (brdr_color && XParseColor(brdr_color, &bdef) &&
	XGetHardwareColor(&bdef))
      border = XMakeTile(bdef.pixel);
    if (mous_color && XParseColor(mous_color, &fdef) &&
	XGetHardwareColor(&fdef))
      mousepix = fdef.pixel;
  }

  {
  int right_side_bottom, right_side_width;
  int minwidth, minheight;
  int default_width, default_height, default_x, default_y;
  int display_width = DisplayWidth();
  int display_height = DisplayHeight();
  char default_geometry[20];

  LayoutStage1();
  right_side_bottom = frames[N_COMMANDS+1].y + frames[N_COMMANDS+1].height
    + 2 /* borders */ + AROUND_RASTER_MARGIN;
  right_side_width = frames[0].width + 2 /* borders */
    + GRID_TO_COMMAND_MARGIN + RIGHT_MARGIN;
  OuterWindowDims (MIN_SQUARE_SIZE, right_side_width, right_side_bottom,
    &minwidth, &minheight);
  OuterWindowDims (DEFAULT_SQUARE_SIZE, right_side_width, right_side_bottom,
    &default_width, &default_height);
  default_x = min (200, display_width - default_width - 2*borderwidth);
  default_y = min (200, display_height - default_height - 2*borderwidth);
  sprintf (default_geometry, "=%dx%d+%d+%d", default_width,
    default_height, default_x, default_y);
  outer_frame.bdrwidth = borderwidth;
  outer_frame.border = border;
  outer_frame.background = backmap;
  outer_window = XCreate ("Bitmap Editor", progname, geometry,
     default_geometry, &outer_frame, minwidth, minheight);
  LayoutStage2 ();
  }
  
  upper_left = XCreateCursor (ul_angle_width, ul_angle_height, 
    ul_angle_bits, ul_angle_mask_bits, ul_angle_x_hot, ul_angle_y_hot,
    mousepix, background, GXcopy);
  lower_right = XCreateCursor (lr_angle_width, lr_angle_height,
    lr_angle_bits, lr_angle_mask_bits,
    lr_angle_x_hot, lr_angle_y_hot, mousepix, background, GXcopy);
  cross = XCreateCursor (cross_width, cross_height, cross_bits, 
    cross_mask_bits, cross_x_hot, cross_y_hot, mousepix, background,
    GXcopy);
  dot = XCreateCursor (dot_width, dot_height, dot_bits,
    dot_mask_bits, dot_x_hot, dot_y_hot, mousepix, background,
    GXcopy);
  XDefineCursor (outer_window, cross);

  XExpandEvents();  /* do NOT collapse adjacent MouseMotion events */
  XSelectInput (outer_window, ExposeWindow);  /* to detect size changes */
  XMapWindow (outer_window);
  XMapSubwindows (outer_window);
  }    /* end of Set_Up procedure */


/* Unfortunately, the current implementation of X (version 10) does not
   handle resize event notification very well.  When the outer window
   is resized, X sends ExposeWindow events for each subwindow BEFORE sending
   an ExposeWindow for the outer window.

   If I handled the events in the order sent, I would repaint each
   subwindow, only then to discover that the outer window had changed
   size...at which time I would unmap and rearrange the subwindows,
   causing ANOTHER set of exposure events on the subwindows.  This would
   not just be inefficient, it would also look ugly on the screen.

   To work around this misfeature, I do not process ExposeWindow events
   immediately upon receipt, but instead go into a recursion (HandleExposure)
   until I either run out of events or get a non-ExposeWindow event.  If
   the list of ExposeWindow events ends with a resize, I discard all of the
   earlier ExposeWindow events.  Otherwise, I unwind out of the recursion,
   processing each exposure event in the opposite of the order received,
   eventually returning to the normal event notification loop.

   This code is admittedly convoluted, and will hopefully go away in a
   future version of X.  */

#define PROCESS_IT 0
#define DISCARD_IT 1

ProcessEvent (event)
  register XEvent *event;
  {
  register Window w = event->window;
  register int i;
  if (event->type == ExposeWindow) {
    int status;
    /* make sure that I get all the exposure events that have been sent */
    XSync(0); 
    status = HandleExposure (event);
    if (status == DISCARD_IT)
    	return;
    }
  ProcessEventReally (event);
  }

ProcessEventReally (event)
  register XEvent *event;
  {
  register Window w = event->window;
  register int i;
  if (w == grid_window)
    ProcessGridWindowEvent (event);
  else if (w == outer_window)
    ProcessOuterWindowEvent (event);
  else if (w == raster_window)
    RepaintRaster();
  else if (w == raster_invert_window)
    RepaintRasterInverted();
  else for (i=0;i<N_COMMANDS;i++)
    if (w == commands[i].window)
      ProcessCommandButtonEvent (&commands[i], event);
  }  /* end of ProcessInput procedure */


int HandleExposure (event)
  XExposeWindowEvent *event;
  {
  int status;
  XEvent next_event;
  
  if (QLength() == 0)
    /* there are no more events, so this can't be a resize */
    return (PROCESS_IT);
    
  XNextEvent (&next_event);
  if (next_event.type != ExposeWindow) {
    /* the list of exposures ended, so this can't be a resize */
    XPutBackEvent (&next_event);
    return (PROCESS_IT);
    }
    
  if (next_event.window == outer_window) {
    if ((outer_frame.height == ((XExposeWindowEvent *)&next_event)->height)
      && (outer_frame.width == ((XExposeWindowEvent *)&next_event)->width))
        /* the list of exposures ended with a non-resize */
        status = PROCESS_IT;
    else
    	/* this IS a resize */
        status = DISCARD_IT;
    /* Handle the outer window exposure, whether or not it's a resize */
    ProcessEventReally (&next_event);
    return (status);
    }

  if ((status = HandleExposure (&next_event)) == PROCESS_IT)
    ProcessEventReally (&next_event);
  return (status);
  }
  
ProcessGridWindowEvent (event)
  XEvent *event;
  {
  int x_square, y_square;
  static int x_square_prev, y_square_prev;
  static boolean raster_outdated;
  switch (event->type) {

    case ExposeWindow:
      RepaintGridLines(e_AgainstBackground);
      RefillGrid(FALSE);
      if (x1_square_exed_through != OUT_OF_RANGE)
      	ExThroughRectangle (x1_square_exed_through, y1_square_exed_through,
	  x2_square_exed_through, y2_square_exed_through);
      if (x1_square_plus_through != OUT_OF_RANGE)
        PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
	  x2_square_plus_through, y2_square_plus_through);
      if (x_hot_spot != OUT_OF_RANGE)
      	HighlightHotSpot();
      break;

    case ExposeRegion: {
#define this_event ((XExposeRegionEvent *)event)
      int x1 = this_event->x;
      int y1 = this_event->y;
      int x2 = x1 + this_event->width;
      int y2 = y1 + this_event->height;
#undef this_event
      x1 /= square_size;
      x2 /= square_size;
      y1 /= square_size;
      y2 /= square_size;
      if (x2 >= squares_wide)
        x2 = squares_wide - 1;  /* sanity check */
      if (y2 >= squares_high)
        y2 = squares_high - 1;  /* sanity check */
      RepaintGridLinesPartially(x1,y1,x2+1,y2+1,e_AgainstBackground,TRUE);
      RefillGridPartially (x1,y1,x2,y2,FALSE);
      if (x1_square_exed_through != OUT_OF_RANGE)
        ExThroughRectangle (
	  max (x1, x1_square_exed_through),
	  max (y1, y1_square_exed_through),
	  min (x2, x2_square_exed_through),
	  min (y2, y2_square_exed_through));
      if (x1_square_plus_through != OUT_OF_RANGE)
        PlusThroughRectangle (
	  max (x1, x1_square_plus_through),
	  max (y1, y1_square_plus_through),
	  min (x2, x2_square_plus_through),
	  min (y2, y2_square_plus_through));
      if (x_hot_spot >= x1 && x_hot_spot <= x2
      	&& y_hot_spot >= y1 && y_hot_spot <= y2)
	    HighlightHotSpot();
      break;
      }

    case ButtonPressed:
      if (WhatSquare (event, &x_square, &y_square))
        return;  /* mouse outside grid; really shouldn't happen, but... */
      switch (((XButtonPressedEvent *)event)->detail & ValueMask) {
	case LeftButton:
	  PaintSquare (x_square, y_square, foreground);
	  if (x_square == x_hot_spot && y_square == y_hot_spot)
	    HighlightHotSpot();
	  SetRasterBit (raster, x_square, y_square, 1);
	  break;
	case MiddleButton:
	  InvertSquare (x_square, y_square);
	  InvertRasterBit (raster, x_square, y_square);
	  break;
	case RightButton:
	  PaintSquare (x_square, y_square, background);
	  if (x_square == x_hot_spot && y_square == y_hot_spot)
	    HighlightHotSpot();
	  SetRasterBit (raster, x_square, y_square, 0);
	  break;
	}
      RepaintRaster();
      RepaintRasterInverted();
      x_square_prev = x_square;
      y_square_prev = y_square;
      raster_outdated = FALSE;
      changed = TRUE;
      break;
    
    case MouseMoved:
      if (WhatSquare (event, &x_square, &y_square))
        return;  /* mouse outside grid; really shouldn't happen, but... */
      if ((x_square != x_square_prev) || (y_square != y_square_prev))
       	  switch (((XMouseMovedEvent *)event)->detail) {
	    case LeftMask:
	      PaintSquare (x_square, y_square, foreground);
	      if (x_square == x_hot_spot && y_square == y_hot_spot)
	      	HighlightHotSpot();
	      SetRasterBit (raster, x_square, y_square, 1);
	      changed = raster_outdated = TRUE;
	      break;
	    case MiddleMask:
	      InvertSquare (x_square, y_square);
	      InvertRasterBit (raster, x_square, y_square);
	      changed = raster_outdated = TRUE;
	      break;
	    case RightMask:
	      PaintSquare (x_square, y_square, background);
	      if (x_square == x_hot_spot && y_square == y_hot_spot)
	      	HighlightHotSpot();
	      SetRasterBit (raster, x_square, y_square, 0);
	      changed = raster_outdated = TRUE;
	      break;
	    default:
	      break;  /* ignore events with multiple buttons down */
	    }
      if (raster_outdated && !MouseMovedEventQueued()) {
	RepaintRaster();
	RepaintRasterInverted();
	raster_outdated = FALSE;
	}
      x_square_prev = x_square;
      y_square_prev = y_square;
      break;
  
    }
  }  /* end of ProcessGridWindowEvent procedure */

boolean MouseMovedEventQueued () {
  XEvent event;
  if (XPending() == 0) return (FALSE);
  XPeekEvent (&event);
  return (event.type == MouseMoved);
  }


ProcessOuterWindowEvent (event)
  XEvent *event;
  {
  if (event->type != ExposeWindow)
    return;
  if ((outer_frame.height == ((XExposeWindowEvent *)event)->height)
     && (outer_frame.width == ((XExposeWindowEvent *)event)->width))
     /* if this isn't a resize, there's nothing to do here. */
     return;

  /* the outer window's size has changed.  Must rearrange subwindows. */
  outer_frame.height = ((XExposeWindowEvent *)event)->height;
  outer_frame.width = ((XExposeWindowEvent *)event)->width;
  XDestroySubwindows (outer_window);
  LayoutStage2 ();
  XMapSubwindows (outer_window);
  }
  
ProcessCommandButtonEvent (command, event)
  struct command_data *command;
  XEvent *event;
  {
  static struct command_data *button_down_command;
  
  switch (event->type) {

    case ExposeWindow:
      if (command->inverted)
        XClear (command->window);
      XTextMask (
        command->window,    /* w */
	command->x_offset,  /* x */
	0,  	    	    /* y */
	command->name,      /* string */
	command->name_length,    /* length */
	font,	    	    /* font */
	foreground); 	    /* source pixel */
      if (command->inverted)
      	InvertCommandWindow (command);
      break;

    case ButtonPressed:
      if (button_down_command != NULL)
        break;  /* must be a second button push--ignore */
      button_down_command = command;
      InvertCommandWindow (command);
      command->inverted = TRUE;
      break;

    case LeaveWindow:
      if (command == button_down_command) {
	InvertCommandWindow (command);
	command->inverted = FALSE;
	button_down_command = NULL;
	}
      break;

    case ButtonReleased:
      if (command == button_down_command) {
	(*command->proc)(command->data);
	button_down_command = NULL;
	InvertCommandWindow (command);
	command->inverted = FALSE;
	}
      break;
      
    }
  }


InvertCommandWindow (command)
  struct command_data *command;
  {
  XPixFill (
    command->window,
    0,	    /* x */
    0, 	    /* y */
    400,    /* width = "infinity " */
    fontInfo.height,	/* height */
    1,	    /* pixel */
    NULL,   /* clipmask bitmap */
    GXinvert,	/* function */
    invertplane);    	/* planes */
  }  /* end of InvertCommandWindow procedure */

	  
/* WhatSquare returns TRUE if mouse is outside grid, FALSE if inside.
   If it returns FALSE, it assigns to *x_square and *y_square. */

boolean WhatSquare (event, x_square, y_square)
  register XEvent *event;
  register int *x_square, *y_square; /*RETURN*/
  {
  int x = ((XKeyPressedEvent *)event)->x;
  int y = ((XKeyPressedEvent *)event)->y;
  if ((x < 0) || (y < 0))
    return (TRUE);
  *x_square = x/square_size;
  *y_square = y/square_size;
  return ((*x_square >= squares_wide) || (*y_square >= squares_high));
  }  /* end of WhatSquare procedure */


RepaintGridLines(how)
  enum RepaintGridType how;
  {
  RepaintGridLinesPartially (0, 0, squares_wide, squares_high, how, TRUE);
  }

RepaintGridLinesPartially (x1, y1, x2, y2, how, include_boundaries)
  int x1, y1, x2, y2;
  enum RepaintGridType how;
  boolean include_boundaries;
  {
  register int i;
  Vertex v[2];
  int pixel, func, planes;
  Pattern pattern;
  switch (how) {
    case e_AgainstBackground:
    	pixel = foreground;
	pattern = DottedPattern;
	func = GXcopy;
	planes = AllPlanes;
	break;
    case e_AgainstForeground:
    	pixel = background;
	pattern = InverseDottedPattern;
	func = GXcopy;
	planes = AllPlanes;
	break;
    case e_Invert:
    	pixel = 1;
	pattern = SolidLine;
	func = GXinvert;
	planes = invertplane;
	break;
    }
    
  /* draw vertical grid lines */
  v[0].flags = v[1].flags = 0;
  v[0].y = y1*square_size;
  v[0].y += (v[0].y & 1);  /* make sure pattern is aligned on even bit boundary */
  v[1].y = y2*square_size;
  if (!include_boundaries) {x1++;x2--;}
  v[0].x = v[1].x = x1*square_size;
  for (i=x1;i<=x2; i++) {
     XDrawDashed (
     	grid_window,
	v,  /* vertices */
	2,  /* vertex count */
	1,  /* width */
	1,  /* height */
	pixel,
	pattern,
	func,
	planes);    	/* planes */
     v[0].x = (v[1].x += square_size);
     }
  if (!include_boundaries) {x1--;x2++;}

  /* draw horizontal grid lines */
  v[0].flags = v[1].flags = 0;
  v[0].x = x1*square_size;
  v[0].x += (v[0].x & 1);  /* make sure pattern is aligned on even bit boundary */
  v[1].x = x2*square_size;
  if (!include_boundaries) {y1++;y2--;}
  v[0].y = v[1].y = y1*square_size;
  for (i=y1;i<=y2;i++) {
    XDrawDashed (
     	grid_window,
	v,  /* vertices */
	2,  /* vertex count */
	1,  /* width */
	1,  /* height */
	pixel,
	pattern,
	func,
	planes);	/* planes */
     v[0].y = (v[1].y += square_size);
     }
  }  /* end of RepaintGridLinesPartially procedure */


RefillGrid (paint_background)
  boolean paint_background;
  {
  RefillGridPartially (0, 0, squares_wide-1, squares_high-1, paint_background);
  }


RefillGridPartially(x1, y1, x2, y2, paint_background)
  register int x1, y1, x2, y2;
  boolean paint_background;
  {
  register i, j;
  for (i=x1; i<=x2; i++) {
    for (j=y1; j<=y2; j++) {
      bit b = GetRasterBit (raster, i, j);
      if (b || paint_background)
      	PaintSquare (i, j, (b ? foreground : background));
      }
    }
  }  /* end of RefillGridPartially procedure */


PaintSquare(x, y, pixel)
  int x, y;
  int pixel;  /* display function */  
  {
  XPixSet (
    grid_window,
    x*square_size + 1,	/* x */
    y*square_size + 1, 	/* y */
    square_size - 1, 	/* width */
    square_size - 1,	/* height */
    pixel);    	    	    	/* pixel */
  }  /* end of PaintSquare procedure */

InvertSquare(x, y)
  int x, y;
  {
  XPixFill (
    grid_window,
    x*square_size + 1,	/* x */
    y*square_size + 1, 	/* y */
    square_size - 1, 	/* width */
    square_size - 1,	/* height */
    1,	    	    	    	/* pixel */
    NULL,   	    	    	/* clipmask */
    GXinvert,  	    	    	/* function */
    invertplane);    	    	/* planes */
  }

bit GetRasterBit (raster, x, y)
  short *raster;
  register int x;
  int y;
  {
  register short *word = raster + x/16 + y*((squares_wide+15)/16);
  return ( (*word & (1 << (x % 16))) ? 1 : 0);
  }  /* end of GetRasterBit procedure */


SetRasterBit (raster, x, y, new)
  short *raster;
  register int x;
  int y;
  bit new;
  {
  register short *word = raster + x/16 + y*((squares_wide+15)/16);
  x %= 16;
  *word = (new << x) | (*word & ~(1 << x));
  }  /* end of SetRasterBit procedure */


InvertRasterBit (raster, x, y)
  short *raster;
  register int x;
  int y;
  {
  register short *word = raster + x/16 + y*((squares_wide+15)/16);
  *word ^= (1 << (x % 16));
  }  /* end of InvertRasterBit procedure */


RepaintRaster() {
  XBitmapBitsPut (
    raster_window,
    3,	/* x */
    3,	/* y */
    squares_wide,   /* width */
    squares_high,   /* height */
    raster,  	    /* data */
    foreground,	    /* foreground */
    background,	    /* background */
    0,	    	    /* mask */
    GXcopy,	    /* func */
    AllPlanes);	    /* planes */
  }  /* end of RepaintRaster procedure */


RepaintRasterInverted () {
  XBitmapBitsPut (
    raster_invert_window,
    3,	/* x */
    3,	/* y */
    squares_wide,   /* width */
    squares_high,   /* height */
    raster,  	    /* data */
    background,	    /* foreground */
    foreground,	    /* background */
    0,	    	    /* mask */
    GXcopy, 	    /* func */
    AllPlanes);	    /* planes */
  } /* end of RepaintRasterInverted procedure */


WriteOutputToFile (file)
  FILE *file;
  {
  register int i;
  fprintf (file, "#define %s_width %d\n", stripped_name, squares_wide);
  fprintf (file, "#define %s_height %d\n", stripped_name, squares_high);
  if (x_hot_spot != OUT_OF_RANGE)
    fprintf (file, "#define %s_x_hot %d\n", stripped_name, x_hot_spot);
  if (y_hot_spot != OUT_OF_RANGE)
    fprintf (file, "#define %s_y_hot %d\n", stripped_name, y_hot_spot);
  fprintf (file, "static short %s_bits[] = {\n   0x%04x",
    stripped_name, (u_short) raster[0]);

  for (i=1;i<raster_length;i++) {
    fprintf (file, ",");
    fprintf (file, (i % 4) ? " " : "\n   ");
    fprintf (file, "0x%04x", (u_short) raster[i]);
    }
  fprintf (file, "};\n");
  }  /* end of WriteOutputToFile procedure */


DimensionsFromFile(file) 
  FILE *file;
  {
  char variable[81];
  int value;
  while (fscanf (file, "#define %80s %d\n", variable, &value) == 2) {
    if (StringEndsWith (variable, "width"))
	squares_wide = value;
    else if (StringEndsWith (variable, "height"))
    	squares_high = value;
    else if (StringEndsWith (variable, "x_hot"))
    	x_hot_spot = value;
    else if (StringEndsWith (variable, "y_hot"))
    	y_hot_spot = value;
    else fprintf (stderr, "Unrecognized variable '%s' in file '%s'\n",
    	variable, filename);
    }

  if (squares_wide <= 0 || squares_wide == OUT_OF_RANGE) {
    fprintf (stderr,
      "%s: file '%s' does not have a valid width dimension\n",
      progname, filename);
    exit(1);
    }
    
  if (squares_high <= 0 || squares_high == OUT_OF_RANGE) {
    fprintf (stderr,
      "%s: file '%s' does not have a valid height dimension\n",
      progname, filename);
    exit(1);
    }	
  }  /* end of DimensionsFromFile procedure */


InitialValuesFromFile(file)
  FILE *file;
  {
  register int i, status;
  char s[81];

  status = fscanf (file, "static short %80s = { 0x%4hx", s, &raster[0]);
  if ((status != 2) || !StringEndsWith (s, "bits[]")) {
    fprintf (stderr,
      "%s: file '%s' has an invalid 1st array element\n",
      progname, filename);
    exit (1);
    }
  for (i=1;i<raster_length;i++) {
    status = fscanf (file, ", 0x%4hx", &raster[i]);
    if (status != 1) {
      fprintf (stderr,
        "%s: file '%s' has an invalid %dth array element\n",
        progname, filename, i+1);
      exit (1);
      }
    }

  }  /* end of InitialValuesFromFile procedure */


char *StripName(name)
  char *name;
  {
  char *rindex(), *index();
  char *begin = rindex (name, '/');
  char *end, *result;
  int length;
  begin = (begin ? begin+1 : name);
  end = index (begin, '.');
  length = (end ? (end - begin) : strlen (begin));
  result = (char *) malloc (length + 1);
  strncpy (result, begin, length);
  result [length] = '\0';
  return (result);
  }


char *BackupName(name)
  char *name;
  {
  int name_length = strlen (name);
  char *result = (char *) malloc (name_length+2);
  strncpy (result, name, name_length);
  result [name_length] = '~';
  result [name_length+1] = '\0';
  return (result);
  }

char *TmpFileName(name)
  char *name;
  {
  {
  char *rindex();
  char *begin = rindex (name, '/');
  if (begin)
    name = begin+1;
  }
  {
  char *tmp = "/tmp/";
  int name_length = strlen (name);
  int tmp_length = strlen (tmp);
  int result_length = name_length + tmp_length;
  char *result = (char *) malloc (result_length + 1);
  strncpy (result, tmp, tmp_length);
  strncpy (result+tmp_length, name, name_length);
  result [result_length] = '\0';
  return (result);
  }
  }

/* StringEndsWith returns TRUE if "s" ends with "suffix", else returns FALSE */
boolean StringEndsWith (s, suffix)
  char *s, *suffix;
  {
  int s_len = strlen (s);
  int suffix_len = strlen (suffix);
  return (strcmp (s + s_len - suffix_len, suffix) == 0);
  }

/* LayoutStage1 determines the size and y-position of all commmand
   windows and both raster windows.  It fills in everything in the
   commands[] array except the "window" field, and fills in everything in
   the frames[] array except for the "self" (window) and "x" fields. It
   returns in *width and *bottom the dimensions of the command area to be
   created.  
   This routine is called only once, at startup time.
   Everything done at this stage stays the same even if the user later
   reshapes the window. */

LayoutStage1 ()
  {
  int widths [N_COMMANDS];
  int maxwidth = 0;
  int ypos = TOP_MARGIN;
  register int i;
  register OpaqueFrame *frame;
  int ClearOrSetAll(), InvertAll(),
      ClearOrSetArea(), InvertArea(), CopyOrMoveArea(),
      Line(), Circle(), FilledCircle(),
      SetHotSpot(), ClearHotSpot(),
      WriteOutput(), Quit();

  commands[0].name = "Clear All";
  commands[0].proc = ClearOrSetAll;
  commands[0].data = 0;
  
  commands[1].name = "Set All";
  commands[1].proc = ClearOrSetAll;
  commands[1].data = 1;

  commands[2].name = "Invert All";
  commands[2].proc = InvertAll;

  commands[3].name = "Clear Area";
  commands[3].proc = ClearOrSetArea;
  commands[3].data = 0;

  commands[4].name = "Set Area";
  commands[4].proc = ClearOrSetArea;
  commands[4].data = 1;

  commands[5].name = "Invert Area";
  commands[5].proc = InvertArea;

  commands[6].name = "Copy Area";
  commands[6].proc = CopyOrMoveArea;
  commands[6].data = COPY;

  commands[7].name = "Move Area";
  commands[7].proc = CopyOrMoveArea;
  commands[7].data = MOVE;

  commands[8].name = "Overlay Area";
  commands[8].proc = CopyOrMoveArea;
  commands[8].data = OVERLAY;

  commands[9].name = "Line";
  commands[9].proc = Line;

  commands[10].name = "Circle";
  commands[10].proc = Circle;
  commands[10].data = 0;

  commands[11].name = "Filled Circle";
  commands[11].proc = Circle;
  commands[11].data = 1;

  commands[12].name = "Set HotSpot";
  commands[12].proc = SetHotSpot;

  commands[13].name = "Clear HotSpot";
  commands[13].proc = ClearHotSpot;

  commands[14].name = "Write Output";
  commands[14].proc = WriteOutput;

  commands[15].name = "Quit";
  commands[15].proc = Quit;


  for (i=0;i<N_COMMANDS;i++) {
    widths[i] = XQueryWidth (commands[i].name, font);
    if (maxwidth < widths[i])
      maxwidth = widths[i];
    }

  maxwidth += 4; /* so even widest command has a little space around it */

  for (i=0;i<N_COMMANDS;i++) {
    register struct command_data *command = &commands[i];
    frame = &frames[i];
    command->name_length = strlen (command->name);
    command->x_offset = (maxwidth - widths[i])/2;
    frame->y = ypos;
    frame->width = maxwidth;
    frame->height = fontInfo.height;
    frame->bdrwidth = 1;
    frame->border = border;
    frame->background = backmap;
    ypos += fontInfo.height + 5;
    if (i==2 || i == 5 || i == 8 || i == 11 || i == 13)
      ypos += fontInfo.height + 5;
      /* for gaps between groups;  pretty random! */

    }
  
  /* set up raster window */
  frame = &frame[N_COMMANDS];
  frame = &frames[i];
  frame->y = (ypos += AROUND_RASTER_MARGIN);
  frame->width = squares_wide + 6;
  frame->height = squares_high + 6;
  frame->bdrwidth = 1;
  frame->border = border;
  frame->background = backmap;
  
  /* raster invert window is the same, except for y position */
  *(frame+1) = *frame;
  (++frame)->y += squares_high + 8 + AROUND_RASTER_MARGIN;

  }

/* LayoutStage2 is called both at startup time and whenever the user
   resizes the outer window.  It figures out what the new grid square size
   should be, determines the size and position of all subwindows, then
   creates (but does not map) the subwindows. */

LayoutStage2 ()
  {
  int x_room = outer_frame.width - 1
     - LEFT_MARGIN - frames[0].width - GRID_TO_COMMAND_MARGIN - RIGHT_MARGIN;
  int y_room = outer_frame.height - 1
     - TOP_MARGIN - BOTTOM_MARGIN;
  int i;
  int command_x_offset;
  OpaqueFrame *grid_frame = &frames[N_COMMANDS+2];
  
  x_room /= squares_wide;
  y_room /= squares_high;
  square_size = min (x_room, y_room);
  
  /* fill in the grid window's frame */
  grid_frame->x = LEFT_MARGIN;
  grid_frame->y = TOP_MARGIN;
  grid_frame->width = (squares_wide * square_size) + 1;
  grid_frame->height = (squares_high * square_size) + 1;
  grid_frame->bdrwidth = 0;
  grid_frame->border = NULL;
  grid_frame->background = backmap;

  /* fill in x offsets for command window frames */
  command_x_offset = grid_frame->x + grid_frame->width
    + GRID_TO_COMMAND_MARGIN;
  for (i=0;i<N_COMMANDS;i++)
    frames[i].x = command_x_offset;

  /* fill in x offsets for raster and raster-inverted window frames */
  frames[N_COMMANDS].x = frames[N_COMMANDS+1].x =
    grid_frame->x + grid_frame->width + AROUND_RASTER_MARGIN;

  /* create all the subwindows */
  XCreateWindows (outer_window, frames, N_COMMANDS+3);

  /* stow away all the resulting window id's, and select input */
  for (i=0;i<N_COMMANDS;i++)
    XSelectInput (commands[i].window = frames[i].self, 
    	ButtonPressed | ButtonReleased | LeaveWindow | ExposeWindow);

  XSelectInput (raster_window = frames[N_COMMANDS].self, ExposeWindow);
  XSelectInput (raster_invert_window = frames[N_COMMANDS+1].self,
    ExposeWindow);
  XSelectInput (grid_window = grid_frame->self,
    RightDownMotion | MiddleDownMotion | LeftDownMotion
    | ExposeRegion | ButtonPressed | ButtonReleased);
    /* ButtonReleased is selected for AskUserForArea's benefit */

  }

/* OuterWindowDims determines the minimum size for the outer window,
   based on three constraints:  the grid square size, the width of
   the command/raster area, and the minimum height of the
   command/raster area ("right side" of the window).  It is called
   at startup time. */

OuterWindowDims (square_size, right_side_width,
  right_side_bottom, width, height)
  int square_size, right_side_width, right_side_bottom;
  int *width, *height; /* RETURN */
  {
  *width = LEFT_MARGIN + squares_wide*square_size + 1 + right_side_width;
  *height = TOP_MARGIN + squares_high*square_size + 1 + BOTTOM_MARGIN;
  if (*height < right_side_bottom)
    *height = right_side_bottom;
  }


ClearOrSetAll(b)
  bit b;  /* 0 for clear, 1 for set */
  {
  register int i;
  register int new = (b ? ~0: 0);
  for (i=0;i<raster_length;i++)
    raster[i] = new;
  changed = TRUE;
  XPixSet (
    grid_window,    /* window */
    0,	    /* x */
    0,	    /* y */
    squares_wide*square_size+1,	/* width */
    squares_high*square_size+1,	/* height */
    b ? foreground : background);
  RepaintGridLines (b ? e_AgainstForeground : e_AgainstBackground);
  RepaintRaster();
  RepaintRasterInverted();
  if (x_hot_spot != OUT_OF_RANGE)
    HighlightHotSpot();
  }  /* end of ClearAll procedure */


ClearOrSetArea(b)
  bit b;  /* 0 for clear, 1 for set */
  {
  int x1, y1, x2, y2;
  register int x, y;
  if (AskUserForArea (&x1, &y1, &x2, &y2))
    return;
  for (x=x1;x<=x2;x++)
    for (y=y1;y<=y2;y++)
      SetRasterBit (raster, x, y, b);
  XPixSet (
    grid_window,    	/* window */
    x1*square_size+1,  /* x */
    y1*square_size+1,  /* y */
    (x2-x1+1)*square_size-1,  /* width */
    (y2-y1+1)*square_size-1,  /* height */
    b ? foreground : background);
  RepaintGridLinesPartially (x1, y1, x2+1, y2+1, b ? e_AgainstForeground : e_AgainstBackground, FALSE);
  if (x_hot_spot >= x1 && x_hot_spot <= x2
    && y_hot_spot >= y1 && y_hot_spot <= y2)
    	HighlightHotSpot();
  changed = TRUE;
  RepaintRaster();
  RepaintRasterInverted();
  x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
  x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
  }  /* end of ClearArea procedure */


InvertAll() {
  register int i;
  for (i=0;i<raster_length;i++)
    raster[i] ^= ~0;  /* invert = exclusive or with all 1's */
  changed = TRUE;
  XPixFill (
    grid_window,    /* window */
    0,	    /* x */
    0,	    /* y */
    squares_wide*square_size+1,	/* width */
    squares_high*square_size+1,	/* height */
    1,	    /* pixel */
    NULL,   /* clipmask */
    GXinvert,	/* function */
    invertplane);  /* plane mask */
  RepaintGridLines (e_Invert);
  RepaintRaster();
  RepaintRasterInverted();
  }  /* end of InvertAll procedure */


InvertArea() {
  int x1, y1, x2, y2;
  register int x, y;
  if (AskUserForArea (&x1, &y1, &x2, &y2))
    return;
  for (x=x1;x<=x2;x++)
    for (y=y1;y<=y2;y++)
      InvertRasterBit (raster, x, y);
  ExThroughRectangle (x1, y1, x2, y2);  /* wipe out X-outs */
  XPixFill (
    grid_window,    	/* window */
    x1*square_size+1,  /* x */
    y1*square_size+1,  /* y */
    (x2-x1+1)*square_size-1,  /* width */
    (y2-y1+1)*square_size-1,  /* height */
    1,	    /* pixel */
    NULL,   /* clipmask */
    GXinvert,	/* function */
    invertplane); /* plane mask */
  RepaintGridLinesPartially (x1, y1, x2+1, y2+1, e_Invert, FALSE);
  changed = TRUE;
  RepaintRaster();
  RepaintRasterInverted();
  x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
  x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
  }  /* end of InvertArea procedure */


CopyOrMoveArea (what)
  {
  int x1, y1, x2, y2;
  int x1dest, y1dest;
  if (AskUserForArea (&x1, &y1, &x2, &y2))
    return;
  if (AskUserForDest (&x1dest, &y1dest, x2-x1+1, y2-y1+1))
    /* button released outside grid */
    ExThroughRectangle (x1_square_exed_through, y1_square_exed_through,
      x2_square_exed_through, y2_square_exed_through);
  else {
    register int xsrc, ysrc, xdest, ydest;
    register short *new_raster =
      (short *) malloc (raster_length*sizeof(short));

    if (x_hot_spot != OUT_OF_RANGE)
    	HighlightHotSpot();  /* actually UNhighlight it */

    /* copy whole raster to new raster */
    bcopy (raster, new_raster, raster_length*sizeof(short));
    
    if (what == MOVE)
      /* clear source bits in new raster.  this is VERY inefficient.
         sure wish we had BitBlt available in user memory! */
      for (xsrc = x1; xsrc <= x2; xsrc++)
        for (ysrc = y1; ysrc <= y2; ysrc++)
	  SetRasterBit (new_raster, xsrc, ysrc, 0);

    /* copy old source bits to new destination. this is VERY inefficient.
       sure wish we had BitBlt available in user memory! */

    for (xsrc = x1, xdest = x1dest;
      xsrc<=x2 && xdest < squares_wide; xsrc++, xdest++) 
        for (ysrc = y1, ydest = y1dest;
          ysrc<=y2 && ydest < squares_high; ysrc++, ydest++)
	    if (what == OVERLAY) {
		if (GetRasterBit (raster, xsrc, ysrc))
		    SetRasterBit (new_raster, xdest, ydest, 1);
	    } else
		SetRasterBit (new_raster, xdest, ydest, 
		  GetRasterBit (raster, xsrc, ysrc));

    free (raster);
    raster = new_raster;
    if (what == MOVE)
    	RepaintRectangles (x1, y1, x2, y2, x1dest, y1dest);
    else {
	ExThroughRectangle (x1_square_exed_through, y1_square_exed_through,
	    x2_square_exed_through, y2_square_exed_through);
    	RefillGridPartially (x1dest, y1dest, xdest-1, ydest-1, TRUE);
	}

    if (x_hot_spot != OUT_OF_RANGE)
    	HighlightHotSpot();  /* put the hot spot back on the screen */

    RepaintRaster();
    RepaintRasterInverted();
    changed = TRUE;
    x1_square_plus_through = y1_square_plus_through = OUT_OF_RANGE;
    x2_square_plus_through = y2_square_plus_through = OUT_OF_RANGE;
    }
    
  x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
  x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
  }  /* end of CopyArea procedure */

#define MAX(a,b)	(((a) > (b)) ? (a) : (b))
#define MIN(a,b)	(((a) < (b)) ? (a) : (b))
#define ABS(a)		(((a) >= 0) ? (a) : -(a))
#define CheckSetRasterBit(r,x,y,c)	\
	if ((x) >= 0 && (x) < squares_wide && (y) >= 0 && (y) < squares_high) \
		SetRasterBit(r, x, y, c)

Line ()
{
	int	i, x1, y1, x2, y2;
	double	dx, dy, x, y, diff;

	if (AskUserForPoint(&x1, &y1, 0))
		return;
	if (AskUserForPoint(&x2, &y2, 1))
		return;
	ExThroughRectangle (x1_square_exed_through, y1_square_exed_through,
		x2_square_exed_through, y2_square_exed_through);
	PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
		x2_square_plus_through, y2_square_plus_through);

	dx = x2 - x1;
	dy = y2 - y1;
	x = x1 + 0.5;
	y = y1 + 0.5;
	diff = MAX(ABS(dx), ABS(dy));
	if (diff == 0)
		diff = 0.9;
	dx /= diff;
	dy /= diff;
	for (i = 0; i <= (int)diff; i++) {
		SetRasterBit(raster, (int)x, (int)y, 1);
		x += dx;
		y += dy;
	}
	RefillGridPartially(MIN(x1, x2), MIN(y1, y2), MAX(x1, x2), MAX(y1, y2),
		FALSE);
	changed = TRUE;
	x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
	x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
	x1_square_plus_through = y1_square_plus_through = OUT_OF_RANGE;
	x2_square_plus_through = y2_square_plus_through = OUT_OF_RANGE;
	RepaintRaster();
	RepaintRasterInverted();
}

#include <math.h>

Circle(filled)
{
	int	i, j, x, x1, y1, x2, y2, dx, dy;
	double	rad, half;

	if (AskUserForPoint(&x1, &y1, 0))
		return;
	if (AskUserForPoint(&x2, &y2, 1))
		return;
	ExThroughRectangle (x1_square_exed_through, y1_square_exed_through,
		x2_square_exed_through, y2_square_exed_through);
	PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
		x2_square_plus_through, y2_square_plus_through);

	dx = x2 - x1;
	dy = y2 - y1;
	rad = sqrt((double)(dx * dx + dy * dy)) + 0.5;
	if (filled)
		for (i = 0; i <= (int)rad; i++) {
			x = sqrt(rad * rad - i * i);
			for (j = x1 - x; j <= x1 + x; j++) {
				CheckSetRasterBit(raster, j, y1 - i, 1);
				CheckSetRasterBit(raster, j, y1 + i, 1);
			}
		}
	else {
		half = rad * sqrt(2.0)/2;
		for (i = 0; i <= (int)half; i++) {
			x = sqrt(rad * rad - i * i);
			CheckSetRasterBit(raster, x1 - x, y1 - i, 1);
			CheckSetRasterBit(raster, x1 - x, y1 + i, 1);
			CheckSetRasterBit(raster, x1 + x, y1 - i, 1);
			CheckSetRasterBit(raster, x1 + x, y1 + i, 1);
			CheckSetRasterBit(raster, x1 - i, y1 - x, 1);
			CheckSetRasterBit(raster, x1 - i, y1 + x, 1);
			CheckSetRasterBit(raster, x1 + i, y1 - x, 1);
			CheckSetRasterBit(raster, x1 + i, y1 + x, 1);
		}
	}
	RefillGridPartially(x1-(int)rad, y1-(int)rad, 
		x1+(int)rad, y1+(int)rad, FALSE);
	changed = TRUE;
	x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
	x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
	x1_square_plus_through = y1_square_plus_through = OUT_OF_RANGE;
	x2_square_plus_through = y2_square_plus_through = OUT_OF_RANGE;
	RepaintRaster();
	RepaintRasterInverted();
}

ClearHotSpot() {
    if (x_hot_spot == OUT_OF_RANGE)
    	return;
    HighlightHotSpot();  /* UNhighlight existing hot spot */
    x_hot_spot = y_hot_spot = OUT_OF_RANGE;
    changed = TRUE;
    }

SetHotSpot() {
    XCompressEvents();
    XDefineCursor (outer_window, dot);
    XSelectInput (outer_window, ButtonPressed | ButtonReleased | ExposeWindow);
    	/* so that we can detect button pressed outside grid */

    while (TRUE) {
	XEvent event;
	int x1, y1;
	XNextEvent (&event);
	switch (event.type) {

	    case ButtonPressed:
	    case MouseMoved:
	    	if ((event.window == grid_window)
		&& !WhatSquare (&event, &x1, &y1)
		&& (x_hot_spot != x1 || y_hot_spot != y1)) {

		    /* UNhighlight old hot spot */
		    if (x_hot_spot != OUT_OF_RANGE)
		    	HighlightHotSpot();  

		    x_hot_spot = x1;
		    y_hot_spot = y1;

		    /* highlight new hot spot */
		    HighlightHotSpot();

		    changed = TRUE;
		    }
		break;  /* keep looping until button is released */

	    case ButtonReleased:
		XExpandEvents();
		XDefineCursor (outer_window, cross);
		XSelectInput (outer_window, ExposeWindow);
	    	return;

	    case ExposeWindow:
	    case ExposeRegion:
	    	ProcessEvent (&event);
		break;
		
	    default:
	    	break;  /* just throw it away */
		
	    }
	}
    }

RepaintRectangles (x1, y1, x2, y2, x3, y3)
    int x1, y1; /* first rectangle's top & left */
    int x2, y2; /* first rectangle's bottom & right */
    int x3, y3; /* second rectangle's top & left */
    {
    int x4 = x3 + (x2 - x1);  /* second rectangle's right edge */
    int y4 = y3 + (y2 - y1);  /* second rectangle's bottom edge */

    if (x4 >= squares_wide) x4 = squares_wide-1;
    if (y4 >= squares_wide) y4 = squares_high-1;

    /* if first rectangle is right of second, swap "first" and "second" variables */
    if (x1 > x3)
    	{int temp;
#define swap(a,b) {temp = a; a = b; b = temp;}
	swap (x1, x3); swap (y1, y3); swap (x2, x4); swap (y2, y4);
#undef swap
    	}
    
    RefillGridPartially (x1, y1, x2, y2, TRUE);

    if ((x3 > x2) || (max (y1, y3) > min (y2, y4)))
    	/* rectangles don't overlap */
	RefillGridPartially (x3, y3, x4, y4, TRUE);

    else if (y1 < y3) {
	/* second rectangle is below & right of first */
	RefillGridPartially (x2+1, y3, x4, y2, TRUE);
	RefillGridPartially (x3, y2+1, x4, y4, TRUE);
	}

    else {
	/* second rectangle is above & right of first */
	RefillGridPartially (x3, y3, x4, y1-1, TRUE);
	RefillGridPartially (x2+1, y1, x4, y4, TRUE);
	}
    }


/* AskUserForArea returns FALSE if the user has defined a valid area,
   TRUE if the user hasn't (e.g. by clicking outside grid) */

boolean AskUserForArea(px1, py1, px2, py2) 
  int *px1, *py1, *px2, *py2;
  {
  XEvent event;
  int x1, y1, x2, y2;
  boolean result;

  XSelectInput (outer_window, ButtonPressed | ExposeWindow);
    /* so that we can detect button pressed outside grid */

  XDefineCursor (outer_window, upper_left);
  
  while (TRUE) {
    XNextEvent (&event);
    switch (event.type) {
      case ButtonPressed:
        if ((event.window != grid_window)
	|| WhatSquare (&event, &x1, &y1)) {
          XDefineCursor (outer_window, cross);
	  XSelectInput (outer_window, ExposeWindow);
          return (TRUE);
	  }
	goto out1;  /* get out of the loop */
      case ExposeWindow:
      case ExposeRegion:
      	ProcessEvent (&event);
      	break;
      default:
      	break;	/* just throw it away */
      }
    }

  out1:
  XCompressEvents();  /* DO collapse consecutive MouseMoved events */
  ExThroughSquare (x1, y1);
  FlushLineBuffer();
  x1_square_exed_through = x2_square_exed_through = x2 = x1;
  y1_square_exed_through = y2_square_exed_through = y2 = y1;
  XDefineCursor (outer_window, lower_right);
  
  while (TRUE) {
    XNextEvent (&event);
    switch (event.type) {
      case ButtonPressed:
      	result = TRUE;
	goto out2;
      case ExposeWindow:
      case ExposeRegion:
      	ProcessEvent (&event);
      	break;
      case MouseMoved:
      case ButtonReleased: {
        int x, y;
      	result = (event.window != grid_window)
            || WhatSquare (&event, &x, &y)  /* mouse outside grid? */
	    || (x < x1) || (y < y1);
      	if (result) {
	  ExThroughRectangle (x1+1, y1, x2, y2);
	  ExThroughRectangle (x1, y1+1, x1, y2);
	  x2 = x2_square_exed_through = x1;
	  y2 = y2_square_exed_through = y1;
	  }
	else if ((x == x2) && (y == y2))
	  ; /* both dimensions the same; do nothing */
	else if ((x > x2) == (y > y2)) {
	  /* both dimensions bigger or smaller */
	  ExThroughRectangle (min(x2,x)+1, y1, max(x2,x), max(y2,y));
	  ExThroughRectangle (x1, min(y2,y)+1, min(x2,x), max(y2,y));
	  x2 = x2_square_exed_through = x;
	  y2 = y2_square_exed_through = y;
	  }
        else {
	  /* one dimension bigger, the other smaller */
	  ExThroughRectangle (min(x2,x)+1, y1, max(x2,x), min(y2,y));
	  ExThroughRectangle (x1, min(y2,y)+1, min(x2,x), max(y2,y));
	  x2 = x2_square_exed_through = x;
	  y2 = y2_square_exed_through = y;
	  }
	if (event.type == ButtonReleased)
	  goto out2;
	break;
	}
      default:
      	break;	/* just throw it away */
      }
    }

  out2:
  XSelectInput (outer_window, ExposeWindow);
  XDefineCursor (outer_window, cross);
  if (result) {
    /* no area properly selected; remove X-outs from display */
    ExThroughRectangle (x1, y1, x2, y2);
    x1_square_exed_through = y1_square_exed_through = OUT_OF_RANGE;
    x2_square_exed_through = y2_square_exed_through = OUT_OF_RANGE;
    }
  else {
    *px1 = x1;
    *px2 = x2;
    *py1 = y1;
    *py2 = y2;
    }
  XExpandEvents();
  return (result);
  }  /* end of AskUserForArea procedure */

boolean AskUserForDest (px1, py1, width, height)
  int *px1, *py1;
  int width, height;
  {
  XEvent event;
  boolean result;
  XCompressEvents();  /* DO collapse consecutive MouseMoved events */
  XSelectInput (outer_window, ButtonPressed | ButtonReleased | ExposeWindow);
    /* so we can detect button action outside grid */
  XDefineCursor (outer_window, upper_left);

  while (TRUE) {
    XNextEvent (&event);
    switch (event.type) {

      case ExposeWindow:
      case ExposeRegion:
        ProcessEvent (&event);
	break;

      case ButtonPressed:
      case MouseMoved: {
	int x1_new, y1_new;
	boolean this_window = (event.window == grid_window)
	  && !WhatSquare (&event, &x1_new, &y1_new);

	if (this_window && (x1_new == *px1) && (y1_new == *py1))
	  break;  /* mouse is still in same square as before; do nothing */

        if (x1_square_plus_through != OUT_OF_RANGE)
          PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
              x2_square_plus_through, y2_square_plus_through);

	if (this_window) {
	  *px1 = x1_square_plus_through = x1_new;
	  *py1 = y1_square_plus_through = y1_new;
	  x2_square_plus_through = min (x1_new + width, squares_wide) - 1;
	  y2_square_plus_through = min (y1_new + height, squares_high) - 1;
          PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
              x2_square_plus_through, y2_square_plus_through);
	  }
	else {
          x1_square_plus_through = y1_square_plus_through = OUT_OF_RANGE;
          x2_square_plus_through = y2_square_plus_through = OUT_OF_RANGE;
	  *px1 = *py1 = OUT_OF_RANGE;
	  }
        break;
	}

      case ButtonReleased: {
        result = (event.window != grid_window)
          || WhatSquare (&event, px1, py1);
	goto out;
	}

      default:
        break;  /* throw it away */
      }
    }

    out:
    if (result) {
      /* button released outside grid */
      if (x1_square_plus_through != OUT_OF_RANGE)
        PlusThroughRectangle (x1_square_plus_through, y1_square_plus_through,
          x2_square_plus_through, y2_square_plus_through);
      x1_square_plus_through = y1_square_plus_through = OUT_OF_RANGE;
      x2_square_plus_through = y2_square_plus_through = OUT_OF_RANGE;
      }

    XExpandEvents();
    XSelectInput (outer_window, ExposeWindow);
    XDefineCursor (outer_window, cross);
    return (result);
    }  /* end of AskUserForDest procedure */

boolean AskUserForPoint (xp, yp, plus)
  int *xp, *yp;
  {
  XEvent event;
  boolean this_window;

  XCompressEvents();  /* DO collapse consecutive MouseMoved events */
  XSelectInput (outer_window, ButtonPressed | ExposeWindow);
    /* so we can detect button action outside grid */
  XDefineCursor (outer_window, dot);

  while (TRUE) {
    XNextEvent (&event);
    switch (event.type) {

      case ExposeWindow:
      case ExposeRegion:
        ProcessEvent (&event);
	break;

      case ButtonReleased:
	this_window = (event.window == grid_window)
			      && !WhatSquare (&event, xp, yp);
	if (this_window) {
		if (plus) {
			PlusThroughRectangle (*xp, *yp, *xp, *yp);
			x1_square_plus_through = x2_square_plus_through = *xp;
			y1_square_plus_through = y2_square_plus_through = *yp;
		} else {
			ExThroughRectangle (*xp, *yp, *xp, *yp);
			x1_square_exed_through = x2_square_exed_through = *xp;
			y1_square_exed_through = y2_square_exed_through = *yp;
		}
	}
	goto out;
        break;

      default:
        break;  /* throw it away */
      }
    }

    out:

    XExpandEvents();
    XSelectInput (outer_window, ExposeWindow);
    XDefineCursor (outer_window, cross);
    return (!this_window);
    }

DialogInputHandler (event)
  XEvent *event;
  {
  switch (event->type) {
    case ExposeWindow:
    case ExposeRegion:
      ProcessEvent (event);
    }
  }

enum output_error {e_rename, e_write};

/* WriteOutput returns TRUE if output successfully written, FALSE if not */

WriteOutput() {
  FILE *file;
  if (!changed)
    return (TRUE);
  if (rename (filename, backup_filename) && errno != ENOENT)
    return (HandleOutputError(e_rename));
  file = fopen (filename, "w+");
  if (!file)
    return (HandleOutputError(e_write));
  WriteOutputToFile (file);
  fclose (file);
  changed = FALSE;
  return (TRUE);
  }


/* HandleOutputError returns TRUE if alternate file written, FALSE if not */

int HandleOutputError(e)
  enum output_error e;
  {
  int result;
  char *strings[2];
  char msg1[120], msg2[120];
  char *tmp_filename;
  if (e == e_rename)
    sprintf (msg1, "Can't rename %s to %s -- %s",
      filename, backup_filename, sys_errlist[errno]);
  else
    sprintf (msg1, "Can't write on file %s -- %s",
      filename, sys_errlist[errno]);
  tmp_filename = TmpFileName (filename);
  sprintf (msg2, "Should I write output to file %s?", tmp_filename);
  strings[0] = "Yes";
  strings[1] = "No";
  result = dialog (outer_window, font, fontInfo.height,
    msg1, msg2, strings, 2, DialogInputHandler);

  if (result == 0)  /* "yes" */ {
    filename = tmp_filename;
    free (backup_filename);
    backup_filename = BackupName (filename);
    return (WriteOutput());
    }
  else {  /* "no" */
    free (tmp_filename);
    return (FALSE);
    }
  }

    
Quit() {
  if (changed) {
    int result;
    char *strings[3];
    strings[0] = "Yes";
    strings[1] = "No";
    strings[2] = "Cancel";
    result = dialog (outer_window, font, fontInfo.height,
      "Save changes before quitting?", "", strings, 3, DialogInputHandler);
      
    switch (result) {
      case 0:     /* "yes" */
      	if (WriteOutput())
	  exit(0);
	else return;
      case 1:    /* "no" */
        exit(0);
      default:  /* "cancel" */
      	return;
      }
    }

  exit(0);
  }

HighlightHotSpot() {
  /* Draw a diamond in the hot spot square */
  /* x1 and y1 are the center of the hot spot square */
  register int x1 = x_hot_spot*square_size + square_size/2;
  register int y1 = y_hot_spot*square_size + square_size/2;
  register int radius = square_size/6;
  register int i;
  Vertex v[5];
  v[0].x = v[2].x = v[4].x = x1;
  v[1].x = x1 + radius;
  v[3].x = x1 - radius;
  v[0].y = v[4].y = y1 + radius;
  v[1].y = v[3].y = y1;
  v[2].y = y1 - radius;
  for (i=0;i<5;i++)
    v[i].flags = 0;
  XDraw (grid_window, v, 5, 1, 1, 1, GXinvert, highlightplane);
  }

ExThroughRectangle (x1, y1, x2, y2)
  register int x1, y1, x2, y2;
  {
  register int x, y;
  for (x=x1;x<=x2;x++)
    for (y=y1;y<=y2;y++)
      ExThroughSquare (x, y);
  FlushLineBuffer();
  }


ExThroughSquare (x, y)
  register int x, y;
  {
  register int x1 = x*square_size;
  register int y1 = y*square_size;
  LineIntoBuffer (x1+1, y1+1,
    x1+square_size, y1+square_size);
  LineIntoBuffer (x1+square_size-1, y1+1,
    x1, y1+square_size);
  }


PlusThroughRectangle (x1, y1, x2, y2)
  register int x1, y1, x2, y2;
  {
  register int x, y;
  for (x=x1;x<=x2;x++)
    for (y=y1;y<=y2;y++)
      PlusThroughSquare (x, y);
  FlushLineBuffer();
  }

PlusThroughSquare (x, y)
  register int x, y;
  {
  register int x1 = x*square_size;
  register int y1 = y*square_size;
  LineIntoBuffer (x1+square_size/2, y1+1,
    x1+square_size/2, y1+square_size);
  LineIntoBuffer (x1+1, y1+square_size/2,
    x1+square_size, y1+square_size/2);
  }


#define BUFFER_MAXLENGTH 200  /* must be even */
static Vertex buffer [BUFFER_MAXLENGTH];
static int buffer_length = 0;

LineIntoBuffer (x1, y1, x2, y2) {
  buffer [buffer_length].x = x1;
  buffer [buffer_length].y = y1;
  buffer [buffer_length++].flags = VertexDontDraw;
  buffer [buffer_length].x = x2;
  buffer [buffer_length].y = y2;
  buffer [buffer_length++].flags = 0;
  if (buffer_length == BUFFER_MAXLENGTH)
    FlushLineBuffer();
  }
  
FlushLineBuffer () {
  XDraw (grid_window, buffer, buffer_length, 1, 1, 1, GXinvert,highlightplane);
  buffer_length = 0;
  }

#ifdef romp
/* 
 * prerelease IBM RT/PC software does not have ffs in its C library.
 * This code should be thrown away by summer, 1986.
 */
int ffs(i)
   int i;
   {
   int j = 1;
   if (i == 0) return (0);
   while (1) {
        if (i & 1) return (j);
        j++;
        i >>= 1;
        }
   }

#endif
