/*
 * 
 *    Copyright (C) 1986 Free Software Foundation, Inc.
 * 
 * This file is part of GNU Emacs.
 * 
 * GNU Emacs is distributed in the hope that it will be useful,
 * but without any warranty.  No author or distributor
 * accepts responsibility to anyone for the consequences of using it
 * or for whether it serves any particular purpose or works at all,
 * unless he says so in writing.
 * 
 * Everyone is granted permission to copy, modify and redistribute
 * GNU Emacs, but only under the conditions described in the
 * document "GNU Emacs copying permission notice".   An exact copy
 * of the document is supposed to have been given to you along with
 * GNU Emacs so that you can know how you may redistribute it all.
 * It should be in a file named COPYING.  Among other things, the
 * copyright notice and this notice must be preserved on all copies.
 * 
 *
 * For Emacs in SunView/Sun-Windows: (supported by Sun Unix v3.2)
 * Insert a notifier filter-function to convert all useful input 
 * to "key" sequences that emacs can understand.  See: Emacstool(1).
 *
 * Author: Jeff Peck, Sun Microsystems, Inc. <peck@sun.com>
 *
 * Original Idea: Ian Batten
 * Updated 15-Mar-88, Jeff Peck: set IN_EMACSTOOL, TERM, TERMCAP
 * 
 */

#include <suntool/sunview.h>
#include <suntool/tty.h>
#include <stdio.h>
#include <sys/file.h>

#define BUFFER_SIZE 128               /* Size of all the buffers */

/* define WANT_CAPS_LOCK to make f-key T1 (aka F1) behave as CapsLock */
#define WANT_CAPS_LOCK
#ifdef WANT_CAPS_LOCK
int caps_lock;		/* toggle indicater for f-key T1 caps lock */
static char *Caps = "[CAPS] ";		/* Caps Lock prefix string */
#define CAPS_LEN 7			/* strlen (Caps) */
#endif

static char *mouse_prefix = "\030\000";	/* C-x C-@ */
static int   m_prefix_length = 2;       /* mouse_prefix length */

static char *key_prefix = "\030*";  	/* C-x *   */
static int   k_prefix_length = 2;       /* key_prefix length */

static char *emacs_name = "emacs";	/* default run command */
static char buffer[BUFFER_SIZE];	/* send to ttysw_input */
static char *title = "Emacstool - ";	/* initial title */

Frame frame;                            /* Base frame for system */
Tty ttysw;                              /* Where emacs is */
int font_width, font_height;            /* For translating pixels to chars */

int console_fd = 0;		/* for debugging: setenv DEBUGEMACSTOOL */
FILE *console;			/* for debugging: setenv DEBUGEMACSTOOL */

Icon frame_icon;
/* make an icon_image for the default frame_icon */
static short default_image[258] = 
{
#include <images/terminal.icon>
};
mpr_static(icon_image, 64, 64, 1, default_image);


/*
 * Assign a value to a set of keys
 */
int
button_value (event)
     Event *event;
{
  int retval = 0;
  /*
   * Code up the current situation:
   *
   * 1 = MS_LEFT;
   * 2 = MS_MIDDLE;
   * 4 = MS_RIGHT;
   * 8 = SHIFT;
   * 16 = CONTROL;
   * 32 = META;
   * 64 = DOUBLE;
   * 128 = UP;
   */

  if (MS_LEFT   == (event_id (event))) retval = 1;
  if (MS_MIDDLE == (event_id (event))) retval = 2;
  if (MS_RIGHT  == (event_id (event))) retval = 4;

  if (event_shift_is_down (event)) retval += 8;
  if (event_ctrl_is_down  (event)) retval += 16;
  if (event_meta_is_down  (event)) retval += 32;
  if (event_is_up         (event)) retval += 128;
  return retval;
}

/*
 *  Variables to store the time of the previous mouse event that was
 *  sent to emacs.
 *
 *  The theory is that to time double clicks while ignoreing UP buttons,
 *  we must keep track of the accumulated time.
 *
 *  If someone writes a SUN-SET-INPUT-MASK for emacstool,
 *  That could be used to selectively disable UP events, 
 *  and then this cruft wouldn't be necessary.
 */
static long prev_event_sec = 0;
static long prev_event_usec = 0;

/*
 *  Give the time difference in milliseconds, where one second
 *  is considered infinite.
 */
int
time_delta (now_sec, now_usec, prev_sec, prev_usec)
     long now_sec, now_usec, prev_sec, prev_usec;
{
  long sec_delta = now_sec - prev_sec;
  long usec_delta = now_usec - prev_usec;
  
  if (usec_delta < 0) {		/* "borrow" a second */
    usec_delta += 1000000;
    --sec_delta;
  }
  
  if (sec_delta >= 10) 
    return (9999);		/* Infinity */
  else
    return ((sec_delta * 1000) + (usec_delta / 1000));
}


/*
 * Filter function to translate selected input events for emacs
 * Mouse button events become ^X^@(button x-col y-line time-delta) .
 * Function keys: ESC-*{c}{lrt} l,r,t for Left, Right, Top; 
 * {c} encodes the keynumber as a character [a-o]
 */
static Notify_value
input_event_filter_function (window, event, arg, type)
     Window window;
     Event *event;
     Notify_arg arg;
     Notify_event_type type;
{
  struct timeval time_stamp;

  if (console_fd) fprintf(console, "Event: %d\n", event_id(event));

  /* UP L1 is the STOP key */
  if (event_id(event) == WIN_STOP) {
    ttysw_input(ttysw, "\007\007\007\007\007\007\007", 7);
    return NOTIFY_IGNORED;
  }

  /* UP L5 & L7 is Expose & Open, let them pass to sunview */
  if (event_id(event) == KEY_LEFT(5) || event_id(event) == KEY_LEFT(7))
    if(event_is_up (event)) 
      return notify_next_event_func (window, event, arg, type);
    else return NOTIFY_IGNORED;

  if (event_is_button (event)) { 	      /* do Mouse Button events */
/* Commented out so that we send mouse up events too.
   if (event_is_up (event)) 
      return notify_next_event_func (window, event, arg, type);
*/
    time_stamp = event_time (event);
    ttysw_input (ttysw, mouse_prefix, m_prefix_length);
    sprintf (buffer, "(%d %d %d %d)\015", 
	     button_value (event),
	     event_x (event) / font_width,
	     event_y (event) / font_height,
	     time_delta (time_stamp.tv_sec, time_stamp.tv_usec,
			 prev_event_sec, prev_event_usec)
	     );
    ttysw_input (ttysw, buffer, strlen(buffer));
    prev_event_sec = time_stamp.tv_sec;
    prev_event_usec = time_stamp.tv_usec;
    return NOTIFY_IGNORED;
  }
  
  { /* Do the function key events */
    int d;
    char c = (char) 0;
    if ((event_is_key_left  (event)) ?
	((d = event_id(event) - KEY_LEFT(1)   + 'a'), c='l') : 
	((event_is_key_right (event)) ?
	 ((d = event_id(event) - KEY_RIGHT(1) + 'a'), c='r') : 
	 ((event_is_key_top   (event)) ?
	  ((d = event_id(event) - KEY_TOP(1)  + 'a'), c='t') : 0)))
      {
	if (event_is_up(event)) return NOTIFY_IGNORED;
	if (event_shift_is_down (event)) c = c -  32;
	/* this will give a non-{lrt} for unshifted keys */
	if (event_ctrl_is_down  (event)) c = c -  64;
	if (event_meta_is_down  (event)) c = c + 128;
#ifdef WANT_CAPS_LOCK
/* set a toggle and relabel window so T1 can act like caps-lock */
	if (event_id(event) == KEY_TOP(1)) 
	  {
	    /* make a frame label with and without CAPS */
	    strcpy (buffer, Caps); 
	    title = &buffer[CAPS_LEN];
	    strncpy (title, (char *)window_get (frame, FRAME_LABEL),
		     BUFFER_SIZE - CAPS_LEN);
	    buffer[BUFFER_SIZE] = (char) 0;	
	    if (strncmp (title, Caps, CAPS_LEN) == 0)
	      title += CAPS_LEN; 		 /* already Caps */
	    caps_lock =  (caps_lock ? 0 : CAPS_LEN);
	    window_set(frame, FRAME_LABEL, (title -= caps_lock), 0);
	    return NOTIFY_IGNORED;
	  }
#endif
	ttysw_input (ttysw, key_prefix, k_prefix_length);
	sprintf (buffer, "%c%c", d, c);
	ttysw_input(ttysw, buffer, strlen(buffer));

	return NOTIFY_IGNORED;
      }
  }
  if ((event_is_ascii(event) || event_is_meta(event)) 
      && event_is_up(event)) return NOTIFY_IGNORED;
#ifdef WANT_CAPS_LOCK
/* shift alpha chars to upper case if toggle is set */
  if ((caps_lock) && event_is_ascii(event)
      && (event_id(event) >= 'a') && (event_id(event) <= 'z'))
    event_set_id(event, (event_id(event) - 32));
/* crufty, but it works for now. is there an UPCASE(event)? */
#endif
  return notify_next_event_func (window, event, arg, type);
}

main (argc, argv)
     int argc;
     char **argv;
{
  int error_code;	/* Error codes */
  
  if(getenv("DEBUGEMACSTOOL"))
    console = fdopen (console_fd = open("/dev/console",O_WRONLY), "w");

  			/* do this first, so arglist can override it */
  frame_icon = icon_create (ICON_LABEL, "Emacstool",
			    ICON_IMAGE, &icon_image,
			    0);

  putenv("IN_EMACSTOOL=t");	/* notify subprocess that it is in emacstool */

  if (putenv("TERM=sun") != 0)	/* TTYSW will be a TERM=sun window */
    {fprintf (stderr, "%s: Could not set TERM=sun, using `%s'\n",
	     argv[0], (char *)getenv("TERM")) ;};
  /*
   * If TERMCAP starts with a slash, it is the pathname of the
   * termcap file, not an entry extracted from it, so KEEP it!
   * Otherwise, it may not relate to the new TERM, so Nuke-It.
   * If there is no TERMCAP environment variable, don't make one.
   */
  {
    char *termcap ;	/* Current TERMCAP value */
    termcap = (char *)getenv("TERMCAP") ;
    if (termcap && (*termcap != '/'))
      {
	if (putenv("TERMCAP=") != 0)
	  {fprintf (stderr, "%s: Could not clear TERMCAP\n", argv[0]) ;} ;
      } ;
  } ;
  
  /* find command to run as subprocess in window */
  if (!(argv[0] = (char *)getenv("EMACSTOOL")))	/* Set emacs command name */
      argv[0] = emacs_name;			
  for (argc = 1; argv[argc]; argc++)		/* Use last one on line */
    if(!(strcmp ("-rc", argv[argc])))		/* Override if -rc given */
      {
	int i = argc;
	argv[argc--]=0;		/* kill the -rc argument */
	if (argv[i+1]) {	/* move to agrv[0] and squeeze the rest */
	  argv[0]=argv[i+1];
	  for (; argv[i+2]; (argv[i]=argv[i+2],argv[++i]=0));
	}
      }

  strcpy (buffer, title);
  strncat (buffer, argv[0],		 /* append run command name */
	   (BUFFER_SIZE - (strlen (buffer)) - (strlen (argv[0]))) - 1);

  			/* Build a frame to run in */
  frame = window_create ((Window)NULL, FRAME,
			 FRAME_LABEL, buffer,
			 FRAME_ICON, frame_icon,
			 FRAME_ARGC_PTR_ARGV, &argc, argv,
			 0);

  /* Create a tty with emacs in it */
  ttysw = window_create (frame, TTY, 
			 TTY_QUIT_ON_CHILD_DEATH, TRUE, 
			 TTY_BOLDSTYLE, 8, 
			 TTY_ARGV, argv, 
			 0);

  window_set(ttysw,
	     WIN_CONSUME_PICK_EVENTS, 
	     WIN_STOP,
	     WIN_MOUSE_BUTTONS, WIN_UP_EVENTS,
	     /* LOC_WINENTER, LOC_WINEXIT, LOC_MOVE, */
	     0,

	     WIN_CONSUME_KBD_EVENTS, 
	     WIN_STOP,
	     WIN_ASCII_EVENTS, 
	     WIN_LEFT_KEYS, WIN_TOP_KEYS, WIN_RIGHT_KEYS,
	     /* WIN_UP_ASCII_EVENTS, */
	     0,
	     
	     0);

  font_height = (int)window_get (ttysw, WIN_ROW_HEIGHT);
  font_width  = (int)window_get (ttysw, WIN_COLUMN_WIDTH);

                                         /* Interpose my event function */
  error_code = (int)  notify_interpose_event_func 
    (ttysw, input_event_filter_function, NOTIFY_SAFE);

  if (error_code != 0)                       /* Barf */
    {
      fprintf (stderr, "notify_interpose_event_func got %d.\n", error_code);
      exit (1);
    }

  window_main_loop (frame);                  /* And away we go */
}
