#ifndef lint
static char sccsid[] = "@(#)xclock.c	1.2 10/16/86";
#endif

#include <X/mit-copyright.h>

/* Copyright 1985 Massachusetts Institute of Technology */

/*
 * xclock.c MIT Project Athena, X Window system clock.
 *
 *  This program provides the user with a small
 * window contining a digital clock with day and date.
 * Parameters are variable from the command line.
 *
 *  Author:	Tony Della Fera, DEC
 *		September, 1984
 * Hacked up by a cast of thousands....
 */

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <X/Xlib.h>

#define PI			3.141592

#define SEG_BUFF_SIZE		128

#define SECOND_HAND_FRACT	90
#define MINUTE_HAND_FRACT	70
#define HOUR_HAND_FRACT		40
#define SECOND_HAND_TIME	30

#define DEF_UPDATE		60

#define DEF_BORDER		2
#define DEF_VECTOR_HEIGHT	1
#define DEF_VECTOR_WIDTH	1

#define DEF_DIGITAL_PADDING	10
#define DEF_DIGITAL_FONT	"6x10"
#define DEF_ANALOG_PADDING	8
#define DEF_ANALOG_WIDTH	164
#define DEF_ANALOG_HEIGHT	164

#define DEF_BORDER_COLOR	BlackPixel
#define DEF_HIGH_COLOR		BlackPixel
#define DEF_FRGRND_COLOR	BlackPixel
#define DEF_BKGRND_COLOR	WhitePixel

#define UNINIT			-1
#define FAILURE			0

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))
#define abs(a) ((a) < 0 ? -(a) : (a))

typedef enum _bool {FALSE, TRUE} Bool;

Bool XClockDebug = FALSE;
Bool AnalogClock = FALSE;
Bool ShowSecondHand = FALSE;
Bool Reverse = FALSE;

int CenterX = 0;
int CenterY = 0;
int NumSegs = 0;

int FrgrndColor;
int BkgrndColor;
int BorderColor;
int HighColor;

Vertex SegBuff[SEG_BUFF_SIZE];
Vertex *SegBuffPtr;

Window ClockWindow;

main(argc, argv)
	int argc;
	char **argv;
{
    char time_string[26];
    register char *time_ptr = time_string;
#ifdef DEBUG
    register Bool debug = XClockDebug;
#endif
    register int i;
    register int radius;
    register int padding = UNINIT; /* UNINIT = parameter uninitialized. */
    int border = UNINIT;	/* UNINIT = parameter uninitialized. */
    int update = UNINIT;	/* UNINIT = parameter uninitialized. */
    int win_width = UNINIT;	/* UNINIT = parameter uninitialized. */
    int win_height = UNINIT;	/* UNINIT = parameter uninitialized. */
    int second_hand_length;
    int minute_hand_length;
    int hour_hand_length;
    int readfds = 0;
    int maxfds = 0;
    int string_width;
    int status;
    Bool even_update = FALSE;

    long time_value;

    char *index();
    char *ctime(), asctim();
    char *strind;
    char *fn = DEF_DIGITAL_FONT;
    char *fore_color = NULL;
    char *back_color = NULL;
    char *high_color = NULL;
    char *brdr_color = NULL;
    char *geom = NULL;
    char *display = NULL;
    int rvflag = 0;

    Color cdef;

    struct tm *localtime();
    struct timeval timeout;
    struct tm tm; 
    struct tm otm;

    Font font;
    FontInfo font_info; 
    XEvent event;
    char *def_val;

    def_val = XGetDefault(argv[0], "BorderWidth");
    if (def_val != NULL) border = atoi(def_val);

    def_val = XGetDefault(argv[0], "BodyFont");
    if (def_val != NULL) fn = def_val;

    fore_color = XGetDefault(argv[0], "Foreground");
    back_color = XGetDefault(argv[0], "Background");
    high_color = XGetDefault(argv[0], "Highlight");
    brdr_color = XGetDefault(argv[0], "Border");

    def_val = XGetDefault(argv[0], "ReverseVideo");
    if(def_val != NULL && strcmp(def_val, "on") == 0) rvflag++;

    def_val = XGetDefault(argv[0], "InternalBorder");
    if (def_val != NULL) padding = atoi(def_val);
	
    if ((def_val = XGetDefault(argv[0], "Mode")) != NULL) {
	if (strcmp(def_val, "analog") == 0) AnalogClock = TRUE;
	if (strcmp(def_val, "digital") == 0) AnalogClock = FALSE;
	if (strcmp(def_val, "Analog") == 0) AnalogClock = TRUE;
	if (strcmp(def_val, "Digital") == 0) AnalogClock = FALSE;
    }

    def_val = XGetDefault(argv[0],"Update");
    if (def_val != NULL) update = atoi(def_val);

    geom = XGetDefault(argv[0], "Geometry");

    for (i = 1; i < argc; i++) {
	if (argv [i] [0] == '=') {
	    geom = argv[i];
	    continue;
	}
	strind = index(argv[i], ':');
	if(strind != NULL) {
	    display = argv[i];
	    continue;
	}
	strind = index(argv [i], '-');
	if (strind == NULL) Syntax(argv[0]);
	if (strcmp(argv [i], "-a") == 0 ||
	    strcmp(argv [i], "-analog") == 0) {
	    AnalogClock = TRUE;
	    continue;
	}
	if (strcmp(argv [i], "-d") == 0 ||
	    strcmp(argv [i], "-digital") == 0) {
	    AnalogClock = FALSE;
	    continue;
	}
	if (strcmp(argv [i], "-bw") == 0 ||
	    strcmp(argv [i], "-border") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    border = atoi(argv [i]);
	    continue;
	}
#ifdef DEBUG
	if (strcmp(argv [i], "-debug") == 0) {
	    XClockDebug = TRUE;
	    debug = TRUE;
	    continue;
	}
#endif
	if (strcmp(argv [i], "-fn") == 0 ||
	    strcmp(argv [i], "-font") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    fn = argv [i];
	    continue;
	}
	if (strcmp(argv [i], "-fg") == 0 ||
	    strcmp(argv [i], "-foreground") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    fore_color = argv [i];
	    continue;
	}
	if (strcmp(argv [i], "-bg") == 0 ||
	    strcmp(argv [i], "-background") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    back_color = argv [i];
	    continue;
	}
	if (strcmp(argv [i], "-hl") == 0 ||
	    strcmp(argv [i], "-highlight") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    high_color = argv [i];
	    continue;
	}
	if (strcmp(argv [i], "-bd") == 0 ||
	    strcmp(argv [i], "-bordercolor") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    brdr_color = argv [i];
	    continue;
	}
	if (strcmp(argv [i], "-help") == 0) {
	    Syntax(argv[0]);
	}
	if (strcmp(argv [i], "-p") == 0 ||
	    strcmp(argv [i], "-padding") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    padding = atoi(argv [i]);
	    continue;
	}
	if (strcmp(argv [i], "-rv") == 0 ||
	    strcmp(argv [i], "-reverse") == 0) {
	    rvflag++;
	    continue;
	}
	if (strcmp(argv [i], "-u") == 0 ||
	    strcmp(argv [i], "-update") == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    update = atoi(argv [i]);
	    continue;
	}
	Syntax(argv[0]);
    }

    /*
     * Open up the display.
     */
    if (XOpenDisplay(display) == NULL) {
	XClockError("Error while trying to open display");
    }
    if (rvflag) Reverse = TRUE;

    /*
     * Set up colors and pixmaps.
     */
    if (brdr_color != NULL) {
	if (DisplayCells() > 2) {
	    if (
		XParseColor(brdr_color, &cdef) &&
		XGetHardwareColor(&cdef)
		) BorderColor = cdef.pixel;
	    else BorderColor = DEF_BORDER_COLOR;
	}
	else if (strcmp(brdr_color, "black") == 0)
	    BorderColor = BlackPixel;
	else if (strcmp(brdr_color, "white") == 0)
	    BorderColor = WhitePixel;
	else BorderColor = DEF_BORDER_COLOR;
    }
    else BorderColor = DEF_BORDER_COLOR;

    if (fore_color != NULL) {
	if (DisplayCells() > 2) {
	    if (
		XParseColor(fore_color, &cdef) &&
		XGetHardwareColor(&cdef)
		) FrgrndColor = cdef.pixel;
	    else FrgrndColor = DEF_FRGRND_COLOR;
	}
	else if (strcmp(fore_color, "black") == 0)
	    FrgrndColor = BlackPixel;
	else if (strcmp(fore_color, "white") == 0)
	    FrgrndColor = WhitePixel;
	else FrgrndColor = DEF_FRGRND_COLOR;
    }
    else FrgrndColor = DEF_FRGRND_COLOR;

    if (back_color != NULL) {
	if (DisplayCells() > 2) {
	    if (
		XParseColor(back_color, &cdef) &&
		XGetHardwareColor(&cdef)
		) BkgrndColor = cdef.pixel;
	    else BkgrndColor = DEF_BKGRND_COLOR;
	}
	else if (strcmp(back_color, "black") == 0)
	    BkgrndColor = BlackPixel;
	else if (strcmp(back_color, "white") == 0)
	    BkgrndColor = WhitePixel;
	else BkgrndColor = DEF_BKGRND_COLOR;
    }
    else BkgrndColor = DEF_BKGRND_COLOR;

    if ((high_color != NULL) && AnalogClock) {
	if (DisplayCells() > 2) {
	    if (
		XParseColor(high_color, &cdef) &&
		XGetHardwareColor(&cdef)
		) HighColor = cdef.pixel;
	    else HighColor = DEF_HIGH_COLOR;
	}
	else if (strcmp(high_color, "black") == 0)
	    HighColor = BlackPixel;
	else if (strcmp(high_color, "white") == 0)
	    HighColor = WhitePixel;
	else HighColor = DEF_HIGH_COLOR;
    }
    else HighColor = DEF_HIGH_COLOR;
	
    if (Reverse) {
	HighColor = BorderColor = BkgrndColor;
	BkgrndColor = FrgrndColor;
	FrgrndColor = HighColor;
    }

    /*
     * Set up analog and digital specific defaults.
     */
    if (AnalogClock == TRUE) {
	/*
	 * Set analog defaults.
	 */
	if (padding == UNINIT) padding = DEF_ANALOG_PADDING;
	if (update == UNINIT) update = DEF_UPDATE;
	if (update <= SECOND_HAND_TIME) ShowSecondHand = TRUE;
	/*
	 * Initialize the segment buffer and segment buffer pointer.
	 */
	SegBuffPtr = SegBuff;

	/*
	 * Initialize the number of "segments" in the buffer; each
	 * segment is one Vertex or 3 shorts, an x value, a y value
	 * and the flags as passed to XDraw.
	 */
	NumSegs = 0;
    } 
    else {
	/* 
	 * Set digital defaults.
	 */

	if (padding == UNINIT) padding = DEF_DIGITAL_PADDING;
	if (update == UNINIT) update = DEF_UPDATE;

	/*
	 * Get font dependent information and determine window
	 * size from a test string.
	 */
	time(&time_value);
	time_ptr = ctime(&time_value);
	time_ptr[strlen(time_ptr) - 1] = 0;

	font = XGetFont(fn);
	if (font == FAILURE) XClockError("Can't get font");

	status = XQueryFont(font, &font_info);
	if (status == FAILURE) XClockError("Can't query font");

	string_width = XQueryWidth (time_ptr, font);
	
    }
	

    /* 
     * Now set analog and digital independent defaults.
     */
    if (border == UNINIT) border = DEF_BORDER;
    if (update > 1 && ((60 / update) * update == 60)) even_update = TRUE;

    /*
     * Open the main window.
     */
{
    int min_width, min_height;
    char default_geom[20];
    OpaqueFrame frame;
    if (AnalogClock) {
	min_width = DEF_ANALOG_WIDTH/3;
	min_height = DEF_ANALOG_HEIGHT/3;
	sprintf (default_geom,
		 "%dx%d-0-0", DEF_ANALOG_WIDTH, DEF_ANALOG_HEIGHT);
    }
    else {
	min_width = string_width + (2 * padding);
	min_height = font_info.height + (2 * padding);
	sprintf (default_geom, "%dx%d-0-0", min_width, min_height);
    }
    frame.bdrwidth = border;
    frame.border = XMakeTile (BorderColor);
    frame.background = XMakeTile (BkgrndColor);
    ClockWindow = XCreate (
			   AnalogClock ? "Analog XClock" : "Digital XClock",
			   argv[0],
			   geom,
			   default_geom,
			   &frame,
			   min_width,
			   min_height);
    if (ClockWindow == FAILURE)
	XClockError("Can't open clock window");
}

    /*
     * Select window exposure events to see if the contents of
     * the window have been erased or altered.  Select unmap events so
     * we can stop output when iconified.
     */
    XSelectInput(ClockWindow, ExposeWindow|UnmapWindow);

    /*
     * Map clock window to screen.
     */
    XMapWindow(ClockWindow);

    /*
     * Initialize the select system call's maximum file
     * descriptor number to be one more than the file descriptor
     * number of the X connection.
     */
    maxfds = dpyno() + 1;

    /*
     * Initialize the select timeout structure.
     */
    timeout.tv_sec = update;
    timeout.tv_usec = 0;

    /*
     * Initialize the old-time structure.
     */
    otm = *localtime(&time_value);

    /*
     * Synchronize X before proceeding.
     */
    XSync(FALSE);

    /*
     * Main clock loop.
     */
    while (TRUE) {
	time(&time_value);
	tm = *localtime(&time_value);
	if (even_update) {
	    /* Truncate to update interval, get new timeout */
	    timeout.tv_sec = update - tm.tm_sec;
	    tm.tm_sec = (tm.tm_sec / update) * update;
	    timeout.tv_sec += tm.tm_sec;
	}
	if (AnalogClock == FALSE) {	
	    /* 
	     * See if there are any events pending that
	     * arn't supposed to be there.
	     */
	    if (XPending() != 0) {
		/*
		 * There is an event pending so we must
		 * check to see if it is an ExposeWindow
		 * event, if it is anything else somthing
		 * went wrong!
		 */
		XNextEvent(&event);
		if (event.type == UnmapWindow) {
		    XPeekEvent(&event);
		    continue;
		}
		if (event.type != ExposeWindow) {
		    XClockError("Unexpected X_Event (digital mode)");
		}
	    }
	    time_ptr = asctime(&tm);
	    time_ptr[strlen(time_ptr) - 1] = 0;
	    XTextPad (
		      ClockWindow,
		      padding, padding, 
		      time_ptr, strlen(time_ptr),
		      font, 0, 0, 
		      FrgrndColor, BkgrndColor,
		      GXcopy, AllPlanes
		      );
	}
	else {
	    /* 
	     * Look for an X_Event associated with xclock.
	     */
	    if (XPending() != 0) {
		/*
		 * There is an event pending so we must
		 * check to see if it is an ExposeWindow
		 * event, if it is anything else, somthing
		 * went wrong!
		 */
		XNextEvent(&event);
		if (event.type == UnmapWindow) {
		    XPeekEvent(&event);
		    continue;
		}
		if (event.type == ExposeWindow) {
		    /*
		     * Ok, we have a window exposure event,
		     * refresh the clock face.  Check to
		     * see if the window has changed size.
		     */
		    XExposeWindowEvent *exp_event = (XExposeWindowEvent *)&event;
		    if ((exp_event->width != win_width) ||
			(exp_event->height != win_height)) {
			win_width = exp_event->width;
			win_height = exp_event->height;
			radius = (min(win_width, win_height) -(2 * padding)) / 2;
			second_hand_length = 
			((SECOND_HAND_FRACT *
			  radius)
			 / 100);
			minute_hand_length =
			((MINUTE_HAND_FRACT *
			  radius) / 100);
			hour_hand_length =
			((HOUR_HAND_FRACT *
			  radius) / 100);
			CenterX = win_width / 2;
			CenterY = win_height / 2;
		    }
		    DrawClockFace(second_hand_length,
				  radius);
		}
		else {
		    /*
		     * We should never get here!
		     */
		    XClockError("Unexpected X_Event (analog mode)");
		}
	    }
	    /*
	     * The second (or minute) hand is sec (or min) 
	     * sixtieths around the clock face. The hour hand is
	     * (hour + min/60) twelfths of the way around the
	     * clock-face.  The derivation is left as an excercise
	     * for the reader.
	     */

	    /*
	     * Erase old hands.
	     */
	    if (ShowSecondHand == TRUE) {
		DrawHand(1, second_hand_length,
			 ((double) otm.tm_sec)/60.0);
	    }
	    DrawHand(1, minute_hand_length,
		     ((double) otm.tm_min)/60.0);
	    DrawHand(1, hour_hand_length,
		     (((double)otm.tm_hour) + 
		      (((double)otm.tm_min)/60.0))/12.0);
	    Sync(BkgrndColor);

	    /*
	     * 12 hour clock.
	     */
	    if(tm.tm_hour > 12)
		tm.tm_hour -= 12;

	    /*
	     * Draw new hands.
	     */
	    if (ShowSecondHand == TRUE) {
		DrawHand(1, second_hand_length,
			 ((double) tm.tm_sec)/60.0);
	    }
	    DrawHand(1, minute_hand_length,
		     ((double) tm.tm_min)/60.0);
	    DrawHand(1, hour_hand_length,
		     (((double) tm.tm_hour) + 
		      (((double) tm.tm_min)/60.0))/12.0);
	    Sync(HighColor);

	    /*
	     * Make the new time now be the old time.
	     */
	    otm = tm;
	}
	XFlush ();

	/*
	 * Use the select system call on the file descriptor in
	 * the display structure to determine if there is work
	 * to be done.  If not block untill timeout.  Remember to
	 * reset the file descriptor before each select.
	 */
	readfds = 1 << dpyno();
	if (select(maxfds, &readfds, NULL, NULL, &timeout) == -1)
	    XClockError("Error in select on display file descriptor");
    }
}


/*
 * DrawHand - Draws (or erases) a hand.  Fraction_of_a_circle is a
 * fraction between 0 and 1 (inclusive) indicating how far around the
 * circle (clockwise) high noon.
 *
 * Blank_length is the distance from the center which the hand begins
 * (logically 0, but I have the feature, I might as well use it).
 * length is the maximum length of the hand.
 *
 * The blank_length feature is because I wanted to draw tick-marks around the
 * circle (for seconds).  The obvious means of drawing lines from the center
 * to the perimeter, then erasing all but the outside most pixels doesn't
 * work because of round-off error (sigh).
 */
DrawHand(blank_length, length, fraction_of_a_circle)
	int blank_length;
	int length;
	double fraction_of_a_circle;
{

	double cos();
	double sin();
	double angle;

	/*
	 *  A full circle is 2 PI radians.
	 *  Angles are measured from 6 o'clock, not noon (or so it seems).
	 *  Thus, when fraction_of_a_circle is 0, we want angle to be PI,
	 *  and when fraction_of_a_circle is 1, we want angle to be 3 PI.
	 *
	 *  Also, angles increase counter-clockwise, not clockwise, so we
	 *  throw in a factor of -1 so our clock doesn't run backwards.
	 */
	angle = (-2 * PI * fraction_of_a_circle) + PI;

	/*
	 * Add the move instruction to the segment buffer and increment
	 * the "next" pointer.
	 */
	SegBuffPtr->x = CenterX + (int)((float)blank_length * sin(angle));
	SegBuffPtr->y = CenterY + (int)((float)blank_length * cos(angle));
	SegBuffPtr->flags = VertexDontDraw;
	SegBuffPtr++;
	NumSegs++;

	/*
	 * Add the new point to the buffer and increment the "next" pointer.
	 */
	SegBuffPtr->x = CenterX + (int)((float)length * sin(angle));
	SegBuffPtr->y = CenterY + (int)((float)length * cos(angle));
	SegBuffPtr->flags = VertexDrawLastPoint;
	SegBuffPtr++;
	NumSegs++;
}


/*
 *  Draw the clock face (every fifth tick-mark is longer
 *  than the others).
 */
DrawClockFace(second_hand, radius)
	int second_hand;
	int radius;
{
	register Bool debug = XClockDebug;
	register int i;
	register int delta = (radius - second_hand) / 3;
	
	XClear(ClockWindow);
	for (i = 0; i < 60; i++) {
		if ((i % 5) == 0) {
			DrawHand(second_hand, radius, ((double) i)/60.);
		}
		else {
			DrawHand((radius - delta), radius, ((double) i)/60.);
		}
	}
	/*
	 * Flush the buffer to the VS100.
	 */
	Sync(FrgrndColor);
}


/*
 * This routine synchronizes the segment buffer contents with the
 * VS100 screen.  In effect, it flushes the buffer contents to the
 * screen.
 */
Sync(color)
	int color;		/* X drawing color. */
{
	register Bool debug = XClockDebug;
	
	/*
	 * Call the X draw curve routine.
	 */
	XDraw(
		ClockWindow,
		SegBuff, NumSegs,
		DEF_VECTOR_WIDTH, DEF_VECTOR_HEIGHT,
		color, GXcopy, AllPlanes
	);

	/*
	 * Reset the segment buffer pointer and the segment counter.
	 */
	SegBuffPtr = SegBuff;
	NumSegs = 0;
}



/*
 * Report the syntax for calling xclock.
 */
Syntax(call)
	char *call;
{
	printf ("Usage: %s [-analog] [-bw <pixels>] [-digital]\n", call);
	printf ("       [-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>]\n");
	printf ("       [-fn <font_name>] [-help] [-padding <pixels>]\n");
	printf ("       [-rv] [-update <seconds>] [[<host>]:[<vs>]]\n");
	printf ("       [=[<width>][x<height>][<+-><xoff>[<+-><yoff>]]]\n\n");
	printf ("Default: %s -digital -bw %d -font %s -padding %d -update %d =-0-0\n\n",
		call, DEF_BORDER, DEF_DIGITAL_FONT, DEF_DIGITAL_PADDING, DEF_UPDATE);
	printf ("Default: %s -analog -bw %d -padding %d -update %d =%dx%d-0-0\n\n",
		call, DEF_BORDER, DEF_DIGITAL_PADDING, DEF_UPDATE,
		DEF_ANALOG_WIDTH, DEF_ANALOG_HEIGHT);
	printf ("Notes: The order in which switches are specified is not significant.\n");
	printf ("       In analog mode the second hand only appears for update times\n");
	printf ("       less than or equal to %d seconds.\n", SECOND_HAND_TIME);
	exit(0);
}


/*
 * XClockError - Fatal xclock error.
 */
XClockError (identifier)
	char *identifier;
{
	register Bool debug = XClockDebug;

	if (debug) printf("XClockError: Fatal xclock error encountered.\n");
	perror("xclock");
	fprintf(stderr, "xclock: %s\n", identifier);
	
	exit(1);
}

/*
 * End of xclock.c
 */
