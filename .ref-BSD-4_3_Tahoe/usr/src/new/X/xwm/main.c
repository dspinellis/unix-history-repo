#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985 */

/*
 *	xwm - X Window System window manager main routine.
 *
 */

#include "xwm.h"

#ifdef PROFIL
#include <signal.h>

#ifndef lint
static char *rcsid_main_c = "$Header: main.c,v 10.7 86/11/19 20:01:48 jg Rel $";
#endif
/*
 * Dummy handler for profiling.
 */
ptrap()
{
    exit(0);
}
#endif

static short gray_bits[16] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555
};

main(argc, argv)
    int argc;			/* Argument count. */
    char **argv;		/* Argument vector. */
{
    register int i;		/* Loop index. */
    register int status;	/* Routine call return status. */
    register char *arg;		/* Current argument pointer. */
    int x, y;			/* Mouse X and Y coordinates. */
    int str_width;		/* Width in pixels of output string. */
    int pop_width, pop_height;	/* Pop up window width and height. */
    int temp_button_mask = 0;	/* Temporary button event mask. */
    char *def_val;		/* X Default value. */
    char *i_font_name;		/* Icon font name. */
    char *p_font_name;		/* Pop up font name. */
    char display[256];		/* Display identifier string. */
    char message[128];		/* Error message buffer. */
    Bitmap gray_bitmap;		/* Gray bitmap used for gray pixmap. */
    Window event_win;		/* Event window. */
    Window focus_win;		/* Keyboard focus window. */
    WindowInfo root_info;	/* Root window info. */
    XButtonEvent button_event;	/* Button input event. */
    Bool focus_seq = FALSE;	/* Has a focus sequence begun? */
    Bool changed = FALSE;	/* Has the window changed? */
    Bool none = FALSE;		/* Allow the mouse with no keys. */
    Bool focus = FALSE;		/* Allow input focusing? */
    Bool reverse = FALSE;	/* Reverse video? */

#ifdef PROFIL
    signal(SIGTERM, ptrap);
#endif

    /*
     * Set up internal defaults.
     */
    i_font_name = DEF_I_FONT;
    p_font_name = DEF_P_FONT;
    CursorFunc = DEF_FUNC;
    ButtonMask = DEF_BUTTON_MASK;
    Delta = DEF_DELTA;
    IBorderWidth = DEF_ICON_BORDER_WIDTH;
    IPadding = DEF_ICON_PADDING;
    PBorderWidth = DEF_POP_BORDER_WIDTH;
    PPadding = DEF_POP_PADDING;

    /*
     * Initialize fixed globals.
     */
    Grid = FALSE;
    Zap = FALSE;

    /*
     * Set XErrorFunction to be non-terminating.
     */
    XErrorHandler(XError);

    /*
     * Check for X defaults.
     */
    def_val = XGetDefault(argv[0], "IconFont");
    if (def_val != NULL) i_font_name = def_val;

    def_val = XGetDefault(argv[0], "BodyFont");
    if (def_val != NULL) p_font_name = def_val;

    def_val = XGetDefault(argv[0], "InternalBorder");
    if (def_val != NULL) {
	IPadding = atoi(def_val);
	PPadding = atoi(def_val);
    }

    def_val = XGetDefault(argv[0], "BorderWidth");
    if (def_val != NULL) {
	IBorderWidth = atoi(def_val);
	PBorderWidth = atoi(def_val);
    }

    def_val = XGetDefault(argv[0], "ReverseVideo");
    if (def_val != NULL) {
	if (strcmp (def_val, "on") == 0) reverse = TRUE;
    }

    /*
     * Parse the command line arguments.
     */
    for (i = 1; i < argc; i++) {
	arg = argv[i];
	switch (*arg) {
	case '\0':
	    continue;
	case '-':
	    arg++;
	    if (*arg == '\0') break;
	    for (; *arg; arg++) {
		switch (*arg) {
		    case 'c':
			/*
			 * Add the control key to the mouse button mask.
			 */
			temp_button_mask |= ControlMask;
			break;
		    case 'd':
			/*
			 * Check for a debug flag.
			 */
			 Debug = TRUE;
			 break;
		    case 's':
			/*
			 * Add the shift key to the mouse button mask.
			 */
			temp_button_mask |= ShiftMask;
			break;
		    case 'm':
			/*
			 * Add the meta key to the mouse button mask.
			 */
			temp_button_mask |= MetaMask;
			break;
		    case 'n':
			/*
			 * No keys are needed with the mouse.
			 */
			none = TRUE;
			break;
		    case 'f':
			/*
			 * Require double clicking to focus input.
			 */
			focus = TRUE;
			break;
		    case 'g':
			/*
			 * Display the tic tac toe grid on window change.
			 */
		    	Grid = TRUE;
			break;
		    case 'r':
			/*
			 * Make icons and pop-ups reverse video.
			 */
			reverse = TRUE;
			break;
		    case 'z':
			/*
			 * Use zap effect?
			 */
			Zap = TRUE;
			break;
		}
	    }
	    break;
	case '+':
	    CursorFunc = atoi(arg + 1);
	    if (CursorFunc <= 0 || CursorFunc > 15) {
		/*
		 * Oops, cursor function code out of range!
		 */
		errno = EDOM;
		sprintf(
		    message,
		    "Cursor function code '%d' out of range (0 - 14).",
		    CursorFunc
		);
		Error(message);
	    }
	    break;
	case '@':
	    Delta = atoi(arg + 1);
	    if (Delta <= 0 || Delta > 100) {
		/*
		 * Oops, delta value out of range!
		 */
		errno = EDOM;
		sprintf(
		    message,
		    "Delta value '%d' out of range (1 - 99).",
		    Delta
		);
		Error(message);
	    }
	    break;
	case 'f':
	    if ((arg[1] == 'n') && (arg[2] == '=')) {
		p_font_name = arg + 3;
	    }
	    else if ((arg[1] == 'i') && (arg[2] == '=')) {
		i_font_name = arg + 3;
	    }
	    break;
	default:
	    /*
	     * All that is left is a possible display string.
	     */
	    strcpy(display, arg);
	}
    }

    /*
     * Set the global mouse button event mask.
     */

    if (temp_button_mask) ButtonMask = temp_button_mask;

    if (none) ButtonMask = 0;

    /*
     * Open the Display.
     */
    if (XOpenDisplay(display) == NULL) {
	/*
	 * Oops, can't open the display!
	 */
        fprintf(stderr, "%s: Can't open display '%s'\n",
	    argv[0], XDisplayName(display));
        exit(1);
    }

    /*
     * Gather information about the root window.
     */
    status = XQueryWindow(RootWindow, &root_info);
    if (status == FAILURE) {
	Error("Can't acquire root window information from X server.");
    }

    ScreenHeight = root_info.height;	/* True height of entire screen. */
    ScreenWidth = root_info.width;	/* True width of entire screen. */

    /*
     * Create and store the icon background pixmap.
     */
    gray_bitmap = XStoreBitmap(16, 16, gray_bits);
    GrayPixmap = XMakePixmap(gray_bitmap, BlackPixel, WhitePixel);


    /*
     * Set up icon window, icon cursor and pop-up window color parameters.
     */
    if (reverse) {
	IconCursorFunc = GXcopyInverted;
	IBorder = WhitePixmap;
	IBackground = GrayPixmap;
	ITextForground = WhitePixel;
	ITextBackground = BlackPixel;
	PBorder = BlackPixmap;
	PBackground = WhitePixmap;
	PTextForground = BlackPixel;
	PTextBackground = WhitePixel;
    } 
    else {
	IconCursorFunc = GXcopy;
	IBorder = BlackPixmap;
	IBackground = GrayPixmap;
	ITextForground = BlackPixel;
	ITextBackground = WhitePixel;
	PBorder = WhitePixmap;
	PBackground = BlackPixmap;
	PTextForground = WhitePixel;
	PTextBackground = BlackPixel;
    }

    /*
     * Store all the cursors.
     */
    StoreCursors();

    /*
     * Grab all 3 mouse buttons w/ respect to the root window.  Grab
     * pressed status with the mouse button mask.
     */
    status =  XGrabButton(
	RootWindow, 
	DotCursor,
	(LeftMask | ButtonMask),
    	(ButtonPressed | ButtonReleased)
    );
    if (status == FAILURE) Error("Can't grab left mouse button.");
    status = XGrabButton(
	RootWindow,
	ArrowCrossCursor,
	(MiddleMask | ButtonMask),
    	(ButtonPressed | ButtonReleased)
    );
    if (status == FAILURE) Error("Can't grab middle mouse button.");
    status = XGrabButton(
	RootWindow, 
	CircleCursor,
	(RightMask | ButtonMask),
    	(ButtonPressed | ButtonReleased)
    );
    if (status == FAILURE) Error("Can't grab right mouse button.");

    /*
     * Load the selected fonts and retrieve the information structure
     * for each.  Set global font information pointers.
     */
    IFont = XGetFont(i_font_name);
    if (IFont == FAILURE) {
	sprintf(message, "Unable to get icon font '%s'.", i_font_name);
	Error(message);
    }

    status = XQueryFont(IFont, &IFontInfo);
    if (status == FAILURE) {
	Error("Unable to query X server for icon font information.");
    }

    PFont = XGetFont(p_font_name);
    if (PFont == FAILURE) {
	sprintf(message, "Unable to get pop up font '%s'.", p_font_name);
	Error(message);
    }

    status = XQueryFont(PFont, &PFontInfo);
    if (status == FAILURE) {
	Error("Unable to query X server for pop up font information.");
    }

    /*
     * Calculate size of the resize pop-up window.
     */
    str_width = XQueryWidth(PText, PFont);
    pop_width = str_width + (PPadding << 1);
    PWidth = pop_width + (PBorderWidth << 1);
    pop_height = PFontInfo.height + (PPadding << 1);
    PHeight = pop_height + (PBorderWidth << 1);

    /*
     * Create the pop-up window.  Create it at (0, 0) for now, we will
     * move it where we want later.
     */
    Pop = XCreateWindow(
	RootWindow,
	0, 0,
	pop_width, pop_height,
	PBorderWidth,
	PBorder, PBackground
    );
    if (Pop == FAILURE) Error("Can't open pop-up dimension display window.");

    /*
     * Main command loop.
     */
    while (TRUE) {
	/*
	 * Get the next mouse button event.  Spin our wheels until
	 * a button event is returned (ie. GetButton == TRUE).
	 * Note that mouse events within an icon window are handled
	 * in the "GetButton" function or by the icon's owner if
	 * it is not xwm.
	 */
	if (!GetButton(&button_event)) continue;

	/*
	 * If the button event recieved is not a ButtonPressed event
	 * then continue until we find one.
	 */
	if (button_event.type != ButtonPressed) continue;
	
	/*
	 * Ok, determine the event window and mouse coordinates.
	 */
	status = XInterpretLocator(
		RootWindow,
		&x, &y,
		&event_win,
		button_event.location
	);
	if (status == FAILURE) continue;

	/*
	 * If the event subwindow is 0 then the event
	 * occured on the root window.
	 */
	if (event_win == 0) {
		event_win = RootWindow;
	}
	
	/*
	 * Invoke a function based on which button was pressed.
	 */
	switch (button_event.detail & ValueMask) {
	    case LeftButton:
		/*
		 * LeftDown is used to lower or iconify a window if
		 * the event window is not the root window.  If it is the
		 * RoowWindow then circulate all windows down.
		 */

		/*
		 * Abort any focus sequence that is in progress.
		 */
		focus_seq = FALSE;

		if (event_win == RootWindow) {
		    XCircWindowDown(RootWindow);
		}
		else {
		    LowerIconify(event_win, x, y);
		}
		break;

	    case MiddleButton:
		/*
		 * MiddleDown is used to resize a window and establish the
		 * focus window.
		 */

		/*
		 * If this is not the root window, go ahead and allow it
		 * to be changed.
		 */
		changed = FALSE;
		if (event_win != RootWindow) {
		    changed = Change(event_win, x, y);
		}

		if (focus) {
		    /*
		     * Two middle clicks will focus the keyboard...
		     */
		    if (focus_seq) {
			/*
			 * ... and this is the second ...
			 */
			if (focus_win == event_win) {
			    /*
			     * ... and both have the same event window then
			     * focus the keyboard provided the window did not
			     * change.  This also ends the focus sequence.
			     */
			    if (!changed) XFocusKeyboard(event_win);
			    focus_seq = FALSE;
			    focus_win = RootWindow;
			}
			else {
			    /*
			     * ... both don't have the same window.  This
			     * ends the focus sequence.
			     */
			    focus_seq = FALSE;
			    focus_win = RootWindow;
			}
		    }
		    else {
			/*
			 * Begin a focus sequence, salt away the 
			 * perspective focus window.
			 */
			focus_seq = TRUE;
			focus_win = event_win;
		    }
		}
		break;

	    case RightButton:
		/*
		 * RightDown is used to move a window or bring it to the
		 * top of the window stack if the event window is not
		 * the root window.  If it is the root window then circulate
		 * all windows up.
		 */

		/*
		 * Abort any focus sequence that is in progress.
		 */
		focus_seq = FALSE;

		if (event_win == RootWindow) {
		    XCircWindowUp(RootWindow);
		}
		else {
		    Move(event_win, x, y);
		}
		break;

	}
    }
}
