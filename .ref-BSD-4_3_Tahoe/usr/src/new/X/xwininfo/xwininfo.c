#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

/*
 * xwininfo.c	- MIT Project Athena, X Window system window
 *		  information utility.
 *
 *	This program will report all relavent information
 *	about a specific window.
 *
 *  Author:	Tony Della Fera, DEC
 *		27-Nov-84
 */
#ifndef lint
static char *rcsid_xhost_c = "$Header: xwininfo.c,v 10.6 86/11/19 19:59:23 jg Rel $";
#endif

#include <X/Xlib.h>
#include <stdio.h>
#include "../cursors/target.cursor"
#include "../cursors/target_mask.cursor"
#include <strings.h>

typedef enum _bool {FALSE, TRUE} Bool;

#define MAX(a, b) (a) > (b) ? (a) : (b)
#define MIN(a, b) (a) < (b) ? (a) : (b)
#define ABS(a) (a) < 0 ? -(a) : (a)

#define FAILURE 0

char *index();

extern int errno;

main(argc, argv)
    int argc;
    char **argv;
{
    int mse_x, mse_y;
    int rel_mse_x, rel_mse_y;
    int w0, h0, w_inc, h_inc;
    int lr_rel_x, lr_rel_y;
    int root_w, root_h;
    int num_children;
    int status;
    char display[256];
    char *strind = NULL;
    char *win_name = NULL;
    char *id = NULL;
    char *win_fmt = " 0x%x\n";
    Bool children = FALSE;
    Bool root_switch = FALSE;
    register int i;

    Window target_win;
    Window temp_win;
    Window parent_win;
    Window *child_list;
    WindowInfo win_info;
    Cursor cursor;
    XButtonPressedEvent event;

    display[0] = '\0';
    
    for (i = 1; i < argc; i++) {
	strind = index(argv[i], ':');
	if (strind != NULL) {
	    (void) strncpy(display, argv[i], sizeof(display));
	    continue;
	}
	strind = index (argv [i], '-');
	if (strind == NULL) Syntax(argv[0]);
	if (strncmp(argv [i], "-children", 9) == 0) {
	    children = TRUE;
	    continue;
	}
	if (strncmp(argv [i], "-help", 5) == 0) {
	    Syntax(argv[0]);
	}
	if (strncmp(argv [i], "-id", 3) == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    id = argv[i];
	    continue;
	}
	if (strncmp(argv [i], "-int", 4) == 0) {
	    win_fmt = " %d\n";
	    continue;
	}
	if (strncmp(argv [i], "-root", 5) == 0) {
	    root_switch = TRUE;
	    continue;
	}
	Syntax(argv[0]);
    }
    
    if (XOpenDisplay(display) == NULL) {
 	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    argv[0], XDisplayName(display));
	    exit(1);
	  }

    /*
     * Store the target cursor incase we need it.
     */
    cursor = XCreateCursor(
    	target_width, target_height, 
    	target_bits, target_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	GXcopy
    );	
    if (cursor == FAILURE) {
	Error("Error occured while trying to store target cursor.");
    }

    /*
     * Depending on the state of the root and id switches, get the
     * target window from the user.
     */
    if (root_switch) {
	/*
	 * The root window selected on the command line.
	 */
	target_win = RootWindow;
    }
    else {
	if (id != NULL) {
	    /*
	     * A window id was provided on the command line.
	     */
	    (void) sscanf(id, "0x%x", &target_win);
	    if (target_win == 0) {
		/*
		 * Then the target was entered in decimal.
		 */
		(void) sscanf(id, "%d", &target_win);
		if (target_win == 0) {
		    Error("Invalid window id format.");
		}
	    }
	}
	else {
	    /*
	     * No selection was provided on the command line.
	     * Allow the user to select a window with the mouse.
	     */
	    status = XGrabMouse(RootWindow, cursor, ButtonPressed);
	    if (status == FAILURE) Error("Can't grab the mouse.");

	    printf("\n");
	    printf("xwininfo ==> Please select the window you wish\n");
	    printf("         ==> information on by clicking the\n");
	    printf("         ==> mouse in that window.\n");

	    XNextEvent(&event);
	    target_win = event.subwindow;

	    XUngrabMouse();

	    if (target_win == 0) {
		/*
		 * The user must have indicated the root window.
		 */
		target_win = RootWindow;
	    }
	}
    }
    
    status = XQueryMouse(RootWindow, &mse_x, &mse_y, &temp_win);
    if (status == FAILURE) Error("Can't query mouse on Root Window.");
    if (target_win == RootWindow){
	rel_mse_x = mse_x;
	rel_mse_y = mse_y;
    }
    else {
	status = XQueryMouse(target_win, &rel_mse_x, &rel_mse_y, &temp_win);
	if (status == FAILURE) Error("Can't query mouse on target window.");
    }

    status = XQueryWindow(RootWindow, &win_info);
    if (status == FAILURE) Error("Couldn't query root window.");    
    root_w = win_info.width;
    root_h = win_info.height;
    
    status = XQueryWindow(target_win, &win_info);
    if (status == FAILURE) Error("Couldn't query target window.");
    lr_rel_x = root_w - (win_info.x + win_info.width +
	(win_info.bdrwidth << 1));
    lr_rel_y = root_h - (win_info.y + win_info.height +
	(win_info.bdrwidth << 1));

    status = XFetchName(target_win, &win_name);
    if (status == FAILURE) Error("Can't fetch window name.");

    status = XQueryTree(target_win, &parent_win, &num_children, &child_list);
    if (status == FAILURE) Error("Can't query window tree.");

    (void) XGetResizeHint(target_win, &w0, &h0, &w_inc, &h_inc);

    printf("\nxwininfo ==> Window name: '%s'\n", win_name);
    printf("         ==> Window id:");
    printf(win_fmt, target_win);
    printf("         ==> Parent window id:");
    printf(win_fmt, parent_win);
    printf("         ==> Number of children: %d\n", num_children);
    if (children) {
        for (i = num_children - 1; i >= 0; i--) {
            printf("             ==> Child window id:"); 
	    printf(win_fmt, child_list[i]);
        }
    }
    printf("         ==> Associated window id:");
    printf(win_fmt, win_info.assoc_wind);
    printf("         ==> Window type: ");
    switch (win_info.type) {
        case IsTransparent:
            printf("IsTransparent\n");
            break;
        case IsOpaque:
            printf("IsOpaque\n");
            break;
        case IsIcon:
            printf("IsIcon\n");
            break;
        default:
            printf("Unknown %d?\n", win_info.type);
            break;
    }
    printf("         ==> Window state: ");
    switch (win_info.mapped) {
        case IsMapped:
            printf("IsMapped\n");
            break;
        case IsUnmapped:
            printf("IsUnmapped\n");
            break;
        case IsInvisible:
            printf("IsInvisible\n");
            break;
        default:
            printf("Unknown %d?\n", win_info.mapped);
            break;
    }
    printf("         ==> Upper left X: %d\n", win_info.x);
    printf("         ==> Upper left Y: %d\n", win_info.y);
    printf("         ==> Width: %d\n", win_info.width);
    printf("         ==> Height: %d\n", win_info.height);
    printf("         ==> Border width: %d\n", win_info.bdrwidth);
    printf("         ==> Geometry specification:\n");
    printf("             ==> Upper left  =%dx%d+%d+%d\n",
	win_info.width, win_info.height, win_info.x, win_info.y);
    printf("             ==> Lower right =%dx%d-%d-%d\n",
	win_info.width, win_info.height, lr_rel_x, lr_rel_y);
    printf("         ==> Resize base width: %d\n", w0);
    printf("         ==> Resize base height: %d\n", h0);
    printf("         ==> Resize width increment: %d\n", w_inc);
    printf("         ==> Resize height increment: %d\n", h_inc);
    printf("         ==> Root absolute mouse X Position: %d\n", mse_x);
    printf("         ==> Root absolute mouse Y Position: %d\n", mse_y);
    printf("         ==> Target relative mouse X Position: %d\n", rel_mse_x);
    printf("         ==> Target relative mouse Y Position: %d\n", rel_mse_y);
    printf("\n");

    exit(0);
}


/*
 * Report the syntax for calling xwininfo.
 */
Syntax(call)
    char *call;
{
    fprintf(stderr, "\n");
    fprintf(stderr, "Usage: %s [-children] [-help] [-id <id>] [-int] ", call);
    fprintf(stderr, "[-root] [[host]:vs]\n\n");
    exit(0);
}


/*
 * Error - Fatal xwininfo error.
 */
Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "\nxwininfo: %s", string);
	fprintf(stderr, "\n\n");

	if (errno != 0) {
		perror("xwininfo");
		fprintf(stderr, "\n");
	}

	exit(1);
}
