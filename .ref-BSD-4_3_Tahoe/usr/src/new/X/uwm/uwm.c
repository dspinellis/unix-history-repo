#ifndef lint
static char *rcsid_uwm_c = "$Header: uwm.c,v 10.7 86/11/19 19:03:58 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)uwm.c	3.8	1/24/86";
#endif

#include <sys/time.h>
#include "uwm.h"

#ifdef PROFIL
#include <signal.h>
/*
 * Dummy handler for profiling.
 */
ptrap()
{
    exit(0);
}
#endif

#include <fcntl.h>

static short gray_bits[16] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555
};

Bool ChkMline();
char *sfilename;
extern FILE *yyin;

/*
 * Main program.
 */
main(argc, argv, environ)
int argc;
char **argv;
char **environ;
{
    short hi;			/* Button event high detail. */
    short lo;			/* Button event low detail. */
    int x, y;                   /* Mouse X and Y coordinates. */
    int cur_x, cur_y;		/* Current mouse X and Y coordinates. */
    int str_width;              /* Width in pixels of output string. */
    int pop_width, pop_height;  /* Pop up window width and height. */
    int context;		/* Root, window, or icon context. */
    Bool func_stat;		/* If true, function swallowed a ButtonUp. */
    Bool delta_done;		/* If true, then delta functions are done. */
    register Binding *bptr;	/* Pointer to Bindings list. */
    char *root_name;		/* Root window name. */
    char *display = NULL;	/* Display name pointer. */
    char message[128];		/* Error message buffer. */
    char *rc_file;		/* Pointer to $HOME/.uwmrc. */
    Bitmap gray_bitmap;		/* Gray bitmap used for gray pixmap. */
    Display *dpy;		/* Display info pointer. */
    Window event_win;           /* Event window. */
    Window sub_win;		/* Subwindow for XUpdateMouse calls. */
    WindowInfo root_info;	/* Root window info. */
    WindowInfo event_info;	/* Event window info. */
    XButtonEvent button_event;  /* Button input event. */
    char *malloc();


#ifdef PROFIL
    signal(SIGTERM, ptrap);
#endif

    /*
     * Set up internal defaults.
     */
    strcpy(IFontName, DEF_FONT);
    strcpy(PFontName, DEF_FONT);
    strcpy(MFontName, DEF_FONT);
    CursorFunc = DEF_FUNC;
    Delta = DEF_DELTA;
    IBorderWidth = DEF_ICON_BORDER_WIDTH;
    HIconPad = DEF_ICON_PADDING;
    VIconPad = DEF_ICON_PADDING;
    PBorderWidth = DEF_POP_BORDER_WIDTH;
    PPadding = DEF_POP_PADDING;
    MBorderWidth = DEF_MENU_BORDER_WIDTH;
    HMenuPad = DEF_MENU_PADDING;
    VMenuPad = DEF_MENU_PADDING;
    Volume = DEF_VOLUME;

    /*
     * Set XErrorFunction to be non-terminating.
     */
    XErrorHandler(XError);

    /* 
     * Parse the command line arguments.
     */
    Argv = argv;
    Environ = environ;
    argc--, argv++;
    while (argc) {
        if (!(strcmp(*argv, "-f"))) {
            argc--, argv++;
            if ((argc == 0) || (Startup_File[0] != '\0'))
                Usage();
            strncpy(Startup_File, *argv, NAME_LEN);
        }
        else display = *argv;
	argc--, argv++;
    }

    /*
     * Initialize the default bindings.
     */
    InitBindings();

    /*
     * Read in and parse $HOME/.uwmrc, if it exists.
     */
    sfilename = rc_file = malloc(NAME_LEN);
    sprintf(rc_file, "%s/.uwmrc", getenv("HOME"));
    if ((yyin = fopen(rc_file, "r")) != NULL) {
        Lineno = 1;
        yyparse();
        fclose(yyin);
        if (Startup_File_Error)
            Error("Bad .uwmrc file...aborting");
    }

    /* 
     * Read in and parse the startup file from the command line, if
     * specified.
     */
    if (Startup_File[0] != '\0') {
        sfilename = Startup_File;
        if ((yyin = fopen(Startup_File, "r")) == NULL) {
    	sprintf(message, "Cannot open startup file '%s'", Startup_File);
            Error(message);
        }
        Lineno = 1;
        yyparse();
        fclose(yyin);
        if (Startup_File_Error)
            Error("Bad startup file...aborting");
    }

    /*
     * Verify the menu bindings.
     */
    VerifyMenuBindings();
    if (Startup_File_Error)
        Error("Bad startup file...aborting");

    /* 
     * Open the display.
     */
    if ((dpy = XOpenDisplay(display)) == NULL) {
	fprintf(stderr, "%s: Can't open display '%s'\n",
		Argv[0], XDisplayName(display));
		exit(1);
      }

    /*
     * Force child processes to disinherit the TCP file descriptor.
     * This helps shell commands forked and exec'ed from menus
     * to work properly.
     */
    if ((status = fcntl(dpyno(), F_SETFD, 1)) == -1) {
        perror("uwm: child cannot disinherit TCP fd");
        Error("TCP file descriptor problems");
    }

    /*
     * If the root window has not been named, name it.
     */
    status = XFetchName(RootWindow, &root_name);
    if (status == FAILURE) Error("Can't fetch Root Window name string");
    if (root_name == NULL) XStoreName(RootWindow, " X Root Window ");
    if (root_name) free(root_name);

    /*
     * Gather information about the root window.
     */
    status = XQueryWindow(RootWindow, &root_info);
    if (status == FAILURE)
        Error("Can't acquire root window information from X server");

    ScreenHeight = root_info.height;	/* True height of entire screen */
    ScreenWidth = root_info.width;	/* True width of entire screen */

    /*
     * Create and store the icon background pixmap.
     */
    gray_bitmap = XStoreBitmap(16, 16, gray_bits);
    GrayPixmap = XMakePixmap(gray_bitmap, BlackPixel, WhitePixel);

    /*
     * Set up icon window, icon cursor and pop-up window color parameters.
     */
    if (Reverse) {
        IconCursorFunc = GXcopyInverted;
        IBorder = WhitePixmap;
        IBackground = GrayPixmap;
        ITextForground = WhitePixel;
        ITextBackground = BlackPixel;
        PBorder = BlackPixmap;
        PBackground = WhitePixmap;
        PTextForground = BlackPixel;
        PTextBackground = WhitePixel;
        MBorder = WhitePixmap;
        MBackground = BlackPixmap;
        MTextForground = WhitePixel;
        MTextBackground = BlackPixel;
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
        MBorder = BlackPixmap;
        MBackground = WhitePixmap;
        MTextForground = BlackPixel;
        MTextBackground = WhitePixel;
    }

    /*
     * Store all the cursors.
     */
    StoreCursors();

    /* 
     * grab the mouse buttons according to the map structure
     */
    Grab_Buttons();

    /*
     * Load the selected fonts.
     */
    IFont = XGetFont(IFontName);
    if (IFont == FAILURE) {
        sprintf(message, "Unable to get font '%s'.", IFontName);
        Error(message);
    }
    PFont = XGetFont(PFontName);
    if (PFont == FAILURE) {
        sprintf(message, "Unable to get font '%s'.", PFontName);
        Error(message);
    }
    MFont = XGetFont(MFontName);
    if (MFont == FAILURE) {
        sprintf(message, "Unable to get font '%s'.", MFontName);
        Error(message);
    }

    /*
     * Retrieve the information structure for the specifed fonts and
     * set the global font information pointers.
     */
    status = XQueryFont(IFont, &IFontInfo);
    if (status == FAILURE) {
        sprintf(message, "Unable to query X server for info on font '%s'.",
                IFontName);
        Error(message);
    }
    status = XQueryFont(PFont, &PFontInfo);
    if (status == FAILURE) {
        sprintf(message, "Unable to query X server for info on font '%s'.",
                PFontName);
        Error(message);
    }
    status = XQueryFont(MFont, &MFontInfo);
    if (status == FAILURE) {
        sprintf(message, "Unable to query X server for info on font '%s'.",
                MFontName);
        Error(message);
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
     * Create the pop-up window.  Create it at (0, 0) for now.  We will
     * move it where we want later.
     */
    Pop = XCreateWindow(RootWindow,
                        0, 0,
                        pop_width, pop_height,
                        PBorderWidth,
                        PBorder, PBackground);
    if (Pop == FAILURE) Error("Can't create pop-up dimension display window.");

    /*
     * Create the menus for later use.
     */
    CreateMenus();

    /*
     * Tell the user we're alive and well.
     */
    XFeep(Volume);

    /* 
     * Main command loop.
     */
    while (TRUE) {

        delta_done = func_stat = FALSE;

        /*
         * Get the next mouse button event.  Spin our wheels until
         * a ButtonPressed event is returned.
         * Note that mouse events within an icon window are handled
         * in the "GetButton" function or by the icon's owner if
         * it is not uwm.
         */
        while (TRUE) {
            if (!GetButton(&button_event)) continue;
            if (button_event.type == ButtonPressed) break;
        }

        /*
         * Okay, determine the event window and mouse coordinates.
         */
        status = XInterpretLocator(RootWindow,
                                    &x, &y,
                                    &event_win,
                                    button_event.location);

        if (status == FAILURE) continue;

        /*
         * Determine the event window and context.
         */
        if (event_win == 0) {
                event_win = RootWindow;
                context = ROOT;
        } else {
            status = XQueryWindow(event_win, &event_info);
            if (status == FAILURE) continue;
            if (event_info.type & IsIcon)
                context = ICON;
            else context = WINDOW;
        }

        /*
         * Get the button event detail.
         */
        lo = (button_event.detail & ValueMask);
        hi = KeyMask(button_event.detail);

        /*
         * Determine which function was selected and invoke it.
         */
        for(bptr = Blist; bptr; bptr = bptr->next) {

            if ((bptr->button != lo) ||
                (KeyMask(bptr->mask) != hi))
                continue;

            if (bptr->context != context)
                continue;

            if (!(bptr->mask & ButtonDown))
                continue;

            /*
             * Found a match! Invoke the function.
             */
            if ((*bptr->func)(event_win,
                              (int)bptr->mask & ~ButtonMods,
                              bptr->button,
                              x, y,
                              bptr->menu)) {
                func_stat = TRUE;
                break;
            }
        }

        /*
         * If the function ate the ButtonUp event, then restart the loop.
         */
        if (func_stat) continue;

        while(TRUE) {
            /*
             * Wait for the next button event.
             */
            if (XPending() && GetButton(&button_event)) {
    
                /*
                 * If it's not a release of the same button that was pressed,
                 * don't do the function bound to 'ButtonUp'.
                 */
                if (button_event.type != ButtonReleased)
                    break;
                if (lo != (button_event.detail & ValueMask))
                    break;
                if (hi != KeyMask(button_event.detail))
                    break;
        
                /*
                 * Okay, determine the event window and mouse coordinates.
                 */
                status = XInterpretLocator(RootWindow,
                                           &x, &y,
                                           &event_win,
                                           button_event.location);

                if (status == FAILURE) break;

                if (event_win == 0) {
                        event_win = RootWindow;
                        context = ROOT;
                } else {
                    status = XQueryWindow(event_win, &event_info);
                    if (status == FAILURE) break;
                    if (event_info.type & IsIcon)
                        context = ICON;
                    else context = WINDOW;
                }
        
                /*
                 * Determine which function was selected and invoke it.
                 */
                for(bptr = Blist; bptr; bptr = bptr->next) {
        
                    if ((bptr->button != lo) ||
                        (KeyMask(bptr->mask) != hi))
                        continue;
        
                    if (bptr->context != context)
                        continue;
        
                    if (!(bptr->mask & ButtonUp))
                        continue;
        
                    /*
                     * Found a match! Invoke the function.
                     */
                    (*bptr->func)(event_win,
                                  (int)bptr->mask & ~ButtonMods,
                                  bptr->button,
                                  x, y,
                                  bptr->menu);
                }
                break;
            }
    
            XUpdateMouse(RootWindow, &cur_x, &cur_y, &sub_win);
            if (!delta_done &&
                ((abs(cur_x - x) > Delta) || (abs(cur_y - y) > Delta))) {
                /*
                 * Delta functions are done once (and only once.)
                 */
                delta_done = TRUE;

                /*
                 * Determine the new event window's coordinates.
                 */
                status = XInterpretLocator(RootWindow,
                                            &x, &y,
                                            &event_win,
                                            button_event.location);
                if (status == FAILURE) break;

                /*
                 * Determine the event window and context.
                 */
                if (event_win == 0) {
                        event_win = RootWindow;
                        context = ROOT;
                } else {
                    status = XQueryWindow(event_win, &event_info);
                    if (status == FAILURE) break;
                    if (event_info.type & IsIcon)
                        context = ICON;
                    else context = WINDOW;
                }
    
                /*
                 * Determine which function was selected and invoke it.
                 */
                for(bptr = Blist; bptr; bptr = bptr->next) {
        
                    if ((bptr->button != lo) ||
                        (KeyMask(bptr->mask) != hi))
                        continue;
        
                    if (bptr->context != context)
                        continue;
        
                    if (!(bptr->mask & DeltaMotion))
                        continue;
        
                    /*
                     * Found a match! Invoke the function.
                     */
                    if ((*bptr->func)(event_win,
                                      (int)bptr->mask & ~ButtonMods,
                                      bptr->button,
                                      x, y,
                                      bptr->menu)) {
                        func_stat = TRUE;
                        break;
                    }
                }
                /*
                 * If the function ate the ButtonUp event,
                 * then restart the loop.
                 */
                if (func_stat) break;
            }
        }
    }
}

/*
 * Initialize the default bindings.  First, write the character array
 * out to a temp file, then point the parser to it and read it in.
 * Afterwards, we unlink the temp file.
 */
InitBindings()
{
    char *mktemp();
    char *tempfile = TEMPFILE;	/* Temporary filename. */
    register FILE *fp;		/* Temporary file pointer. */
    register char **ptr;	/* Default bindings string array pointer. */

    /*
     * Create and write the temp file.
     */
    sfilename = mktemp(tempfile);
    if ((fp = fopen(tempfile, "w")) == NULL) {
        perror("uwm: cannot create temp file");
        exit(1);
    }
    for (ptr = DefaultBindings; *ptr; ptr++) {
        fputs(*ptr, fp);
        fputc('\n', fp);
    }
    fclose(fp);

    /*
     * Read in the bindings from the temp file and parse them.
     */
    if ((yyin = fopen(tempfile, "r")) == NULL) {
        perror("uwm: cannot open temp file");
        exit(1);
    }
    Lineno = 1;
    yyparse();
    fclose(yyin);
    unlink(tempfile);
    if (Startup_File_Error)
        Error("Bad default bindings...aborting");

    /*
     * Parse the system startup file, if one exists.
     */
    if ((yyin = fopen(SYSFILE, "r")) != NULL) {
        sfilename = SYSFILE;
        Lineno = 1;
        yyparse();
        fclose(yyin);
        if (Startup_File_Error)
            Error("Bad system startup file...aborting");
    }
}

/*
 * Verify menu bindings by checking that a menu that is mapped actually
 * exists.  Stash a pointer in the binding to the relevant menu info data
 * structure.
 * Check nested menu consistency.
 */
VerifyMenuBindings()
{
    Binding *bptr;
    MenuLink *mptr;

    for(bptr = Blist; bptr; bptr = bptr->next) {
        if (bptr->func == Menu) {
            for(mptr = Menus; mptr; mptr = mptr->next) {
                if(!(strcmp(bptr->menuname, mptr->menu->name))) {
                    bptr->menu = mptr->menu;
                    break;
                }
            }
            if (mptr == NULL) {
                fprintf(stderr,
                        "uwm: non-existent menu reference: \"%s\"\n",
                        bptr->menuname);
                Startup_File_Error = TRUE;
            }
        }
    }
    CheckMenus();
}

/*
 * Check nested menu consistency by verifying that every menu line that
 * calls another menu references a menu that actually exists.
 */
CheckMenus()
{
    MenuLink *ptr;
    Bool errflag = FALSE;

    for(ptr = Menus; ptr; ptr = ptr->next) {
        if (ChkMline(ptr->menu))
            errflag = TRUE;
    }
    if (errflag)
        Error("Nested menu inconsistency");
}

Bool ChkMline(menu)
MenuInfo *menu;
{
    MenuLine *ptr;
    MenuLink *lptr;
    Bool errflag = FALSE;

    for(ptr = menu->line; ptr; ptr = ptr->next) {
        if (ptr->type == IsMenuFunction) {
            for(lptr = Menus; lptr; lptr = lptr->next) {
                if(!(strcmp(ptr->text, lptr->menu->name))) {
                    ptr->menu = lptr->menu;
                    break;
                }
            }
            if (lptr == NULL) {
                fprintf(stderr,
                        "uwm: non-existent menu reference: \"%s\"\n",
                        ptr->text);
                errflag = TRUE;
            }
        }
    }
    return(errflag);
}

/*
 * Grab the mouse buttons according to the bindings list.
 */
Grab_Buttons()
{
    Binding *bptr;

    for(bptr = Blist; bptr; bptr = bptr->next)
        Grab(bptr->mask);
}

/*
 * Grab a mouse button according to the given mask.
 */
Grab(mask)
short mask;
{
    short m = LeftMask | MiddleMask | RightMask;

    switch (mask & m) {
    case LeftMask:
        status = XGrabButton(RootWindow, LeftButtonCursor,
                             mask & ~ButtonMods,
                             EVENTMASK);
        if (status == FAILURE)
            Error("Can't grab left mouse button.");
        break;

    case MiddleMask:
        status = XGrabButton(RootWindow, MiddleButtonCursor,
                             mask & ~ButtonMods,
                             EVENTMASK);
        if (status == FAILURE)
            Error("Can't grab middle mouse button.");
        break;

    case RightMask:
        status = XGrabButton(RootWindow, RightButtonCursor,
                             mask & ~ButtonMods,
                             EVENTMASK);
        if (status == FAILURE)
            Error("Can't grab right mouse button.");
        break;
    }
}

/*
 * error routine for .uwmrc parser
 */
yyerror(s)
char*s;
{
    fprintf(stderr, "uwm: %s: %d: %s\n", sfilename, Lineno, s);
    Startup_File_Error = TRUE;
}

/*
 * Print usage message and quit.
 */
Usage()
{
    fputs("Usage:  uwm [-f <file>] [<host>:<display>]\n", stderr);
    exit(1);
}

/*
 * error handler for X I/O errors
 */
XIOError(dsp)
Display *dsp;
{
    perror("uwm");
    exit(3);
}
