#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

/*
 * xwd.c MIT Project Athena, X Window system window raster image dumper.
 *
 * This program will dump a raster image of the contents of a window into a 
 * file for output on graphics printers or for other uses.
 *
 *  Author:	Tony Della Fera, DEC
 *		17-Jun-85
 */

#ifndef lint
static char *rcsid_xwd_c = "$Header: xwd.c,v 10.6 86/02/01 16:07:54 tony Rel $";
#endif

#include <X/Xlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <strings.h>

char *calloc();

typedef enum _bool {FALSE, TRUE} Bool;

#include "../cursors/target.cursor"
#include "../cursors/target_mask.cursor"

#include "XWDFile.h"

#define MAX(a, b) (a) > (b) ? (a) : (b)
#define MIN(a, b) (a) < (b) ? (a) : (b)
#define ABS(a) (a) < 0 ? -(a) : (a)

#define FAILURE 0

#define FEEP_VOLUME 0

extern int errno;

main(argc, argv)
    int argc;
    char **argv;
{
    register int i;
    int status;
    unsigned buffer_size;
    int virt_x, virt_y;
    int virt_width, virt_height;
    int pixmap_format = XYFormat;
    int win_name_size;
    int header_size;
    char *str_index;
    char *file_name;
    char display[256];
    char *win_name;
    char *buffer;
    Bool nobdrs = FALSE;
    Bool debug = FALSE;
    Bool standard_out = TRUE;

    Display *dpy;
    Window target_win;
    Window image_win;
    WindowInfo win_info;
    Cursor cursor;
    XButtonEvent rep;

    XWDFileHeader header;

    FILE *out_file = stdout;

    for (i = 1; i < argc; i++) {
	str_index = (char *)index (argv[i], ':');
	if(str_index != NULL) {
	    (void) strncpy(display,argv[i],sizeof(display));
	    continue;
        }
	str_index = (char *) index (argv [i], '-');
	if (str_index == NULL) Syntax(argv[0]);
	if (strncmp(argv[i], "-nobdrs", 6) == 0) {
	    nobdrs = TRUE;
	    continue;
	}
	if (strncmp(argv[i], "-debug", 6) == 0) {
	    debug = TRUE;
	    continue;
	}
	if (strncmp(argv[i], "-help", 5) == 0) {
	    Syntax(argv[0]);
	}
	if (strncmp(argv[i], "-out", 4) == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    file_name = argv[i];
	    standard_out = FALSE;
	    continue;
	}
	if (strncmp(argv[i], "-z", 2) == 0) {
	    pixmap_format = ZFormat;
	    continue;
	}
	Syntax(argv[0]);
    }
    
    if (!standard_out) {
	/*
	 * Open the output file.
	 */
	out_file = fopen(file_name, "w");
	if (out_file == NULL) {
	    Error("Can't open output file as specified.");
	}
    }

    /*
     * Open the display.
     */
    if (debug) printf("xwd: Opening display.\n");
    if ((dpy = XOpenDisplay(display)) == NULL) {
	Error("Error occured while trying open display.");
    }

    /*
     * Store the cursor incase we need it.
     */
    if (debug) printf("xwd: Storing target cursor.\n");
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
     * Check to see if we are in the right pixmap format for the
     * display type.
     */
    if ((dpy->dplanes == 1) && (pixmap_format == ZFormat)) {
	Error("ZFormat is not valid on a monochrome display.");
    }

    /*
     * Let the user select the target window.
     */
    status = XGrabMouse(RootWindow, cursor, ButtonPressed);
    if (status == FAILURE) Error("Can't grab the mouse.");
    XNextEvent(&rep);
    XUngrabMouse();
    target_win = rep.subwindow;
    if (target_win == 0) {
	/*
	 * The user must have indicated the root window.
	 */
	if (debug) printf("xwd: Root window selected as target.\n");
	target_win = RootWindow;
    }
    else if (debug) printf("xwd: Window 0x%x slected as target.\n", target_win);

    /*
     * Inform the user not to alter the screen.
     */
    XFeep(FEEP_VOLUME);

    /*
     * Get the parameters of the window being dumped.
     */
    if (debug) printf("xwd: Getting target window information.\n");
    status = XQueryWindow(target_win, &win_info);
    if (status == FAILURE) Error("Can't query target window.");
    status = XFetchName(target_win, &win_name);
    if (status == FAILURE) Error("Can't fetch target window name.");
    win_name_size = strlen(win_name) + sizeof(char);
    /* sizeof(char) is included for the null string terminator. */

    /*
     * Calculate the virtual x, y, width and height of the window pane image
     * (this depends on wether or not the borders are included.
     */
    if (nobdrs) {
	if (debug) printf("xwd: Image without borders selected.\n");
	image_win = target_win;
	virt_x = 0;
	virt_y = 0;
	virt_width = win_info.width;
	virt_height = win_info.height;
    }
    else {
	if (debug) printf("xwd: Image with borders selected.\n");
	image_win = RootWindow;
	virt_x = win_info.x;
	virt_y = win_info.y;
	virt_width = win_info.width + (win_info.bdrwidth << 1);
    	virt_height = win_info.height + (win_info.bdrwidth << 1);
    }

    /*
     * Determine the pixmap size.
     */
    if (pixmap_format == XYFormat) {
	buffer_size = XYPixmapSize(virt_width, virt_height, dpy->dplanes);
	if (debug) {
	    printf("xwd: Pixmap in XYFormat, size %d bytes.\n", buffer_size);
	}
    }
    else if (dpy->dplanes < 9) {
	buffer_size = BZPixmapSize(virt_width, virt_height);
	if (debug) {
	    printf("xwd: Pixmap in byte ZFormat, size %d bytes.\n", buffer_size);
	}
    }
    else {
	buffer_size = WZPixmapSize(virt_width, virt_height);
	if (debug) {
	    printf("xwd: Pixmap in word ZFormat, size %d bytes.\n", buffer_size);
	}
    }


    /*
     * Calloc the buffer.
     */
    if (debug) printf("xwd: Calloc'ing data buffer.\n");
    buffer = calloc(buffer_size , 1);
    if (buffer == NULL) Error("Can't calloc data buffer.");

    /*
     * Snarf the pixmap out of the frame buffer.
     */
    if (debug) printf("xwd: Getting pixmap.\n");
    if (pixmap_format == XYFormat) {
	(void) XPixmapGetXY(
	    image_win,
	    virt_x, virt_y,
	    virt_width, virt_height,
	    (short *)buffer
	);
    }
    else {
	(void) XPixmapGetZ(
	    image_win,
	    virt_x, virt_y,
	    virt_width, virt_height,
	    (caddr_t)buffer
	);
    }

    /*
     * Inform the user that the image has been retrieved.
     */
    XFeep(FEEP_VOLUME);
    XFeep(FEEP_VOLUME);
    XFlush();

    /*
     * Calculate header size.
     */
    if (debug) printf("xwd: Calculating header size.\n");
    header_size = sizeof(header) + win_name_size;

    /*
     * Writ out header information.
     */
    if (debug) printf("xwd: Constructing and dumping file header.\n");
    header.header_size = header_size;
    header.file_version = XWD_FILE_VERSION;
    header.display_type = dpy->dtype;
    header.display_planes = dpy->dplanes;
    header.pixmap_format = pixmap_format;
    header.pixmap_width = virt_width;
    header.pixmap_height = virt_height;
    header.window_width = win_info.width;
    header.window_height = win_info.height;
    header.window_x = win_info.x;
    header.window_y = win_info.y;
    header.window_bdrwidth = win_info.bdrwidth;
    header.padding = 0;

    (void) fwrite((char *)&header, sizeof(header), 1, out_file);
    (void) fwrite(win_name, win_name_size, 1, out_file);

    /*
     * Write out the buffer.
     */
    if (debug) printf("xwd: Dumping pixmap.\n");
    (void) fwrite(buffer, (int) buffer_size, 1, out_file);

    /*
     * Close the output file.
     */
    if (debug) printf("xwd: Closing output file.\n");
    (void) fclose(out_file);

    /*
     * Free the pixmap buffer.
     */
    if (debug) printf("xwd: Freeing pixmap buffer.\n");
    free(buffer);

    /*
     * Free window name string.
     */
    if (debug) printf("xwd: Freeing window name string.\n");
    free(win_name);
    exit(0);
}


/*
 * Report the syntax for calling xwd.
 */
Syntax(call)
    char *call;
{
    fprintf(
	stderr,
	"\nUsage: %s [-debug] [-help] [-nobdrs] [-out <file>]\n",
    	call
    );
    fprintf(stderr, "                [-z] [[host]:vs]\n\n");
    exit(0);
}


/*
 * Error - Fatal xwd error.
 */
Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "\nxwd: Error => %s", string);
	fprintf(stderr, "\n\n");

	if (errno != 0) {
		perror("xwd");
		fprintf(stderr, "\n");
	}

	exit(1);
}

/* End of xwd.c */
