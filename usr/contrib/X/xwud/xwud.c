#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

/*
 * xwud.c - MIT Project Athena, X Window system window raster image
 *	    undumper.
 *
 * This program will read a raster image of a window from stdin or a file
 * and display it on an X display.
 *
 *  Author:	Tony Della Fera, DEC
 */

#ifndef lint
static char *rcsid_xwud_c = "$Header: xwud.c,v 10.7 86/02/01 15:23:08 tony Rel $";
#endif

#include <X/Xlib.h>
#include <stdio.h>
#include <strings.h>

extern char *calloc();

#include "../xwd/XWDFile.h"

typedef enum _bool {FALSE, TRUE} Bool;

#define MAX(a, b) (a) > (b) ? (a) : (b)
#define MIN(a, b) (a) < (b) ? (a) : (b)
#define ABS(a) (a) < 0 ? -(a) : (a)

#define FAILURE 0

extern int errno;

main(argc, argv)
    int argc;
    char **argv;
{
    register int i;
    int status;
    unsigned buffer_size;
    int win_name_size;
    char *str_index;
    char *file_name;
    char display[256];
    char *win_name;
    char *buffer;
    Bool standard_in = TRUE;

    Display *dpy;
    Window image_win;
    Pixmap image_pixmap;
    XEvent event;

    XWDFileHeader header;

    FILE *in_file = stdin;

    for (i = 1; i < argc; i++) {
	str_index = (char *)index (argv[i], ':');
	if(str_index != NULL) {
	    (void) strncpy(display,argv[i],sizeof(display));
	    continue;
        }
	str_index = (char *) index (argv [i], '-');
	if (str_index == NULL) Syntax(argv[0]);
	if (strncmp(argv[i], "-help", 5) == 0) {
	    Syntax(argv[0]);
	}
	if (strncmp(argv[i], "-in", 4) == 0) {
	    if (++i >= argc) Syntax(argv[0]);
	    file_name = argv[i];
	    standard_in = FALSE;
	    continue;
	}
	Syntax(argv[0]);
    }
    
    if (!standard_in) {
	/*
	 * Open the output file.
	 */
	in_file = fopen(file_name, "r");
	if (in_file == NULL) {
	    Error("Can't open output file as specified.");
	}
    }
    
    /*
     * Open the display.
     */
    if ((dpy = XOpenDisplay(display)) == NULL) {
	Error("Error occured while trying open display.");
    }

    /*
     * Read in header information.
     */
    status = fread((char *)&header, sizeof(header), 1, in_file);
    if (status != 1) Error("Unable to read dump file header.");

    /*
     * check to see if the dump file is in the proper format.
     */
    if (header.file_version != XWD_FILE_VERSION) {
	Error("XWD file format version missmatch.");
    }

    /*
     * Check to see if we are in the right pixmap format for the
     * display type.
     */
    if ((dpy->dplanes == 1) && (header.pixmap_format != XYFormat)) {
	Error("Windump is in ZFormat which is not valid on a monochrome display.");
    }

    /*
     * Calloc window name.
     */
    win_name_size = abs(header.header_size - sizeof(header));
    win_name = calloc((unsigned) win_name_size, sizeof(char));
    if (win_name == NULL) Error("Can't calloc window name storage.");

    /*
     * Read in window name.
     */
    status = fread(win_name, sizeof(char), win_name_size, in_file);
    if (status != win_name_size) {
	Error("Unable to read window name from dump file.");
    }

    /*
     * Determine the pixmap size.
     */
    if (header.pixmap_format == XYFormat) {
	buffer_size = XYPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height,
	    header.display_planes
	);
    }
    else if (header.display_planes < 9) {
	buffer_size = BZPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height
	);
    }
    else {
	buffer_size = WZPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height
	);
    }

    /*
     * Calloc the buffer.
     */
    buffer = calloc(buffer_size, 1);
    if (buffer == NULL) Error("Can't calloc data buffer.");

    /*
     * Read in the pixmap buffer.
     */
    status = fread(buffer, sizeof(char), (int)buffer_size, in_file);
    if (status != buffer_size) Error("Unable to read pixmap from dump file.");

    /*
     * Close the input file.
     */
    (void) fclose(in_file);

    /*
     * Undump pixmap.
     */
    if (header.pixmap_format == XYFormat) {
	image_pixmap = XStorePixmapXY(
	    header.pixmap_width,
	    header.pixmap_height,
	    buffer
	);
    }
    else {
	image_pixmap = XStorePixmapZ(
	    header.pixmap_width,
	    header.pixmap_height,
	    buffer
	);
    }

    /*
     * Create the image window.
     */
    image_win = XCreateWindow(
	RootWindow,
	header.window_x, header.window_y,
	header.pixmap_width, header.pixmap_height,
	0, (Pixmap) 0,
	(Pixmap) 0
    );
    if (image_win == FAILURE) Error("Can't create image window.");

    /*
     * Select mouse ButtonPressed on the window, this is how we determine
     * when to stop displaying the window.
     */
    XSelectInput(image_win, (ButtonPressed | ExposeWindow));
     
    /*
     * Store the window name string.
     */
    XStoreName(image_win, win_name);
    
    /*
     * Map the image window.
     */
    XMapWindow(image_win);

    /*
     * Set up a while loop to maintain the image.
     */
    while (TRUE) {
	/*
	 * Blit the pixmap into the window.
	 */
	XPixmapPut(
	    image_win,
	    0, 0, 0, 0,
	    header.pixmap_width, header.pixmap_height,
	    image_pixmap,
	    GXcopy,
	    AllPlanes
	);

	/*
	 * Wait on mouse input event to terminate.
	 */
	XNextEvent(&event);
	if (event.type == ButtonPressed) break;
    }

    /*
     * Destroy the image window.
     */
    XDestroyWindow(image_win);
    
    /*
     * Free the pixmap buffer.
     */
    free(buffer);

    /*
     * Free window name string.
     */
    free(win_name);
    exit(0);
}


/*
 * Report the syntax for calling xwud.
 */
Syntax(call)
    char *call;
{
    fprintf(
	stderr,
	"\nUsage: %s [-debug] [-help] [-in <file>] [[host]:vs]\n\n",
    	call
    );
    exit(0);
}


/*
 * Error - Fatal xwud error.
 */
Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "\nxwud: Error => %s", string);
	fprintf(stderr, "\n\n");

	if (errno != 0) {
		perror("xwud");
		fprintf(stderr, "\n");
	}

	exit(1);
}

/* End of xwud.c */
