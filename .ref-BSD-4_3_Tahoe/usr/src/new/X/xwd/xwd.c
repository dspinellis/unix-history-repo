#include <X/mit-copyright.h>

/* Copyright 1985, 1986, Massachusetts Institute of Technology */

/*
 * xwd.c MIT Project Athena, X Window system window raster image dumper.
 *
 * This program will dump a raster image of the contents of a window into a 
 * file for output on graphics printers or for other uses.
 *
 *  Author:	Tony Della Fera, DEC
 *		17-Jun-85
 * 
 *  Modification history:
 *
 *  11/14/86 Bill Wyatt, Smithsonian Astrophysical Observatory
 *    - Removed Z format option, changing it to an XY option. Monochrome 
 *      windows will always dump in XY format. Color windows will dump
 *      in Z format by default, but can be dumped in XY format with the
 *      -xy option.
 *
 *  11/18/86 Bill Wyatt
 *    - VERSION 6 is same as version 5 for monchrome. For colors, the 
 *      appropriate number of Color structs are dumped after the header,
 *      which has the number of colors (=0 for monochrome) in place of the
 *      V5 padding at the end. Up to 16-bit displays are supported. I
 *      don't yet know how 24- to 32-bit displays will be handled under
 *      the Version 11 protocol.
 */

#ifndef lint
static char *rcsid_xwd_c = "$Header: xwd.c,v 10.12 86/11/25 09:01:08 jg Rel $";
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
    register int i, *histbuffer;
    register u_short *wbuffer;
    register char *buffer, *cbuffer;

    unsigned buffer_size;
    int virt_x, virt_y;
    int virt_width, virt_height;
    int pixmap_format = -1;
    int win_name_size;
    int header_size;
    int ncolors = 0;
    char *str_index;
    char *file_name;
    char display[256];
    char *win_name;
    Bool nobdrs = FALSE;
    Bool debug = FALSE;
    Bool standard_out = TRUE;

    Color *pixcolors;
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
	if(str_index != (char *)NULL) {
	    (void) strncpy(display,argv[i],sizeof(display));
	    continue;
        }
	str_index = (char *) index (argv [i], '-');
	if (str_index == (char *)NULL) Syntax(argv[0]);
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
	if(strncmp(argv[i], "-xy") == 0) {
	    pixmap_format = XYFormat;
	    continue;
	}
	Syntax(argv[0]);
    }
    
    if (!standard_out) {
	/*
	 * Open the output file.
	 */
	if((out_file = fopen(file_name, "w")) == NULL)
	  Error("Can't open output file as specified.");
    }

    /*
     * Open the display.
     */
    if ((dpy = XOpenDisplay(display)) == NULL) {
        fprintf(stderr, "%s: Can't open display '%s'\n",
		argv[0], XDisplayName(display));
	exit(1);
      }


    /*
     * Store the cursor incase we need it.
     */
    if (debug) fprintf(stderr,"xwd: Storing target cursor.\n");
    if((cursor = XCreateCursor(
    	target_width, target_height, 
    	target_bits, target_mask_bits, 
	8, 8,
	BlackPixel, WhitePixel,
	GXcopy
    )) == FAILURE)
	Error("Error occured while trying to store target cursor.");

    /*
     * Set the right pixmap format for the display type.
     */
    if(DisplayPlanes() == 1) pixmap_format = XYFormat;
    else {
	if(pixmap_format != XYFormat) pixmap_format = ZFormat;
    }

    /*
     * Let the user select the target window.
     */
    if(XGrabMouse(RootWindow, cursor, ButtonPressed) == FAILURE)
      Error("Can't grab the mouse.");
    XNextEvent(&rep);
    XUngrabMouse();
    target_win = rep.subwindow;
    if (target_win == 0) {
	/*
	 * The user must have indicated the root window.
	 */
	if (debug) fprintf(stderr,"xwd: Root window selected as target.\n");
	target_win = RootWindow;
    }
    else if (debug) 
     fprintf(stderr,
	     "xwd: Window 0x%x slected as target.\n", target_win);

    /*
     * Inform the user not to alter the screen.
     */
    XFeep(FEEP_VOLUME);

    /*
     * Get the parameters of the window being dumped.
     */
    if (debug) fprintf(stderr,"xwd: Getting target window information.\n");

    if(XQueryWindow(target_win, &win_info) == FAILURE) 
     Error("Can't query target window.");
    if(XFetchName(target_win, &win_name) == FAILURE)
     Error("Can't fetch target window name.");

    /* sizeof(char) is included for the null string terminator. */
    win_name_size = strlen(win_name) + sizeof(char);

    /*
     * Calculate the virtual x, y, width and height of the window pane image
     * (this depends on wether or not the borders are included.
     */
    if (nobdrs) {
	if (debug) fprintf(stderr,"xwd: Image without borders selected.\n");
	image_win = target_win;
	virt_x = 0;
	virt_y = 0;
	virt_width = win_info.width;
	virt_height = win_info.height;
    }
    else {
	if (debug) fprintf(stderr,"xwd: Image with borders selected.\n");
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
	buffer_size = XYPixmapSize(virt_width, virt_height, DisplayPlanes());
	if (debug) {
	    fprintf(stderr,
		    "xwd: Pixmap in XYFormat, size %d bytes.\n", buffer_size);
	}
    }
    else if (DisplayPlanes() < 9) {
	buffer_size = BZPixmapSize(virt_width, virt_height);
	if (debug) {
	    fprintf(stderr,
	      "xwd: Pixmap in byte ZFormat, size %d bytes.\n", buffer_size);
	}
    }
    else {
	buffer_size = WZPixmapSize(virt_width, virt_height);
	if (debug) {
	    fprintf(stderr,
	      "xwd: Pixmap in word ZFormat, size %d bytes.\n", buffer_size);
	}
    }


    /*
     * Calloc the buffer.
     */
    if (debug) fprintf(stderr,"xwd: Calloc'ing data buffer.\n");
    if((buffer = calloc(buffer_size , 1)) == NULL)
       Error("Can't calloc data buffer.");

    /*
     * Snarf the pixmap out of the frame buffer.
     * Color windows get snarfed in Z format first to check the color
     * map allocations before resnarfing if XY format selected.
     */
    if (debug) fprintf(stderr,"xwd: Getting pixmap.\n");
    if (DisplayPlanes() == 1) {
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
     * Find the number of colors used, then write them out to the file.
     */
    ncolors = 0;
    if(DisplayPlanes() > 1) {
	if(DisplayPlanes() < 9) {
	    histbuffer = (int *)calloc(256, sizeof(int));
	    bzero(histbuffer, 256*sizeof(int));
	    pixcolors = (Color *)calloc(1, sizeof(Color));
	    for(i=0; i<buffer_size; i++) {
		/* if previously found, skip color query */
		if(histbuffer[(int)buffer[i]] == 0) {
		    pixcolors = 
		      (Color *)realloc(pixcolors, sizeof(Color)*(++ncolors));
		    if(debug)
		      fprintf(stderr,"Color %3d at pixel val %5d, i= %5d =",
			      ncolors, buffer[i], i);
		    histbuffer[(int)buffer[i]]++;
		    pixcolors[ncolors-1].pixel = (int)buffer[i];
		    if(XQueryColor(&pixcolors[ncolors-1]) == 0) 
		      Error("Unable to query color table?");
		    if(debug) fprintf(stderr,"%5d %5d %5d\n",
				      pixcolors[ncolors-1].red,
				      pixcolors[ncolors-1].green,
				      pixcolors[ncolors-1].blue);
		}
	    }
	}
	else if(DisplayPlanes() < 17) {
	    wbuffer = (u_short *)buffer;
	    histbuffer = (int *)calloc(65536, sizeof(int));
	    bzero(histbuffer, 65536*sizeof(int));
	    pixcolors = (Color *)calloc(1, sizeof(Color));
	    for(i=0; i<(buffer_size/sizeof(u_short)); i++) {
		/* if previously found, skip color query */
		if(histbuffer[(int)wbuffer[i]] == 0) {
		    pixcolors = 
		      (Color *)realloc(pixcolors, sizeof(Color)*(++ncolors));
		    if(debug)
		      fprintf(stderr,"Color %2d at pixel val %d, i= %d =",
			      ncolors, wbuffer[i], i);
		    histbuffer[(int)wbuffer[i]]++;
		    pixcolors[ncolors-1].pixel = (int)wbuffer[i];
		    if(XQueryColor(&pixcolors[ncolors-1]) == 0) 
		      Error("Unable to query color table?");
		    if(debug) fprintf(stderr,"%d %d %d\n",
				      pixcolors[ncolors-1].red,
				      pixcolors[ncolors-1].green,
				      pixcolors[ncolors-1].blue);
		}
	    }
	} 
	else if(DisplayPlanes() > 16)
	  Error("Unable to handle more than 16 planes at this time");

	/* reread in XY format if necessary */
	if(pixmap_format == XYFormat) {
	    (void) XPixmapGetXY(image_win,
				virt_x, virt_y,
				virt_width, virt_height,
				(short *)buffer);
	}

	free(histbuffer);
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
    if (debug) fprintf(stderr,"xwd: Calculating header size.\n");
    header_size = sizeof(header) + win_name_size;

    /*
     * Write out header information.
     */
    if (debug) fprintf(stderr,"xwd: Constructing and dumping file header.\n");
    header.header_size = header_size;
    header.file_version = XWD_FILE_VERSION;
    header.display_type = DisplayType();
    header.display_planes = DisplayPlanes();
    header.pixmap_format = pixmap_format;
    header.pixmap_width = virt_width;
    header.pixmap_height = virt_height;
    header.window_width = win_info.width;
    header.window_height = win_info.height;
    header.window_x = win_info.x;
    header.window_y = win_info.y;
    header.window_bdrwidth = win_info.bdrwidth;
    header.window_ncolors = ncolors;

    (void) fwrite((char *)&header, sizeof(header), 1, out_file);
    (void) fwrite(win_name, win_name_size, 1, out_file);

    /*
     * Write out the color maps, if any
     */
    if (debug) fprintf(stderr,"xwd: Dumping %d colors.\n",ncolors);
    (void) fwrite(pixcolors, sizeof(Color), ncolors, out_file);

    /*
     * Write out the buffer.
     */
    if (debug) fprintf(stderr,"xwd: Dumping pixmap.\n");
    (void) fwrite(buffer, (int) buffer_size, 1, out_file);

    /*
     * Close the output file.
     */
    if (debug) fprintf(stderr,"xwd: Closing output file.\n");
    (void) fclose(out_file);

    /*
     * free the color buffer.
     */
    if(debug && ncolors > 0) fprintf(stderr,"xwd: Freeing color map.\n");
    if(ncolors > 0) free(pixcolors);

    /*
     * Free the pixmap buffer.
     */
    if (debug) fprintf(stderr,"xwd: Freeing pixmap buffer.\n");
    free(buffer);

    /*
     * Free window name string.
     */
    if (debug) fprintf(stderr,"xwd: Freeing window name string.\n");
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
	"xwd: %s [-debug] [-help] [-nobdrs] [-out <file>]\n",
    	call
    );
    fprintf(stderr, "                [-xy] [[host]:vs]\n");
    exit(1);
}


/*
 * Error - Fatal xwd error.
 */
Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "\nxwd: Error => %s", string);
	if (errno != 0) {
		perror("xwd");
		fprintf(stderr, "\n");
	}

	exit(1);
}

/* End of xwd.c */
