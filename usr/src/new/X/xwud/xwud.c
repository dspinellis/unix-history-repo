#include <X/mit-copyright.h>

/* Copyright 1985, 1986, Massachusetts Institute of Technology */

/*
 * xwud.c - MIT Project Athena, X Window system window raster image
 *	    undumper.
 *
 * This program will read a raster image of a window from stdin or a file
 * and display it on an X display.
 *
 *  Author:	Tony Della Fera, DEC
 *
 *  Modified 11/14/86 by William F. Wyatt,
 *                        Smithsonian Astrophysical Observatory
 *    allows writing of monochrome XYFormat window dump files on a color
 *    display, using default WhitePixel for 1's and BlackPixel for 0's.
 *
 *  Modified 11/20/86 WFW
 *    VERSION 6 - same as V5 for monochrome, but expects color map info
 *    in the file for color images. Checks to see if the requested
 *    colors are already in the display's map (e.g. if the window dump
 *    and undump are contemporaneous to the same display). If so,
 *    undump immediately. If not, request new colors, alter the 
 *    pixels to the new values, then write the pixmap. Note that
 *    multi-plane XY format undumps don't work if the pixel values
 *    corresponding to the requested colors have to be changed.
 */

#ifndef lint
static char *rcsid_xwud_c = "$Header: xwud.c,v 10.10 86/11/25 08:44:25 jg Rel $";
#endif

#include <X/Xlib.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
extern char *calloc();

#include <X/XWDFile.h>

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
    register int *histbuffer;
    register u_short *wbuffer;
    register char *buffer;

    int j, status;
    int *cpixels, cplanes;
    int onebufsize;
    int planes;
    int forepixel;
    int backpixel;
    unsigned buffer_size, total_buffer_size;
    int win_name_size;
    char *str_index;
    char *file_name;
    char display[256];
    char *win_name;
    Bool standard_in = TRUE;
    Bool newcolors = FALSE, debug = FALSE, inverse = FALSE;

    Color *pixcolors, *newpixcolors;
    Display *dpy;
    Window image_win;
    Pixmap image_pixmap;
    XEvent event;
    register XExposeEvent *xevent = (XExposeEvent *)&event;

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
	if(strcmp(argv[i], "-inverse") == 0) {
	    inverse = TRUE;
	    continue;
	}
	if(strcmp(argv[i], "-debug") == 0) {
	    debug = TRUE;
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
        fprintf(stderr, "%s: Can't open display '%s'\n",
		argv[0], XDisplayName(display));
	exit(1);
    }

    /*
     * Read in header information.
     */
    if(fread((char *)&header, sizeof(header), 1, in_file) != 1)
      Error("Unable to read dump file header.");

    /*
     * check to see if the dump file is in the proper format.
     */
    if (header.file_version != XWD_FILE_VERSION) {
	fprintf(stderr,"xwud: XWD file format version missmatch.");
	if(header.file_version == 5 && header.display_planes == 1)
	  fprintf(stderr,"\n      (monochrome works anyway)\n");
	else Error("exiting.");
    }

    if(DisplayPlanes() < header.display_planes)
      Error("Windump has more planes than display.");

    /*
     * Check to see if we are in the right pixmap format for the
     * display type.
     */
    if ((DisplayPlanes() == 1) && (header.pixmap_format != XYFormat)) {
	Error(
	 "Windump is in ZFormat which is not valid on a monochrome display.");
    }

    /*
     * Calloc window name.
     */
    win_name_size = ABS(header.header_size - sizeof(header));
    if((win_name = calloc((unsigned) win_name_size, sizeof(char))) == NULL)
      Error("Can't calloc window name storage.");

    /*
     * Read in window name.
     */
    if(fread(win_name, sizeof(char), win_name_size, in_file) != win_name_size)
      Error("Unable to read window name from dump file.");
    if(debug) fprintf(stderr,"win_name =%s\n", win_name);

    /*
     * Determine the pixmap size.
     */
    if (header.pixmap_format == XYFormat) {
	buffer_size =
	  XYPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height,
	    header.display_planes);
	total_buffer_size = 
	  XYPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height,
	    DisplayPlanes() );
    }
    else if (header.display_planes < 9) {
	total_buffer_size = buffer_size = BZPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height
	);
    }
    else if(header.display_planes < 17) {
	total_buffer_size = buffer_size = WZPixmapSize(
	    header.pixmap_width,
	    header.pixmap_height
	);
    }
    else {
	Error("Can't undump pixmaps more than 16 bits deep.\n");
    } 


    /* Calloc the color map buffer.
     * Read it in, copy it and use the copy to query for the
     * existing colors at those pixel values.
     */
    if(header.window_ncolors) {
	pixcolors = (Color *)calloc(header.window_ncolors,sizeof(Color));
	if(fread(pixcolors,sizeof(Color),header.window_ncolors, in_file)
	   != header.window_ncolors)
	  Error("Unable to read color map from dump file.");
	if(debug)
	  fprintf(stderr,"Read %d colors\n", header.window_ncolors);
	newpixcolors = (Color *)calloc(header.window_ncolors,sizeof(Color));
	bcopy(pixcolors, newpixcolors, sizeof(Color)*header.window_ncolors);
	if(XQueryColors(newpixcolors,header.window_ncolors) == 0)
	  Error("Can't query the color map?");
	for(i=0; i<header.window_ncolors; i++)
	  if(!ColorEqual(&pixcolors[i], &newpixcolors[i])) {
	      newcolors = TRUE;
	      break;
	  }
	if(debug) {
	    if(newcolors)  fprintf(stderr,"New colors needed\n");
	    else fprintf(stderr,"Old colors match!\n");
	}
    }

    /*
     * Calloc the pixel buffer.
     */
    if((buffer = calloc(total_buffer_size, 1)) == NULL)
      Error("Can't calloc data buffer.");
    bzero(buffer,total_buffer_size);

    /*
     * Read in the pixmap buffer.
     */
    if((status = fread(buffer, sizeof(char), (int)buffer_size, in_file))
       != buffer_size)
      Error("Unable to read pixmap from dump file.");
    /*
     * Close the input file.
     */
    (void) fclose(in_file);

    /*
     * If necessary, get and store the new colors, convert the pixels to the
     * new colors appropriately.
     */
    if(newcolors) {
	cpixels = (int *)calloc(header.window_ncolors+1,sizeof(int));
	if(XGetColorCells(0, header.window_ncolors, 0, &cplanes, cpixels)
	   == 0)
	  Error("Can't allocate colors.");
	for(i=0; i<header.window_ncolors; i++) {
	    newpixcolors[i].pixel = cpixels[i];
	    newpixcolors[i].red   = pixcolors[i].red;
	    newpixcolors[i].green = pixcolors[i].green;
	    newpixcolors[i].blue  = pixcolors[i].blue;
	    if(debug) 
	      fprintf(stderr,"Pixel %4d, r = %5d  g = %5d  b = %5d\n",
		      newpixcolors[i].pixel, newpixcolors[i].red,
		      newpixcolors[i].green, newpixcolors[i].blue);
	}
	XStoreColors(header.window_ncolors, newpixcolors);

	/* now, make a lookup table to convert old pixels into the new ones*/
	if(header.pixmap_format == ZFormat) {
	    if(header.display_planes < 9) {
		histbuffer = (int *)calloc(256, sizeof(int));
		bzero(histbuffer, 256*sizeof(int));
		for(i=0; i<header.window_ncolors; i++)
		  histbuffer[pixcolors[i].pixel] = newpixcolors[i].pixel;
		for(i=0; i<buffer_size; i++)
		  buffer[i] = histbuffer[buffer[i]];
	    }
	    else if(header.display_planes < 17) {
		histbuffer = (int *)calloc(65536, sizeof(int));
		bzero(histbuffer, 65536*sizeof(int));
		for(i=0; i<header.window_ncolors; i++)
		  histbuffer[pixcolors[i].pixel] = newpixcolors[i].pixel;
		wbuffer = (u_short *)buffer;
		for(i=0; i<(buffer_size/sizeof(u_short)); i++)
		  wbuffer[i] = histbuffer[wbuffer[i]];
	    } 
	    else if(header.display_planes > 16) {
		Error("Unable to handle more than 16 planes at this time");
	    }
	    free(histbuffer);
	}
	free(cpixels);
	bcopy(newpixcolors, pixcolors, sizeof(Color)*header.window_ncolors);
	free(newpixcolors);
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
    XSelectInput(image_win, (ButtonPressed | ExposeWindow | ExposeRegion));
     
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
	int i, nbytes;
	/*
	 * Wait on mouse input event to terminate.
	 */
	XNextEvent(&event);
	if (event.type == ButtonPressed) break;

	switch((int)event.type) {
	  case ExposeWindow:  /* Copy the data into the window.*/
	  case ExposeRegion:  /* simpler to copy from x=0 for full width */
	    if(header.pixmap_format == XYFormat) {
		onebufsize =  /* size of each bitmap */
		  XYPixmapSize(header.pixmap_width,
			       header.pixmap_height, 1);
		nbytes = BitmapSize(header.pixmap_width,1);
		if(header.display_planes > 1) {
		    forepixel = -1;
		    backpixel = 0;
		    planes = 1<<(DisplayPlanes()); /* MSB << 1 */
		}
		else {
		    forepixel = WhitePixel;
		    backpixel = BlackPixel;
		    planes = AllPlanes;
		}
		for(j=0; j<header.display_planes; j++) {	
		    if(header.display_planes > 1)
			planes >>= 1; /* shift down a bit */
		    for(i=0; i<xevent->height; i+=100)
		      if(inverse)
			XBitmapBitsPut(image_win,
				       0, i + xevent->y,
				       header.pixmap_width, 
				       MIN(100, xevent->height - i),
				       buffer+((i+xevent->y)*nbytes)
				          + (onebufsize * j), 
				       backpixel, forepixel,
				       0, GXcopy, planes);
		      else
			XBitmapBitsPut(image_win,
				       0, i + xevent->y,
				       header.pixmap_width, 
				       MIN(100, xevent->height - i),
				       buffer+((i+xevent->y)*nbytes)
				          + (onebufsize * j), 
				       forepixel, backpixel,
				       0, GXcopy, planes);
		}
	    } 
	    else if(DisplayPlanes() < 9) {
		nbytes = BZPixmapSize(header.pixmap_width,1);
		for(i=0; i<xevent->height; i+=100)
		  XPixmapBitsPutZ(image_win, 
				  0, i + xevent->y,
				  header.pixmap_width,
				  MIN(100, xevent->height - i),
				  buffer+((i+xevent->y)*nbytes),
				  0, GXcopy, AllPlanes);
	    }
	    else {  /* Display Planes > 8 */
		nbytes = WZPixmapSize(header.pixmap_width, 1);
		for(i=0; i<xevent->height; i+=100)
		  XPixmapBitsPutZ(image_win, 
				  0, i + xevent->y,
				  header.pixmap_width,
				  MIN(50, xevent->height - i),
				  buffer+((i+xevent->y)*nbytes),
				  0, GXcopy, AllPlanes);
	    }
	}
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
 * test two color map entries for equality
 */
ColorEqual(color1, color2)
     register Color *color1, *color2;
{
    return(color1->pixel == color2->pixel &&
	   color1->red   == color2->red &&
	   color1->green == color2->green &&
	   color1->blue  == color2->blue);
}

/*
 * Report the syntax for calling xwud.
 */
Syntax(call)
    char *call;
{
    fprintf( stderr,
	"xwud: %s [-help][-debug][-inverse][-in <file>][[host]:vs]\n",
    	call
    );
    exit(1);
}


/*
 * Error - Fatal xwud error.
 */
Error(string)
	char *string;	/* Error description string. */
{
	fprintf(stderr, "xwud: Error => %s\n", string);

	if (errno != 0) {
		perror("xwud");
		fprintf(stderr, "\n");
	}

	exit(1);
}

/* End of xwud.c */
