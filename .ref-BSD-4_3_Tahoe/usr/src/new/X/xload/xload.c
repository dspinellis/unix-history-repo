#include <X/mit-copyright.h>

/* Copyright 1985, by the Massachusetts Institute of Technology */
/* xload -- graph load average on X window system display.
 * K. Shane Hartman and Stuart A. Malone with ripoffs from xclock.
 * Host name feature added by Jim Gettys.
 * Scale feature added by Bob Scheifler.
 * Rescale feature added by Stuart A. Malone.
 */
#ifndef lint
static char *rcsid_xload_c = "$Header: xload.c,v 10.13 86/11/30 14:59:08 jg Rel $";
#endif  lint

#include <stdio.h>
#include <strings.h>
#include <nlist.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/param.h>
#include <X/Xlib.h>

typedef enum _bool {FALSE, TRUE} Bool;

#define KMEM_FILE "/dev/kmem"
#define KMEM_ERROR "cannot open /dev/kmem"

#define DEFAULT_BORDER_WIDTH 3
#define DEFAULT_UPDATE 5		    /* Any smaller leads to lossage */
#define DEFAULT_FONT "6x10"
#define DEFAULT_POSITION "=360x120-0+0"	    /* upper right hand corner */
#define DEFAULT_SCALE 1

struct nlist namelist[] = {		    /* namelist for vmunix grubbing */
#define LOADAV 0
    {"_avenrun"},
    {0}
};

extern char *getenv();

/* GLOBAL */

Window win;				    /* load average window */
double data[2048];			    /* accumulated load average data */
int background;			    	    /* color of background */
int foreground;			    	    /* color of graph */
int highlight;				    /* color of text, scale */
Font font;				    /* font for printing hostname */
char *fn = DEFAULT_FONT;		    /* font for hostname */
FontInfo font_info;
char host[256];                             /* the host name */
double scale = DEFAULT_SCALE;		    /* n divisions for graph */
double min_scale = DEFAULT_SCALE;	    /* minimum value for scale */
double max_loadavg = 0.0;		    /* maximum loadavg on the graph */
int mapped = 1;				    /* should really display? */
					      
/* Diagnostic printer - Print message and exit */

void xload_error(message)
    char *message;
{
    fprintf(stderr, "xload: %s\n", message);
    perror("xload");
    exit(1);
}

/* Blts data according to current size, then redraws the load average window.
 * Next represents the number of valid points in data.  Returns the (possibly)
 * adjusted value of next.  If next is 0, this routine draws an empty window
 * (scale - 1 lines for graph).  If next is less than the current window width,
 * the returned value is identical to the initial value of next and data is
 * unchanged.  Otherwise keeps half a window's worth of data.  If data is
 * changed, then max_loadavg is updated to reflect the largest data point.
 */

int repaint_window(width, height, next)
    register int width, height, next;
{
    register int i, j;

    if (mapped)
	XClear(win);
    if (next >= width) {
	j = width >> 1;
	bcopy((char *)(data + next - j),(char *)data, j * sizeof(double));
	next = j;
	/* Since we just lost some data, recompute the max_loadavg. */
	max_loadavg = 0.0;
	for (i = 0; i < next; i++) {
	    if (data[i] > max_loadavg) max_loadavg = data[i];
	}
    }

    /* Compute the minimum scale required to graph the data, but don't go
       lower than min_scale. */
    if (max_loadavg > min_scale) scale = ((int)max_loadavg) + 1;
    else scale = min_scale;

    if (!mapped) return(next);

    /* Print hostname */
    XTextMask(win, 2, 2, host, strlen(host), font, highlight);

    /* Draw graph reference lines */
    for (i = 1; i < scale; i++) {
	j = (i * height) / scale;
	XLine(win, 0, j, width, j, 1, 1, highlight, GXcopy, AllPlanes);
    }

    /* Draw data point lines. */
    for (i = 0; i < next; i++)
    	XLine(win, i, height, i, (int)(height - (data[i] * height) / scale),
		1, 1, foreground, GXcopy, AllPlanes);
    return(next);
}

/* Exit with message describing command line format */

void usage()
{
    fprintf(stderr,
"usage: xload [-fn {font}] [-update {seconds}] [-scale {integer}] [-rv]\n"
);
    fprintf(stderr,
"             [=[{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]] [[{host}]:[{vs}]]\n"
);
    fprintf(stderr,
"             [-fg {color}] [-bg {color}] [-hl {color}] [-bd {color}] [-bw {pixels}]\n");
    exit(1);
}

/* Returns pointer to first char in search which is also in what, else NULL. */

char *strscan(search, what)
    register char *search;
    register char *what;
{
    register int i;
    register len = strlen(what);
    register char c;
    while (c = *(search++))
	for (i = 0; i < len; i++)
	    if (c == what[i]) return (--search);
    return (NULL);
}


void main(argc, argv)
    int argc;
    char **argv;
{
    char *arg;

    register int i;

    register int kmem;                      /* kmem pointer */
    register double loadavg;                /* load average value */
    long loadavg_seek;                      /* offset to load average in kmem */

    char display[256];                      /* will contain vs host */
    int vsnum;                              /* will contain vs number */
    
    int reverse = 0;
    char *border_color;
    char *fore_color;
    char *back_color;
    char *high_color;
    int border_pixmap;
    int border_width = DEFAULT_BORDER_WIDTH;
    int update = DEFAULT_UPDATE;
    Color cdef;
    OpaqueFrame window;			    /* frame for the window */
    char *geometry = NULL;
    char *def = DEFAULT_POSITION;	    /* default position */
    char *option;

    XEvent event;

    int maxfds;                             /* for select call */
    int readfds;
    int fdmask;
    struct timeval timeout;                 /* will contain update interval */

    /* Get name list. Then open kmem so we can seek for load average. */

    nlist("/vmunix", namelist);
    if (namelist[LOADAV].n_type == 0) xload_error("cannot get name list");
    loadavg_seek = namelist[LOADAV].n_value;
    kmem = open(KMEM_FILE, O_RDONLY);
    if (kmem < 0) xload_error(KMEM_ERROR);

    gethostname(host, sizeof(host));	    /* Who are we? */
    display[0] = '\0';

    if ((option = XGetDefault(argv[0],"ReverseVideo")) != NULL )
    		if (strcmp (option, "on") == 0)
			reverse = 1;
    if ((option = XGetDefault(argv[0],"BorderWidth")) != NULL)
    	border_width = atoi(option);
    if ((option = XGetDefault(argv[0],"BodyFont")) != NULL)
	fn = option;
    if ((border_color = XGetDefault(argv[0],"Border")) == NULL)
	border_color = XGetDefault(argv[0],"BorderColor");
    back_color = XGetDefault(argv[0],"Background");
    fore_color = XGetDefault(argv[0],"Foreground");
    high_color = XGetDefault(argv[0],"Highlight");
    if ((option = XGetDefault(argv[0],"Update")) != NULL)
	update = atoi(option);
    if ((option = XGetDefault(argv[0],"Scale")) != NULL)
	min_scale = atoi(option);
    if ((option = XGetDefault(argv[0], "Geometry")) != NULL)
	geometry = option;

    for (i = 1; i < argc; i++) {  	                /* Parse line */
	if (argv[i][0] == '=') {
	    geometry = argv[i];
	    continue;
	}
	if (index(argv[i], ':') != NULL) {	        /* host:display */
	    strncpy(display, argv[i], sizeof(display));
	    continue;
	}
	if (strcmp(argv[i], "-rv") == 0 ||
	    strcmp(argv[i], "-reverse") == 0) {		/* black on white */
	    reverse = 1;
	    continue;
	}
	if (strcmp(argv[i], "-fw") == 0 ||
	    strcmp(argv[i], "-forward") == 0) {		/* white on black */
	    reverse = 0;
	    continue;
	}
	if (strcmp(argv[i], "-bw") == 0 ||
	    strcmp(argv[i], "-border") == 0) {		/* border width */
	    if (++i >= argc) usage();
	    border_width = atoi(argv[i]);
	    continue;
	}
	if (strcmp(argv[i], "-fn") == 0 ||
	    strcmp(argv[i], "-font") == 0) {	        /* host name font */
	    if (++i >= argc) usage();
	    fn = argv[i];
	    continue;
	}
	if (strcmp(argv[i], "-bd") == 0 ||
	    strcmp(argv[i], "-color") == 0) {	       /* border color */
	   if (++i >= argc) usage();
	   border_color = argv[i];
	   continue;
	}
	if (strcmp(argv[i], "-fg") == 0 ||
	    strcmp(argv[i], "-foreground") == 0) {     /* foreground color */
	   if (++i >= argc) usage();
	   fore_color = argv[i];
	   continue;
	}
	if (strcmp(argv[i], "-bg") == 0 ||
	    strcmp(argv[i], "-background") == 0) {     /* background color */
	   if (++i >= argc) usage();
	   back_color = argv[i];
	   continue;
	}
	if (strcmp(argv[i], "-hl") == 0 ||
	    strcmp(argv[i], "-highlight") == 0) {     /* highlight color */
	   if (++i >= argc) usage();
	   high_color = argv[i];
	   continue;
	}
	if (strcmp(argv[i], "-u") == 0 ||
	    strcmp(argv[i], "-update") == 0) {		/* update interval */
	    if (++i >= argc) usage();
	    update = atoi(argv[i]);
	    continue;
	}
	if (strcmp(argv[i], "-s") == 0 ||
	    strcmp(argv[i], "-scale") == 0) {		/* load scale */
	    if (++i >= argc) usage();
	    min_scale = atoi(argv[i]);
	    continue;
	}
	usage();
    }

    if (border_width < 0) border_width = DEFAULT_BORDER_WIDTH;
    if (update < DEFAULT_UPDATE) update = DEFAULT_UPDATE;
    if (min_scale <= 0) min_scale = DEFAULT_SCALE;
    scale = min_scale;

    /* Open display  */
    if (!XOpenDisplay(display)) {
	fprintf(stderr, "%s: Can't open display '%s'\n",
		argv[0], XDisplayName(display));
	exit(1);
      }

    /* Need a font to print hostname in */
    font = XGetFont(fn);
    if (!font)
	xload_error("cannot open font");
    XQueryFont(font, &font_info);

    if (border_color && DisplayCells() > 2 &&
	XParseColor(border_color, &cdef) && XGetHardwareColor(&cdef))
	border_pixmap = XMakeTile(cdef.pixel);
    else if (reverse) border_pixmap = WhitePixmap;
    else border_pixmap = BlackPixmap;

    if (back_color && DisplayCells() > 2 &&
	XParseColor(back_color, &cdef) && XGetHardwareColor(&cdef))
	background = cdef.pixel;
    else if (reverse) background = BlackPixel;
    else background = WhitePixel;

    if (fore_color && DisplayCells() > 2 &&
	XParseColor(fore_color, &cdef) && XGetHardwareColor(&cdef))
	foreground = cdef.pixel;
    else if (reverse) foreground = WhitePixel;
    else foreground = BlackPixel;

    if (high_color && DisplayCells() > 2 &&
	XParseColor(high_color, &cdef) && XGetHardwareColor(&cdef))
	highlight = cdef.pixel;
    else highlight = foreground;

    window.bdrwidth = border_width;
    window.border = border_pixmap;
    window.background = XMakeTile(background);
    win = XCreate ("Load Average", argv[0], geometry, def, &window,
		   font_info.width * strlen(host) + 4, font_info.height + 4);
    XSelectInput(win, ExposeWindow|UnmapWindow);
    XMapWindow(win);			    /* Map window to screen */
    timeout.tv_sec = update;		    /* Set up timeout for select */
    timeout.tv_usec = 0;
    maxfds = dpyno() + 1;		    /* Set up select arguments */
    fdmask = 1 << dpyno();
    i = 0;				    /* Window is initially empty */

    while (1) {			  	    /* Main loop */
	if (XPending()) {
	    XNextEvent(&event);
	    switch (event.type) {
	    case ExposeWindow:
		mapped = 1;
		window.width = ((XExposeEvent *) &event)->width;
		window.height = ((XExposeEvent *) &event)->height;
		i = repaint_window(window.width, window.height, i);
		break;
	    case UnmapWindow:
		mapped = 0;
		break;
	    default:
		xload_error("unexpected X event");
	    }
	}
	else if (i >= window.width) i = repaint_window(window.width, window.height, i);
	/* Get the load average, stash the point and draw corresponding line. */
	lseek(kmem, loadavg_seek, 0);
#ifdef	sun
	{
		long temp;
		read(kmem, (char *)&temp, sizeof(long));
		loadavg = (double)temp/FSCALE;
	}
#else
	read(kmem, (char *)&loadavg, sizeof(double));
#endif

	/* Keep max_loadavg up to date, and if this data point is off the
	   graph, change the scale to make it fit. */
	if (loadavg > max_loadavg) {
	    max_loadavg = loadavg;
	    if (max_loadavg > scale) {
		scale = ((int)max_loadavg) + 1;
		i = repaint_window(window.width, window.height, i);
	    }
	}

	data[i] = loadavg;
	if (mapped) {
	    XLine(win, i, window.height, i, 
		    (int)(window.height - (window.height * loadavg) / scale),
		    1, 1, foreground, GXcopy, AllPlanes);
	    XFlush();			    /* Flush output buffers */
	}
	i++;				    /* Next point */
	readfds = fdmask;		    /* Initialize select mask */
					    /* and select on display fd */
	if (select(maxfds, &readfds, NULL, NULL, &timeout) == -1)
	    xload_error("select error on display file descriptor");
    } /* while */
} /* main */
