#ifndef lint
static char *sccsid = "%G% hack (aps) %W%";
#endif

#include  "def.h"
#include  "flying_bee.h"

/*
 *  hack  --  playing with windows.
 *	This program puts up two windows, each with different
 *	cursors.
 *	This program was actually the beginnings of a window based
 *	talk program.
 *		aps.
 */

#define INCREMENT 1
#define DECREMENT 0

#define DEBUG 0

int	debug = DEBUG;		/* 10 or greater is painfully detailed */
int	verbose = NO;		/* print some stuff */
int	myuid;			/* my uid */
char	me[UTMPNAMSIZ];		/* my login name */
char	mytty[NAMLEN];		/* my dev node name */


/*
 * X and window stuff.
 */
#define	XLOC1	40		/* x cordinate for window 1 */
#define	YLOC1	10		/* y cordinate for window 1 */
#define	XLOC2	500		/* x cordinate for window 2 */
#define	YLOC2	10		/* y cordinate for window 2 */
#define	WIDTH	300		/* width of window */
#define	HEIGHT	400		/* height of window */
#define BDRWDTH	6		/* window border width */
#define	BORDER	WhitePixmap	/* border pixmap */
#define	BCKGRND	BlackPixmap	/* background pixmap */
int	xloc;
int	yloc;
Display	*mydisp, *herdisp;	/* descriptors for the displays */
char	mydispname[NAMLEN];	/* descriptors for the displays */
char	herdispname[NAMLEN];	/* descriptors for the displays */
Window  mywin, herwin;		/* descriptors for the windows */
Window	mywinparent;		/* parent window */
Window	herwinparent;		/* parent window */


/*
 * cursor bitmaps for window 1
 */

#define aps_width 24
#define aps_height 24
static short aps_bits[] = {
   0xffff, 0xffff, 0xffff, 0xffff,
   0xffff, 0xffe7, 0xffff, 0xffdb,
   0xffff, 0xffbd, 0xff87, 0xffbd,
   0xff33, 0xffbd, 0xff79, 0xffdb,
   0xff7c, 0xffe7, 0x8f7e, 0xfff7,
   0xb33e, 0xff31, 0xb93c, 0xffce,
   0xb91d, 0xffd2, 0xbc49, 0xffdc,
   0x3ce3, 0xffdd, 0x98ff, 0xffe3,
   0xe6ff, 0xffff, 0xfe7f, 0xffff,
   0xff7f, 0xffff, 0xff3f, 0xffff,
   0xffbf, 0xffff, 0xffff, 0xffff,
   0xffff, 0xffff, 0xffff, 0xffff};

/* Inverse bitmap */
#define Iaps_width 24
#define Iaps_height 24
static short Iaps_bits[] = {
   0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0018, 0x0000, 0x0024,
   0x0000, 0x0042, 0x0078, 0x0042,
   0x00cc, 0x0042, 0x0086, 0x0024,
   0x0083, 0x0018, 0x7081, 0x0008,
   0x4cc1, 0x00ce, 0x46c3, 0x0031,
   0x46e2, 0x002d, 0x43b6, 0x0023,
   0xc31c, 0x0022, 0x6700, 0x001c,
   0x1900, 0x0000, 0x0180, 0x0000,
   0x0080, 0x0000, 0x00c0, 0x0000,
   0x0040, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000};


/*
 * array to store bees in flight
 */
#define CUR_COUNT	4	/* number of bee frames */
Cursor Bee[CUR_COUNT];		/* cursor table */


/*
 * main() -- What can one say about main...
 *   it is where it's at!
 *
 *  xhack
 */
main(argc, argv)
int argc;
char *argv[];
{
	Cursor curcur, hackcur;
	XEvent herxevent, myxevent;
	struct _XMouseOrCrossingEvent *mmevp;
	char *rindex(), *p;
	int curp;
	int inc_or_dec;

	/*
	 * get what few options we allow.  (currently none)
	 */
	while (--argc > 0 && (++argv)[0][0]=='-')
		{
		char *cp = argv[0] + 1;
		if (debug >= 10)
			printf("argv=%s\n", *argv);
		while (*cp)
			switch (*cp++)
				{
				case 'v':
					/* verbose flag */  /* nothing yet */
					verbose = YES;
					continue;
		
				default:
					fprintf(stderr, "Unknown flag %s\n",
					   *argv);
					fprintf(stderr,
					   "hack [-v] \n");
					exit(-1);
				}
		}

	/*****
	 * Create the windows.
	 *****/

	/*
	 * Open the display.
	 */
	if (debug >= 15)
		printf("About to XOpenDisplay(NULL) my display.\n");
	mydisp = XOpenDisplay(NULL);
	if (debug >= 15)
		printf("XopenDisplay(NULL) returned %x.\n", mydisp);
	if (mydisp == NULL)
		{
		fprintf(stderr, "xhack: can't open my display.\n");
		exit(-1);
		}

	/* creat my window*/
	xloc = XLOC1;
	yloc = YLOC1;
	if (debug >= 15)
		printf("About to XCreateWindow().\n");
	mywin = XCreateWindow(mydisp->root, xloc, yloc, WIDTH, HEIGHT,
		    BDRWDTH, BORDER, BCKGRND);
	if (debug >= 15)
		printf("XCreatWindow() returned %x.\n", (int) mywin);
	if (mywin == NULL)
		{
		fprintf(stderr, "hack: can't creat window.\n");
		exit(-1);
		}

	XStoreName(mywin, "xhack_window-1");	/* icon name */

	/* creat (describe) a cursor */
	curcur = XCreateCursor(aps_width, aps_height, aps_bits,
			Iaps_bits, 9, 10, BlackPixel, WhitePixel, GXxor);
	if (curcur == 0)
		{
		fprintf(stderr, "xhack: can't creat cursor.\n");
		exit(-1);
		}

	/* define (set) the cursor */
	XDefineCursor(mywin, curcur);
	if (debug >= 15)
		printf("About to XMapWindow().\n");

	XMapWindow(mywin);
	/*
	 * Normally, a window is really made on the screen automatically
	 * the next time input is read.  Since we don't do any input,
	 * we have to do an explicit flush.
	 */
	XFlush();


	/* creat the second window */
	xloc = XLOC2;
	yloc = YLOC2;

	if (debug >= 15)
		printf("About to XCreateWindow().\n");
	herwin = XCreateWindow(mydisp->root, xloc, yloc, WIDTH, HEIGHT,
		    BDRWDTH, BORDER, BCKGRND);
	if (debug >= 15)
		printf("XCreatWindow() returned %x.\n", (int) herwin);
	if (herwin == NULL)
		{
		fprintf(stderr, "hack: can't creat window.\n");
		exit(-1);
		}

	XStoreName(herwin, "bee_window-2");	/* icon name */

	/* creat (describe) some cursors */

	Bee[0] = XCreateCursor(bee_wing1_width, bee_wing1_height,
			bee_wing1_bits, Ibee_wing1_bits, 46, 16, BlackPixel,
			WhitePixel, GXxor);
	Bee[1] = XCreateCursor(bee_wing2_width, bee_wing2_height,
			bee_wing2_bits, Ibee_wing2_bits, 46, 16, BlackPixel,
			WhitePixel, GXxor);
	Bee[2] = XCreateCursor(bee_wing3_width, bee_wing3_height,
			bee_wing3_bits, Ibee_wing3_bits, 46, 16, BlackPixel,
			WhitePixel, GXxor);
	Bee[3] = XCreateCursor(bee_wing4_width, bee_wing4_height,
			bee_wing4_bits, Ibee_wing4_bits, 46, 16, BlackPixel,
			WhitePixel, GXxor);
	if ((Bee[0] == 0) || (Bee[1] == 0) ||
	       (Bee[2] == 0) || (Bee[3] == 0))
		{
		fprintf(stderr, "hack: can't creat bee cursors.\n");
		exit(-1);
		}

	/* define (set) the initial cursor */
	XDefineCursor(herwin, Bee[0]);	/* initial cursor define */
	if (debug >= 15)
		printf("About to XMapWindow().\n");

	XMapWindow(herwin);
	XFlush();	/* necessary because we don't get input */

	inc_or_dec = INCREMENT;   /* set direction to traverse cursor table */
	curp = 0;
	XSelectInput(herwin, MouseMoved|LeaveWindow|EnterWindow);
	for (EVER)
		{
		XNextEvent(&herxevent);

		(XEvent *)mmevp =  &herxevent;
		switch (mmevp->type)
			{
			case EnterWindow:
				if (debug >=10)
					printf("Mouse entered.\n");
				continue;

			case LeaveWindow:
				if (debug >=10)
					printf("Mouse left.\n");
				continue;
			
			case MouseMoved:
				if (debug >=15)
					printf("rel %d, %d.\n",
					   mmevp->x, mmevp->y);
				XDefineCursor(herwin, Bee[curp]);

				/*
				 * This little code allows smoother flight
				 * of the wings by moving forward then
				 * backwards through the different bitmaps
				 * in the table instead of recycling to 0.
				 * There are probably more straight forward
				 * ways to do this.
				 */
				if (inc_or_dec == INCREMENT)
					curp++;
				else
					curp--;
				if ((curp >= CUR_COUNT) || (curp <= 0))
					/* switch directions */
					if (inc_or_dec == INCREMENT)
						{
						inc_or_dec = DECREMENT;
						curp--;
						}
					else
						{
						inc_or_dec = INCREMENT;
						curp++;
						}
				continue;

			default:
				printf("unknown event.\n");
				continue;

			}
		}

	/* NOTREACHED */
}

