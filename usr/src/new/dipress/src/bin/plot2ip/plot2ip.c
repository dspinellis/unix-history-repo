/*
 * (c) Copyright 1986, Xerox Corporation
 *
 * pl2ip: convert Unix plot format to and interpress master
 *
 * Generally, this program is straightforward.  The main complication comes
 * from the desire to string contigious lines into one Interpress trajectory.
 * The main modivation for this is to provide for nice dashed lines on printers
 * that accept IP 3.0 and higher.  It can also be used to round the corners
 * of other lines.
 *
 * HISTORY
 * 22-Apr-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Created.
 *
 */

#include <stdio.h>
#include "iptokens.h"
#include "operator.h"
#include "literal.h"

/* #define DEBUG  /* */

#define MAXSTRINGSIZE	128

#define TRUE		1
#define FALSE		0

#define F_font		10
#define STROKE_WIDTH	15L	/* was 50 */	/* width for drawing lines */
#define STROKEEND_BUTT	1L		/* end lines hard */

int HaveTrajectory = FALSE,		/* is a trajectory being built? */
    CurrentX = 0,			/* the current X position */	
    CurrentY = 0,			/* the current Y position */
    Pitch = 10;				/* pitch of the default font */


/*
 * declare the Unix plot commands
 */

#define MOVECMD		'm'
#define CONTCMD		'n'
#define POINTCMD	'p'
#define LINECMD		'l'
#define LABELCMD	't'
#define ARCCMD		'a'
#define CIRCLECMD	'c'
#define ERASECMD	'e'
#define LINEMODCMD	'f'
#define SPACECMD	's'


main(argc, argv)
    int argc;
    char *argv[]; {
	char *outputName = NULL;
	int command, ipFD,
	    textSize = 8,		/* size of text (in points) */
	    c;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "o:s:")) != EOF)
	    switch (c) {
		case 'o':
			outputName = optarg;
			break;

		case 's':
			textSize = atoi(optarg);
			break;
	    }


	if( outputName != NULL ) {
		if( (ipFD = creat(outputName, 0644)) < 0 ) {
			fprintf(stderr, "pl2ip: can't open %s for writing\n", outputName);
			exit(1);
		}
	} else
		ipFD = 1;	/* std. output */

	ip_select(ipFD);
	AppendOp(OP_beginBlock);
	AppendOp(OP_beginBody);
	AppendOp(OP_endBody);	/* end preamble */
	AppendOp(OP_beginBody); /* begin page 1 */
#ifdef notdef
	SetupFont("Xerox/XC1-1-1/Classic", (10.*2.54)/(72.0*100.0), F_font);
	Setfont(F_font);
#else
	AppendIdentifier("Xerox");
	AppendIdentifier("XC1-1-1");
	AppendIdentifier("Terminal");
	AppendInteger(3L);
	AppendOp(OP_makevec);
	AppendOp(OP_findfont);
	AppendRational(4096L * textSize, 36L*13);
	AppendOp(OP_scale);
	AppendOp(OP_modifyfont);
	AppendInteger((long) I_font);
	AppendOp(OP_iset);
#endif

	/* set coordinate system */
	AppendRational( (long)(6.5*2.54*100.0 + .5), 4096L*100L*100L);
	AppendOp(OP_scale);
	AppendOp(OP_concatt);

	/* set the stroke width */
	AppendInteger(STROKE_WIDTH);		/* stroke width */
	AppendInteger(15L);		/* stroke width imager variable */
	AppendOp(OP_iset);

#ifdef notdef
	/* set stroke end */
	AppendInteger(STROKEEND_BUTT);
	AppendInteger(16L);		/* stroke end imager variable */
	AppendOp(OP_iset);
#endif

	while( (command = getchar()) != EOF ) {
		switch( command ) {
		case MOVECMD:		move();	break;
		case CONTCMD:		cont(); break;
		case POINTCMD:		point(); break;
		case LINECMD:		line(); break;
		case LABELCMD:		label(); break;
		case ARCCMD:		arc(); break;
		case CIRCLECMD:		circle(); break;
		case ERASECMD:		erase(); break;
		case LINEMODCMD:	lineMod(); break;
		case SPACECMD:		space(); break;

		default:
			printf("unknown command: ");

			if( command <= ' ' || command >= 0177 )
				printf("\\%03o", command);
			else
				putchar(command);

			putchar('\n');
		}
	}

	if( HaveTrajectory )
		ShowTrajectory();

	/* wrap it up */
	AppendOp(OP_endBody);
	AppendOp(OP_endBlock);
	ip_flush();
	exit(0);
}

getShort() {
	unsigned char c;

	c = getchar();
	return (getchar() << 8) | c;
}


/*
 * move: The next four bytes give a new currect point
 *
 * The problem here is that there are two kind of moves: those
 * for lines and those for characters.  We try to keep the apart.
 */

move() {
	if( HaveTrajectory )
		ShowTrajectory();

	CurrentX = getShort();
	CurrentY = getShort();
#ifdef DEBUG
	printf("move to (%d, %d)\n", CurrentX, CurrentY);
#endif DEBUG
}


/*
 * cont: Draw a line from the current point to the point given by the next
 *	four bytes.  See plot(1g)
 */

cont() {
	int x, y;

	x = getShort();
	y = getShort();
#ifdef DEBUG
	printf("continue to (%d, %d)\n", x, y);
#endif DEBUG

	if( ! HaveTrajectory ) {
		AppendInteger((long) CurrentX);
		AppendInteger((long) CurrentY);
		AppendOp(OP_moveto);
		HaveTrajectory = TRUE;
	}

	AppendInteger((long) x);
	AppendInteger((long) y);
	AppendOp(OP_lineto);
	CurrentX = x;
	CurrentY = y;
}


/*
 * point: Plot the point given by the next four bytes.
 */

point() {
	int x, y;

	if( HaveTrajectory )
		ShowTrajectory();

	x = getShort();
	y = getShort();
#ifdef DEBUG
	printf("point at (%d, %d)\n", x, y);
#endif DEBUG
	AppendInteger((long) x);
	AppendInteger((long) y);
	CurrentX = x;
	CurrentY = y;
}


/*
 * line: Draw a line from the point given by the next four bytes to the point
 *	given by the following four bytes.
 */

line() {
	int x0, y0, x1, y1;

	x0 = getShort();
	y0 = getShort();
	x1 = getShort();
	y1 = getShort();
#ifdef DEBUG
	printf("line from (%d, %d) to (%d, %d)\n", x0, y0, x1, y1);
#endif DEBUG

	if( ! HaveTrajectory ) {
		AppendInteger((long) x0);
		AppendInteger((long) y0);
		AppendOp(OP_moveto); }
	else
		if( x0 != CurrentX  ||  y0 != CurrentY ) {
			ShowTrajectory();

			AppendInteger((long) x0);
			AppendInteger((long) y0);
			AppendOp(OP_moveto); }

	AppendInteger((long) x1);
	AppendInteger((long) y1);
	AppendOp(OP_lineto);
	HaveTrajectory = TRUE;
	CurrentX = x1;
	CurrentY = y1;
}


/*
 * label: Place the following ASCII string so that its first character falls
 *	on the current point.  The string is terminated by a newline.
 *
 * Unspoken in the documentation but implied by practice is 
 * that the current position is changed to the end of text string.
 * This artifact can cause problems because we don't know the width
 * of characters in a font.
 */

label() {
	char labelString[MAXSTRINGSIZE];

	if( HaveTrajectory ) {
		printf("pl2ip: opps trying to put text at end of line!\n");
		ShowTrajectory();
	}

	(void) gets(labelString);
#ifdef DEBUG
	printf("output label string: '%s'\n", labelString);
#endif DEBUG
	AppendInteger((long) CurrentX);
	AppendInteger((long) CurrentY);
	AppendOp(OP_setxy);
	AppendString(labelString);
	AppendOp(OP_show);

	CurrentX += strlen(labelString)*(4096.0/(Pitch * 6.5)) + .4999999;
}


/*
 * arc: The first four bytes give the center, the next four give the starting
 *	proint and the last four give the end point of a circular arc.  The
 *	least significant coordinate of the end point is used to only to
 *	determine the quadrant.  The arc is drawn counter-clockwise.
 */

arc() {
#ifdef DEBUG
	int	centerX, centerY,
		startX, startY,
		endX, endY;

	if( HaveTrajectory )
		ShowTrajectory();

	centerX = getShort();
	centerY = getShort();
	startX = getShort();
	startY = getShort();
	endX = getShort();
	endY = getShort();
	printf("arc with center (%d, %d), starting at (%d, %d) and ending at (%d, %d)\n", centerX, centerY, startX, startY, endX, endY);
#endif DEBUG
}


/*
 * circle: The first four bytes give the center of the circle, the next
 *	two the radius.
 */

circle() {
#ifdef DEBUG
	int	centerX, centerY,
		radius;

	if( HaveTrajectory )
		ShowTrajectory();

	centerX = getShort();
	centerY = getShort();
	radius = getShort();
	printf("circle at (%d, %d) with radius %d\n", centerX, centerY, radius);
#endif DEBUG
}


/*
 * erase: Start another frame of output
 */

erase() {
	if( HaveTrajectory )
		ShowTrajectory();

#ifdef DEBUG
	printf("erase\n");
#endif DEBUG
}


/*
 * linemod: Take the following string, up to a newline, as the style for
 *	drawing further lines.  The syles are 'dotted', 'solid', 'longdashed',
 *	'shortdashed', and 'dotdashed'.
 */

lineMod() {
	char lineType[MAXSTRINGSIZE];

	if( HaveTrajectory )
		ShowTrajectory();

	(void) gets(lineType);
#ifdef DEBUG
	printf("change line mode to '%s'\n", lineType);
#endif DEBUG
}

/*
 * space: The next four bytes give the lower left corner of the plotting area;
 *	the following four give the upper right corner.  The plot will be
 * 	magnified or reduced to fit the device as closely as possible.
 */

space() {
#ifdef DEBUG
	int	lowerLeftX, lowerLeftY,
		upperRightX, upperRightY;

	lowerLeftX = getShort();
	lowerLeftY = getShort();
	upperRightX = getShort();
	upperRightY = getShort();
	printf("space with lower left at (%d, %d) and upper right at (%d, %d)\n", lowerLeftX, lowerLeftY, upperRightX, upperRightY);
#endif DEBUG
} 


/*
 * draw the current trajectory
 */

ShowTrajectory() {
	AppendOp(OP_maskstroke);
	HaveTrajectory = FALSE;
}
