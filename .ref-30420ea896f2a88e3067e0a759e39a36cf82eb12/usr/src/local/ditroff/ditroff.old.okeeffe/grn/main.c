/*	main.c	1.8	(Berkeley) 83/09/23
 *
 *	This file contains the main and file system dependent routines
 * for processing gremlin files into troff input.  The program watches
 * input go by to standard output, only interpretting things between .GS
 * and .GE lines.  Default values may be overridden, as in gprint, on the
 * command line and are further overridden by commands in the input.  A
 * description of the command-line options are listed below.  A space is
 * NOT required for the operand of an option.
 *
 *	command options are:
 *
 *	-r ss	sets gremlin's roman font to troff font ss.  Also for -i,
 *		-b and -s for italics, bold, and special fonts.  This does
 *		NOT affect font changes imbedded into strings.  A \fI, for
 *		instance, will get the italics font regardless of what -i
 *		is set to.
 *
 *	-1 #	sets point size 1 to #.  also for -2, -3, -4.  Defaults
 *		are 12, 16, 24, 36.
 *
 *	-n #	set narrow line thickness to # pixels.  Also for -m (medium)
 *		and -t (thick).  Defaults are 1, 3, and 5 pixels.
 *
 *	-x #	scale the picture by x (integer or decimal).
 *
 *	-T dev	Prepare output for "dev" printer.  Default is for the varian
 *		and versatec printers.  Devices acceptable are:  ver, var, ip.
 *
 *	-p	prompt user for fonts, sizes and thicknesses.
 *
 *
 *		Inside the GS and GE, there are commands similar to command-
 *	    line options listed above.  At most one command may reside on each
 *	    line, and each command is followed by a parameter separated by white
 *	    space.  The commands are as follows, and may be abbreviated down to
 *	    one character (with exception of "scale" down to "sc") and may be
 *	    upper or lower case.
 *
 *			   1, 2, 3, 4  -  set size 1, 2, 3, or 4 (followed
 *	roman, italics, bold, special  -  set gremlin's fonts to any other
 *					  troff font (one or two characters)
 *			     scale, x  -  scale is IN ADDITION to the global
 *					  scale factor from the -x option.
 *		narrow, medium, thick  -  set pixel widths of lines.
 *				 file  -  set the file name to read the
 *					  gremlin picture from.
 *			width, height  -  these two commands override any
 *					  scaling factor that is in effect,
 *					  and forces the picture to fit into
 *					  either the height or width specified,
 *					  whichever makes the picture smaller.
 *					  The operand for these two commands is
 *					  a floating-point number in units of
 *					  inches
 *
 *	Troff number registers used:  g1 through g9.  g1 is the width of the
 *	picture, and g2 is the height.  g2-g6 save information, g8 and g9 are
 *	used for text processing and g7 is reserved.
 */


#include <ctype.h>
#include "gprint.h"
#include "dev.h"

extern char *malloc();
extern char *rindex();

/* database imports */

extern HGPrintElt();
extern ELT *DBInit(), *DBRead();
extern POINT *PTInit(), *PTMakePoint();


#define GREMLIB		"/usr/local/gremlib/"
#define DEVDIR		"/usr/lib/font/dev"
#define DEFAULTDEV	"var"

#define MAXINLINE	100		/* input line length */
#define DEFTHICK	3		/* default thicknes */
#define DEFSTYLE	SOLID		/* default line style */

#define SCREENtoINCH	0.02		/* scaling factor, screen to inches */
#define BIG	100000000000.0		/* unweildly large floating number */

#define JLEFT		-1		/* justification constants - for the */
#define JCENTER		0		/*    whole picture - where it will */
#define JRIGHT		1		/*    get placed within the line */


char	SccsId[] = "main.c	1.8	83/09/23";

char	*printer = DEFAULTDEV;	/* device to look up resolution of */
double	res;			/* that printer's resolution goes here */

int	linethickness;		/* brush styles */
int	linmod;
int	lastx;			/* point registers for printing elements */
int	lasty;
int	lastyline;		/* a line's vertical position is NOT the same */
				/* after that line is over, so for a line of */
				/* drawing commands, vertical spacing is kept */
				/* in lastyline */
double	scale = SCREENtoINCH;	/* default scale to map gremlin screen to inches
				   (modified by -x command-line option) */

		    /* list of prompts for asking user to set default values */
char	*prompt[] = {				  /* used only for -p option */
		"Roman font name? (%s): ",	"Italic font name? (%s): ",
		"Bold font name? (%s): ",	"Special font name? (%s): ",
		"font size 1? (%s): ",		"font size 2? (%s): ",
		"font size 3? (%s): ",		"font size 4? (%s): ",
	};

			/* these are the default fonts, sizes, */
			/*   line styles, and thicknesses.  These */
			/*   can be modified from command-line */
			/*   options, and are reset each time the */
			/*   start of a picture (.GS) is found. */

char	*defstring[] = {
		"R\0         ", "I\0         ", "B\0         ", "S\0         ",
		"10\0        ", "16\0        ", "24\0        ", "36\0        "
	};
int	defthick[STYLES] = { 1, 1, 5, 1, 1, 3 };	/* defaults... */
int	style[STYLES] = { DOTTED, DOTDASHED, SOLID, DASHED, SOLID, SOLID };
int	thick[STYLES];	/* thicknesses set by defaults, then by commands */
char	*tfont[FONTS];	/* fonts originally set to defstring values, then */
char 	*tsize[SIZES];	/*    optionally changed by commands inside grn */

double	xscale;		/* scaling factor from individual pictures */
double	troffscale;	/* scaling factor at output time */ 
double	width;		/* user-request maximum width for picture (in inches) */
double	height;		/* user-request height */

double	toppoint;		/* remember the picture */
double	bottompoint;		/* bounds in these variables */
double	leftpoint;
double	rightpoint;

int	ytop;			/* these are integer versions of the above */
int	ybottom;		/* so not to convert each time they're used */
int	xleft;
int	xright;

int	linenum = 0;			/* line number of input file */
char	inputline[MAXINLINE];		/* spot to filter through the file */
char	*c1 = inputline;		/* c1, c2, and c3 will be used to */
char	*c2 = inputline + 1;		/* hunt for lines that begin with */
char	*c3 = inputline + 2;		/* ".GS" by looking individually */
char	GScommand[MAXINLINE];		/* put user's ".GS" command line here */
char	gremlinfile[50];		/* filename to use for a picture */

char *doinput();


/*----------------------------------------------------------------------------*
 | Routine:	main (argument_count, argument_pointer)
 |
 | Results:	parses the command line, accumulating input file names, then
 |		reads the inputs, passing it directly to output until a ".GS"
 |		line is read.  Main then passes control to "conv" to do the
 |		gremlin file conversions.
 |
 | Bugs:	a -p option ALWAYS reads standard input.  Even if the input
 |		file is coming in that way.
 *----------------------------------------------------------------------------*/

main(argc, argv)
int argc;
char **argv;
{
	register FILE *fp = stdin;
	register int k;
	register char c;
	char *file[50], string[50];
	float mult;
	int brsh, gfil = 0;

	char *operand();


	while (--argc) {
	    if (**++argv != '-')
		file[gfil++] = *argv;
	    else
	      switch (c = (*argv)[1]) {

		case '1':	/* select sizes */
		case '2':
		case '3':
		case '4':
			strcpy(defstring[c + FONTS-'1'], operand(&argc, &argv));
			break;
		case 'r':	/* select Roman font */
			strcpy(defstring[0], operand(&argc, &argv));
			break;
		case 'i':	/* select italics font */
			strcpy(defstring[1], operand(&argc, &argv));
			break;
		case 'b':	/* select bold font */
			strcpy(defstring[2], operand(&argc, &argv));
			break;
		case 's':	/* select special font */
			strcpy(defstring[3], operand(&argc, &argv));
			break;
		case 'n':	/* select narrow brush width */
			(void) sscanf(operand(&argc, &argv), "%d", &brsh);
			defthick[0]=defthick[1]=defthick[3]=defthick[4]=brsh;
			break;
		case 't':	/* select thick brush width */
			(void) sscanf(operand(&argc, &argv), "%d", &brsh);
			defthick[2] = brsh;
			break;
		case 'm':	/* select medium brush width */
			(void) sscanf(operand(&argc, &argv), "%d", &brsh);
			defthick[5] = brsh;
			break;
		case 'x':	/* select scale */
			sscanf(operand(&argc, &argv),"%f", &mult);
			scale *= mult;
			break;
		case 'T':	/* final output typesetter name */
			printer = operand(&argc, &argv);
			break;
		case 'p':	/* prompt for font and size parameters */
			for (k = 0; k < 8; k++) {
			    fprintf(stderr, prompt[k], defstring[k]);
			    gets(string);
			    if (*string != '\0') strcpy(defstring[k], string);
			}
			fprintf(stderr,"narrow brush size? (%d): ",defthick[0]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    defthick[0] = defthick[1] = defthick[3]
							= defthick[4] = brsh;
			}
			fprintf(stderr,"medium brush size? (%d): ",defthick[5]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    defthick[5] = brsh;
			}
			fprintf(stderr, "thick brush size? (%d): ",defthick[2]);
			gets(string);
			if (*string != '\0') {
			    sscanf(string, "%d", &brsh);
			    defthick[2] = brsh;
			}
			break;
		default:
			fprintf(stderr, "unknown switch: %c\n", c);
	    }
	}
				/* set the resolution for an output device */
	getres(printer);	/* named in "printer" */

	if (gfil == 0) {	/* no filename, use standard input */
		file[0] = NULL;
		gfil++;
	}

	for (k=0; k<gfil; k++) {
		if (file[k] != NULL) {
			if ((fp = fopen(file[k], "r")) == NULL) {
			    fprintf(stderr, "grn: can't open %s\n", file[k]);
			    continue;
			}
		} else {
			fp = stdin;
		}

		while (doinput(fp) != NULL) {
		    if (*c1 == '.' && *c2 == 'G' && *c3 == 'S') {
			conv(fp, linenum);
		    } else {
			fputs(inputline, stdout);
		    }
		}
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * operand (& argc,  & argv)
 |
 | Results:	returns address of the operand given with a command-line
 |		option.  It uses either "-Xoperand" or "-X operand", whichever
 |		is present.  The program is terminated if no option is present.
 |
 | Side Efct:	argc and argv are updated as necessary.
 *----------------------------------------------------------------------------*/

char *operand(argcp, argvp)
int * argcp;
char ***argvp;
{
	if ((**argvp)[2]) return(**argvp + 2); /* operand immediately follows */
	if ((--*argcp) <= 0) {			/* no operand */
	    fprintf (stderr, "command-line option operand missing.\n");
	    exit(1);
	}
	return(*(++(*argvp)));			/* operand is next word */
}


/*----------------------------------------------------------------------------*
 | Routine:	getres (device_name)
 |
 | Results:	sets "res" to the resolution of the output device specified
 |		by the string dev.
 *----------------------------------------------------------------------------*/

getres(name)
char *name;
{
	int fin;
	struct dev device;
	char temp[60];

	sprintf(temp, "%s%s/DESC.out", DEVDIR, name);
	if ((fin = open(temp, 0)) < 0) {
	    fprintf(stderr, "can't open tables for %s\n", temp);
	    exit(1);
	}
	read(fin, &device, sizeof(struct dev));
	res = (double) device.res;
	close(fin);
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * doinput (file_pointer)
 |
 | Results:	a line of input is read into "inputline".
 |
 | Side Efct:	"linenum" is incremented.
 |
 | Bugs:	lines longer than MAXINLINE are NOT checked, except for 
 |		updating "linenum"
 *----------------------------------------------------------------------------*/

char *doinput(fp)
FILE *fp;
{
    register char *k;


    if ((k = fgets(inputline, MAXINLINE, fp)) == NULL)
	return k;
    if (index (inputline, '\n'))	/* ++ only if it's a complete line */
	linenum++;
    return (char*) !NULL;
}


/*----------------------------------------------------------------------------*
 | Routine:	initpic ( )
 |
 | Results:	sets all parameters to the normal defaults, possibly overridden
 |		by the command line flags.  Initilaize the picture variables,
 |		and output the startup commands to troff to begin the picture.
 *----------------------------------------------------------------------------*/

initpic()
{
    register int i;

    for (i = 0; i < STYLES; i++) {	/* line thickness defaults */
	thick[i] = defthick[i];
    }
    for (i = 0; i < FONTS; i++) {	/* font name defaults */
	tfont[i] = defstring[i];
    }
    for (i = 0; i < SIZES; i++) {	/* font size defaults */
	tsize[i] = defstring[FONTS + i];
    }

    gremlinfile[0] = 0;		/* filename is "null" */

    toppoint = BIG;		/* set the picture bounds out */
    bottompoint = 0.0;		/* of range so they'll be set */
    leftpoint = BIG;		/* by "savebounds" on input */
    rightpoint = 0.0;

    xscale = scale;		/* default scale of individual pictures */
    width = 0.0;		/* size specifications input by user */
    height = 0.0;

    linethickness = DEFTHICK;	/* brush styles */
    linmod = DEFSTYLE;
}


/*----------------------------------------------------------------------------*
 | Routine:	conv (file_pointer, starting_line)
 |
 | Results:	at this point, we just passed a ".GS" line in the input file.
 |		conv reads the input and calls "interpret" to process commands,
 |		gathering up information until a ".GE" line is found.  It then
 |		calls "HGPrint" to do the translation of the gremlin file to
 |		troff commands.
 *----------------------------------------------------------------------------*/

conv(fp, baseline)
register FILE *fp;
int baseline;
{
	register FILE *gfp = NULL;
	register int done = 0;
	register ELT *e;
	ELT *PICTURE;
	double temp;
	POINT ptr;


	initpic();			/* set defaults, ranges, etc. */
	strcpy (GScommand, inputline);	/* save ".GS" line for later */
	do {
	    done = (doinput(fp) == NULL);		     /* test for EOF */
	    done |= (*c1 == '.' && *c2 == 'G' && *c3 == 'E');	 /*  and .GE */

	    if (done) {
		if (!gremlinfile[0]) {
		    fprintf(stderr, "grn: at line %d: no picture filename.\n",
								    baseline);
		    return;
		}
		if ((gfp = fopen(gremlinfile, "r")) == NULL) {
		    char name[100];	/* if the file isn't in the current */
					/* directory, try the gremlin library */
		    sprintf(name, "%s%s", GREMLIB, gremlinfile);
		    if ((gfp = fopen(name, "r")) == NULL) {
			fprintf(stderr, "grn: can't open %s\n", gremlinfile);
			return;
		    }
		}
		PICTURE = DBRead(gfp);			/* read picture file */
		fclose(gfp);
		if (DBNullelt(PICTURE))
		    return;		/* if a request is made to make the */
					/* picture fit into a specific area, */
					/* set the scale to do that. */
		if ((temp = bottompoint - toppoint) < 0.1) temp = 0.1;
		temp = (height != 0.0) ? height / temp  : BIG;
		if ((troffscale = rightpoint - leftpoint) < 0.1) troffscale=0.1;
		troffscale = (width != 0.0) ? width / troffscale  : BIG;
		if (temp == BIG && troffscale == BIG) {
		    troffscale = xscale;
		} else {
		    if (temp < troffscale) troffscale = temp;
		}
		troffscale *= res;	/* change to device units from inches */

		ytop = toppoint * troffscale;		/* calculate integer */
		ybottom = bottompoint * troffscale;	/* versions of the */
		xleft = leftpoint * troffscale;		/* picture limits */
		xright = rightpoint * troffscale;
					/* save stuff in number registers, */
					/*   register g1 = picture width and */
					/*   register g2 = picture height, */
					/*   set vertical spacing, no fill, */
					/*   and break (to make sure picture */
					/*   starts on left), and put out the */
					/*   user's ".GS" line. */
		printf(".nr g1 %d\n.nr g2 %d\n", xright-xleft, ybottom-ytop);
		printf(".br\n%s", GScommand);
		printf(".nr g3 \\n(.f\n.nr g4 \\n(.s\n");
		printf(".nr g5 \\n(.v\n.nr g6 \\n(.u\n.nf\n.vs 0");

		lastx = xleft;		/* note where we are, (upper left */
		lastyline = lasty = ytop;	/* corner of the picture) */

		e = PICTURE;
		while (!DBNullelt(e)) {
		    HGPrintElt(e);	/* traverse picture;  print elements */
		    e = DBNextElt(e);
		}
					/* end picture at lower left */
		ptr.x = leftpoint;
		ptr.y = bottompoint;
		tmove(&ptr);		/* restore default line parameters, */
					/* put out the ".GE" line from user */
					/* then restore everything to the way */
					/* it was before the .GS */
		printf("\\D't %du'\\D's %du'\n", DEFTHICK, DEFSTYLE);
		printf(".ft \\n(g3\n.ps \\n(g4\n");
		printf(".vs \\n(g5u\n.if \\n(g6 .fi\n%s", inputline);
	    } else {
		interpret(inputline);	/* take commands from the input file */
	    }
	} while (!done);
}


/*----------------------------------------------------------------------------*
 | Routine:	savebounds (x_coordinate, y_coordinate)
 |
 | Results:	keeps track of the maximum and minimum extent of a picture
 |		in the global variables:  left-, right-, top- and bottompoint.
 |		"savebounds" assumes that the points have been oriented to
 |		the correct direction.  No scaling has taken place, though.
 *----------------------------------------------------------------------------*/

savebounds(x, y)
float x;
float y;
{
    if (x < leftpoint) leftpoint = x;
    if (x > rightpoint) rightpoint = x;
    if (y < toppoint) toppoint = y;
    if (y > bottompoint) bottompoint = y;
}


/*----------------------------------------------------------------------------*
 | Routine:	interpret (character_string)
 |
 | Results:	commands are taken from the input string and performed.
 |		Commands are separated by the endofline, and are of the
 |		format:
 |			string1 string2
 |
 |		where string1 is the command and string2 is the argument.
 |
 | Side Efct:	font and size strings, plus the gremlin file name and the
 |		width and height variables are set by this routine.
 *----------------------------------------------------------------------------*/

interpret (line)
char *line;
{
    char str1[MAXINLINE];
    char str2[MAXINLINE];
    register char *chr;

    sscanf(line, "%80s%80s", &str1[0], &str2[0]);
    for (chr = &str1[0]; *chr; chr++)		/* convert command to */
	if(isupper(*chr)) *chr = tolower(*chr);		/* lower case */
    switch (str1[0]) {

	case '1':
	case '2':	/* font sizes */
	case '3':
	case '4':
	    tsize[str1[0] - '1'] = malloc(strlen(str2) + 1);
	    strcpy(tsize[str1[0] - '1'], str2);
	    break;

	case 'r':	/* roman */
	    tfont[0] = malloc(strlen(str2) + 1);
	    strcpy(tfont[0], str2);
	    break;

	case 'i':	/* italics */
	    tfont[1] = malloc(strlen(str2) + 1);
	    strcpy(tfont[1], str2);
	    break;

	case 'b':	/* bold */
	    tfont[2] = malloc(strlen(str2) + 1);
	    strcpy(tfont[2], str2);
	    break;

	case 's':	/* special */
	    if (str1[1] == 'c') goto scalecommand;
	    tfont[3] = malloc(strlen(str2) + 1);
	    strcpy(tfont[3], str2);
	    break;

	case 't':	/* thick */
	    thick[2] = atoi(str2);
	    break;

	case 'm':	/* medium */
	    thick[5] = atoi(str2);
	    break;

	case 'n':	/* narrow */
	    thick[0] = thick[1] = thick[3] = thick[4] = atoi(str2);
	    break;

	case 'x':	/* x */
	scalecommand:	/* scale */
	    xscale *= atof(str2);
	    if (xscale < 0.0) xscale = -xscale;
	    break;

	case 'f':	/* file */
	    strcpy(gremlinfile, str2);
	    break;

	case 'w':	/* width */
	    width = atof(str2);
	    if (width < 0.0) width = -width;
	    break;

	case 'h':	/* height */
	    height = atof(str2);
	    if (height < 0.0) height = -height;
	    break;

	default: 
	    break;
    };
}
