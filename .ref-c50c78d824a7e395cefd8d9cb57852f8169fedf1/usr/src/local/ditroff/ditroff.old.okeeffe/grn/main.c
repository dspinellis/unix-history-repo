/*	main.c	1.24	(Berkeley)	86/04/14
 *
 *	This file contains the main and file system dependent routines
 * for processing gremlin files into troff input.  The program watches
 * input go by to standard output, only interpretting things between .GS
 * and .GE lines.  Default values (font, size, scale, thickness) may be
 * overridden with a "default" command and are further overridden by
 * commands in the input.  A description of the command-line options are
 * listed below.  A space is NOT required for the operand of an option.
 *
 *	command options are:
 *
 *	-L dir	set the library directory to dir.  If a file is not found
 *		in the current directory, it is looked for in dir (default
 *		is /usr/lib/gremlib).
 *
 *	-T dev	Prepare output for "dev" printer.  Default is for the varian
 *	-P dev	and versatec printers.  Devices acceptable are:  ver, var, ip.
 *
 *		Inside the GS and GE, commands are accepted to reconfigure
 *	    the picture.  At most one command may reside on each line, and
 *	    each command is followed by a parameter separated by white space.
 *	    The commands are as follows, and may be abbreviated down to one
 *	    character (with exception of "scale" and "stipple" down to "sc"
 *	    and "st") and may be upper or lower case.
 *
 *			      default  -  make all settings in the current
 *					  .GS/.GE the global defaults.
 *					  Height, width and file are NOT saved.
 *			   1, 2, 3, 4  -  set size 1, 2, 3, or 4 (followed
 *					  by an integer point size).
 *	roman, italics, bold, special  -  set gremlin's fonts to any other
 *					  troff font (one or two characters)
 *			   stipple, l  -  use a stipple font for polygons.  Arg
 *					  is troff font name.  No Default.  Can
 *					  use only one stipple font per picture.
 *					  (see below for stipple font index)
 *			     scale, x  -  scale is IN ADDITION to the global
 *					  scale factor from the default.
 *			   pointscale  -  turn on scaling point sizes to
 *					  match "scale" commands.  (optional
 *					  operand "off" to turn it off)
 *		narrow, medium, thick  -  set pixel widths of lines.
 *				 file  -  set the file name to read the
 *					  gremlin picture from.  If the file
 *					  isn't in the current directory, the
 *					  gremlin library is tried.
 *			width, height  -  these two commands override any
 *					  scaling factor that is in effect,
 *					  and forces the picture to fit into
 *					  either the height or width specified,
 *					  whichever makes the picture smaller.
 *					  The operand for these two commands is
 *					  a floating-point number in units of
 *					  inches
 *		        oldstipplemap  -  use the old-style stipple mapping.
 *					  THE FOLLOWING COMMANDS ARE IGNORED
 *					  UNLESS OLDSTIPPLEMAP IS SPECIFIED.
 *     l1, l2, l3, l4, l5, l6, l7, l8  -  set association between stipples
 *					  (1 - 8) and the stipple font file
 *					  index.  Valid cifplot indices are
 *					  1 - 32 (although 24 is not defined)
 *					  and valid unigrafix indices are
 *					  1 - 64.  Nonetheless, any number
 *					  between 0 and 255 is accepted since
 *					  new stipple fonts may be added.
 *					  An integer operand is required.
 *
 *	Troff number registers used:  g1 through g9.  g1 is the width of the
 *	picture, and g2 is the height.  g3, and g4, save information, g8
 *	and g9 are used for text processing and g5-g7 are reserved.
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


#ifndef GREMLIB
#define GREMLIB		"/usr/local/gremlib/"
#endif

#define SUN_SCALEFACTOR 0.70

#ifndef DEVDIR
#define DEVDIR		"/usr/lib/font"
#endif
#define DEFAULTDEV	"va"
#define DEFSTIPPLE	"cf"

#define MAXINLINE	100		/* input line length */
#define DEFTHICK	3		/* default thicknes */
#define DEFSTYLE	SOLID		/* default line style */

#ifdef oldversion
#define SCREENtoINCH	0.02		/* scaling factor, screen to inches */
#endif

double SCREENtoINCH;			/* scaling factor, screen to inches */

#define BIG	999999999999.0		/* unweildly large floating number */


static char sccsid[] = "@(#) (Berkeley) %G%";

char	*printer = DEFAULTDEV;	/* device to look up resolution of */
char	*gremlib = GREMLIB;	/* place to find files after current dir. */
double	res;			/* that printer's resolution goes here */

int	linethickness;		/* brush styles */
int	linmod;
int	lastx;			/* point registers for printing elements */
int	lasty;
int	lastyline;		/* a line's vertical position is NOT the same */
				/* after that line is over, so for a line of */
				/* drawing commands, vertical spacing is kept */
				/* in lastyline */

			/* these are the default fonts, sizes, line styles, */
			/*   and thicknesses.  These can be modified from a */
			/*   "default" command and are reset each time the  */
			/*   start of a picture (.GS) is found.		    */

char *	deffont[] = {  "R", "I", "B", "S"  };
int	defsize[] = {  10, 16, 24, 36  };
int	defthick[STYLES] = {  1, 1, 5, 1, 1, 3  };
int	defstipple_index[NSTIPPLES] = { 1, 3, 12, 14, 16, 19, 21, 23 };
int	style[STYLES] = {  DOTTED, DOTDASHED, SOLID, DASHED, SOLID, SOLID  };
double	scale = 1.0;		/* no scaling, default */
int	defpoint = 0;		/* flag for pointsize scaling */
char *  defstipple = (char *) 0;

int	thick[STYLES];	/* thicknesses set by defaults, then by commands */
char	*tfont[FONTS];	/* fonts originally set to deffont values, then */
int 	tsize[SIZES];	/*    optionally changed by commands inside grn */
int	stipple_index[NSTIPPLES];	/* stipple font file indices */
char *  stipple;

double	xscale;		/* scaling factor from individual pictures */
double	troffscale;	/* scaling factor at output time */ 
double	width;		/* user-request maximum width for picture (in inches) */
double	height;		/* user-request height */
int	pointscale;	/* flag for pointsize scaling */
int	setdefault;	/* flag for a .GS/.GE to remember all settings */

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
char	gremlinfile[MAXINLINE];		/* filename to use for a picture */
int	SUNFILE = FALSE;		/* TRUE if SUN gremlin file */
int	oldstipmap = FALSE;		/* TRUE if old-style stipple mapping */

char *doinput();


/*----------------------------------------------------------------------------*
 | Routine:	main (argument_count, argument_pointer)
 |
 | Results:	parses the command line, accumulating input file names, then
 |		reads the inputs, passing it directly to output until a ".GS"
 |		line is read.  Main then passes control to "conv" to do the
 |		gremlin file conversions.
 *----------------------------------------------------------------------------*/

main(argc, argv)
int argc;
char **argv;
{
	register FILE *fp;
	register int k;
	register char c;
	register gfil = 0;
	char *file[50];

	char *operand();
	char *getenv();


	if (fp = (FILE *) getenv("PRINTER")) printer = (char *) fp;
	if (fp = (FILE *) getenv("TYPESETTER")) printer = (char *) fp;
	while (--argc) {
	    if (**++argv != '-')
		file[gfil++] = *argv;
	    else
	      switch (c = (*argv)[1]) {

		case 0:
			file[gfil++] = NULL;
			break;

		case 'P':
		case 'T':	/* final output typesetter name */
			printer = operand(&argc, &argv);
			break;

		case 'L':	/* set library directory */
			gremlib = operand(&argc, &argv);
			break;

		default:
			error("unknown switch: %c", c);
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
			    error("can't open %s", file[k]);
			    exit(1);
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
 | Routine:	error (control_string, args, . . . )
 |
 | Results:	prints ("grn: ", the control_string + args, "\n") to stderr
 *----------------------------------------------------------------------------*/

/* VARARGS1 */
error(s, a1, a2, a3, a4)
char *	s;
{
	fprintf(stderr, "grn: ");
	fprintf(stderr, s, a1, a2, a3, a4);
	fprintf(stderr, "\n");
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
	    error("command-line option operand missing.");
	    exit(8);
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

	sprintf(temp, "%s/dev%s/DESC.out", DEVDIR, name);
	if ((fin = open(temp, 0)) < 0) {
	    error("can't open tables for %s", temp);
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
    char *k;


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
 |		by a setdefault command.  Initilaize the picture variables,
 |		and output the startup commands to troff to begin the picture.
 *----------------------------------------------------------------------------*/

initpic()
{
    register int i;

    for (i = 0; i < STYLES; i++) {	/* line thickness defaults */
	thick[i] = defthick[i];
    }
    for (i = 0; i < FONTS; i++) {	/* font name defaults */
	tfont[i] = deffont[i];
    }
    for (i = 0; i < SIZES; i++) {	/* font size defaults */
	tsize[i] = defsize[i];
    }
    for (i = 0; i < NSTIPPLES; i++) {	/* stipple font file default indices */
	stipple_index[i] = defstipple_index[i];
    }
    stipple = defstipple;

    gremlinfile[0] = 0;		/* filename is "null" */
    setdefault = 0;		/* this is not the default settings (yet) */

    toppoint = BIG;		/* set the picture bounds out */
    bottompoint = -BIG;		/* of range so they'll be set */
    leftpoint = BIG;		/* by "savebounds" on input */
    rightpoint = -BIG;

    pointscale = defpoint;	/* Flag for scaling point sizes default. */
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
	register FILE *gfp = NULL;	/* input file pointer */
	register int done = 0;		/* flag to remember if finished */
	register ELT *e;	/* current element pointer */
	ELT *PICTURE;		/* whole picture data base pointer */
	double temp;		/* temporary calculating area */
	POINT ptr;	/* coordinates of a point to pass to "mov" routine */
	int flyback;	/* flag "want to end up at the top of the picture?" */


	initpic();			/* set defaults, ranges, etc. */
	strcpy (GScommand, inputline);	/* save ".GS" line for later */
	do {
	    done = (doinput(fp) == NULL);		     /* test for EOF */
	    flyback = *c3 == 'F';			   /* and .GE or .GF */
	    done |= (*c1 == '.' && *c2 == 'G' && (*c3 == 'E' || flyback));

	    if (done) {
		if (setdefault) savestate();

		if (!gremlinfile[0]) {
		    if(!setdefault)
			error("at line %d: no picture filename.\n", baseline);
		    return;
		}
		if ((gfp = fopen(gremlinfile, "r")) == NULL) {
		    char name[MAXINLINE]; /* if the file isn't in the current */
					/* directory, try the gremlin library */
		    sprintf(name, "%s%s", gremlib, gremlinfile);
		    if ((gfp = fopen(name, "r")) == NULL) {
			error("can't open %s", gremlinfile);
			return;
		    }
		}
		PICTURE = DBRead(gfp);			/* read picture file */
		fclose(gfp);
		if (DBNullelt(PICTURE))
		    return;		/* if a request is made to make the */
					/* picture fit into a specific area, */
					/* set the scale to do that. */

		SCREENtoINCH = (SUNFILE) ? 0.014 : 0.02;

		if (stipple == (char *) NULL)	/* if user forgot stipple */
		    if (has_polygon(PICTURE))	/* and picture has a polygon */
			stipple = DEFSTIPPLE;	/* then set the default */

		if ((temp = bottompoint - toppoint) < 0.1) temp = 0.1;
		temp = (height != 0.0) ? height / (temp * SCREENtoINCH)  : BIG;
		if ((troffscale = rightpoint - leftpoint) < 0.1) troffscale=0.1;
		troffscale = (width != 0.0) ?
				width / (troffscale * SCREENtoINCH)  : BIG;
		if (temp == BIG && troffscale == BIG) {
		    troffscale = xscale;
		} else {
		    if (temp < troffscale) troffscale = temp;
		}				/* here, troffscale is the */
						/* picture's scaling factor */
		if (pointscale) {
		    register int i;		/* do pointscaling here, when */
					     /* scale is known, before output */

		    for (i = 0; i < SIZES; i++)
			tsize[i] = (int) (troffscale * (double) tsize[i] + 0.5);

		}
						   /* change to device units */
		troffscale *= SCREENtoINCH * res;	/* from screen units */

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
		printf(
".br\n.nr g1 %du\n.nr g2 %du\n%s.nr g3 \\n(.f\n.nr g4 \\n(.s\n\\0\n.sp -1\n",
			xright-xleft, ybottom-ytop, GScommand);

		if (stipple) {		/* stipple requested for this picture */
		    printf(".st %s\n", stipple);
		}

		lastx = xleft;		/* note where we are, (upper left */
		lastyline = lasty = ytop;	/* corner of the picture) */

		e = PICTURE;
		while (!DBNullelt(e)) {	/* traverse picture;  print elements */
		    HGPrintElt(e, baseline);
		    e = DBNextElt(e);
		}
				/* decide where to end picture */
		if (flyback) {		/* end piture at upper left */
		    ptr.x = leftpoint;
		    ptr.y = toppoint;
		} else {		/* end picture at lower left */
		    ptr.x = leftpoint;
		    ptr.y = bottompoint;
		}
		tmove(&ptr);		/* restore default line parameters, */
					/* restore everything to the way */
					/* it was before the .GS, then put */
					/* out the ".GE" line from user */
		printf("\\D't %du'\\D's %du'\n", DEFTHICK, DEFSTYLE);
		if (flyback) {		/* make sure we end up at top of */
		    printf(".sp -1\n");		/* picture if "flying back" */
		}
		if (stipple) {		/* restore stipple to previous */
		    printf(".st\n");
		}
		printf(".br\n.ft \\n(g3\n.ps \\n(g4\n%s", inputline);
	    } else {
		interpret(inputline);	/* take commands from the input file */
	    }
	} while (!done);
}


/*----------------------------------------------------------------------------*
 | Routine:	savestate  ( )
 |
 | Results:	all the current  scaling / font size / font name / thickness /
 |		pointscale  settings are saved to be the defaults.  Scaled
 |		point sizes are NOT saved.  The scaling is done each time a
 |		new picture is started.
 |
 | Side Efct:	scale, and def* are modified.
 *----------------------------------------------------------------------------*/

savestate()
{
    register int i;

    for (i = 0; i < STYLES; i++) {	/* line thickness defaults */
	defthick[i] = thick[i];
    }
    for (i = 0; i < FONTS; i++) {	/* font name defaults */
	deffont[i] = tfont[i];
    }
    for (i = 0; i < SIZES; i++) {	/* font size defaults */
	defsize[i] = tsize[i];
    }
    for (i = 0; i < NSTIPPLES; i++) {	/* stipple font file default indices */
	defstipple_index[i] = stipple_index[i];
    }
    defstipple = stipple;	/* if stipple has been set, it's remembered */

    scale *= xscale;		/* default scale of individual pictures */
    defpoint = pointscale;	/* flag for scaling pointsizes from x factors */
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
    register int i;
    double par;

    str2[0] = '\0';
    sscanf(line, "%80s%80s", &str1[0], &str2[0]);
    for (chr = &str1[0]; *chr; chr++)		/* convert command to */
	if(isupper(*chr)) *chr = tolower(*chr);		/* lower case */
    switch (str1[0]) {

	case '1':
	case '2':	/* font sizes */
	case '3':
	case '4':
	    i = atoi(str2);
	    if (i > 0 && i < 1000)
		tsize[str1[0] - '1'] = i;
	    else
		error("bad font size value at line %d", linenum);
	    break;

	case 'r':	/* roman */
	    if(str2[0] < '0') goto nofont;
	    tfont[0] = malloc(strlen(str2) + 1);
	    strcpy(tfont[0], str2);
	    break;

	case 'i':	/* italics */
	    if(str2[0] < '0') goto nofont;
	    tfont[1] = malloc(strlen(str2) + 1);
	    strcpy(tfont[1], str2);
	    break;

	case 'b':	/* bold */
	    if(str2[0] < '0') goto nofont;
	    tfont[2] = malloc(strlen(str2) + 1);
	    strcpy(tfont[2], str2);
	    break;

	case 's':	/* special */
	    if (str1[1] == 'c') goto scalecommand;	/* or scale */

	    if(str2[0] < '0') {
	nofont:	error("no fontname specified in line %d", linenum);
		break;
	    }
	    if (str1[1] == 't') goto stipplecommand;	/* or stipple */

	    tfont[3] = malloc(strlen(str2) + 1);
	    strcpy(tfont[3], str2);
	    break;

	case 'l':	/* l */
	    if ((str1[1] < '1') || (str1[1] > '8'))
		goto stipplecommand;

	    /* else set stipple index */
	    i = atoi(str2);
	    if (i >= 0 && i < 256)
		stipple_index[str1[1] - '1'] = i;
	    else
		error("bad stipple index value at line %d", linenum);
	    break;

	stipplecommand:	/* stipple */
	    stipple = malloc(strlen(str2) + 1);
	    strcpy(stipple, str2);
	    break;

	case 'o':	/* oldstipplemap */
	    oldstipmap = TRUE;
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
	    par = atof(str2);
	    if (par > 0.0)
		xscale *= par;
	    else
		error("illegal scale value on line %d", linenum);
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

	case 'd':	/* defaults */
	    setdefault = 1;
	    break;

	case 'p':	/* pointscale */
	    if (strcmp("off", str2))
		pointscale = 1;
	    else
		pointscale = 0;
	    break;

	default: 
	    error("unknown command, %s, on line %d", str1, linenum);
	    exit(8);
	    break;
    };
}


/*
 * return TRUE if picture contains a polygon
 * otherwise FALSE
 */
has_polygon(elist)
register ELT *elist;
{
    while (!DBNullelt(elist)) {
	if (elist->type == POLYGON)
	    return(1);
	elist = DBNextElt(elist);
    }

    return(0);
}
