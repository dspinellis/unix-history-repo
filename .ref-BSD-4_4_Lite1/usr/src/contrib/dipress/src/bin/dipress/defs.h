/******************************************************************************
 *
 *	defs	--	constant and macro definitions for dipress
 *
 *	John Mellor-Crummey (Xerox Corp)
 *	
 *	Copyright (c) 1985 Xerox Corporation
 *
 *****************************************************************************/

#define	QUIT		1
#define CONTINUE	0

#define MAX_NUM_FONTS	60

#define DEFAULTRANGEBOT -9999 	/* used to fill in an unspecified lower page# */
#define DEFAULTRANGETOP 9999 	/* used to fill in an unspecified top page# */
#define MAXPAGESPEC	25	/* maximum number of page specifications on 
				 * the command line using -o
				 */

#define TRUE		1

#define BUFFERSIZE	512	/* buffer for reading ditroff commands */
#define Showbuff_size	255
#define INCH		2540	/* micas per inch */
#define F_transform	0
#define maxPointsInSpline 	60	/* maximum number of points in a 
					 * wiggly line
					 */
#define maxPointsInPolygon	500	/* Similarly */

#define curr_strokewidth	(pointsizeTab[size - 1] * 4)
#define gobj_size(h1,v1,h2,v2)	new_bitmap(curr_strokewidth, h1, v1, h2, v2)

/* macros for positioning within the document */
#define	hInc(n)		hor_pos += n
#define	hMov(n)		hor_pos = n
#define	vInc(n)		ver_pos += n
#define	vMov(n)		ver_pos = n

/* translate ditroff h and v into interpress x and y */
/* (rx and ry are for relative x and y values) */

# define    xloc(h)	((h) * scale)
# define    rx(h)	x(h)
# define    yloc(v)	(11 * INCH - ((v) * scale))
# define    ry(v)	(-(v) * scale)

/* fontNumber -- coerce an out of range font index to the default font */
#define fontNumber(n)	((n < 0 || n > device.num_fonts) ? 1 : n)

#define	white(ch)	(ch == ' ' || ch == '\t')
#define abs(n)		(((n) < 0) ? - (n) : (n))
#define sqr(n)		((n) * (n))


/* constants for drawarc -- definition of the directions of movement */
#define M1		1
#define M2		2
#define M3		3

#define setpixel()	set_pixel(hor_pos,ver_pos)

#define	MAXSTATE	5	/* maximum number of stacked environments */

/* mneumonics for the Device Independent Troff Commands */

#define cmdPointSize		's'
#define cmdFont			'f'
#define cmdChar			'c'
#define cmdSpecChar		'C'
#define cmdAbsHoriz		'H'
#define cmdRelHoriz		'h'
#define cmdAbsVert		'V'
#define cmdRelVert		'v'
#define cmdEol			'n'
#define cmdStippleFamily	'i'
#define cmdWordSep		'w'
#define cmdNewPage		'p'
#define cmdPushEnv		'{'
#define cmdPopEnv		'}'
#define cmdCharString		't'
#define cmdComment		'#'
#define cmdDraw			'D'
#define drawLine		'l'
#define drawThick		't'
#define drawStyle		's'
#define drawCircle		'c'
#define drawEllipse		'e'
#define drawArc			'a'
#define drawWigglyLine		'~'
#define drawGremlinSpline	'g'
#define drawPolygon		'p'
#define drawUbPolygon		'P'
#define cmdDevice		'x'
#define deviceInit		'i'
#define deviceName		'T'
#define deviceResolution	'r'
#define devicePause		'p'
#define deviceStop		's'
#define deviceTrailer		't'
#define deviceFont		'f'
#define deviceHeight		'H'
#define deviceSlant		'S'
#define deviceXerox		'X'

/* Xerox specific device commands */
#define xeroxDeviceInsertIP	'I'
#define xeroxDeviceInsertRES	'R'
