/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  bitmap - routines that help build a bitmap of a graphical image
 *
 *  These are primarily used to implement all of the drawing primitives
 *  (except for lines)
 *
 *  HISTORY
 * 13-Aug-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Rather than outputing an error message for devices that can't print
 *	big pixel arrays, the code now breaks up large pixel arrays into
 *	smaller ones.
 *
 * 04-Mar-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Put in code to check the output device type.  Right now, if
 *	a bitmap is allocated that won't print then a warning message
 *	is produced.
 *
 *  28-aug-85 John Mellor-Crummey
 *	added flush_bitmap which is used instead
 *	of print_bitmap if current page is not outputted
 *
 *  17-jun-85  ed flint
 *	bitmap width(bm_width) must be multiple of 32
 *	bits for print services 12 (?)
 *
 *
 */

# include <stdio.h>
# include "iptokens.h"
# include "literal.h"
# include "operator.h"

# include "defs.h"
# include "externs.h"

# define    local	static
# define    No		0
# define    Yes		1

extern int drawidth;
extern double drawscale;

char *malloc();

local int minx;
local int miny;
local int maxx;
local int maxy;
local int bm_width;		/* in real bytes */
local int bm_size;
local unsigned char *bitmap;
local char  bm_active = No;

# define    scale_x(x)		x = (double)x * drawscale
# define    scale_y(y)		y = (double)y * drawscale

/*
 *  new_bitmap(x1, y1, x2, y2) - prepare for a new bitmap, the extreme x and y
 *				 values are x1, y1, x2, and y2.
 */

new_bitmap(stroke, x1, y1, x2, y2)

int stroke, x1, y1, x2, y2;

{
    int new_size;
    register int temp;

#ifdef DEBUG
    fprintf(stderr, "new_bitmap(%d, %d, %d, %d, %d)\n", stroke,x1, y1, x2, y2);
#endif
    drawidth = stroke * drawscale;
    scale_x(x1);
    scale_y(y1);
    scale_x(x2);
    scale_y(y2);
#ifdef DEBUG
    fprintf(stderr, "after scaling: width %d: %d, %d and %d, %d\n", drawidth, x1, y1, x2, y2);
#endif

    /* insure that x1, y1 is the lower left */
    if (x1 > x2)
    {
#ifdef DEBUG
	fprintf(stderr, "exchanging pairs ... ");
#endif
	temp = x1;	/* exchange x1 and x2 */
	x1   = x2;
	x2   = temp;
	temp = y1;	/* exchange y1 and y2 */
	y1   = y2;
	y2   = temp;
#ifdef DEBUG
	fprintf(stderr, "now %d, %d and %d, %d\n", x1, y1, x2, y2);
#endif
    }

    /* adjust the extremes to allow for pen thickness */
    temp = (drawidth + 1) / 2;
    x1 -= temp;
    y1 -= temp;
    x2 += temp;
    y2 += temp;

    if (!bm_active || !(x1 >= minx && y1 >= miny && x2 <= maxx && y2 <= maxy))
    {
	/* we need to set up a new map */
	/* but first, print the old one if it is still active */
#ifdef DEBUG
	fprintf(stderr, "setting up new map, bm_active is %d\n", bm_active);
#endif
	print_bitmap();

	minx = x1;
	miny = y1;
	maxx = x2;
	maxy = y2;

	bm_width = (y2 - y1 + 7) / 8;

	/* print services 12 (?) wants packed pixel vectors to produce each */
	/* scan line as a mutliple of 32 bits, this is backward compatible  */
	/* with previous versions of services				    */
	/* (the previous version of bitmap produced 16 bit multiples which  */
	/* will NOT work with services 12 (?) and beyond)		    */

	if ( bm_width%4 != 0 )
	{
	    bm_width= bm_width + (4 - bm_width%4);
	}

	new_size = (x2 - x1) * bm_width;

	if (new_size > bm_size)
	{
	    /* need to allocate a larger area for the bitmap */
	    if (bitmap != NULL)
		free((char *)bitmap);

	    /* check to see that this bitmap is printable */
	    if (new_size + 4 >= (1L << 16))
	    {
#ifdef notdef
		if (IPDeviceType == Xerox8044_Services8)
		    fprintf(stderr, "dipress: bitmap bigger than 2^16-1 -- won't print on a Xerox 8044 under services 8.0\n");
		else if (IPDeviceType == Xerox8044_Services9)
		    fprintf(stderr, "dipress: bitmap of %d is bigger than 2^16-1 -- won't print on a Xerox 8044 under services 9.0\n", new_size);
		else if (IPDeviceType == Xerox8044_Services10)
		    fprintf(stderr, "dipress: bitmap of %d is bigger than 2^16-1 -- won't print on a Xerox 8044 under services 10.0\n", new_size);
#endif
	    }

	    bitmap = (unsigned char *) malloc( (unsigned)(new_size) );
	    bm_size = new_size;
	}
#ifdef DEBUG
	fprintf(stderr, "bm_width %d, bm_size %d\n", bm_width, bm_size);
#endif
    
	bzero((char *)bitmap, bm_size);
    }
}

set_pixel(x, y)

int x;
int y;

{
    int mask;
    int half_drawidth;
    register int i;
    register int ybit, temp;
    register unsigned char *ptr;

    scale_x(x);
    scale_y(y);

    bm_active = Yes;

#ifdef DEBUG
    if (x < minx || x > maxx || y < miny || y > maxy)
    {
	fprintf(stderr, "point off map: (%d, %d)\n", x, y);
	return;
    }
#endif
    x -= minx;
    y  = maxy - y;	/* yes, it works */
    half_drawidth = drawidth >> 1;

    /* Remember the bitmap is built in increasing y */

    /* draw the "x" line vertically */
    mask = 0x80 >> (y & 007);
    ptr = (x - half_drawidth) * bm_width + (y >> 3) + bitmap;
    for (i = 0; i < drawidth; i++, ptr += bm_width)
    {
	*ptr |= mask;
    }

    /* draw the "y" line horizontally */
    y -= half_drawidth;
    ptr = (x * bm_width) + (y >> 3) + bitmap;
    ybit = y & 007;
    temp = ybit + drawidth;
    if (temp < 8)
    {
	/* special case -- less than one byte */
	*ptr |= (0xff >> ybit) ^ (0xff >> temp);
    }
    else
    {
	*ptr |= (0xff >> ybit);
	for (i = drawidth - 8 + ybit; i > 8; i -= 8)
	{
	    *++ptr |= 0xff;
	}
	*++ptr |= ~(0xff >> i);
    }
}

flush_bitmap()
{
	bm_active = No;
}

print_bitmap()

{
    int	bytesPerScanLine;
#ifdef DEBUG2
    int x;
    int y;
    int i;
    char *ptr;
#endif

    if (!bm_active)
    {
	/* don't bother */
	return;
    }

#ifdef DEBUG2
    /* debugging version for now */
    ptr = bitmap;
    for (x = minx; x <= maxx; x++)
    {
	for (y = miny; y <= maxy; y += 8, ptr++)
	{
	    for (i = 0x80; i != 0; i >>= 1)
	    {
		if (*ptr & i)
		{
		    putchar('*');
		}
		else
		{
		    putchar(' ');
		}
	    }
	}
	putchar('\n');
    }
#endif

    bytesPerScanLine = bm_width;

    /* inside a dosavesimplebody to preserve transform */
    AppendOp(OP_dosavesimplebody);
    AppendOp(OP_beginBody);

    /* set the transform to the right scale and position */
    /* transform must scale back to micas and position bitmap */
    AppendInteger((long) minx);
    AppendInteger((long)((int)(11 * INCH * drawscale) - maxy));
    AppendOp(OP_translate);
    AppendInteger(5L);
    AppendOp(OP_scale);
    AppendOp(OP_concat);
    AppendOp(OP_concatt);
  
    switch (IPDeviceType) {
	case Xerox8044_Services8:
	case Xerox8044_Services9:
	case Xerox8044_Services10:
	    {
	    unsigned char *bitmapP;
	    int	numberOfScanLines,
		pixelsPerScanLine,
		scanLinesPerStripe,
		bytesPerStripe,
		numberOfFullStripes,
		scanLinesRemaining,
		n;
	
	    numberOfScanLines = maxx - minx;
	    pixelsPerScanLine = bytesPerScanLine << 3;
	    scanLinesPerStripe = (64*1024 - 4 - 16)/bytesPerScanLine;
	    bytesPerStripe = scanLinesPerStripe * bytesPerScanLine;
	    numberOfFullStripes = numberOfScanLines / scanLinesPerStripe;
	    scanLinesRemaining = numberOfScanLines % scanLinesPerStripe;
	
	    for (n = 0, bitmapP = bitmap; n < numberOfFullStripes; n++, bitmapP += bytesPerStripe) {
	    	WritePixelArray(bitmapP, scanLinesPerStripe, pixelsPerScanLine);
		/*
		 * position the next stripe.  I tried going a setXRel followed
		 * by a trans but this cause strange things to happen on
		 * an 8044 running services 9.0
		 */
		AppendInteger((long)scanLinesPerStripe);
		AppendInteger(0L);
		AppendOp(OP_translate);
		AppendOp(OP_concatt);
	    }

    	    if (scanLinesRemaining != 0)
		WritePixelArray(bitmapP, scanLinesRemaining, pixelsPerScanLine);
    
	    }
	    break;

	case Xerox9700_V10:
	default:
	    WritePixelArray(bitmap, maxx - minx, bytesPerScanLine << 3);
    }

    AppendOp(OP_endBody);

    /* no longer active, now is it? */
    bm_active = No;
}

static
WritePixelArray(pixelArray, xPixels, yPixels)
unsigned char *pixelArray;
int xPixels, yPixels;
{
    int bytesPerScanLine,
	pixelArraySize;		/* in bytes */

    bytesPerScanLine = (yPixels + 7)/8;
    pixelArraySize = xPixels * bytesPerScanLine;

    /* build the pixel array */
    AppendInteger((long) xPixels);	/* xPixels */
    AppendInteger((long) yPixels);	/* yPixels */
    AppendInteger(1L);			/* samplesPerPixel */
    AppendInteger(1L);			/* maxSampleValue */
    AppendInteger(1L);			/* samplesInterleaved */
    Translate(0.0, 0.0);		/* (null) transform */

    AppendPPVector(pixelArraySize, 1, yPixels, pixelArray);

    /* make the pixel array */
    AppendOp(OP_makepixelarray);

    /* mask it */
    AppendOp(OP_maskpixel);
}

/*-----------------------------------------------------------------------------
 *	Gremlin additions:  This should be in a separate file.
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 *	drawPolygon(buffer, WantBorder)
 *---------------------------------------------------------------------------*/
#include <ctype.h>
#include <vfont.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>

static StippleDesFD = -1;
static StippleChoice = -1;
static struct {
	int fileDes;
	int member;
	int maxSize;
	struct {
		char prelude[4];
		short bits[16];
	} ppv;		/* In Services <= 10  max sampled black is 
					at most 16 x 16 */
	struct header header;
	struct dispatch dispatch;
} curStipple;

iPdrawPolygon(s, cmd)
char *s;
char cmd;
{
	int x[maxPointsInPolygon],y[maxPointsInPolygon]; 
	int stippleMember, strokeWidth = 254;
	extern int hor_pos, ver_pos, lineThickness;
	int xi,yi,i,j,numPoints;
	float temp1,temp2,temp3,t,dis;
	float euclidDist();
	char more;

	/* skip all leading white space */
	while(white(*s)) s++;		
	if(!isdigit(*s)) return; 

	if(readNumber(&s, &stippleMember) == NULL)
		return;

	/* read in the x y pairs of points for the polygon */
	for(numPoints = 0; numPoints < maxPointsInPolygon;) {
		x[numPoints] = hor_pos;
		more = readNumber(&s, &xi);
		y[numPoints] = (11 * INCH) - ver_pos;
		more *= readNumber(&s, &yi);
		hor_pos += xi;
		ver_pos += yi;
		numPoints++;
		if (more == 0) break;
	}
	getStippleMember(stippleMember);
	/* Doit */
	if (numPoints < 2)
		return;
	Op(dosavesimplebody);
	Op(beginBody);

	/* First, draw outline, if desired */
	if (cmd == drawPolygon) {
		AppendRational((long) strokeWidth, 10);
		AppendInteger((long) I_strokeWidth);
		Op(iset);
		Moveto((double)x[0], (double)y[0]);
		for(j = 1; j < numPoints; j++)
			Lineto((double)x[j], (double)y[j]);
		Lineto((double)x[0], (double)y[0]);
		Op(maskstroke);
	}
	/* Make Sampled Color */
	AppendInteger(16L);			/* xPixels */
	AppendInteger(16L);			/* yPixels */
	AppendInteger(1L);			/* samplesPerPixel */
	AppendInteger(1L);			/* maxSampleValue */
	AppendInteger(1L);			/* samplesInterleaved */
	/*AppendRational((long) 2*strokeWidth, 10); fails in serv < 11 */
	AppendRational((long) strokeWidth, 30); /* 1 bit per 300th inch */
	Op(scale);				/* transform for color */

			    /* spew out the packed pixel vector */
	append_Sequence(sequencePackedPixelVector, 32 + 4,
					(char *) &curStipple.ppv);
	Op(makepixelarray); /* make the pixel array */
	AppendInteger(4L);
	Op(iget);	    /* current transformation */
	AppendInteger(1L);
	Op(makesampledblack);
	AppendInteger(13L);
	Op(iset);	    /* Set Color */


	/* Print Polygon */
	Moveto((double)x[0], (double)y[0]);
	for(j = 1; j < numPoints; j++)
		Lineto((double)x[j], (double)y[j]);
	Lineto((double)x[0], (double)y[0]);
	AppendInteger(1L);
	Op(makeoutline);
	Op(maskfill);
	AppendInteger(1L); Op(setgray); /* Restore normal black */
	Op(endBody);
}
newStippleFamily(new)
{
	char stippleFilename[80];

	if (new == stippleFamily)
		return;
	if (curStipple.fileDes > 0)
		close(curStipple.fileDes);
	stippleFamily = new;

	(void) sprintf(stippleFilename, "/usr/lib/vfont/%s.0",
				stipTypeName[new]);
	if ((curStipple.fileDes = open(stippleFilename, O_RDONLY, 0)) < 0) {
		(void) sprintf(stippleFilename, "%s/dev%s/%s.0",
				fontdirectory, devicename, stipTypeName[new]);
		if ((curStipple.fileDes
				= open(stippleFilename, O_RDONLY, 0)) < 0) {
			reportError(QUIT,
			  "can't open file %s for device characteristics (%s)",
			    stippleFilename, sys_errlist[errno]);
		}
	}
	read(curStipple.fileDes, (char *)&(curStipple.header),
					    sizeof (curStipple.header));
	curStipple.member = -1;
	getStippleMember(0);
			    /* build the packed pixel vector */
	curStipple.ppv.prelude[0] = 0;
	curStipple.ppv.prelude[1] = 1;
	curStipple.ppv.prelude[2] = (char)(0);		/* high order byte */ 
	curStipple.ppv.prelude[3] = (char) (16);	/* low  order byte */
}

int showStip = 0;
getStippleMember(member)
{
	register struct dispatch *dp = & curStipple.dispatch;
	register int i;
	long offset;
	short bits[32];

	if (curStipple.member == member)
		return;
	offset = sizeof(struct header) + (member*sizeof(*dp));
	lseek(curStipple.fileDes, offset, 0);
	read(curStipple.fileDes, (char *)dp, sizeof (*dp));
	if (dp->nbytes == 0) {
		if (member == 0)  return;
		reportError(CONTINUE,
			"Stipple Family %s doesn't have member %d",
			stipTypeName[stippleFamily], member);
		curStipple.member = 0;
		return;
	}
	if ((dp->up + dp->down) != 32 || ((dp->left + dp->right) != 32)) {
		if (member == 0)  return;
		reportError(CONTINUE,
			"Stipple Family %s member %d is not 32x32\n",
			stipTypeName[stippleFamily], member);
		curStipple.member = 0;
		return;
	}
	offset = sizeof(struct header) + (256*sizeof(*dp)) + dp->addr;
	lseek(curStipple.fileDes, offset, 0);
	read(curStipple.fileDes, (char *)bits, sizeof (bits));
	if (showStip)
		    fprintf(stderr, "Stipple Family %s, member %d\n",
					stipTypeName[stippleFamily], member);
	for (i = 0; i < 16; i++) {
		curStipple.ppv.bits[i] = bits[i+i];
		if (showStip) {
			register int j;
			register int mask = bits[i+i];
			for (j = 0; j < 16; j++) {
				if (mask & 1)
					putc('*', stderr);
				else
					putc(' ', stderr);
				mask >>= 1;
			}
			putc('\n', stderr);
		}
	}
}
