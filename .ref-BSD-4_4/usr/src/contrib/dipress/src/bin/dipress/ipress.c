
/*-----------------------------------------------------------------------------
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  ipress - most of the code directly dependent on interpress used by dipress
 *	     to produce an interpress file from device independent troff
 *	     intermediate code.  
 *
 *  William LeFebvre
 *
 *
 * HISTORY
 * 12-Aug-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added positioning option to the RES command.
 *	
 *
 * 07-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Updated IPDeviceType for services 10.0 .
 *
 * 24-Feb-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added a subroutine to emit the SequenceInsertFile command.
 *
 * John Mellor-Crummey 28-aug-1985
 *	restructuring and minor modifications
 *
 * ed flint 10-may-1985
 *	coerce device.num_char_wid to unsigned char, change
 *	ch argument in outputChar to unsigned int 
 *	since we now have > 128 special characters
 *---------------------------------------------------------------------------*/


#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <math.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#include "deviceinfo.h"	/* typesetter characteristics */

#include "iptokens.h"	/* \					*/
#include "literal.h"	/*  > interface levels for interpress	*/
#include "operator.h"	/* /					*/

#include "defs.h"	/* constant and macro definitions */
#include "externs.h"	/* declarations for global variables */



/* initialize device */
initDevice()		
{
	int  lines = 0;
	int  timapf;
	int  ret;
	register char *ptr;
	register char **ipp;
	register char **trp;
	char temp[60];
	struct stat stbuf;

	if (dbg) printf("initDevice called\n");

	/* start the preamble */
	ip_select(outputfile);
	/* special master instructions go here... */
	AppendOp(OP_beginBlock);
	AppendOp(OP_beginBody);

	/* save scaling transform that uses a mica co-ordinate system */
	AppendRational(1L, 100000);
	AppendOp(OP_scale);
	AppendInteger((long) F_transform);
	AppendOp(OP_fset);

	/* select file that holds page bodies */
	ip_select(pagebodyfile);

	/* open file that maps troff names to interpress names */
	(void) sprintf(temp, "%s/dev%s/interpress-fonts", fontdirectory, devicename);
	if ((timapf = open(temp, O_RDONLY,0)) == -1)
		reportError(QUIT, "can't open %s (%s)", temp, sys_errlist[errno]);

	/* read in the whole thing */
	ret = fstat(timapf, &stbuf);
	if (dbg) printf("stat returned %d, errno %d\n", ret, errno);
	timap = malloc((unsigned)(stbuf.st_size + 1));
	if (dbg) printf("reading %d bytes from timapf\n", stbuf.st_size);
	ret = read(timapf, timap, (int)stbuf.st_size);
	if (dbg) printf("read returned %d, errno %d, timapf %d\n", ret, errno, timapf);
	timap[(int)stbuf.st_size] = '\0';
	(void) close(timapf);

	/* count the newlines */
	if (dbg) printf("pointer starts at %08x ... ", timap);

	for (ptr = timap; *ptr != '\0'; ptr++)
		if (*ptr == '\n') lines++;

	if (dbg) printf("ends at %08x\n", ptr);
	if (dbg) printf("found %d lines\n", lines);

	/* allocate the mapping arrays */
	trp = trname = (char **)malloc((unsigned)(lines * 2 * sizeof(char *)));
	ipp = ipname = trname + lines;

	/* break out the strings and store pointers in the arrays */
	ptr = timap;
	mapcnt = 0;
	while (*ptr)
	{
		if (dbg) printf("loop: ptr = %08x, *ptr = %c\n", ptr, *ptr);
		*trp++ = ptr;
		while (!white(*ptr)) ptr++;
		*ptr++ = '\0';
		while (white(*ptr)) ptr++;
		*ipp++ = ptr;
		while (*++ptr != '\n') /* nothing */;
		*ptr++ = '\0';
		mapcnt++;
	}

	if (dbg)
	{
		int i;

		for (i = 0; i < lines; i++)
			printf("%s\t%s\n", trname[i], ipname[i]);
	}

	/* reset vertical and horizontal positions */
	hor_pos = ver_pos = old_hor = old_ver = 0;

	/* reset the font information */
	bzero((char *) currfonts, sizeof(currfonts));
}

setScale(spi)		/* set resolution */

int spi;

{
	/* set the scaling variable used in all x and y calculations */
	scale = floor(2540.0 / (double)spi + 0.5);
	if (dbg) printf("setScale(%d) sets scale to %e\n", spi, scale);

	/*
	     *  Set the drawing scale based on the scale.  This factor is applied to
	     *	all points drawn in the bitmap for graphical objects.  It is scaled
	     *	down from micas to 508 dpi so that the bitmaps aren't of unwieldy
	     *	size, but still retain enough information to look decent on a good
	     *	device.  508/2540 == 0.2
	     */
	drawscale = scale * .2;
	if (dbg) printf("setScale(%d) sets drawscale to %e\n", spi, drawscale);
}

pushCurrentEnv()	/* begin a new block */
{
	statep->ssize = size;
	statep->sfont = font;
	statep->shorig = hor_orig;
	statep->svorig = ver_orig;
	statep->shpos = hor_pos;
	statep->svpos = ver_pos;
	hor_orig = hor_pos;
	ver_orig = ver_pos;
	hor_pos = ver_pos = 0;
	if (statep++ >= state + MAXSTATE)
	{
		reportError(QUIT, "{ nested too deep");
	}
	hor_pos = ver_pos = 0;
}

popSavedEnv()	/* pop to previous state */
{
	if (--statep < state)
	{
		reportError(QUIT, "extra }");
	}
	size = statep->ssize;
	font = statep->sfont;
	hor_pos = statep->shpos;
	ver_pos = statep->svpos;
	hor_orig = statep->shorig;
	ver_orig = statep->svorig;
}

newpage(n)		/* new page */
int n;
{
	int i;
	char buff[15];

	/* print any pending bitmap */

	/* terminate previous page if outputting */
	if (outputflag)
	{
		print_bitmap();
		AppendOp(OP_endBody);
	}
	else flush_bitmap();

	/* reset vertical positions */
	ver_pos = old_ver = 0;

	/* check new page number against those found in the nPageRanges */
	if (nPageRanges == 0)
	{
		/* no -o specified -- do all pages */
		outputflag = 1;
	}
	else
	{
		/* see if new page has been selected for output */
		outputflag = 0;
		for (i = 0; i < nPageRanges; i++)
		{
			if ((n >= pagerange[i][0]) && (n <= pagerange[i][1]))
			{
				outputflag = 1;
				break;
			}
		}
	}

	/* start new page */
	if (outputflag)
	{
		AppendOp(OP_beginBody);
		(void) sprintf(buff, "Page %d", n);
		AppendComment(buff);
		Fget(F_transform);
		AppendOp(OP_concatt);
		AppendInteger(2L);
		AppendInteger((long) I_strokeEnd);
		AppendOp(OP_iset);
	}

	/* font/size no longer valid -- force a new assignment */
	oldftsz = -1;
}

newLine()		/* new line (no vertical motion implied) */
{
	if (dbg == 3)
		putchar('\n');
	flushbuff();
	endcorrect();
	hor_pos = 0;
	virgin_line = 1;
}

internalSize(number)		/* convert integer to internal size number */
int number;
{
	int index;

	if (number >= pointsizeTab[device.num_sizes - 1])
	{
		/* larger than largest -- use largest */
		return(device.num_sizes-1);
	}
	else if (number <= pointsizeTab[0])
	{
		/* smaller than smallest -- use smallest */
		return(0);
	}

	/* else find the size in pointsizeTab and return index */
	for (index = 0; number > pointsizeTab[index]; index++);
	return(index);
}





/* handle device stop command */
resetDevice()		
{
	int amt;
	static int is_reset = 0;
	char bigbuff[1024];

	if (is_reset) return; /* ignore multiple resets */

	print_bitmap();

	/* this is the absolute last thing that we do */
	/* wrap up the preamble and the body */
	ip_select(outputfile);
	AppendOp(OP_endBody);
	ip_select(pagebodyfile);		
	AppendOp(OP_endBody);
	AppendOp(OP_endBlock);
	ip_close();			/* close the body */

	/*
	     *  Reopen the body and copy it onto the end of the real file (which is
	     *	where we have been building the preamble).  We don't need to ip_flush
	     *	the preamble since that is done everytime we ip_select the body.
	     *	Conveniently enough, "tempfilename" still holds the name of the body
	     *	temporary.
	     */
	pagebodyfile = open(tempfilename, O_RDONLY,0);
	while ((amt = read(pagebodyfile, bigbuff, sizeof(bigbuff))) != 0)
	{
		(void) write(outputfile, bigbuff, amt);
	}

	/* close and unlink the body temporary file */
	(void) close(pagebodyfile);
	(void) unlink(tempfilename);

	/* send the file off to the printer */
	tempfilename[strlen(tempfilename) - 1] = '\0';
	if (outputfile != 1)
	{
		IPprint(tempfilename);
	}

	/* we are now reset */
	is_reset = 1;
}

outputString(character)		/* print a "funny" character */
char *character;
{
	int i;

	if (!outputflag)
		return;
	if (dbg > 2) printf("\\(%s", character);
	if (dbg > 3) putchar(' ');
	for (i = 0; i < device.spec_char_num; i++)
		if (strcmp(&specCharStrTab[specCharTab[i]], character) == 0)
			break;
	if (i < device.spec_char_num) {
		/*	printf(" i = %d so i+128 = %d\n", i, i+128);	*/
		outputChar((unsigned int) i + 128);
	}
	else {
		if (dbg > 3)
			printf("-- character not found");
	}

	if (dbg > 2)
		putchar('\n');
}

outputChar(character)		/* put a character */
unsigned int character;
{
	unsigned char *widp;	/* pointer to appropriate width table */
	register char *codep;	/* pointer to appropriate table of codes */
	register int i, old_font, fnt_index;
	int j, value;

	if (!outputflag)
		return;

	if (character <= 32)
	{
		if (dbg) printf("non-existent character 0%o\n", character);
		charw = charWidthTab[font][0] * pointsizeTab[size-1] / device.width_units;
		return;
	}
	character -= 32;

	old_font= font;
	i = fontIndexTab[font][character] & 255;
	if (i != 0)				/* it's on this font */
	{
		codep = charCodeTab[font];
		widp = charWidthTab[font];
	}
	else if (specFontPos > 0)			/* on special (we hope) */
	{
		/* assertion:  i == 0 */
		fnt_index= specFontPos;
		for (j=0; j <= device.num_fonts; j++)
		{
			struct font_entry *fb;

			fnt_index= (fnt_index+1) % (device.num_fonts+1);

			if ((fb = fontPtr[fnt_index]) != NULL)
			{
				if (fb->special_flag && (i = fontIndexTab[fnt_index][character] & 255) != 0)
				{
					codep = charCodeTab[fnt_index];
					widp = charWidthTab[fnt_index];
					setFont(fnt_index);
					break;
				}
			}
		}
		/* assertion:  if j > device.num_fonts then i == 0 and character was not found */
	}

	value= codep[i] & 255;

	if (i == 0 || value == 0)
	{
		if (dbg) printf("character not found 0%o\n", character+32);
		return;
	}

	/* remember this character's width */
	/* This MUST be done before calling showchar */
	charw = (widp[i] * pointsizeTab[size-1] + device.width_units/2) / device.width_units;
	if (dbg == 3)
	{
		if (isprint(character+32))
			putchar(character+32);
	}
	if (dbg > 3)
	{
		if (isprint(character+32))
			printf("character %c %d\n", character+32, value);
		else
			printf("character %03o %d\n", character+32, value);
	}

	if (value == 0377)
	{
		/* special escape to an extended code */
		value = getexcode(i);
	}

	if (dbg < 6)
		showchar(value);

	if (font != old_font)
	{
		setFont(old_font);
	}
}


/*
 * set point size to n
 */
setPointSize(n)

int n;			/* internal value:  index into pointsizeTab */

{
	size = n;
	ftsz = ((long)font << 16) | ((long)size);
}


/*
 * set the current font to number
 */
setFont(n)

int n;			/* internal index */

{
	font = n;
	ftsz = ((long)font << 16) | ((long)size);
}


/*
 *	reportError	an error reporting hack that uses dummy parameters 
 *			as place holders for arguments that may or may not 
 *			exist, fprintf will sort out how many should be there
 */
/*VARARGS 2*/
reportError(f, s, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) 
char *s, *arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	fprintf(stderr, "dipress: ");
	fprintf(stderr, s, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
	fprintf(stderr, "\nerror encountered near line %d\n", linenumber);
	if (f == QUIT) goodbye();
}




/*
 *  Graphics drawing primitives.  These use the vector drawing capabilities of
 *  interpress to draw straight lines.  All other primitive objects (circle,
 *  ellipse, etc.) are built in a bitmap and printed with a pixel vector.
 */

drawline(dh, dv)
int dh,dv;
{

	if(!outputflag) return;
	AppendInteger((long) curr_strokewidth);
	AppendInteger((long) I_strokeWidth);
	AppendOp(OP_iset);
	Moveto(xloc(hor_pos), yloc(ver_pos));
	hor_pos += dh;
	ver_pos += dv;
	Lineto(xloc(hor_pos), yloc(ver_pos));
	AppendOp(OP_maskstroke);
}

/* routines used by interpress dependent routines */

char showbuff[Showbuff_size + 1];
char *showp = showbuff;
int  showcnt = 0;

showchar(ch)		/* buffer "ch" for use in a "show" command */
int ch;
{
	char *framep;
	register int hdiff, vdiff;

	/* set correct position */
	vdiff = ver_pos - old_ver;
	hdiff = hor_pos - old_hor;
	if (dbg > 4)
	{
		printf("old_hor %d, hor_pos %d, hdiff %d;  old_ver %d, ver_pos %d, vdiff %d %s\n",
		old_hor, hor_pos, hdiff, old_ver, ver_pos, vdiff,
		virgin_line ? "(virgin)" : "");
	}

	/* NOTE:  this expression depends on boolean true being 1! */
	/* See K&R, Appendix A, section 7.7, page 190 */
	switch (((vdiff != 0) << 1) | (hdiff != 0))
	{
	case 0:		/* no change */
		break;
	default:
		flushbuff();
		Setxy(xloc(hor_pos), yloc(ver_pos));
		break;
	}

	/*
	     *  Update old_hor and old_ver.  Account for character width in old_hor but not in
	     *	hor_pos.  If the next hInc call is for the width of the character, the
	     *	next time showchar gets called, old_hor will equal hor_pos.
	     */
	old_ver = ver_pos;
	old_hor = hor_pos + charw;

	/* line is no longer virgin */
	virgin_line = 0;

	/* font and point still the same? */
	if (ftsz != oldftsz)
	{
		flushbuff();
		if ((framep = currfonts[font]->frames) == NULL)
		{
			/* previously unused -- give it a frame table */
			framep = currfonts[font]->frames = malloc((unsigned)(device.num_sizes * sizeof(char)));
			bzero(framep, device.num_sizes * sizeof(char));
		}

		if (framep[size] == 0)
		{
			/* make a new font */
			ip_select(outputfile);
			SetupFont(currfonts[font]->uname,
			floor(pointsizeTab[size-1] * 35.28 + .5),
			frameindex);
			ip_select(pagebodyfile);
			framep[size] = frameindex++;
		}

		/* switch to new font/size combo */
		Setfont(framep[size]);
		oldftsz = ftsz;
	}

	/* adjust for character codes > 0377 */
	if (ch > 0377)
	{
		if (dbg > 4)
		{
			printf("processing large code: 0%o (%d)\n", ch, ch);
		}

		if (showcnt + 5 > Showbuff_size)
		{
			flushbuff();
		}
		*showp++ = '\377';
		*showp++ = (ch & 0177400) >> 8;
		*showp++ = ch & 255;
		*showp++ = '\377';
		*showp++ = '\0';
		showcnt += 5;
	}
	else
	{
		*showp++ = ch;
		if (++showcnt > Showbuff_size)
		{
			flushbuff();
		}
	}
}

/*
 *  getexcode(findex) - get the extended code for the character "findex"
 */

getexcode(findex)
int findex;
{
	register int extfd;
	register int i;
	register unsigned short *tab;
	char temp[132];

	if (dbg > 4)
	{
		printf("getexcode(%d)\n", findex);
	}

	if ((tab = currfonts[font]->extab) == NULL)
	{
		/* load in the extended code table */

		(void) sprintf(temp, "%s/dev%s/%s.out.ext",
		fontdirectory, devicename, currfonts[font]->name);
		if (dbg > 4)
		{
			printf("opening and reading %s\n", temp);
		}
		if ((extfd = open(temp, O_RDONLY,0)) == -1)
		{
			reportError(CONTINUE, "can't open %s (%s)", temp, sys_errlist[errno]);
			return(0);
		}
		currfonts[font]->extab = tab = (unsigned short *)
			malloc( (unsigned)(i = (device.spec_char_num + 128-32) * sizeof(short)) );
		(void) read(extfd, (char *)tab, i); /* should test result! */
		(void) close(extfd);
	}

	if (dbg > 4)
	{
		printf("getexcode returning %.7o\n", tab[findex]);
	}
	return(tab[findex]);
}

flushbuff()		/* flush and reset "showbuff" */
{
	if (showcnt == 0)
		return;

	if (!in_correct)
	{
		startcorrect();
	}

	/* we must do the append_Sequence explicitly, */
	/* because showbuff might have nulls in it.   */
	append_Sequence(sequenceString, showcnt, (unsigned char *)showbuff);
	AppendOp(OP_show);
	showp = showbuff;
	showcnt = 0;
}

int hstart;

startcorrect()
{
#ifdef CORRECT_BLOCKS
	AppendOp(OP_correct);
	AppendOp(OP_beginBody);
#endif
	in_correct = 1;
	hstart = hor_pos;
}

endcorrect()
{
	/* append a Setcorrectmeasure operation */
	/* "hor_pos" or "old_hor"???  Make it "old_hor" for now */
#ifdef CORRECT_BLOCKS
	Setcorrectmeasure(xloc(old_hor), 0.0);
	AppendOp(OP_endBody);
#endif
	in_correct = 0;
}

/*
 *  IPprint(filename) - send the file "filename" to the interpress printer.
 *			 This routine is *very* dependent on local
 *			 environments.
 */

IPprint(filename)
char *filename;
{
	if (dbg)
	{
		printf("interpress file saved in %s.\n", filename);
		return;
	}

	if (vfork() == 0)
	{
		/* is child */
		execl("/usr/local/bin/qip", "qip", "-nc", "-nk", filename, 0);
		exit(1);
	}
}

/* bitmap graphics object sizing functions */

g_sizearc(x1, y1, xc, yc, x2, y2)
int x1, y1, xc, yc, x2, y2;
{
	int minx;
	int miny;
	int maxx;
	int maxy;
	int quad1;
	int quad2;
	int radius;
	int axc;
	int ayc;
	int i;

	/* the center and the second point are offsets from the first */
	/* calculate actual center and radius */
	axc = x1 + xc;
	ayc = y1 + yc;
	radius = (int)(hypot((double)xc, (double)yc) + .5);
	if (dbg > 1)
	{
		printf("g_sizearc(%d, %d, %d, %d, %d, %d): radius is %d\n",
		x1, y1, xc, yc, x2, y2, radius);
	}

	/* preset the minmal and maximal points -- this is our first guess */
	if ((minx = x1 + xc + x2) > x1)
	{
		maxx = minx;
		minx = x1;
	}
	else
	{
		maxx = x1;
	}
	if ((miny = y1 + yc + y2) > y1)
	{
		maxy = miny;
		miny = y1;
	}
	else
	{
		maxy = y1;
	}

	/* calculate the offset from the center to the first point */
	x1 = -xc;
	y1 = -yc;		/* now all three arguments are offsets */

	/* calculate the quadrant of each endpoint */
	quad1 = quadrant(x1, y1);
	quad2 = quadrant(x2, y2);
	if (dbg > 1)
	{
		printf("(%d, %d) in quadrant %d ... ", x1, y1, quad1);
		printf("(%d, %d) in quadrant %d\n", x2, y2, quad2);
	}

	/* insure that quad1 < quad2 */
	if (quad2 < quad1)
	{
		quad2 += 4;
	}

	/* compensate for arc at each quadrant boundary */
	for (i = quad1 + 1; i <= quad2; i++)
	{
		switch (i & 3)
		{
		case 0:		/* 1st quadrant */
			maxx = axc + radius;
			break;

		case 1:		/* 2nd quadrant */
			miny = ayc - radius;
			break;

		case 2:		/* 3rd quadrant */
			minx = axc - radius;
			break;

		case 3:		/* 4th quadrant */
			maxy = ayc + radius;
			break;
		}
	}

	/* now set the extremes */
	if (dbg > 1)
	{
		printf("extremes are %d, %d, %d, %d\n", minx, miny, maxx, maxy);
	}
	gobj_size(minx, miny, maxx, maxy);
}

quadrant(dx, dy)
int dx,dy;
{
	register int yplus;

	yplus = dy > 0;
	if (dx > 0)
	{
		return(yplus ? 3 : 0);
	}
	else
	{
		return(yplus ? 2 : 1);
	}
}


g_sizeWigglyLine(str)
char *str;
{
	int minx;
	int miny;
	int maxx;
	int maxy;
	int currx;
	int curry;
	int incx;
	int incy;

	currx = minx = maxx = hor_pos;
	curry = miny = maxy = ver_pos;

	while(white(*str)) str++; /* trim leading white space */

	while (*str)
	{

		(void) readNumber(&str,&incx);
		(void) readNumber(&str,&incy);
		currx += incx;
		curry += incy;
		if (currx > maxx)
			maxx = currx;
		else if (currx < minx)
			minx = currx;
		if (curry > maxy)
			maxy = curry;
		else if (curry < miny)
			miny = curry;
	}
	gobj_size(minx, miny, maxx, maxy);
}


/*
 * Request the insertion of an Interpress master
 */

doSequenceInsertIP(fileName)
char *fileName; {
	AppendOp(OP_dosavesimplebody);
	AppendOp(OP_beginBody);
	/* set the IP position at the current troff position */
	Setxy(xloc(hor_pos), yloc(ver_pos));

	/* undo the Troff scaling */
	AppendRational(spotsPerInch*10000L, 254L);
	AppendOp(OP_scale);
	AppendOp(OP_concatt);
	AppendOp(OP_trans);

	AppendInsertFile(fileName);
	AppendOp(OP_endBody);
}


#define SAME	0

/*
 * define frame variables for the RES image
 */
#define IMAGE_EDIT	47L
#define IMAGE_COLOR_OP	46L
#define IMAGE_COLOR	45L
#define IMAGE_MASK	44L
#define IMAGE_Y		43L
#define IMAGE_X		42L
#define IMAGE_SCALE	41L

/*
 * Request the insertion of an RES file
 */

doSequenceInsertRES(anchor, resolutionString, fileName) 
char *anchor,
     *resolutionString,
     *fileName; {
	int resolution;

	resolution = atoi(resolutionString);

	AppendOp(OP_dosavesimplebody);
	AppendOp(OP_beginBody);
	/* set the IP position at the current troff position */
	Setxy(xloc(hor_pos), yloc(ver_pos));

	AppendInsertFile(fileName);

	/* pop and store */
	AppendOp(OP_pop);		/* for now, don't check the signature */
	AppendInteger(IMAGE_EDIT);
	AppendOp(OP_fset);
	AppendInteger(IMAGE_COLOR_OP);
	AppendOp(OP_fset);
	AppendInteger(IMAGE_COLOR);
	AppendOp(OP_fset);
	AppendOp(OP_pop);			/* discard mask image */
	AppendInteger(IMAGE_Y);
	AppendOp(OP_fset);
	AppendInteger(IMAGE_X);
	AppendOp(OP_fset);
	AppendInteger(IMAGE_SCALE);
	AppendOp(OP_fset);

	/* undo the Troff scaling */
	AppendRational(spotsPerInch*10000L, 254L);
	AppendOp(OP_scale);

	/* set the resolution */
	if( resolution != 0 ) {
		/* display the RES file at the specified resolution */
		AppendRational(254L, resolution*10000L);
		AppendOp(OP_scale); }
	else 
		/* get the default scale from the RES file */
		/* can the device do a "get" operator? */
		if( IPDeviceType == Xerox8044_Services8  ||
		    IPDeviceType == Xerox8044_Services9  ||
		    IPDeviceType == Xerox8044_Services10 )
			reportError(CONTINUE, "requested device can't read resolution from an RES file");
		else {
			/* Yes!  Use the imageScale suggested by the image */
			Fget(IMAGE_SCALE);
			Get(1L);
			Fget(IMAGE_SCALE);
			Get(2L);
			AppendOp(OP_scale2); }

	AppendOp(OP_concat);		/* concat undo-scale with image scale */

	if( strcmp(anchor, "bl") == SAME )		/* bottom left */
		;
	else if( strcmp(anchor, "tl") == SAME ) {	/* top left */
		AppendInteger(-1L);		/* invert scale */
		AppendOp(OP_scale);
		AppendOp(OP_concat);
		AppendOp(OP_concatt);

		FGet(IMAGE_Y);
		AppendOp(OP_setyrel);

		AppendInteger(-1L);
		AppendOp(OP_scale);
	}
	else if( strcmp(anchor, "c") == SAME ) {	/* center */
		AppendRational(-1L, 2L);
		AppendOp(OP_scale);
		AppendOp(OP_concat);
		AppendOp(OP_concatt);	/* half normal scale for centering */

		FGet(IMAGE_X);
		FGet(IMAGE_Y);
		AppendOp(OP_setxyrel);

		AppendRational(-2L, 1L);
		AppendOp(OP_scale);
	}
	else if( strcmp(anchor, "br") == SAME ) {	/* bottom right */
		AppendInteger(-1L);		/* invert scale */
		AppendOp(OP_scale);
		AppendOp(OP_concat);
		AppendOp(OP_concatt);

		FGet(IMAGE_X);
		AppendOp(OP_setxrel);

		AppendInteger(-1L);		/* invert scale */
		AppendOp(OP_scale);
	}
	else if( strcmp(anchor, "tr") == SAME ) {	/* top right */
		AppendInteger(-1L);		/* invert scale */
		AppendOp(OP_scale);
		AppendOp(OP_concat);
		AppendOp(OP_concatt);

		FGet(IMAGE_X);
		FGet(IMAGE_Y);
		AppendOp(OP_setxyrel);

		AppendInteger(-1L);		/* invert scale */
		AppendOp(OP_scale);
	}
	else
		reportError(CONTINUE, "unknown position in RES command: %s", anchor );

	AppendOp(OP_concatt);

	AppendOp(OP_trans);

	FGet(IMAGE_COLOR);
	AppendOp(OP_maskpixel);
	AppendOp(OP_endBody);
}
