/*******************************************************************************
 *
 *	globals	--	global variables used by the ditroff to interpress 
 *			conversion routines
 *
 *	John Mellor-Crummey (Xerox Corp)
 *	
 *	Copyright (c) 1985 Xerox Corporation
 *
 ******************************************************************************/

#include <stdio.h>
#include "defs.h"			/* constant and macro declarations */
#include "types.h"			/* data type declarations */
#include "deviceinfo.h"


/*-----------------------------------------------------------------------------
 *
 *	global variables used by the ditroff to interpress conversion routines
 *
 *---------------------------------------------------------------------------*/

char	*tempfilename	= "/tmp/dipXXXXXb"; /* used to form names of output files */
int	outputfile;			/* output file */
int	pagebodyfile;			/* temporary to hold page bodies */
int	outputflag = 0;			/* output this page? */

int	nPageRanges = 0;		/* output page list if > 0 */
int	pagerange[MAXPAGESPEC][2];	/* pairs of page numbers */

/* font tables */
char	*fontdirectory	= "/usr/local/export/ip/lib/font";
char	devicename[20];
struct	device_entry device;
struct	font_entry *fontPtr[MAX_NUM_FONTS+1];
short	*pointsizeTab;
int	specFontPos = 0;		/* position of first special font */
char	*specCharStrTab;
short	*specCharTab;
unsigned char	*fontIndexTab[MAX_NUM_FONTS+1];
char	*charCodeTab[MAX_NUM_FONTS+1];	/* character codes for device */
char	*stipTypeName[MAX_NUM_FONTS+1]; /* names of the stipple families */
unsigned char	*charWidthTab[MAX_NUM_FONTS+1];	/* char widths for fonts */

int	dbg	= 0;
int linenumber = 1;			/* for error reporting */

int	spotsPerInch;			/* input uses this resolution */
int	lineThickness = 0;		/* not used if unset by draw command */
int	lineStyle = 0;			/* not used if unset by draw command */
int	stippleFamily = 0;		/* not used if unset by set command */
int	size	= -1;			/* current size */
int	font	= -1;			/* current font */
long	ftsz	= -1;			/* combination of size and font */
long	oldftsz = -1;			/* former value of ftsz */
int	hor_pos;			/* next horizontal position (= 0, left of page) */
int	old_hor;			/* previous horizontal position */
int	ver_pos;			/* current vertical position (> 0, down page) */
int	old_ver;			/* previous vertical position */
int	hor_orig; 			/* horizontal origin of current block */
int	ver_orig;			/* vertical origin of current block */

/* interpress stuff */

struct ifont *currfonts[MAX_NUM_FONTS+1];
#ifndef lint
struct ifont *inactfonts = NULL;
#endif

int frameindex = 1;			/* current frame index */
int mapcnt;				/* number of names in troff to ip map */
int charw;				/* current character's troff width */
char **trname;				/* troff name pointers (into timap) */
char **ipname;				/* interpress name pointers (into timap) */
char *timap;				/* buffer holds strings for font mapping */
char in_correct = 0;			/* true when inside a "correct" body */
char virgin_line = 1;			/* true if line untouched (character not yet shown) */
double scale;				/* scale used in translating troff to ip co-ords */

/* this is for the bitmap interface */
int drawidth;				/* width of the drawing pen */
double drawscale;			/* scaling factor for drawing points */

/* for saving environments with push,pop */
struct	state	state[MAXSTATE];
struct	state	*statep = state;
