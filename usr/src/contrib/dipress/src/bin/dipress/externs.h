/******************************************************************************
 *
 *	externs	--	external declarations for global variables and 
 *			external functions used by the ditroff to 
 *			interpress conversion routines
 *
 *	John Mellor-Crummey (Xerox Corp)
 *
 *	Copyright (c) 1985 Xerox Corporation
 *
 *****************************************************************************/

#include 	"types.h"		/* data type declarations */


/*-----------------------------------------------------------------------------
 *
 *	external declarations for
 *	global variables used by the ditroff to interpress conversion routines
 *
 *---------------------------------------------------------------------------*/

extern char	*tempfilename;		/* used to form names of output files */
extern int	outputfile;		/* output file descriptor */
extern int	pagebodyfile;		/* temporary file to hold page bodies */
extern int	outputflag;		/* output this page? */

extern int	nPageRanges;		/* output page list if > 0 */
extern int	pagerange[][2];		/* pairs of page numbers */

extern enum IPDeviceType IPDeviceType;

/* font tables  */
extern char	*fontdirectory;
extern char	devicename[];
extern struct	device_entry device;
extern struct	font_entry *fontPtr[];
extern short	*pointsizeTab;
extern int	specFontPos;		/* position of first special font */
extern char	*specCharStrTab;
extern short	*specCharTab;
extern unsigned char *fontIndexTab[];
extern char 	*charCodeTab[];		/* character codes for device */
char	*stipTypeName[];		/* names of the stipple families */
extern unsigned char *charWidthTab[];	/* contains width info for fonts */
 
extern int	dbg;

extern int	spotsPerInch;		/* input uses this resolution */
extern int	lineThickness;		/* set by draw command, not pointsize */
extern int	lineStyle;		/* set by draw command, not pointsize */
extern int	stippleFamily;		/* current stipple Family */
extern int	size;			/* current size */
extern int	font;			/* current font */
extern long	ftsz;			/* combination of size and font */
extern long	oldftsz;		/* former value of ftsz */
extern int	hor_pos;		/* next horizontal position (= 0, left of page ) */
extern int	old_hor;		/* previous horizontal position */
extern int	ver_pos;		/* current vertical position (> 0, down page) */
extern int	old_ver;		/* previous vertical position */
extern int	hor_orig;		/* horizontal origin of current block */
extern int	ver_orig;		/* vertical origin of current block */

extern int linenumber;			/* line number for error reporting */

/* hooks for reporting system errors */
extern int  	errno;		
extern char 	*sys_errlist[];

/* interpress stuff */

extern struct	ifont *currfonts[MAX_NUM_FONTS+1];
extern struct	ifont *inactfonts;

extern int frameindex;			/* current frame index */
extern int mapcnt;			/* number of names in troff to ip map */
extern int charw;			/* current character's troff width */
extern char **trname;			/* troff name pointers (into timap) */
extern char **ipname;			/* interpress name pointers (into timap) */
extern char *timap;			/* buffer holds strings for font mapping */
extern char in_correct;			/* true when inside a "correct" body */
extern char virgin_line;		/* true if line untouched (char not yet shown) */
extern double scale;			/* scale used to trans troff to ip co-ords */

/* this is for the bitmap interface */
extern int drawidth;			/* width of the drawing pen */
extern double drawscale;		/* scaling factor for drawing points */

/* external function declarations */
extern char *malloc();

/* for saving environments with push, pop */
extern struct	state	state[MAXSTATE];
extern struct	state	*statep;
