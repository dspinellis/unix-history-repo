#ifndef lint
static char Notice[] = "Copyright (c) 1985 Adobe Systems Incorporated";
static char *RCSID = "$Header: ps4014.c,v 2.1 85/11/24 11:49:18 shore Rel $";
#endif
/* ps4014.c
 *
 * Copyright (c) 1985 Adobe Systems Incorporated
 *
 * tektronics 4014 to PostScript filter
 *
 * Edit History:
 * Original Version: Tom Malloy
 * Andrew Shore: Mon Nov  4 13:46:08 1985
 * End Edit History.
 *
 * RCSLOG:
 * $Log:	ps4014.c,v $
 * Revision 2.1  85/11/24  11:49:18  shore
 * Product Release 2.0
 * 
 * Revision 1.1  85/11/20  00:16:33  shore
 * Initial revision
 * 
 *
 */

#include <stdio.h>
#include <pwd.h>
#ifdef SYSV
extern struct passwd *getpwuid();
#include <string.h>
#else
#include <strings.h>
#endif
#include <setjmp.h>
#include "transcript.h"

/* move this to transcript.h some day */
private char scProlog[512];

private struct Params {
    char   *scSrcFile;		/* string name of 4014 source file */
    char   *scDstFile;		/* string name of PS destination file */
    char   *scDbgFile;		/* string name of debug output file */
    short   fCrGenLf;		/* true if carriage return generates line
    				   feed */
    short   fLfGenCr;		/* true if line feed generates carriage
				   return  */
    short   fMarg2;		/* true if there are two left margins for
				   text, one a left and one in the middle
				   of the page */
    short   fDbg;		/* true if debug output should be
				   generated */
    float   xRtInch;		/* top, left location of output on page */
    float   yBotInch;
    float   dxWidInch;		/* width, height of output */
    float   dyHtInch;
    int     fLandscape;		/* do landscape page */
};


private struct Real4110 {
    long    mantissa, exp;
};				/* 4110-format real number */
private struct Sts {
    short   md;			/* current mode */
    int     xh;			/* current x-position (in 4096ths) */
    int     yh;			/* current y-position (in 4096ths) */
    char    chHiY, chHiX, chExtra, chLoY;
 /* current values for the vector-mode address bytes that may be omitted
    when sending an address - see GetXhYh */
    short   syline;		/* current line style from 4014 input
				   viewpoint */
    short   sychar;		/* current character style */
    short   charset;		/* normal or alternate character set */
    short   sylinePrt;		/* current line style from printers
				   viewpoint */
    short   sycharPrt;		/* current character style from printers
				   viewpoint */
    short   charsetPrt;		/* normal or alternate characters from
				   printers viewpoint */
    short   fPenDown;		/* true implies vector command draws false
				   implies vector command moves */
    short   fMovePending;	/* true implies 4014 position and
				   PostScript position are different */
    short   fVectorDrawn;	/* true implies Stroke command is pending
				   on current PostScript path */
    short   dir;		/* current incremental point plot
				   direction */
    int     cincr;		/* count of increments tom move/draw in
				   current incremental point plot
				   direction */
    int     xhLeftMarg;		/* current left margin */
};

#define mdAlpha 0		/* Alpha mode */
#define mdVector 1		/* Vector graphics mode */
#define mdPtPlot 2		/* Point plot mode */
#define mdSpecPtPlot 3		/* Special point plot mode */
#define mdIncrPtPlot 4		/* Incremental point plot mode */
#define mdBypass 5		/* Bypass mode */

#define chFstSyline	'`'	/* first valid line style command
				   character */
#define chLstSyline	'w'	/* last valid line style command character */

#define sylineNormal	0	/* normal line style */
#define sylineDot	1	/* dotted line style */
#define sylineDotDashed 2	/* dot-dashed line style */
#define sylineShortDash 3	/* short dash line style */
#define sylineLongDash	4	/* long dash line style */
#define sylineMax	5	/* number of valid line styles */
#define mskSyline	7	/* mask for extracting line style indexes
				   from line style command characters. */

#define chFstSychar	'8'	/* first valid character style command
				   character */
#define chLstSychar	';'	/* last valid character style command
				   character */

#define sycharLarge	0	/* character style: large character */
#define sychar2		1	/* character style 2 */
#define sychar3		2	/* charactr style 3 */
#define sycharSmall	3	/* charactr style: small characters */
#define sycharMax	4	/* number of valid character styles */

#define charsetStd	0	/* normal character set */
#define charsetAlt	1	/* Alternate character set */

#define chFstShow	' '	/* first valid printable character */
#define chLstShow	126	/* last valid printable character */
#define chFstCoord	' '	/* first valid graphics address byte */
#define chLstCoord	127	/* last valid graphcs address byte */
#define chFstLoX	'@'	/* first valid low x byte */
#define chFstLoY	'`'	/* last valid low x byte */
#define mskLoBits	3	/* mask for extracting low order address
				   bits from extra byte */
#define mskBit4		16	/* mask for extracting margin bit from
				   extra byte */
#define shiftHiByte	7	/* number of bits to shift high-byte
				   address bits in composed address */
#define shiftLoByte	2	/* number of bits to shift low-byte
				   address bits in composed address */
#define shiftYExtra	2	/* number of bits to shift extra byte to
				   right align the low order y bits */

/* Some 4110 command code definitions */
#define chFst4110	'I'	/* first valid 4110 command code character
				   */
#define chLst4110	'Z'	/* last valid 4110 command code character 
				*/
#define chFstEsc2	'A'	/* first valid character for the second
				   character of a 2-byte 4110 command code
				   sequence */
#define chLstEsc2	'Z'	/* last valid character for the second
				   character of a 2-byte 4110 command code
				   sequence */
#define chFstHiInt	'@'	/* first valid character for non-low-order
				   portion of integer parameter */
#define chLstHiInt	127	/* last valid character for non-low-order
				   portion of integer parameter */
#define chFstLoPosInt	'0'	/* first valid character for low-order
				   portion of positive integer */
#define chLstLoPosInt	'?'	/* last valid character for low-order
				   portion of positive integer */
#define chFstLoNegInt	32	/* first valid character for low-order
				   portion of negative integer */
#define chLstLoNegInt	'/'	/* last valid character for low-order
				   portion of positive integer */

/* Some useful ASCII command character defintions */
#define chNull		0x0
#define chEnq		0x5
#define chBell		0x7
#define chBs		0x8
#define chTab		0x9
#define chLf		0xA
#define chVTab		0xB
#define chFf		0xC
#define chCr		0xD
#define chSo		0xE
#define chSi		0xF
#define chEtb		0x17
#define chCan		0x18
#define chSub		0x1A
#define chEsc		0x1B
#define chFs		0x1C
#define chGs		0x1D
#define chRs		0x1E
#define chUs		0x1F
#define chSp		0x20
#define chDel		0x7F

/* Types of 4014 postion movement commands */
#define tymoveNil	0
#define tymoveLeft	1
#define tymoveRt	2
#define tymoveUp	3
#define tymoveDown	4
#define tymoveGDown	5
#define tymoveXMarg	6
#define tymoveMax	7

/* masks for extracting movement direction 
from incremental point plot commands */
#define mskDir		0xF
#define mskDirUp	4
#define mskDirRt	1
#define mskDirDown	8
#define mskDirLeft	2
#define dirNil	0

/* Some x and y coordinates (in 4096ths) */
#define xhHome		0
#define xhMarg1		0
#define xhMarg2		2048
#define yhHome		3071
#define xhMax		4096
#define yhMax		3120

/* Constants and parser tables for 4110 parser */

/* parameter types */
#define typrmNil	0
#define typrmLong	1	/* 32-bit integer parameter */
#define typrmAryLong	2	/* array of 32-bit integers */
#define typrmXhYh	3	/* vector mode xy-coordinate */
#define typrmString	4	/* 4110 format string */
#define typrmCh		5	/* character */
#define typrmReal4110	6	/* 4110 format real number */
#define typrmAryXhYh	7	/* array of vector mode corrdinates */
#define typrmMax	8	/* number of valid parameter types */

#define iprmMax		4	/* maximum number of parameters to a 4110
				   command */
/* Identifiers for some, but not all, commonly used 
   parameter sequences for 4110 commands */
#define ifmtNil		0
#define ifmtL		1
#define ifmtLL		2
#define ifmtLLL		3
#define ifmtLLLL	4
#define ifmtXhYh	9
#define ifmtMax		31	/* total number of distinct parameter
				   seqeunces */

#define cchEsc1		18	/* number of valid 4110 command character
				   that can follow an <Esc> */
#define chEsc1Fst	'I'	/* first valid 4110 command character */
#define chEsc1Lst	'Z'	/* last valid 4110 command character */
#define cchEsc2		26	/* number of valid command characters for
				   the second character in two character
				   4110 command sequences */
#define chEsc2Fst	'A'	/* first valid character for second
				   character in a 4110 command sequence */
#define chEsc2Lst	'Z'	/* last valid character for second
				   character in a 4110 command sequence */

/* Parser table 1.  Indexed by ifmt, a parameter sequence id.  Yields
   an array of four elements each of which identifies the type of
   the i-th parameter in the sequence. */
private char    aryfmt[ifmtMax][iprmMax] =
{
    {typrmNil, typrmNil, typrmNil, typrmNil},
    {typrmLong, typrmNil, typrmNil, typrmNil},
    {typrmLong, typrmLong, typrmNil, typrmNil},
    {typrmLong, typrmLong, typrmLong, typrmNil},
    {typrmLong, typrmLong, typrmLong, typrmLong}, /*[5]*/
    {typrmAryLong, typrmNil, typrmNil, typrmNil},
    {typrmAryLong, typrmAryLong, typrmNil, typrmNil},
    {typrmLong, typrmAryLong, typrmNil, typrmNil},
    {typrmLong, typrmAryLong, typrmAryLong, typrmNil},
    {typrmXhYh, typrmNil, typrmNil, typrmNil}, /*[10]*/
    {typrmXhYh, typrmXhYh, typrmNil, typrmNil},
    {typrmLong, typrmXhYh, typrmNil, typrmNil},
    {typrmXhYh, typrmLong, typrmNil, typrmNil},
    {typrmLong, typrmLong, typrmXhYh, typrmXhYh},
    {typrmXhYh, typrmXhYh, typrmLong, typrmNil}, /*[15]*/
    {typrmLong, typrmXhYh, typrmXhYh, typrmXhYh},
    {typrmAryXhYh, typrmNil, typrmNil, typrmNil},
    {typrmString, typrmNil, typrmNil, typrmNil},
    {typrmString, typrmString, typrmNil, typrmNil},
    {typrmString, typrmString, typrmString, typrmNil},
     /*[20]*/ {typrmString, typrmLong, typrmNil, typrmNil},
    {typrmString, typrmString, typrmLong, typrmNil},
    {typrmString, typrmLong, typrmLong, typrmNil},
    {typrmString, typrmLong, typrmString, typrmLong},
    {typrmCh, typrmNil, typrmNil, typrmNil}, /*[25]*/
    {typrmCh, typrmCh, typrmNil, typrmNil},
    {typrmLong, typrmString, typrmNil, typrmNil},
    {typrmReal4110, typrmNil, typrmNil, typrmNil},
    {typrmString, typrmLong, typrmString, typrmString},
    {typrmString, typrmAryLong, typrmNil, typrmNil},
    {typrmString, typrmLong, typrmLong, typrmLong}
};

/* Parser table 2.  Indexed by the two characters of a 4110 command 
   sequence.  Yields a parameter sequence index, which can be used to
   retreive the parameter types from aryfmt. */
private char    aryaryifmt[cchEsc1][cchEsc2] =
{
     /*['I']*/ {
	ifmtL, ifmtNil, ifmtLL, ifmtL, ifmtLL, ifmtLLL, ifmtLLL,
	ifmtL, ifmtLL, ifmtNil, ifmtNil, ifmtL, ifmtL, ifmtL,
	ifmtNil, ifmtL, 25, ifmtLL, ifmtLLL, ifmtL, ifmtL,
	13, 10, 11, ifmtNil, ifmtNil
    },
     /*['J']*/ {
	ifmtNil, 17, 19, 19, ifmtNil, 17, ifmtNil,
	ifmtL, ifmtNil, 17, 17, 17, ifmtNil, ifmtNil,
	ifmtNil, 20, 17, 19, 19, ifmtNil, ifmtL,
	28, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['K']*/ {
	ifmtL, ifmtNil, ifmtNil, 7, ifmtL, ifmtL, ifmtNil,
	ifmtL, ifmtL, ifmtNil, ifmtNil, ifmtL, ifmtL, ifmtL, ifmtNil,
	ifmtL, ifmtNil, ifmtL, ifmtL, ifmtL, ifmtNil, ifmtNil,
	ifmtNil, ifmtL, ifmtNil, ifmtNil
    },
     /*['L']*/ {
	ifmtNil, ifmtL, ifmtL, ifmtNil, ifmtNil, ifmtXhYh, ifmtXhYh,
	ifmtXhYh, ifmtLLL, ifmtL, ifmtL, ifmtL, ifmtL, ifmtNil,
	ifmtNil, 12, ifmtNil, ifmtNil, ifmtL, 17, ifmtNil,
	ifmtL, ifmtNil, ifmtXhYh, ifmtNil, ifmtNil
    },
     /*['M']*/ {
	27, ifmtLL, ifmtLLL, ifmtLLLL, ifmtNil, ifmtL, ifmtL,
	ifmtNil, ifmtL, ifmtNil, ifmtNil, ifmtL, ifmtL, ifmtNil,
	ifmtNil, ifmtL, ifmtL, 27, ifmtLLL, ifmtL, ifmtNil,
	ifmtL, ifmtL, ifmtNil, ifmtL, ifmtLLL
    },
     /*['N']*/ {
	ifmtNil, ifmtL, ifmtLL, ifmtL, 5, ifmtL, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtL, ifmtL, ifmtL, ifmtNil,
	ifmtNil, ifmtL, ifmtL, ifmtLL, 5, 5, ifmtL,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['O']*/ {
	ifmtNil, ifmtL, ifmtLL, ifmtL, ifmtLL, ifmtNil, ifmtNil,
	6, ifmtNil, ifmtNil, ifmtNil, ifmtL, ifmtLL, 6,
	ifmtNil, ifmtLLLL, ifmtNil, ifmtNil, ifmtLL, ifmtL, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['P']*/ {
	21, 22, 19, ifmtNil, 29, 30, ifmtNil,
	ifmtNil, 22, ifmtNil, ifmtNil, 18, 29, ifmtNil,
	ifmtNil, 20, 17, 20, ifmtNil, ifmtNil, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['Q']*/ {
	ifmtNil, ifmtL, ifmtNil, ifmtL, ifmtNil, ifmtL, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtLLL, ifmtL, ifmtL,
	ifmtL, ifmtNil, ifmtNil, ifmtL, ifmtNil, ifmtNil, ifmtL,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['R']*/ {
	ifmtLLL, ifmtL, ifmtL, 5, ifmtL, ifmtL, 7,
	ifmtXhYh, 5, ifmtL, ifmtL, 5, ifmtNil, 5,
	ifmtNil, 26, 5, 14, 10, ifmtLLL, ifmtLLL,
	10, 10, 15, ifmtNil, ifmtNil
    },
     /*['S']*/ {
	8, ifmtNil, ifmtNil, ifmtLL, ifmtL, ifmtNil, ifmtLLL,
	ifmtLL, 31, ifmtNil, ifmtL, 6, ifmtLL, ifmtNil,
	ifmtL, ifmtXhYh, 26, ifmtLL, ifmtLL, ifmtLL, ifmtNil,
	ifmtLL, ifmtNil, 11, ifmtNil, ifmtLL
    },
     /*['T']*/ {
	ifmtNil, ifmtLLL, ifmtNil, ifmtNil, ifmtNil, ifmtNil, 7,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtLLL,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil
    },
     /*['U']*/ {
	ifmtNil, ifmtL, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil, ifmtNil,
	ifmtNil, ifmtNil, ifmtNil, 16, ifmtNil, ifmtNil, ifmtNil,
	ifmtNil, 10, ifmtL, ifmtNil, ifmtNil
    }
};

private jmp_buf env;

/* these strange numbers were arrived at by the following observations: */
/* Table 3-2 in spec gives the spacing values in mils */
/* Using "Point Spacing" section on page 3-26 we compute that there are */
/* approximately 286 points/inch on a 10.9 x 14.5 inch image area
/* I rounded to "reasonable" 4096th's of an inch */
private int     rgdxhSpace[sycharMax] = {
    56, 51, 34, 31
};
private int     rgdyhSpace[sycharMax] = {
    90, 81, 53, 48
};

private FILE * fpSrc, *fpPsDst, *fpDbg;
private struct Params   params;

private char *prog;

/* Outputs a conforming PostScript header to destination file */
private CommentHeader(pparams)
struct Params  *pparams;
{
    long    clock;
    struct passwd  *pswd;
    char    hostname[40];
    fprintf(fpPsDst, "%%!\n"); /* no reversal */
    fprintf(fpPsDst, "%%%%Creator: ");
    pswd = getpwuid(getuid ());
    VOIDC gethostname(hostname, sizeof hostname);
    fprintf(fpPsDst,"%s:%s (%s)\n",hostname,pswd->pw_name,pswd->pw_gecos);

    fprintf(fpPsDst, "%%%%Title: %s\n",
	    ((pparams->scSrcFile == NULL) ? "stdin" : pparams->scSrcFile));

    fprintf(fpPsDst, "%%%%CreationDate: %s",
    	(VOIDC time(&clock), ctime(&clock)));
}

/* Filter some input characters that are ignored under all(?) circumstances */
private ChGet(psts)
struct Sts *psts;
{
    int ch;

    while (TRUE) {
	ch = getc (fpSrc);
	switch (ch) {
	    case chEsc: 
		ch = ChEscapeAction (psts);
		if (ch = chDel)
		    return (chDel);
		else
		    longjmp (env, 1);
		break;
	    case chFs: 
		FsAction (psts);
		longjmp (env, 1);
		break;
	    case chGs: 
		GsAction (psts);
		longjmp (env, 1);
		break;
	}
	if ((ch >= chEsc) || (ch == EOF) || ((ch >= chBell) && (ch < chSo)))
	    break;
    }
    return (ch);
}

/* Parse a 4110 31-bit integer parameter */
private long    LongGet(psts)
struct Sts *psts;
{
    char    ch;
    long    longT;

    longT = 0;
    while (TRUE) {
	ch = ChGet (psts);
	if (params.fDbg)
	    PrintLongByte (ch);
	if ((ch >= chFstHiInt) && (ch <= chLstHiInt))
	    longT = longT * 64 + (ch - chFstHiInt);
	else
	    break;
    }
    if ((ch >= chFstLoPosInt) && (ch <= chLstLoPosInt))
	longT = longT * 16 + (ch - chFstLoPosInt);
    else if ((ch >= chFstLoNegInt) && (ch <= chLstLoNegInt))
	longT = -(longT * 16 + (ch - chFstLoNegInt));
    else {
	fprintf(stderr,
	"%s: Error while parsing int, expecting low int char. ch = %d\n",
	prog, ch);
	longT = 0;
    }
    if (params.fDbg)
	fprintf(fpDbg, "long: %ld\n", longT);
    return (longT);
}

/* Parse an array of 32-bit integers */
private GetAryLong (psts, arylong, ilongMax, pilongMac)
struct Sts *psts;
long    arylong[];
int     ilongMax, *pilongMac;
{
    int     ilong, ilongMac;

    if (params.fDbg)
	fprintf(fpDbg, "AryLong - ilongMac = ");
    ilongMac = LongGet (psts);
    *pilongMac = (ilongMac < ilongMax) ? ilongMac : ilongMax;
    for (ilong = 0; ilong < *pilongMac; ilong++)
	arylong[ilong] = LongGet (psts);
    for (ilong = *pilongMac; ilong < ilongMac; ilong++) VOIDC LongGet(psts);
}

/* Parse a 4100-format string parameter */
private GetString (psts, sc, ichMax)
struct Sts *psts;
char   *sc;
int     ichMax;
{
    int     ich, ichMac;
    char    chT;
    long    longT;

    if (params.fDbg)
	fprintf(fpDbg, "String - length = ");
    longT = LongGet (psts);
    ichMac = (longT < ichMax - 1) ? longT : ichMax - 1;
    for (ich = 0; ich < ichMac; ich++) {
	sc[ich] = ChGet (psts);
	if (params.fDbg)
	    PrintCmd (sc[ich]);
    }
    sc[ichMac] = '\0';
    for (ich = ichMac; ich < longT; ich++) {
	chT = ChGet (psts);
	if (params.fDbg)
	    PrintCmd (chT);
    }
}

/* parse a 4110-format real parameter */
private GetReal4110 (psts, preal4110)
struct Sts *psts;
struct Real4110 *preal4110;
{
    if (params.fDbg)
	fprintf(fpDbg, "Real4110: \n");
    preal4110->mantissa = LongGet (psts);
    preal4110->exp = LongGet (psts);
}

/* parse a 4014 xy-coordinate */
private GetXhYh (ch, psts, pxh, pyh)
char    ch;
struct Sts *psts;
int    *pxh, *pyh;
{
    char    chNxt;

    if ((ch >= chFstCoord) && (ch < chFstLoX)) {/* it is a High Y */
	if (params.fDbg)
	    PrintAdrByte (ch, TRUE, FALSE);
	psts->chHiY = ch;
	ch = ChGet (psts);
    }

    if (ch >= chFstLoY) {
	chNxt = ChGet (psts);
	if ( /* psts->fHiRes && */ (chNxt >= chFstLoY)) {
	    if (params.fDbg)
		PrintAdrByte (ch, FALSE, TRUE);
	    psts->chExtra = ch;
	    ch = chNxt;
	    chNxt = ChGet (psts);
	}
	if (params.fDbg)
	    PrintAdrByte (ch, FALSE, FALSE);
	psts->chLoY = ch;
	ch = chNxt;
    }
    if ((ch >= chFstCoord) && (ch < chFstLoX)) {/* it is a High X */
	if (params.fDbg)
	    PrintAdrByte (ch, FALSE, FALSE);
	psts->chHiX = ch;
	ch = ChGet (psts);
    }
    if ((ch < chFstLoX) || (ch >= chFstLoY)) {
	fprintf(stderr,
		"%s: Expecting Lo X Coordinate.  ch = %d\n", prog, ch);
	if (params.fDbg)
	    fprintf(fpDbg,
		    "Error: Expecting Lo X Coordinate.  ch = %d\n", ch);
	ch = chFstLoX;
    }
    if (params.fDbg)
	PrintAdrByte (ch, FALSE, FALSE);

    *pyh = ((psts->chHiY - chFstCoord) << shiftHiByte)
	| ((psts->chExtra >> shiftYExtra) & mskLoBits)
	| ((psts->chLoY - chFstLoY) << shiftLoByte);
    *pxh = ((psts->chHiX - chFstCoord) << shiftHiByte)
	| (psts->chExtra & mskLoBits)
	| ((ch - chFstLoX) << shiftLoByte);
    if (params.fDbg)
	fprintf(fpDbg, "*pxh: %d or 0X%x, *pyh: %d or 0X%x\n",
		*pxh, *pxh, *pyh, *pyh);
}

/* parse an array of xy-coordinates */
private GetAryXhYh (psts, aryxh, aryyh, iMax, piMac)
struct Sts *psts;
int    *aryxh, *aryyh;
int     iMax, *piMac;
{
    int     i, iMac, xhT, yhT;

    if (params.fDbg)
	fprintf(fpDbg, "AryXyYh - length = ");
    iMac = LongGet (psts);
    *piMac = (iMac < iMax) ? iMac : iMax;
    for (i = 0; i < *piMac; i++)
	GetXhYh (ChGet (psts), psts, &aryxh[i], &aryyh[i]);
    for (i = *piMac; i < iMac; i++)
	GetXhYh (ChGet (psts), psts, &xhT, &yhT);
}

#define scMoveTo "%d %d m\n"
/* char	*rgscMoveCmd[tymoveMax] =
	    { "",
	    "%d ml\n",
	    "%d mr\n",
	    "%d mu\n",
	    "%d md\n",
	    "%d md\n",
	    ""
	    };
*/

/* Process a line-feed from the 4014 file:
   Update the current position in the Sts - do NOT output any PostScript.
   All PostScript marking commands move issue moveto commands 
   whenever necessary */
private MoveDown (psts)
struct Sts *psts;
{
    int     xhLeftNew;

    psts->yh -= rgdyhSpace[psts->sychar];
    if (psts->yh < 0) {
	psts->yh = yhHome;
	if (params.fMarg2) {
	    xhLeftNew = (psts->xhLeftMarg == xhMarg1) ? xhMarg2 : xhMarg1;
	    psts->xh = xhLeftNew + (psts->xh - psts->xhLeftMarg);
	    psts->xhLeftMarg = xhLeftNew;
	}
    }
}

/* Move the current position.  No PostScript move commands are emitted.
   PostScript marking commands (i.e. stroke) issue moveto's when needed */
private MovePos (psts, tymove)
struct Sts *psts;
short   tymove;
{
/* char *scMoveCmd;
#define scXMarg1 "%d cr1\n"
#define scXMarg2 "cr2\n"

if (psts->fMovePending)
    fprintf(fpPsDst, scMoveTo, psts->xh, psts->yh);
psts->fMovePending = FALSE;
*/
    switch (tymove) {
	case tymoveNil: 
	    return;
	case tymoveLeft: 
	    psts->xh -= rgdxhSpace[psts->sychar];
	    if (psts->xh < psts->xhLeftMarg)
		psts->xh = psts->xhLeftMarg;
/*        fprintf(fpPsDst, rgscMoveCmd[tymove], psts->sychar); */
	    break;
	case tymoveRt: 
	    psts->xh += rgdxhSpace[psts->sychar];
	    if (psts->xh > xhMax) {
		MoveDown (psts);
		psts->xh = psts->xhLeftMarg;
	    }
/*        fprintf(fpPsDst, rgscMoveCmd[tymove], psts->sychar); */
	    break;
	case tymoveUp: 
	    psts->yh += rgdyhSpace[psts->sychar];
/*        fprintf(fpPsDst, rgscMoveCmd[tymove], psts->sychar); */
	    break;
	case tymoveGDown: 
	case tymoveDown: 
	    MoveDown (psts);
	    if (params.fLfGenCr)
		psts->xh = psts->xhLeftMarg;
/*         if (params.fLfGenCr)
             { fprintf(fpPsDst, scXMarg1, psts->sychar);
             psts->xh = xhLeftMarg;
	     }
        else fprintf(fpPsDst, rgscMoveCmd[tymove], psts->sychar);
*/
	    break;
	case tymoveXMarg: 
	    if (params.fCrGenLf)
		MoveDown (psts);
	    psts->xh = psts->xhLeftMarg;
/*         if (params.fCrGenLf)
             { fprintf(fpPsDst, scXMarg1, psts->sychar);
             psts->yh -= rgdyhSpace[psts->sychar];
	     }
        else fprintf(fpPsDst, scXMarg2);
*/
	    break;
    }
    psts->fMovePending = TRUE;
}


/* Process form feed */
private EraseAndHome ()
{
#define scEraseAndHome "erasepage\nxHome yHome m\n"

    fprintf(fpPsDst, scEraseAndHome);
}

/* Process Etb character */
private MakeCopy ()
{
#define scMakeCopy "showpage\n"

    fprintf(fpPsDst, scMakeCopy);
}

/* Process a movement/drawing command in on of the graphics modes */
private VectorGoTo (psts, xhNew, yhNew)
struct Sts *psts;
int     xhNew, yhNew;
{
#define scLineTo "%d %d rl\n"

    if (psts->fPenDown) {
	if (psts->syline != psts->sylinePrt) {
	    fprintf(fpPsDst, "%d SetLineStyle\n", psts->syline);
	    psts->sylinePrt = psts->syline;
	}
	if (psts->md == mdVector) {
	    if (psts->fMovePending)
		fprintf(fpPsDst, scMoveTo, psts->xh, psts->yh);
	    fprintf(fpPsDst, scLineTo, xhNew - psts->xh, yhNew - psts->yh);
	}
	else {			/* md = mdPtPlot */
	    fprintf(fpPsDst, scMoveTo, xhNew, yhNew);
	    fprintf(fpPsDst, scLineTo, 0, 0);
	}
	psts->fVectorDrawn = TRUE;
    }
    psts->fMovePending = (!psts->fPenDown);
    psts->xh = xhNew;
    psts->yh = yhNew;
}

/* Emit a stoke command if there are any vector's pending */
private Stroke (psts)
struct Sts *psts;
{
    if (psts->fVectorDrawn && psts->fPenDown) {
	fprintf(fpPsDst, "s\n");
	psts->fVectorDrawn = FALSE;
    }
}

/* Emit a show command */
private EmitShow (psts, scShow, ichFst, ichLim)
struct Sts *psts;
char    scShow[];
int     ichFst, ichLim;
{
    int     ich;

    if (psts->fMovePending)
	fprintf(fpPsDst, scMoveTo, psts->xh, psts->yh);
    psts->fMovePending = FALSE;
    if (psts->sychar != psts->sycharPrt) {
	fprintf(fpPsDst, "%d SetCharStyle\n", psts->sychar);
	psts->sycharPrt = psts->sychar;
    }

    if (ichFst < ichLim) {
	putc('\(', fpPsDst);
	for (ich = ichFst; ich < ichLim; ich++)
	    putc(scShow[ich], fpPsDst);
	fprintf(fpPsDst, ")sh\n");
    }
}

/* Output a string of characters to PostScript file.  Deal with
   line overflow and special character '\' */
private Show (psts, scShow, cchShow)
struct Sts *psts;
char    scShow[];
int     cchShow;
{
    int     ichFst, ich;
    int     xh, dxhSpace;

    scShow[cchShow] = '\0';

    dxhSpace = rgdxhSpace[psts->sychar];
    ichFst = ich = 0;
    xh = psts->xh;
    while (ich < cchShow) {
	if (scShow[ich] == '\\')
	    ich++;
	xh += dxhSpace;
	if (xh >= xhMax) {
	    EmitShow (psts, scShow, ichFst, ich);
	    ichFst = ich;
	    MoveDown (psts);
	    xh = psts->xh = psts->xhLeftMarg;
	    psts->fMovePending = TRUE;
	}
	ich++;
    }

    if (params.fDbg)
	fprintf(fpDbg, "Show(%s)\n", scShow);

    EmitShow (psts, scShow, ichFst, cchShow);
    psts->xh = xh;
}

/* Process the <Fs> character */
private FsAction (psts)
struct Sts *psts;
{
    if (params.fDbg)
	PrintCmd (chFs);
    Stroke (psts);
    psts->md = mdPtPlot;
}

/* Process the Graphic Shift character */
private GsAction (psts)
struct Sts *psts;
{
    if (params.fDbg)
	PrintCmd (chGs);
    Stroke (psts);
    psts->fPenDown = FALSE;
    psts->md = mdVector;
    psts->syline = sylineNormal;
}

/* Process a sequence beginning with an Escape character.  Return
   the character following the Escape or the Del character if the
   escape sequence is <Esc>? */
private ChEscapeAction (psts)
struct Sts *psts;
{
    int ch;
    short   sylineT;

    ch = getc (fpSrc);
    if (params.fDbg)
	PrintEscCmd (ch);
    while (ch != EOF) {
	switch (ch) {
	    case chEnq: 
		Stroke (psts);
		psts->md = mdBypass;
		goto Done;
	    case chBell: 
		psts->fPenDown = TRUE;
		goto Done;
	    case chBs: 
		MovePos (psts, tymoveLeft);
		goto Done;
	    case chTab: 
		MovePos (psts, tymoveRt);
		goto Done;
	    case chVTab: 
		MovePos (psts, tymoveUp);
		goto Done;
	    case chFf: 
		EraseAndHome ();
		goto Done;
	    case chSo: 
		psts->charset = charsetAlt;
		goto Done;
	    case chSi: 
		psts->charset = charsetStd;
		goto Done;
	    case chEtb: 
		Stroke (psts);
		MakeCopy ();
		goto Done;
	    case chCan: 
		Stroke (psts);
		psts->md = mdBypass;
		goto Done;
	    case chSub: 
		Stroke (psts);
		psts->md = mdBypass;
		goto Done;
	    case chFs: 
		Stroke (psts);
		psts->md = mdSpecPtPlot;
		goto Done;
	    case chGs: 
		GsAction (psts);
		goto Done;
	    case chRs: 
		Stroke (psts);
		psts->md = mdIncrPtPlot;
		psts->dir = dirNil;
		goto Done;
	    case chUs: 
		Stroke (psts);
		psts->md = mdAlpha;
		goto Done;
	    case '?': 
		ch = chDel;
		goto Done;
	    default: 
		if ((ch >= chFstSychar) && (ch <= chLstSychar)) {
		    psts->sychar = (ch - chFstSychar);
		    goto Done;
		}
		else if ((ch >= chFstSyline) && (ch <= chLstSyline)) {
		    sylineT = ((ch - chFstSyline) & mskSyline);
		    if (sylineT < sylineMax)
			psts->syline = sylineT;
		    else
			psts->syline = sylineNormal;
/* What about Defocused? */
		    goto Done;
		}
		else if ((ch >= chFst4110) && (ch <= chLst4110)) {
		    Skip4110 (ch, psts);
		    goto Done;
		}
		break;
	}
	ch = getc (fpSrc);
	if (params.fDbg)
	    PrintCmd (ch);
    }
Done: 
    if (params.fDbg)
	PrintCr ();
    return (ch);
}

/* Process some characters in one of the vector modes */
private VectorAction (ch, psts)
char    ch;
struct Sts *psts;
{
    int     xhNew, yhNew;

    if ((ch >= chFstCoord) && (ch <= chLstCoord)) {
	if (psts->md == mdIncrPtPlot)
	    IncrPtPlotAction (ch, psts);
	else {
	    if ((psts->md != mdVector) && (psts->md != mdPtPlot))
		fprintf(stderr, "%s: Unimplemented vector mode: %d\n",
			prog, psts->md);

	    GetXhYh (ch, psts, &xhNew, &yhNew);
	    VectorGoTo (psts, xhNew, yhNew);
	    psts->fPenDown = TRUE;
	}
    }
    else {
	if (params.fDbg)
	    PrintCmd (ch);
	switch (ch) {
	    case chLf: 
		MovePos (psts, tymoveGDown);
		break;
	    case chCr: 
		if (psts->fVectorDrawn)
		    MovePos (psts, tymoveXMarg);
		psts->md = mdAlpha;
		break;
	    case chUs: 
		psts->md = mdAlpha;
		break;
	}
    }
}

/* Finish processing a sequence of characters in Incremental
   Point Plot Mode by issuing an appropriate VectorGoTo */
private DoIncrPtPlot (psts)
struct Sts *psts;
{
    int     xhNew;
    int     yhNew;

    xhNew = psts->xh;
    yhNew = psts->yh;
    if (psts->dir & mskDirUp)
	yhNew += psts->cincr;
    else if (psts->dir & mskDirDown)
	yhNew -= psts->cincr;
    if (psts->dir & mskDirLeft)
	xhNew -= psts->cincr;
    else if (psts->dir & mskDirRt)
	xhNew += psts->cincr;
    VectorGoTo (psts, xhNew, yhNew);
    psts->cincr = 0;
}

/* Process a character in Incremental Point Plot Mode */
private IncrPtPlotAction (ch, psts)
char    ch;
struct Sts *psts;
{
    short   fPenDown;
    short   dir;

    if (params.fDbg)
	PrintCmd (ch);
    fPenDown = psts->fPenDown;
    dir = psts->dir;
    if (ch == chSp)
	fPenDown = FALSE;
    else if (ch == 'P')
	fPenDown = TRUE;
    else
	dir = (ch & mskDir);
    if ((psts->dir == dir) && (psts->fPenDown == fPenDown)
	    && (psts->dir != dirNil))
	psts->cincr++;
    else {
	if (psts->dir != dirNil)
	    DoIncrPtPlot (psts);
	psts->dir = dir;
	psts->cincr = 1;
	psts->fPenDown = fPenDown;
    }
}

/* Process a character in bypass mode */
private BypassAction (ch, psts)
char    ch;
struct Sts *psts;
{
    if (params.fDbg)
	PrintCmd (ch);
    switch (ch) {
	case chLf: 
	    MovePos (psts, tymoveGDown);
	    break;
	case chCr: 
	    MovePos (psts, tymoveXMarg);
	    psts->md = mdAlpha;
	    break;
	case chUs: 
	    psts->md = mdAlpha;
	    break;
    }
}

/* Some debug print routines and data */
private char   *rgsc[0x21] =
{
    "<Null>", "<Soh>", "<Stx>", "<Etx>",
    "<Eot>", "<Enq>", "<Ack>", "<Bell>",
    "<Bs>", "<Tab>", "<Lf>", "<VTab>",
    "<Ff>", "<Cr>", "<So>", "<Si>",
    "<Dle>", "<Dcl>", "<Dc2>", "<Dc3>",
    "<Dc4>", "<Nak>", "<Syn>", "<Etb>",
    "<Can>", "<Em>", "<Sub>", "<Esc>",
    "<Fs>", "<Gs>", "<Rs>", "<Us>",
    "<Sp>"
};

/* Concatenate a human-readable form of a character code to
   the supplied string */
private CatChName (ch, scDst)
char    ch;
char    scDst[];
{
    char   *sc;
    char    scT[3];

    if (ch < 0x21)
	sc = rgsc[ch];
    else if (ch == chDel)
	sc = "<Del>";
    else {
	scT[0] = ch;
	scT[1] = '\0';
	sc = scT;
    }
    VOIDC strcat(scDst, sc);
}

/* Output a line of test output for the given command character */
private PrintCmd (ch)
char    ch;
{
    char    scPrint[100];

    scPrint[0] = '\0';
    CatChName (ch, scPrint);
    VOIDC strcat(scPrint, "\n");
    fprintf(fpDbg, scPrint);
}

/* Print a line of test output for and Escape command */
private PrintEscCmd (ch)
char    ch;
{
    char    scPrint[100];

    VOIDC strcpy(scPrint, "<Esc> ");
    CatChName(ch, scPrint);
    fprintf(fpDbg, scPrint);
}

/* Print some diagnostic info on a vector-mode address byte */
private PrintAdrByte (ch, fHiY, fExtra)
char    ch;
char    fHiY, fExtra;
{
    if (ch < chFstCoord) {
	fprintf(fpDbg, "Bad Vector Command");
	PrintCmd (ch);
    }
    else if (ch < chFstLoX) {
	if (fHiY)
	    fprintf(fpDbg, "yHi: %x    ", ch - chFstCoord);
	else
	    fprintf(fpDbg, "xHi: %x    ", ch - chFstCoord);
    }
    else if (ch < chFstLoY)
	fprintf(fpDbg, "xLo: %x\n", ch - chFstLoX);
    else if (ch <= chLstCoord) {
	if (fExtra)
	    fprintf(fpDbg, "x12: %x    y12: %x   margin bit: %s ",
		    ch & mskLoBits,
		    (ch >> shiftYExtra) & mskLoBits,
		    (ch & mskBit4) ? "TRUE" : "FALSE");
	else
	    fprintf(fpDbg, "yLo: %x    ", ch - chFstLoY);
    }
    else {
	fprintf(fpDbg, "Bad Vector Command");
	PrintCmd (ch);
    }
}

/* Print some diagnostic info on a 32-bit integer parameter byte */
private PrintLongByte (ch)
char    ch;
{
    if (ch < chFstLoNegInt) {
	fprintf(fpDbg, "Bad Integer Command");
	PrintCmd (ch);
    }
    else if (ch < chFstLoPosInt)
	fprintf(fpDbg, "iLoNeg: (-) %x, ", ch - chFstLoNegInt);
    else if (ch < chFstHiInt)
	fprintf(fpDbg, "iLoPos: %x, ", ch - chFstLoPosInt);
    else if (ch <= chLstHiInt)
	fprintf(fpDbg, "iHi: %x, ", ch - chFstHiInt);
    else {
	fprintf(fpDbg, "Bad Vector Command");
	PrintCmd (ch);
    }
}

private PrintCr () {
    putc('\n', fpDbg);
}

/* 4110 Command Scanner */
private Skip4110 (ch, psts)
struct Sts *psts;
char    ch;
{
    char    chNxt;
    int     ifmt, i, iprm, xh, yh;
    char   *pfmt;
    long    longT;
#define ichScMax 32
    char    sc[ichScMax];
    struct Real4110 realT;

    chNxt = ChGet (psts);
    if (params.fDbg) {
	PrintCmd (chNxt);
	fprintf(fpDbg, "    ");
    }
    if ((ch < chFst4110) || (ch > chLst4110) || (chNxt < chFstEsc2)
	    || (chNxt > chLstEsc2)) {
	if (params.fDbg)
	    fprintf(fpDbg, "Bad 4110 command: %c%c\n", ch, chNxt);
	fprintf(stderr, "%s: Bad 4110 command: %c%c\n", prog, ch, chNxt);
	return;
    }
    if ((ch == 'S') && (chNxt == 'I')) {
				/* this one command takes five parameters 
				*/
	longT = LongGet (psts);
	GetReal4110 (psts, &realT);
	GetReal4110 (psts, &realT);
	GetReal4110 (psts, &realT);
	GetXhYh (ChGet (psts), psts, &xh, &yh);
	return;
    }
    ifmt = aryaryifmt[ch - chFst4110][chNxt - chFstEsc2];
    pfmt = aryfmt[ifmt];
    for (iprm = 0; iprm < iprmMax; iprm++) {
	switch (pfmt[iprm]) {
	    case typrmNil: 
		goto Exit;
	    case typrmLong: 
		longT = LongGet (psts);
		break;
	    case typrmAryLong: 
		GetAryLong (psts, &longT, 1, &i);
		break;
	    case typrmXhYh: 
		GetXhYh (ChGet (psts), psts, &xh, &yh);
		break;
	    case typrmAryXhYh: 
		GetAryXhYh (psts, &xh, &yh, 1, &i);
		break;
	    case typrmString: 
		GetString (psts, sc, ichScMax);
		break;
	    case typrmReal4110: 
		GetReal4110 (psts, &realT);
		break;
	    case typrmCh: 
		VOIDC ChGet (psts);
		break;
	}
    }
Exit: 
    return;
}

/* 4104 Parser */
private Convert4014 () {
    struct Sts  stsCur;
#define chLim	0x80
    short   rgtymove[chLim];
    short   tymoveT;
    int    ch;
#define ichShowMax 500
    char    scShow[ichShowMax];
    int     ichShowCur;
    int     chT;

    ichShowCur = 0;
    stsCur.md = mdAlpha;
    stsCur.xh = xhHome;
    stsCur.xhLeftMarg = xhHome;
    stsCur.yh = yhHome;
    stsCur.syline = stsCur.sylinePrt = sylineNormal;
    stsCur.sychar = stsCur.sycharPrt = sycharLarge;
    stsCur.charset = stsCur.charsetPrt = charsetStd;
    stsCur.fPenDown = FALSE;
    stsCur.fMovePending = FALSE;
    stsCur.fVectorDrawn = FALSE;
    stsCur.cincr = 0;

    for (chT = 0; chT < chLim; chT++)
	rgtymove[chT] = tymoveNil;
    rgtymove[chBs] = tymoveLeft;
    rgtymove[chTab] = tymoveRt;
    rgtymove[chLf] = tymoveDown;
    rgtymove[chVTab] = tymoveUp;
    rgtymove[chCr] = tymoveXMarg;
    rgtymove[chSp] = tymoveRt;

    VOIDC setjmp(env);

    ch = getc(fpSrc);
    while (ch != EOF) {
	if ((stsCur.md != mdIncrPtPlot) && (stsCur.cincr != 0))
	    DoIncrPtPlot (&stsCur);
	if ((stsCur.md == mdAlpha) &&
		(ch >= chFstShow) && (ch <= chLstShow)) {
	    if (ichShowCur >= ichShowMax - 2) {
		Show (&stsCur, scShow, ichShowCur);
		ichShowCur = 0;
	    }
	    if ((ch == '(') || (ch == ')') || (ch == '\\'))
		scShow[ichShowCur++] = '\\';
	    scShow[ichShowCur++] = ch;
	}
	else {
	    if (ichShowCur > 0) {
		Show (&stsCur, scShow, ichShowCur);
		ichShowCur = 0;
	    }
	    switch (ch) {
		case chEsc: 
		    ch = ChEscapeAction (&stsCur);
		    if (ch == chDel)
			continue;
		    break;
		case chBell: 
		    if (params.fDbg)
			PrintCmd (ch);
		    stsCur.fPenDown = TRUE;
		    break;
		case chFs: 
		    FsAction (&stsCur);
		    break;
		case chGs: 
		    GsAction (&stsCur);
		    break;
		case chRs: 
		    if (params.fDbg)
			PrintCmd (ch);
		    Stroke (&stsCur);
		    stsCur.md = mdIncrPtPlot;
		    stsCur.dir = dirNil;
		    break;
		default: 
		    switch (stsCur.md) {
			case mdAlpha: 
			    Stroke (&stsCur);
			    if ((tymoveT = rgtymove[ch]) != tymoveNil)
				MovePos (&stsCur, tymoveT);
			    if (params.fDbg)
				PrintCmd (ch);
			    break;
			case mdVector: 
			case mdPtPlot: 
			case mdSpecPtPlot: 
			case mdIncrPtPlot: 
			    VectorAction (ch, &stsCur);
			    break;
			case mdBypass: 
			    BypassAction (ch, &stsCur);
			    break;
			default: 
			    fprintf(stderr, "%s: Illegal mode\n",prog);
			    exit (2);
			    break;
		    }
		    break;
	    }
	}
	ch = getc (fpSrc);
    }
    MakeCopy ();
}

#define ARGS "RCNmp:d:l:s:S:"
#define USAGE "ps4014 [-RCNm] [-p outfile] [-l left,bottom] [-s width,height] [-S width] [file]"

main(argc, argv)
int     argc;
char   *argv[];
{
    register int argp;
    extern int optind;
    extern char *optarg;
    char *libdir;

    params.scSrcFile = NULL;
    params.scDstFile = NULL;
    params.scDbgFile = NULL;
    params.fDbg = FALSE;
    params.fLfGenCr = TRUE;
    params.fCrGenLf = TRUE;
    params.fMarg2 = FALSE;
/* these numbers make the image occupy almost the whole page 
   with the correct proportions */
    params.xRtInch = 0.38;
    params.yBotInch = 0.35;
    params.dxWidInch = 10.24;
    params.dyHtInch = 7.8;
    params.fLandscape = TRUE;

    prog = *argv;

    while ((argp = getopt(argc, argv, ARGS)) != EOF) {
	switch (argp) {
	    case 'R':
		params.fLandscape = FALSE;
		break;
	    case 'C':
		params.fCrGenLf = FALSE;
		break;
	    case 'N':
		params.fLfGenCr = FALSE;
		break;
	    case 'm':
		params.fMarg2 = TRUE;
		break;
	    case 'p':
		params.scDstFile = optarg;
		break;
	    case 'd':
		params.scDbgFile = optarg;
		break;
	    case 'l':
	    	if (sscanf(optarg, " %f,%f",
			&params.xRtInch,&params.yBotInch) != 2) {
		    fprintf(stderr,"%s: bad parameter -l%s\n",prog,optarg);
		    exit(2);
		}
		break;
	    case 's':
	    	if (sscanf(optarg, " %f,%f",
			&params.dxWidInch,&params.dyHtInch) != 2) {
		    fprintf(stderr,"%s: bad parameter -s%s\n",prog,optarg);
		    exit(2);
		}
		break;
	    case 'S':
	    	if (sscanf(optarg, " %f",
			&params.dxWidInch) != 1) {
		    fprintf(stderr,"%s: bad parameter -S%s\n",prog,optarg);
		    exit(2);
		}
		params.dyHtInch = 0;
		break;
	    case '?':
	    default:
		fprintf(stderr,"%s: bad option -%c\n",prog,argp);
		exit(2);
	}
    }
    if ((optind + 1) < argc) {
	fprintf(stderr,"%s: %s\n", USAGE);
	exit(2);
    }
    if (optind < argc) {
	params.scSrcFile = argv[optind];
	if ((fpSrc = fopen(params.scSrcFile, "r")) == NULL) {
	    fprintf(stderr,"%s: can't open %s\n",prog,params.scSrcFile);
	    exit(2);
	}
    }
    else fpSrc = stdin;

    if (params.scDstFile == NULL)
	fpPsDst = stdout;
    else {
	if ((fpPsDst = fopen(params.scDstFile, "w")) == NULL) {
	    fprintf(stderr,"%s: can't open output file %s\n",
	    	prog,params.scDstFile);
	    exit(2);
	}
    }
    if (params.scDbgFile != NULL) {
	if ((fpDbg = fopen(params.scDbgFile, "w")) == NULL) {
	    fprintf(stderr,"%s: can't open debug file %s\n",
	    	prog, params.scDbgFile);
	    exit(2);
	}
        params.fDbg = TRUE;
    }
    if ((libdir = envget("PSLIBDIR")) == NULL) libdir = LibDir;
    VOIDC mstrcat(scProlog,libdir,PS4014PRO,sizeof scProlog);
    CommentHeader (&params);
    putc('\n', fpPsDst);

    if (copyfile(scProlog,fpPsDst) != 0) {
	fprintf(stderr,"%s: trouble copying prolog file %s\n",prog,scProlog);
	exit(2);
    }
/* rotate and translate before we scale */
    if (params.fLandscape)
	fprintf(fpPsDst, "90 rotate 0 -8.5 inch translate\n");
    fprintf(fpPsDst, "%g inch %g inch translate\n",
	    params.xRtInch, params.yBotInch);
    if (params.dyHtInch == 0) {	/* we got a dxWidInch but not a dyHtInch */
    /* -> we are scaling y axis proportional to x axis */
	params.dyHtInch = (params.dxWidInch * yhMax) / xhMax;
    }
    fprintf(fpPsDst, "/dxWidInch %g def\n", params.dxWidInch);
    fprintf(fpPsDst, "/dyHtInch %g def\n", params.dyHtInch);
    fprintf(fpPsDst,
     "ScaleCoords\n0 SetCharStyle\n0 SetLineStyle\nxHome yHome moveto\n");

    Convert4014 ();

    fprintf(fpPsDst, "\n%%%%Trailer\ngrestore\n");
    fprintf(fpPsDst, "ps4014sav restore\n");

    VOIDC fclose(fpPsDst);
    VOIDC fclose(fpSrc);
    if (fpDbg != NULL) VOIDC fclose(fpDbg);
    exit(0);

}
