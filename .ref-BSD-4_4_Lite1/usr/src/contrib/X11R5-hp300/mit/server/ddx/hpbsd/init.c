/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/*******************************************************************************
*
* File:         init.c
* RCS:          $Revision: 1.2 $
* Description:  Multiple screen initialization
* Author:       John Howard Palevich
* Created:      April 22, 1987
* Modified:     April 29, 1987  17:41:59 (John Howard Palevich)
* Language:     C
* Package:      USER
* Status:       Experimental (Do Not Distribute)
*
* (c) Copyright 1987, Hewlett-Packard, Inc., all rights reserved.
*
*******************************************************************************/

#include "X.h"
#include "Xproto.h"
#include <servermd.h>
#include "screenint.h"
#include "input.h"
#include "cursor.h"
#include "misc.h"
#include "scrnintstr.h"

#include "gcstruct.h"
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>

#include <sys/types.h>
#include <sys/stat.h>
#ifdef hpux
# include <sys/sysmacros.h>
#endif

#ifndef hpux
# include <sys/ioctl.h>
# include <grfioctl.h>
#else
# include <sys/graphics.h>
#endif
#include "hpDisplays.h"
#include <fcntl.h>

extern char	*display;		/* display number as a string */

static char	xscreens[80];
static int	ConfigFile=FALSE;
#ifdef hpux
static char	DefaultScreen[]="/dev/crt";
#else
static char	DefaultScreen[]="/dev/grf0";
#endif

/*
 * NewRule is an array of replacement rules.  Given a replacement rule,
 *
 *   colunm	foreground pixel	background pixel
 *	0		0			0
 *	1		0			1
 *	2		1			0
 *	3		1			1
 *	4		1		      clear
 *	5		0		      clear
 */

u_char XHP_NewRule [16][6] = 
{
GXclear, GXclear,	GXclear,       GXclear, GXandInverted,GXandInverted,
GXclear, GXandInverted, GXand,	       GXnoop,  GXnoop,	      GXandInverted,
GXclear, GXnor,	        GXandReverse,  GXinvert,GXxor,	      GXandInverted,
GXclear, GXcopyInverted,GXcopy,	       GXset,   GXor,	      GXandInverted,
GXnoop,  GXand,		GXandInverted, GXclear, GXandInverted,GXnoop,
GXnoop,  GXnoop,	GXnoop,	       GXnoop,  GXnoop,	      GXnoop,
GXnoop,  GXequiv,	GXxor,	       GXinvert,GXxor,	      GXnoop,
GXnoop,  GXorInverted,	GXor,	       GXset,   GXor,	      GXnoop,
GXinvert,GXandReverse,  GXnor,	       GXclear, GXandInverted,GXxor,
GXinvert,GXxor,		GXequiv,       GXnoop,  GXnoop,	      GXxor,
GXinvert,GXinvert,	GXinvert,      GXinvert,GXxor,	      GXxor,
GXinvert,GXnand,	GXorReverse,   GXset,   GXor,	      GXxor,
GXset, 	 GXcopy,	GXcopyInverted,GXclear, GXandInverted,GXor,
GXset, 	 GXor,		GXorInverted,  GXnoop,  GXnoop,	      GXor,
GXset, 	 GXorReverse,	GXnand,	       GXinvert,GXxor,	      GXor,
GXset, 	 GXset,		GXset,	       GXset,	GXor,	      GXor
};

#define IOMAP_BASE 0xb00000
static unsigned char *iomapBase;

int TopcatBrainDamage = 0;
int catseyeMono = 0;


static PixmapFormatRec formats[] = {
    1, 1, BITMAP_SCANLINE_PAD,	/* 1-bit deep for all */
#if 0
    4, 4, BITMAP_SCANLINE_PAD,  /* 4-bit deep for Burgundy */
#endif
    8, 8, BITMAP_SCANLINE_PAD,	/* 8-bit deep for most color displays */
#if 0
   16,16, BITMAP_SCANLINE_PAD,	/*16-bit deep for most color displays */
#endif
};
#define NUMFORMATS	(sizeof formats)/(sizeof formats[0])


/* Pmap represents all 256 combinations of 8 bits of information. */
/* For example, if my byte of 8 pixels of information is 01010101 */
/* then the bytes that get written in memory are 00ff00ff,00ff00ff*/
/* The bytes are written using the current write enable and       */
/* drawmode value.                                                */

   int XHP_pmap[256][2]={0x00000000,0x00000000,
                         0x00000000,0x000000ff,
                         0x00000000,0x0000ff00,
                         0x00000000,0x0000ffff,
                         0x00000000,0x00ff0000,
                         0x00000000,0x00ff00ff,
                         0x00000000,0x00ffff00,
                         0x00000000,0x00ffffff,
                         0x00000000,0xff000000,
                         0x00000000,0xff0000ff,
                         0x00000000,0xff00ff00,
                         0x00000000,0xff00ffff,
                         0x00000000,0xffff0000,
                         0x00000000,0xffff00ff,
                         0x00000000,0xffffff00,
                         0x00000000,0xffffffff,
                         0x000000ff,0x00000000,
                         0x000000ff,0x000000ff,
                         0x000000ff,0x0000ff00,
                         0x000000ff,0x0000ffff,
                         0x000000ff,0x00ff0000,
                         0x000000ff,0x00ff00ff,
                         0x000000ff,0x00ffff00,
                         0x000000ff,0x00ffffff,
                         0x000000ff,0xff000000,
                         0x000000ff,0xff0000ff,
                         0x000000ff,0xff00ff00,
                         0x000000ff,0xff00ffff,
                         0x000000ff,0xffff0000,
                         0x000000ff,0xffff00ff,
                         0x000000ff,0xffffff00,
                         0x000000ff,0xffffffff,
                         0x0000ff00,0x00000000,
                         0x0000ff00,0x000000ff,
                         0x0000ff00,0x0000ff00,
                         0x0000ff00,0x0000ffff,
                         0x0000ff00,0x00ff0000,
                         0x0000ff00,0x00ff00ff,
                         0x0000ff00,0x00ffff00,
                         0x0000ff00,0x00ffffff,
                         0x0000ff00,0xff000000,
                         0x0000ff00,0xff0000ff,
                         0x0000ff00,0xff00ff00,
                         0x0000ff00,0xff00ffff,
                         0x0000ff00,0xffff0000,
                         0x0000ff00,0xffff00ff,
                         0x0000ff00,0xffffff00,
                         0x0000ff00,0xffffffff,
                         0x0000ffff,0x00000000,
                         0x0000ffff,0x000000ff,
                         0x0000ffff,0x0000ff00,
                         0x0000ffff,0x0000ffff,
                         0x0000ffff,0x00ff0000,
                         0x0000ffff,0x00ff00ff,
                         0x0000ffff,0x00ffff00,
                         0x0000ffff,0x00ffffff,
                         0x0000ffff,0xff000000,
                         0x0000ffff,0xff0000ff,
                         0x0000ffff,0xff00ff00,
                         0x0000ffff,0xff00ffff,
                         0x0000ffff,0xffff0000,
                         0x0000ffff,0xffff00ff,
                         0x0000ffff,0xffffff00,
                         0x0000ffff,0xffffffff,
                         0x00ff0000,0x00000000,
                         0x00ff0000,0x000000ff,
                         0x00ff0000,0x0000ff00,
                         0x00ff0000,0x0000ffff,
                         0x00ff0000,0x00ff0000,
                         0x00ff0000,0x00ff00ff,
                         0x00ff0000,0x00ffff00,
                         0x00ff0000,0x00ffffff,
                         0x00ff0000,0xff000000,
                         0x00ff0000,0xff0000ff,
                         0x00ff0000,0xff00ff00,
                         0x00ff0000,0xff00ffff,
                         0x00ff0000,0xffff0000,
                         0x00ff0000,0xffff00ff,
                         0x00ff0000,0xffffff00,
                         0x00ff0000,0xffffffff,
                         0x00ff00ff,0x00000000,
                         0x00ff00ff,0x000000ff,
                         0x00ff00ff,0x0000ff00,
                         0x00ff00ff,0x0000ffff,
                         0x00ff00ff,0x00ff0000,
                         0x00ff00ff,0x00ff00ff,
                         0x00ff00ff,0x00ffff00,
                         0x00ff00ff,0x00ffffff,
                         0x00ff00ff,0xff000000,
                         0x00ff00ff,0xff0000ff,
                         0x00ff00ff,0xff00ff00,
                         0x00ff00ff,0xff00ffff,
                         0x00ff00ff,0xffff0000,
                         0x00ff00ff,0xffff00ff,
                         0x00ff00ff,0xffffff00,
                         0x00ff00ff,0xffffffff,
                         0x00ffff00,0x00000000,
                         0x00ffff00,0x000000ff,
                         0x00ffff00,0x0000ff00,
                         0x00ffff00,0x0000ffff,
                         0x00ffff00,0x00ff0000,
                         0x00ffff00,0x00ff00ff,
                         0x00ffff00,0x00ffff00,
                         0x00ffff00,0x00ffffff,
                         0x00ffff00,0xff000000,
                         0x00ffff00,0xff0000ff,
                         0x00ffff00,0xff00ff00,
                         0x00ffff00,0xff00ffff,
                         0x00ffff00,0xffff0000,
                         0x00ffff00,0xffff00ff,
                         0x00ffff00,0xffffff00,
                         0x00ffff00,0xffffffff,
                         0x00ffffff,0x00000000,
                         0x00ffffff,0x000000ff,
                         0x00ffffff,0x0000ff00,
                         0x00ffffff,0x0000ffff,
                         0x00ffffff,0x00ff0000,
                         0x00ffffff,0x00ff00ff,
                         0x00ffffff,0x00ffff00,
                         0x00ffffff,0x00ffffff,
                         0x00ffffff,0xff000000,
                         0x00ffffff,0xff0000ff,
                         0x00ffffff,0xff00ff00,
                         0x00ffffff,0xff00ffff,
                         0x00ffffff,0xffff0000,
                         0x00ffffff,0xffff00ff,
                         0x00ffffff,0xffffff00,
                         0x00ffffff,0xffffffff,
                         0xff000000,0x00000000,
                         0xff000000,0x000000ff,
                         0xff000000,0x0000ff00,
                         0xff000000,0x0000ffff,
                         0xff000000,0x00ff0000,
                         0xff000000,0x00ff00ff,
                         0xff000000,0x00ffff00,
                         0xff000000,0x00ffffff,
                         0xff000000,0xff000000,
                         0xff000000,0xff0000ff,
                         0xff000000,0xff00ff00,
                         0xff000000,0xff00ffff,
                         0xff000000,0xffff0000,
                         0xff000000,0xffff00ff,
                         0xff000000,0xffffff00,
                         0xff000000,0xffffffff,
                         0xff0000ff,0x00000000,
                         0xff0000ff,0x000000ff,
                         0xff0000ff,0x0000ff00,
                         0xff0000ff,0x0000ffff,
                         0xff0000ff,0x00ff0000,
                         0xff0000ff,0x00ff00ff,
                         0xff0000ff,0x00ffff00,
                         0xff0000ff,0x00ffffff,
                         0xff0000ff,0xff000000,
                         0xff0000ff,0xff0000ff,
                         0xff0000ff,0xff00ff00,
                         0xff0000ff,0xff00ffff,
                         0xff0000ff,0xffff0000,
                         0xff0000ff,0xffff00ff,
                         0xff0000ff,0xffffff00,
                         0xff0000ff,0xffffffff,
                         0xff00ff00,0x00000000,
                         0xff00ff00,0x000000ff,
                         0xff00ff00,0x0000ff00,
                         0xff00ff00,0x0000ffff,
                         0xff00ff00,0x00ff0000,
                         0xff00ff00,0x00ff00ff,
                         0xff00ff00,0x00ffff00,
                         0xff00ff00,0x00ffffff,
                         0xff00ff00,0xff000000,
                         0xff00ff00,0xff0000ff,
                         0xff00ff00,0xff00ff00,
                         0xff00ff00,0xff00ffff,
                         0xff00ff00,0xffff0000,
                         0xff00ff00,0xffff00ff,
                         0xff00ff00,0xffffff00,
                         0xff00ff00,0xffffffff,
                         0xff00ffff,0x00000000,
                         0xff00ffff,0x000000ff,
                         0xff00ffff,0x0000ff00,
                         0xff00ffff,0x0000ffff,
                         0xff00ffff,0x00ff0000,
                         0xff00ffff,0x00ff00ff,
                         0xff00ffff,0x00ffff00,
                         0xff00ffff,0x00ffffff,
                         0xff00ffff,0xff000000,
                         0xff00ffff,0xff0000ff,
                         0xff00ffff,0xff00ff00,
                         0xff00ffff,0xff00ffff,
                         0xff00ffff,0xffff0000,
                         0xff00ffff,0xffff00ff,
                         0xff00ffff,0xffffff00,
                         0xff00ffff,0xffffffff,
                         0xffff0000,0x00000000,
                         0xffff0000,0x000000ff,
                         0xffff0000,0x0000ff00,
                         0xffff0000,0x0000ffff,
                         0xffff0000,0x00ff0000,
                         0xffff0000,0x00ff00ff,
                         0xffff0000,0x00ffff00,
                         0xffff0000,0x00ffffff,
                         0xffff0000,0xff000000,
                         0xffff0000,0xff0000ff,
                         0xffff0000,0xff00ff00,
                         0xffff0000,0xff00ffff,
                         0xffff0000,0xffff0000,
                         0xffff0000,0xffff00ff,
                         0xffff0000,0xffffff00,
                         0xffff0000,0xffffffff,
                         0xffff00ff,0x00000000,
                         0xffff00ff,0x000000ff,
                         0xffff00ff,0x0000ff00,
                         0xffff00ff,0x0000ffff,
                         0xffff00ff,0x00ff0000,
                         0xffff00ff,0x00ff00ff,
                         0xffff00ff,0x00ffff00,
                         0xffff00ff,0x00ffffff,
                         0xffff00ff,0xff000000,
                         0xffff00ff,0xff0000ff,
                         0xffff00ff,0xff00ff00,
                         0xffff00ff,0xff00ffff,
                         0xffff00ff,0xffff0000,
                         0xffff00ff,0xffff00ff,
                         0xffff00ff,0xffffff00,
                         0xffff00ff,0xffffffff,
                         0xffffff00,0x00000000,
                         0xffffff00,0x000000ff,
                         0xffffff00,0x0000ff00,
                         0xffffff00,0x0000ffff,
                         0xffffff00,0x00ff0000,
                         0xffffff00,0x00ff00ff,
                         0xffffff00,0x00ffff00,
                         0xffffff00,0x00ffffff,
                         0xffffff00,0xff000000,
                         0xffffff00,0xff0000ff,
                         0xffffff00,0xff00ff00,
                         0xffffff00,0xff00ffff,
                         0xffffff00,0xffff0000,
                         0xffffff00,0xffff00ff,
                         0xffffff00,0xffffff00,
                         0xffffff00,0xffffffff,
                         0xffffffff,0x00000000,
                         0xffffffff,0x000000ff,
                         0xffffffff,0x0000ff00,
                         0xffffffff,0x0000ffff,
                         0xffffffff,0x00ff0000,
                         0xffffffff,0x00ff00ff,
                         0xffffffff,0x00ffff00,
                         0xffffffff,0x00ffffff,
                         0xffffffff,0xff000000,
                         0xffffffff,0xff0000ff,
                         0xffffffff,0xff00ff00,
                         0xffffffff,0xff00ffff,
                         0xffffffff,0xffff0000,
                         0xffffffff,0xffff00ff,
                         0xffffffff,0xffffff00,
                         0xffffffff,0xffffffff}; /*XHP_pmap end*/

int XHP_QUADALIGN;
static jmp_buf env;
/*
 * routine to handle the bus error we might get in testing for the alignment
 * restrictions of this cpu.
 */
static int 
sigbusHandler()
{
    XHP_QUADALIGN = 1;
    longjmp(env, 1);
}

static int ReadLine();

typedef struct {
    char *productNumber;
    char *productNickname;
    Bool (*InfoScreen)();
    Bool (*InitScreen)();
} ScreenTableRec;

static ScreenTableRec *FindScreen();

#define MAXARG 10
#define MAXSTRING 120

/*-
 *-----------------------------------------------------------------------
 * InitOutput --
 *	Initialize screenInfo for all actually accessible framebuffers.
 *	The
 *
 * Results:
 *	screenInfo init proc field set
 *
 * Side Effects:
 *	None
 *
 *-----------------------------------------------------------------------
 */

void
InitOutput(pScreenInfo, argc, argv)
     ScreenInfo	  *pScreenInfo;
     int     	  argc;
     char    	  **argv;
{
    int i;
    FILE *in;
    static int firstTime = 1;
    static int numScreens;
    static ScreenTableRec *FoundScreens[MAXSCREENS];
    ScreenTableRec *s;
#ifdef hpux
    char *dispaddr, *getenv();
#endif
    struct stat statbuf;
    char minornumber[10];

    /*
     * test for data alignment restriction of this cpu.  If this cpu
     * doesn't allow long-word writes on an odd address, then we assume
     * that it requires quad-word alignment.  This reduces our headaches
     * to only two cases - 68020s (no restrictions) and others (e.g.
     * 68010 and Spectrum).
     */
    {
	int *test;
	char foo[8]; 
	static struct sigvec timeout_vec = { sigbusHandler, 0, 0 };
	struct sigvec old_vec;


	test = (int *) ((int)foo | 3) + 1; /* generate an odd address */
	XHP_QUADALIGN = 0;

#ifdef hp300
#define sigvector sigvec
#endif
#if defined(hp9000s300) || defined(hp300)     /* check for 310 */
	sigvector(SIGBUS, &timeout_vec, &old_vec);
	if(!setjmp(env))
	    *test = 1;  /* generate a bus error on 68010s or Spectrums */
	sigvector(SIGBUS, &old_vec, 0);
#else  /* need word align on 800 */
	XHP_QUADALIGN = 1;
#endif /* hp9000s300 */
    }

    iomapBase = (unsigned char *) IOMAP_BASE;
#ifdef hpux
    if (dispaddr = getenv("SB_DISPLAY_ADDR"))
	iomapBase = (unsigned char *) strtol(dispaddr, (char **)NULL, 0);
#endif

    pScreenInfo->imageByteOrder = IMAGE_BYTE_ORDER;
    pScreenInfo->bitmapScanlineUnit = BITMAP_SCANLINE_UNIT;
    pScreenInfo->bitmapScanlinePad = BITMAP_SCANLINE_PAD;
    pScreenInfo->bitmapBitOrder = BITMAP_BIT_ORDER;
    pScreenInfo->numPixmapFormats = NUMFORMATS;
    for (i=0; i< NUMFORMATS; i++)
	pScreenInfo->formats[i] = formats[i];
    
    if (firstTime)
    {
	firstTime = 0;
	    
	(void) sprintf(xscreens, "%s/X%sscreens", LIBDIR, display);

	if (NULL == (in = fopen (xscreens, "r")))
	{
	    perror(xscreens);
	    ErrorF("Can't open screen configuration file, defaulting to %s.\n",
		   DefaultScreen);
	    ConfigFile=FALSE;
        }
	else
	    ConfigFile=TRUE;

	numScreens = 0;
	for (i = 0; i<MAXSCREENS && ((!ConfigFile) ? TRUE : ! feof(in) ); i++)
	{
	    char *argv[MAXARG];
	    int argc;

	    if (!ConfigFile)
		argv[0] = DefaultScreen;
	    else
	    {
		while (!(argc = ReadLine(in, argv))) /* handle blank lines */
		    ;

		if (argc == -1)	/* eof */
		    break;

		if (argc < 0)
		{
		    ErrorF("InitOutput: %s: line %d: Too many fields.\n",
			   xscreens, i+1);
		    goto fatal_error;
		}

		/* for compatibility with R2 screens tables */
		if (argc == 3)
		{
		    if ((strcmp (argv[0], "topcat") == 0) || 
			(strcmp (argv[0], "Topcat") == 0))
		    {
			argv[0] = argv[2];
			argc = 1;
		    }
		    else
		    {
			ErrorF ("InitOutput:  %s, line %d:  unsupported display type \"%s\".\n",
				xscreens, i+1, argv[0]);
			goto fatal_error;
		    }
		}

		if ( argc != 1 )
		{
		    ErrorF("InitOutput: %s: line %d: Wrong number of fields.\n", xscreens, i+1);
		    goto fatal_error;
		}
	    }
    
	    if ((s = FindScreen(argv[0])) == NULL)
	    {
		ErrorF("InitOutput: %s: line %d: Unknown screen %s.\n",
		       xscreens, i+1, argv[0]);
		goto fatal_error;
	    }

	    /* munge new structure to match old argv structures */
	    /* BOGOSITY ALERT argv and argc are munged for subsequent calls */

	    argv[2] = argv[0];
	    argv[0] = s->productNickname;

	    if (stat(argv[2], &statbuf) < 0)
	    {
		ErrorF("InitOutput: %s: could not stat %s.\n",argv[0],argv[2]);
		goto fatal_error;
	    }
#ifdef hpux
	    (void) sprintf(minornumber, "%x", minor(statbuf.st_rdev));
#else
	    (void) sprintf(minornumber, "%d", minor(statbuf.st_rdev));
#endif
	    argv[3] = minornumber;
	    argc = 4;

	    if (!(s->InfoScreen)(numScreens, argv, argc))
	    {
		ErrorF("InitOutput: %s: line %d: Couldn't find this screen %s.\n",
		       xscreens, i+1, argv[0]);
		goto fatal_error;
	    }

	    FoundScreens[numScreens++] = s;

	    if (!ConfigFile) break;
	}

	if (ConfigFile)
	    fclose(in);
    }

    for (i=0; i<numScreens; i++)
    {
	s = FoundScreens[i];
	if (AddScreen(s->InitScreen, argc, argv) < 0)
	{
	    ErrorF("InitOutput: Couldn't add a screen.\n");
	    exit (1);
	}
    }

    return;

  fatal_error:
    fclose(in);
    ErrorF("InitOutput: Couldn't initialize screens.\n");
    exit(1);
}

static int
ReadLine(in, argv)
    FILE *in;
    char **argv;
{
    int argc;
    static char line[MAXSTRING];
    char *s;
    int state;

    if (NULL == fgets(line, MAXSTRING, in))
	return(-1);

    for (state = argc = 0, s = line; argc < MAXARG; s++)
    {
	switch ( *s )
	{
	    case '#':
	    case '\n':
		*s = '\0';
	    case '\0':
		return (argc);
	    case ' ':
	    case '\t':
		state = 0;
		*s = '\0';
		break;
	    default:
		if ( state == 0 )
		{
		    state = 1;
		    argv[argc++] = s;
		}
		break;
	}
    }
    return(-2);
}

#define STUPID_MOBERLY -1

/* Declare the ScreenInfo(), ScreenInit() and ScreenClose() functions here.
 */
extern Bool mobScreenInfo(), mobScreenInit();
extern Bool gbxScreenInfo(), gbxScreenInit();
extern Bool topcatScreenInfo(), topcatScreenInit();
extern Bool mrtopcatScreenInfo(), mrtopcatScreenInit();
extern Bool catseyeScreenInfo(), catseyeScreenInit();
extern Bool renScreenInfo(), renScreenInit();
extern Bool orenScreenInfo(), orenScreenInit();
extern Bool davinciScreenInfo(), davinciScreenInit();
extern Bool oDavinciScreenInit();
extern Bool hyperScreenInfo(), hyperScreenInit();

/*
 * Table of known frame buffers
 */

static ScreenTableRec screenTable[] = {
    {"98633", "moberly", mobScreenInfo, mobScreenInit},
    {"98700", "gatorbox", gbxScreenInfo, gbxScreenInit},
    {"98547", "topcat", topcatScreenInfo, topcatScreenInit},
    {"98543", "mrtopcat", mrtopcatScreenInfo, mrtopcatScreenInit},
    {"98550", "catseye", topcatScreenInfo, topcatScreenInit},
    {"98720", "renaissance", renScreenInfo, renScreenInit},
    {"98720", "orenaissance", orenScreenInfo, orenScreenInit},
    {"98730", "davinci", davinciScreenInfo, davinciScreenInit},
    {"98730", "odavinci", davinciScreenInfo, oDavinciScreenInit},
    {"a1096a", "hyperion", hyperScreenInfo, hyperScreenInit},
    {(char *) NULL, (char *) NULL, (Bool (*)()) NULL, (Bool (*)()) NULL}
};

static ScreenTableRec *
FindScreen(devname)
    char *devname;
{
    ScreenTableRec *s;
    int fd, gcid;
#ifdef hpux
    int gcon;
#endif
    char name[40];
    static struct stat buf;
    struct hp_grfreg *ce;

    if ((fd = open(devname, O_RDWR)) <  0)
    {
        perror(devname);
        ErrorF("FindScreen couldn't open %s \n", devname);
        return (NULL);
    }
#ifdef hpux
    if (ioctl(fd, GCON, &gcon) < 0 || ioctl(fd, GCID, &gcid) < 0)
	gcid = STUPID_MOBERLY;
#else
    {
	struct grfinfo gi;

	if (ioctl(fd, GRFIOCGINFO, &gi) < 0 || ioctl(fd, GRFIOCON, 0) < 0)
	    gcid = STUPID_MOBERLY;
	else
	    gcid = gi.gd_id;
    }
#endif
    fstat(fd, &buf);

    switch (gcid)
    {
      case GCID_GATORBOX:
	strcpy(name, "gatorbox");
	break;
      case GCID_TOPCAT:
#ifdef hpux
	ce = (struct hp_grfreg *) iomapBase;

	if (ioctl(fd, GCMAP, &ce) < 0)
	{
	    perror("GCMAP:");
	    ErrorF("FindScreen: Error getting address of %s\n", devname);
	    close (fd);
	    return (NULL);
	}
#else
        {
	    u_char *Addr = (u_char *) 0;
		  
	    if (ioctl (fd, GRFIOCMAP, &Addr) < 0)
	    {
		(void) ioctl (fd, GRFIOCOFF, 0);
		perror("GRFIOCMAP:");
		ErrorF("FindScreen: Error getting address of %s\n", devname);
		close (fd);
		return (NULL);
	    }
	    ce = (struct hp_grfreg *) Addr;
	}
#endif
	if (ce->gr_id2 >= ID2_LCC)
	    strcpy(name, "catseye");
	else if (ce->gr_bits)
	    strcpy(name, "mrtopcat");
	else
	    strcpy(name, "topcat");
#ifdef hpux
	if (ioctl(fd, GCUNMAP, &ce) < 0)
	{
	    perror("GCUNMAP:");
	    ErrorF("FindScreen: Error freeing temp storage %s\n", devname);
	    close (fd);
	    return (NULL);
	}	
#else
	if (ioctl(fd, GRFIOCUNMAP, &ce) < 0)
	{
	    perror("GRFIOCUNMAP:");
	    ErrorF("FindScreen: Error freeing temp storage %s\n", devname);
	    close (fd);
	    return (NULL);
	}
#endif
	break;
      case GCID_RENAISSANCE:
	if ((minor(buf.st_rdev)) & 0x000003)
	    strcpy(name, "orenaissance");
	else
	    strcpy(name, "renaissance");
	break;
      case GCID_FIREEYE:
	strcpy(name, "catseye");
	break;
      case GCID_HYPERION:
	strcpy(name, "hyperion");
	break;
      case GCID_DAVINCI:
	if ((minor(buf.st_rdev)) & 0x000003)
	    strcpy(name, "odavinci");
	else
	    strcpy(name, "davinci");
	break;
      case STUPID_MOBERLY:
	strcpy(name, "moberly");
	break;
      default:
	ErrorF("FindScreen: unknown screen type  %s \n", devname);
	return (NULL);
    }
    close (fd);

    for (s = screenTable; s->productNumber != NULL; s++)
    {
	if (strcmp(s->productNumber, name) == 0 ||
	    strcmp(s->productNickname, name) == 0)
	    return (s);
    }
    return (NULL);
}

#ifdef hpux
unsigned char *
FrameBufferBase(size)
    long int size;
{
    unsigned char *base = iomapBase;

    /* Round size to a 4K page */
    size = (size + 4095) & 0xfffff000;
    iomapBase += size;
    return ( base );
}
#endif

/*
 * DDX - specific abort routine.  Called by AbortServer().
 */
void
AbortDDX()
{
}

/* Called by GiveUp(). */
void
ddxGiveUp()
{
}

int
ddxProcessArgument (argc, argv, i)
    int	argc;
    char *argv[];
    int	i;
{
    if (strcmp (argv[i], "-tcbd") == 0)
    {
	TopcatBrainDamage++;
	return 1;
    }
    if (strcmp (argv[i], "-catseyeMono") == 0)
    {
	catseyeMono++;
	return 1;
    }
    return 0;
}

void
ddxUseMsg()
{
    ErrorF("-tcbd                  run in topcat braindamage mode\n");
    ErrorF("-catseyeMono           run 2bit deep catseyes in Monochrome\n");
}
