/* @(#)menu.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 * 
 *      This file contains the routines for initializing and displaying
 * the graphics menu.
 */

#include "gremlin.h"

/* imports from graphics1.c */

extern GRchex();                 /* hex conversion routine */
extern GRsetpos();               /* sets the current access position */
extern GRVector();               /* vector drawing primitive */
extern int GrXMax, GrYMax;       /* screen dimensions */
extern FILE *display;            /* graphics display stream */
extern GRDisableTablet();        /* disable graphics tablet */

/*  imports from long.c */

extern LGCommand(), LGPoint(), LGUndo(), LGDeletePoint();

/*  imports from short.c */

extern SHCommand();

/* imports from main.c */

extern float PX, PY;            /* X and Y of last points specified */

#define horiz 0
#define vert 1
#define menusize 32
#define menuspace 30
#define origin 10
#define margin 25
#define itemxsize 16
#define itemysize 16
#define fontsize (itemxsize * itemysize/8)
#define itemlength (itemxsize * itemysize/4 + 1)
#define maxchars 8
#define smask 1
#define columns 2
#define border 395


/* bytewise definitions of menu symbols (special fonts), left to right,
 * top to bottom.  See textcommand array for corresponding commands.
 */

static char menu[menusize][itemlength] =
{
/* 1 */  "07F00080008000800080008000800080008000800080008000800080008007F0",
/* 2 */  "08100810082008200840084008800F0008C00820081008100810082008C00F00",
/* 3 */  "0000000000000000000000000000FFFFFFFFFFFF000000000000000000000000",
/* 4 */  "0000000000000000000000000000E4E4E4E40000000000000000000000000000",
/* 5 */  "0000000000000000000000000000924992490000000000000000000000000000",
/* 6 */  "FFFF8001800187E18401840183018081804180218421842183C180018001FFFF",
/* 7 */  "FFFF80018001808180818081808180818081808182818181808180018001FFFF",
/* 8 */  "008001C002A0008000800080008000800080008000800080008002A001C00080",
/* 9 */  "00000000000000000000000020044002FFFF4002200400000000000000000000",
/* 10 */ "000080028002400440042008183007C0FFFF07C0183020084004400480028002",
/* 11 */ "20002000FFFC2704288430443044304428842704200427C424443FFF044407C4",
/* 12 */ "FFFF804080E0815080408080808081008600F800800080008000800080008000",
/* 13 */ "00FF008100810081008100810E8106FF0A00100020000000F00090009000F000",
/* 14 */ "FFFF8001A001900180018401820180018001804180218001800980058001FFFF",
/* 15 */ "000000000000000030001C000B800460021804600B801C003000000000000000",
/* 16 */ "00000000000000001C02220143018101808180C2804440380000000000000000",
/* 17 */ "03C004200010001000100010002003C004000800080008000800042003C00000",
/* 18 */ "0F0008C008200810081008100810082008C00F0008C008200820082008C00F00",
/* 19 */ "00000000000000000000000000000000FFFFFFFF000000000000000000000000",
/* 20 */ "00000000000000000000000000000000FFFF0000000000000000000000000000",
/* 21 */ "00000000000000000000000000000000F87C0000000000000000000000000000",
/* 22 */ "FFFF800180018041804180418041804187E1844184418441804180018001FFFF",
/* 23 */ "FFFF80018001838184418821802180418181804180218821844183818001FFFF",
/* 24 */ "80028002400440042008183007C001000280044008201EF00280028002800280",
/* 25 */ "000400023FFF2002200420002000200020002000200020002000A80070002000",
/* 26 */ "20042004FFFF2004200420042004200420042004200420042004FFFF20042004",
/* 27 */ "FFFF9C39A245C183C183C183A2459C3981C18221841184118411822181C1FFFF",
/* 28 */ "800140023FFC300C28142424224421842184224424242814300C3FFC40028001",
/* 29 */ "000000000000000F00090009F04F902097F0F02F00490009000F000000000000",
/* 30 */ "F00090009000F000080004000200014000C001C00000001E00120012001E0000",
/* 31 */ "000007C0183020084004400480028002810280028002400440042008183007C0",
/* 32 */ "02000100FF80018002400040002000200010001000082019400EFFFC40002000"
};

/* menu item color map  */

static int mencolor[menusize] = {
4, 1, 3, 1, 1, 6, 6, 1, 1, 5, 5, 5, 5, 3, 3, 3, 6, 5, 2, 1, 1, 6, 6, 1, 1, 1, 
5, 5, 5, 5, 3, 3 };

/* menu equivalent text commands */

static char textcommand[menusize][maxchars] =
{
/* 1 */  ":f,2",		/* font,2 */
/* 2 */  ":f,1",		/* font,1 */
/* 3 */  ":br,3",		/* brush,3 */
/* 4 */  ":br,2",		/* brush,2 */
/* 5 */  ":br,1",		/* brush,1 */
/* 6 */  ":buf,2",		/* set buffer 2  */
/* 7 */  ":buf,1",		/* set buffer 1  */
/* 8 */  ":vadj",		/* vertical adjust */
/* 9 */  ":hadj",		/* horizontal adjust */
/* 10 */  ":mi",		/* mirror */
/* 11 */  "f",			/* select area in current set */
/* 12 */  "r",			/* rotate current set */
/* 13 */  "s",			/* scale current set */
/* 14 */  "x",    		/* rectangle from 2 points */
/* 15 */  "w",			/* arrow heads */
/* 16 */  "b",			/* draw curve */
/* 17 */  ":f,4",		/* font,4 */
/* 18 */  ":f,3",		/* font,3 */
/* 19 */  ":br,6",		/* brush,6 */
/* 20 */  ":br,5",		/* brush,5 */
/* 21 */  ":br,4",		/* brush,4 */
/* 22 */  ":buf,4",		/* set buffer 4 */
/* 23 */  ":buf,3",		/* set buffer 3 */
/* 24 */  "g",			/* gravity */
/* 25 */  "z",			/* manhattan adjust */
/* 26 */  "q",			/* grid */
/* 27 */  "d",			/* define current set */
/* 28 */  "e",			/* erase */
/* 29 */  "c",			/* copy current set */
/* 30 */  "t",			/* translate current set */
/* 31 */  "a",			/* draw arc */
/* 32 */  "v",			/* draw vector */
};

/*  screen map for menu symbols  */

struct bounds
{
	int lowx, lowy, hix, hiy;
};

static struct bounds area[menusize];

/* Following is a map of brushes and mode made available to the
 * outside world for selection of these items for highlighting.
 * The numbers indicate the items location in the menu.
 */

int HiMen[4] = { 6, 5, 22, 21 };      /* user symbols */
int HiFont[4] = { 1, 0, 17, 16};         /* fonts */
int HiBrush[6] = { 4, 3, 2, 20, 19, 18 };  /* brushes */
int HiMode[4] = { 8, 7, 24, 23 };   /* horz, vert, man, grav. */

static int xorig, yorig, deltax, deltay;
static POINT bord1, bord2;

/* forward references within this file */

extern MNHighLt(), MNUnHighLt();
/* variables to save previous command */

static char *last;
static lastint;

MNIcon()
/*
 *     This routine initializatizes the menu symbols by defining to
 * the AED each menu symbol as a special font.  The special fonts are
 * defined by a series of bytes in the array menu which is initialized.
 * they could also be read in from an external file.
 */

{
	int i;

#ifndef FASTIO
	char s1[3], s2[3], s3[3], s4[3];
#else
	int  c, k;
#endif

		/* add code here to read in special fonts if 
		   desired.                                  */

	for (i=1; i<=menusize; ++i)    
	{
		putc('7',display);		/* define special font */

#ifndef FASTIO
		GRchex(i,s1,2);
		GRchex(itemxsize,s2,2);
		GRchex(itemysize,s3,2);
		GRchex(mencolor[i-1],s4,2);
		fprintf(display,"%s%s%s%s%s00",s1,s2,s3,s4,menu[i-1]);
#else
		fprintf(display,"%c%c%c%c",i&0377, itemxsize&0377,
		                    itemysize&0377, mencolor[i-1]&0377);
		for (k=0; k<itemlength-1; k+=2)
		{
			(void) sscanf(&(menu[i-1][k]),"%2x",&c);
			putc(c&0377,display);
		};   /* end for k */
		putc('\0',display);
#endif

	};     /* end for i */
	(void) fflush(display);
};   /* end Icon */


MNInitMenu(orientation)
int orientation;		/* orientation of work space */
/*
 *     This routine initializatizes the spacing and orientation of
 * the menu so that elements can be properly displayed and selected.
 */

{
	int xcum, ycum, i;

	last = textcommand[0];
	lastint = 0;

	if (orientation == horiz)   /* set up spacing for menu display */
	{
		xorig = GrXMax - origin - menuspace;
		yorig = GrYMax - menuspace;
		deltax = -menuspace;
		deltay = 0;
		bord1.y = bord2.y = border;
		bord1.x = 0; 
		bord2.x = GrXMax;
	}
	else
	{
		xorig = margin ;
		yorig = origin;
		deltax = 0;
		deltay = menuspace;
		bord1.x = bord2.x = GrXMax - border;
		bord1.y = 0; 
		bord2.y = GrYMax;
	};
	xcum = xorig - (menuspace - itemxsize)/2;
	ycum = yorig - (menuspace - itemysize)/2;

	for (i=0; i<menusize; ++i)    /* store boundaries for each menu item */
	{
		area[i].lowx = xcum;
		area[i].lowy = ycum;
		area[i].hix = xcum + menuspace;
		area[i].hiy = ycum + menuspace;
		xcum += deltax;
		ycum += deltay;
		if (((i+1)%(menusize/columns)) == 0) /* end of row/col */
		{
			xcum = xorig + deltay - (menuspace - itemxsize)/2;
			ycum = yorig + deltax - (menuspace - itemysize)/2;
		}    /* end if */;
	}   /* end for */;
}   /* end initmenu */


MNDisplayMenu()
/*
 *      This routine displays the menu defined by initmenu
 */

{
	int i;

#ifndef FASTIO
	char s1[3], s2[3], s3[3];
#endif

	GRsetpos(xorig, yorig);
	putc('8',display);           /* write special font */
	for (i=1; i<=menusize; ++i)
	{

#ifndef FASTIO
		GRchex(i,s1,2);
		GRchex(deltax,s2,2);
		GRchex(deltay,s3,2);
		fprintf(display,"%s%s%s", s1, s2, s3);
#else
		fprintf(display,"%c%c%c",i&0377, deltax&0377, deltay&0377);
#endif
		(void) fflush(display);
		if ((i%(menusize/columns)) == 0)    /* new row/col */
		{

#ifndef FASTIO
			fputs("00",display);
#else
			putc('\00',display);
#endif

			GRsetpos(xorig+deltay, yorig+deltax);  /* reposition */
			putc('8',display);
		}  /* end if */;
	}   /* end for */

#ifndef FASTIO
	fputs("00",display);
#else
	putc('\00',display);
#endif

	GRsetwmask(linemask);
	GRVector(&bord1, &bord2, bordstyle);
} /* end displaymenu */;


char *MNFindMenuItem(x,y)
int x, y;
/*
 * returns a pointer to the text string equivalent of the selected
 * menu item.
 */

{
    int i;

    for (i=0; i<menusize; ++i)
        if (x < area[i].hix)
            if (y < area[i].hiy)
                if (x > area[i].lowx)
                    if (y > area[i].lowy)
                    {
                        last = textcommand[i];
			lastint = i;
                        return(textcommand[i]);
                    }  /* end, nested if */;

    return(last);      /* cursor out of menu area */
} /* end findmenuitem */

MNHighLt(sym,color)
int sym, color;
/*
 *      This routine highlights the specified menu item by drawing a 
 * box around it in the highlighting color.
 */

{
	POINT p1, p2, p3, p4;

	GRsetwmask(himask);
	p1.x = area[sym].lowx;
	p1.y = area[sym].lowy;
	p2.x = area[sym].lowx;
	p2.y = area[sym].hiy - 1;
	p3.x = area[sym].hix - 1;
	p3.y = area[sym].hiy - 1;
	p4.x = area[sym].hix - 1;
	p4.y = area[sym].lowy;
	GRVector(&p1, &p2, color);
	GRVector(&p2, &p3, color);
	GRVector(&p3, &p4, color);
	GRVector(&p4, &p1, color);
}  /* end MNHighLt */

MNUnHighLt(sym)
int sym;
/*
 *      This routine unhighlights the specified symbol by calling 
 * MNHighLt with the erase color specified.
 */

{
	MNHighLt(sym, eraseany);
}


MNInterpretCursor(button, cx, cy)
int cx, cy, button;
/* 
 *      This routine interprets the cursor button function and calls
 * the appropriate command interpretation routines.
 */

{
	char *cmd;
	
	PX = cx;
	PY = cy;
	if (button == 0)
	{
		cmd = MNFindMenuItem(cx, cy);
		if (*cmd == ':') LGCommand(++cmd);
		else  SHCommand(cmd);
	}
	if (button == 1) LGDeletePoint(++cmd);
	if (button == 2) LGUndo(++cmd);
	if (button == 3) 
	{
		LGPoint(++cmd);
	}
}
