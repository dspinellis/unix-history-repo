/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Macintosh related things. K. Mitchum 2/88 */


#define NMENUS 6
#define NMENUITEMS 40	/* This has GOT to be enough! */
	
typedef data_obj *menumap[NMENUITEMS];
#ifndef _mac
	typedef char **MenuHandle;
#endif
struct menu {
	char *Name;
	int menu_id;
	MenuHandle Mn;
	menumap m;
};

struct stat {
	int st_dev;		/* volume number */
	long st_ino;		/* file number on volume */
	dev_t st_rdev;
	off_t st_size;		/* logical end of file */
	int st_mode;
	time_t st_mtime;	/* last modified */
};

#define S_IFDIR 2

typedef char *va_list;
#define va_dcl va_list va_alist;
#define va_start(l) l = (va_list)&va_alist
#define va_arg(l,m) ((m*)(l += sizeof(m)))[-1]
#define va_end(l) l = NULL

#ifdef _mac
extern struct menu Menus[NMENUS];

static	EventRecord the_Event;

/* keycodes (from Inside MacIntosh I-251). because of changes with
the MacPlus, there are some duplicate codes between cursor keys and
keypad keys. these can be deciphered by the corresponding character
codes, which are different. this table simply translates a keycode
into a character code that is appropriate. */

#define NOKEY -1
#define RET 0x0D	
#define TAB 0x09	
#define BACKSP 0x08
#define ENTERL NOKEY	/* left enter key absent on MacPlus */
#define COMMAND NOKEY	/* will be no translation anyway for these */
#define SHIFT NOKEY
#define CAPSLOCK NOKEY
#define OPTION NOKEY
#define PADDOT '.'		/* PAD period */
#define PAD0 '0'
#define PAD1 '1'
#define PAD2 '2'
#define PAD3 '3'
#define PAD4 '4'
#define PAD5 '5'
#define PAD6 '6'
#define PAD7 '7'
#define PAD8 '8'
#define PAD9 '9'
#define LEFTCURS 'B'		/* jove only, make like commands */
#define RIGHTCURS 'F'
#define UPCURS 'P'
#define DOWNCURS 'N'
#define PADENTER RET
#define PADMINUS '-'
#define CLEAR 0

static char nsh_keycodes[] = {
	'a','s','d','f','h',						/* 0 - 4 */
	'g','z','x','c','v',						/* 5 - 9 */
	NOKEY,'b','q','w','e',					/* 10 - 14 */
	'r','y','t','1','2',					/* 15 - 19 */
	'3','4','6','5','=',					/* 20 - 24 */
	'9','7','-','8','0',					/* 25 - 29 */
	']','O','u','[','i',					/* 30 - 34 */
	'p',RET,'l','j','\'',					/* 35 - 39 */
	'k',';','\\',',','/',					/* 40 - 44 */
	'n','m','.',TAB,NOKEY,					/* 45 - 49 */
	'`',BACKSP,ENTERL,NOKEY,NOKEY,			/* 50 - 54 */
	COMMAND,SHIFT,CAPSLOCK,OPTION, NOKEY,	/* 55 - 59 */
	NOKEY,NOKEY,NOKEY,NOKEY,NOKEY,			/* 60 - 64 */
	PADDOT,RIGHTCURS,NOKEY,NOKEY,NOKEY,		/* 65 - 69 */
	LEFTCURS,CLEAR,DOWNCURS,NOKEY,NOKEY,	/* 70 - 74 */
	NOKEY,PADENTER,UPCURS,PADMINUS,NOKEY,	/* 75 - 79 */
	NOKEY,NOKEY,PAD0,PAD1,PAD2,				/* 80 - 84 */
	PAD3,PAD4,PAD5,PAD6,PAD7,				/* 85 - 89 */
	NOKEY,PAD8,PAD9
};

static char sh_keycodes[] = {
	'A','S','D','F','H',						/* 0 - 4 */
	'G','Z','X','C','V',						/* 5 - 9 */
	NOKEY,'B','Q','W','E',					/* 10 - 14 */
	'R','Y','T','!','@',					/* 15 - 19 */
	'#','$','^','%','+',					/* 20 - 24 */
	'(','&','_','*',')',					/* 25 - 29 */
	'}','O','U','{','I',					/* 30 - 34 */
	'P',RET,'L','J','\'',					/* 35 - 39 */
	'K',';','|','<','?',					/* 40 - 44 */
	'N','M','>',TAB,NOKEY,					/* 45 - 49 */
	'~',BACKSP,ENTERL,NOKEY,NOKEY,			/* 50 - 54 */
	COMMAND,SHIFT,CAPSLOCK,OPTION, NOKEY,	/* 55 - 59 */
	NOKEY,NOKEY,NOKEY,NOKEY,NOKEY,			/* 60 - 64 */
	PADDOT,RIGHTCURS,NOKEY,NOKEY,NOKEY,		/* 65 - 69 */
	LEFTCURS,CLEAR,DOWNCURS,NOKEY,NOKEY,	/* 70 - 74 */
	NOKEY,PADENTER,UPCURS,PADMINUS,NOKEY,	/* 75 - 79 */
	NOKEY,NOKEY,PAD0,PAD1,PAD2,				/* 80 - 84 */
	PAD3,PAD4,PAD5,PAD6,PAD7,				/* 85 - 89 */
	NOKEY,PAD8,PAD9
};



/* tn.h Modified for variable screen size 11/21/87. K. Mitchum */

static int tn_rows, tn_cols, tn_top, tn_left, tn_bottom, tn_right;
int MAXROW, MAXCOL;

#define SCREENSIZE (tn_rows * ROWSIZE)
#define FONT monaco
#define TEXTSIZE 9

#define HEIGHT 11
#define WIDTH 6
#define DESCENT 2
#define TWIDTH tn_cols * WIDTH
#define THEIGHT tn_rows * HEIGHT

/* window specs */

#define SCROLLWIDTH 16 /* width of scroll bar control in pixels */
#define WINDWIDTH tn_right - tn_left - SCROLLWIDTH - 1/* local coordinates */
#define WINDHEIGHT tn_bottom - tn_top	/* local coordinates */

/* for keyboard routines */
#define MCHARS 32	/* must be power of two */
#define NMASK MCHARS -1	/* circular buffer */

#endif /* _mac */


