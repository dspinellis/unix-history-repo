/*
 * Define global variables
 *
 * 3/5/81 (Berkeley) @(#)curses.c	1.2
 */
# include	"curses.h"

bool	_echoit		= TRUE,	/* set if stty indicates ECHO		*/
	_rawmode	= FALSE,/* set if stty indicates RAW mode	*/
	My_term		= FALSE,/* set if user specifies terminal type	*/
	_endwin		= FALSE;/* set if endwin has been called	*/

char	ttytype[10],		/* long name of tty			*/
	*Def_term	= "unknown";	/* default terminal type	*/

int	_tty_ch		= 1,	/* file channel which is a tty		*/
	LINES,			/* number of lines allowed on screen	*/
	COLS,			/* number of columns allowed on screen	*/
	_res_flg;		/* sgtty flags for reseting later	*/

WINDOW	*stdscr		= NULL,
	*curscr		= NULL;

# ifdef DEBUG
FILE	*outf;			/* debug output file			*/
# endif

SGTTY	_tty;			/* tty modes				*/

bool	AM, BS, CA, DA, DB, EO, GT, HZ, IN, MI, MS, NC, OS, UL, XN,
	NONL, UPPERCASE, normtty, _pfast;

char	*AL, *BC, *BT, *CD, *CE, *CL, *CM, *DC, *DL, *DM, *DO, *ED,
	*EI, *HO, *IC, *IM, *IP, *LL, *MA, *ND, *SE, *SF, *SO, *SR,
	*TA, *TE, *TI, *UC, *UE, *UP, *US, *VB, *VE, *VS, PC;
