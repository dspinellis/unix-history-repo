static char *sccsid = "@(#)data.c	35.5 7/1/81";

#include 	"global.h"
#include	"gtabs.h"
#include	"structs.h"
#include	"frame.h"
#include	<stdio.h>

/*char firstalloc[NBPG] = { 'x' };	/* first thing allocated in file */
lispval lispsys[SIGNIF];	/* lisp data used by system */

lispval gftab[GFTABLEN];	/* global function table for interpreter */

lispval gctab[GCTABLEN] = 	/* global constant table for interpreter */
	{nil,0,SMALL(-1),SMALL(0),SMALL(1),SMALL(2),SMALL(3),SMALL(4)};


/* Port definitions *****************************************************/
FILE	*piport,		/* standard input port		*/
	*poport,		/* standard output port		*/
	*errport,		/* port for error messages	*/
	*rdrport,		/* temporary port for readr	*/
	*proport;		/* port for protocal		*/
int	lineleng =	80;		/* line length desired		*/
int	rlevel;			/* used to indicate depth of recursion
				   in reader.  No longer really necessary */
char	keybin =	FALSE;		/* logical flag: using keyboard	*/
char	protflag =	FALSE;		/* logical flag: want protocall */
char	rbktf;				/* logical flag: ] mode		*/

lispval ioname[_NFILE];		/* strings of names of files currently open	*/

/* name stack ***********************************************************/
struct argent *orgnp;		/* used by top level to reset to start  */
struct argent		*namptr,		/* temporary pointer	*/
			*nplim;			/* don't have this = np	*/
struct nament		*bnp,			/* top of bind stack    */
			*orgbnp,		/* absolute bottom of ""*/
			*bnplim;		/* absolute top of ""   */



/* hashing things *******************************************************/
int	hash;					/* set by ratom		*/
int	atmlen;			/* length of atom including final null	*/


/* big string buffer for whomever needs it ******************************/
char	strbuf[STRBLEN];
char	*endstrb	= strbuf + STRBLEN - 1 ;

/* set by sstatus commands */
int uctolc = 0;		/* when set, uc chars in atoms go to lc */
int dmpmode = 413;	/* default mode for dumplisp 
			   (note this is decimal not octal) */

/* break and error declarations *****************************************/
int	depth =	0;		/* depth of nested breaks		*/
lispval	contval;		/* the value being returned up		*/
int	retval;			/* used by each error/prog call		*/
lispval lispretval;		/* used by non-local goto's		*/
int	rsetsw;			/* when set, trace frames built		*/
int	bcdtrsw;		/* when set with rsetsw, trace bcd too	*/
int 	evalhcallsw;		/* when set will not evalhook next eval */
int 	funhcallsw;		/* when set will not funcallhook next eval */
lispval Vevalframe;		/* pointer to lisp atom 'evalframe'	*/


/* exception handling stuff *********************************************/
int exception;			/* true if an exception is pending */
int sigintcnt;			/* number of SIGINT's pending	   */

/* current state of the hole (for fasling into) *************************/
int curhbeg;			/* next location to fasl into */
int usehole;			/* if TRUE, fasl tries to use hole */

/* other stuff **********************************************************/
lispval	ftemp,vtemp,argptr,ttemp;	/* temporaries: use briefly	*/
int itemp;
lispval sigacts[16];			/* for catching interrupts	*/
int sigstruck,sigdelay;			/* for catching interrupts	*/
lispval stattab[16];			/* miscelleneous options	*/

/*  interpreter globals    */

int lctrace;
int fvirgin = 1;		/* set to 1 initially 			*/
int GCtime;
struct frame *errp;		/* stack of error frames 		*/


/* global pointers to the transfer tables */


struct trtab *trhead=		/* first in list of transfer tables 	   */
      (struct trtab *) 0;
struct trent *trcur;		/* next entry to allocate	    	   */
int trleft = 0;			/* number of entries left in current table */

/* globals from sysat.c  */

int *beginsweep;		/* place for sweeper to begin 		*/
int initflag = TRUE;		/* inhibit gcing initially		*/
int tgcthresh = 15;

/* globals from rlc  */

int usehole;			/* TRUE if allocator should consider the 
				   hole for allocation		*/

/* global used in io.c */

lispval lastrtab;

/* globals from [VT]alloc.c  */


char purepage[TTSIZE];
int fakettsize = TTSIZE - 8;
int  *bind_lists = (int *) CNIL;	/*  lisp data for compiled code */


struct types
	atom_str =
	{
		(char *)CNIL,	0,	ATOMSPP,	ATOM,	5,
		&atom_items,	&atom_pages,	&atom_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	strng_str =
	{
		(char *) CNIL,	0,	STRSPP,		STRNG,	1,
		&str_items,	&str_pages,	&str_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	int_str =
	{
		(char *) CNIL,	0,	INTSPP,		INT,	1,
		&int_items,	&int_pages,	&int_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	dtpr_str =
	{
		(char *) CNIL,	0,	DTPRSPP,	DTPR,	2,
		&dtpr_items,	&dtpr_pages,	&dtpr_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	doub_str =
	{
		(char *) CNIL,	0,	DOUBSPP,	DOUB,	2,
		&doub_items,	&doub_pages,	&doub_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	array_str =
	{
		(char *) CNIL,	0,	ARRAYSPP,	ARRAY,	5,
		&array_items,	&array_pages,	&array_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	sdot_str =
	{
		(char *) CNIL,	0,	SDOTSPP,	SDOT,	2,
		&sdot_items,	&sdot_pages,	&sdot_name,
		(struct heads *) CNIL, (char *)CNIL
	},
	val_str =
	{
		(char *) CNIL,	0,	VALSPP,		VALUE,	1,
		&val_items,	&val_pages,	&val_name,
		(struct heads *) CNIL, (char *)CNIL
	},
funct_str =
	{
		(char *) CNIL,	0,	BCDSPP,		BCD,	2,
		&funct_items,	&funct_pages,	&funct_name,
		(struct heads *) CNIL, (char *)CNIL
	},
hunk_str[7] =
	{
		{
			(char *) CNIL,	0,	HUNK2SPP,	HUNK2,	2,
			&hunk_items[0],	&hunk_pages[0],	&hunk_name[0],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK4SPP,	HUNK4,	4,
			&hunk_items[1],	&hunk_pages[1],	&hunk_name[1],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK8SPP,	HUNK8,	8,
			&hunk_items[2],	&hunk_pages[2],	&hunk_name[2],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK16SPP,	HUNK16,	16,
			&hunk_items[3],	&hunk_pages[3],	&hunk_name[3],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK32SPP,	HUNK32,	32,
			&hunk_items[4],	&hunk_pages[4],	&hunk_name[4],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK64SPP,	HUNK64,	64,
			&hunk_items[5],	&hunk_pages[5],	&hunk_name[5],
			(struct heads *) CNIL, (char *)CNIL
		},
		{
			(char *) CNIL,	0,	HUNK128SPP,	HUNK128, 128,
			&hunk_items[6],	&hunk_pages[6],	&hunk_name[6],
			(struct heads *) CNIL, (char *)CNIL
		}
	};
extern struct readtable { unsigned char	ctable[132]; } initread;
unsigned char *ctable = initread.ctable;

int hashtop = HASHTOP;
int xcycle = 0;		/* used by xsbrk   */
struct	atom *hasht[HASHTOP];
lispval datalim;	/* pointer to next location to allocate */

char typetable[TTSIZE] = {UNBO,ATOM,PORT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT,INT};

/* this must be the last thing allocated in this file	*/
#ifdef VMS
char *lsbrkpnt = (char *)0;
char zfreespace[FREESIZE];
#else 
char lsbrkpnt,zfreespace;
#endif
