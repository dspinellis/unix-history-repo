#include <stdio.h>

#include 	"global.h"
#include	"gtabs.h"

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


/* name stack ***********************************************************/
struct	argent		*namptr,		/* temporary pointer	*/
			*nplim;			/* don't have this = np	*/
struct nament		*bnp,			/* top of bind stack    */
			*orgbnp,		/* absolute bottom of ""*/
			*bnplim;		/* absolute top of ""   */


/* the typeing table ****************************************************/
#ifndef ROWAN
char typetab[TTSIZE] = {UNBO,ATOM,INT,INT,PORT};
#else
char typetab[TTSIZE] = {UNBO,ATOM,INT,INT,INT,PORT};
#endif

/* hashing things *******************************************************/
struct	atom	*hasht[HASHTOP];
int	hash;					/* set by ratom		*/
int	atmlen;			/* length of atom including final null	*/


/* big string buffer for whomever needs it ******************************/
char	strbuf[STRBLEN];
char	*endstrb	= strbuf + 255;

/* set by sstatus commands */
int uctolc = 0;		/* when set, uc chars in atoms go to lc */
int dmpmode = 413;	/* default mode for dumplisp 
			   (note this is decimal not octal) */

/* break and error declarations *****************************************/
int	depth =	0;		/* depth of nested breaks		*/
lispval	contval;		/* the value being returned up		*/
struct argent *orgnp;		/* used by top level to reset to start  */
int	retval;			/* used by each error/prog call		*/


/* other stuff **********************************************************/
lispval	ftemp,vtemp,argptr,ttemp;	/* temporaries: use briefly	*/
int itemp;
lispval sigacts[16];			/* for catching interrupts	*/
int sigstruck,sigdelay;			/* for catching interrupts	*/
lispval stattab[16];			/* miscelleneous options	*/

/*  interpreter globals    */

int lctrace;
int fvirgin;
int GCtime;
int errp;			/* where are lying through our teeth. This
				   is a pointer to inside a function. */
