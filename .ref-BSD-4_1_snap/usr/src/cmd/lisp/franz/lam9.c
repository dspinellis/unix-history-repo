
#include "global.h"
/*
/*			Program name	:  Init.c
/*			Anthor		:  Morris Djavaheri
/*			Date		:  April 8,82
/*			Version		:  1.2c
/*
/* These routines writen in C will allow use of the termcap file
/* by any lisp program. They are very basic routines which initialize
/* termcap and allow the lisp to execute any of the termcap functions.
/* 									*/

#include <stdio.h>		/*add definations for I/O and bandrate */
#include <sgtty.h>

#undef putchar
int	putchar();		/* functions used from the termlib */
int	tgetflag();
char	*getenv();
char	*tgoto();
char	*tgetstr();

char 	bpbuf[1024];
char 	tstrbuf[100];
short 	ospeed;
char	PC;
char   *BC;
char   *UP;

/*
/*	This routine will initialize the termcap for the lisp programs.
/*	If the termcap file is not found, or terminal type is undefined,
/*	it will print out an error mesg.				*/

lispval
Ltci()
{
char *bp = bpbuf;
char *cp = getenv("TERM");
char *pc;
int found;
struct sgttyb tty;

found = tgetent(bpbuf,cp);		/* open ther termcap file */
switch(found) {
      case -1: 	printf("\nError Termcap File not found \n");break;
      case 0 :	printf("\nError No Termcap Entry for this terminal \n");
		break;
      case 1 : {			/* everything was ok	*/
		gtty(1, &tty);
		ospeed = tty.sg_ospeed;
	        }
		break;
	}
cp = tstrbuf;
BC = tgetstr("bc", &cp);
UP = tgetstr("up", &cp);
pc = tgetstr("pc", &cp);
if (pc)
    PC = *pc;
return(nil);
}
/* This routine will execute any of the termcap functions used by the lisp
/* program. If the feature is not include in the terminal defined it will
/* ignore the call.
/*		option	: feature to execute
/*		line	: line if is nessery
/*		colum	: colum if is nessaery
/*									*/
lispval
Ltcx()
{
	register struct argent *mylbot = lbot;
	char *option; int line, column;

	switch(np-lbot) {
	case 1:
		line = column = 0;
		break;
	case 2:
		error("Wrong number of Arguments to Termcapexecute",FALSE);
		break;
	case 3:
		line = mylbot[1].val->i;
		column = mylbot[2].val->i;
	}
	return(inewint(show(mylbot->val,&line,&column)));
}


static show(option,line,colum)
char *option;
int  *line,*colum;
{
int found;
char clbuf[20];
char *clbp = clbuf;
char *clear;
char *cm;
/* the tegetflag doesnot work ? */
clear = tgetstr(option,&clbp);  
/*printf("option = %d , %s \n",clear,option);*/
if (!clear) 
	{found = tgetnum(option);
	 if (found)
		return(found);
	  return(-1);
       }
PC = ' ';
if (strcmp(option, "cm") == 0) {		/* if cursor motion, do it */
	clear=tgoto(clear,*colum,*line);
       	if (*clear == 'O')
		clear = 0;
	}
if (clear)					/* execute the feature */
     tputs(clear,0,putchar);
return (0);
}

