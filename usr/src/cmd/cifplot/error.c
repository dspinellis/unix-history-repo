/*******************************************************************
*                                                                  *
*    File: CIFPLOT/error.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/


#include <stdio.h>
#include <signal.h>
#include "defs.h"
#include "globals.h"
#include "scanner.h"

IMPORT string Concat();
IMPORT alloc();

/* The struct 'ErrorType' is used to create a linked listed which
 * contains error messages. The error messages are held till the
 * entire line has been read, then are printed out with an arrow
 * marking the spot where the error occured. */
struct ErrorType {
	struct ErrorType *ELink;
	int LineNo,CharNo;
	string EString;
	} *ErrorList,*ErrorEnd;

int FatalError;

yyerror()
/* YACC wants this */
{
   return(0);
   }

Error(s,type)
char *s;
int type;
/* 'Error' is the general error reporting facility */
{
    char buf[128];
    switch(type) {
	case INTERNAL: 
		fprintf(stderr,"Internal Error: %s\n",s);
		abort();
	case RUNTIME:
		fprintf(stderr,"Run Time Error: %s\n",s);
		abort();
	case WARNING:
		sprintf(buf,"warning: %s",s);
		DispError(TokenLine,TokenChar,buf);
		break;
	case RECOVERABLE:
		sprintf(buf,"error: %s",s);
		DispError(TokenLine,TokenChar,buf);
		break;
	case FATAL:
		FatalError = 1;
		sprintf(buf,"Error: %s",s);
		DispError(TokenLine,TokenChar,buf);
		break;
	default:
		fprintf(stderr,"Unknown Error Type: %s\n",s);
		abort();
	}
    }

DispError(ln,cn,s)
int ln,cn;
string s;
/* 'DispError' places an error message,'s', on the error list */
{
    struct ErrorType *p;

    p = (struct ErrorType *) alloc(sizeof(struct ErrorType));
    p->LineNo = ln;
    p->CharNo = cn;
    p->EString = Concat(s,0);
    if (ErrorList == NIL){
	 ErrorEnd = ErrorList = p;
	 }
      else
	 ErrorEnd->ELink = p;
    ErrorEnd = p;
    p->ELink = NIL;
    PrintError();
    }


PrintError()
/* 'PrintError' prints the error messages on 'ErrorList' if the
 * line which the error occured on has been completely read in. */
{
    int i;
    while(ErrorList != NIL) {
	PrintLine(ErrorList->LineNo);
	if (ErrorList->LineNo <= maxlines) {
	    if (ErrorList->LineNo < maxlines) {
		/* The line which the error occured on has been passed.
		 * Print the line number and the message	*/
		fprintf(stderr,"Line %d  %s\n",ErrorList->LineNo,ErrorList->EString);
		}
	     else {
		/* A negitive CharNo indicates the error was on
		 * the previous line	*/
		if (ErrorList->CharNo < 0)
		       ErrorList->CharNo = OldLength-ErrorList->CharNo-2;
		/* Print the line number */
		fprintf(stderr,"%6d----",ErrorList->LineNo);
		/* Step out the proper number of places and put an up-
		 * arrow under the offender.	*/
		for(i=1; i<ErrorList->CharNo;i++) fprintf(stderr,"-");
		fprintf(stderr,"^----");
		for(i=ErrorList->CharNo; i<10;i++) fprintf(stderr,"-");
		/* Print the error message */
		fprintf(stderr,"%s\n",ErrorList->EString);
		Free(ErrorList->EString);
		}
	    ErrorList = ErrorList->ELink;
	    }
	 else
	   /* If we have not reached the offending line just return */
	   return;
      }
    }

ErrorSummary()
{
    PrintError();
    while(ErrorList != NIL) {
	fprintf(stderr,"Line %d  %s\n",ErrorList->LineNo,ErrorList->EString);
	ErrorList = ErrorList->ELink;
	}
    if(FatalError) fprintf(stderr,"Plotting Supressed\n");
    return;
    } 
extern Abort();		/* Forward reference Abort	*/
extern Trap();		/* Forward reference Trap	*/

InitError()
{
    int i;

    ErrorList = NIL;
    ErrorEnd = NIL;
    FatalError = 0;

    if(signal(SIGINT,SIG_IGN) == SIG_IGN) {
    	signal(SIGHUP,SIG_IGN);
    	signal(SIGQUIT,SIG_IGN);
    	signal(SIGINT,SIG_IGN);
	background = 1;
	}
      else {
    	signal(SIGHUP,Trap);
    	signal(SIGQUIT,Trap);
    	signal(SIGINT,Trap);
	background = 0;
	}
    /* Catch traps */
    for(i=4;i<16;i++)
	signal(i,Trap);
    }

/*
background()
{
    signal(SIGHUP,SIG_IGN);
    signal(SIGQUIT,SIG_IGN);
    signal(SIGINT,SIG_IGN);
    }
    */

Trap(n)
int n;
{
    switch(n) {
	case SIGHUP:
		fprintf(stderr,"hangup - ");
		break;
	case SIGINT:
		fprintf(stderr,"interrupt - ");
		break;
	case SIGQUIT:
		fprintf(stderr,"quit - ");
		break;
	case SIGILL:
		break;
	case SIGTRAP:
		fprintf(stderr,"trap - ");
		break;
	case SIGIOT:
		fprintf(stderr,"IOT instruction - ");
		break;
	case SIGEMT:
		fprintf(stderr,"EMT instruction - ");
		break;
	case SIGFPE:
		fprintf(stderr,"floating point exception - ");
		break;
	case SIGBUS:
		fprintf(stderr,"bus error - ");
		break;
	case SIGSEGV:
		fprintf(stderr,"segmentation violation - ");
		break;
	case SIGSYS:
		fprintf(stderr,"bad argument to system call - ");
		break;
	case SIGPIPE:
		fprintf(stderr,"write on a pipe with no one to read it - ");
		break;
	case SIGALRM:
		fprintf(stderr,"alarm clock - ");
		break;
	case SIGTERM:
		fprintf(stderr,"cifplot killed - ");
		break;
	default:
		fprintf(stderr,"unknown error - ");
		break;
	}
    Abort();
    }

Abort()
/* Abort does a semi-graceful closing of the program */	
{
    fprintf(stderr,"abort\n");
    signal(SIGILL,SIG_DFL);
    fflush(stdout);
    /*
    Summary();
    */
    fflush(stdout);
    if(fileopen) {
	DumpBuf(-INFINITY);
	vclose();
	}
    unlock();
    if(debug) {
	AllocSummary();
	abort();
	}
    exit(2);
    }
