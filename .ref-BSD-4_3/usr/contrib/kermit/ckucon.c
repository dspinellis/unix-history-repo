char *connv = "Connect Command for Unix, V4C(014) 29 Jul 85";

/*  C K U C O N  --  Dumb terminal connection to remote system, for Unix  */
/*
 This module should work under all versions of Unix.  It calls externally
 defined system-dependent functions for i/o, but depends upon the existence
 of the fork() function.

 Author: Frank da Cruz (SY.FDC@CU20B),
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 

 Enhanced by H. Fischer to detect when child process (modem reader)
 reports that the communications line has been broken and hang up.
 Also enhanced to allow escaping from connect state to command
 interpreter, to allow sending/receiving without breaking connection.
*/

#include <stdio.h>
#include <ctype.h>			/* Character types */
#include "ckcdeb.h"
#include "ckcker.h"
#include <signal.h>
#include <setjmp.h>

#ifndef SIGUSR1
#define SIGUSR1 16
#endif

extern int local, speed, escape, duplex, parity, flow, seslog, mdmtyp;
extern int errno;
extern char ttname[], sesfil[];
extern CHAR dopar();

int i, active;				/* Variables global to this module */
char *chstr();
char temp[50];

#define LBUFL 200			/* Line buffer */
char lbuf[LBUFL];

/* Connect state parent/child communication signal handlers */

static jmp_buf env_con;			/* Envir ptr for connect errors */

static
conn_int() {				/* Modem read failure handler, */
    longjmp(env_con,1);			/* notifies parent process to stop */
}

/*  C O N E C T  --  Perform terminal connection  */

conect() {
    int pid, 			/* process id of child (modem reader) */
	parent_id,		/* process id of parent (keyboard reader) */
	n;
    int c;			/* c is a character, but must be signed 
				   integer to pass thru -1, which is the
				   modem disconnection signal, and is
				   different from the character 0377 */
    char errmsg[50], *erp;

	if (!local) {
	    printf("Sorry, you must 'set line' first\n");
	    return(-2);
	}
	if (speed < 0) {
	    printf("Sorry, you must 'set speed' first\n");
	    return(-2);
        }
	if ((escape < 0) || (escape > 0177)) {
	    printf("Your escape character is not ASCII - %d\n",escape);
	    return(-2);
	}
	if (ttopen(ttname,&local,mdmtyp) < 0) {
	    erp = errmsg;
	    sprintf(erp,"Sorry, can't open %s",ttname);
	    perror(errmsg);
	    return(-2);
    	}
    	printf("Connecting thru %s, speed %d.\r\n",ttname,speed);
	printf("The escape character is %s (%d).\r\n",chstr(escape),escape);
	printf("Type the escape character followed by C to get back,\r\n");
	printf("or followed by ? to see other options.\r\n");
	if (seslog) printf("(Session logged to %s.)\r\n",sesfil);

/* Condition console terminal and communication line */	    

    	if (conbin(escape) < 0) {
	    printf("Sorry, can't condition console terminal\n");
	    return(-2);
    	}
	if (ttvt(speed,flow) < 0) {
	    conres();
	    printf("Sorry, Can't condition communication line\n");
	    return(-2);
    	}

/* cont'd... */

/* ...connect, cont'd */


	parent_id = getpid();		/* get parent id for signalling */
	pid = fork();			/* All ok, make a fork */
	if (pid) {			
	  active = 1;			/* This fork reads, sends keystrokes */
	  if (!setjmp(env_con)) {	/* comm error in child process */
	    signal(SIGUSR1,conn_int);	/* routine for child process exit */
	    while (active) {
		c = coninc(0) & 0177;	/* Get character from keyboard */
		if (c == escape) {   	/* Look for escape char */
		    c = coninc(0) & 0177;   /* Got esc, get its arg */
		    doesc(c);		    /* And process it */
		} else {		/* Ordinary character */
		    if (ttoc(dopar(c)) > -1) {
		    	if (duplex) {	/* Half duplex? */
			    conoc(c);	/* Yes, also echo it. */
			    if (seslog) 	/* And maybe log it. */
			    	if (zchout(ZSFILE,c) < 0) seslog = 0;
			}
    	    	    } else {
			perror("\r\nCan't send character");
			active = 0;
		    }
		}
	      }
    	    }				/* Come here on death of child */
	    kill(pid,9);		/* Done, kill inferior fork. */
	    wait(0);			/* Wait till gone. */
	    conres();			/* Reset the console. */
	    printf("[Back at Local System]\n");
	    return(0);

	} else {			/* Inferior reads, prints port input */

	    while (1) {			/* Fresh read, wait for a character */
		if ((c = ttinc(0)) < 0) { /* Comm line hangup detected */
		    if (errno == 9999)	/* this value set by ckutio.c myread */
			 printf("\r\nCommunications disconnect ");
		    else perror("\r\nCan't get character");
		    kill(parent_id,SIGUSR1);	/* notify parent. */
		    pause();		/* Wait to be killed by parent. */
                }
		c &= 0177;		/* Got a char, strip parity, etc */
		conoc(c);		/* Put it on the screen. */
		if (seslog) zchout(ZSFILE,c);	/* If logging, log it. */
		while ((n = ttchk()) > 0) {	/* Any more left in buffer? */
		    if (n > LBUFL) n = LBUFL;   /* Get them all at once. */
		    if ((n = ttxin(n,lbuf)) > 0) {
			for (i = 0; i < n; i++) lbuf[i] &= 0177;   /* Strip */
			conxo(n,lbuf);	    	    	    	   /* Output */
			if (seslog) zsoutx(ZSFILE,lbuf,n);  	   /* Log */
		    }
	    	}
	    }
    	}
}

/*  H C O N N E  --  Give help message for connect.  */

hconne() {
    int c;
    static char *hlpmsg[] = {"\
\r\nC to close the connection, or:",
"\r\n  0 (zero) to send a null",
"\r\n  B to send a BREAK",
"\r\n  H to hangup and close connection",
"\r\n  S for status",
"\r\n  ? for help",
"\r\n escape character twice to send the escape character.\r\n\r\n",
"" };

    conola(hlpmsg);			/* Print the help message. */
    conol("Command>");			/* Prompt for command. */
    c = coninc(0);
    conoc(c);				/* Echo it. */
    conoll("");
    c &= 0177;				/* Strip any parity. */
    return(c);				/* Return it. */
}


/*  C H S T R  --  Make a printable string out of a character  */

char *
chstr(c) int c; {
    static char s[8];
    char *cp = s;

    if (c < SP) {
	sprintf(cp,"CTRL-%c",ctl(c));
    } else sprintf(cp,"'%c'\n",c);
    cp = s;
    return(cp);
}

/*  D O E S C  --  Process an escape character argument  */

doesc(c) char c; {
    CHAR d;
  
    c &= 0177;
    while (1) {
	if (c == escape) {		/* Send escape character */
	    d = dopar(c); ttoc(d); return;
    	} else				/* Or else look it up below. */
	    if (isupper(c)) c = tolower(c);

	switch (c) {

	case 'c':			/* Close connection */
	case '\03':
	    active = 0; conol("\r\n"); return;

	case 'b':			/* Send a BREAK signal */
	case '\02':
	    ttsndb(); return;

	case 'h':			/* Hangup */
	case '\010':
	    tthang(); active = 0; conol("\r\n"); return;

	case 's':			/* Status */
	case '\023':
	    conol("\r\nConnected thru ");
	    conol(ttname);
	    if (speed >= 0) {
		sprintf(temp,", speed %d",speed); conol(temp);
	    }
	    if (parity) {
		conol(", ");
		switch (parity) {
		    case 'e': conol("even");  break;
		    case 'o': conol("odd");   break;
		    case 's': conol("space"); break;
		    case 'm': conol("mark");  break;
		}
		conol(" parity");
	    }
	    if (seslog) {
		conol(", logging to "); conol(sesfil);
            }
	    conoll(""); return;

	case '?':			/* Help */
	    c = hconne(); continue;

	case '0':			/* Send a null */
	    c = '\0'; d = dopar(c); ttoc(d); return;

	case SP:			/* Space, ignore */
	    return;

	default:			/* Other */
	    conoc(BEL); return; 	/* Invalid esc arg, beep */
    	}	    
    }
}    
