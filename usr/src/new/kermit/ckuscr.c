char *loginv = "Script Command, V2.0(007) 5 Jul 85";

/*  C K U S C R  --  Login script for logging onto remote system */

/*
 This module should work under all versions of Unix.  It calls externally
 defined system-depended functions for i/o.

 The module expects a login string of the expect send [expect send] ...
 format.  It is intended to operate similarly to the way the common
 uucp "L.sys" login entries work.  Conditional responses are supported
 expect[-send-expect[...]] as with uucp.  The send keyword EOT sends a
 control-d, and the keyword BREAK sends a break.  Letters prefixed
 by '~' are '~b' backspace, '~s' space, '~n' linefeed, '~r' return, '~x' xon,
 '~t' tab, '~q' ? (not allowed on kermit command lines), '~' ~, '~'', 
 '~"', '~c' don't append return, '~o[o[o]]' octal character.  As with
 some uucp systems, sent strings are followed by ~r (not ~n) unless they
 end with ~c. Null expect strings (e.g., ~0 or --) cause a short
 delay, and are useful for sending sequences requiring slight pauses.

 Author: Herm Fischer (HFISCHER@USC-ECLB)
 Contributed to Columbia University for inclusion in C-Kermit.
 Copyright (C) 1985, Herman Fischer, 16400 Ventura Blvd, Encino CA 91436
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 
*/

#include "ckcdeb.h"
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include "ckcker.h"

extern int local, speed, flow, seslog, mdmtyp;
extern char ttname[];
extern CHAR dopar();
static char * chstr();

static int EXP_ALRM = 15;		/* Time to wait for expect string */
#define SND_ALRM	15		/* Time to allow for sending string */
#define NULL_EXP	2		/* Time to pause on null expect strg*/ 
#define DEL_MSEC	300		/* milliseconds to pause on ~d */

#define SBUFL 300			/* Login Sequence buffer */
static char seq_buf[SBUFL], *s;
static int got_it, no_cr;

/*  connect state parent/child communication signal handlers */

static jmp_buf alrmRng;		/* Envir ptr for connect errors */

scrtime() {				/* modem read failure handler, */
    longjmp(alrmRng,1);		/* notifies parent process to stop */
}


/*
 Sequence interpreter -- pick up next sequence from command string,
 decode escapes and place into seq_buf

 If string contains a ~d (delay) then sequenc returns a 1 expecting
 to be called again after the ~d executes.
*/
static 
sequenc()  {

    int i;
    char c, oct_char;

    no_cr = 0;				/* output needs cr appended */

    for (i=0; i<SBUFL; ) {		
	if (*s == '\0' || *s == '-' || isspace(*s) ) { /* done */
	    seq_buf[i] = '\0';
	    return(0) ;
	}

	if (*s == '~') {		/* escape character */
	    switch (c = *(++s) ) {
		case 'n':	seq_buf[i++] = '\n'; break;
		case 'r':	seq_buf[i++] = '\r'; break;
		case 't':	seq_buf[i++] = '\t'; break;
		case 'b':	seq_buf[i++] = '\b'; break;
		case 'q':	seq_buf[i++] = '?';  break;
		case '~':	seq_buf[i++] = '~';  break;
		case '\'':	seq_buf[i++] = '\''; break;
		case '\"':	seq_buf[i++] = '\"'; break;
		case 's':	seq_buf[i++] = ' ';  break;
		case 'x':	seq_buf[i++] = '\021'; break;
		case 'c':	no_cr = 1; break;
		case 'd': {			/* send what we have and then */
		    seq_buf[i] = '\0';		/* expect to send rest after */
		    no_cr = 1;			/* sender delays a little */
		    s++;
		    return(1);
		    }
		case 'w': {			/* wait count */
		    EXP_ALRM = 15;		/* default to 15 sec */
		    if ( isdigit( *(s+1) ) ) { 
			EXP_ALRM = (*(++s)) & 15;
			if ( isdigit( *(s+1) ) ) {
			    EXP_ALRM = EXP_ALRM*10 + ( (*(++s)) & 15 );
			    }
			}
		    break;
		    }
		default:
		    if ( isdigit(c) ) {	    	/* octal character */
		    	oct_char = (c & 7);	/* most significant digit */
			if (isdigit( *(s+1) ) ) {
			    oct_char = (oct_char<<3) | ( (*(++s)) & 7 ) ;
			    if (isdigit( *(s+1) ) ) {
			    	oct_char = (oct_char<<3) | ( (*(++s)) & 7 ) ;
			    }
			}
			seq_buf[i++] = oct_char;
			break;
		    }
	    }
	}
	else seq_buf[i++] = *s;		/* plain old character */
	s++;
    }
    seq_buf[i] = '\0';
    return(0);			/* end of space, return anyway */
}


/*
 Receive sequence -- see if expected response comes return success
 (or failure) in got_it
*/ 
static 
recvSeq()  {
   
    char *e, got[7], trace[300];
    int i, l;
    
	sequenc();
	l = strlen(e=seq_buf);		/* no more than 7 chars allowed */
	if (l > 7) {
	    e += l-7;
	    l = 7;
	}

	tlog(F111,"expecting sequence",e,(long) l);
	if (l == 0) {		/* null sequence, just delay a little */
	    sleep (NULL_EXP);
	    got_it = 1;
	    tlog(F100,"got it (null sequence)","",0l);
	    return;
	}
	*trace = '\0';
	for (i=0; i<7; i++) got[i]='\0';

	signal(SIGALRM,scrtime);	/* did we get it? */
	if (!setjmp(alrmRng)) {	/* not timed out yet */
	    alarm(EXP_ALRM);
	    while (!got_it) {
		for (i=0; i<(l-1); i++) got[i] = got[i+1]; /* shift over one */
		got[l-1] = ttinc(0) & 0177;		/* next char */
		if (strlen(trace) < sizeof(trace)-2 ) 
			strcat(trace,chstr(got[l-1]));
		got_it = (!strncmp(seq_buf, got, l) ) ;
	    }
	} else got_it = 0;		/* timed out here */

	alarm(0);
	signal(SIGALRM,SIG_IGN);
	tlog(F110,"received sequence: ",trace,0l);
	tlog(F101,"returning with got-it code","",(long) got_it);
	return;
}


/*
 Output A Sequence starting at pointer s,
 return 0 if okay,
 1 if failed to read (modem hangup or whatever)
*/
static int
outSeq()  {
    char *sb;
    int l;
    int delay;
    int retCode = 0;

  while(1) {
    delay = sequenc();  
    l = strlen(seq_buf);
    tlog(F111,"sending sequence ",seq_buf,(long) l);
    signal(SIGALRM,scrtime);
    if (!setjmp(alrmRng)) {
	alarm(SND_ALRM);
	if (!strcmp(seq_buf,"EOT")) ttoc(dopar('\004'));
	else if (!strcmp(seq_buf,"BREAK")) ttsndb();
 	else {
	    if (l > 0) {
		for ( sb=seq_buf; *sb; sb++) *sb = dopar(*sb);
		ttol(seq_buf,l);		/* with parity */
	    }
	    if (!no_cr) ttoc( dopar('\r') );
	}
    }
        else retCode |= -1;    		/* else -- alarm rang */
    alarm(0);
    signal(SIGALRM,SIG_IGN);
    if (!delay) return ( retCode );
    msleep(DEL_MSEC);		/* delay, and loop to next stuff to send */
    }
}


/*  L O G I N  --  Login to remote system */

login(cmdstr) char *cmdstr; {

	int (*saveAlm)();	/* save incomming alarm function */
	char *e;

	s = cmdstr;			/* make global to ckuscr.c */

	tlog(F100,loginv,"",0l);

	if (!local) {
	    printf("Sorry, you must 'set line' first\n");
	    return(-2);
	}
	if (speed < 0) {
	    printf("Sorry, you must 'set speed' first\n");
	    return(-2);
        }
	if (ttopen(ttname,&local,mdmtyp) < 0) {
	    sprintf(seq_buf,"Sorry, can't open %s",ttname);
	    perror(seq_buf);
	    return(-2);
    	}
    	printf("Executing script thru %s, speed %d.\r\n",ttname,speed);
	*seq_buf=0;
	for (e=s; *e; e++) strcat(seq_buf, chstr(*e) );
	printf("The logon string is: %s\r\n",seq_buf);
	tlog(F110,"Logon command string: ",seq_buf, 0l);

/* Condition console terminal and communication line */	    

	if (ttvt(speed,flow) < 0) {
	    printf("Sorry, Can't condition communication line\n");
	    return(-2);
    	}
				/* save initial timer interrupt value */
	saveAlm = signal(SIGALRM,SIG_IGN);

	ttflui();		/* flush stale input */

/* cont'd... */


/* ...login, cont'd */

/* start expect - send sequence */

    while (*s) {		/* while not done with buffer */

	while (*s && isspace(*s)) s++;	/* skip over separating whitespaces */
				/* gather up expect sequence */
	got_it = 0;
	recvSeq();

	while (!got_it) {
				/* no, is there a conditional send */
	    if (*s++ != '-') goto failRet;    	/* no -- return failure */
		
	    		/* start of conditional send */
	    ttflui();				/* flush out input buffer */
	    if (outSeq()) goto failRet; 	/* if unable to send! */

	    if (*s++ != '-') goto failRet; 	/* must have condit respon.*/
	    recvSeq();
	}	/* loop back and check got_it */

	while (*s && !isspace(*s++) ) ; 	/* skip over conditionals */
	while (*s && isspace(*s)) s++;	/* skip over separating whitespaces */
	ttflui();			/* Flush */
	if (*s) if (outSeq()) goto failRet; 	/* if any */
    }
    signal(SIGALRM,saveAlm);
    printf("Logged on!\r\n");
    tlog(F100,"Logged on!","",0l);
    return(0);

failRet:
    signal(SIGALRM,saveAlm);
    printf("Sorry, logon failed\r\n");
    tlog(F100,"Logon failed","",0l);
    return(-2);
}


/*  C H S T R  --  Make printable string from a character */

static char *
chstr(c) char c; {
    static char sc[4];

    if (c < SP) sprintf(sc, "^%c",ctl(c) );
    else sprintf(sc, "%c", c);
  
    return(sc);
}
