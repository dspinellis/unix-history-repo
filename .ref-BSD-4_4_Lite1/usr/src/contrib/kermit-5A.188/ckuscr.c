#ifndef NOICP
#ifndef NOSCRIPT
char *loginv = "Script Command, 5A(015) 2 Nov 92";

/*  C K U S C R  --  Login script for logging onto remote system */

/*
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.

  Original (version 1, 1985) author: Herm Fischer, Encino, CA.
  Contributed to Columbia University in 1985 for inclusion in C-Kermit 4.0.
  Author and maintainer since 1985: Frank da Cruz, Columbia University,
  fdc@columbia.edu.
*/

/*
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

  This module calls externally defined system-dependent functions for
  communications i/o, as defined in CKCPLM.DOC, the C-Kermit Program Logic
  Manual, and thus should be portable to all systems that implement those
  functions, and where alarm() and signal() work as they do in UNIX.
*/

#include "ckcdeb.h"
#include <signal.h>
#include <setjmp.h>
#include "ckcasc.h"
#include "ckcker.h"
#include "ckuusr.h"
#include "ckcnet.h"

_PROTOTYP( VOID flushi, (void) );

#ifdef MAC
#define SIGALRM (1<<10)
#undef SIGTYP				/* put in ckcdeb.h later */
#define SIGTYP void
#endif /* MAC */

extern int local, flow, seslog, mdmtyp, msgflg, duplex, backgrd, secho, quiet;
#ifdef NETCONN
extern int network, ttnproto;
#endif /* NETCONN */
extern long speed;
extern char ttname[];

#ifndef NOSPL
extern struct cmdptr cmdstk[];
extern int techo, cmdlvl;
extern int mecho;
#endif /* NOSPL */

static int scr_echo;			/* Whether to echo script commands */

static int exp_alrm = 15;		/* Time to wait for expect string */
#define SND_ALRM 15			/* Time to allow for sending string */
#define NULL_EXP 2			/* Time to pause on null expect strg*/ 
#define DEL_MSEC 300			/* milliseconds to pause on ~d */

#define SBUFL 512		
static char seq_buf[SBUFL], *s;		/* Login Sequence buffer */
static char fls_buf[SBUFL];		/* Flush buffer */
static int got_it, no_cr;

/*  connect state parent/child communication signal handlers */

static jmp_buf alrmrng;			/* Envir ptr for connect errors */

SIGTYP
scrtime(foo) int foo; {			/* modem read failure handler, */
    longjmp(alrmrng,1);			/* notifies parent process to stop */
}

/*
 Sequence interpreter -- pick up next sequence from command string,
 decode escapes and place into seq_buf.

 If string contains a ~d (delay) then sequenc returns a 1 expecting
 to be called again after the ~d executes.
*/
static int
sequenc() {
    int i;
    char c, oct_char;

    no_cr = 0;				/* output needs cr appended */
    for (i = 0; i < SBUFL; ) {		
	if (*s == '\0' || *s == '-' || isspace(*s) ) { /* done */
	    seq_buf[i] = '\0';
	    return(0) ;
	}
	if (*s == '~') {		/* escape character */
	    s++;
	    switch (c = *s) {
		case 'n':  seq_buf[i++] = LF; break;
		case 'r':  seq_buf[i++] = CR; break;
		case 't':  seq_buf[i++] = '\t'; break;
		case 'b':  seq_buf[i++] = '\b'; break;
		case 'q':  seq_buf[i++] = '?';  break;
#ifdef COMMENT
/* The default case should catch these now... */
		case '~':  seq_buf[i++] = '~';  break;
		case '-':  seq_buf[i++] = '-';  break;
#endif /* COMMENT */
		case '\'': seq_buf[i++] = '\''; break;
		case '\"': seq_buf[i++] = '\"'; break;
		case 's':  seq_buf[i++] = ' ';  break;
		case 'x':  seq_buf[i++] = '\021'; break;
		case 'c':  no_cr = 1; break;
		case 'd': {			/* send what we have & then */
		    seq_buf[i] = '\0';		/* expect to send rest after */
		    no_cr = 1;			/* sender delays a little */
		    s++;
		    return(1);
		}
		case 'w': {			/* wait count */
		    exp_alrm = 15;		/* default to 15 sec */
		    if (isdigit(*(s+1))) { 
			s++;
			exp_alrm = *s & 15;
			if (isdigit(*(s+1)) ) {
			    s++;
			    exp_alrm = exp_alrm * 10 + (*s & 15);
			}
		    }
		    break;
		}
		default:
		    if ( isdigit(c) ) {	    	/* octal character */
		    	oct_char = (c & 7);	/* most significant digit */
			if (isdigit( *(s+1) ) ) {
			    s++;
			    oct_char = (oct_char<<3) | ( *s & 7 ) ;
			    if (isdigit( *(s+1) ) ) {
				s++;
			    	oct_char = (oct_char<<3) | ( *s & 7 ) ;
			    }
			}
			seq_buf[i++] = oct_char;
			break;
		    } else seq_buf[i++] = *s; /* Treat ~ as quote */
	      }
	} else seq_buf[i++] = *s;	/* Plain old character */
	s++;
    }
    seq_buf[i] = '\0';
    return(0);				/* end of space, return anyway */
}

/*
 Receive sequence -- see if expected response comes,
 return success (or failure) in got_it.
*/ 
static VOID
recvseq()  {
   
    char *e, got[7], trace[SBUFL];
    int i, l, x;
    
	sequenc();
	l = (int)strlen(e=seq_buf);	/* no more than 7 chars allowed */
	if (l > 7) {
	    e += l-7;
	    l = 7;
	}
	tlog(F111,"expecting sequence",e,(long) l);
	if (l == 0) {		/* null sequence, just delay a little */
	    sleep (NULL_EXP);
	    got_it = 1;
	    tlog(F100,"got it (null sequence)","",0L);
	    return;
	}
	*trace = '\0';
	for (i = 0; i < 7; i++) got[i]='\0';

	signal(SIGALRM,scrtime);	/* did we get it? */
	if (!setjmp(alrmrng)) {		/* not timed out yet */
	    alarm(exp_alrm);
	    while (!got_it) {
		for (i = 0; i < l-1; i++) got[i] = got[i+1]; /* Shift over */
		x = ttinc(0);		/* Read a character */
		debug(F101,"recvseq","",x);
		if (x < 0) goto rcvx;	/* Check for error */
#ifdef NETCONN
#ifdef TNCODE
/* Check for telnet protocol negotiation */
		if (network &&
		    (ttnproto == NP_TELNET) &&
		    ( (x & 0xff) == IAC) ) {
		    switch (tn_doop((CHAR)(x & 0xff),duplex,ttinc)) {
		      case 2: duplex = 0; continue;
		      case 1: duplex = 1;
		      default: continue;
		    }
		}
#endif /* TNCODE */
#endif /* NETCONN */
		got[l-1] = x & 0x7f;	/* Got a character */
		if (scr_echo) conoc(got[l-1]); /* Echo it */
		if (seslog)		/* Log it in session log */
		  if (zchout(ZSFILE,got[l-1]) < 0) seslog = 0;
		if ((int)strlen(trace) < sizeof(trace)-2 ) 
		  strcat(trace,dbchr(got[l-1]));
		got_it = (!strncmp(e, got, l));
	    }
	} else got_it = 0;		/* timed out here */
rcvx:
	alarm(0);
	signal(SIGALRM,SIG_IGN);
	tlog(F110,"received sequence: ",trace,0L);
	tlog(F101,"returning with got-it code","",(long) got_it);
	return;
}

/*
 Output A Sequence starting at pointer s,
 return 0 if okay,
 1 if failed to read (modem hangup or whatever)
*/
static int oseqret = 0;			/* Return code for outseq */
					/* Out here to prevent clobbering */
					/* by longjmp. */
static int
outseq() {
    char *sb;
    int l;
    int delay;

    oseqret = 0;			/* Initialize return code */
    while(1) {
	delay = sequenc();  
	l = (int)strlen(seq_buf);
	tlog(F111,"sending sequence ",seq_buf,(long) l);
	signal(SIGALRM,scrtime);
	if (!setjmp(alrmrng)) {
	    alarm(SND_ALRM);
	    if (!strcmp(seq_buf,"EOT")) {
		ttoc(dopar('\004'));
		if (scr_echo) conol("<EOT>");
		if (seslog && duplex) if (zsout(ZSFILE,"<EOT>") < 0)
		  seslog = 0;
	    } else if (!strcmp(seq_buf,"BREAK") ||
		       !strcmp(seq_buf,"\\b") ||
		       !strcmp(seq_buf,"\\B")) {
		ttsndb();
		if (scr_echo) conol("<BREAK>");
		if (seslog) if (zsout(ZSFILE,"{BREAK}") < 0) seslog = 0;
	    } else {
		if (l > 0) {
		    for ( sb = seq_buf; *sb; sb++)
		      *sb = dopar(*sb);	/* add parity */
		    ttol((CHAR *)seq_buf,l); /* send it */
		    if (scr_echo && duplex) conxo(l,seq_buf);
		    if (seslog && duplex) /* log it */
		      if (zsout(ZSFILE,seq_buf) < 0)
			seslog=0;
		}
		if (!no_cr) {
		    ttoc( dopar(CR) );
		    if (seslog && duplex)
		      if (zchout(ZSFILE,dopar(CR)) < 0)
			seslog = 0;
		}
	    }
	} else oseqret |= -1;		/* else -- alarm rang */
	alarm(0);
	signal(SIGALRM,SIG_IGN);
	if (!delay) return(oseqret);
#ifndef MAC
	msleep(DEL_MSEC);		/* delay, loop to next send */
#endif /* MAC */
    }
}

/*  L O G I N  --  Login to remote system */

int
dologin(cmdstr) char *cmdstr; {

	SIGTYP (*savealm)();		/* save incoming alarm function */
	char *e;

	s = cmdstr;			/* make global to ckuscr.c */

	tlog(F100,loginv,"",0L);

	if (speed < 0L) speed = ttgspd();
	if (ttopen(ttname,&local,mdmtyp,0) < 0) {
	    sprintf(seq_buf,"Sorry, can't open %s",ttname);
	    perror(seq_buf);
	    return(0);
    	}
	/* Whether to echo script cmds */
	scr_echo = (!quiet && !backgrd && secho);
#ifndef NOSPL
	if (scr_echo && cmdlvl > 1) {
	    if (cmdstk[cmdlvl].src == CMD_TF)
	      scr_echo = techo;
	    if (cmdstk[cmdlvl].src == CMD_MD)
	      scr_echo = mecho;
	}
#endif /* NOSPL */
    	if (scr_echo) {
#ifdef NETCONN
	    if (network)
	      printf("Executing SCRIPT to host %s.\n",ttname);
	    else
#endif /* NETCONN */
	      printf("Executing SCRIPT through %s, speed %ld.\n",ttname,speed);
	}
	*seq_buf=0;
	for (e = s; *e; e++) strcat(seq_buf, dbchr(*e) );
#ifdef COMMENT
/* Skip this because it tends to contain a password... */
	if (scr_echo) printf("SCRIPT string: %s\n",seq_buf);
#endif /* COMMENT */
	tlog(F110,"SCRIPT string: ",seq_buf, 0L);

/* Condition console terminal and communication line */	    

	if (ttvt(speed,flow) < 0) {
	    printf("Sorry, Can't condition communication line\n");
	    return(0);
    	}
	/* Save initial timer interrupt value */
	savealm = signal(SIGALRM,SIG_IGN);

	flushi();		/* flush stale input */

/* start expect - send sequence */

    while (*s) {		/* while not done with buffer */

	while (*s && isspace(*s)) s++;	/* skip over separating whitespaces */
					/* gather up expect sequence */
	got_it = 0;
	recvseq();

	while (!got_it) {		/* Have it yet? */
	    if (*s++ != '-')		/* No, is there a conditional send? */
	      goto failret;		/* No, return failure */
	    flushi();			/* Yes, flush out input buffer */
	    if (outseq())		/* If unable to send, */
	      goto failret;		/* return failure. */
	    if (*s++ != '-')		/* If no conditional response here, */
	      goto failret;		/* return failure. */
	    recvseq();			/* All OK, read response from host. */
	}				/* Loop back and check got_it */

	while (*s && !isspace(*s++) ) ;	/* Skip over conditionals */
	while (*s && isspace(*s)) s++;	/* Skip over separating whitespaces */
	flushi();			/* Flush */
	if (*s) if (outseq()) goto failret; /* If any */
    }
    signal(SIGALRM,savealm);
    if (scr_echo) printf("Script successful.\n");
    tlog(F100,"Script successful.","",0L);
    return(1);

failret:
    signal(SIGALRM,savealm);
    if (scr_echo) printf("Sorry, script failed\n");
    tlog(F100,"Script failed","",0L);
    return(0);
}

/*  F L U S H I  --  Flush, but log, input buffer  */

VOID
flushi() {
    int n;
    if (seslog) {			/* Logging session? */
        n = ttchk();			/* Yes, anything in buffer? */
        if (n > 0) {			/* If so, */
	    if (n > SBUFL) n = SBUFL;	/* make sure not too much, */
	    n = ttxin(n,(CHAR *)fls_buf); /* then read it, */
	    if (n > 0) if (zsout(ZSFILE,fls_buf) < 0) seslog = 0;
	}
    } else ttflui();			/* Otherwise just flush. */
}

#ifdef MAC
alarm (s) int s; {			/* Fix this later */
}
#endif /* MAC */
#else
char *loginv = "Script Command Disabled";
#endif /* NOSCRIPT */
#endif /* NOICP */
