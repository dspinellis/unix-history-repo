char *protv = "C-Kermit Protocol Module 5A(052), 23 Nov 92"; /* -*-C-*- */

/* C K C P R O  -- C-Kermit Protocol Module, in Wart preprocessor notation. */
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/
#include "ckcdeb.h"
#include "ckcasc.h"
#include "ckcker.h"
/*
 Note -- This file may also be preprocessed by the Unix Lex program, but 
 you must indent the above #include statements before using Lex, and then
 restore them to the left margin in the resulting C program before compilation.
 Also, the invocation of the "wart()" function below must be replaced by an
 invocation  of the "yylex()" function.  It might also be necessary to remove
 comments in the %%...%% section.
*/

/* State definitions for Wart (or Lex) */
%states ipkt rfile rattr rdata ssinit ssfile ssattr ssdata sseof sseot
%states serve generic get rgen

/* External C-Kermit variable declarations */
  extern char *versio, *srvtxt, *cmarg, *cmarg2, **cmlist;
  extern char filnam[], ttname[];
  extern CHAR sstate, *rpar(), encbuf[], *srvptr, *data;
  extern int timint, rtimo, nfils, hcflg, xflg, flow, mdmtyp, network;
  extern int cxseen, czseen, server, srvdis, local, displa, bctu, bctr, bctl;
  extern int quiet, tsecs, parity, backgrd, nakstate, atcapu, wslotn, winlo;
  extern int wslots, success, xitsta, rprintf, discard, cdtimo, keep, fdispla;
  extern int timef;
  extern long speed, ffc;
  extern char *DIRCMD, *DIRCM2, *DELCMD, *TYPCMD, *SPACMD, *SPACM2, *WHOCMD;
  extern CHAR *rdatap;
  extern struct zattr iattr;
#ifdef DYNAMIC
  extern CHAR *srvcmd;
#else
  extern CHAR srvcmd[];
#endif /* DYNAMIC */

#ifndef NOSPL
  extern int cmdlvl;
#else
  extern int tlevel;
#endif /* NOSPL */

#ifdef NOMSEND
  extern int sndsrc;
#endif /* NOMSEND */

/* Flags for the ENABLE and DISABLE commands */
extern int
  en_cwd, en_del, en_dir, en_fin, en_get, en_bye,
  en_hos, en_sen, en_spa, en_set, en_typ, en_who;

/* Global variables declared here */

  int what = W_NOTHING;			/* What we're doing */

/* Local variables */

  static char vstate = 0;  		/* Saved State   */
  static char vcmd = 0;    		/* Saved Command */

  static int x;				/* General-purpose integer */
  static char *s;			/* General-purpose string pointer */

/* Macros - Note, BEGIN is predefined by Wart (and Lex) as "state = ", */
/* BEGIN is NOT a GOTO! */
#define TINIT  if (tinit() < 0) return(-9)
#define SERVE  TINIT; nakstate = 1; what = W_NOTHING; BEGIN serve
#define RESUME if (server) { SERVE; } else { sleep(2); return(0); }
#define QUIT x=quiet; quiet=1; clsif(); clsof(1); tsecs=gtimer(); quiet=x; \
 return(1)

%%
/*
  Protocol entry points, one for each start state (sstate).
  The lowercase letters are internal "inputs" from the user interface.
*/

s { TINIT;				/* Do Send command */
    if (sinit()) BEGIN ssinit;
       else RESUME; }

v { TINIT; nakstate = 1; BEGIN get; }			        /* Receive */
r { TINIT; vstate = get;  vcmd = 0;   sipkt('I'); BEGIN ipkt; } /* Get */
c { TINIT; vstate = rgen; vcmd = 'C'; sipkt('I'); BEGIN ipkt; } /* Host */
k { TINIT; vstate = rgen; vcmd = 'K'; sipkt('I'); BEGIN ipkt; } /* Kermit */
g { TINIT; vstate = rgen; vcmd = 'G'; sipkt('I'); BEGIN ipkt; } /* Generic */

x { sleep(1); SERVE; }			/* Be a Server */

a { if (!data) TINIT;			/* "ABEND" -- Tell other side. */
    errpkt((CHAR *)"User cancelled");
    success = 0;
    return(0); }			/* Return from protocol. */

/*
  Dynamic states: <current-states>input-character { action }
  nakstate != 0 means we're in a receiving state, in which we send ACKs & NAKs.
*/
<rgen,get,serve>S {			/* Receive Send-Init packet. */
    if (state == serve && !en_sen) {	/* Not allowed if in server mode */
	errpkt((CHAR *)"SEND disabled"); /* and SEND is disabled. */
	SERVE;
    } else {				/* OK to go ahead. */
	nakstate = 1;			/* Can send NAKs from here. */
	rinit(rdatap);			/* Set parameters */
	bctu = bctr;			/* Switch to agreed-upon block check */
	bctl = (bctu == 4) ? 2 : bctu;	/* Set block-check length */
	what = W_RECV;			/* Remember we're receiving */
	resetc();			/* Reset counters */
	rtimer();			/* Reset timer */
	BEGIN rfile;			/* Go into receive-file state */
    }
}

/* States in which we get replies back from commands sent to a server. */
/* Complicated because direction of protocol changes, packet number    */
/* stays at zero through I-G-S sequence, and complicated even more by  */
/* sliding windows buffer allocation. */

<ipkt>Y {				/* Get ack for I-packet */
    spar(rdatap);			/* Set parameters */
#ifdef COMMENT
    getsbuf(winlo = 0);			/* Set window-low back to zero */
#else
    winlo = 0;
#endif /* COMMENT */
    if (vcmd) {				/* If sending a generic command */
	scmd(vcmd,(CHAR *)cmarg);	/* Do that */
	vcmd = 0;			/* and then un-remember it. */
    } else if (vstate == get) srinit();	/* If sending GET command, do that. */
    rtimer();				/* Reset the elapsed seconds timer. */
    winlo = 0;				/* Window back to 0, again. */
    nakstate = 1;			/* Can send NAKs from here. */
    BEGIN vstate;			/* Switch to desired state */
}

<ipkt>E {				/* Ignore Error reply to I packet */
#ifdef COMMENT
    getsbuf(winlo = 0);			/* Set window-low back to zero */
#else
    winlo = 0;
#endif /* COMMENT */
    if (vcmd) {				/* In case other Kermit doesn't */
	scmd(vcmd,(CHAR *)cmarg);	/* understand I-packets. */
	vcmd = 0;			/* Otherwise act as above... */
    } else if (vstate == get) srinit();
    winlo = 0;				/* Back to packet 0 again. */
    freerpkt(winlo);			/* Discard the Error packet. */
    nakstate = 1;			/* Can send NAKs from here. */
    BEGIN vstate;
}

<get>Y {		/* Resend of previous I-pkt ACK, same seq number! */
    srinit();
}

/* States in which we're being a server */

<serve>I {				/* Get I-packet */
    spar(rdatap);			/* Set parameters from it */
    ack1(rpar());			/* Respond with our own parameters */
    pktinit();				/* Reinitialize packet numbers */
}

<serve>R {				/* Get Receive-Init (GET) */
    debug(F100,"<serve>R","",0);
    if (!en_get) {			/* Only if not disabled!  */
	errpkt((CHAR *)"GET disabled");
	SERVE;
    } else {				/* OK to go ahead. */
	srvptr = srvcmd;		/* Point to server command buffer */
	decode(rdatap,putsrv,0);	/* Decode the GET command into it */
	/* Accept multiple filespecs */
        cmarg2 = "";			/* Don't use cmarg2 */
	cmarg = "";			/* Don't use cmarg */
#ifndef NOMSEND				/* New way. */
	nfils = fnparse((char *)srvcmd); /* Use cmlist instead */
#else
	nfils = 0 - zxpand((char *)srvcmd);
#endif /* NOMSEND */
	nakstate = 0;			/* Now I'm the sender! */
	if (sinit()) {			/* Send Send-Init */
	    timint = chktimo(rtimo,timef); /* Switch to per-packet timer */
	    BEGIN ssinit;		/* If successful, switch state */
	} else { SERVE; }		/* Else back to server command wait */
    }
}

<serve>G {				/* Generic server command */
    srvptr = srvcmd;			/* Point to command buffer */
    decode(rdatap,putsrv,0);		/* Decode packet data into it */
    putsrv('\0');			/* Insert a couple nulls */
    putsrv('\0');			/* for termination */
    if (srvcmd[0]) {
	sstate = srvcmd[0];		/* Set requested start state */
	nakstate = 0;			/* Now I'm the sender. */
	what = W_REMO;			/* Doing a REMOTE command. */
	if (timint < 1)
	  timint = chktimo(rtimo,timef); /* Switch to per-packet timer */
	BEGIN generic;			/* Switch to generic command state */
    } else {
	errpkt((CHAR *)"Badly formed server command"); /* report error */
	SERVE;				/* & go back to server command wait */
    }
}

<serve>C {				/* Receive Host command */
    if (!en_hos) {
	errpkt((CHAR *)"REMOTE HOST disabled");
	SERVE;
    } else {
	srvptr = srvcmd;		/* Point to command buffer */
	decode(rdatap,putsrv,0);	/* Decode command packet into it */
	putsrv('\0');			/* Null-terminate */
	nakstate = 0;			/* Now sending, not receiving */
	if (syscmd((char *)srvcmd,"")) { /* Try to execute the command */
	    what = W_REMO;		/* Doing a REMOTE command. */
	    if (timint < 1)
	      timint = chktimo(rtimo,timef); /* Switch to per-packet timer */
	    BEGIN ssinit;		/* If OK, send back its output */
	} else {			/* Otherwise */
	    errpkt((CHAR *)"Can't do system command"); /* report error */
	    SERVE;			/* & go back to server command wait */
	}
    }
}

<serve>q {				/* User typed Ctrl-C... */
    if (!en_fin) {
	errpkt((CHAR *)"QUIT disabled");
	SERVE;
    } else {
	success = 0; QUIT;
    }
}

<serve>N {				/* Server got a NAK in command-wait */
    errpkt((CHAR *)"Did you say RECEIVE instead of GET?");
    SERVE;
}

<serve>. {				/* Any other command in this state */
    errpkt((CHAR *)"Unimplemented server function"); /* we don't know about */
    SERVE;				/* back to server command wait */
}

<generic>C {				/* Got REMOTE CWD command */
    if (!en_cwd) {
	errpkt((CHAR *)"REMOTE CD disabled");
	SERVE;
    } else {
	if (!cwd((char *)(srvcmd+1))) errpkt((CHAR *)"Can't change directory");
	SERVE;				/* Back to server command wait */
    }
}

<generic>D {				/* REMOTE DIRECTORY command */
    char *n2;
    if (!en_dir) {			/* If DIR is disabled, */
	errpkt((CHAR *)"REMOTE DIRECTORY disabled"); /* refuse. */
	SERVE;
    } else {				/* DIR is enabled. */
	if (!en_cwd) {			/* But if CWD is disabled */
	    zstrip((char *)(srvcmd+2),&n2); /* and they included a pathname, */
	    if (strcmp((char *)(srvcmd+2),n2)) { /* refuse. */
		errpkt((CHAR *)"Access denied");
		SERVE;			/* Remember, this is not a goto! */
	    }
	}	
	if (state == generic) {			/* It's OK to go ahead. */
	    n2 = (*(srvcmd+2)) ? DIRCMD : DIRCM2;
	    if (syscmd(n2,(char *)(srvcmd+2)))  /* If it can be done */
	      BEGIN ssinit;			/* send the results back */
	    else {				/* otherwise */
		errpkt((CHAR *)"Can't list directory"); /* report failure, */
		SERVE;			/* return to server command wait */
	    }
	}
    }
}

<generic>E {				/* REMOTE DELETE (Erase) command */
    char *n2;
    if (!en_del) {
	errpkt((CHAR *)"REMOTE DELETE disabled");    
	SERVE;
    } else {
	if (!en_cwd) {			/* But if CWD is disabled */
	    zstrip((char *)(srvcmd+2),&n2); /* and they included a pathname, */
	    if (strcmp((char *)(srvcmd+2),n2)) { /* refuse. */
		errpkt((CHAR *)"Access denied");
		SERVE;			/* Remember, this is not a goto! */
	    }
	}	
	if (state == generic) {			/* It's OK to go ahead. */
	    if (syscmd(DELCMD,(char *)(srvcmd+2))) /* Try to do it */
	      BEGIN ssinit;			/* If OK send results back */
	    else {				/* otherwise */
		errpkt((CHAR *)"Can't remove file"); /* report failure */
		SERVE;			/* & return to server command wait */
	    }
	}
    }
}

<generic>F {				/* FINISH */
    if (!en_fin) {
	errpkt((CHAR *)"FINISH disabled");    
	SERVE;
    } else {
	ack();				/* Acknowledge */
	screen(SCR_TC,0,0l,"");		/* Display */
	return(0);			/* Done */
    }
}

<generic>L {				/* BYE (LOGOUT) */
    if (!en_bye) {
	errpkt((CHAR *)"BYE disabled");    
	SERVE;
    } else {
	ack();				/* Acknowledge */
	ttres();			/* Reset the terminal */
	screen(SCR_TC,0,0l,"");		/* Display */
	doclean();			/* Clean up files, etc */
	return(zkself());		/* Try to log self out */
    }
}

<generic>H {				/* REMOTE HELP */
    if (sndhlp()) BEGIN ssinit;		/* Try to send it */
    else {				/* If not ok, */
	errpkt((CHAR *)"Can't send help"); /* send error message instead */
	SERVE;				/* and return to server command wait */
    }
}

<generic>S {				/* REMOTE SET */
    if (!en_set) {
	errpkt((CHAR *)"REMOTE SET disabled");
	SERVE;
    } else {
	if (remset((char *)(srvcmd+1)))	/* Try to do what they ask */
	  ack();			/* If OK, then acknowledge */
	else				/* Otherwise */
	  errpkt((CHAR *)"Unknown REMOTE SET parameter"); /* give error msg */
	SERVE;				/* Return to server command wait */
    }
}

<generic>T {				/* REMOTE TYPE */
    char *n2;
    if (!en_typ) {
	errpkt((CHAR *)"REMOTE TYPE disabled");
	SERVE;
    } else {
	if (!en_cwd) {			/* But if CWD is disabled */
	    zstrip((char *)(srvcmd+2),&n2); /* and they included a pathname, */
	    if (strcmp((char *)(srvcmd+2),n2)) { /* refuse. */
		errpkt((CHAR *)"Access denied");
		SERVE;			/* Remember, this is not a goto! */
	    }
	}	
	if (state == generic) {			/* It's OK to go ahead. */
	    if (syscmd(TYPCMD,(char *)(srvcmd+2))) /* Try */
	      BEGIN ssinit;			/* OK */
	    else {				/* not OK */
		errpkt((CHAR *)"Can't type file"); /* give error message */
		SERVE;			/* wait for next server command */
	    }
	}
    }
}

<generic>U {				/* REMOTE SPACE */
    if (!en_spa) {
	errpkt((CHAR *)"REMOTE SPACE disabled");
	SERVE;
    } else {
	x = *(srvcmd+1);		/* Get area to check */
	x = ((x == '\0') || (x == SP)
#ifdef OS2
	     || (x == '!')
#endif /* OS2 */
	     );
	if (!x && !en_cwd) {		/* If CWD disabled and they gave */
	    errpkt((CHAR *)"Access denied"); /* a non-default area, */
	    SERVE;			/* refuse. */
	} else {
#ifdef OS2
_PROTOTYP(int sndspace,(int));
	    if (sndspace(x ? toupper(srvcmd[2]) : 0))
	      BEGIN ssinit;		/* Try to send it */
	    else {			/* If not ok, */
		errpkt((CHAR *)"Can't send space"); /* send error message */
		SERVE;			/* and return to server command wait */
	    }
#else
	    x = (x ? syscmd(SPACMD,"") : syscmd(SPACM2,(char *)(srvcmd+2)));
	    if (x) {				/* If we got the info */
		BEGIN ssinit;			/* send it */
	    } else {				/* otherwise */
		errpkt((CHAR *)"Can't check space"); /* send error message */
		SERVE;			/* and await next server command */
	    }
#endif /* OS2 */
	}
    }
}

<generic>W {				/* REMOTE WHO */
    if (!en_who) {
	errpkt((CHAR *)"REMOTE WHO disabled");
	SERVE;
    } else {
	if (syscmd(WHOCMD,(char *)(srvcmd+2))) /* The now-familiar scenario. */
	  BEGIN ssinit;
	else {
	    errpkt((CHAR *)"Can't do who command");
	    SERVE;
	}
    }
}

<generic>q {
    if (!en_fin) {			/* Ctrl-C typed */
	errpkt((CHAR *)"QUIT disabled");
	SERVE;
    } else {
	success = 0; QUIT;
    }
}

<generic>. {				/* Anything else in this state... */
    errpkt((CHAR *)"Unimplemented REMOTE command"); /* Complain */
    SERVE;				/* and return to server command wait */
}

<rgen>Y {				/* Short-Form reply */
    decode(rdatap,puttrm,0);		/* in ACK Data field */
    if (rdatap && *rdatap) conoll("");	/* Maybe add a CRLF */
    RESUME;
}

<rgen,rfile>F {				/* File header */
    xflg = 0;				/* Not screen data */
    if (!rcvfil(filnam)) {		/* Figure out local filename */
	errpkt((CHAR *)"Can't transform filename"); /* Trouble */
	RESUME;
    } else {				/* OK to receive */
	encstr((CHAR *)filnam);		/* Encode the name */
	ack1((CHAR *)(encbuf+7));	/* Send it back in ACK */
	initattr(&iattr);		/* Clear file attribute structure */
	if (window(wslotn) < 0) {	/* Allocate negotiated window slots */
	    errpkt((CHAR *)"Can't open window");
	    RESUME;
	}
	BEGIN rattr;			/* Now expect Attribute packets */
    }
}

<rgen,rfile>X {				/* X-packet instead of file header */
    xflg = 1;				/* Screen data */
    ack();				/* Acknowledge the X-packet */
    initattr(&iattr);			/* Initialize attribute structure */
    if (window(wslotn) < 0) {		/* allocate negotiated window slots */
	errpkt((CHAR *)"Can't open window");
	RESUME;
    }
    what = W_REMO;			/* we're doing a REMOTE command */
    BEGIN rattr;			/* Expect Attribute packets */
}

<rattr>A {				/* Attribute packet */
    if (discard) {			/* If SET FILE COLLISION DISCARD */
	ack1((CHAR *)"N");		/* refuse it */
	screen(SCR_ST,ST_REFU,0L,"file collision setting");
    } else if (gattr(rdatap,&iattr) == 0) { /* Read into attribute structure */
	ack();				/* If OK, acknowledge */
    } else {				/* Otherwise */
	ack1((CHAR *)iattr.reply.val);	/* refuse to accept the file */
	screen(SCR_ST,ST_REFU,0L,getreason(iattr.reply.val)); /* give reason */
    }
}

<rattr>D {				/* First data packet */
    if (discard) {			/* if we're discarding the file */
	ack1((CHAR *)"X");		/* just ack the data like this. */
	BEGIN rdata;			/* and wait for more data packets. */
    } else {				/* Not discarding. */
	if (xflg)			/* If screen data */
	  x = opent(&iattr);		/* "open" the screen */
	else				/* otherwise */
	  x = opena(filnam,&iattr);	/* open the file, with attributes */
	if (x) {			/* If file was opened ok */
	    if (decode(rdatap,putfil,1) < 0) { /* decode first data packet */
		errpkt((CHAR *)"Error writing data");
		RESUME;
	    }
	    ack();			/* acknowledge it */
	    BEGIN rdata;		/* and switch to receive-data state */
	} else {			/* otherwise */
	    errpkt((CHAR *)"Can't open file");	/* send error message */
	    RESUME;			/* and quit. */
	}
    }
}

<rfile>B {				/* EOT, no more files */
    ack();				/* Acknowledge */
    tsecs = gtimer();			/* Get timing for statistics */
    reot();				/* Do EOT things */
    RESUME;				/* and quit */
}

<rdata>D {				/* Data packet */
    if (cxseen || discard)		/* If file interrupt */
      ack1((CHAR *)"X");		/* put "X" in ACK */
    else if (czseen)			/* If file-group interrupt */
      ack1((CHAR *)"Z");		/* put "Z" in ACK */
    else if (decode(rdatap,putfil,1) < 0) { /* Normal case, decode to file */
	errpkt((CHAR *)"Error writing data"); /* If failure, */
	clsof(!keep);			/*   Close & keep/discard the file */
	RESUME;				/* Send ACK only after data */
    } else ack();			/* written to file OK. */
}

<rattr>Z {				/* EOF immediately after A-Packet. */
    if (xflg)				/* Zero-length file. If screen data */
      x = opent(&iattr);		/* "open" the screen */
    else				/* otherwise */
      x = opena(filnam,&iattr);		/* open the file, with attributes. */
    if (!x || reof(filnam, &iattr) < 0) { /* Now close & dispose of the file */
	errpkt((CHAR *)"Can't create file"); /* If problem, send error msg */
	RESUME;				/* and quit */
    } else {				/* otherwise */
	ack();				/* acknowledge the EOF packet */
	BEGIN rfile;			/* and await another file */
    }
}

<rdata>Z {				/* End Of File (EOF) Packet */
/*  wslots = 1;	*/			/* Window size back to 1 */
#ifndef COHERENT
/*
  Coherent compiler blows up on this switch() statement.
*/
    x = reof(filnam, &iattr);		/* Handle the EOF packet */
    switch (x) {			/* reof() sets the success flag */
      case -3:				/* If problem, send error msg */
	errpkt((CHAR *)"Can't print file"); /* Fatal */
        RESUME;
	break;
      case -2:
	errpkt((CHAR *)"Can't mail file"); /* Fatal */
        RESUME;
	break;
      case 2:
      case 3:
	screen(SCR_EM,0,0l,"Can't delete temp file"); /* Not fatal */
        RESUME;
	break;
      default:
	if (x < 0) {			/* Fatal */
	    errpkt((CHAR *)"Can't close file");
	    RESUME;
	} else {			/* Success */
	    ack();			/* Acknowledge the EOF packet */
	    BEGIN rfile;		/* and await another file */
	}
    }
#else
    if (reof(filnam, &iattr) < 0) {	/* Close and dispose of the file */
	errpkt((CHAR *)"Error at end of file");
	RESUME;
    } else {				/* reof() sets success flag */
	ack();
	BEGIN rfile;
    }
#endif /* COHERENT */
}

<ssinit>Y {				/* ACK for Send-Init */
    spar(rdatap);			/* set parameters from it */
    bctu = bctr;			/* switch to agreed-upon block check */
    bctl = (bctu == 4) ? 2 : bctu;	/* Set block-check length */
    what = W_SEND;			/* Remember we're sending */
    x = sfile(xflg);			/* Send X or F header packet */
    if (x) {				/* If the packet was sent OK */
	resetc();			/* reset per-transaction counters */
	rtimer();			/* reset timers */
	BEGIN ssfile;			/* and switch to receive-file state */
    } else {				/* otherwise send error msg & quit */
	s = xflg ? "Can't execute command" : "Can't open file";
	errpkt((CHAR *)s);
	RESUME;
    }
}

/*
 These states are necessary to handle the case where we get a server command
 packet (R, G, or C) reply with an S packet, but the client retransmits the 
 command packet.  The input() function doesn't catch this because the packet 
 number is still zero.
*/
<ssinit>R {				/* R packet was retransmitted. */
    xsinit();				/* Resend packet 0 */
}

<ssinit>G {				/* Same deal if G packet comes again */
    xsinit();
}

<ssinit>C {				/* Same deal if C packet comes again */
    xsinit();
}

<ssfile>Y {				/* ACK for F packet */
    srvptr = srvcmd;			/* Point to string buffer */
    decode(rdatap,putsrv,0);		/* Decode data field, if any */
    putsrv('\0');			/* Terminate with null */
    ffc = 0L;				/* Reset file byte counter */
    if (*srvcmd) {			/* If remote name was recorded */
	if (fdispla == XYFD_C) screen(SCR_AN,0,0L,(char *)srvcmd);
	tlog(F110," stored as",(char *) srvcmd,0L); /* Transaction log. */
    }
    if (atcapu) {			/* If attributes are to be used */
	if (sattr(xflg) < 0) {		/* set and send them */
	    errpkt((CHAR *)"Can't send attributes"); /* if problem, say so */
	    RESUME;			     /* and quit */
	} else BEGIN ssattr;		/* if ok, switch to attribute state */
    } else {
	if (window(wslotn) < 0) {
	    errpkt((CHAR *)"Can't open window");
	    RESUME;
	}
	if (sdata() < 0) {		/* No attributes, send data */
	    clsif();			/* If not ok, close input file, */
	    window(1);			/* put window size back to 1, */
	    seof((CHAR *)"");		/* send EOF packet, */
	    BEGIN sseof;		/* and switch to EOF state. */
	} else BEGIN ssdata;		/* All ok, switch to send-data state */
    }
}

<ssattr>Y {				/* Got ACK to A packet */
    ffc = 0L;				/* Reset file byte counter */
    if (rsattr(rdatap) < 0) {		/* Was the file refused? */
	discard = 1;			/* Set the discard flag */
	clsif();			/* Close the file */
	sxeof((CHAR *)"D");		/* send EOF with "discard" code */
	BEGIN sseof;			/* switch to send-EOF state */
    } else {
	if (window(wslotn) < 0) {	/* Allocate negotiated window slots */
	    errpkt((CHAR *)"Can't open window");
	    RESUME;
	}
	if (sdata() < 0) {		/* File accepted, send data */
	    clsif();			/* If problem, close input file */
	    window(1);			/* Window size back to 1... */
	    seof((CHAR *)"");		/* send EOF packet */
	    BEGIN sseof;		/* and switch to send-EOF state. */
	} else {			/* All ok, enter send-data state. */
	    BEGIN ssdata;
	}
    }
}

<ssdata>Y {				/* Got ACK to Data packet */
    canned(rdatap);			/* Check if file transfer cancelled */
    if (sdata() < 0) {			/* Try to send next data */
	clsif();			/* If no more data, close file */
	window(1);			/* Window size back to 1... */
	if (cxseen || czseen)		/* If interrupted */
	  seof((CHAR *)"D");		/* send special EOF packet */
	else seof((CHAR *)"");		/* Otherwise regular EOF packet */
	BEGIN sseof;			/* And enter send-eof state */
    }
}

<sseof>Y {				/* Got ACK to EOF */
    success = (cxseen == 0 && czseen == 0); /* Set this for IF command */
    cxseen = 0;				/* This goes back to zero. */
    if (gnfile() > 0) {			/* Any more files to send? */
	if (sfile(xflg))		/* Yes, try to send next file header */
	  BEGIN ssfile;			/* if ok, enter send-file state */
	else {				/* otherwise */
	    errpkt((CHAR *)"Can't open file");	/* send error message */
	    RESUME;			/* and quit */
	}
    } else {				/* No next file */
	tsecs = gtimer();		/* get statistics timers */
	seot();				/* send EOT packet */
	BEGIN sseot;			/* enter send-eot state */
    }
}

<sseot>Y {				/* Got ACK to EOT */
    RESUME;				/* All done, just quit */
}

E {					/* Got Error packet, in any state */
    ermsg((char *)rdatap);		/* Issue message. */
    success = 0;			/* For IF SUCCESS/FAIL. */
    debug(F101,"ckcpro.w sstate at E pkt","",sstate);
    x = quiet; quiet = 1;		/* Close files silently, */
    clsif(); clsof(1);			/* discarding any output file. */
    tsecs = gtimer();			/* Get timers */
    quiet = x;				/* restore quiet state */
/*
  If we are executing commands from a command file or macro, let the command
  file or macro decide whether to exit, based on SET { TAKE, MACRO } ERROR.
*/
    if (
#ifndef NOSPL
	cmdlvl == 0
#else
	tlevel < 0
#endif /* NOSPL */
	)
      if (backgrd && !server)
	fatal("Protocol error");
    xitsta |= what;			/* Save this for doexit(). */
    RESUME;
}

q { QUIT; }				/* Ctrl-C interrupt during packets. */

. {					/* Anything not accounted for above */
    errpkt((CHAR *)"Unexpected packet type"); /* Give error message */
    xitsta |= what;			/* Save this for doexit(). */
    RESUME;				/* and quit */
}
%%

/*  P R O T O  --  Protocol entry function  */

VOID
proto() {

    int x;
    long lx;

/* Set up the communication line for file transfer. */

    if (local && (speed < 0L) && (network == 0)) {
	screen(SCR_EM,0,0l,"Sorry, you must 'set speed' first");
	return;
    }
    x = -1;
    if (ttopen(ttname,&x,mdmtyp,cdtimo) < 0) {
	debug(F111,"failed: proto ttopen local",ttname,local);
	screen(SCR_EM,0,0l,"Can't open line");
	return;
    }
    if (x > -1) local = x;
    debug(F111,"proto ttopen local",ttname,local);

    lx = (local && !network) ? speed : -1;
    if (ttpkt(lx,flow,parity) < 0) {	/* Put line in packet mode, */
	screen(SCR_EM,0,0l,"Can't condition line");
	return;
    }
    if (!local) connoi();		/* No console interrupts if remote */

    if (sstate == 'x') {		/* If entering server mode, */
	server = 1;			/* set flag, */
	debug(F101,"server backgrd","",backgrd);
	debug(F101,"server quiet","",quiet);
	if (!quiet && !backgrd) {
    	    debug(F100,"SHOULD NOT SEE THIS IF IN BACKGROUND!","",0);
	    if (!local)	{		/* and issue appropriate message. */
	    	conoll(srvtxt);
		conoll("KERMIT READY TO SERVE...");
	    } else {
	    	conol("Entering server mode on ");
		conoll(ttname);
		conoll("Type Ctrl-C to quit.");
		if (srvdis) intmsg(-1L);
	    }
	}
    } else server = 0;
#ifdef VMS
    if (!quiet && !backgrd)    /* So message doesn't overwrite prompt */
      conoll("");
    if (local) conres();       /* So Ctrl-C will work */
#endif /* VMS */
/*
  If in remote mode, not shushed, not in background, and at top command level,
  issue a helpful message telling what to do...
*/
    if (!local && !quiet && !backgrd &&
#ifndef NOSPL
	cmdlvl == 0
#else
	tlevel < 0
#endif /* NOSPL */
	) {
	if (sstate == 'v') {
	    conoll("Return to your local Kermit and give a SEND command.");
	    conoll("");
	    conoll("KERMIT READY TO RECEIVE...");
	} else if (sstate == 's') {
	    conoll("Return to your local Kermit and give a RECEIVE command.");
	    conoll("");
	    conoll("KERMIT READY TO SEND...");
	} else if ( sstate == 'g' || sstate == 'r' || sstate == 'c' ) {
	    conoll("Return to your local Kermit and give a SERVER command.");
	    conoll("");
	    conoll((sstate == 'r') ?
		   "KERMIT READY TO GET..." :
		   "KERMIT READY TO SEND SERVER COMMAND...");
	}
    }
    sleep(1);
/*
 The 'wart()' function is generated by the wart program.  It gets a
 character from the input() routine and then based on that character and
 the current state, selects the appropriate action, according to the state
 table above, which is transformed by the wart program into a big case
 statement.  The function is active for one transaction.
*/
    wart();				/* Enter the state table switcher. */
    
    if (server) {			/* Back from packet protocol. */
    	if (!quiet && !backgrd) {	/* Give appropriate message */
	    conoll("");
	    conoll("C-Kermit server done");
	}
    }
/*
  Note: the following is necessary in case we have just done a remote-mode
  file transfer, in which case the controlling terminal modes have been
  changed by ttpkt().  In particular, special characters like Ctrl-C and
  Ctrl-\ might have been turned off (see ttpkt).  So this call to ttres() is
  essential.
*/
#ifndef OS2
    if (!local)
#endif /* OS2 */
      ttres();				/* Reset the communication device */
    screen(SCR_TC,0,0l,"");		/* Transaction complete */
    server = 0;				/* Not a server any more */
}
