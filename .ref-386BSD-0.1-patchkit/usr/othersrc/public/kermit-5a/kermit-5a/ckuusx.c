/*  C K U U S X --  "User Interface" common functions. */
 
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

/*
  This module contains user interface functions needed by both the interactive
  user interface and the command-line-only user interface.
*/

/* Includes */

#include "ckcdeb.h"
#include "ckcasc.h"
#include "ckcker.h"
#include "ckuusr.h"
#include <signal.h>

#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
#include <stsdef.h>
#endif /* VMS */

/* Variables declared here */

#ifdef DEBUG
char debfil[50];			/* Debugging log file name */
#endif /* DEBUG */

#ifdef TLOG
char trafil[50];			/* Transaction log file name */
#endif /* TLOG */

char pktfil[50];			/* Packet log file name */
char sesfil[50];			/* Session log file name */

#ifndef NOFRILLS
char optbuf[50];			/* Options for MAIL or REMOTE PRINT */
#endif /* NOFRILLS */
char cmdstr[256];			/* Place to build generic command */

char fspec[FSPECL];			/* Filename string for \v(filespec) */

int success = 1,			/* Command success/failure flag */

#ifndef NOSPL
    cmdlvl = 0,				/* Command level */
#endif /* NOSPL */
    action,				/* Action selected on command line*/
    sessft = 0,				/* Session log file type, 0 = text */
    pflag = 1;				/* Print prompt & messages */

#ifndef NOMSEND				/* Multiple SEND */
char *msfiles[MSENDMAX];
#endif /* NOMSEND */

/* External variables */

extern int local, quiet, binary, bctu, rptflg, ebqflg, network, server,
  what, spsiz, urpsiz, wmax, czseen, cxseen, winlo, displa, timint,
  npad, ebq, ebqflg, bctr, rptq, atcapu, lpcapu, swcapu, wslotn, wslotr, rtimo,
  mypadn, sq, capas, rpsiz, tsecs, dfloc, tralog, pktlog, seslog, lscapu,
  xitsta, escape, tlevel, bgset, backgrd, wslots, suspend, srvdis;

extern long speed, filcnt, ffc, rptn, fsize;

extern CHAR *rdatap, padch, seol, ctlq, mypadc, eol;

extern char ttname[], *dftty, *cmarg, **cmlist;
#ifndef NOICP
#ifdef DCMDBUF
extern char *cmdbuf;			/* Command buffer */
#else
extern char cmdbuf[];			/* Command buffer */
#endif /* DCMDBUF */
#endif /* NOICP */

#ifndef NOCCTRAP
#include <setjmp.h>			/* Control-C trap */
jmp_buf cmjbuf;
#endif /* NOCCTRAP */

/*  S D E B U  -- Record spar results in debugging log  */

VOID
sdebu(len) int len; {
    debug(F111,"spar: data",(char *) rdatap,len);
    debug(F101," spsiz ","", spsiz);
    debug(F101," timint","",timint);
    debug(F101," npad  ","",  npad);
    debug(F101," padch ","", padch);
    debug(F101," seol  ","",  seol);
    debug(F101," ctlq  ","",  ctlq);
    debug(F101," ebq   ","",   ebq);
    debug(F101," ebqflg","",ebqflg);
    debug(F101," bctr  ","",  bctr);
    debug(F101," rptq  ","",  rptq);
    debug(F101," rptflg","",rptflg);
    debug(F101," lscapu","",lscapu);
    debug(F101," atcapu","",atcapu);
    debug(F101," lpcapu","",lpcapu);
    debug(F101," swcapu","",swcapu);
    debug(F101," wslotn","", wslotn);
}
/*  R D E B U -- Debugging display of rpar() values  */

VOID
rdebu(d,len) CHAR *d; int len; {
    debug(F111,"rpar: data",d,len);
    debug(F101," rpsiz ","", xunchar(d[0]));
    debug(F101," rtimo ","", rtimo);
    debug(F101," mypadn","",mypadn);
    debug(F101," mypadc","",mypadc);
    debug(F101," eol   ","",   eol);
    debug(F101," ctlq  ","",  ctlq);
    debug(F101," sq    ","",    sq);
    debug(F101," ebq   ","",   ebq);
    debug(F101," ebqflg","",ebqflg);
    debug(F101," bctr  ","",  bctr);
    debug(F101," rptq  ","",  d[8]);
    debug(F101," rptflg","",rptflg);
    debug(F101," capas ","", capas);
    debug(F101," bits  ","",d[capas]);
    debug(F101," lscapu","",lscapu);
    debug(F101," atcapu","",atcapu);
    debug(F101," lpcapu","",lpcapu);
    debug(F101," swcapu","",swcapu);
    debug(F101," wslotr","", wslotr);
    debug(F101," rpsiz(extended)","",rpsiz);
}

/*  F A T A L  --  Fatal error message */

VOID
fatal(msg) char *msg; {
    screen(SCR_EM,0,0L,msg);
    debug(F110,"fatal",msg,0);
    tlog(F110,"Fatal:",msg,0L);
    doexit(BAD_EXIT,xitsta);		/* Exit indicating failure */
}

/*  B L D L E N  --  Make length-encoded copy of string  */
 
char *
bldlen(str,dest) char *str, *dest; {
    int len;
    len = (int)strlen(str);
    *dest = tochar(len);
    strcpy(dest+1,str);
    return(dest+len+1);
}
 
 
/*  S E T G E N  --  Construct a generic command  */
 
CHAR
#ifdef CK_ANSIC
setgen(char type,char * arg1, char * arg2, char * arg3)
#else
setgen(type,arg1,arg2,arg3) char type, *arg1, *arg2, *arg3;
#endif /* CK_ANSIC */
/* setgen */ {
    char *upstr, *cp;
 
    cp = cmdstr;
    *cp++ = type;
    *cp = NUL;
    if (*arg1 != NUL) {
	upstr = bldlen(arg1,cp);
	if (*arg2 != NUL) {
	    upstr = bldlen(arg2,upstr);
	    if (*arg3 != NUL) bldlen(arg3,upstr);
	}
    }
    cmarg = cmdstr;
    debug(F110,"setgen",cmarg,0);
 
    return('g');
}

#ifndef NOMSEND
static char *mgbufp = NULL;

/*  F N P A R S E  --  */

/*
  Argument is a character string containing one or more filespecs.
  This function breaks the string apart into an array of pointers, one
  to each filespec, and returns the number of filespecs.  Used by server
  when it receives a GET command to allow it to process multiple file
  specifications in one transaction.  Sets cmlist to point to a list of
  file pointers, exactly as if they were command line arguments.

  This version of fnparse treats spaces as filename separators.  If your
  operating system allows spaces in filenames, you'll need a different 
  separator.  

  This version of fnparse mallocs a string buffer to contain the names.  It
  cannot assume that the string that is pointed to by the argument is safe.
*/
int
fnparse(string) char *string; {
    char *p, *s, *q;
    int r = 0, x;			/* Return code */

    if (mgbufp) free(mgbufp);		/* Free this from last time. */
    mgbufp = malloc((int)strlen(string)+2);
    if (!mgbufp) {
	debug(F100,"fnparse malloc error","",0);
	return(0);
    }	
#ifndef NOICP
#ifndef NOSPL
    strncpy(fspec,string,FSPECL);	/* Make copy for \v(filespec) */
#endif /* NOSPL */
#endif /* NOICP */
    s = string;				/* Input string */
    p = q = mgbufp;			/* Point to the copy */
    r = 0;				/* Initialize our return code */
    while (*s == SP) s++;		/* Skip leading spaces */
    while (1) {				/* Loop through rest of string */
	if (*s == CMDQ) {		/* Backslash (quote character)? */
	    if ((x = xxesc(&s)) > -1) {	/* Go interpret it. */
		*q++ = (char) x;	/* Numeric backslash code, ok */
	    } else {			/* Just let it quote next char */
		s++;			/* get past the backslash */
		*q++ = *s++;		/* deposit next char */
	    }
	    continue;
	} else if (*s == SP || *s == NUL) { /* Unquoted space or NUL? */
	    *q++ = NUL;			/* End of output filename. */
	    msfiles[r] = p;		/* Add this filename to the list */
	    debug(F111,"fnparse",msfiles[r],r);
	    r++;			/* Count it */
	    if (*s == NUL) break;	/* End of string? */
	    while (*s == SP) s++;	/* Skip repeated spaces */
	    p = q;			/* Start of next name */
	    continue;
	} else *q++ = *s;		/* Otherwise copy the character */
	s++;				/* Next input character */
    }
    debug(F101,"fnparse r","",r);
    cmlist = msfiles;
    return(r);
}
#endif /* NOMSEND */

char *					/* dbchr() for DEBUG SESSION */
dbchr(c) int c; {
    static char s[8];
    char *cp = s;

    c &= 0xff;
    if (c & 0x80) {			/* 8th bit on */
	sprintf(cp++,"~");
	c &= 0x7f;
    }
    if (c < SP) {			/* Control character */
	sprintf(cp,"^%c",ctl(c));
    } else if (c == DEL) {
	sprintf(cp,"^?");
    } else {				/* Printing character */
	sprintf(cp,"%c",c);
    }
    cp = s;				/* Return pointer to it */
    return(cp);
}

/*  C K H O S T  --  Get name of local host (where C-Kermit is running)  */

/*
  Call with pointer to buffer to put hostname in, and length of buffer.
  Copies hostname into buffer on success, puts null string in buffer on
  failure.
*/
#ifdef ATTSV
#include <sys/utsname.h>
#endif /* ATTSV */

VOID
ckhost(vvbuf,vvlen) char * vvbuf; int vvlen; {
    char *g;
#ifdef VMS
    int x;
#endif /* VMS */
#ifdef ATTSV
    struct utsname hname;
#endif /* ATTSV */

    *vvbuf = NUL;
#ifdef ATTSV
    if (uname(&hname) > -1) strncpy(vvbuf,hname.nodename,vvlen);
#else
#ifdef BSD4
    if (gethostname(vvbuf,vvlen) < 0) *vvbuf = NUL;
#else
#ifdef VMS
    g = getenv("SYS$NODE");
    if (g) strncpy(vvbuf,g,vvlen);
    x = (int)strlen(vvbuf);
    if (x > 1 && vvbuf[x-1] == ':' && vvbuf[x-2] == ':') vvbuf[x-2] = NUL;
#endif /* VMS */
#endif /* BSD4 */
#endif /* ATTSV */
    if (*vvbuf == NUL) {		/* If it's still empty */
        g = getenv("HOST");		/* try this */
        if (g) strncpy(vvbuf,g,vvlen);
    }
    vvbuf[vvlen-1] = NUL;		/* Make sure result is terminated. */
}

#ifndef NOSPL
#define ASKMORE
#endif /* NOSPL */
#ifndef NOHELP
#define ASKMORE
#endif /* NOHELP */

#ifdef ASKMORE
/*
  A S K M O R E  --  Poor person's "more".
  Returns 0 if no more, 1 if more wanted.
  Presently used by SHO MAC, SHO GLOB, and HELP, so compiled out if
  those options are also compiled out.
*/
int
askmore() {
    char c; int rv;

    rv = -1;
    while (rv < 0) {
#ifdef UNIX
#ifdef NOSETBUF
	printf("more? ");
	fflush(stdout);
#endif /* NOSETBUF */
#endif /* UNIX */
	c = coninc(0);
	switch (c) {
	  /* Yes */
	  case SP: case 'y': case 'Y': case 012:  case 015:
	    printf("\015      \015");
	    rv = 1;
	    break;
          /* No */
	  case 'n': case 'N': case 'q': case 'Q':
	    printf("\015\012");
	    rv = 0;
	    break;
	  /* Invalid answer */
	  default:
	    printf("Y or space-bar for yes, N for no\n");
	    continue;
	}
    }
    return(rv);
}
#endif /* ASKMORE */

/*  T R A P  --  Terminal interrupt handler */
 
SIGTYP
trap(sig) int sig; {
#ifdef VMS
    int i; FILE *f;
#endif /* VMS */
#ifdef GEMDOS
/* GEM is not reentrant, no i/o from interrupt level */
    longjmp(cmjbuf,1);			/* Jump back to parser now! */
#endif /* GEMDOS */
    debug(F101,"^C trap() caught signal","",sig);
    zclose(ZIFILE);			/* If we were transferring a file, */
    zclose(ZOFILE);			/* close it. */
#ifdef VMS
/*
  Fix terminal.
*/  
    conres();
    i = printf("^C...\n");		/* Echo ^C to standard output */
    if (i < 1 && ferror(stdout)) {	/* If there was an error */
	fclose(stdout);			/* close standard output */
	f = fopen(dftty, "w");		/* open the controlling terminal */
	if (f) stdout = f;		/* and make it standard output */
	printf("^C...\n");		/* and echo the ^C again. */
    }
#else
    printf("^C...\n");			/* Not VMS, no problem... */
#endif /* VMS */

#ifndef NOCCTRAP
#ifdef UNIX
    ttimoff();				/* Turn off any timer interrupts */
#endif /* UNIX */
#ifdef OSK
    ttimoff();				/* Turn off any timer interrupts */
#endif /* OSK */
    longjmp(cmjbuf,1);			/* Jump back to parser */
#else
/* No Ctrl-C trap, just exit. */
    doexit(BAD_EXIT,what);		/* Exit poorly */
#endif /* NOCCTRAP */
}

/*  C C _ C L E A N  --  Cleanup after terminal interrupt handler */

#ifdef GEMDOS
int
cc_clean() {
    zclose(ZIFILE);			/* If we were transferring a file, */
    zclose(ZOFILE);			/* close it. */
    printf("^C...\n");			/* Not VMS, no problem... */
}
#endif /* GEMDOS */


/*  S T P T R A P -- Handle SIGTSTP (suspend) signals */

SIGTYP
stptrap(sig) int sig; {
#ifndef NOJC
    int x; extern int cmflgs;
    debug(F101,"stptrap() caught signal","",sig);
    if (!suspend) {
	printf("\r\nsuspend disabled\r\n");
#ifndef NOICP
	if (what == W_COMMAND) {	/* If command parsing was */
	    prompt(xxstring);		/* reissue the prompt and partial */
	    if (!cmflgs)		/* command (if any) */
	      printf("%s",cmdbuf);
	}
#endif /* NOICP */
    } else {
	conres();			/* Reset the console */
#ifndef OS2
	x = psuspend(suspend);		/* Try to suspend. */
	if (x < 0)
#endif /* OS2 */
	  printf("Job control not supported\r\n"); 
	conint(trap,stptrap);		/* Rearm the trap. */
	debug(F100,"stptrap back from suspend","",0);
	switch (what) {
	  case W_CONNECT:		/* If suspended during CONNECT? */
	    conbin((char)escape);	/* put console back in binary mode */
	    debug(F100,"stptrap W_CONNECT","",0);
	    break;
#ifndef NOICP
	  case W_COMMAND:		/* Suspended in command mode */
	    debug(F101,"stptrap W_COMMAND pflag","",pflag);
	    concb((char)escape);	/* Put back CBREAK tty mode */
	    if (pflag) {		/* If command parsing was */
		prompt(xxstring);	/* reissue the prompt and partial */
		if (!cmflgs)		/* command (if any) */
		  printf("%s",cmdbuf);
	    }
	    break;
#endif /* NOICP */
	  default:			/* All other cases... */
	    debug(F100,"stptrap default","",0);
	    concb((char)escape);	/* Put it back in CBREAK mode */
	    break;
	}
    }
#endif /* NOJC */
}

#ifndef MAC
/*
  The rest of this file is for all implementations but the Macintosh.
*/

/*  C H K I N T  --  Check for console interrupts  */

int 
chkint() {
    int ch, cn; long zz;
 
    if ((!local) || (quiet)) return(0);	/* Only do this if local & not quiet */
#ifdef datageneral
    cn = (con_reads_mt) ? 1 : conchk();	/* Any input waiting? */
#else
    cn = conchk();			/* Any input waiting? */
#endif /* datageneral */
    debug(F101,"conchk","",cn);
 
    if (cn < 1) return(0);

#ifdef datageneral
    /* We must be careful to just print out one result for each character
     * read.  The flag, conint_avl, controls duplication of characters.
     * Only one character is handled at a time, which is a reasonable
     * limit.  More complicated schemes could handle a buffer.
     */
    if (con_reads_mt) {
	if ((ch = conint_ch) <= 0) return(0);   /* I/O error, or no data */
	else if (conint_avl == 0) return(0);    /* Char already read */
	else conint_avl = 0;                    /* Flag char as read */
    }
    else { if ((ch = coninc(5)) < 0) return(0);  }
#else
    if ((ch = coninc(5)) < 0) return(0);
#endif /* datageneral */
    switch (ch & 0177) {
      case 'A': case 'a': case 0001:	/* Status report */
	screen(SCR_TN,0,0l,"Status report:");
	screen(SCR_TN,0,0l," file type: ");
	if (binary) {
#ifdef VMS
	    if (binary == XYFT_I)
	      screen(SCR_TZ,0,0l,"image");
	    else if (binary == XYFT_L)
	      screen(SCR_TZ,0,0l,"labeled");
	    else screen(SCR_TZ,0,0l,"binary");
#else
	    screen(SCR_TZ,0,0l,"binary");
#endif /* VMS */
	} else {
	    screen(SCR_TZ,0,0l,"text");
	}
	screen(SCR_QE,0,filcnt," file number");
	if (fsize) screen(SCR_QE,0,fsize," size");
	screen(SCR_QE,0,ffc,   " characters so far");
	if (fsize) {
	    zz = ( ffc * 100L ) / fsize;
	    screen(SCR_QE,0,zz,      " percent done");
	}
	screen(SCR_QE,0,(long)bctu,  " block check");
	screen(SCR_QE,0,(long)rptflg," compression");
	screen(SCR_QE,0,(long)ebqflg," 8th-bit prefixing");
	screen(SCR_QE,0,(long)lscapu," locking shifts");
	if (!network)
	  screen(SCR_QE,0, speed, " speed");
	if (what == W_SEND)
	  screen(SCR_QE,0,(long)spsiz, " packet length");
	else if (what == W_RECV || what == W_REMO)
	  screen(SCR_QE,0,(long)urpsiz," packet length");
	screen(SCR_QE,0,(long)wslots,  " window slots");
	return(0);

      case 'B': case 'b': case 0002:	/* Cancel batch */
      case 'Z': case 'z': case 0032:
	screen(SCR_TN,0,0l,"Cancelling Batch ");
	czseen = 1;
	return(0);

      case 'F': case 'f': case 0006:	/* Cancel file */
      case 'X': case 'x': case 0030:
	screen(SCR_TN,0,0l,"Cancelling File ");
	cxseen = 1;
	return(0);

      case 'R': case 'r': case 0022:	/* Resend */
      case 0015: case 0012:
	screen(SCR_TN,0,0l,"Resending packet ");
	resend(winlo);
	return(1);

      case 'E': case 'e':		/* Send error packet */
      case 0005:
	return(-1);

      default:				/* Anything else, print message */
	intmsg(1);
	return(0);
    }
}

/*  I N T M S G  --  Issue message about terminal interrupts  */
 
VOID
intmsg(n) long n; {
    char buf[80];

    if (!displa || quiet)		/* Not if we're being quiet */
      return;
    if (server && (!srvdis || n > -1L))	/* Special for server */
      return;
    buf[0] = NUL;			/* Keep compilers happy */
#ifdef ATTSV
    conchk();				/* Clear out pending escape-signals */
#endif /* ATTSV */
#ifdef VMS
    conres();				/* So Ctrl-C will work */
#endif /* VMS */
    if ((!server && n == 1L) || (server && n < 0L)) {

#ifdef ATTSV				/* We need to signal before kb input */
#ifndef aegis
#ifndef datageneral
	sprintf(buf,"Type escape character (%s) followed by:",dbchr(escape));
	screen(SCR_TN,0,0l,buf);
#endif /* datageneral */
#endif /* aegis */
#endif /* ATTSV */

 screen(SCR_TN,0,0l,"X to cancel file,  CR to resend current packet");
 screen(SCR_TN,0,0l,"Z to cancel group, A for status report");
 screen(SCR_TN,0,0l,"E to send Error packet, Ctrl-C to quit immediately: ");
/* if (server) */ screen(SCR_TN,0,0l,"");
    }
    else screen(SCR_TU,0,0l," ");
}

/*  S C R E E N  --  Screen display function  */
 
/*  screen(f,c,n,s)
      f - argument descriptor
      c - a character or small integer
      n - a long integer
      s - a string.
 Fill in this routine with the appropriate display update for the system.
 This version is for a dumb tty.
*/
VOID
#ifdef CK_ANSIC
screen(int f, char c,long n,char *s)
#else
screen(f,c,n,s) int f; char c; long n; char *s;
#endif /* CK_ANSIC */
/* screen */ {
    static int p = 0;			/* Screen position */
    int len;				/* Length of string */
    char buf[80];			/* Output buffer */
    len = (int)strlen(s);		/* Length of string */
    if ((f != SCR_WM) && (f != SCR_EM)) /* Always update warning & errors */
      if (!displa || quiet || backgrd || (server && !srvdis))
	return;
 
    switch (f) {
 
case SCR_FN:    			/* filename */
    conoll(""); conol(s); conoc(SP); p = len + 1; return;
 
case SCR_AN:    			/* as-name */
    if (p + len > 75) { conoll(""); p = 0; }
    conol("=> "); conol(s); if ((p += (len + 3)) > 78) { conoll(""); p = 0; }
    return;
 
case SCR_FS: 				/* file-size */
    sprintf(buf,", Size: %ld",n);  conoll(buf);  p = 0; return;
 
case SCR_XD:    			/* x-packet data */
    conoll(""); conoll(s); p = 0; return;
    
case SCR_ST:      			/* File status */
    switch (c) {
	case ST_OK:   	   		/*  Transferred OK */
	    if ((p += 5) > 78) { conoll(""); p = 0; }
	    conoll(" [OK]"); p += 5; return;
 
	case ST_DISC: 			/*  Discarded */
	    if ((p += 12) > 78) { conoll(""); p = 0; }
	    conoll(" [discarded]"); p += 12; return;
 
	case ST_INT:       		/*  Interrupted */
	    if ((p += 14) > 78) { conoll(""); p = 0; }
	    conoll(" [interrupted]"); p += 14; return;
 
	case ST_SKIP: 			/*  Skipped */
	    conoll("");
	    conol("Skipping "); conoll(s); p = 0;
	    return;
 
        case ST_ERR:
	    conoll("");
	    conol("Error "); conoll(s); p = 0;
	    return;

	case ST_REFU:
	    conoll("");
	    conol("Refused: "); conoll(s); p = 0;
	    return;

        default:
	    conoll("*** screen() called with bad status ***"); p = 0; return;
    }

case SCR_PN:    			/* Packet number */
    sprintf(buf,"%s: %ld",s,n); conol(buf); p += (int)strlen(buf); return;
 
case SCR_PT:    			/* Packet type or pseudotype */
    if (c == 'Y') return;		/* Don't bother with ACKs */
    if (c == 'D') {			/* Only show every 4th data packet */
	if (n % 4) return;
	c = '.';
    }
#ifndef AMIGA
    if (p++ > 77) {			/* If near right margin, */
	conoll("");			/* Start new line */
	p = 0;				/* and reset counter. */
    }
#endif /* AMIGA */
    conoc(c);				/* Display the character. */
#ifdef AMIGA
    if (c == 'G') conoll("");           /* new line after G packets */
#endif /* AMIGA */
    return;
 
case SCR_TC:    			/* transaction complete */
    conoc(BEL); return;
 
case SCR_EM:				/* Error message */
    conoll(""); conoc('?'); conoll(s); p = 0; return;		/* +1	*/
 
case SCR_WM:				/* Warning message */
    conoll(""); conoll(s); p = 0; return;
 
case SCR_TU:				/* Undelimited text */
    if ((p += len) > 77) { conoll(""); p = len; }
    conol(s); return;
 
case SCR_TN:				/* Text delimited at beginning */
    conoll(""); conol(s); p = len; return;
 
case SCR_TZ:				/* Text delimited at end */
    if ((p += len) > 77) { conoll(""); p = len; }
    conoll(s); return;
    
case SCR_QE:				/* Quantity equals */
    sprintf(buf,"%s: %ld",s,n);
    conoll(buf); p = 0; return;
 
default:
    conoll("*** screen() called with bad object ***"); p = 0; return;
    }
}

/*  E R M S G  --  Nonfatal error message  */

/* Should be used only for printing the message text from an Error packet. */

VOID
ermsg(msg) char *msg; {			/* Print error message */
    if (local && !quiet) {
#ifdef OSK
	printf("\n%s %s\n","Protocol Error:",msg);
#else
	printf("\r\n%s %s\r\n","Protocol Error:",msg);
#endif /* OSK */
    }
    tlog(F110,"Protocol Error:",msg,0L);
}

VOID
doclean() {				/* General cleanup upon exit */
#ifndef NOICP
#ifndef NOSPL
    extern struct mtab *mactab;		/* For ON_EXIT macro. */
    extern int nmac;
#endif /* NOSPL */
#endif /* NOICP */

#ifdef DEBUG
    if (deblog) {			/* Close any open logs. */
	debug(F100,"Debug Log Closed","",0);
	*debfil = '\0';
	deblog = 0;
	zclose(ZDFILE);
    }
#endif /* DEBUG */
    if (pktlog) {
	*pktfil = '\0';
	pktlog = 0;
	zclose(ZPFILE);
    }
    if (seslog) {
    	*sesfil = '\0';
	seslog = 0;
	zclose(ZSFILE);
    }
#ifdef TLOG
    if (tralog) {
	tlog(F100,"Transaction Log Closed","",0L);
	*trafil = '\0';
	tralog = 0;
	zclose(ZTFILE);
    }
#endif /* TLOG */

#ifndef NOICP
#ifndef NOSPL
    zclose(ZRFILE);			/* READ and WRITE files, if any. */
    zclose(ZWFILE);
/*
  If a macro named "on_exit" is defined, execute it.  Also remove it from the
  macro table, in case its definition includes an EXIT or QUIT command, which
  would cause much recursion and would prevent the program from ever actually
  EXITing.
*/
    if (nmac) {				/* Any macros defined? */
	int k;				/* Yes */
	k = mlook(mactab,"on_exit",nmac); /* Look up "on_exit" */
	if (k >= 0) {			/* If found, */
	    *(mactab[k].kwd) = NUL;	/* poke its name from the table, */
	    if (dodo(k,"") > -1)	/* set it up, */
	      parser(1);		/* and execute it */
        }
    }
#endif /* NOSPL */
#endif /* NOICP */

/*
  Put console terminal back to normal.  This is done here because the
  ON_EXIT macro calls the parser, which meddles with console terminal modes.
*/
    ttclos(0);				/* Close external line, if any */    
    if (local) {
	strcpy(ttname,dftty);		/* Restore default tty */
	local = dfloc;			/* And default remote/local status */
    }
    conres();				/* Restore console terminal. */

#ifdef COMMENT
/* Should be no need for this, and maybe it's screwing this up? */
    connoi();				/* Turn off console interrupt traps */
#endif /* COMMENT */

    syscleanup();			/* System-dependent cleanup, last */
}

/*  D O E X I T  --  Exit from the program.  */

/*
  First arg is general, system-independent symbol: GOOD_EXIT or BAD_EXIT.
  If second arg is -1, take 1st arg literally.
  If second arg is not -1, work it into the exit code.
*/
VOID
doexit(exitstat,what) int exitstat, what; {
#ifdef VMS
    char envstr[64];
    static $DESCRIPTOR(symnam,"CKERMIT_STATUS");
    static struct dsc$descriptor_s symval;
    int i;
#endif /* VMS */

    debug(F101,"doexit exitstat","",exitstat);
    debug(F101,"doexit what","",what);

    doclean();				/* First, clean up everything */

#ifdef VMS
    if (what == -1)
	what = 0;			/* Since we set two different items */
    sprintf(envstr,"%d", exitstat | what);
    symval.dsc$w_length = (int)strlen(envstr);
    symval.dsc$a_pointer = envstr;
    symval.dsc$b_class = DSC$K_CLASS_S;
    symval.dsc$b_dtype = DSC$K_DTYPE_T;
    i = 2;				/* Store in global table */
    LIB$SET_SYMBOL(&symnam, &symval, &i);
    if (exitstat == BAD_EXIT)
	exitstat = SS$_ABORT | STS$M_INHIB_MSG;
    if (exitstat == GOOD_EXIT)
	exitstat = SS$_NORMAL | STS$M_INHIB_MSG;
    exit(exitstat);
#else /* Not VMS */
    if (what == -1)			/* Take 1st arg literally */
      exit(exitstat);			/* e.g. user-supplied exit code */
    else				/* otherwise */
      exit(exitstat | what);		/* OR in the bits */
#endif /* VMS */
}

/* Set up interrupts */

VOID
setint() {
    conint(trap,stptrap);       /* Turn on console terminal interrupts. */
    bgchk();	    	        /* Check background status */
}

VOID
bgchk() {				/* Check background status */
    if (bgset < 0) pflag = !backgrd;
    else pflag = (bgset == 0 ? 1 : 0);
}

#ifdef DEBUG
/*  D E B U G  --  Enter a record in the debugging log  */
 
/*
 Call with a format, two strings, and a number:
   f  - Format, a bit string in range 0-7.
        If bit x is on, then argument number x is printed.
   s1 - String, argument number 1.  If selected, printed as is.
   s2 - String, argument number 2.  If selected, printed in brackets.
   n  - Int, argument 3.  If selected, printed preceded by equals sign.
 
   f=0 is special: print s1,s2, and interpret n as a char.
*/
#define DBUFL 2300
static char *dbptr = (char *)0;

int
dodebug(f,s1,s2,n) int f; char *s1, *s2; long n; {
    char *sp;
 
    if (!dbptr) {
	dbptr = malloc(DBUFL+1);
	if (!dbptr)
	  return(0);
    }
    sp = dbptr;
    if (!deblog) return(0);	/* If no debug log, don't. */
    switch (f) {
    	case F000:		/* 0, print both strings, and n as a char */
	    if ((int)strlen(s1) + (int)strlen(s2) + 5 > DBUFL) {
		sprintf(sp,"DEBUG string too long\n");
	    } else {
		if (n > 31 && n < 127)
		  sprintf(sp,"%s%s:%c\n",s1,s2,n);
		else if (n < 32 || n == 127)
		  sprintf(sp,"%s%s:^%c\n",s1,s2,(n+64) & 0x7F);
		else if (n > 127 && n < 160)
		  sprintf(sp,"%s%s:~^%c\n",s1,s2,(n-64) & 0x7F);
		else if (n > 159 && n < 256)
		  sprintf(sp,"%s%s:~%c\n",s1,s2,n & 0x7F);
		else sprintf(sp,"%s%s:%ld\n",s1,s2,n);
	    }
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
    	case F001:			/* 1, "=n" */
	    sprintf(sp,"=%ld\n",n);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
    	case F010:			/* 2, "[s2]" */
	    if ((int)strlen(s2) + 4 > DBUFL)
	      sprintf(sp,"DEBUG string too long\n");
	    else sprintf(sp,"[%s]\n",s2);
	    if (zsout(ZDFILE,"") < 0) deblog = 0;
	    break;
    	case F011:			/* 3, "[s2]=n" */
	    if ((int)strlen(s2) + 15 > DBUFL)
	      sprintf(sp,"DEBUG string too long\n");
	    else sprintf(sp,"[%s]=%ld\n",s2,n);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
    	case F100:			/* 4, "s1" */
	    if (zsoutl(ZDFILE,s1) < 0) deblog = 0;
	    break;
    	case F101:			/* 5, "s1=n" */
	    if ((int)strlen(s1) + 15 > DBUFL)
	      sprintf(sp,"DEBUG string too long\n");
	    else sprintf(sp,"%s=%ld\n",s1,n);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
    	case F110:			/* 6, "s1[s2]" */
	    if ((int)strlen(s1) + (int)strlen(s2) + 4 > DBUFL)
	      sprintf(sp,"DEBUG string too long\n");
	    else sprintf(sp,"%s[%s]\n",s1,s2);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
    	case F111:			/* 7, "s1[s2]=n" */
	    if ((int)strlen(s1) + (int)strlen(s2) + 15 > DBUFL)
	      sprintf(sp,"DEBUG string too long\n");
	    else sprintf(sp,"%s[%s]=%ld\n",s1,s2,n);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
	    break;
	default:
	    sprintf(sp,"\n?Invalid format for debug() - %d\n",f);
	    if (zsout(ZDFILE,dbptr) < 0) deblog = 0;
    }
    return(0);
}
#endif /* DEBUG */

#ifdef TLOG
#define TBUFL 300
/*  T L O G  --  Log a record in the transaction file  */
/*
 Call with a format and 3 arguments: two strings and a number:
   f  - Format, a bit string in range 0-7, bit x is on, arg #x is printed.
   s1,s2 - String arguments 1 and 2.
   n  - Int, argument 3.
*/
VOID
tlog(f,s1,s2,n) int f; long n; char *s1, *s2; {
    static char s[TBUFL];
    char *sp = s; int x;
    
    if (!tralog) return;		/* If no transaction log, don't */
    switch (f) {
    	case F000:			/* 0 (special) "s1 n s2"  */
	    if ((int)strlen(s1) + (int)strlen(s2) + 15 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"%s %ld %s\n",s1,n,s2);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
    	case F001:			/* 1, " n" */
	    sprintf(sp," %ld\n",n);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
    	case F010:			/* 2, "[s2]" */
	    x = (int)strlen(s2);
	    if (s2[x] == '\n') s2[x] = '\0';
	    if (x + 6 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"[%s]\n",s2);
	    if (zsout(ZTFILE,"") < 0) tralog = 0;
	    break;
    	case F011:			/* 3, "[s2] n" */
	    x = (int)strlen(s2);
	    if (s2[x] == '\n') s2[x] = '\0';
	    if (x + 6 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"[%s] %ld\n",s2,n);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
    	case F100:			/* 4, "s1" */
	    if (zsoutl(ZTFILE,s1) < 0) tralog = 0;
	    break;
    	case F101:			/* 5, "s1: n" */
	    if ((int)strlen(s1) + 15 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"%s: %ld\n",s1,n);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
    	case F110:			/* 6, "s1 s2" */
	    x = (int)strlen(s2);
	    if (s2[x] == '\n') s2[x] = '\0';
	    if ((int)strlen(s1) + x + 4 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"%s %s\n",s1,s2);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
    	case F111:			/* 7, "s1 s2: n" */
	    x = (int)strlen(s2);
	    if (s2[x] == '\n') s2[x] = '\0';
	    if ((int)strlen(s1) + x + 15 > TBUFL)
	      sprintf(sp,"?T-Log string too long\n");
	    else sprintf(sp,"%s %s: %ld\n",s1,s2,n);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
	    break;
	default:
	    sprintf(sp,"\n?Invalid format for tlog() - %ld\n",n);
	    if (zsout(ZTFILE,s) < 0) tralog = 0;
    }
}
#endif /* TLOG */
#endif /* MAC */
