char *fnsv = "C-Kermit functions, 4C(047) 31 Jul 85";

/*  C K C F N S  --  System-independent Kermit protocol support functions.  */

/*  ...Part 1 (others moved to ckcfn2 to make this module small enough) */
/*
 Author: Frank da Cruz (SY.FDC@CU20B),
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 
*/
/*
 System-dependent primitives defined in:

   ck?tio.c -- terminal i/o
   cx?fio.c -- file i/o, directory structure
*/
#include "ckcker.h"			/* Symbol definitions for Kermit */
#include "ckcdeb.h"			/* Debug formats, typedefs, etc. */

#ifndef NULL
#define NULL 0
#endif

/* Externals from ckcmai.c */
extern int spsiz, rpsiz, timint, rtimo, npad, chklen, ebq, ebqflg, rpt, rptq,
 rptflg, capas, keep;
extern int pktnum, prvpkt, sndtyp, bctr, bctu,
 size, osize, maxsize, spktl, nfils, stdouf, warn, timef, spsizf;
extern int parity, speed, turn, turnch, 
 delay, displa, pktlog, tralog, seslog, xflg, mypadn;
extern long filcnt, ffc, flci, flco, tlci, tlco, tfc, tsecs, fsize;
extern int deblog, hcflg, binary, savmod, fncnv, local, server, cxseen, czseen;
extern CHAR padch, mypadc, eol, ctlq, myctlq, sstate;
extern CHAR filnam[], sndpkt[], recpkt[], data[], srvcmd[], stchr, mystch;
extern char *cmarg, *cmarg2, *hlptxt, **cmlist;
extern CHAR *srvptr;
long zchki();
char *strcpy();

/* Variables local to this module */

static char *memptr;			/* Pointer for memory strings */

static char cmdstr[100];		/* Unix system command string */

static int  sndsrc;			/* Flag for where to send from: */
					/* -1: name in cmdata */
					/*  0: stdin          */
					/* >0: list in cmlist */

static int  memstr,			/* Flag for input from memory string */
    first;				/* Flag for first char from input */
static CHAR t,				/* Current character */
    next;				/* Next character */

/*  E N C S T R  --  Encode a string from memory. */

/*  Call this instead of getpkt() if source is a string, rather than a file. */

encstr(s) char* s; {
    int m; char *p;

    m = memstr; p = memptr;		/* Save these. */

    memptr = s;				/* Point to the string. */
    memstr = 1;				/* Flag memory string as source. */
    first = 1;				/* Initialize character lookahead. */
    getpkt(spsiz);			/* Fill a packet from the string. */
    memstr = m;				/* Restore memory string flag */
    memptr = p;				/* and pointer */
    first = 1;				/* Put this back as we found it. */
}

/* E N C O D E - Kermit packet encoding procedure */

encode(a) CHAR a; {			/* The current character */
    int a7;				/* Low order 7 bits of character */
    int b8;				/* 8th bit of character */

    if (rptflg)	{   	    		/* Repeat processing? */
        if (a == next && (first == 0)) { /* Got a run... */
	    if (++rpt < 94)		/* Below max, just count */
                return;
	    else if (rpt == 94) {	/* Reached max, must dump */
                data[size++] = rptq;
                data[size++] = tochar(rpt);
                rpt = 0;
	    }
        } else if (rpt == 1) {		/* Run broken, only 2? */
            rpt = 0;			/* Yes, reset repeat flag & count. */
	    encode(a);			/* Do the character twice. */
	    if (size <= maxsize) osize = size;
	    rpt = 0;
	    encode(a);
	    return;
	} else if (rpt > 1) {		/* More than two */
            data[size++] = rptq;	/* Insert the repeat prefix */
            data[size++] = tochar(++rpt); /* and count. */
            rpt = 0;			/* Reset repeat counter. */
        }
    }
    a7 = a & 0177;			/* Isolate ASCII part */
    b8 = a & 0200;			/* and 8th (parity) bit. */

    if (ebqflg && b8) {			/* Do 8th bit prefix if necessary. */
        data[size++] = ebq;
        a = a7;
    }
    if ((a7 < SP) || (a7==DEL))	{	/* Do control prefix if necessary */
        data[size++] = myctlq;
	a = ctl(a);
    }
    if (a7 == myctlq)			/* Prefix the control prefix */
        data[size++] = myctlq;

    if ((rptflg) && (a7 == rptq))	/* If it's the repeat prefix, */
        data[size++] = myctlq;		/* quote it if doing repeat counts. */

    if ((ebqflg) && (a7 == ebq))	/* Prefix the 8th bit prefix */
        data[size++] = myctlq;		/* if doing 8th-bit prefixes */

    data[size++] = a;			/* Finally, insert the character */
    data[size] = '\0';			/* itself, and mark the end. */
}

/* D E C O D E  --  Kermit packet decoding procedure */

/* Call with string to be decoded and an output function. */
/* Returns 0 on success, -1 on failure (e.g. disk full).  */

decode(buf,fn) char *buf; int (*fn)(); {
    unsigned int a, a7, b8;		/* Low order 7 bits, and the 8th bit */

    rpt = 0;				/* Initialize repeat count. */

    while ((a = *buf++) != '\0') {
	if (rptflg) {			/* Repeat processing? */
	    if (a == rptq) {		/* Yes, got a repeat prefix? */
		rpt = unchar(*buf++);	/* Yes, get the repeat count, */
		a = *buf++;		/* and get the prefixed character. */
	    }
	}
	b8 = 0;				/* Check high order "8th" bit */
	if (ebqflg) {			/* 8th-bit prefixing? */
	    if (a == ebq) {		/* Yes, got an 8th-bit prefix? */
		b8 = 0200;		/* Yes, remember this, */
		a = *buf++;		/* and get the prefixed character. */
	    }
	}
	if (a == ctlq) {		/* If control prefix, */
	    a  = *buf++;		/* get its operand. */
	    a7 = a & 0177;		/* Only look at low 7 bits. */
	    if ((a7 >= 0100 && a7 <= 0137) || a7 == '?') /* Uncontrollify */
	    a = ctl(a);			/* if in control range. */
	}
	a |= b8;			/* OR in the 8th bit */
	if (rpt == 0) rpt = 1;		/* If no repeats, then one */
#ifdef NLCHAR
	if (!binary) {			/* If in text mode, */
	    if (a == CR) continue;	/* discard carriage returns, */
    	    if (a == LF) a = NLCHAR; 	/* convert LF to system's newline. */
    	}
#endif
	for (; rpt > 0; rpt--) {	/* Output the char RPT times */
	    ffc++, tfc++;		/* Count the character */
	    if ((*fn)(a) < 0) return(-1); /* Send it to the output function. */
	}
    }
    return(0);
}


/*  Output functions passed to 'decode':  */

putsrv(c) char c; { 	/*  Put character in server command buffer  */
    *srvptr++ = c;
    *srvptr = '\0';	/* Make sure buffer is null-terminated */
    return(0);
}

puttrm(c) char c; {     /*  Output character to console.  */
    conoc(c);
    return(0);
}

putfil(c) char c; {			/*  Output char to file. */
    if (zchout(ZOFILE,c) < 0) {
	czseen = 1;   			/* If write error... */
	debug(F101,"putfil zchout write error, setting czseen","",1);
	return(-1);
    }
    return(0);
}

/*  G E T P K T -- Fill a packet data field  */

/*
 Gets characters from the current source -- file or memory string.
 Encodes the data into the packet, filling the packet optimally.
 Set first = 1 when calling for the first time on a given input stream
 (string or file).

 Uses global variables:
 t     -- current character.
 first -- flag: 1 to start up, 0 for input in progress, -1 for EOF.
 next  -- next character.
 data  -- the packet data buffer.
 size  -- number of characters in the data buffer.

Returns the size as value of the function, and also sets global size,
and fills (and null-terminates) the global data array.  Returns 0 upon eof.
*/

getpkt(maxsize) int maxsize; {		/* Fill one packet buffer */
    int i, x;				/* Loop index. */
	
    static char leftover[6] = { '\0', '\0', '\0', '\0', '\0', '\0' };

    if (first == 1) {		/* If first time thru...  */
	first = 0;		/* remember, */
	*leftover = '\0';   	/* discard any interrupted leftovers, */
	x = getchx(&t);		/* get first character of file into t, */
	if (x == 0) {	    	/* watching out for null file, */
	    first = 1;
	    return(size = 0);	
	}
    } else if (first == -1) {	/* EOF from last time? */
	first = 1;		/* Setup for next time. */
        return(size = 0);
    } else x = 1;

    /* Do any leftovers */

    for (size = 0; (data[size] = leftover[size]) != '\0'; size++)
    	;
    *leftover = '\0';

    /* Now fill up the rest of the packet. */

    rpt = 0;				/* Clear out any old repeat count. */
    while (x > 0) {			/* Until EOF... */
	x = getchx(&next);		/* Get next character for lookahead. */
	if (x == 0) first = -1;		/* Flag eof for next time. */
	osize = size;			/* Remember current position. */
        encode(t);			/* Encode the current character. */
	t = next;			/* Next is now current. */

	if (size == maxsize) { 		/* If the packet is exactly full, */
	    debug(F101,"getpkt exact fit","",size);
            return(size);		/* ... return. */
	}
	if (size > maxsize) {		/* If too big, save some for next. */
	    for (i = 0; (leftover[i] = data[osize+i]) != '\0'; i++)
	    	;
	    debug(F111,"getpkt leftover",leftover,size);
	    debug(F101," osize","",osize);
	    size = osize;		/* Return truncated packet. */
	    data[size] = '\0';
	    return(size);
	}
    }					/* Otherwise, keep filling. */
    debug(F111,"getpkt eof/eot",data,size); /* Fell thru before packet full, */
    return(size);		   /* return partially filled last packet. */
}

/*  G E T C H X  --  Get the next character from file (or pipe). */
 
/*
 On systems like Unix, the Macintosh, etc, that use a single character
 (NLCHAR, defined in ckcdeb.h) to separate lines in text files, and
 when in text/ascii mode (binary == 0), this function maps the newline
 character to CRLF.  If NLCHAR is not defined, then this mapping is
 not done, even in text mode.

 Returns 1 on success with ch set to the character, or 0 on failure (EOF)
*/
getchx(ch) char *ch; {			/* Get next character */
    int x; CHAR a;			/* The character to return. */
    static int b = 0;			/* A character to remember. */
    
    if (b > 0) {			/* Do we have a LF saved? */
	b = 0;				/* Yes, return that. */
	*ch = LF;
	return(1);
    }

    if (memstr)				/* Try to get the next character */
    	x = ((a = *memptr++) == '\0');	/* from the appropriate source, */
    else				/* memory or the current file. */
    	x = (zchin(ZIFILE,&a) == -1);

    if (x)
    	return(0);			/* No more, return 0 for EOF. */
    else {				/* Otherwise, read the next char. */
	ffc++, tfc++;			/* Count it. */
#ifdef NLCHAR
	if (!binary && (a == NLCHAR)) {	/* If nl and we must do nl-CRLF */
	    b = 1;			/* mapping, remember a linefeed, */
	    *ch = CR;			/* and return a carriage return. */
	    return(1);
	} else {
	    *ch = a;			/*  General case, return the char. */
	    return(1);	
        }
#else
        *ch = a;
        return(1);	
#endif
    }
}


/*  C A N N E D  --  Check if current file transfer cancelled */

canned(buf) char *buf; {
    if (*buf == 'X') cxseen = 1;
    if (*buf == 'Z') czseen = 1;
    debug(F101,"canned: cxseen","",cxseen);
    debug(F101," czseen","",czseen);
    return((czseen || cxseen) ? 1 : 0);
}

/*  T I N I T  --  Initialize a transaction  */

tinit() {
    xflg = 0;				/* Reset x-packet flag */
    memstr = 0;				/* Reset memory-string flag */
    memptr = NULL;			/*  and pointer */
    bctu = 1;				/* Reset block check type to 1 */
    ebq = ebqflg = 0;			/* Reset 8th-bit quoting stuff */
    if (savmod) {			/* If binary file mode was saved, */
    	binary = 1;			/*  restore it, */
	savmod = 0;			/*  unsave it. */
    }
    filcnt = 0;				/* Reset file counter */
    tfc = tlci = tlco = 0;		/* Reset character counters */
    prvpkt = -1;			/* Reset packet number */
    pktnum = 0;
    cxseen = czseen = 0;		/* Reset interrupt flags */
    *filnam = '\0';			/* Clear file name */
    if (server) {			/* If acting as server, */
	timint = 30;			/* Use 30 second timeout, */
	nack();				/* Send first NAK */
    }
}


/*  R I N I T  --  Respond to S packet  */

rinit(d) char *d; {
    char *tp;
    ztime(&tp);
    tlog(F110,"Transaction begins",tp,0l); /* Make transaction log entry */
    tfc = tlci = tlco = 0;
    spar(d);
    rpar(d);
    ack1(d);
}

/*  S I N I T  --  Make sure file exists, then send Send-Init packet */

sinit() {
    int x; char *tp;

    sndsrc = nfils;			/* Where to look for files to send */
    ztime(&tp);
    tlog(F110,"Transaction begins",tp,0l); /* Make transaction log entry */
    debug(F101,"sinit: sndsrc","",sndsrc);
    if (sndsrc < 0) {			/* Must expand from 'send' command */
	nfils = zxpand(cmarg);		/* Look up literal name. */
	if (nfils < 0) {
	    screen(SCR_EM,0,0l,"Too many files");
	    return(0);
        } else if (nfils == 0) {	/* If none found, */
	    char xname[100];		/* convert the name. */
	    zrtol(cmarg,xname);
	    nfils = zxpand(xname); 	/* Look it up again. */
	}
	if (nfils < 1) {		/* If no match, report error. */
	    if (server) 
	    	errpkt("File not found");
	    else
		screen(SCR_EM,0,0l,"File not found");
	    return(0);
	}
	x = gnfile();			/* Position to first file. */
	if (x < 1) {
	    if (!server) 
	    	screen(SCR_EM,0,0l,"No readable file to send");
            else
	    	errpkt("No readable file to send");
	    return(0);
    	} 
    } else if (sndsrc > 0) {		/* Command line arglist -- */
	x = gnfile();			/* Get the first file from it. */
	if (x < 1) return(0);		/* (if any) */
    } else if (sndsrc == 0) {		/* stdin or memory always exist... */
	if ((cmarg2 != NULL) && (*cmarg2)) {
	    strcpy(filnam,cmarg2);	/* If F packet, "as" name is used */
	    cmarg2 = "";		/* if provided, */
        } else				/* otherwise */
	    strcpy(filnam,"stdin");	/* just use this. */
    }
    debug(F101,"sinit: nfils","",nfils);
    debug(F110," filnam",filnam,0);
    debug(F110," cmdstr",cmdstr,0);
    ttflui();				/* Flush input buffer. */
    if (!local && !server) sleep(delay);
    sipkt('S');				/* Send the Send-Init packet. */
    return(1);
}
sipkt(c) char c; {			/* Send S or I packet. */
    int x;
    ttflui();				/* Flush pending input. */
    x = rpar(data);			/* Send an I-Packet. */
    spack(c,pktnum,x,data);
}

/*  R C V F I L -- Receive a file  */

rcvfil() {
    int x;
    ffc = flci = flco = 0;		/* Init per-file counters */
    srvptr = srvcmd;			/* Decode file name from packet. */
    decode(data,putsrv);
    if (*srvcmd == '\0')		/* Watch out for null F packet. */
    	strcpy(srvcmd,"NONAME");
    screen(SCR_FN,0,0l,srvcmd);		/* Put it on screen */
    tlog(F110,"Receiving",srvcmd,0l);	/* Transaction log entry */
    if (cmarg2 != NULL) {               /* Check for alternate name */
        if (*cmarg2 != '\0') {
            strcpy(srvcmd,cmarg2);	/* Got one, use it. */
	    *cmarg2 = '\0';
        }
    }
    x = openo(srvcmd,filnam);		/* Try to open it */
    if (x) {
	tlog(F110," as",filnam,0l);
	screen(SCR_AN,0,0l,filnam);
	intmsg(++filcnt);
    } else {
        tlog(F110,"Failure to open",filnam,0l);
	screen(SCR_EM,0,0l,"Can't open file");
    }
    return(x);				/* Pass on return code from openo */
}

/*  R E O F  --  Receive End Of File  */

reof() {

    if (cxseen == 0) cxseen = (*data == 'D');	/* Got discard directive? */
    clsof(cxseen | czseen);
    if (cxseen || czseen) {
	tlog(F100," *** Discarding","",0l);
    } else
	fstats();
}


/*  R E O T  --  Receive End Of Transaction  */

reot() {
    cxseen = czseen = 0;		/* Reset interruption flags */
    tstats();
}

/*  S F I L E -- Send File header or teXt header packet  */

/*  Call with x nonzero for X packet, zero for F packet  */
/*  Returns 1 on success, 0 on failure                   */

sfile(x) int x; {
    char pktnam[100];			/* Local copy of name */
    char *s;

    if (x == 0) {			/* F-Packet setup */

    	if (*cmarg2 != '\0') {		/* If we have a send-as name, */
	    strcpy(pktnam,cmarg2);	/* copy it literally, */
	    cmarg2 = "";		/* and blank it out for next time. */
    	} else {			/* Otherwise use actual file name: */
	    if (fncnv) {		/* If converting names, */
	    	zltor(filnam,pktnam);	/* convert it to common form, */
	    } else {			/* otherwise, */
	    	strcpy(pktnam,filnam);	/* copy it literally. */
            }
    	}
    	debug(F110,"sfile",filnam,0);	/* Log debugging info */
    	debug(F110," pktnam",pktnam,0);
    	if (openi(filnam) == 0) 	/* Try to open the file */
	    return(0); 		
    	s = pktnam;			/* Name for packet data field */

    } else {				/* X-packet setup */

    	debug(F110,"sxpack",cmdstr,0);	/* Log debugging info */
    	s = cmdstr;			/* Name for data field */
    }

    flci = flco = ffc = 0;		/* Init counters, etc. */
    encstr(s);				/* Encode the name into data[]. */
    nxtpkt(&pktnum);			/* Increment the packet number */
    spack(x ? 'X' : 'F', pktnum, size, data); /* Send the F or X packet */

    if (x == 0) {			/* Display for F packet */
    	if (displa) {			/* Screen */
	    screen(SCR_FN,'F',(long)pktnum,filnam);
	    screen(SCR_AN,0,0l,pktnam);
	    screen(SCR_FS,0,(long)fsize,"");
    	}
    	tlog(F110,"Sending",filnam,0l);	/* Transaction log entry */
    	tlog(F110," as",pktnam,0l);

    } else {				/* Display for X-packet */

    	screen(SCR_XD,'X',(long)pktnum,cmdstr);	/* Screen */
    	tlog(F110,"Sending from:",cmdstr,0l);	/* Transaction log */
    }
    intmsg(++filcnt);			/* Count file, give interrupt msg */
    first = 1;				/* Init file character lookahead. */
    return(1);
}

/*  S D A T A -- Send a data packet */

/*  Return -1 if no data to send, else send packet and return length  */

sdata() {
    int len;
    if (cxseen || czseen) return(-1);	/* If interrupted, done. */
    if ((len = getpkt(spsiz-chklen-3)) == 0) /* Done if no data. */
    	return(-1);
    nxtpkt(&pktnum);			/* Increment the packet number */
    spack('D',pktnum,len,data);		/* Send the packet */
    return(len);
}


/*  S E O F -- Send an End-Of-File packet */

/*  Call with a string pointer to character to put in the data field, */
/*  or else a null pointer or "" for no data.  */

seof(s) char *s; {
    nxtpkt(&pktnum);			/* Increment the packet number */
    if ((s != NULL) && (*s != '\0')) {
	spack('Z',pktnum,1,s);
	tlog(F100," *** interrupted, sending discard request","",0l);
    } else {
	spack('Z',pktnum,0,"");
	fstats();
    }
}


/*  S E O T -- Send an End-Of-Transaction packet */

seot() {
    nxtpkt(&pktnum);			/* Increment the packet number */
    spack('B',pktnum,0,"");		/* Send the EOT packet */
    cxseen = czseen = 0;		/* Reset interruption flags */
    tstats();				/* Log timing info */
}

/*  F S T A T S  --  Record file statistics in transaction log  */

fstats() {
    tlog(F100," end of file","",0l);
    tlog(F101,"  file characters        ","",ffc);
    tlog(F101,"  communication line in  ","",flci);
    tlog(F101,"  communication line out ","",flco);
}


/*  T S T A T S  --  Record statistics in transaction log  */

tstats() {
    char *tp; int x;

    ztime(&tp);				/* Get time stamp */
    tlog(F110,"End of transaction",tp,0l);  /* Record it */

    if (filcnt < 1) return;		/* If no files, done. */

/* If multiple files, record character totals for all files */

    if (filcnt > 1) {
	tlog(F101," files","",filcnt);
	tlog(F101," total file characters   ","",tfc);
	tlog(F101," communication line in   ","",tlci);
	tlog(F101," communication line out  ","",tlco);
    }

/* Record timing info for one or more files */

    tlog(F101," elapsed time (seconds)  ","",tsecs);
    if (tsecs > 0) {
	x = (tfc / tsecs) * 10;
	tlog(F101," effective baud rate     ","",x);
	if (speed > 0) {
	    x = (x * 100) / speed;
	    tlog(F101," efficiency (percent)    ","",x);
	}
    }
    tlog(F100,"","",0);			/* Leave a blank line */
}

/*   R P A R -- Fill the data array with my send-init parameters  */

rpar(data) char data[]; {
    data[0] = tochar(rpsiz);		/* Biggest packet I can receive */
    data[1] = tochar(rtimo);		/* When I want to be timed out */
    data[2] = tochar(mypadn);		/* How much padding I need (none) */
    data[3] = ctl(mypadc);		/* Padding character I want */
    data[4] = tochar(eol);		/* End-Of-Line character I want */
    data[5] = CTLQ;			/* Control-Quote character I send */
    if (parity || ebqflg) {		/* 8-bit quoting... */
    	data[6] = '&';	    	    	    /* If parity or flag on, send &. */
	if ((ebq > 0040 && ebq < 0100) ||   /* If flag off, then turn it on  */
	    (ebq > 0140 && ebq < 0177) ||   /* if other side has asked us to */
	    (ebq == 'Y')) ebqflg = 1;
    } else {				    /* Normally, */
	data[6] = 'Y';			    /* just say we're willing. */
    }
    data[7] = bctr + '0';		/* Block check type */
    data[8] = MYRPTQ;			/* Do repeat counts */
    data[9] = '\0';
    return(9);				/* Return the length. */
}

/*   S P A R -- Get the other system's Send-Init parameters.  */

spar(data) char data[]; {
    int len, x;

    len = strlen(data);		    	/* Number of fields */

    x = (len-- > 0) ? unchar(data[0]) : DSPSIZ;	        /* Packet size */
    if (spsizf == 0)
	spsiz = (x < 10) ? DSPSIZ : x;

    x = (len-- > 0) ? unchar(data[1]) : DMYTIM;		/* Timeout */
    if (timef == 0)
	timint = (x < 0) ? DMYTIM : x;

    npad = 0; padch = '\0';		    	    	/* Padding */
    if (len-- > 0) {
	npad = unchar(data[2]);
	if (len-- > 0) padch = ctl(data[3]); else padch = 0;
    }

    eol = (len-- > 0) ? unchar(data[4]) : '\r';	    	/* Terminator  */
    if ((eol < 2) || (eol > 037)) eol = '\r';

    ctlq = (len-- > 0) ? data[5] : CTLQ;    	    	/* Control prefix */

    if (len-- > 0) {			    	    	/* 8th-bit prefix */
	ebq = data[6];
	if ((ebq > 040 && ebq < 0100) || (ebq > 0140 && ebq < 0177)) {
	    ebqflg = 1;
	} else if ((parity || ebqflg) && (ebq == 'Y')) {
	    ebqflg = 1;
	    ebq = '&';
	} else if (ebq == 'N') {
	    ebqflg = 0;
	} else ebqflg = 0;
    } else ebqflg = 0;

    chklen = 1;				    	    	/* Block check */
    if (len-- > 0) {
	chklen = data[7] - '0';
	if ((chklen < 1) || (chklen > 3)) chklen = 1;
    }
    bctr = chklen;

    if (len-- > 0) {			    	    	/* Repeat prefix */
	rptq = data[8]; 
	rptflg = ((rptq > 040 && rptq < 0100) || (rptq > 0140 && rptq < 0177));
    } else rptflg = 0;

    if (deblog) sdebu(len);
}

/*  S D E B U  -- Record spar results in debugging log  */

sdebu(len) int len; {
    debug(F111,"spar: data",data,len);
    debug(F101," spsiz ","",spsiz);
    debug(F101," timint","",timint);
    debug(F101," npad  ","",npad);
    debug(F101," padch ","",padch);
    debug(F101," eol   ","",eol);
    debug(F101," ctlq  ","",ctlq);
    debug(F101," ebq   ","",ebq);
    debug(F101," ebqflg","",ebqflg);
    debug(F101," chklen","",chklen);
    debug(F101," rptq  ","",rptq);
    debug(F101," rptflg","",rptflg);
}

/*  G N F I L E  --  Get the next file name from a file group.  */

/*  Returns 1 if there's a next file, 0 otherwise  */

gnfile() {
    int x; long y;

/* If file group interruption (C-Z) occured, fail.  */

    debug(F101,"gnfile: czseen","",czseen);

    if (czseen) {
	tlog(F100,"Transaction cancelled","",0l);
	return(0);
    }

/* If input was stdin or memory string, there is no next file.  */

    if (sndsrc == 0) return(0);

/* If file list comes from command line args, get the next list element. */

    y = -1;
    while (y < 0) {			/* Keep trying till we get one... */

	if (sndsrc > 0) {
	    if (nfils-- > 0) {
		strcpy(filnam,*cmlist++);
		debug(F111,"gnfile: cmlist filnam",filnam,nfils);
	    } else {
		*filnam = '\0';
		debug(F101,"gnfile cmlist: nfils","",nfils);
		return(0);
	    }
	}

/* Otherwise, step to next element of internal wildcard expansion list. */

	if (sndsrc < 0) {
	    x = znext(filnam);
	    debug(F111,"gnfile znext: filnam",filnam,x);
	    if (x == 0) return(0);
	}

/* Get here with a filename. */

	y = zchki(filnam);		/* Check if file readable */
	if (y < 0) {
	    debug(F110,"gnfile skipping:",filnam,0);
	    tlog(F111,filnam,"not sent, reason",(long)y);
	    screen(SCR_ST,ST_SKIP,0l,filnam);
	} else fsize = y;
    }    	
    return(1);
}

/*  O P E N I  --  Open an existing file for input  */

openi(name) char *name; {
    int x, filno;
    if (memstr) return(1);		/* Just return if file is memory. */

    debug(F110,"openi",name,0);
    debug(F101," sndsrc","",sndsrc);

    filno = (sndsrc == 0) ? ZSTDIO : ZIFILE;    /* ... */

    debug(F101," file number","",filno);

    if (zopeni(filno,name)) {		/* Otherwise, try to open it. */
	debug(F110," ok",name,0);
    	return(1);
    } else {				/* If not found, */
	char xname[100];		/* convert the name */
	zrtol(name,xname);		/* to local form and then */
	x = zopeni(filno,xname);	/* try opening it again. */
	debug(F101," zopeni","",x);
	if (x) {
	    debug(F110," ok",xname,0);
	    return(1);			/* It worked. */
        } else {
	    screen(SCR_EM,0,0l,"Can't open file");  /* It didn't work. */
	    tlog(F110,xname,"could not be opened",0l);
	    debug(F110," openi failed",xname,0);
	    return(0);
        }
    }
}

/*  O P E N O  --  Open a new file for output.  */

/*  Returns actual name under which the file was opened in string 'name2'. */

openo(name,name2) char *name, *name2; {
    char xname[100], *xp;

    if (stdouf)				/* Receiving to stdout? */
	return(zopeno(ZSTDIO,""));

    debug(F110,"openo: name",name,0);

    if (cxseen || czseen) {		/* If interrupted, get out before */
	debug(F100," open cancelled","",0); /* destroying existing file. */
	return(1);			/* Pretend to succeed. */
    }
    xp = xname;				/* OK to proceed. */
    if (fncnv)				/* If desired, */
    	zrtol(name,xp);			/* convert name to local form */
    else				/* otherwise, */
    	strcpy(xname,name);		/* use it literally */

    debug(F110,"openo: xname",xname,0);

    if (warn) {				/* File collision avoidance? */
	if (zchki(xname) != -1) {	/* Yes, file exists? */
	    znewn(xname,&xp);		/* Yes, make new name. */
	    strcpy(xname,xp);
	    debug(F110," exists, new name ",xname,0);
        }
    }
    if (zopeno(ZOFILE,xname) == 0) {	/* Try to open the file */
	debug(F110,"openo failed",xname,0);
	tlog(F110,"Failure to open",xname,0l);
	return(0);
    } else {
	strcpy(name2,xname);
	debug(F110,"openo ok, name2",name2,0);
	return(1);
    }
}

/*  O P E N T  --  Open the terminal for output, in place of a file  */

opent() {
    ffc = tfc = 0;
    return(zopeno(ZCTERM,""));
}

/*  C L S I F  --  Close the current input file. */

clsif() {
    if (memstr) {			/* If input was memory string, */
	memstr = 0;			/* indicate no more. */
    } else zclose(ZIFILE);		/* else close input file. */

    if (czseen || cxseen) 
    	screen(SCR_ST,ST_DISC,0l,"");
    else
    	screen(SCR_ST,ST_OK,0l,"");
    cxseen = hcflg = 0;			/* Reset flags, */
    *filnam = '\0';			/* and current file name */
}


/*  C L S O F  --  Close an output file.  */

/*  Call with disp != 0 if file is to be discarded.  */
/*  Returns -1 upon failure to close, 0 or greater on success. */

clsof(disp) int disp; {
    int x;
    if ((x = zclose(ZOFILE)) < 0) {	/* Try to close the file */
	tlog(F100,"Failure to close",filnam,0l);
	screen(SCR_ST,ST_ERR,0l,"");
    } else if (disp && (keep == 0)) {	/* Delete it if interrupted, */
	if (*filnam) zdelet(filnam);	/* and not keeping incomplete files */
	debug(F100,"Discarded","",0);
	tlog(F100,"Discarded","",0l);
	screen(SCR_ST,ST_DISC,0l,"");
    } else {				/* Nothing wrong, just keep it */
	debug(F100,"Closed","",0);	/* and give comforting messages. */
	screen(SCR_ST,ST_OK,0l,"");
    }
    *filnam = '\0';			/* Zero the current file name. */
    return(x);				/* Send back zclose() return code. */
}

/*  S N D H L P  --  Routine to send builtin help  */

sndhlp() {
    nfils = 0;				/* No files, no lists. */
    xflg = 1;				/* Flag we must send X packet. */
    strcpy(cmdstr,"help text");		/* Data for X packet. */
    first = 1;				/* Init getchx lookahead */
    memstr = 1;				/* Just set the flag. */
    memptr = hlptxt;			/* And the pointer. */
    if (binary) {			/* If file mode is binary, */
	binary = 0;			/*  turn it back to text for this, */
	savmod = 1;			/*  remember to restore it later. */
    }
    return(sinit());
}


/*  C W D  --  Change current working directory  */

/*
 String passed has first byte as length of directory name, rest of string
 is name.  Fails if can't connect, else ACKs (with name) and succeeds. 
*/

cwd(vdir) char *vdir; {
    vdir[unchar(*vdir) + 1] = '\0';	/* End with a null */
    if (zchdir(vdir+1)) {
	encstr(vdir+1);
	ack1(data);
	tlog(F110,"Changed directory to",vdir+1,0l);
	return(1); 
    } else {
	tlog(F110,"Failed to change directory to",vdir+1,0l);
	return(0);
    }
}


/*  S Y S C M D  --  Do a system command  */

/*  Command string is formed by concatenating the two arguments.  */

syscmd(prefix,suffix) char *prefix, *suffix; {
    char *cp;

    if (prefix == NULL || *prefix == '\0') return(0);

    for (cp = cmdstr; *prefix != '\0'; *cp++ = *prefix++) ;
    while (*cp++ = *suffix++) ;

    debug(F110,"syscmd",cmdstr,0);
    if (zopeni(ZSYSFN,cmdstr) > 0) {
	debug(F100,"syscmd zopeni ok",cmdstr,0);
	nfils = sndsrc = 0;		/* Flag that input from stdin */
	xflg = hcflg = 1;		/* And special flags for pipe */
	if (binary) {			/* If file mode is binary, */
	    binary = 0;			/*  turn it back to text for this, */
	    savmod = 1;			/*  remember to restore it later. */
	}
	return (sinit());		/* Send S packet */
    } else {
	debug(F100,"syscmd zopeni failed",cmdstr,0);
	return(0);
    }
}
