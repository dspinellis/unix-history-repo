#ifndef MAC
char *versio = "C-Kermit 5A(188), 23 Nov 92"; /* Version herald. */
#else
/*
  For Macintosh, also remember to change the Mac-specific version in ckmkr2.r.
*/
char *versio = "Mac Kermit 0.99(188) Pre-ALPHA, 23 Nov 92";
#endif /* MAC */
long vernum = 501188L;
/*
  String and numeric version numbers, keep these three in sync!
  First digit of vermum = major version, i.e. 5.
  Second 2 digits of vernum: 00 = no minor version, 01 = A, 02 = B, etc.
  Last three digits are edit number. 
*/
#ifndef VERWHO
/* Change verwho in following line, or with -DVERWHO=x in makefile CFLAGS. */
#define VERWHO 0
#endif /* VERWHO */
int verwho = VERWHO; /* Who produced this version, 0 = Columbia University */
/*
  IMPORTANT: If you are working on your own private version of C-Kermit, please
  include some special notation, like your site name or your initials, in the
  "versio" string, e.g. "5A(182)-XXX", and use a nonzero code for the "verwho"
  variable (e.g. in the USA use your zip code).  Unless we stick to this
  discipline, divergent copies of C-Kermit will begin to appear that are
  intistinguishable from each other, which is a big support issue.  Also, if
  you have edited C-Kermit and made copies available to others, please add
  appropriate text to the BUG command (ckuus6.c, function dobug()).
*/
#define CKCMAI

/*  C K C M A I  --  C-Kermit Main program  */

/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.

  COPYRIGHT NOTICE:

  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.

  DOCUMENTATION:

  "Using C-Kermit" by Frank da Cruz and Christine M. Gianone,
  Digital Press, Burlington, MA, USA.  Publication date: Winter 1992.
  Order Number: EY-J896E-DP
  Digital Press ISBN: 1-55558-108-0
  Prentice Hall ISBN: 0-13-037490-3

  DISCLAIMER:

  The C-Kermit software is provided in source code form by Kermit Development
  and Distribution, Columbia University.  The software is provided "as is;" no
  other warranty is provided, express or implied, including without
  limitations, any implied warranty of merchantability or implied warranty of
  fitness for a particular purpose.

  Neither Columbia University nor any of the contributors to the C-Kermit
  development effort, including, but not limited to, AT&T, Digital Equipment
  Corporation, Data General Corporation, or International Business Machines
  Corporation, warrant C-Kermit software or documentation in any way.  In
  addition, neither the authors of any Kermit programs, publications or
  documentation, nor Columbia University nor any contributing institutions or
  individuals acknowledge any liability resulting from program or
  documentation errors.

  ACKNOWLEDGMENTS:

  The Kermit file transfer protocol was developed at the Columbia University
  Center for Computing Activities (CUCCA).  It is named after Kermit the Frog,
  star of the television series THE MUPPET SHOW; the name is used by permission
  of Henson Associates, Inc.

  Thanks to at least the following people for their contributions to this
  program over the years, and apologies to anybody I missed:

   Chris Adie, Edinburgh U, Scotland (OS/2 support)
   Robert Adsett, University of Waterloo, Canada
   Larry Afrin, Clemson U
   Greg Andrews, Telebit Corp
   Barry Archer, U of Missouri
   Robert Andersson, International Systems A/S, Oslo, Norway
   Chris Armstrong, Brookhaven National Lab (OS/2)
   William Bader, Software Consulting Services, Nazareth, PA
   Fuat Baran, CUCCA
   Stan Barber, Rice U
   Jim Barbour, U of Colorado
   Donn Baumgartner, Dell
   Nelson Beebe, U of Utah
   Karl Berry, UMB
   Dean W Bettinger, SUNY
   Gary Bilkus
   Marc Boucher, U of Montreal
   Charles Brooks, EDN
   Bob Brown
   Mike Brown, Purdue U
   Jack Bryans, California State U at Long Beach
   Mark Buda, DEC (VAX/VMS)
   Fernando Cabral, Padrao IX, Brasilia, Brazil
   Bjorn Carlsson, Stockholm University Computer Centre QZ, Sweden
   Bill Catchings, formerly of CUCCA
   Bob Cattani, Columbia U CS Dept
   Davide Cervone, Rochester University
   Seth Chaiklin, Denmark
   John Chandler, Harvard U / Smithsonian Astronomical Observatory
   John L Chmielewski, AT&T, Lisle, IL
   Howard Chu, U of Michigan
   Bill Coalson, McDonnell Douglas
   Bertie Coopersmith, London, UK
   Chet Creider, University of Western Ontario, Canada
   Alan Crosswell, CUCCA
   Jeff Damens, formerly of CUCCA
   Mark Davies, Bath U, UK
   S. Dezawa, Fujifilm, Japan
   Joe R. Doupnik, Utah State U
   Frank Dreano (Honeywell)
   John Dunlap, University of Washington
   David Dyck, John Fluke Mfg Co.
   Stefaan A. Eeckels, Eurokom, Luxembourg
   Paul Eggert, Twin Sun, Inc., El Segundo, CA
   Bernie Eiben, DEC
   Kristoffer Eriksson, Peridot Konsult AB, Oerebro, Sweden
   John R. Evans, IRS, Kansas City
   Glenn Everhart, RCA Labs
   Charlie Finan, Cray Research
   Herm Fischer, Encino, CA (extensive contributions to version 4.0)
   Carl Fongheiser, CWRU
   Marcello Frutig, Catholic University, Sao Paulo, Brazil (X.25 support)
   Hirofumi Fujii, Japan Nat'l Lab for High Energy Physics, Tokyo (Kanji)
   Chuck Fuller, Westinghouse Corporate Computer Services
   Andy Fyfe, Caltech
   Christine M. Gianone, CUCCA
   John Gilmore, UC Berkeley
   German Goldszmidt, IBM
   Alistair Gorman, New Zealand
   Richard Gration, ADFA, Australia
   Chris Green, Essex U, UK
   Alan Grieg, Dundee Tech, Scotland, UK
   Yekta Gursel, MIT
   Jim Guyton, Rand Corp
   Michael Haertel
   Bob Hain, UMN
   Marion Hakanson, ORST
   John Hamilston, Iowa State U
   Simon Hania, Netherlands
   Stan Hanks, Rice U.
   Ken Harrenstein, SRI
   Eugenia Harris, Data General (AOS/VS)
   David Harrison, Kingston Warren Corp
   James Harvey, Indiana/Purdue U (VMS)
   Rob Healey
   Chuck Hedrick, Rutgers U
   Ron Heiby, Technical Systems Division, Motorola Computer Group
   Steve Hemminger, Tektronix
   Christian Hemsing, RWTH Aachen, Germany (OS-9)
   Andrew Herbert, Monash Univ, Australia
   Mike Hickey, ITI
   R E Hill
   Bill Homer, Cray Research
   Ray Hunter, The Wollongong Group
   Randy Huntziger, National Library of Medicine
   Larry Jacobs, Transarc
   Steve Jenkins, Lancaster University, UK
   Dave Johnson, Gradient Technologies
   Mark B Johnson, Apple Computer
   Eric F Jones, AT&T
   Luke Jones, AT&T
   Peter Jones, U of Quebec Montreal
   Phil Julian, SAS Institute
   Peter Kabal, U of Quebec
   Mic Kaczmarczik, U of Texas at Austin
   Sergey Kartashoff, Inst. of Precise Mechanics & Computer Equipment, Moscow
   Howie Kaye, CUCCA
   Rob Kedoin, Linotype Co, Hauppauge, NY (OS/2)
   Mark Kennedy, IBM
   Terry Kennedy, St Peter's College, Jersey City, NJ (VAX/VMS, 2.11 BSD)
   Douglas Kingston, morgan.com
   Tom Kloos, Sequent Computer Systems
   Jim Knutson, U of Texas at Austin
   Scott Kramer, SRI International, Menlo Park, CA
   David Kricker, Encore Computer
   Thomas Krueger, UWM
   Bo Kullmar, Central Bank of Sweden, Kista
   R. Brad Kummer, AT&T Bell Labs, Atlanta, GA
   John Kunze, UC Berkeley
   Bob Larson, USC (OS-9)
   Bert Laverman, Groningen U, Netherlands
   Steve Layton
   David Lawyer, UC Irvine
   David LeVine, National Semiconductor Corporation
   S.O. Lidie, Lehigh U
   Tor Lillqvist, Helsinki University, Finland
   Dean Long
   Kevin Lowey, U of Saskatchewan (OS/2)
   Andy Lowry, Columbia University
   David MacKenzie, Environmental Defense Fund, University of Maryland
   John Mackin, University of Sidney, Australia
   Martin Maclaren, Bath U, UK
   Chris Maio, Columbia U CS Dept
   Fulvio Marino, Olivetti, Ivrea, Italy
   Peter Mauzey, AT&T
   Tye McQueen, Utah State U
   Ted Medin
   Hellmuth Michaelis, Hanseatischer Computerservice GmbH, Hamburg, Germany
   Leslie Mikesell, American Farm Bureau
   Martin Minow, DEC (VAX/VMS)
   Pawan Mistra, Bellcore
   Ken Mizialko, IBM, Manassas, VA
   Ray Moody, Purdue U
   Bruce J Moore, Allen-Bradley Co, Highland Heights, OH (Atari ST)
   Steve Morley, Convex
   Peter Mossel, Columbia University
   Tony Movshon, NYU
   Lou Muccioli, Swanson Analysis Systems
   Dan Murphy
   Gary Mussar
   John Nall, FSU
   Jack Nelson, University of Pittsburgh
   Jim Noble, Planning Research Corporation (Macintosh)
   Ian O'Brien, Bath U, UK
   John Owens
   Michael Pins, Iowa Computer Aided Engineering Network
   Andre' Pirard, University of Liege, Belgium
   Paul Placeway, Ohio State U (Macintosh & more)
   Piet W. Plomp, ICCE, Groningen University, Netherlands
   Ken Poulton, HP Labs
   Manfred Prange, Oakland U
   Christopher Pratt, APV Baker, UK
   Frank Prindle, NADC
   Tony Querubin, U of Hawaii
   Anton Rang
   Scott Ribe
   Alan Robiette, Oxford University, UK
   Michel Robitaille, U of Montreal (Mac)
   Kai Uwe Rommel, Technische Universitaet Muenchen (OS/2)
   Larry Rosenman (Amiga)
   Jay Rouman, U of Michigan
   Jack Rouse, SAS Institute (Data General and/or Apollo)
   Stew Rubenstein, Harvard U (VAX/VMS)
   Bill Schilit, Columbia University
   Michael Schmidt, U of Paderborn, Germany
   Eric Schnoebelen, Convex
   Benn Schreiber, DEC
   Dan Schullman, DEC (modems, DIAL command, etc)
   John Schultz, 3M
   Steven Schultz, Contel (PDP-11)
   APPP Scorer, Leeds Polytechnic, UK
   Gordon Scott, Micro Focus, Newbury UK
   Gisbert W. Selke, WIdO, Bonn, Germany
   David Sizeland, U of London Medical School
   Fridrik Skulason, Iceland
   Dave Slate
   Bradley Smith, UCLA
   Richard S Smith, Cal State
   Ryan Stanisfer, UNT
   Bertil Stenstroem, Stockholm University Computer Centre (QZ), Sweden
   James Sturdevant, CAP GEMENI AMERICA, Minneapolis
   Peter Svanberg, Royal Techn. HS, Sweden
   James R. Swenson, Accu-Weather, Inc.
   Andy Tanenbaum, Vrije U, Amsterdam, Netherlands
   Markku Toijala, Helsinki U of Technology
   Rick Troxel, NIH
   Warren Tucker, Tridom Corp, Mountain Park, GA
   Dave Tweten, AMES-NAS
   G Uddeborg, Sweden
   Walter Underwood, Ford Aerospace
   Pieter Van Der Linden, Centre Mondial, Paris
   Ge van Geldorp, Netherlands
   Fred van Kempen, MINIX User Group, Voorhout, Netherlands
   Wayne Van Pelt, GE/CRD
   Mark Vasoll, Oklahoma State U (V7 UNIX)
   Konstantin Vinogradov, ICSTI, Moscow
   Paul Vixie, DEC
   Dimitri Vulis, CUNY
   Roger Wallace, Raytheon
   Stephen Walton, Calif State U, Northridge (Amiga)
   Jamie Watson, Adasoft, Switzerland (RS/6000)
   Rick Watson, U of Texas (Macintosh)
   Robert Weiner, Programming Plus, New York City
   Lauren Weinstein, Vortex Technlogy
   David Wexelblat, AT&T
   Joachim Wiesel, U of Karlsruhe
   Lon Willett, U of Utah
   Michael Williams, UCLA
   Nate Williams, U of Montana
   David Wilson
   Patrick Wolfe, Kuck & Associates, Inc.
   Gregg Wonderly, Oklahoma State U (V7 UNIX)
   Farrell Woods, Concurrent (formerly Masscomp)
   Dave Woolley, CAP Communication Systems, London
   Jack Woolley, SCT Corp
   Frank Wortner
   Ken Yap, U of Rochester
   John Zeeff, Ann Arbor, MI
*/
/*
  ckcsym.h is used for for defining symbols that normally would be defined
  using -D or -d on the cc command line, for use with compilers that don't
  support this feature.
*/
#include "ckcsym.h"
#include "ckcasc.h"			/* ASCII character symbols */
#include "ckcdeb.h"			/* Debug & other symbols */
#include "ckcker.h"			/* Kermit symbols */
#include "ckcnet.h"			/* Network symbols */
#ifndef NOSPL
#include "ckuusr.h"
#endif /* NOSPL */

#ifndef NOSERVER
/* Text message definitions.. each should be 256 chars long, or less. */
#ifdef MAC
char *hlptxt = "\r\
Mac Kermit Server Commands:\r\
\r\
    BYE\r\
    FINISH\r\
    GET filespec\r\
    REMOTE CD directory\r\
    REMOTE HELP\r\
    SEND filespec\r\
\r\0";
#else
#ifdef AMIGA
char *hlptxt = "C-Kermit Server Commands:\n\
\n\
GET filespec, SEND filespec, FINISH, BYE, REMOTE HELP\n\
\n\0";
#else
#ifdef OS2
char *hlptxt = "C-Kermit Server REMOTE Commands:\n\
\n\
GET files  REMOTE CD [dir]     REMOTE DIRECTORY [files]\n\
SEND files REMOTE SPACE [dir]  REMOTE HOST command\n\
FINISH     REMOTE DELETE files REMOTE TYPE files\n\
BYE        REMOTE HELP         REMOTE SET parameter value\n\
\n\0";
#else
#ifdef MINIX
char *hlptxt = "C-Kermit Server REMOTE Commands:\n\
GET SEND BYE FINISH REMOTE: CD DEL DIR HELP HOST SET SPACE TYPE WHO\n\0";
#else
#ifdef VMS
char *hlptxt = "C-Kermit Server REMOTE Commands:\r\n\
\r\n\
GET files  REMOTE CD [dir]     REMOTE DIRECTORY [files]\r\n\
SEND files REMOTE SPACE [dir]  REMOTE HOST command\r\n\
MAIL files REMOTE DELETE files REMOTE WHO [user]\r\n\
BYE        REMOTE PRINT files  REMOTE TYPE files\r\n\
FINISH     REMOTE HELP         REMOTE SET parameter value\r\n\
\0";
#else
#ifdef datageneral
char *hlptxt = "C-Kermit Server REMOTE Commands:\n\
\n\
GET files  REMOTE CD [dir]     REMOTE DIRECTORY [filespec]\n\
SEND files REMOTE SPACE [dir]  REMOTE HOST command\n\
BYE        REMOTE TYPE file    REMOTE DELETE files\n\
FINISH     REMOTE WHO          REMOTE SET\n\
\0";
#else
char *hlptxt = "C-Kermit Server REMOTE Commands:\n\
\n\
GET files  REMOTE CD [dir]     REMOTE DIRECTORY [files]\n\
SEND files REMOTE SPACE [dir]  REMOTE HOST command\n\
MAIL files REMOTE DELETE files REMOTE WHO [user]\n\
BYE        REMOTE PRINT files  REMOTE TYPE files\n\
FINISH     REMOTE HELP         REMOTE SET parameter value\n\
\n\0";
#endif
#endif
#endif
#endif
#endif
#endif

#ifdef MINIX
char *srvtxt = "\r\n\
Entering server mode.\r\n\0";
#else
#ifdef OSK
char *srvtxt = "\r\l\
Entering server mode.  If your local Kermit software is menu driven, use\r\l\
the menus to send commands to the server.  Otherwise, enter the escape\r\l\
sequence to return to your local Kermit prompt and issue commands from\r\l\
there.  Use SEND and GET for file transfer.  Use REMOTE HELP for a list of\r\l\
other available services.  Use BYE or FINISH to end server mode.\r\l\0";
#else /* UNIX, VMS, AOS/VS, and all others */
char *srvtxt = "\r\n\
Entering server mode.  If your local Kermit software is menu driven, use\r\n\
the menus to send commands to the server.  Otherwise, enter the escape\r\n\
sequence to return to your local Kermit prompt and issue commands from\r\n\
there.  Use SEND and GET for file transfer.  Use REMOTE HELP for a list of\r\n\
other available services.  Use BYE or FINISH to end server mode.\r\n\0";
#endif /* OSK */
#endif /* MINIX */
#else  /* server mode disabled */
char *srvtxt = "";
#endif /* NOSERVER */

/* Declarations for Send-Init Parameters */

int spsiz = DSPSIZ,                     /* Current packet size to send */
    spmax = DSPSIZ,			/* Biggest packet size we can send */
    spsizr = DSPSIZ,			/* Send-packet size requested */
    spsizf = 0,                         /* Flag to override size negotiation */
    rpsiz = DRPSIZ,                     /* Biggest we want to receive */
    urpsiz = DRPSIZ,			/* User-requested receive pkt size */
    maxrps = MAXRP,			/* Maximum incoming long packet size */
    maxsps = MAXSP,			/* Maximum outbound l.p. size */
    maxtry = MAXTRY,			/* Maximum retries per packet */
    wslots = 1,				/* Window size currently in use */
    wslotr = 1,				/* Window size from SET WINDOW */
    wslotn = 1,				/* Window size negotiated in S-pkt */
    timeouts = 0,			/* For statistics reporting */
    spackets = 0,			/*  ... */
    rpackets = 0,			/*  ... */
    retrans = 0,			/*  ... */
    crunched = 0,			/*  ... */
    wmax = 0,				/*  ... */
    wcur = 0,				/*  ... */
    srvdis = 0,				/* Server file xfer display */
    srvtim = DSRVTIM,			/* Server command wait timeout */
/*
  timint is the timeout interval I use when waiting for a packet.
  pkttim is the SET RECEIVE TIMEOUT value, sent to the other Kermit.
  rtimo is the SET SEND TIMEOUT value.  rtimo is the initial value of
  timint.  timint is changed by the value in the incoming negotiation
  packet unless a SET SEND TIMEOUT command was given.
*/
    timint = DMYTIM,                    /* Timeout interval I use */
    pkttim = URTIME,			/* Timeout I want you to use */
    rtimo = DMYTIM,			/* Normal packet wait timeout */
    timef = 0,                          /* Flag to override what you ask */
    npad = MYPADN,                      /* How much padding to send */
    mypadn = MYPADN,                    /* How much padding to ask for */
    bctr = 1,                           /* Block check type requested */
    bctu = 1,                           /* Block check type used */
    bctl = 1,				/* Block check length */
    ebq =  MYEBQ,                       /* 8th bit prefix */
    ebqflg = 0,                         /* 8th-bit quoting flag */
    rqf = -1,				/* Flag used in 8bq negotiation */
    rq = 0,				/* Received 8bq bid */
    sq = 'Y',				/* Sent 8bq bid */
    rpt = 0,                            /* Repeat count */
    rptq = MYRPTQ,                      /* Repeat prefix */
    rptflg = 0;                         /* Repeat processing flag */

int capas = 9,				/* Position of Capabilities */
    atcapb = 8,				/* Attribute capability */
    atcapr = 1,				/*  requested */
    atcapu = 0,				/*  used */
    swcapb = 4,				/* Sliding Window capability */
    swcapr = 1,				/*  requested (allowed) */
    swcapu = 0,				/*  used */
    lpcapb = 2,				/* Long Packet capability */
    lpcapr = 1,				/*  requested */
    lpcapu = 0,				/*  used */
    lscapb = 32,			/* Locking Shift capability */
    lscapr = 1,				/*  requested by default */
    lscapu = 0;				/*  used */

/* Flags for whether to use particular attributes */

int atenci = 1,				/* Encoding in */
    atenco = 1,				/* Encoding out */
    atdati = 1,				/* Date in */
    atdato = 1,				/* Date out */
    atdisi = 1,				/* Disposition in/out */
    atdiso = 1,
    atleni = 1,				/* Length in/out (both kinds) */
    atleno = 1,
    atblki = 1,				/* Blocksize in/out */
    atblko = 1,
    attypi = 1,				/* File type in/out */
    attypo = 1,
    atsidi = 1,				/* System ID in/out */
    atsido = 1,
    atsysi = 1,			       /* System-dependent parameters in/out */
    atsyso = 1;

CHAR padch = MYPADC,                    /* Padding character to send */
    mypadc = MYPADC,                    /* Padding character to ask for */
    seol = MYEOL,                       /* End-Of-Line character to send */
    eol = MYEOL,                        /* End-Of-Line character to look for */
    ctlq = CTLQ,                        /* Control prefix in incoming data */
    myctlq = CTLQ;                      /* Outbound control character prefix */

struct zattr iattr;			/* Incoming file attributes */

/* File related variables, mainly for the benefit of VAX/VMS */

int fblksiz = DBLKSIZ;		/* File blocksize */
int frecl = DLRECL;		/* File record length */
int frecfm = XYFF_S;		/* File record format (default = stream) */
int forg = XYFO_S;		/* File organization (sequential) */
int fcctrl = XYFP_N;		/* File carriage control (ctrl chars) */

#ifdef VMS
/* VMS labeled file options */
int lf_opts = LBL_NAM;
#else
int lf_opts = 0;
#endif /* VMS */

/* Packet-related variables */

int pktnum = 0,                         /* Current packet number */
    sndtyp = 0,				/* Type of packet just sent */
    rsn,				/* Received packet sequence number */
    rln,				/* Received packet length */
    size,                               /* Current size of output pkt data */
    osize,                              /* Previous output packet data size */
    maxsize,                            /* Max size for building data field */
    spktl = 0,				/* Length packet being sent */
    rpktl = 0,				/* Length of packet just received */
    rprintf,				/* REMOTE PRINT flag */
    rmailf;				/* MAIL flag */

CHAR
#ifdef NO_MORE  /* Buffers used before sliding windows... */
    sndpkt[MAXSP+100],			/* Entire packet being sent */
    recpkt[MAXRP+200],			/* Packet most recently received */
    data[MAXSP+4],			/* Packet data buffer */
#endif
#ifdef DYNAMIC
    *srvcmd = (CHAR *)0,		/* Where to decode server command */
#else
    srvcmd[MAXRP+4],                    /* Where to decode server command */
#endif
    padbuf[95],				/* Buffer for send-padding */
    *recpkt,
    *rdatap,				/* Pointer to received packet data */
    *data = (CHAR *)0,			/* Pointer to send-packet data */
    *srvptr,                            /* Pointer to srvcmd */
    mystch = SOH,                       /* Outbound packet-start character */
    stchr = SOH;                        /* Incoming packet-start character */

/* File-related variables */

char filnam[257];                       /* Name of current file. */
char cmdfil[80];			/* Application file name. */

int nfils = 0;				/* Number of files in file group */
long fsize;                             /* Size of current file */
int wildxpand = 0;			/* Who expands wildcards */
int clfils = 0;				/* Flag for command-line files */
int stayflg = 0;			/* Flag for "stay", i.e. "-S" */

/* Communication line variables */

char ttname[80];                        /* Name of communication line. */

#ifdef MAC
int connected = 0;			/* true if connected */
int startconnected;			/* initial state of connected */
#endif /* MAC */

long speed = -1L;			/* Line speed */

int parity,                             /* Parity specified, 0,'e','o',etc */
    autopar = 0,			/* Automatic parity change flag */
    sosi = 0,				/* Shift-In/Out flag */
    flow,                               /* Flow control */
    turn = 0,                           /* Line turnaround handshake flag */
    turnch = XON,                       /* Line turnaround character */
    duplex = 0,                         /* Duplex, full by default */
    escape = DFESC,			/* Escape character for connect */
    delay = DDELAY,                     /* Initial delay before sending */
    tnlm = 0,				/* Terminal newline mode */
    mdmtyp = 0;                         /* Modem type (initially none)  */

/* Networks for SET HOST */

#define MYHOSTL 100
    char myhost[MYHOSTL];		/* Local host name */
    int network = 0;			/* Network vs tty connection */
#ifdef OS2
/* For now, DECnet is the only type supported by OS/2 */
    int nettype = NET_DEC;
#else
    int nettype = NET_TCPB;		/* Assume TCP/IP (BSD sockets) */
#endif /* OS2 */

#ifdef SUNX25
    extern initpad();
    int revcall = 0;            /* X.25 reverse call not selected */
    int closgr  = -1;		/* X.25 closed user group not selected */
    int cudata = 0;		/* X.25 call user data not specified */
    char udata[MAXCUDATA];	/* X.25 call user data */
#endif /* SUNX25 */

/* Other recent additions */

    int tlevel = -1;			/* Take-file command level */
#ifndef NOSPL
    extern int cmdlvl;			/* Command level */
    extern int maclvl;			/* Macro invocation level */
#endif /* NOSPL */
    int carrier = CAR_AUT;		/* Pay attention to carrier signal */
    int cdtimo = 0;			/* Carrier wait timeout */
    int xitsta = GOOD_EXIT;		/* Program exit status */
#ifdef VMS				/* Default filename collision action */
    int fncact = XYFX_X;		/* REPLACE for VAX/VMS */
#else
    int fncact = XYFX_B;		/* BACKUP for everybody else */
#endif /* VMS */
    int bgset = -1;			/* BACKGROUND mode set explicitly */
#ifdef UNIX
    int suspend = DFSUSP;		/* Whether SUSPEND command, etc, */
#else					/* is to be allowed. */
    int suspend = 0;
#endif /* UNIX */

/* Statistics variables */

long filcnt,                    /* Number of files in transaction */
    flci,                       /* Characters from line, current file */
    flco,                       /* Chars to line, current file  */
    tlci,                       /* Chars from line in transaction */
    tlco,                       /* Chars to line in transaction */
    ffc,                        /* Chars to/from current file */
    tfc,                        /* Chars to/from files in transaction */
    rptn;			/* Repeated characters compressed */

int tsecs = 0;                  /* Seconds for transaction */
int fsecs = 0;			/* Per-file timer */

/* Flags */

int deblog = 0,                         /* Flag for debug logging */
    debses = 0,				/* Flag for DEBUG SESSION */
    pktlog = 0,                         /* Flag for packet logging */
    seslog = 0,                         /* Session logging */
    tralog = 0,                         /* Transaction logging */
    displa = 0,                         /* File transfer display on/off */
    stdouf = 0,                         /* Flag for output to stdout */
    stdinf = 0,				/* Flag for input from stdin */
    xflg   = 0,                         /* Flag for X instead of F packet */
    hcflg  = 0,                         /* Doing Host command */
    fncnv  = 1,                         /* Flag for file name conversion */
    binary = 0,                         /* Flag for binary file */
    savmod = 0,                         /* Saved file mode (whole session) */
    bsave  = 0,				/* Saved file mode (per file) */
    bsavef = 0,				/* Flag if bsave was used. */
    cmask  = 0177,			/* Connect byte mask */
    fmask  = 0377,			/* File byte mask */
    warn   = 0,                         /* Flag for file warning */
    quiet  = 0,                         /* Be quiet during file transfer */
    local  = 0,                         /* Flag for external tty vs stdout */
    server = 0,                         /* Flag for being a server */
    cflg   = 0,				/* Connect before transaction */
    cnflg  = 0,                         /* Connect after transaction */
    cxseen = 0,                         /* Flag for cancelling a file */
    czseen = 0,                         /* Flag for cancelling file group */
    discard = 0,			/* Flag for file to be discarded */
    keep = 0,                           /* Keep incomplete files */
    unkcs = 1,				/* Keep file w/unknown character set */
    nakstate = 0,			/* In a state where we can send NAKs */
    dblchar = -1;			/* Character to double when sending */

/* Variables passed from command parser to protocol module */

#ifndef NOSPL
_PROTOTYP( int parser, (int) );         /* The parser itself */
char *clcmds = NULL;			/* Pointer to command-line commands */
#endif /* NOSPL */

CHAR sstate  = (CHAR) 0;                /* Starting state for automaton */
CHAR zstate  = (CHAR) 0;		/* For remembering sstate */
char *cmarg  = "";                      /* Pointer to command data */
char *cmarg2 = "";                      /* Pointer to 2nd command data */
char **cmlist;                          /* Pointer to file list in argv */

/* Flags for the ENABLE and DISABLE commands */

int en_cwd = 1;				/* CD/CWD */
int en_del = 1;				/* DELETE */
int en_dir = 1;				/* DIRECTORY */
int en_fin = 1;				/* FINISH/BYE */
int en_get = 1;				/* GET */
#ifndef NOPUSH
int en_hos = 1;				/* HOST enabled */
#else
int en_hos = 0;				/* HOST disabled */
#endif /* NOPUSH */
int en_sen = 1;				/* SEND */
int en_set = 1;				/* SET */
int en_spa = 1;				/* SPACE */
int en_typ = 1;				/* TYPE */
int en_who = 1;				/* WHO */
#ifdef datageneral
/* Data General AOS/VS can't do this */
int en_bye = 0;				/* BYE */
#else
int en_bye = 1;				/* BYE */
#endif /* datageneral */

/* Miscellaneous */

char **xargv;                           /* Global copies of argv */
int  xargc;                             /* and argc  */
int xargs;				/* an immutable copy of argc */
char *xarg0;				/* and of argv[0] */

extern char *dftty;                     /* Default tty name from ck?tio.c */
extern int dfloc;                       /* Default location: remote/local */
extern int dfprty;                      /* Default parity */
extern int dfflow;                      /* Default flow control */

/*
  Buffered file input and output buffers.  See getpkt() in ckcfns.c
  and zoutdump() in the system-dependent file i/o module (usually ck?fio.c).
*/
#ifndef DYNAMIC
/* Now we allocate them dynamically, see getiobs() below. */
char zinbuffer[INBUFSIZE], zoutbuffer[OBUFSIZE];
#endif
char *zinptr, *zoutptr;
int zincnt, zoutcnt;

_PROTOTYP( int getiobs, (void) );

/*  M A I N  --  C-Kermit main program  */

#ifndef NOCCTRAP
#include <setjmp.h>
extern jmp_buf cmjbuf;
#ifdef GEMDOS				/* Special for Atari ST */
    cc_clean();
#endif /* GEMDOS */
#endif /* NOCCTRAP */

#ifdef aegis
/* On the Apollo, intercept main to insert a cleanup handler */
int
ckcmai(argc,argv) int argc; char **argv;
#else
#ifdef MAC
int main (void)
#else
int
main(argc,argv) int argc; char **argv;
#endif /* MAC */
#endif /* aegis */
{
#ifdef datageneral
short *pfha = 016000000036;             /* Get around LANG_RT problem -- */
*pfha = (short) 0;                      /* No user protection fault handler */
#endif /* datageneral */
  
/* Do some initialization */

    if (sysinit() < 0)			/* System-dependent initialization. */
      fatal("Can't initialize!");
    connoi();				/* Console interrupts off */
#ifndef MAC
    xargc = xargs = argc;		/* Make global copies of argc */
    xargv = argv;                       /* ...and argv. */
    xarg0 = argv[0];
#endif /* MAC */
    sstate = 0;                         /* No default start state. */
#ifdef DYNAMIC
    if (getiobs() < 0) 
      fatal("Can't allocate i/o buffers!");
#endif /* DYNAMIC */
    ckhost(myhost,MYHOSTL);		/* Name of local host */
    strcpy(ttname,dftty);               /* Set up default tty name. */
    local = dfloc;                      /* And whether it's local or remote. */
    parity = dfprty;                    /* Set initial parity, */
    flow = dfflow;                      /* and flow control. */
    if (local) if (ttopen(ttname,&local,0,0) < 0) { /* If default tty line */
	printf("%s: Can't open device\n",ttname);   /* is external, open it */
	local = 0;			            /* now... */
	strcpy(ttname,CTTNAM);
    }
    speed = ttgspd();			/* Get transmission speed. */

#ifdef SUNX25
    initpad();                          /* Initialize X.25 PAD */
#endif /* SUNX25 */

    if (inibufs(SBSIZ,RBSIZ) < 0)	/* Allocate packet buffers */
      fatal("Can't allocate packet buffers!");

#ifndef NOICP
#ifdef MAC
    cmdini();
#else /* Not MAC */
/* Attempt to take ini file before doing command line */

    *cmdfil = '\0';			/* Assume no command file. */
    prescan();				/* But first check for -y option */

#ifndef NOCCTRAP
    setint();				/* Set up interrupts */
    if (setjmp(cmjbuf)) {		/* Control-C trap returns to here. */
#ifdef GEMDOS
	cc_clean();			/* Atari: Clean up after ^C-trap. */
#endif /* GEMDOS */
	doexit(GOOD_EXIT,-1);		/* Exit with good status. */
    } else {
#endif /* NOCCTRAP */
	cmdini();			/* Sets tlevel */
	while (tlevel > -1) {		/* Execute init file. */
	    sstate = parser(0);		/* Loop getting commands. */
	    if (sstate) proto();	/* Enter protocol if requested. */
	}
#ifndef NOCCTRAP
    }
#endif /* NOCCTRAP */

/*
  In UNIX there are two ways to invoke Kermit with a cmdfile:
  (1) From the kermit command line, e.g. "kermit cmdfile [ options... ]"
      argv[0] = "kermit"
      argv[1] = "cmdfile"
  (2) By executing a cmdfile whose first line is like "#!/path/kermit"
      argv[0] = "/path/kermit" (sometimes just "kermit")
      argv[1] = "/path/cmdfile"
*/
    if (argc > 1) {
	if (*argv[1] != '-') {
	    if (zchki(argv[1]) > 0) {
		strcpy(cmdfil,argv[1]);
	    }
	}
    }
    if (*cmdfil) {			/* If we got one, */
	dotake(cmdfil);			/* execute it */
	while (tlevel > -1) {		/* until it runs out. */
	    sstate = parser(1);		/* Loop getting commands. */
	    if (sstate) proto();	/* Enter protocol if requested. */
	}
    }
    *cmdfil = '\0';			/* Done, nullify the file name */
#endif /* MAC */
#endif /* NOICP */

#ifndef NOCMDL
/* Look for a UNIX-style command line... */

    if (argc > 1) {                     /* Command line arguments? */
        sstate = cmdlin();              /* Yes, parse. */
	zstate = sstate;		/* Remember sstate around protocol */
	if (cflg) conect();		/* Connect first if requested */
        if (sstate) {
	    if (displa) concb((char)escape); /* (for console "interrupts") */
#ifndef NOCCTRAP
	    setint();			/* Set up interrupts */
	    if (setjmp(cmjbuf)) {	/* Control-C trap returns to here. */
#ifdef GEMDOS
		cc_clean();
#endif /* GEMDOS */
		if (cnflg) conect();	/* connect again if requested, */
	    } else {
#endif /* NOCCTRAP */
		proto();		/* Take any requested action, then */
		if (!quiet)		/* put cursor back at left margin, */
		  conoll("");
		if (cnflg) conect();	/* connect if requested, */
#ifndef NOCCTRAP
	    }
#endif /* NOCCTRAP */
	}
/*
  If interactive commands were given on the command line (using the
  -C "command, command, ..." option), assign them to a macro called
  "cl_commands", then execute the macro and leave it defined for
  subsequent re-execution if desired.
*/
#ifndef NOSPL
	if (clcmds) {			/* Check for -C commands */
	    int x;
	    x = addmac("cl_commands",clcmds); /* Put macro in table */
	    if (x > -1) {		/* If successful, */
		dodo(x,NULL);		/* set up for macro execution */
		while (maclvl > -1) {	/* Loop getting macro commands. */
		    sstate = parser(1);
		    if (sstate) proto(); /* Enter protocol if requested. */
		}
	    }
	}
#endif /* NOSPL */
#ifndef NOICP
/*
  If a command-line action argument was given and -S ("stay") was not given,
  exit now.
*/
	if ((cflg || cnflg || zstate) && !stayflg)
#endif /* NOICP */
	  doexit(GOOD_EXIT,xitsta); /* exit with good status */
    }
#endif /* NOCMDL */

#ifdef NOICP				/* No interactive command parser */
    else {
#ifndef NOCMDL
	usage();			/* Command-line-only version */
	doexit(BAD_EXIT,-1);
#else					/* Neither one! */
	doexit(BAD_EXIT,-1);
#endif /* NOCMDL */
    }
#else /* not NOICP */
/*
  If no action requested on command line, or if -S ("stay") was included,
  enter the interactive command parser.
*/
    herald();				/* Display program herald. */

#ifndef NOCCTRAP			/* If not no Control-C trap */
ccagain:
    if (setjmp(cmjbuf)) {		/* Control-C trap returns to here. */
#ifdef GEMDOS
	cc_clean();
#endif /* GEMDOS */
	fixcmd();			/* Pop command stacks, etc. */
	debug(F100,"ckcmai got interrupt","",0);
	goto ccagain;			/* set up trap again. */
    } else {
	debug(F100,"ckcmai setting interrupt trap","",0);
	setint();			/* Set up command interrupt traps */
    }
#else /* NOCCTRAP */
    setint();				/* Set up command interrupt traps */
#endif /* NOCCTRAP */

#ifdef MAC
    while (1) {
	extern char *lfiles;		/* fake pointer cast */

	if (connected) {
	    debug(F100, "main: calling macparser", "", 0);
	    sstate = newparser(1, 1, 0L);

	    /* ignore null command state */
	    if (sstate == 'n')
		sstate = '\0';

	    if (sstate)
		proto();
	}
	else {
	    /*
	     * process take files the finder gave us.
	     */
	    if ((tlevel == -1) && lfiles)
		startlfile();

	    debug(F100, "main: calling parser", "", 0);
	    sstate = parser(0);
	    if (sstate == 'c')		/* if MAC connect */
		sstate = 0;
	    if (sstate)
		proto();
	}
    }
#else /* Not MAC */

/*
  Running from an application file, or a command filename was
  specified on the command line.
*/
    if (*cmdfil) dotake(cmdfil);	/* Command file spec'd on cmd line */
    while(1) {				/* Loop getting commands. */
	sstate = parser(0);
        if (sstate) proto();            /* Enter protocol if requested. */
    }
#endif /* MAC */
#endif /* NOICP */
}

#ifdef DYNAMIC
/* Allocate file i/o buffers */

char *zinbuffer, *zoutbuffer;

int
getiobs() {
    zinbuffer = (char *)malloc(INBUFSIZE);
    if (!zinbuffer) return(-1);
    zoutbuffer = (char *)malloc(OBUFSIZE);
    if (!zoutbuffer) return(-1);    
    debug(F100,"getiobs ok","",0);
    return(0);
}
#endif /* DYNAMIC */
