#define VERSION "sz 3.03 5-09-89"
#define PUBDIR "/usr/spool/uucppublic"

/*% cc -compat -M2 -Ox -K -i -DTXBSIZE=16384  -DNFGVMIN -DREADCHECK
sz.c -lx -o sz; size sz

/*% cc -Zi -DXX -DNFGVMIN -DREADCHECK sz.c -lx -o xsz; size xsz
<-xtx-*> cc -Osal -DTXBSIZE=32768  -DSV sz.c -lx -o $B/sz; size $B/sz

 ****************************************************************************
 *
 * sz.c By Chuck Forsberg,  Omen Technology INC
 *
 ****************************************************************************
 *
 * Typical Unix/Xenix/Clone compiles:
 *
 *	cc -O sz.c -o sz		USG (SYS III/V) Unix
 *	cc -O -DSV sz.c -o sz		Sys V Release 2 with non-blocking input
 *					Define to allow reverse channel checking
 *	cc -O -DV7  sz.c -o sz		Unix Version 7, 2.8 - 4.3 BSD
 *
 *	cc -O -K -i -DNFGVMIN -DREADCHECK sz.c -lx -o sz	Classic Xenix
 *
 *	ln sz sb			**** All versions ****
 *	ln sz sx			**** All versions ****
 *
 ****************************************************************************
 *
 * Typical VMS compile and install sequence:
 *
 *		define LNK$LIBRARY   SYS$LIBRARY:VAXCRTL.OLB
 *		cc sz.c
 *		cc vvmodem.c
 *		link sz,vvmodem
 *	sz :== $disk$user2:[username.subdir]sz.exe
 *
 *  If you feel adventureous, remove the #define BADSEEK line
 *  immediately following the #ifdef vax11c line!  Some VMS
 *  systems know how to fseek, some don't.
 *
 ****************************************************************************
 *
 *
 * A program for Unix to send files and commands to computers running
 *  Professional-YAM, PowerCom, YAM, IMP, or programs supporting Y/XMODEM.
 *
 *  Sz uses buffered I/O to greatly reduce CPU time compared to UMODEM.
 *
 *  USG UNIX (3.0) ioctl conventions courtesy Jeff Martin
 *
 *	This version implements ZMODEM Run Length Encoding, Comparision,
 *	and variable length headers.  These features were not funded
 *	by the original Telenet development contract.  This software,
 *	including these features, may be freely used for non
 *	commercial and educational purposes.  This software may also
 *	be freely used to support file transfer operations to or from
 *	licensed Omen Technology products.  Contact Omen Technology
 *	for licensing for other uses.  Any programs which use part or
 *	all of this software must be provided in source form with this
 *	notice intact except by written permission from Omen
 *	Technology Incorporated.
 *
 *		Omen Technology Inc		FAX: 503-621-3745
 *		Post Office Box 4681
 *		Portland OR 97208
 *
 *	Previous versions of this program (not containing the extensions
 *	listed above) remain in the public domain.
 *
 *	This code is made available in the hope it will be useful,
 *	BUT WITHOUT ANY WARRANTY OF ANY KIND OR LIABILITY FOR ANY
 *	DAMAGES OF ANY KIND.
 *
 *  2.1x hacks to avoid VMS fseek() bogosity, allow input from pipe
 *     -DBADSEEK -DTXBSIZE=32768  
 *  2.x has mods for VMS flavor
 *
 * 1.34 implements tx backchannel garbage count and ZCRCW after ZRPOS
 * in accordance with the 7-31-87 ZMODEM Protocol Description
 */

#ifdef XX
#define XARGSFILE "args"
long Thisflen;
#endif

char *substr(), *getenv();

#ifdef vax11c
#define STATIC
#define BADSEEK
#define TXBSIZE 32768		/* Must be power of two, < MAXINT */
#include <types.h>
#include <stat.h>
#define STAT
#define LOGFILE "szlog.tmp"
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
#define OS "VMS"
#define ROPMODE "r"
#define READCHECK
#define BUFWRITE
extern int errno;
#define SS_NORMAL SS$_NORMAL
#define xsendline(c) sendline(c)

#ifndef PROGNAME
#define PROGNAME "sz"
#endif


#else	/* vax11c */

#ifdef GENIE
#define STATIC static
#define LOGFILE "szlog"
#define BADSEEK
#define TXBSIZE 32768		/* Must be power of two, < MAXINT */
#define OS "GEnie"
#define SS_NORMAL 0
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <fildes.h>
FILDES fdes;
extern int errno;
int Binfile;
long Thisflen;

#define sendline(c) putchar(c & 0377)
#define xsendline(c) putchar(c)

#else	/* GENIE */

#define LOGFILE "/tmp/szlog"
#define SS_NORMAL 0
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
extern int errno;
#define STATIC

#define sendline(c) putchar(c & 0377)
#define xsendline(c) putchar(c)

#endif
#endif

#define PATHLEN 256
#define OK 0
#define FALSE 0
#ifdef TRUE
#undef TRUE
#endif
#define TRUE 1
#define ERROR (-1)
/* Ward Christensen / CP/M parameters - Don't change these! */
#define ENQ 005
#define CAN ('X'&037)
#define XOFF ('s'&037)
#define XON ('q'&037)
#define SOH 1
#define STX 2
#define EOT 4
#define ACK 6
#define NAK 025
#define CPMEOF 032
#define WANTCRC 0103	/* send C not NAK to get crc not checksum */
#define WANTG 0107	/* Send G not NAK to get nonstop batch xmsn */
#define TIMEOUT (-2)
#define RCDO (-3)
#define GCOUNT (-4)
#define RETRYMAX 10


#define HOWMANY 2
STATIC int Zmodem=0;		/* ZMODEM protocol requested by receiver */
unsigned Baudrate=4800;		/* Default, set by first mode() call */
STATIC unsigned Effbaud = 4800;
STATIC unsigned Txwindow;	/* Control the size of the transmitted window */
STATIC unsigned Txwspac;	/* Spacing between zcrcq requests */
STATIC unsigned Txwcnt;	/* Counter used to space ack requests */
STATIC long Lrxpos;		/* Receiver's last reported offset */
STATIC int errors;

#ifdef vax11c
#include "vrzsz.c"	/* most of the system dependent stuff here */
#else
#ifdef GENIE
#include "genie.c"	/* most of the system dependent stuff here */
#else
#include "rbsb.c"	/* most of the system dependent stuff here */
#ifdef XX
#undef STAT
#endif
#endif
#endif

#include "crctab.c"

STATIC int Filesleft;
STATIC long Totalleft;

/*
 * Attention string to be executed by receiver to interrupt streaming data
 *  when an error is detected.  A pause (0336) may be needed before the
 *  ^C (03) or after it.
 */
#ifdef READCHECK
STATIC char Myattn[] = { 0 };
#else
#ifdef USG
STATIC char Myattn[] = { 03, 0336, 0 };
#else
#ifndef GENIE
STATIC char Myattn[] = { 0 };
#endif
#endif
#endif

FILE *in;

#ifdef BADSEEK
STATIC int Canseek = 0;	/* 1: Can seek 0: only rewind -1: neither (pipe) */
#ifndef TXBSIZE
#define TXBSIZE 16384		/* Must be power of two, < MAXINT */
#endif
#else
STATIC int Canseek = 1;	/* 1: Can seek 0: only rewind -1: neither (pipe) */
#endif

#ifdef TXBSIZE
#define TXBMASK (TXBSIZE-1)
STATIC char Txb[TXBSIZE];		/* Circular buffer for file reads */
STATIC char *txbuf = Txb;		/* Pointer to current file segment */
#else
STATIC char txbuf[1024];
#endif
STATIC long vpos = 0;			/* Number of bytes read from file */

STATIC char Lastrx;
STATIC char Crcflg;
STATIC int Verbose=0;
STATIC int Modem2=0;		/* XMODEM Protocol - don't send pathnames */
STATIC int Restricted=0;	/* restricted; no /.. or ../ in filenames */
STATIC int Quiet=0;		/* overrides logic that would otherwise
set verbose */
STATIC int Ascii=0;		/* Add CR's for brain damaged programs */
STATIC int Fullname=0;		/* transmit full pathname */
STATIC int Unlinkafter=0;	/* Unlink file after it is sent */
STATIC int Dottoslash=0;	/* Change foo.bar.baz to foo/bar/baz */
STATIC int firstsec;
STATIC int errcnt=0;		/* number of files unreadable */
STATIC int blklen=128;		/* length of transmitted records */
STATIC int Optiong;		/* Let it rip no wait for sector ACK's */
STATIC int Eofseen;		/* EOF seen on input set by zfilbuf */
STATIC int BEofseen;		/* EOF seen on input set by fooseek */
STATIC int Totsecs;		/* total number of sectors this file */
STATIC int Filcnt=0;		/* count of number of files opened */
STATIC int Lfseen=0;
STATIC unsigned Rxbuflen = 16384;	/* Receiver's max buffer length */
STATIC int Tframlen = 0;	/* Override for tx frame length */
STATIC int blkopt=0;		/* Override value for zmodem blklen */
STATIC int Rxflags = 0;
STATIC long bytcnt;
STATIC int Wantfcs32 = TRUE;	/* want to send 32 bit FCS */
STATIC char Lzconv;	/* Local ZMODEM file conversion request */
STATIC char Lzmanag;	/* Local ZMODEM file management request */
STATIC int Lskipnocor;
STATIC char Lztrans;
STATIC int Command;		/* Send a command, then exit. */
STATIC char *Cmdstr;		/* Pointer to the command string */
STATIC int Cmdtries = 11;
STATIC int Cmdack1;		/* Rx ACKs command, then do it */
STATIC int Exitcode;
STATIC int Test;		/* 1= Force receiver to send Attn, etc with qbf. */
			/* 2= Character transparency test */
STATIC char *qbf=
 "The quick brown fox jumped over the lazy dog's back 1234567890\r\n";
STATIC long Lastsync;		/* Last offset to which we got a ZRPOS */
STATIC int Beenhereb4;		/* How many times we've been ZRPOS'd same place */

STATIC jmp_buf tohere;		/* For the interrupt on RX timeout */
STATIC jmp_buf intrjmp;	/* For the interrupt on RX CAN */

#ifdef XARGSFILE
char *
mystrsave(s)
char *s;
{
	register char *p;
	char *malloc();

	if (p = malloc(strlen(s)+1) ) {
		strcpy(p, s); return p;
	}
	fprintf(stderr, "No memory for mystrsave!\n");
	exit(1);
}

/* Remove (presumably) terminating CR and/or LF from string */
uncrlf(s)
register char *s;
{
	for ( ; *s; ++s)
		switch (*s) {
		case '\r':
		case '\n':
			*s = 0;  return;
		}
}
#endif


/* called by signal interrupt or terminate to clean things up */
SIGTYPE
bibi(n)
{
	canit(); fflush(stdout); mode(0);
	fprintf(stderr, "sz: caught signal %d; exiting\n", n);
#ifndef GENIE
	if (n == SIGQUIT)
		abort();
#endif
	if (n == 99)
		fprintf(stderr, "mode(2) in rbsb.c not implemented!!\n");
	cucheck();
	exit(128+n);
}
/* Called when ZMODEM gets an interrupt (^X) */
onintr()
{
	signal(SIGINT, SIG_IGN);
	longjmp(intrjmp, -1);
}

STATIC int Zctlesc;	/* Encode control characters */
STATIC int Nozmodem = 0;	/* If invoked as "sb" */
STATIC char *Progname = "sz";
STATIC int Zrwindow = 1400;	/* RX window size (controls garbage count) */
#include "zm.c"

#include "zmr.c"

#ifdef XARGSFILE
#define XARGSMAX 256
char *xargv[XARGSMAX+1];
#endif

main(argc, argv)
char *argv[];
{
	register char *cp;
	register npats;
	int dm;
	char **patts;
	static char xXbuf[BUFSIZ];

	if ((cp = getenv("ZNULLS")) && *cp)
		Znulls = atoi(cp);
	if ((cp=getenv("SHELL")) && (substr(cp, "rsh") || substr(cp, "rksh")))
		Restricted=TRUE;
	from_cu();
#ifdef vax11c
	chkinvok(PROGNAME);
#else
	chkinvok(argv[0]);
#endif

	Rxtimeout = 600;
	npats=0;
	if (argc<2)
		usage();
	setbuf(stdout, xXbuf);		
	while (--argc) {
		cp = *++argv;
		if (*cp++ == '-' && *cp) {
			while ( *cp) {
				switch(*cp++) {
				case '\\':
					 *cp = toupper(*cp);  continue;
				case '+':
					Lzmanag = ZMAPND; break;
#ifdef CSTOPB
				case '2':
					Twostop = TRUE; break;
#endif
				case 'a':
					Lzconv = ZCNL;
					Ascii = TRUE; break;
				case 'b':
					Lzconv = ZCBIN; break;
				case 'C':
					if (--argc < 1) {
						usage();
					}
					Cmdtries = atoi(*++argv);
					break;
				case 'i':
					Cmdack1 = ZCACK1;
					/* **** FALL THROUGH TO **** */
				case 'c':
					if (--argc != 1) {
						usage();
					}
					Command = TRUE;
					Cmdstr = *++argv;
					break;
				case 'd':
					++Dottoslash;
					/* **** FALL THROUGH TO **** */
				case 'f':
					Fullname=TRUE; break;
				case 'e':
					Zctlesc = 1; break;
				case 'k':
					blklen=1024; break;
				case 'L':
					if (--argc < 1) {
						usage();
					}
					blkopt = atoi(*++argv);
					if (blkopt<24 || blkopt>1024)
						usage();
					break;
				case 'l':
					if (--argc < 1) {
						usage();
					}
					Tframlen = atoi(*++argv);
					if (Tframlen<32 || Tframlen>1024)
						usage();
					break;
				case 'N':
					Lzmanag = ZMNEWL;  break;
				case 'n':
					Lzmanag = ZMNEW;  break;
				case 'o':
					Wantfcs32 = FALSE; break;
				case 'p':
					Lzmanag = ZMPROT;  break;
				case 'r':
					if (Lzconv == ZCRESUM)
						Lzmanag = (Lzmanag &
ZMMASK) | ZMCRC;
					Lzconv = ZCRESUM; break;
				case 'q':
					Quiet=TRUE; Verbose=0; break;
				case 't':
					if (--argc < 1) {
						usage();
					}
					Rxtimeout = atoi(*++argv);
					if (Rxtimeout<10 || Rxtimeout>1000)
						usage();
					break;
				case 'T':
					if (++Test > 1) {
						chartest(1); chartest(2);
						mode(0);  exit(0);
					}
					break;
#ifndef vax11c
				case 'u':
					++Unlinkafter; break;
#endif
				case 'v':
					++Verbose; break;
				case 'w':
					if (--argc < 1) {
						usage();
					}
					Txwindow = atoi(*++argv);
					if (Txwindow < 256)
						Txwindow = 256;
					Txwindow = (Txwindow/64) * 64;
					Txwspac = Txwindow/4;
					if (blkopt > Txwspac
					 || (!blkopt && Txwspac < 1024))
						blkopt = Txwspac;
					break;
				case 'X':
					++Modem2; break;
				case 'Y':
					Lskipnocor = TRUE;
					/* **** FALLL THROUGH TO **** */
				case 'y':
					Lzmanag = ZMCLOB; break;
				case 'Z':
				case 'z':
					Lztrans = ZTRLE;  break;
				default:
					usage();
				}
			}
		}
		else if ( !npats && argc>0) {
			if (argv[0][0]) {
				npats=argc;
				patts=argv;
			}
		}
	}
	if (npats < 1 && !Command && !Test) 
		usage();
	if (Verbose) {
		if (freopen(LOGFILE, "a", stderr)==NULL) {
			printf("Can't open log file %s\n",LOGFILE);
			exit(0200);
		}
		setbuf(stderr, NULL);
	}
	if (Fromcu && !Quiet) {
		if (Verbose == 0)
			Verbose = 2;
	}
	vfile("%s %s for %s\n", Progname, VERSION, OS);

#ifdef XARGSFILE
	vfile("npats=%d *patts=%s", npats, *patts);
	if (npats == 1 && !strcmp(XARGSFILE, *patts)) {
		in = fopen(XARGSFILE, "r");
		if (!in) {
			printf(stderr, "Can't open / control file!\n");
			exit(2);
		}
		for (npats=0,argv=patts=xargv; npats<XARGSMAX; ++npats,++argv) {
			if (fgets(txbuf, 1024, in) <= 0)
				break;
			uncrlf(txbuf);
			*argv = mystrsave(txbuf);
		}
		fclose(in);
	}
#endif

	mode(1);

#ifdef GENIE
	signal(SIGINT, SIG_IGN);
#else
	if (signal(SIGINT, bibi) == SIG_IGN) {
		signal(SIGINT, SIG_IGN); signal(SIGKILL, SIG_IGN);
	} else {
		signal(SIGINT, bibi); signal(SIGKILL, bibi);
	}
#endif
#ifdef SIGQUIT
	if ( !Fromcu)
		signal(SIGQUIT, SIG_IGN);
#endif
#ifdef SIGTERM
	signal(SIGTERM, bibi);
#endif

	if ( !Modem2) {
		if (!Nozmodem) {
			printf("rz\r");  fflush(stdout);
		}
		countem(npats, patts);
		if (!Nozmodem) {
			stohdr(0L);
			if (Command)
				Txhdr[ZF0] = ZCOMMAND;
			zshhdr(4, ZRQINIT, Txhdr);
		}
	}
	fflush(stdout);

	if (Command) {
		if (getzrxinit()) {
			Exitcode=0200; canit();
		}
		else if (zsendcmd(Cmdstr, 1+strlen(Cmdstr))) {
			Exitcode=0200; canit();
		}
	} else if (wcsend(npats, patts)==ERROR) {
		Exitcode=0200;
		canit();
	}
	fflush(stdout);
	mode(0);
	dm = ((errcnt != 0) | Exitcode);
	if (dm) {
		cucheck();  exit(dm);
	}
	exit(SS_NORMAL);
	/*NOTREACHED*/
}

wcsend(argc, argp)
char *argp[];
{
	register n;

	Crcflg=FALSE;
	firstsec=TRUE;
	bytcnt = -1;
	if (Nozmodem) {
		printf("Start your YMODEM receive. ");  fflush(stdout);
	}
	for (n=0; n<argc; ++n) {
		Totsecs = 0;
		if (wcs(argp[n])==ERROR)
			return ERROR;
	}
	Totsecs = 0;
	if (Filcnt==0) {	/* bitch if we couldn't open ANY files */
		if (!Nozmodem && !Modem2) {
			Command = TRUE;
			Cmdstr = "echo \"sz: Can't open any requested files\"";
			if (getnak()) {
				Exitcode=0200; canit();
			}
			if (!Zmodem)
				canit();
			else if (zsendcmd(Cmdstr, 1+strlen(Cmdstr))) {
				Exitcode=0200; canit();
			}
			Exitcode = 1; return OK;
		}
		canit();
		fprintf(stderr,"\r\nCan't open any requested files.\r\n");
		return ERROR;
	}
	if (Zmodem)
		saybibi();
	else if ( !Modem2)
		wctxpn("");
	return OK;
}

wcs(oname)
char *oname;
{
	register c;
	register char *p, *q;
#ifdef STAT
	struct stat f;
#endif
	char name[PATHLEN];

	strcpy(name, oname);

#ifdef XARGSFILE
	/* Parse GEniename:REALname:length pathname syntax */
	Thisflen = -1;
	for (p = oname; *p; ++p) {
		if (*p == ':') {
			*p++ = 0;
			q = p;
			for (++p; *p; ++p) {
				if (*p == ':') {
					*p++ = 0;
					Thisflen = atol(p);
					break;
				}
			}
			strcpy(name, q);
			break;
		}
	}
#endif

#ifdef GENIE
	_describe(oname,&fdes);		/* An undocumented goodie */
	if (fdes.type_file == 1) {	/* Fortran Sequential Binary */
		Binfile = 1;
		in = fopen(oname,"rB");
	}
	else if (fdes.type_file == 0)  { /* Ascii */
		Binfile = 0;
		in = fopen(oname,"r");
	}
	else {				/* not a SL filetype */
		fprintf(stderr, "\nUnknown file type %d\n",fdes.type_file);
		++errcnt;
		return OK;		/* pass over it, there may be others */
	}
#else
	if (Restricted) {
		/* restrict pathnames to current tree or uucppublic */
		if ( substr(name, "../")
		 || (name[0]== '/' && strncmp(name, PUBDIR, strlen(PUBDIR))) ) {
			canit();
			fprintf(stderr,"\r\nsz:\tSecurity Violation\r\n");
			return ERROR;
		}
	}

	in=fopen(oname, ROPMODE);
#endif

	if (in==NULL) {
		++errcnt;
		return OK;	/* pass over it, there may be others */
	}
	BEofseen = Eofseen = 0;  vpos = 0;

#ifdef STAT
	/* Check for directory or block special files */
	fstat(fileno(in), &f);
	c = f.st_mode & S_IFMT;
	if (c == S_IFDIR || c == S_IFBLK) {
		fclose(in);
		return OK;
	}
#endif

	++Filcnt;
	switch (wctxpn(name)) {
	case ERROR:
		return ERROR;
	case ZSKIP:
		return OK;
	}
#ifdef STAT
	if (!Zmodem && wctx(f.st_size)==ERROR)
		return ERROR;
#else
	if (!Zmodem && wctx(1000000000L)==ERROR)
		return ERROR;
#endif

#ifndef vax11c
#ifndef GENIE
	if (Unlinkafter)
		unlink(oname);
#endif
#endif

	return 0;
}

/*
 * generate and transmit pathname block consisting of
 *  pathname (null terminated),
 *  file length, mode time and file mode in octal
 *  as provided by the Unix fstat call.
 *  N.B.: modifies the passed name, may extend it!
 */
wctxpn(name)
char *name;
{
	register char *p, *q;
	char name2[PATHLEN];
#ifdef STAT
	struct stat f;
#endif

	if (Modem2) {
#ifdef STAT
		if (*name && fstat(fileno(in), &f)!= -1) {
			fprintf(stderr, "Sending %s, %ld XMODEM blocks. ",
			  name, (127+f.st_size)>>7);
		}
#endif
		fprintf(stderr, "Give your local XMODEM receive command now.\r\n");
		fflush(stderr);
		return OK;
	}
	zperr("Awaiting pathname nak for %s", *name?name:"<END>");
	if ( !Zmodem)
		if (getnak())
			return ERROR;

	q = (char *) 0;
	if (Dottoslash) {		/* change . to . */
		for (p=name; *p; ++p) {
			if (*p == '/')
				q = p;
			else if (*p == '.')
				*(q=p) = '/';
		}
		if (q && strlen(++q) > 8) {	/* If name>8 chars */
			q += 8;			/*   make it .ext */
			strcpy(name2, q);	/* save excess of name */
			*q = '.';
			strcpy(++q, name2);	/* add it back */
		}
	}

	for (p=name, q=txbuf ; *p; )
		if ((*q++ = *p++) == '/' && !Fullname)
			q = txbuf;
	*q++ = 0;
	p=q;
	while (q < (txbuf + 1024))
		*q++ = 0;
	if (*name) {
#ifdef XX
		if (Thisflen >= 0) {
			sprintf(p, "%u 0 0 0 %d %ld",
			  Thisflen, Filesleft, Totalleft);
			Totalleft -= Thisflen;
		}
#endif
#ifdef GENIE
		else
			sprintf(p, "%d", fdes.current_file_size * 1260);
		vfile("%s open Binfile=%d size=%ld", name, Binfile,
		  fdes.current_file_size * 1260);
#endif

#ifdef STAT
#ifndef XX
		if (fstat(fileno(in), &f)!= -1)
			sprintf(p, "%lu %lo %o 0 %d %ld", f.st_size, f.st_mtime,
			  f.st_mode, Filesleft, Totalleft);
		Totalleft -= f.st_size;
#endif
#endif

	}
	if (--Filesleft <= 0)
		Totalleft = 0;
	if (Totalleft < 0)
		Totalleft = 0;

#ifdef STAT
	/* force 1k blocks if name won't fit in 128 byte block */
	if (txbuf[125])
		blklen=1024;
	else {		/* A little goodie for IMP/KMD */
		txbuf[127] = (f.st_size + 127) >>7;
		txbuf[126] = (f.st_size + 127) >>15;
	}
#endif
	if (Zmodem)
		return zsendfile(txbuf, 1+strlen(p)+(p-txbuf));
	if (wcputsec(txbuf, 0, 128)==ERROR)
		return ERROR;
	return OK;
}

getnak()
{
	register firstch;

	Lastrx = 0;
	for (;;) {
		switch (firstch = readline(800)) {
		case ZPAD:
			if (getzrxinit())
				return ERROR;
			Ascii = 0;	/* Receiver does the conversion */
			return FALSE;
		case TIMEOUT:
			zperr("Timeout on pathname");
			return TRUE;
		case WANTG:
#ifdef MODE2OK
			mode(2);	/* Set cbreak, XON/XOFF, etc. */
#endif
			Optiong = TRUE;
			blklen=1024;
		case WANTCRC:
			Crcflg = TRUE;
		case NAK:
			return FALSE;
		case CAN:
			if ((firstch = readline(20)) == CAN && Lastrx == CAN)
				return TRUE;
		default:
			break;
		}
		Lastrx = firstch;
	}
}


wctx(flen)
long flen;
{
	register int thisblklen;
	register int sectnum, attempts, firstch;
	long charssent;

	charssent = 0;  firstsec=TRUE;  thisblklen = blklen;
	vfile("wctx:file length=%ld", flen);

	while ((firstch=readline(Rxtimeout))!=NAK && firstch != WANTCRC
	  && firstch != WANTG && firstch!=TIMEOUT && firstch!=CAN)
		;
	if (firstch==CAN) {
		zperr("Receiver CANcelled");
		return ERROR;
	}
	if (firstch==WANTCRC)
		Crcflg=TRUE;
	if (firstch==WANTG)
		Crcflg=TRUE;
	sectnum=0;
	for (;;) {
		if (flen <= (charssent + 896L))
			thisblklen = 128;
		if ( !filbuf(txbuf, thisblklen))
			break;
		if (wcputsec(txbuf, ++sectnum, thisblklen)==ERROR)
			return ERROR;
		charssent += thisblklen;
	}
	fclose(in);
	attempts=0;
	do {
		purgeline();
		sendline(EOT);
		flushmo();
		++attempts;
	}
		while ((firstch=(readline(Rxtimeout)) != ACK) &&
attempts < RETRYMAX);
	if (attempts == RETRYMAX) {
		zperr("No ACK on EOT");
		return ERROR;
	}
	else
		return OK;
}

wcputsec(buf, sectnum, cseclen)
char *buf;
int sectnum;
int cseclen;	/* data length of this sector to send */
{
	register checksum, wcj;
	register char *cp;
	unsigned oldcrc;
	int firstch;
	int attempts;

	firstch=0;	/* part of logic to detect CAN CAN */

	if (Verbose>2)
		fprintf(stderr, "Sector %3d %2dk\n", Totsecs, Totsecs/8 );
	else if (Verbose>1)
		fprintf(stderr, "\rSector %3d %2dk ", Totsecs, Totsecs/8 );
	for (attempts=0; attempts <= RETRYMAX; attempts++) {
		Lastrx= firstch;
		sendline(cseclen==1024?STX:SOH);
		sendline(sectnum);
		sendline(-sectnum -1);
		oldcrc=checksum=0;
		for (wcj=cseclen,cp=buf; --wcj>=0; ) {
			sendline(*cp);
			oldcrc=updcrc((0377& *cp), oldcrc);
			checksum += *cp++;
		}
		if (Crcflg) {
			oldcrc=updcrc(0,updcrc(0,oldcrc));
			sendline((int)oldcrc>>8);
			sendline((int)oldcrc);
		}
		else
			sendline(checksum);
		flushmo();

		if (Optiong) {
			firstsec = FALSE; return OK;
		}
		firstch = readline(Rxtimeout);
gotnak:
		switch (firstch) {
		case CAN:
			if(Lastrx == CAN) {
cancan:
				zperr("Cancelled");  return ERROR;
			}
			break;
		case TIMEOUT:
			zperr("Timeout on sector ACK"); continue;
		case WANTCRC:
			if (firstsec)
				Crcflg = TRUE;
		case NAK:
			zperr("NAK on sector"); continue;
		case ACK: 
			firstsec=FALSE;
			Totsecs += (cseclen>>7);
			return OK;
		case ERROR:
			zperr("Got burst for sector ACK"); break;
		default:
			zperr("Got %02x for sector ACK", firstch); break;
		}
		for (;;) {
			Lastrx = firstch;
			if ((firstch = readline(Rxtimeout)) == TIMEOUT)
				break;
			if (firstch == NAK || firstch == WANTCRC)
				goto gotnak;
			if (firstch == CAN && Lastrx == CAN)
				goto cancan;
		}
	}
	zperr("Retry Count Exceeded");
	return ERROR;
}

/* fill buf with count chars padding with ^Z for CPM */
filbuf(buf, count)
register char *buf;
{
	register c, m;

	if ( !Ascii) {
		m = read(fileno(in), buf, count);
		if (m <= 0)
			return 0;
		while (m < count)
			buf[m++] = 032;
		return count;
	}
	m=count;
	if (Lfseen) {
		*buf++ = 012; --m; Lfseen = 0;
	}
	while ((c=getc(in))!=EOF) {
		if (c == 012) {
			*buf++ = 015;
			if (--m == 0) {
				Lfseen = TRUE; break;
			}
		}
		*buf++ =c;
		if (--m == 0)
			break;
	}
	if (m==count)
		return 0;
	else
		while (--m>=0)
			*buf++ = CPMEOF;
	return count;
}

/* Fill buffer with blklen chars */
zfilbuf()
{
	int n;

#ifdef TXBSIZE
	vfile("zfilbuf: bytcnt =%lu vpos=%lu blklen=%d", bytcnt, vpos, blklen);
	/* We assume request is within buffer, or just beyond */
	txbuf = Txb + (bytcnt & TXBMASK);
	if (vpos <= bytcnt) {
#ifdef GENIE
		if (Binfile) {
			long l, m;  char *p;

			for (p=txbuf, n=0, l=blklen;  l;  l -= 128, p+= 128) {
				n += m = fgetb(p, 128, in);
				if (m == 0)
					break;
			}
		} else
#endif
		n = fread(txbuf, 1, blklen, in);

		vpos += n;
		if (n < blklen)
			Eofseen = 1;
		vfile("zfilbuf: n=%d vpos=%lu Eofseen=%d", n, vpos, Eofseen);
		return n;
	}
	if (vpos >= (bytcnt+blklen))
		return blklen;
	/* May be a short block if crash recovery etc. */
	Eofseen = BEofseen;
	return (vpos - bytcnt);
#else
	n = fread(txbuf, 1, blklen, in);
	if (n < blklen)
		Eofseen = 1;
	return n;
#endif
}

#ifdef TXBSIZE
/* Replacement for brain damaged fseek function.  Returns 0==success */
fooseek(fptr, pos, whence)
FILE *fptr;
long pos;
{
	long m, n;
#ifdef GENIE
	long l, k;  char *p;
#endif

	vfile("fooseek: pos =%lu vpos=%lu Canseek=%d", pos, vpos, Canseek);
	/* Seek offset < current buffer */
	if (pos < (vpos -TXBSIZE +1024)) {
		BEofseen = 0;
		if (Canseek > 0) {
			vpos = pos & ~TXBMASK;
			if (vpos >= pos)
				vpos -= TXBSIZE;
			if (fseek(fptr, vpos, 0))
				return 1;
		}
		else if (Canseek == 0) {
#ifdef GENIE
			if (Binfile) {
				if (fseekb(fptr, vpos = 0L, 0))
					return 1;
				} else
#endif
			if (fseek(fptr, vpos = 0L, 0))
				return 1;
		} else
			return 1;
		while (vpos < pos) {
#ifdef GENIE
			if (Binfile) {
				for (p=Txb,n=0,l=TXBSIZE; l; l -= 128,p+= 128) {
					n += (k = fgetb(p, 128, fptr));
					vfile("bsk1: l=%d k=%d", l, k);
					if (k == 0)
						break;
				}
			} else
#endif
			n = fread(Txb, 1, TXBSIZE, fptr);
			vpos += n;
			vfile("n=%d vpos=%ld", n, vpos);
			if (n < TXBSIZE) {
				BEofseen = 1;
				break;
			}
		}
		vfile("vpos=%ld", vpos);
		return 0;
	}
	/* Seek offset > current buffer (Crash Recovery, etc.) */
	if (pos > vpos) {
		if (Canseek)
			if (fseek(fptr, vpos = (pos & ~TXBMASK), 0))
				return 1;
		while (vpos <= pos) {
			txbuf = Txb + (vpos & TXBMASK);
			m = TXBSIZE - (vpos & TXBMASK);
			vfile("m=%ld vpos=%ld", m,vpos);
#ifdef GENIE
			if (Binfile) {
				for (p=txbuf,n=0,l=m; l; l -= 128,p+= 128) {
					n += (k = fgetb(p, 128, fptr));
					vfile("bsk2: l=%d k=%d n=%d", l, k, n);
					if (k == 0)
						break;
				}
			} else
#endif
				n = fread(txbuf, 1, m, fptr);
			vfile("n=%ld vpos=%ld", n,vpos);
			vpos += n;
			vfile("bo=%d m=%ld vpos=%ld", txbuf-Txb,m,vpos);
			if (n < m) {
				BEofseen = 1;
				break;
			}
		}
		return 0;
	}
	/* Seek offset is within current buffer */
	vfile("within buffer: vpos=%ld", vpos);
	return 0;
}
#define fseek fooseek
#endif


/* VARARGS1 */
vfile(f, a, b, c, d)
register char *f;
{
	if (Verbose > 2) {
		fprintf(stderr, f, a, b, c, d);
		fprintf(stderr, "\n");
	}
}


void
alrm()
{
	longjmp(tohere, -1);
}


#ifndef GENIE
#ifndef vax11c
/*
 * readline(timeout) reads character(s) from file descriptor 0
 * timeout is in tenths of seconds
 */
readline(timeout)
{
	register int c;
	static char byt[1];

	fflush(stdout);
	if (setjmp(tohere)) {
		zperr("TIMEOUT");
		return TIMEOUT;
	}
	c = timeout/10;
	if (c<2)
		c=2;
	if (Verbose>5) {
		fprintf(stderr, "Timeout=%d Calling alarm(%d) ", timeout, c);
	}
	signal(SIGALRM, alrm); alarm(c);
	c=read(0, byt, 1);
	alarm(0);
	if (Verbose>5)
		fprintf(stderr, "ret %x\n", byt[0]);
	if (c<1)
		return TIMEOUT;
	return (byt[0]&0377);
}

flushmo()
{
	fflush(stdout);
}


purgeline()
{
#ifdef USG
	ioctl(0, TCFLSH, 0);
#else
	lseek(0, 0L, 2);
#endif
}
#endif
#endif

/* send cancel string to get the other end to shut up */
canit()
{
	static char canistr[] = {
	 24,24,24,24,24,24,24,24,24,24,8,8,8,8,8,8,8,8,8,8,0
	};

#ifdef vax11c
	raw_wbuf(strlen(canistr), canistr);
	purgeline();
#else
	printf(canistr);
	fflush(stdout);
#endif
}


/*
 * Log an error
 */
/*VARARGS1*/
zperr(s,p,u)
char *s, *p, *u;
{
	if (Verbose <= 0)
		return;
	fprintf(stderr, "Retry %d: ", errors);
	fprintf(stderr, s, p, u);
	fprintf(stderr, "\n");
}

/*
 * substr(string, token) searches for token in string s
 * returns pointer to token within string if found, NULL otherwise
 */
char *
substr(s, t)
register char *s,*t;
{
	register char *ss,*tt;
	/* search for first char of token */
	for (ss=s; *s; s++)
		if (*s == *t)
			/* compare token with substring */
			for (ss=s,tt=t; ;) {
				if (*tt == 0)
					return s;
				if (*ss++ != *tt++)
					break;
			}
	return NULL;
}

char *babble[] = {
#ifdef vax11c
	"Send file(s) with ZMODEM/YMODEM/XMODEM Protocol",
	"	(Y) = Option applies to YMODEM only",
	"	(Z) = Option applies to ZMODEM only",
	"Usage:	sz [-2+abdefkLlNnquvwYy] [-] file ...",
	"	sz [-2Ceqv] -c COMMAND",
	"	\\ Force next option letter to upper case",
	"	sb [-2adfkquv] [-] file ...",
	"	sx [-2akquv] [-] file",
#endif
#ifndef vax11c
	"Send file(s) with ZMODEM/YMODEM/XMODEM Protocol",
	"	(Y) = Option applies to YMODEM only",
	"	(Z) = Option applies to ZMODEM only",
	"Usage:	sz [-2+abdefkLlNnquvwYy] [-] file ...",
	"	sz [-2Ceqv] -c COMMAND",
	"	sb [-2adfkquv] [-] file ...",
	"	sx [-2akquv] [-] file",
#endif
#ifdef CSTOPB
	"	2   Use 2 stop bits",
#endif
	"	+   Append to existing destination file (Z)",
	"	a   (ASCII) change NL to CR/LF",
	"	b   Binary file transfer override",
	"	c   send COMMAND (Z)",
#ifndef vax11c
	"	d   Change '.' to '/' in pathnames (Y/Z)",
#endif
	"	e   Escape all control characters (Z)",
	"	f   send Full pathname (Y/Z)",
	"	i   send COMMAND, ack Immediately (Z)",
	"	k   Send 1024 byte packets (Y)",
	"	L N Limit subpacket length to N bytes (Z)",
	"	l N Limit frame length to N bytes (l>=L) (Z)",
	"	n   send file only if source newer (Z)",
	"	N   send file only if source newer or longer (Z)",
	"	o   Use 16 bit CRC instead of 32 bit CRC (Z)",
	"	p   Protect existing destination file (Z)",
	"	r   Resume/Recover interrupted file transfer (Z)",
	"	q   Quiet (no progress reports)",
#ifndef vax11c
	"	u   Unlink (remove) file after transmission",
#endif
	"	v   Verbose - provide debugging information",
	"	w N restrict Window to N bytes (Z)",
	"	Y   Yes, overwrite existing file, skip if not present at rx (Z)",
	"	y   Yes, overwrite existing file (Z)",
	"	Z   Activate ZMODEM compression(Z)",
	""
};

usage()
{
	char **pp;

	for (pp=babble; **pp; ++pp)
		fprintf(stderr, "%s\n", *pp);
	fprintf(stderr, "%s for %s by Chuck Forsberg, Omen Technology INC\n",
	 VERSION, OS);
	fprintf(stderr, "\t\t\042The High Reliability Software\042\n");
	cucheck();
	exit(SS_NORMAL);
}

/*
 * Get the receiver's init parameters
 */
getzrxinit()
{
	register n;
#ifdef STAT
	struct stat f;
#endif

	for (n=10; --n>=0; ) {
		
		switch (zgethdr(Rxhdr, 1)) {
		case ZCHALLENGE:	/* Echo receiver's challenge numbr */
			stohdr(Rxpos);
			zshhdr(4, ZACK, Txhdr);
			continue;
		case ZCOMMAND:		/* They didn't see out ZRQINIT */
			stohdr(0L);
			zshhdr(4, ZRQINIT, Txhdr);
			continue;
		case ZRINIT:
			Rxflags = 0377 & Rxhdr[ZF0];
			Usevhdrs = Rxhdr[ZF1] & CANVHDR;
			Txfcs32 = (Wantfcs32 && (Rxflags & CANFC32));
			Zctlesc |= Rxflags & TESCCTL;
			Rxbuflen = (0377 & Rxhdr[ZP0])+((0377 & Rxhdr[ZP1])<<8);
			if ( !(Rxflags & CANFDX))
				Txwindow = 0;
			vfile("Rxbuflen=%d Tframlen=%d", Rxbuflen, Tframlen);
			if ( !Fromcu)
				signal(SIGINT, SIG_IGN);
#ifdef MODE2OK
			mode(2);	/* Set cbreak, XON/XOFF, etc. */
#endif

#ifndef READCHECK
#ifndef USG
#ifndef GENIE
			/* Use 1024 byte frames if no sample/interrupt */
			if (Rxbuflen < 32 || Rxbuflen > 1024) {
				Rxbuflen = 1024;
				vfile("Rxbuflen=%d", Rxbuflen);
			}
#endif
#endif
#endif

			/* Override to force shorter frame length */
			if (Rxbuflen && (Rxbuflen>Tframlen) && (Tframlen>=32))
				Rxbuflen = Tframlen;
			if ( !Rxbuflen && (Tframlen>=32) && (Tframlen<=1024))
				Rxbuflen = Tframlen;
			vfile("Rxbuflen=%d", Rxbuflen);

#ifndef GENIE
#ifndef vax11c
#ifdef STAT
			/* If using a pipe for testing set lower buf len */
			fstat(0, &f);
			if ((f.st_mode & S_IFMT) != S_IFCHR) {
				Rxbuflen = 1024;
			}
#endif
#endif
#endif

#ifdef BADSEEK
#ifdef GENIE
			if (Txwindow == 0) {
				Txwspac = (Txwindow = 4096)/4;
			}
#else
			if (Txwindow == 0)
				Txwindow = TXBSIZE - 1024;
			Txwspac = TXBSIZE/4;
#endif
			Canseek = 0;
#endif

			/*
			 * If input is not a regular file, force ACK's to
			 *  prevent running beyond the buffer limits
			 */
#ifdef STAT
			if ( !Command) {
				fstat(fileno(in), &f);
				if ((f.st_mode & S_IFMT) != S_IFREG) {
					Canseek = -1;
#ifdef TXBSIZE
					Txwindow = TXBSIZE - 1024;
					Txwspac = TXBSIZE/4;
#else
					return ERROR;
#endif
				}
			}
#endif

			/* Set initial subpacket length */
			if (blklen < 1024) {	/* Command line override? */
				if (Effbaud > 300)
					blklen = 256;
				if (Effbaud > 1200)
					blklen = 512;
				if (Effbaud > 2400)
					blklen = 1024;
			}
			if (Rxbuflen && blklen>Rxbuflen)
				blklen = Rxbuflen;
			if (blkopt && blklen > blkopt)
				blklen = blkopt;
#ifdef GENIE
			blklen /= 128;  blklen *= 128;
			if (blklen < 128)
				blklen = 128;
#endif
			vfile("Rxbuflen=%d blklen=%d", Rxbuflen, blklen);
			vfile("Txwindow = %u Txwspac = %d", Txwindow, Txwspac);


			if (Lztrans == ZTRLE && (Rxflags & CANRLE))
				Txfcs32 = 2;
			else
				Lztrans = 0;

			return (sendzsinit());
		case ZCAN:
		case TIMEOUT:
			return ERROR;
		case ZRQINIT:
			if (Rxhdr[ZF0] == ZCOMMAND)
				continue;
		default:
			zshhdr(4, ZNAK, Txhdr);
			continue;
		}
	}
	return ERROR;
}

/* Send send-init information */
sendzsinit()
{
	register c;

	if (Myattn[0] == '\0' && (!Zctlesc || (Rxflags & TESCCTL)))
		return OK;
	errors = 0;
	for (;;) {
		stohdr(0L);
#ifdef ALTCANOFF
		Txhdr[ALTCOFF] = ALTCANOFF;
#endif
		if (Zctlesc) {
			Txhdr[ZF0] |= TESCCTL; zshhdr(4, ZSINIT, Txhdr);
		}
		else
			zsbhdr(4, ZSINIT, Txhdr);
		zsdata(Myattn, ZATTNLEN, ZCRCW);
		c = zgethdr(Rxhdr, 1);
		switch (c) {
		case ZCAN:
			return ERROR;
		case ZACK:
			return OK;
		default:
			if (++errors > 19)
				return ERROR;
			continue;
		}
	}
}

/* Send file name and related info */
zsendfile(buf, blen)
char *buf;
{
	register c;
	register UNSL long crc;
	long lastcrcrq = -1;
	char *p;

	for (;;) {
		Txhdr[ZF0] = Lzconv;	/* file conversion request */
		Txhdr[ZF1] = Lzmanag;	/* file management request */
		if (Lskipnocor)
			Txhdr[ZF1] |= ZMSKNOLOC;
		Txhdr[ZF2] = Lztrans;	/* file transport request */
		Txhdr[ZF3] = 0;
		zsbhdr(4, ZFILE, Txhdr);
		zsdata(buf, blen, ZCRCW);
again:
		c = zgethdr(Rxhdr, 1);
		switch (c) {
		case ZRINIT:
			while ((c = readline(50)) > 0)
				if (c == ZPAD) {
					goto again;
				}
			/* **** FALL THRU TO **** */
		default:
			continue;
		case ZCAN:
		case TIMEOUT:
		case ZABORT:
		case ZFIN:
			return ERROR;
		case ZCRC:
			if (Rxpos != lastcrcrq) {
				lastcrcrq = Rxpos;
				crc = 0xFFFFFFFFL;
				if (Canseek >= 0) {
					fseek(in, 0L, 0);
					while (((c = getc(in)) != EOF)
&& --lastcrcrq)
						crc = UPDC32(c, crc);
					crc = ~crc;
					clearerr(in);	/* Clear possible EOF */
					lastcrcrq = Rxpos;
				}
			}
			stohdr(crc);
			zsbhdr(4, ZCRC, Txhdr);
			goto again;
		case ZSKIP:
			fclose(in); return c;
		case ZRPOS:
			/*
			 * Suppress zcrcw request otherwise triggered by
			 * lastyunc==bytcnt
			 */
#ifdef GENIE
			/*
			 *  Special case - turn on RLE if not archive, etc.
			 *   otherwise turn off RLE unless cmd line specified
			 */
			if (Rxflags & CANRLE) {		/* RX can do it */
				bytcnt = 0;
				zfilbuf();
				vfile("txbuf012: %x %x %x", txbuf[0], txbuf[1],
				  txbuf[2]);
				if ((txbuf[0] != 032)	/* .ARC file */
				 && (txbuf[0] != 0x1f)	/* .Z file */
				 && (txbuf[0] != 0x1c)	/* .LHZ file */
				 && strncmp(txbuf, "ZOO", 3)
				 && strncmp(txbuf, "GIF", 3)
				 && (txbuf[2] != 3))	/* .ZIP file */
					Txfcs32 = 2;
				else if ( !(Lztrans & ZTRLE))
					Txfcs32 = 1;
			}
			/* GEnie binary can't seek to byte */
			if (Binfile) {
				Rxpos &= ~127L;
			}
#endif
			if (fseek(in, Rxpos, 0))
				return ERROR;
			Lastsync = (bytcnt = Txpos = Lrxpos = Rxpos) -1;
			return zsendfdata();
		}
	}
}

/* Send the data in the file */
zsendfdata()
{
	register c, e, n;
	register newcnt;
	register long tcount = 0;
	int junkcount;		/* Counts garbage chars received by TX */
	static int tleft = 6;	/* Counter for test mode */

	junkcount = 0;
	Beenhereb4 = FALSE;
somemore:
	if (setjmp(intrjmp)) {
waitack:
		junkcount = 0;
		c = getinsync(0);
gotack:
		switch (c) {
		default:
		case ZCAN:
			fclose(in);
			return ERROR;
		case ZSKIP:
			fclose(in);
			return c;
		case ZACK:
		case ZRPOS:
			break;
		case ZRINIT:
			return OK;
		}
#ifdef READCHECK
		/*
		 * If the reverse channel can be tested for data,
		 *  this logic may be used to detect error packets
		 *  sent by the receiver, in place of setjmp/longjmp
		 *  rdchk(fd) returns non 0 if a character is available
		 */
		while (rdchk(0)) {
#ifdef EATSIT
			switch (checked)
#else
			switch (readline(1))
#endif
			{
			case CAN:
			case ZPAD:
				c = getinsync(1);
				goto gotack;
			case XOFF:		/* Wait a while for an XON */
			case XOFF|0200:
				readline(100);
			}
		}
#endif
	}

	if ( !Fromcu)
		signal(SIGINT, onintr);
	newcnt = Rxbuflen;
	Txwcnt = 0;
	stohdr(Txpos);
	zsbhdr(4, ZDATA, Txhdr);

	/*
	 * Special testing mode.  This should force receiver to Attn,ZRPOS
	 *  many times.  Each time the signal should be caught, causing the
	 *  file to be started over from the beginning.
	 */
	if (Test) {
		if ( --tleft)
			while (tcount < 20000) {
				printf(qbf); fflush(stdout);
				tcount += strlen(qbf);
#ifdef READCHECK
				while (rdchk(0)) {
#ifdef EATSIT
					switch (checked)
#else
					switch (readline(1))
#endif
					{
					case CAN:
					case ZPAD:
#ifdef TCFLSH
						ioctl(0, TCFLSH, 1);
#endif
						goto waitack;
					case XOFF:	/* Wait for XON */
					case XOFF|0200:
						readline(100);
					}
				}
#endif
			}
		signal(SIGINT, SIG_IGN); canit();
		sleep(3); purgeline(); mode(0);
		printf("\nsz: Tcount = %ld\n", tcount);
		if (tleft) {
			printf("ERROR: Interrupts Not Caught\n");
			exit(1);
		}
		exit(SS_NORMAL);
	}

	do {
		n = zfilbuf();
		if (Eofseen)
			e = ZCRCE;
		else if (junkcount > 3)
			e = ZCRCW;
		else if (bytcnt == Lastsync)
			e = ZCRCW;
		else if (Rxbuflen && (newcnt -= n) <= 0)
			e = ZCRCW;
		else if (Txwindow && (Txwcnt += n) >= Txwspac) {
			Txwcnt = 0;  e = ZCRCQ;
		} else
			e = ZCRCG;
		if (Verbose>1)
			fprintf(stderr, "\r%7ld ZMODEM%s    ",
			  Txpos, Crc32t?" CRC-32":"");
		zsdata(txbuf, n, e);
		bytcnt = Txpos += n;
		if (e == ZCRCW)
			goto waitack;
#ifdef READCHECK
		/*
		 * If the reverse channel can be tested for data,
		 *  this logic may be used to detect error packets
		 *  sent by the receiver, in place of setjmp/longjmp
		 *  rdchk(fd) returns non 0 if a character is available
		 */
		fflush(stdout);
		while (rdchk(0)) {
#ifdef EATSIT
			switch (checked)
#else
			switch (readline(1))
#endif
			{
			case CAN:
			case ZPAD:
				c = getinsync(1);
				if (c == ZACK)
					break;
#ifdef TCFLSH
				ioctl(0, TCFLSH, 1);
#endif
				/* zcrce - dinna wanna starta ping-pong game */
				zsdata(txbuf, 0, ZCRCE);
				goto gotack;
			case XOFF:		/* Wait a while for an XON */
			case XOFF|0200:
				readline(100);
			default:
				++junkcount;
			}
		}
#endif	/* READCHECK */
		if (Txwindow) {
			while ((tcount = Txpos - Lrxpos) >= Txwindow) {
				vfile("%ld window >= %u", tcount, Txwindow);
				if (e != ZCRCQ)
					zsdata(txbuf, 0, e = ZCRCQ);
				c = getinsync(1);
				if (c != ZACK) {
#ifdef TCFLSH
					ioctl(0, TCFLSH, 1);
#endif
					zsdata(txbuf, 0, ZCRCE);
					goto gotack;
				}
			}
			vfile("window = %ld", tcount);
		}
	} while (!Eofseen);
	if ( !Fromcu)
		signal(SIGINT, SIG_IGN);

	for (;;) {
		stohdr(Txpos);
		zsbhdr(4, ZEOF, Txhdr);
		switch (getinsync(0)) {
		case ZACK:
			continue;
		case ZRPOS:
			goto somemore;
		case ZRINIT:
			return OK;
		case ZSKIP:
			fclose(in);
			return c;
		default:
			fclose(in);
			return ERROR;
		}
	}
}

/*
 * Respond to receiver's complaint, get back in sync with receiver
 */
getinsync(flag)
{
	register c;

	for (;;) {
		if (Test) {
			printf("\r\n\n\n***** Signal Caught *****\r\n");
			Rxpos = 0; c = ZRPOS;
		} else
			c = zgethdr(Rxhdr, 0);
		switch (c) {
		case ZCAN:
		case ZABORT:
		case ZFIN:
		case TIMEOUT:
			return ERROR;
		case ZRPOS:
			/* ************************************* */
			/*  If sending to a buffered modem, you  */
			/*   might send a break at this point to */
			/*   dump the modem's buffer.		 */
			clearerr(in);	/* In case file EOF seen */
			if (fseek(in, Rxpos, 0))
				return ERROR;
			Eofseen = 0;
			bytcnt = Lrxpos = Txpos = Rxpos;
#ifndef GENIE
			if (Lastsync == Rxpos) {
				if (++Beenhereb4 > 4)
					if (blklen > 32)
						blklen /= 2;
			}
#endif
			Lastsync = Rxpos;
			return c;
		case ZACK:
			Lrxpos = Rxpos;
			if (flag || Txpos == Rxpos)
				return ZACK;
			continue;
		case ZRINIT:
		case ZSKIP:
			fclose(in);
			return c;
		case ERROR:
		default:
			zsbhdr(4, ZNAK, Txhdr);
			continue;
		}
	}
}


/* Say "bibi" to the receiver, try to do it cleanly */
saybibi()
{
	for (;;) {
		stohdr(0L);		/* CAF Was zsbhdr - minor change */
		zshhdr(4, ZFIN, Txhdr);	/*  to make debugging easier */
		switch (zgethdr(Rxhdr, 0)) {
		case ZFIN:
			sendline('O'); sendline('O'); flushmo();
		case ZCAN:
		case TIMEOUT:
			return;
		}
	}
}

/* Local screen character display function */
bttyout(c)
{
	if (Verbose)
		putc(c, stderr);
}

/* Send command and related info */
zsendcmd(buf, blen)
char *buf;
{
	register c;
	long cmdnum;

#ifdef GENIE
	cmdnum = 69;
#else
	cmdnum = getpid();
#endif
	errors = 0;
	for (;;) {
		stohdr(cmdnum);
		Txhdr[ZF0] = Cmdack1;
		zsbhdr(4, ZCOMMAND, Txhdr);
		zsdata(buf, blen, ZCRCW);
listen:
		Rxtimeout = 100;		/* Ten second wait for resp. */
		Usevhdrs = 0;		/* Allow rx to send fixed len headers */
		c = zgethdr(Rxhdr, 1);

		switch (c) {
		case ZRINIT:
			goto listen;	/* CAF 8-21-87 */
		case ERROR:
		case GCOUNT:
		case TIMEOUT:
			if (++errors > Cmdtries)
				return ERROR;
			continue;
		case ZCAN:
		case ZABORT:
		case ZFIN:
		case ZSKIP:
		case ZRPOS:
			return ERROR;
		default:
			if (++errors > 20)
				return ERROR;
			continue;
		case ZCOMPL:
			Exitcode = Rxpos;
			saybibi();
			return OK;
		case ZRQINIT:
#ifdef vax11c		/* YAMP :== Yet Another Missing Primitive */
			return ERROR;
#else
			vfile("******** RZ *******");
			system("rz");
			vfile("******** SZ *******");
			goto listen;
#endif
		}
	}
}

/*
 * If called as sb use YMODEM protocol
 */
chkinvok(s)
char *s;
{
	register char *p;

	p = s;
	while (*p == '-')
		s = ++p;
	while (*p)
		if (*p++ == '/')
			s = p;
	if (*s == 'v') {
		Verbose=1; ++s;
	}
	Progname = s;
	if (s[0]=='s' && s[1]=='b') {
		Nozmodem = TRUE; blklen=1024;
	}
	if (s[0]=='s' && s[1]=='x') {
		Modem2 = TRUE;
	}
}

#ifdef STAT
countem(argc, argv)
register char **argv;
{
	register c;
	struct stat f;

	for (Totalleft = 0, Filesleft = 0; --argc >=0; ++argv) {
		f.st_size = -1;
		if (Verbose>2) {
			fprintf(stderr, "\nCountem: %03d %s ", argc, *argv);
			fflush(stderr);
		}
		if (access(*argv, 04) >= 0 && stat(*argv, &f) >= 0) {
			c = f.st_mode & S_IFMT;
			if (c != S_IFDIR && c != S_IFBLK) {
				++Filesleft;  Totalleft += f.st_size;
			}
		}
		if (Verbose>2)
			fprintf(stderr, " %ld", f.st_size);
	}
	if (Verbose>2)
		fprintf(stderr, "\ncountem: Total %d %ld\n",
		  Filesleft, Totalleft);
}
#else
countem(argc, argv)
register char **argv;
{
	register c;
	register char *p;
	long size;

	for (Totalleft = 0, Filesleft = 0; --argc >=0; ++argv) {
		size = -1;
		if (Verbose>2) {
			fprintf(stderr, "\nCountem: %03d %s ", argc, *argv);
			fflush(stderr);
		}
		++Filesleft;  
#ifdef XARGSFILE
		/* Look for file length in third colon sep field */
		for (p = *argv; *p; ++p) {
			if (*p == ':') {
				for (++p; *p; ++p) {
					if (*p == ':') {
						++p;
						size = atol(p);
						Totalleft += size;
						break;
					}
				}
			break;
			}
		}
#endif

		if (Verbose>2)
			fprintf(stderr, " %ld", size);
	}
	if (Verbose>2)
		fprintf(stderr, "\ncountem: Total %d %ld\n",
		  Filesleft, Totalleft);
}
#endif

chartest(m)
{
	register n;

	mode(m);
	printf("\r\n\nCharacter Transparency Test Mode %d\r\n", m);
	printf("If Pro-YAM/ZCOMM is not displaying ^M hit ALT-V NOW.\r\n");
	printf("Hit Enter.\021");  fflush(stdout);
	readline(500);

	for (n = 0; n < 256; ++n) {
		if (!(n%8))
			printf("\r\n");
		printf("%02x ", n);  fflush(stdout);
		sendline(n);	flushmo();
		printf("  ");  fflush(stdout);
		if (n == 127) {
			printf("Hit Enter.\021");  fflush(stdout);
			readline(500);
			printf("\r\n");  fflush(stdout);
		}
	}
	printf("\021\r\nEnter Characters, echo is in hex.\r\n");
	printf("Hit SPACE or pause 40 seconds for exit.\r\n");

	while (n != TIMEOUT && n != ' ') {
		n = readline(400);
		printf("%02x\r\n", n);
		fflush(stdout);
	}
	printf("\r\nMode %d character transparency test ends.\r\n", m);
	fflush(stdout);
}

/* End of sz.c */
