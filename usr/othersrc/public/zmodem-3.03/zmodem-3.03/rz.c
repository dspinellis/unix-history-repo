#define VERSION "3.00 4-19-89"
#define PUBDIR "/usr/spool/uucppublic"

/*% cc -compat -M2 -Ox -K -i -DMD % -o rz; size rz;
<-xtx-*> cc386 -Ox -DMD rz.c -o $B/rz;  size $B/rz
 *
 * rz.c By Chuck Forsberg
 *
 *	cc -O rz.c -o rz		USG (3.0) Unix
 * 	cc -O -DV7  rz.c -o rz		Unix V7, BSD 2.8 - 4.3
 *
 *	ln rz rb;  ln rz rx			For either system
 *
 *	ln rz /usr/bin/rzrmail		For remote mail.  Make this the
 *					login shell. rzrmail then calls
 *					rmail(1) to deliver mail.
 *
 * To compile on VMS:
 *
 *	define LNK$LIBRARY   SYS$LIBRARY:VAXCRTL.OLB
 *	cc rz.c
 *	cc vvmodem.c
 *	link rz,vvmodem
 *	rz :== $disk:[username.subdir]rz.exe
 *      For high speed, try increasing the SYSGEN parameter TTY_TYPAHDSZ to 256.
 *
 *
 *  Unix is a trademark of Western Electric Company
 *
 * A program for Unix to receive files and commands from computers running
 *  Professional-YAM, PowerCom, YAM, IMP, or programs supporting XMODEM.
 *  rz uses Unix buffered input to reduce wasted CPU time.
 *
 *	This version implements ZMODEM Run Length Encoding 
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
 *
 * Iff the program is invoked by rzCOMMAND, output is piped to 
 * "COMMAND filename"  (Unix only)
 *
 *  Some systems (Venix, Coherent, Regulus) may not support tty raw mode
 *  read(2) the same way as Unix. ONEREAD must be defined to force one
 *  character reads for these systems. Added 7-01-84 CAF
 *
 *  Alarm signal handling changed to work with 4.2 BSD 7-15-84 CAF 
 *
 *  BIX added 6-30-87 to support BIX(TM) upload protocol used by the
 *  Byte Information Exchange.
 *
 *  NFGVMIN Updated 2-18-87 CAF for Xenix systems where c_cc[VMIN]
 *  doesn't work properly (even though it compiles without error!),
 *
 *  SEGMENTS=n added 2-21-88 as a model for CP/M programs
 *    for CP/M-80 systems that cannot overlap modem and disk I/O.
 *
 *  VMS flavor hacks begin with rz version 2.00
 *
 *  -DMD may be added to compiler command line to compile in
 *    Directory-creating routines from Public Domain TAR by John Gilmore
 *
 *  HOWMANY may be tuned for best performance
 *
 *  USG UNIX (3.0) ioctl conventions courtesy  Jeff Martin
 */

#ifdef vax11c
#include <types.h>
#include <stat.h>
#define LOGFILE "rzlog.tmp"
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
#define OS "VMS"
#define BUFREAD
extern int errno;
#define SS_NORMAL SS$_NORMAL

#ifndef PROGNAME
#define PROGNAME "rz"
#endif


#else


#define SS_NORMAL 0
#define LOGFILE "/tmp/rzlog"
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
extern int errno;
FILE *popen();
#endif

#define OK 0
#define FALSE 0
#define TRUE 1
#define ERROR (-1)

/*
 * Max value for HOWMANY is 255.
 *   A larger value reduces system overhead but may evoke kernel bugs.
 *   133 corresponds to an XMODEM/CRC sector
 */
#ifndef HOWMANY
#define HOWMANY 133
#endif

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
#define TIMEOUT (-2)
#define RCDO (-3)
#define GCOUNT (-4)
#define ERRORMAX 5
#define RETRYMAX 5
#define WCEOT (-10)
#define PATHLEN 257	/* ready for 4.2 bsd ? */
#define UNIXFILE 0xF000	/* The S_IFMT file mask bit for stat */

int Zmodem=0;		/* ZMODEM protocol requested */
int Nozmodem = 0;	/* If invoked as "rb" */
unsigned Baudrate = 2400;
unsigned Effbaud = 2400;
#ifdef vax11c
#include "vrzsz.c"	/* most of the system dependent stuff here */
#else
#include "rbsb.c"	/* most of the system dependent stuff here */
#endif
#include "crctab.c"

char *substr();
FILE *fout;

/*
 * Routine to calculate the free bytes on the current file system
 *  ~0 means many free bytes (unknown)
 */
long getfree()
{
	return(~0L);	/* many free bytes ... */
}

int Lastrx;
int Crcflg;
int Firstsec;
int Eofseen;		/* indicates cpm eof (^Z) has been received */
int errors;
int Restricted=0;	/* restricted; no /.. or ../ in filenames */
#ifdef ONEREAD
/* Sorry, Regulus and some others don't work right in raw mode! */
int Readnum = 1;	/* Number of bytes to ask for in read() from modem */
#else
int Readnum = HOWMANY;	/* Number of bytes to ask for in read() from modem */
#endif

#define DEFBYTL 2000000000L	/* default rx file size */
long Bytesleft;		/* number of bytes of incoming file left */
long Modtime;		/* Unix style mod time for incoming file */
int Filemode;		/* Unix style mode for incoming file */
char Pathname[PATHLEN];
char *Progname;		/* the name by which we were called */

int Batch=0;
int Topipe=0;
int MakeLCPathname=TRUE;	/* make received pathname lower case */
int Verbose=0;
int Quiet=0;		/* overrides logic that would otherwise set verbose */
int Nflag = 0;		/* Don't really transfer files */
int Rxclob=FALSE;	/* Clobber existing file */
int Rxbinary=FALSE;	/* receive all files in bin mode */
int Rxascii=FALSE;	/* receive files in ascii (translate) mode */
int Thisbinary;		/* current file is to be received in bin mode */
int Blklen;		/* record length of received packets */

#ifdef SEGMENTS
int chinseg = 0;	/* Number of characters received in this data seg */
char secbuf[1+(SEGMENTS+1)*1024];
#else
char secbuf[1025];
#endif


char linbuf[HOWMANY];
int Lleft=0;		/* number of characters in linbuf */
time_t timep[2];
char Lzmanag;		/* Local file management request */
char zconv;		/* ZMODEM file conversion request */
char zmanag;		/* ZMODEM file management request */
char ztrans;		/* ZMODEM file transport request */
int Zctlesc;		/* Encode control characters */
int Zrwindow = 1400;	/* RX window size (controls garbage count) */

jmp_buf tohere;		/* For the interrupt on RX timeout */

#define xsendline(c) sendline(c)

#include "zm.c"

#include "zmr.c"

int tryzhdrtype=ZRINIT;	/* Header type to send corresponding to
Last rx close */

void
alrm()
{
	longjmp(tohere, -1);
}

/* called by signal interrupt or terminate to clean things up */
SIGTYPE
bibi(n)
{
	if (Zmodem)
		zmputs(Attn);
	canit(); mode(0);
	fprintf(stderr, "rz: caught signal %d; exiting", n);
	cucheck();
	exit(128+n);
}

main(argc, argv)
char *argv[];
{
	register char *cp;
	register npats;
	char *virgin, **patts;
	char *getenv();
	int exitcode;

	Rxtimeout = 100;
	setbuf(stderr, NULL);
	if ((cp=getenv("SHELL")) && (substr(cp, "rsh") || substr(cp, "rksh")))
		Restricted=TRUE;

	from_cu();
#ifdef vax11c
	chkinvok(virgin = PROGNAME);
#else
	chkinvok(virgin=argv[0]);	/* if called as [-]rzCOMMAND set flag */
#endif
	npats = 0;
	while (--argc) {
		cp = *++argv;
		if (*cp == '-') {
			while( *++cp) {
				switch(*cp) {
				case '\\':
					 cp[1] = toupper(cp[1]);  continue;
				case '+':
					Lzmanag = ZMAPND; break;
				case 'a':
					Rxascii=TRUE;  break;
				case 'b':
					Rxbinary=TRUE; break;
				case 'c':
					Crcflg=TRUE; break;
#ifndef vax11c
				case 'D':
					Nflag = TRUE; break;
#endif
				case 'e':
					Zctlesc = 1; break;
				case 'p':
					Lzmanag = ZMPROT;  break;
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
				case 'w':
					if (--argc < 1) {
						usage();
					}
					Zrwindow = atoi(*++argv);
					break;
				case 'u':
					MakeLCPathname=FALSE; break;
				case 'v':
					++Verbose; break;
				case 'y':
					Rxclob=TRUE; break;
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
	if (npats > 1)
		usage();
	if (Batch && npats)
		usage();
	if (Verbose) {
		if (freopen(LOGFILE, "a", stderr)==NULL) {
			printf("Can't open log file %s\n",LOGFILE);
			exit(0200);
		}
		setbuf(stderr, NULL);
		fprintf(stderr, "argv[0]=%s Progname=%s\n", virgin, Progname);
	}
	if (Fromcu && !Quiet) {
		if (Verbose == 0)
			Verbose = 2;
	}
	vfile("%s %s for %s\n", Progname, VERSION, OS);
	mode(1);
	if (signal(SIGINT, bibi) == SIG_IGN) {
		signal(SIGINT, SIG_IGN); signal(SIGKILL, SIG_IGN);
	}
	else {
		signal(SIGINT, bibi); signal(SIGKILL, bibi);
	}
	signal(SIGTERM, bibi);
	if (wcreceive(npats, patts)==ERROR) {
		exitcode=0200;
		canit();
	}
	mode(0);
	if (exitcode && !Zmodem)	/* bellow again with all thy might. */
		canit();
	if (exitcode)
		cucheck();
	exit(exitcode ? exitcode:SS_NORMAL);
}


usage()
{
	cucheck();
	fprintf(stderr,"Usage:	rz [-abeuvy]		(ZMODEM)\n");
	fprintf(stderr,"or	rb [-abuvy]		(YMODEM)\n");
	fprintf(stderr,"or	rx [-abcv] file	(XMODEM or XMODEM-1k)\n");
	fprintf(stderr,"	  -a ASCII transfer (strip CR)\n");
	fprintf(stderr,"	  -b Binary transfer for all files\n");
#ifndef vax11c
	fprintf(stderr,"	  -c Use 16 bit CRC	(XMODEM)\n");
#endif
	fprintf(stderr,"	  -e Escape control characters	(ZMODEM)\n");
	fprintf(stderr,"	  -v Verbose more v's give more info\n");
	fprintf(stderr,"	  -y Yes, clobber existing file if any\n");
	fprintf(stderr,"%s %s for %s by Chuck Forsberg, Omen Technology INC\n",
	  Progname, VERSION, OS);
	fprintf(stderr, "\t\t\042The High Reliability Software\042\n");
	exit(SS_NORMAL);
}
/*
 *  Debugging information output interface routine
 */
/* VARARGS1 */
vfile(f, a, b, c)
register char *f;
{
	if (Verbose > 2) {
		fprintf(stderr, f, a, b, c);
		fprintf(stderr, "\n");
	}
}

/*
 * Let's receive something already.
 */

char *rbmsg =
"%s ready. To begin transfer, type \"%s file ...\" to your modem program\r\n\n";

wcreceive(argc, argp)
char **argp;
{
	register c;

	if (Batch || argc==0) {
		Crcflg=1;
		if ( !Quiet)
			fprintf(stderr, rbmsg, Progname, Nozmodem?"sb":"sz");
		if (c=tryz()) {
			if (c == ZCOMPL)
				return OK;
			if (c == ERROR)
				goto fubar;
			c = rzfiles();
			if (c)
				goto fubar;
		} else {
			for (;;) {
				if (wcrxpn(secbuf)== ERROR)
					goto fubar;
				if (secbuf[0]==0)
					return OK;
				if (procheader(secbuf) == ERROR)
					goto fubar;
				if (wcrx()==ERROR)
					goto fubar;
			}
		}
	} else {
		Bytesleft = DEFBYTL; Filemode = 0; Modtime = 0L;

		procheader(""); strcpy(Pathname, *argp); checkpath(Pathname);
		fprintf(stderr, "\nrz: ready to receive %s\r\n", Pathname);
		if ((fout=fopen(Pathname, "w")) == NULL)
			return ERROR;
		if (wcrx()==ERROR)
			goto fubar;
	}
	return OK;
fubar:
	canit();
#ifndef vax11c
	if (Topipe && fout) {
		pclose(fout);  return ERROR;
	}
#endif
	Modtime = 1;
	if (fout)
		fclose(fout);
#ifndef vax11c
	if (Restricted) {
		unlink(Pathname);
		fprintf(stderr, "\r\nrz: %s removed.\r\n", Pathname);
	}
#endif
	return ERROR;
}


/*
 * Fetch a pathname from the other end as a C ctyle ASCIZ string.
 * Length is indeterminate as long as less than Blklen
 * A null string represents no more files (YMODEM)
 */
wcrxpn(rpn)
char *rpn;	/* receive a pathname */
{
	register c;

#ifdef NFGVMIN
	readline(1);
#else
	purgeline();
#endif

et_tu:
	Firstsec=TRUE;  Eofseen=FALSE;
	sendline(Crcflg?WANTCRC:NAK);
	Lleft=0;	/* Do read next time ... */
	while ((c = wcgetsec(rpn, 100)) != 0) {
		if (c == WCEOT) {
			zperr( "Pathname fetch returned %d", c);
			sendline(ACK);
			Lleft=0;	/* Do read next time ... */
			readline(1);
			goto et_tu;
		}
		return ERROR;
	}
	sendline(ACK);
	return OK;
}

/*
 * Adapted from CMODEM13.C, written by
 * Jack M. Wierda and Roderick W. Hart
 */

wcrx()
{
	register int sectnum, sectcurr;
	register char sendchar;
	register char *p;
	int cblklen;			/* bytes to dump this block */

	Firstsec=TRUE;sectnum=0; Eofseen=FALSE;
	sendchar=Crcflg?WANTCRC:NAK;

	for (;;) {
		sendline(sendchar);	/* send it now, we're ready! */
		Lleft=0;	/* Do read next time ... */
		sectcurr=wcgetsec(secbuf, (sectnum&0177)?50:130);
		report(sectcurr);
		if (sectcurr==(sectnum+1 &0377)) {
			sectnum++;
			cblklen = Bytesleft>Blklen ? Blklen:Bytesleft;
			if (putsec(secbuf, cblklen)==ERROR)
				return ERROR;
			if ((Bytesleft-=cblklen) < 0)
				Bytesleft = 0;
			sendchar=ACK;
		}
		else if (sectcurr==(sectnum&0377)) {
			zperr( "Received dup Sector");
			sendchar=ACK;
		}
		else if (sectcurr==WCEOT) {
			if (closeit())
				return ERROR;
			sendline(ACK);
			Lleft=0;	/* Do read next time ... */
			return OK;
		}
		else if (sectcurr==ERROR)
			return ERROR;
		else {
			zperr( "Sync Error");
			return ERROR;
		}
	}
}

/*
 * Wcgetsec fetches a Ward Christensen type sector.
 * Returns sector number encountered or ERROR if valid sector not received,
 * or CAN CAN received
 * or WCEOT if eot sector
 * time is timeout for first char, set to 4 seconds thereafter
 ***************** NO ACK IS SENT IF SECTOR IS RECEIVED OK **************
 *    (Caller must do that when he is good and ready to get next sector)
 */

wcgetsec(rxbuf, maxtime)
char *rxbuf;
int maxtime;
{
	register checksum, wcj, firstch;
	register unsigned short oldcrc;
	register char *p;
	int sectcurr;

	for (Lastrx=errors=0; errors<RETRYMAX; errors++) {

		if ((firstch=readline(maxtime))==STX) {
			Blklen=1024; goto get2;
		}
		if (firstch==SOH) {
			Blklen=128;
get2:
			sectcurr=readline(1);
			if ((sectcurr+(oldcrc=readline(1)))==0377) {
				oldcrc=checksum=0;
				for (p=rxbuf,wcj=Blklen; --wcj>=0; ) {
					if ((firstch=readline(1)) < 0)
						goto bilge;
					oldcrc=updcrc(firstch, oldcrc);
					checksum += (*p++ = firstch);
				}
				if ((firstch=readline(1)) < 0)
					goto bilge;
				if (Crcflg) {
					oldcrc=updcrc(firstch, oldcrc);
					if ((firstch=readline(1)) < 0)
						goto bilge;
					oldcrc=updcrc(firstch, oldcrc);
					if (oldcrc & 0xFFFF)
						zperr( "CRC");
					else {
						Firstsec=FALSE;
						return sectcurr;
					}
				}
				else if (((checksum-firstch)&0377)==0) {
					Firstsec=FALSE;
					return sectcurr;
				}
				else
					zperr( "Checksum");
			}
			else
				zperr("Sector number garbled");
		}
		/* make sure eot really is eot and not just mixmash */
#ifdef NFGVMIN
		else if (firstch==EOT && readline(1)==TIMEOUT)
			return WCEOT;
#else
		else if (firstch==EOT && Lleft==0)
			return WCEOT;
#endif
		else if (firstch==CAN) {
			if (Lastrx==CAN) {
				zperr( "Sender CANcelled");
				return ERROR;
			} else {
				Lastrx=CAN;
				continue;
			}
		}
		else if (firstch==TIMEOUT) {
			if (Firstsec)
				goto humbug;
bilge:
			zperr( "TIMEOUT");
		}
		else
			zperr( "Got 0%o sector header", firstch);

humbug:
		Lastrx=0;
		while(readline(1)!=TIMEOUT)
			;
		if (Firstsec) {
			sendline(Crcflg?WANTCRC:NAK);
			Lleft=0;	/* Do read next time ... */
		} else {
			maxtime=40; sendline(NAK);
			Lleft=0;	/* Do read next time ... */
		}
	}
	/* try to stop the bubble machine. */
	canit();
	return ERROR;
}

#ifndef vax11c
/*
 * This version of readline is reasoably well suited for
 * reading many characters.
 *  (except, currently, for the Regulus version!)
 *
 * timeout is in tenths of seconds
 */
readline(timeout)
int timeout;
{
	register n;
	static char *cdq;	/* pointer for removing chars from linbuf */

	if (--Lleft >= 0) {
		if (Verbose > 8) {
			fprintf(stderr, "%02x ", *cdq&0377);
		}
		return (*cdq++ & 0377);
	}
	n = timeout/10;
	if (n < 2)
		n = 3;
	if (Verbose > 5)
		fprintf(stderr, "Calling read: alarm=%d  Readnum=%d ",
		  n, Readnum);
	if (setjmp(tohere)) {
#ifdef TIOCFLUSH
/*		ioctl(0, TIOCFLUSH, 0); */
#endif
		Lleft = 0;
		if (Verbose>1)
			fprintf(stderr, "Readline:TIMEOUT\n");
		return TIMEOUT;
	}
	signal(SIGALRM, alrm); alarm(n);
	Lleft=read(0, cdq=linbuf, Readnum);
	alarm(0);
	if (Verbose > 5) {
		fprintf(stderr, "Read returned %d bytes\n", Lleft);
	}
	if (Lleft < 1)
		return TIMEOUT;
	--Lleft;
	if (Verbose > 8) {
		fprintf(stderr, "%02x ", *cdq&0377);
	}
	return (*cdq++ & 0377);
}



/*
 * Purge the modem input queue of all characters
 */
purgeline()
{
	Lleft = 0;
#ifdef USG
	ioctl(0, TCFLSH, 0);
#else
	lseek(0, 0L, 2);
#endif
}
#endif


/*
 * Process incoming file information header
 */
procheader(name)
char *name;
{
	register char *openmode, *p, **pp;

	/* set default parameters and overrides */
	openmode = "w";
	Thisbinary = (!Rxascii) || Rxbinary;
	if (Lzmanag)
		zmanag = Lzmanag;

	/*
	 *  Process ZMODEM remote file management requests
	 */
	if (!Rxbinary && zconv == ZCNL)	/* Remote ASCII override */
		Thisbinary = 0;
	if (zconv == ZCBIN)	/* Remote Binary override */
		Thisbinary = TRUE;
	else if (zmanag == ZMAPND)
		openmode = "a";

#ifndef BIX
	/* Check for existing file */
	if (!Rxclob && (zmanag&ZMMASK) != ZMCLOB && (fout=fopen(name, "r"))) {
		fclose(fout);  return ERROR;
	}
#endif

	Bytesleft = DEFBYTL; Filemode = 0; Modtime = 0L;

	p = name + 1 + strlen(name);
	if (*p) {	/* file coming from Unix or DOS system */
		sscanf(p, "%ld%lo%o", &Bytesleft, &Modtime, &Filemode);
#ifndef vax11c
		if (Filemode & UNIXFILE)
			++Thisbinary;
#endif
		if (Verbose) {
			fprintf(stderr,  "Incoming: %s %ld %lo %o\n",
			  name, Bytesleft, Modtime, Filemode);
		}
	}

#ifdef BIX
	if ((fout=fopen("scratchpad", openmode)) == NULL)
		return ERROR;
	return OK;
#else

	else {		/* File coming from CP/M system */
		for (p=name; *p; ++p)		/* change / to _ */
			if ( *p == '/')
				*p = '_';

		if ( *--p == '.')		/* zap trailing period */
			*p = 0;
	}

#ifndef vax11c
	if (!Zmodem && MakeLCPathname && !IsAnyLower(name)
	  && !(Filemode&UNIXFILE))
		uncaps(name);
#endif
	if (Topipe > 0) {
		sprintf(Pathname, "%s %s", Progname+2, name);
		if (Verbose)
			fprintf(stderr,  "Topipe: %s %s\n",
			  Pathname, Thisbinary?"BIN":"ASCII");
#ifndef vax11c
		if ((fout=popen(Pathname, "w")) == NULL)
			return ERROR;
#endif
	} else {
		strcpy(Pathname, name);
		if (Verbose) {
			fprintf(stderr,  "Receiving %s %s %s\n",
			  name, Thisbinary?"BIN":"ASCII", openmode);
		}
		checkpath(name);
		if (Nflag)
			name = "/dev/null";
#ifndef vax11c
		if (name[0] == '!' || name[0] == '|') {
			if ( !(fout = popen(name+1, "w"))) {
				return ERROR;
			}
			Topipe = -1;  return(OK);
		}
#endif
#ifdef MD
		fout = fopen(name, openmode);
		if ( !fout)
			if (make_dirs(name))
				fout = fopen(name, openmode);
#else
		fout = fopen(name, openmode);
#endif
		if ( !fout)
			return ERROR;
	}
	return OK;
#endif /* BIX */
}

#ifdef MD
/*
 *  Directory-creating routines from Public Domain TAR by John Gilmore
 */

/*
 * After a file/link/symlink/dir creation has failed, see if
 * it's because some required directory was not present, and if
 * so, create all required dirs.
 */
make_dirs(pathname)
register char *pathname;
{
	register char *p;		/* Points into path */
	int madeone = 0;		/* Did we do anything yet? */
	int save_errno = errno;		/* Remember caller's errno */
	char *strchr();

	if (errno != ENOENT)
		return 0;		/* Not our problem */

	for (p = strchr(pathname, '/'); p != NULL; p = strchr(p+1, '/')) {
		/* Avoid mkdir of empty string, if leading or double '/' */
		if (p == pathname || p[-1] == '/')
			continue;
		/* Avoid mkdir where last part of path is '.' */
		if (p[-1] == '.' && (p == pathname+1 || p[-2] == '/'))
			continue;
		*p = 0;				/* Truncate the path there */
		if ( !mkdir(pathname, 0777)) {	/* Try to create it as a dir */
			vfile("Made directory %s\n", pathname);
			madeone++;		/* Remember if we made one */
			*p = '/';
			continue;
		}
		*p = '/';
		if (errno == EEXIST)		/* Directory already exists */
			continue;
		/*
		 * Some other error in the mkdir.  We return to the caller.
		 */
		break;
	}
	errno = save_errno;		/* Restore caller's errno */
	return madeone;			/* Tell them to retry if we made one */
}

#if (MD != 2)
#define	TERM_SIGNAL(status)	((status) & 0x7F)
#define TERM_COREDUMP(status)	(((status) & 0x80) != 0)
#define TERM_VALUE(status)	((status) >> 8)
/*
 * Make a directory.  Compatible with the mkdir() system call on 4.2BSD.
 */
mkdir(dpath, dmode)
char *dpath;
int dmode;
{
	int cpid, status;
	struct stat statbuf;

	if (stat(dpath,&statbuf) == 0) {
		errno = EEXIST;		/* Stat worked, so it already exists */
		return -1;
	}

	/* If stat fails for a reason other than non-existence, return error */
	if (errno != ENOENT) return -1; 

	switch (cpid = fork()) {

	case -1:			/* Error in fork() */
		return(-1);		/* Errno is set already */

	case 0:				/* Child process */
		/*
		 * Cheap hack to set mode of new directory.  Since this
		 * child process is going away anyway, we zap its umask.
		 * FIXME, this won't suffice to set SUID, SGID, etc. on this
		 * directory.  Does anybody care?
		 */
		status = umask(0);	/* Get current umask */
		status = umask(status | (0777 & ~dmode)); /* Set for mkdir */
		execl("/bin/mkdir", "mkdir", dpath, (char *)0);
		_exit(-1);		/* Can't exec /bin/mkdir */
	
	default:			/* Parent process */
		while (cpid != wait(&status)) ;	/* Wait for kid to finish */
	}

	if (TERM_SIGNAL(status) != 0 || TERM_VALUE(status) != 0) {
		errno = EIO;		/* We don't know why, but */
		return -1;		/* /bin/mkdir failed */
	}

	return 0;
}
#endif /* MD != 2 */
#endif /* MD */

/*
 * Putsec writes the n characters of buf to receive file fout.
 *  If not in binary mode, carriage returns, and all characters
 *  starting with CPMEOF are discarded.
 */
putsec(buf, n)
char *buf;
register n;
{
	register char *p;

	if (n == 0)
		return OK;
	if (Thisbinary) {
		for (p=buf; --n>=0; )
			putc( *p++, fout);
	}
	else {
		if (Eofseen)
			return OK;
		for (p=buf; --n>=0; ++p ) {
			if ( *p == '\r')
				continue;
			if (*p == CPMEOF) {
				Eofseen=TRUE; return OK;
			}
			putc(*p ,fout);
		}
	}
	return OK;
}

#ifndef vax11c
/*
 *  Send a character to modem.  Small is beautiful.
 */
sendline(c)
{
	char d;

	d = c;
	if (Verbose>6)
		fprintf(stderr, "Sendline: %x\n", c);
	write(1, &d, 1);
}

flushmo() {}
#endif





/* make string s lower case */
uncaps(s)
register char *s;
{
	for ( ; *s; ++s)
		if (isupper(*s))
			*s = tolower(*s);
}
/*
 * IsAnyLower returns TRUE if string s has lower case letters.
 */
IsAnyLower(s)
register char *s;
{
	for ( ; *s; ++s)
		if (islower(*s))
			return TRUE;
	return FALSE;
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
	Lleft=0;	/* Do read next time ... */
	fflush(stdout);
#endif
}


report(sct)
int sct;
{
	if (Verbose>1)
		fprintf(stderr,"%03d%c",sct,sct%10? ' ' : '\r');
}

/*
 * If called as [-][dir/../]vrzCOMMAND set Verbose to 1
 * If called as [-][dir/../]rzCOMMAND set the pipe flag
 * If called as rb use YMODEM protocol
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
	if (s[0]=='r' && s[1]=='z')
		Batch = TRUE;
	if (s[0]=='r' && s[1]=='b')
		Batch = Nozmodem = TRUE;
	if (s[2] && s[0]=='r' && s[1]=='b')
		Topipe = 1;
	if (s[2] && s[0]=='r' && s[1]=='z')
		Topipe = 1;
}

/*
 * Totalitarian Communist pathname processing
 */
checkpath(name)
char *name;
{
	if (Restricted) {
		if (fopen(name, "r") != NULL) {
			canit();
			fprintf(stderr, "\r\nrz: %s exists\n", name);
			bibi(-1);
		}
		/* restrict pathnames to current tree or uucppublic */
		if ( substr(name, "../")
		 || (name[0]== '/' && strncmp(name, PUBDIR, strlen(PUBDIR))) ) {
			canit();
			fprintf(stderr,"\r\nrz:\tSecurity Violation\r\n");
			bibi(-1);
		}
	}
}

/*
 * Initialize for Zmodem receive attempt, try to activate Zmodem sender
 *  Handles ZSINIT frame
 *  Return ZFILE if Zmodem filename received, -1 on error,
 *   ZCOMPL if transaction finished,  else 0
 */
tryz()
{
	register c, n;
	register cmdzack1flg;

	if (Nozmodem)		/* Check for "rb" program name */
		return 0;


	for (n=Zmodem?15:5; --n>=0; ) {
		/* Set buffer length (0) and capability flags */
#ifdef SEGMENTS
		stohdr(SEGMENTS*1024L);
#else
		stohdr(0L);
#endif
#ifdef CANBREAK
		Txhdr[ZF0] = CANFC32|CANFDX|CANOVIO|CANBRK;
#else
		Txhdr[ZF0] = CANFC32|CANFDX|CANOVIO;
#endif
		if (Zctlesc)
			Txhdr[ZF0] |= TESCCTL;
		Txhdr[ZF0] |= CANRLE;
		Txhdr[ZF1] = CANVHDR;
		/* tryzhdrtype may == ZRINIT */
		zshhdr(4,tryzhdrtype, Txhdr);
		if (tryzhdrtype == ZSKIP)	/* Don't skip too far */
			tryzhdrtype = ZRINIT;	/* CAF 8-21-87 */
again:
		switch (zgethdr(Rxhdr, 0)) {
		case ZRQINIT:
			if (Rxhdr[ZF3] & 0x80)
				Usevhdrs = 1;	/* we can var header */
			continue;
		case ZEOF:
			continue;
		case TIMEOUT:
			continue;
		case ZFILE:
			zconv = Rxhdr[ZF0];
			zmanag = Rxhdr[ZF1];
			ztrans = Rxhdr[ZF2];
			if (Rxhdr[ZF3] & ZCANVHDR)
				Usevhdrs = TRUE;
			tryzhdrtype = ZRINIT;
			c = zrdata(secbuf, 1024);
			mode(3);
			if (c == GOTCRCW)
				return ZFILE;
			zshhdr(4,ZNAK, Txhdr);
			goto again;
		case ZSINIT:
			Zctlesc = TESCCTL & Rxhdr[ZF0];
			if (zrdata(Attn, ZATTNLEN) == GOTCRCW) {
				stohdr(1L);
				zshhdr(4,ZACK, Txhdr);
				goto again;
			}
			zshhdr(4,ZNAK, Txhdr);
			goto again;
		case ZFREECNT:
			stohdr(getfree());
			zshhdr(4,ZACK, Txhdr);
			goto again;
		case ZCOMMAND:
#ifdef vax11c
			return ERROR;
#else
			cmdzack1flg = Rxhdr[ZF0];
			if (zrdata(secbuf, 1024) == GOTCRCW) {
				if (cmdzack1flg & ZCACK1)
					stohdr(0L);
				else
					stohdr((long)sys2(secbuf));
				purgeline();	/* dump impatient questions */
				do {
					zshhdr(4,ZCOMPL, Txhdr);
				}
				while (++errors<20 && zgethdr(Rxhdr,1) != ZFIN);
				ackbibi();
				if (cmdzack1flg & ZCACK1)
					exec2(secbuf);
				return ZCOMPL;
			}
			zshhdr(4,ZNAK, Txhdr); goto again;
#endif
		case ZCOMPL:
			goto again;
		default:
			continue;
		case ZFIN:
			ackbibi(); return ZCOMPL;
		case ZCAN:
			return ERROR;
		}
	}
	return 0;
}

/*
 * Receive 1 or more files with ZMODEM protocol
 */
rzfiles()
{
	register c;

	for (;;) {
		switch (c = rzfile()) {
		case ZEOF:
		case ZSKIP:
			switch (tryz()) {
			case ZCOMPL:
				return OK;
			default:
				return ERROR;
			case ZFILE:
				break;
			}
			continue;
		default:
			return c;
		case ERROR:
			return ERROR;
		}
	}
}

/*
 * Receive a file with ZMODEM protocol
 *  Assumes file name frame is in secbuf
 */
rzfile()
{
	register c, n;
	long rxbytes;

	Eofseen=FALSE;
	if (procheader(secbuf) == ERROR) {
		return (tryzhdrtype = ZSKIP);
	}

	n = 20; rxbytes = 0l;

	for (;;) {
#ifdef SEGMENTS
		chinseg = 0;
#endif
		stohdr(rxbytes);
		zshhdr(4,ZRPOS, Txhdr);
nxthdr:
		switch (c = zgethdr(Rxhdr, 0)) {
		default:
			vfile("rzfile: zgethdr returned %d", c);
			return ERROR;
		case ZNAK:
		case TIMEOUT:
#ifdef SEGMENTS
			putsec(secbuf, chinseg);
			chinseg = 0;
#endif
			if ( --n < 0) {
				vfile("rzfile: zgethdr returned %d", c);
				return ERROR;
			}
		case ZFILE:
			zrdata(secbuf, 1024);
			continue;
		case ZEOF:
#ifdef SEGMENTS
			putsec(secbuf, chinseg);
			chinseg = 0;
#endif
			if (rclhdr(Rxhdr) != rxbytes) {
				/*
				 * Ignore eof if it's at wrong place - force
				 *  a timeout because the eof might have gone
				 *  out before we sent our zrpos.
				 */
				errors = 0;  goto nxthdr;
			}
			if (closeit()) {
				tryzhdrtype = ZFERR;
				vfile("rzfile: closeit returned <> 0");
				return ERROR;
			}
			vfile("rzfile: normal EOF");
			return c;
		case ERROR:	/* Too much garbage in header search error */
#ifdef SEGMENTS
			putsec(secbuf, chinseg);
			chinseg = 0;
#endif
			if ( --n < 0) {
				vfile("rzfile: zgethdr returned %d", c);
				return ERROR;
			}
			zmputs(Attn);
			continue;
		case ZSKIP:
#ifdef SEGMENTS
			putsec(secbuf, chinseg);
			chinseg = 0;
#endif
			Modtime = 1;
			closeit();
			vfile("rzfile: Sender SKIPPED file");
			return c;
		case ZDATA:
			if (rclhdr(Rxhdr) != rxbytes) {
				if ( --n < 0) {
					return ERROR;
				}
#ifdef SEGMENTS
				putsec(secbuf, chinseg);
				chinseg = 0;
#endif
				zmputs(Attn);  continue;
			}
moredata:
			if (Verbose>1)
				fprintf(stderr, "\r%7ld ZMODEM%s    ",
				  rxbytes, Crc32r?" CRC-32":"");
#ifdef SEGMENTS
			if (chinseg >= (1024 * SEGMENTS)) {
				putsec(secbuf, chinseg);
				chinseg = 0;
			}
			switch (c = zrdata(secbuf+chinseg, 1024))
#else
			switch (c = zrdata(secbuf, 1024))
#endif
			{
			case ZCAN:
#ifdef SEGMENTS
				putsec(secbuf, chinseg);
				chinseg = 0;
#endif
				vfile("rzfile: zgethdr returned %d", c);
				return ERROR;
			case ERROR:	/* CRC error */
#ifdef SEGMENTS
				putsec(secbuf, chinseg);
				chinseg = 0;
#endif
				if ( --n < 0) {
					vfile("rzfile: zgethdr returned %d", c);
					return ERROR;
				}
				zmputs(Attn);
				continue;
			case TIMEOUT:
#ifdef SEGMENTS
				putsec(secbuf, chinseg);
				chinseg = 0;
#endif
				if ( --n < 0) {
					vfile("rzfile: zgethdr returned %d", c);
					return ERROR;
				}
				continue;
			case GOTCRCW:
				n = 20;
#ifdef SEGMENTS
				chinseg += Rxcount;
				putsec(secbuf, chinseg);
				chinseg = 0;
#else
				putsec(secbuf, Rxcount);
#endif
				rxbytes += Rxcount;
				stohdr(rxbytes);
				zshhdr(4,ZACK, Txhdr);
				sendline(XON);
				goto nxthdr;
			case GOTCRCQ:
				n = 20;
#ifdef SEGMENTS
				chinseg += Rxcount;
#else
				putsec(secbuf, Rxcount);
#endif
				rxbytes += Rxcount;
				stohdr(rxbytes);
				zshhdr(4,ZACK, Txhdr);
				goto moredata;
			case GOTCRCG:
				n = 20;
#ifdef SEGMENTS
				chinseg += Rxcount;
#else
				putsec(secbuf, Rxcount);
#endif
				rxbytes += Rxcount;
				goto moredata;
			case GOTCRCE:
				n = 20;
#ifdef SEGMENTS
				chinseg += Rxcount;
#else
				putsec(secbuf, Rxcount);
#endif
				rxbytes += Rxcount;
				goto nxthdr;
			}
		}
	}
}

/*
 * Send a string to the modem, processing for \336 (sleep 1 sec)
 *   and \335 (break signal)
 */
zmputs(s)
char *s;
{
	register c;

	while (*s) {
		switch (c = *s++) {
		case '\336':
			sleep(1); continue;
		case '\335':
			sendbrk(); continue;
		default:
			sendline(c);
		}
	}
}

/*
 * Close the receive dataset, return OK or ERROR
 */
closeit()
{
	time_t time();

#ifndef vax11c
	if (Topipe) {
		if (pclose(fout)) {
			return ERROR;
		}
		return OK;
	}
#endif
	if (fclose(fout)==ERROR) {
		fprintf(stderr, "file close ERROR\n");
		return ERROR;
	}
#ifndef vax11c
	if (Modtime) {
		timep[0] = time(NULL);
		timep[1] = Modtime;
		utime(Pathname, timep);
	}
#endif
	if ((Filemode&S_IFMT) == S_IFREG)
		chmod(Pathname, (07777 & Filemode));
	return OK;
}

/*
 * Ack a ZFIN packet, let byegones be byegones
 */
ackbibi()
{
	register n;

	vfile("ackbibi:");
	Readnum = 1;
	stohdr(0L);
	for (n=3; --n>=0; ) {
		purgeline();
		zshhdr(4,ZFIN, Txhdr);
		switch (readline(100)) {
		case 'O':
			readline(1);	/* Discard 2nd 'O' */
			vfile("ackbibi complete");
			return;
		case RCDO:
			return;
		case TIMEOUT:
		default:
			break;
		}
	}
}



/*
 * Local console output simulation
 */
bttyout(c)
{
	if (Verbose || Fromcu)
		putc(c, stderr);
}

#ifndef vax11c
/*
 * Strip leading ! if present, do shell escape. 
 */
sys2(s)
register char *s;
{
	if (*s == '!')
		++s;
	return system(s);
}
/*
 * Strip leading ! if present, do exec.
 */
exec2(s)
register char *s;
{
	if (*s == '!')
		++s;
	mode(0);
	execl("/bin/sh", "sh", "-c", s);
}
#endif
/* End of rz.c */
