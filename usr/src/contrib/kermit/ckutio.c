char *ckxv = "Unix tty I/O, 4C(037), 31 Jul 85";

/*  C K U T I O  */

/* C-Kermit interrupt, terminal control & i/o functions for Unix systems */

/*
 Author: Frank da Cruz (SY.FDC@CU20B),
 Columbia University Center for Computing Activities, January 1985.
 Copyright (C) 1985, Trustees of Columbia University in the City of New York.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as it is not sold for profit, provided this
 copyright notice is retained. 
*/
/* Includes for all Unixes (conditional includes come later) */

#include <sys/types.h>			/* Types */
#include <sys/dir.h>			/* Directory */
#include <sys/param.h>		
#include <ctype.h>			/* Character types */
#include <stdio.h>			/* Unix Standard i/o */
#include <signal.h>			/* Interrupts */
#include <setjmp.h>			/* Longjumps */
#include "ckcdeb.h"			/* Typedefs, formats for debug() */

/* Maximum length for the name of a tty device */

#ifndef DEVNAMLEN
#define DEVNAMLEN 25
#endif

/* 4.1 BSD support added by Charles E. Brooks, EDN-VAX */
/* Fortune 16:32 For:Pro 1.7 support mostly like 4.1, added by J-P Dumas */

#ifdef BSD4
#define ANYBSD
#undef DIRSIZ
#define DIRSIZ (sizeof(struct direct))
#ifdef MAXNAMLEN
#define BSD42
char *ckxsys = " 4.2 BSD";
#else
#ifdef FT17
#define BSD41
char *ckxsys = " For:Pro Fortune 1.7";
#else
#define BSD41
#ifndef C70
char *ckxsys = " 4.1 BSD";
#endif
#endif
#endif
#endif

/* 2.9bsd support contributed by Bradley Smith, UCLA */
#ifdef BSD29
#define ANYBSD
char *ckxsys = " 2.9 BSD";
#endif

/*
 Version 7 UNIX support contributed by Gregg Wonderly,
 Oklahoma State University:  gregg@okstate.csnet
*/
#ifdef	V7
char *ckxsys = " Version 7 UNIX (tm)";
#endif	V7

/* BBN C70 support from Frank Wancho, WANCHO@SIMTEL20 */
#ifdef C70
char *ckxsys = " BBN C/70";
#endif

/* Amdahl UTS 2.4 (v7 derivative) for IBM 370 series compatible mainframes */
/* Contributed by Garard Gaye, Jean-Pierre Dumas, DUMAS@SUMEX-AIM. */
#ifdef UTS24
char *ckxsys = " Amdahl UTS 2.4";
#endif

/* Pro/Venix Version 1.x support from Columbia U. */
#ifdef PROVX1
char *ckxsys = " Pro-3xx Venix v1";
#endif

/* Tower support contributed by John Bray, Auburn, Alabama */
#ifdef TOWER1
char *ckxsys = " NCR Tower 1632, OS 1.02";
#endif

/* Sys III/V, Xenix, PC/IX support by Herm Fischer, Encino, CA */
#ifdef UXIII
#ifdef XENIX
char *ckxsys = " Xenix/286";
#else
#ifdef PCIX
char *ckxsys = " PC/IX";
#else
#ifdef ISIII
char *ckxsys = " Interactive Systems Corp System III";
#else
char *ckxsys = " AT&T System III/System V";
#endif
#endif
#endif
#endif

/* Features... */

/* Do own buffering, using unbuffered read() calls... */
#ifdef UXIII
#define MYREAD
#endif

#ifdef BSD42
#define MYREAD
#endif

/*
 Note - KERLD is the Berkeley Unix Berknet line driver, modified to pass
 through all 8  bits, and to allow an arbitrary break character to be set.
 Don't define this symbol unless you have made this modification to your
 4.2BSD kernel!
*/
#ifdef BSD4
/* #define KERLD */  /* <-- note, commented out */
#endif

/*
 Variables available to outside world:

   dftty  -- Pointer to default tty name string, like "/dev/tty".
   dfloc  -- 0 if dftty is console, 1 if external line.
   dfprty -- Default parity
   dfflow -- Default flow control
   ckxech -- Flag for who echoes console typein:
     1 - The program (system echo is turned off)
     0 - The system (or front end, or terminal).
   functions that want to do their own echoing should check this flag
   before doing so.

   flfnam -- Name of lock file, including its path, e.g.,
		"/usr/spool/uucp/LCK..cul0" or "/etc/locks/tty77"
   hasLock -- Flag set if this kermit established a uucp lock.
   inbufc -- number of tty line rawmode unread characters 
		(system III/V unixes)
   backgrd -- Flag indicating program executing in background ( & on 
		end of shell command). Used to ignore INT and QUIT signals.

 Functions for assigned communication line (either external or console tty):

   sysinit()               -- System dependent program initialization
   ttopen(ttname,local,mdmtyp) -- Open the named tty for exclusive access.
   ttclos()                -- Close & reset the tty, releasing any access lock.
   ttpkt(speed,flow)       -- Put the tty in packet mode and set the speed.
   ttvt(speed,flow)        -- Put the tty in virtual terminal mode.
				or in DIALING or CONNECTED modem control state.
   ttinl(dest,max,timo)    -- Timed read line from the tty.
   ttinc(timo)             -- Timed read character from tty.
   myread()		   -- System 3 raw mode bulk buffer read, gives
			   --   subsequent chars one at a time and simulates
			   --   FIONREAD!
   myunrd(c)		   -- Places c back in buffer to be read (one only)
   ttchk()                 -- See how many characters in tty input buffer.
   ttxin(n,buf)            -- Read n characters from tty (untimed).
   ttol(string,length)     -- Write a string to the tty.
   ttoc(c)                 -- Write a character to the tty.
   ttflui()                -- Flush tty input buffer.

   ttlock(ttname)	   -- Lock against uucp collisions (Sys III)
   ttunlck()		   -- Unlock "       "     "
   look4lk(ttname)	   -- Check if a lock file exists
*/

/*
Functions for console terminal:

   congm()   -- Get console terminal modes.
   concb(esc) -- Put the console in single-character wakeup mode with no echo.
   conbin(esc) -- Put the console in binary (raw) mode.
   conres()  -- Restore the console to mode obtained by congm().
   conoc(c)  -- Unbuffered output, one character to console.
   conol(s)  -- Unbuffered output, null-terminated string to the console.
   conola(s) -- Unbuffered output, array of strings to the console.
   conxo(n,s) -- Unbuffered output, n characters to the console.
   conchk()  -- Check if characters available at console (bsd 4.2).
		Check if escape char (^\) typed at console (System III/V).
   coninc(timo)  -- Timed get a character from the console.
   conint()  -- Enable terminal interrupts on the console if not background.
   connoi()  -- Disable terminal interrupts on the console if not background.

Time functions

   msleep(m) -- Millisecond sleep
   ztime(&s) -- Return pointer to date/time string
   rtimer() --  Reset timer
   gtimer()  -- Get elapsed time since last call to rtimer()
*/

/* Conditional Includes */

#ifdef FT17
#include <sys/file.h>	  		/* File information */
#endif

#ifndef PROVX1
#include <sys/file.h>	  		/* File information */
#endif

/* System III, System V */

#ifdef UXIII
#include <termio.h>
#include <sys/ioctl.h>
#include <fcntl.h>			/* directory reading for locking */
#include <errno.h>			/* error numbers for system returns */
#endif

/* Not Sys III/V */

#ifndef UXIII
#include <sgtty.h>			/* Set/Get tty modes */
#ifndef PROVX1
#ifndef V7
#ifndef BSD41
#include <sys/time.h>			/* Clock info (for break generation) */
#endif
#endif
#endif
#endif

#ifdef BSD41
#include <sys/timeb.h>			/* BSD 4.1 ... ceb */
#endif

#ifdef TOWER1
#include <sys/timeb.h>			/* Clock info for NCR Tower */
#endif

/* Declarations */

long time();				/* All Unixes should have this... */
extern int errno;			/* System call error return */

/* Special stuff for V7 input buffer peeking */

#ifdef	V7
int kmem[2] = { -1, -1};
char *initrawq(), *qaddr[2]={0,0};
#define CON 0
#define TTY 1
#endif

/* dftty is the device name of the default device for file transfer */
/* dfloc is 0 if dftty is the user's console terminal, 1 if an external line */

#ifdef PROVX1
    char *dftty = "/dev/com1.dout"; /* Only example so far of a system */
    int dfloc = 1;		    /* that goes in local mode by default */
#else
    char *dftty = CTTNAM;		/* Remote by default, use normal */
    int dfloc = 0;			/* controlling terminal name. */
#endif

    int dfprty = 0;			/* Parity (0 = none) */
    int dfflow = 1;			/* Xon/Xoff flow control */
    int backgrd = 0;			/* Assume in foreground (no '&' ) */

int ckxech = 0; /* 0 if system normally echoes console characters, else 1 */

/* Declarations of variables global within this module */

static long tcount;			/* Elapsed time counter */

static char *brnuls = "\0\0\0\0\0\0\0"; /* A string of nulls */

static jmp_buf sjbuf, jjbuf;		/* Longjump buffer */
static int lkf = 0,			/* Line lock flag */
    conif = 0,				/* Console interrupts on/off flag */
    cgmf = 0,				/* Flag that console modes saved */
    xlocal = 0,				/* Flag for tty local or remote */
    ttyfd = -1;				/* TTY file descriptor */
static char escchr;			/* Escape or attn character */

/* Special line discipline, 4.2bsd only, and only with kernel mods... */
#ifdef KERLD
    static int kerld = 1;		/* Special Kermit line discipline... */
    struct tchars oldc, newc;		/* Special characters */
    int ld = NETLDISC;			/* Really a hack to "Berknet" l.d. */
    int oldld;				/* Old discipline */
#else
    static int kerld = 0;		/* Not selected, no special l.d. */
#endif

#ifdef BSD42
    static struct timeval tv;		/* For getting time, from sys/time.h */
    static struct timezone tz;
#endif

#ifdef BSD29
    static struct timeval tv;		/* For getting time, from sys/time.h */
    static struct timezone tz;		/* Same as 4.2 */
#endif

#ifdef BSD41
    static long clock;			/* For getting time from sys/time.h */
    static struct timeb ftp;		/* And from sys/timeb.h */
#endif

#ifdef TOWER1
static long clock;			/* For getting time from sys/time.h */
static struct timeb ftp;		/* And from sys/timeb.h */
#endif

#ifdef V7
static long clock;
#endif

#ifdef UXIII
  static struct termio 			/* sgtty info... */
    ttold, ttraw, tttvt,		/* for communication line */
    ccold, ccraw, cccbrk;		/* and for console */
#else
  static struct sgttyb 			/* sgtty info... */
    ttold, ttraw, tttvt, ttbuf,		/* for communication line */
    ccold, ccraw, cccbrk;		/* and for console */
#endif

static char flfnam[80];			/* uucp lock file path name */
static int hasLock = 0;			/* =1 if this kermit locked uucp */
static int inbufc = 0;			/* stuff for efficient SIII raw line */
static int ungotn = -1;			/* pushback to unread character */
static int conesc = 0;			/* set to 1 if esc char (^\) typed */

static int ttlock();			/* definition of ttlock subprocedure */
static int ttunlck();			/* and unlock subprocedure */
static char ttnmsv[DEVNAMLEN];		/* copy of open path for tthang */

/*  S Y S I N I T  --  System-dependent program initialization.  */

sysinit() {

/* for now, nothing... */

}

/*  T T O P E N  --  Open a tty for exclusive access.  */

/*  Returns 0 on success, -1 on failure.  */
/*
  If called with lcl < 0, sets value of lcl as follows:
  0: the terminal named by ttname is the job's controlling terminal.
  1: the terminal named by ttname is not the job's controlling terminal.
  But watch out: if a line is already open, or if requested line can't
  be opened, then lcl remains (and is returned as) -1.
*/
ttopen(ttname,lcl,modem) char *ttname; int *lcl, modem; {

#ifdef UXIII
    char *ctermid();			/* Wish they all had this! */
#endif
    char *x; extern char* ttyname();
    char cname[DEVNAMLEN];

    if (ttyfd > -1) return(0);		/* If already open, ignore this call */
    xlocal = *lcl;			/* Make this available to other fns */

#ifndef UXIII
    ttyfd = open(ttname,2);		/* Try to open for read/write */
#else
    /* if modem connection, don't wait for carrier */
    ttyfd = open(ttname,O_RDWR | (modem ? O_NDELAY : 0) );
#endif

    if (ttyfd < 0) {			/* If couldn't open, fail. */
	return(-1);
    }
    strncpy(ttnmsv,ttname,DEVNAMLEN);	/* Open, keep copy of name locally. */

/* Caller wants us to figure out if line is controlling tty */

    debug(F111,"ttopen",ttname,*lcl);
    if (*lcl == -1) {
	if (strcmp(ttname,CTTNAM) == 0) {   /* "/dev/tty" always remote */
	    debug(F110," Same as CTTNAM",ttname,0);
	    xlocal = 0;
	} else if (isatty(0)) {		/* Else, if stdin not redirected */
	    x = ttyname(0);		/* then compare its device name */
	    strncpy(cname,x,DEVNAMLEN);	/* (copy from internal static buf) */
	    debug(F110," ttyname(0)",x,0);
	    x = ttyname(ttyfd); 	/* ...with real name of ttname. */
	    xlocal = (strncmp(x,cname,DEVNAMLEN) == 0) ? 0 : 1;
	    debug(F111," ttyname",x,xlocal);
	} else {			/* Else, if stdin redirected... */
#ifdef UXIII
/* Sys III/V provides nice ctermid() function to get name of controlling tty */
    	    ctermid(cname);		/* Get name of controlling terminal */
	    debug(F110," ctermid",cname,0);
	    x = ttyname(ttyfd); 	/* Compare with name of comm line. */
	    xlocal = (strncmp(x,cname,DEVNAMLEN) == 0) ? 0 : 1;
	    debug(F111," ttyname",x,xlocal);
#else
/* Just assume local, so "set speed" and similar commands will work */
/* If not really local, how could it work anyway?... */
	    xlocal = 1;
	    debug(F101," redirected stdin","",xlocal);
#endif
        }
    }    

/* Now check if line is locked -- if so fail, else lock for ourselves */

    lkf = 0;				/* Check lock */
    if (xlocal > 0) {
	if (ttlock(ttname) < 0) {
	    fprintf(stderr,"Exclusive access to %s denied\n",ttname);
	    close(ttyfd); ttyfd = -1;
	    debug(F110," Access denied by lock",ttname,0);
	    return(-1);			/* Not if already locked */
    	} else lkf = 1;
    }

/* Got the line, now set the desired value for local. */

    if (*lcl < 0) *lcl = xlocal;

/* Some special stuff for v7... */

#ifdef	V7
	if (kmem[TTY] < 0) {	/*  If open, then skip this.  */
		qaddr[TTY] = initrawq(ttyfd);	/* Init the queue. */
		if ((kmem[TTY] = open("/dev/kmem", 0)) < 0) {
			fprintf(stderr, "Can't read /dev/kmem in ttopen.\n");
			perror("/dev/kmem");
			exit(1);
		}
	}
#endif	V7

/* Request exclusive access on systems that allow it. */

#ifndef XENIX
/* Xenix exclusive access prevents open(close(...)) from working... */
#ifdef TIOCEXCL
    	if (ioctl(ttyfd,TIOCEXCL, NULL) < 0)
	    fprintf(stderr,"Warning, problem getting exclusive access\n");
#endif
#endif

/* Get tty device settings */

#ifndef UXIII
    gtty(ttyfd,&ttold);			/* Get sgtty info */
    gtty(ttyfd,&ttraw);			/* And a copy of it for packets*/
    gtty(ttyfd,&tttvt);			/* And one for virtual tty service */
#else
    ioctl(ttyfd,TCGETA,&ttold);		/* Same deal for Sys III, Sys V */
    ioctl(ttyfd,TCGETA,&ttraw);
    ioctl(ttyfd,TCGETA,&tttvt);
#endif
    debug(F101,"ttopen, ttyfd","",ttyfd);
    debug(F101," lcl","",*lcl);
    debug(F111," lock file",flfnam,lkf);
    return(0);
}

/*  T T C L O S  --  Close the TTY, releasing any lock.  */

ttclos() {
    if (ttyfd < 0) return(0);		/* Wasn't open. */
    if (xlocal) {
	if (tthang())			/* Hang up phone line */
	    fprintf(stderr,"Warning, problem hanging up the phone\n");
    	if (ttunlck())			/* Release uucp-style lock */
	    fprintf(stderr,"Warning, problem releasing lock\n");
    }
    ttres();				/* Reset modes. */
/* Relinquish exclusive access if we might have had it... */
#ifndef XENIX
#ifdef TIOCEXCL
#ifdef TIOCNXCL
    if (ioctl(ttyfd, TIOCNXCL, NULL) < 0)
    	fprintf(stderr,"Warning, problem relinquishing exclusive access\n");
#endif
#endif
#endif
    close(ttyfd);			/* Close it. */
    ttyfd = -1;				/* Mark it as closed. */
    return(0);
}

/*  T T H A N G -- Hangup phone line */

tthang() {
#ifdef UXIII
    unsigned short ttc_save;
#endif

    if (ttyfd < 0) return(0);		/* Not open. */
#ifdef ANYBSD
    ioctl(ttyfd,TIOCCDTR,0);		/* Clear DTR */
    msleep(500);			/* Let things settle */
    ioctl(ttyfd,TIOCSDTR,0);		/* Restore DTR */
#endif
#ifdef UXIII
    ttc_save = ttraw.c_cflag;
    ttraw.c_cflag &= ~CBAUD;		/* swa: set baud rate to 0 to hangup */
    if (ioctl(ttyfd,TCSETAF,&ttraw) < 0) return(-1); /* do it */
    msleep(100);			/* let things settle */
    ttraw.c_cflag = ttc_save;
#ifndef XENIX		/* xenix cannot do close/open when carrier drops */
				/* following corrects a PC/IX defficiency */
    ttc_save = fcntl(ttyfd,F_GETFL,0);
    close(ttyfd);		/* close/reopen file descriptor */
    if ((ttyfd = open(ttnmsv, ttc_save)) < 0) return(-1);
#endif
    if (ioctl(ttyfd,TCSETAF,&ttraw) < 0) return(-1); /* un-do it */
#endif
    return (0);
}


/*  T T R E S  --  Restore terminal to "normal" mode.  */

ttres() {				/* Restore the tty to normal. */
    if (ttyfd < 0) return(-1);		/* Not open. */
#ifndef UXIII				/* except for sIII, */
    sleep(1);				/* Wait for pending i/o to finish. */
#endif					/*   (sIII does wait in ioctls) */
#ifdef KERLD
    if (kerld) ioctl(ttyfd,TIOCSETD,&oldld); /* Restore old line discipline. */
#endif
#ifdef UXIII
    if (ioctl(ttyfd,TCSETAW,&ttold) < 0) return(-1); /* restore termio stuff */
#else
    if (stty(ttyfd,&ttold) < 0) return(-1); /* Restore sgtty stuff */
#endif
#ifdef KERLD
    if (kerld) ioctl(ttyfd,TIOCSETC,&oldc); /* Restore old special chars. */
#endif

    return(0);
}

/* Exclusive uucp file locking control */
/*
 by H. Fischer, creative non-Bell coding !
 copyright rights for lock modules assigned to Columbia University
*/
static char *
xxlast(s,c) char *s; char c; {		/* Equivalent to strrchr() */
    int i;
    for (i = strlen(s); i > 0; i--)
    	if ( s[i-1] == c ) return( s + (i - 1) );
    return(NULL);	    
}
static
look4lk(ttname) char *ttname; {
    extern char *strcat(), *strcpy();
    char *device, *devname;
    char lockfil[DIRSIZ+1];

#ifdef ISIII
    char *lockdir = "/etc/locks";
#else
#ifdef ATT3BX
    char *lockdir = "/usr/spool/locks";
#else
    char *lockdir = "/usr/spool/uucp";
#endif
#endif

    device = ( (devname=xxlast(ttname,'/')) != NULL ? devname+1 : ttname);

#ifdef ISIII
    (void) strcpy( lockfil, device );
#else
    strcat( strcpy( lockfil, "LCK.." ), device );
#endif

    if (access( lockdir, 04 ) < 0) {	/* read access denied on lock dir */
	fprintf(stderr,"Warning, read access to lock directory denied\n");
	return( 1 );			/* cannot check or set lock file */
    }
	
    strcat(strcat(strcpy(flfnam,lockdir),"/"), lockfil);
    debug(F110,"look4lk",flfnam,0);

    if ( ! access( flfnam, 00 ) ) {	/* print out lock file entry */
	char lckcmd[40] ;
	strcat( strcpy(lckcmd, "ls -l ") , flfnam);
	system(lckcmd);
	if (access(flfnam,02) == 0)
	    printf("(You may type \"! rm %s\" to remove this file)\n",flfnam);
	return( -1 );
    }
    if ( access( lockdir, 02 ) < 0 ) {	/* lock file cannot be written */
	fprintf(stderr,"Warning, write access to lock directory denied\n");
	return( 1 );
    }
    return( 0 );			/* okay to go ahead and lock */
}

/*  T T L O C K  */


static
ttlock(ttyfd) char *ttyfd; {		/* lock uucp if possible */
#ifdef ATT3BX
    FILE *lck_fild;
#endif
    int lck_fil, l4l;
    int pid_buf = getpid();		/* pid to save in lock file */
	
    hasLock = 0;			/* not locked yet */
    l4l = look4lk(ttyfd);
    if (l4l < 0) return (-1);		/* already locked */
    if (l4l == 1) return (0);		/* can't read/write lock directory */
    lck_fil = creat(flfnam, 0444);	/* create lock file ... */
    if (lck_fil < 0) return (-1);	/* create of lockfile failed */
		/* creat leaves file handle open for writing -- hf */
#ifdef ATT3BX
    fprintf((lck_fild = fdopen(lck_fil, "w")), "%10d\n", pid_buf);
    fflush(lck_fild);
#else
    write (lck_fil, &pid_buf, sizeof(pid_buf) ); /* uucp expects int in file */
#endif
    close (lck_fil);
    hasLock = 1;			/* now is locked */
    return(0);
}

/*  T T U N L O C K  */

static
ttunlck() {				/* kill uucp lock if possible */
    if (hasLock) return( unlink( flfnam ) );
}

/*  T T P K T  --  Condition the communication line for packets. */
/*		or for modem dialing */

#define DIALING	4		/* flags (via flow) for modem handling */
#define CONNECT 5

/*  If called with speed > -1, also set the speed.  */

/*  Returns 0 on success, -1 on failure.  */

ttpkt(speed,flow) int speed, flow; {
    int s;
    if (ttyfd < 0) return(-1);		/* Not open. */

#ifdef KERLD
/* Note, KERLD ignores the TANDEM, ECHO, and CRMOD bits */
    if (kerld) {
	ioctl(ttyfd,TIOCGETD,&oldld);	/* Get line discipline */
	ioctl(ttyfd,TIOCGETC,&oldc);	/* Get special chars */
	newc = oldc;			/* Copy special chars */
	newc.t_brkc = '\r';		/* Set CR to be break character */
	if(ioctl(ttyfd,TIOCSETC,&newc) < 0) return(-1);
    }
#endif

    s = ttsspd(speed);			/* Check the speed */

#ifndef UXIII
    if (flow == 1) ttraw.sg_flags |= TANDEM; /* Use XON/XOFF if selected */
    if (flow == 0) ttraw.sg_flags &= ~TANDEM;
    ttraw.sg_flags |= RAW;		/* Go into raw mode */
    ttraw.sg_flags &= ~(ECHO|CRMOD);	/* Use CR for break character */
#ifdef TOWER1
    ttraw.sg_flags &= ~ANYP; 		/* Must tell Tower no parityr */
#endif
    if (s > -1) ttraw.sg_ispeed = ttraw.sg_ospeed = s; /* Do the speed */
    if (stty(ttyfd,&ttraw) < 0) return(-1);	/* Set the new modes. */

#ifdef MYREAD
#ifdef BSD4
/* Try to make reads nonblocking */
    if (kerld == 0) {
	if (fcntl(ttyfd,F_SETFL,fcntl(ttyfd,F_GETFL,0) & FNDELAY) == -1)
    	    return(-1);
	else return(0);
    }
#endif
#endif
#endif

#ifdef UXIII
    if (flow == 1) ttraw.c_iflag |= (IXON|IXOFF);
    if (flow == 0) ttraw.c_iflag &= ~(IXON|IXOFF);

    if (flow == DIALING)  ttraw.c_cflag |= CLOCAL|HUPCL;
    if (flow == CONNECT)  ttraw.c_cflag &= ~CLOCAL;

    ttraw.c_lflag &= ~(ICANON|ECHO);
    ttraw.c_lflag |= ISIG;		/* do check for interrupt */
    ttraw.c_iflag |= (BRKINT|IGNPAR);
    ttraw.c_iflag &= ~(IGNBRK|INLCR|IGNCR|ICRNL|IUCLC|INPCK|ISTRIP|IXANY);
    ttraw.c_oflag &= ~OPOST;
    ttraw.c_cflag &= ~(CSIZE|PARENB);
    ttraw.c_cflag |= (CS8|CREAD);
    ttraw.c_cc[4] = 1;
    ttraw.c_cc[5] = 0;

    if (s > -1) ttraw.c_cflag &= ~CBAUD, ttraw.c_cflag |= s; /* set speed */

    if (ioctl(ttyfd,TCSETAW,&ttraw) < 0) return(-1);  /* set new modes . */
    if (flow == DIALING) {
	if (fcntl(ttyfd,F_SETFL, fcntl(ttyfd, F_GETFL, 0) & ~O_NDELAY) < 0 )
		return(-1);
	close( open(ttnmsv,2) );	/* magic to force mode change!!! */
	}
#endif

#ifdef KERLD
    if (kerld) {
	if (ioctl(ttyfd,TIOCSETD,&ld) < 0)
	    return(-1); /* Set line discpline. */
    }
#endif

    ttflui();				/* Flush any pending input */
    return(0);
}

/*  T T V T -- Condition communication line for use as virtual terminal  */

ttvt(speed,flow) int speed, flow; {
    int s;
    if (ttyfd < 0) return(-1);		/* Not open. */

    s = ttsspd(speed);			/* Check the speed */

#ifndef UXIII
    if (flow == 1) tttvt.sg_flags |= TANDEM; /* XON/XOFF if selected */
    if (flow == 0) tttvt.sg_flags &= ~TANDEM;
    tttvt.sg_flags |= RAW;		/* Raw mode */
#ifdef TOWER1
    tttvt.sg_flags &= ~(ECHO|ANYP);	/* No echo or system III ??? parity */
#else
    tttvt.sg_flags &= ~ECHO;		/* No echo */
#endif    
    if (s > -1) tttvt.sg_ispeed = tttvt.sg_ospeed = s; /* Do the speed */
    if (stty(ttyfd,&tttvt) < 0) return(-1);
#ifdef MYREAD
#ifdef BSD4
/* Make reads nonblocking */
    if (kerld == 0) {
	if (fcntl(ttyfd,F_SETFL,fcntl(ttyfd,F_GETFL,0) & FNDELAY) == -1)
	    return(-1);
	else return(0);
    }
#endif
#endif

#else
    if (flow == 1) tttvt.c_iflag |= (IXON|IXOFF);
    if (flow == 0) tttvt.c_iflag &= ~(IXON|IXOFF);

    if (flow == DIALING)  tttvt.c_cflag |= CLOCAL|HUPCL;
    if (flow == CONNECT)  tttvt.c_cflag &= ~CLOCAL;

    tttvt.c_lflag &= ~(ISIG|ICANON|ECHO);
    tttvt.c_iflag |= (IGNBRK|IGNPAR);
    tttvt.c_iflag &= ~(INLCR|IGNCR|ICRNL|IUCLC|BRKINT|INPCK|ISTRIP|IXANY);
    tttvt.c_oflag &= ~OPOST;
    tttvt.c_cflag &= ~(CSIZE|PARENB);
    tttvt.c_cflag |= (CS8|CREAD);
    tttvt.c_cc[4] = 1;
    tttvt.c_cc[5] = 0;

    if (s > -1) tttvt.c_cflag &= ~CBAUD, tttvt.c_cflag |= s; /* set speed */

    if (ioctl(ttyfd,TCSETAW,&tttvt) < 0) return(-1);  /* set new modes . */
    if (flow == DIALING) {
	if (fcntl(ttyfd,F_SETFL, fcntl(ttyfd, F_GETFL, 0) & ~O_NDELAY) < 0 )
		return(-1);
	close( open(ttnmsv,2) );	/* magic to force mode change!!! */
	}
    return(0);
#endif
}

/*  T T S S P D  --  Return the internal baud rate code for 'speed'.  */

ttsspd(speed) {
    int s, spdok;

    if (speed < 0) return(-1);
	spdok = 1;			/* Assume arg ok */
	switch (speed) {
	    case 0:    s = B0;    break;	/* Just the common ones. */
	    case 110:  s = B110;  break;	/* The others from ttydev.h */
	    case 150:  s = B150;  break;	/* could also be included if */
	    case 300:  s = B300;  break;	/* necessary... */
	    case 600:  s = B600;  break;
	    case 1200: s = B1200; break;
	    case 1800: s = B1800; break;
	    case 2400: s = B2400; break;
	    case 4800: s = B4800; break;
	    case 9600: s = B9600; break;
#ifdef PLEXUS
	    case 19200: s = EXTA; break;
#endif
	    default:
	    	spdok = 0;
		fprintf(stderr,"Unsupported line speed - %d\n",speed);
		fprintf(stderr,"Current speed not changed\n");
		break;
	}	    
	if (spdok) return(s); else return(-1);
 }



/*  T T F L U I  --  Flush tty input buffer */

ttflui() {

#ifndef UXIII
    long n;
#endif
    if (ttyfd < 0) return(-1);		/* Not open. */

    ungotn = -1;			/* Initialize myread() stuff */
    inbufc = 0;

#ifdef UXIII
    if (ioctl(ttyfd,TCFLSH,0) < 0) perror("flush failed");
#else
#ifdef TIOCFLUSH
#if defined(ANYBSD) && !defined(BSD4_4)
    n = FREAD;				/* Specify read queue */
    if (ioctl(ttyfd,TIOCFLUSH,&n) < 0) perror("flush failed");
#else
    if (ioctl(ttyfd,TIOCFLUSH,0) < 0) perror("flush failed");
#endif
#endif
#endif
    return(0);
}

/* Interrupt Functions */


/* Timeout handler for communication line input functions */

timerh() {
    longjmp(sjbuf,1);
}

 
/* Set up terminal interrupts on console terminal */

#ifdef UXIII
esctrp() {				/* trap console escapes (^\) */
    conesc = 1;
    signal(SIGQUIT,SIG_IGN);		/* ignore until trapped */
}
#endif

#ifdef V7
esctrp() {				/* trap console escapes (^\) */
    conesc = 1;
    signal(SIGQUIT,SIG_IGN);		/* ignore until trapped */
}
#endif

#ifdef C70
esctrp() {				/* trap console escapes (^\) */
    conesc = 1;
    signal(SIGQUIT,SIG_IGN);		/* ignore until trapped */
}
#endif

/*  C O N I N T  --  Console Interrupt setter  */

conint(f) int (*f)(); {			/* Set an interrupt trap. */

    if (backgrd) return;		/* must ignore signals in bkgrd */

/*
 Except for special cases below, ignore keyboard quit signal.
 ^\ too easily confused with connect escape, and besides, we don't want
 to leave lock files around.  (Frank Prindle)
*/
    signal(SIGQUIT,SIG_IGN);

#ifdef UXIII
    signal(SIGQUIT,esctrp);		/* console escape in pkt modes */
    if (conesc) {			/* clear out pending escapes */
	conesc = 0;
    }
#endif

#ifdef V7
    signal(SIGQUIT,esctrp);		/* console escape in pkt modes */
    if (conesc) {			/* clear out pending escapes */
	conesc = 0;
    }
#endif

    if (conif) return;			/* Nothing to do if already on. */

/* check if invoked in background -- if so signals set to be ignored */

    if (signal(SIGINT,SIG_IGN) == SIG_IGN) {
	backgrd = 1;			/*   means running in background */
#ifdef UXIII
	signal(SIGQUIT,SIG_IGN);	/*   must leave signals ignored */
#endif
#ifdef V7
	signal(SIGQUIT,SIG_IGN);	/*   must leave signals ignored */
#endif
	return;
    }
    signal(SIGINT,f);			/* Function to trap to on interrupt. */
    signal(SIGHUP,f);			/* Or hangup, so lock file cleared. */
    conif = 1;				/* Flag console interrupts on. */
}


/*  C O N N O I  --  Reset console terminal interrupts */

connoi() {				/* Console-no-interrupts */

    if (backgrd) return;		/* Ignore signals in background */

    signal(SIGINT,SIG_DFL);
    signal(SIGHUP,SIG_DFL);
    signal(SIGQUIT,SIG_DFL);
    conif = 0;				/* Flag interrupt trapping off */
}

/*  myread() -- For use by systems that can do nonblocking read() calls  */
/*
 Returns:
  -1  if no characters available,
  -2  upon error (such as disconnect),
  otherwise value of character (0 or greater)
*/
myread() {
    static int inbuf_item;
    static CHAR inbuf[257];
    CHAR readit;
    
    if (ungotn >= 0) {
	readit = ungotn;
    } else {
        if (inbufc > 0) {
	    readit = inbuf[++inbuf_item];
        } else {
	    if ((inbufc = read(ttyfd,inbuf,256)) == 0) {  /* end of file */
			/* means carrier dropped on modem connection */
		errno = 9999;		/* magic number for no carrier */
		return(-2);		/* end of file has no errno */
		}
	    if (inbufc < 0) return(-2);
	    readit = inbuf[inbuf_item = 0];
	}
        inbufc--;	
    }
    ungotn = -1;
    return(readit);
}

myunrd(ch) CHAR ch; {			/* push back up to one character */
    ungotn = ch;
}

/*  I N I T R A W Q  --  Set up to read /DEV/KMEM for character count.  */

#ifdef	V7
/*
 Used in Version 7 to simulate Berkeley's FIONREAD ioctl call.  This
 eliminates blocking on a read, because we can read /dev/kmem to get the
 number of characters available for raw input.  If your system can't
 or you won't let it read /dev/kmem (the world that is) then you must
 figure out a different way to do the counting of characters available,
 or else replace this by a dummy function that always returns 0.
*/
/*
 * Call this routine as: initrawq(tty)
 * where tty is the file descriptor of a terminal.  It will return
 * (as a char *) the kernel-mode memory address of the rawq character
 * count, which may then be read.  It has the side-effect of flushing
 * input on the terminal.
 */
/*
 * John Mackin, Physiology Dept., University of Sydney (Australia)
 * ...!decvax!mulga!physiol.su.oz!john
 *
 * Permission is hereby granted to do anything with this code, as
 * long as this comment is retained unmodified and no commercial
 * advantage is gained.
 */
#include <a.out.h>
#include <sys/proc.h>

char *initrawq(tty) int tty; {
#ifdef UTS24
    return(0);
#else
#ifdef BSD29
    return(0);
#else
    long lseek();
    static struct nlist nl[] = {
	{PROCNAME},
	{NPROCNAME},
	{""}
    };
    static struct proc *pp;
    char *malloc(), *qaddr, *p, c;
    int m, pid, me;
    NPTYPE xproc;			/* Its type is defined in makefile. */
    int catch();

    me = getpid();
    if ((m = open("/dev/kmem", 0)) < 0) err("kmem");
    nlist(BOOTNAME, nl);
    if (nl[0].n_type == 0) err("proc array");
 
    if (nl[1].n_type == 0) err("nproc");

    lseek(m, (long)(nl[1].n_value), 0);
    read (m, &xproc, sizeof(xproc));
    signal(SIGALRM, catch);
    if ((pid = fork()) == 0) {
	while(1)
	    read(tty, &c, 1);
    }
    alarm(2);
 
    if(setjmp(jjbuf) == 0) {
	while(1)
	    read(tty, &c, 1);
    }
    signal(SIGALRM, SIG_DFL);

#ifdef DIRECT
    pp = (struct proc *) nl[0].n_value;
#else 
    if (lseek(m, (long)(nl[0].n_value), 0) < 0L) err("seek");
    if (read(m, &pp, sizeof(pp)) != sizeof(pp))  err("no read of proc ptr");
#endif
    lseek(m, (long)(nl[1].n_value), 0);
    read(m, &xproc, sizeof(xproc));

    if (lseek(m, (long)pp, 0) < 0L) err("Can't seek to proc");
    if ((p = malloc(xproc * sizeof(struct proc))) == NULL) err("malloc");
    if (read(m,p,xproc * sizeof(struct proc)) != xproc*sizeof(struct proc))
    	err("read proc table");
    for (pp = (struct proc *)p; xproc > 0; --xproc, ++pp) {
	if (pp -> p_pid == (short) pid) goto iout;
    }
    err("no such proc");
 
iout:
    close(m);
    qaddr = (char *)(pp -> p_wchan);
    free (p);
    kill(pid, SIGKILL);
    wait((int *)0);		/* Destroy the ZOMBIEs! */
    return (qaddr);
#endif
#endif
}

/*  More V7-support functions...  */

static
err(s) char *s; {
    char buf[200];

    sprintf(buf, "fatal error in initrawq: %s", s);
    perror(buf);
    doexit(1);
}

static
catch() {
    longjmp(jjbuf, -1);
}


/*  G E N B R K  --  Simulate a modem break.  */

#define	BSPEED	B150

genbrk(fn) int fn; {
    struct sgttyb ttbuf;
    int ret, sospeed;

    ret = ioctl(fn, TIOCGETP, &ttbuf);
    sospeed = ttbuf.sg_ospeed;
    ttbuf.sg_ospeed = BSPEED;
    ret = ioctl(fn, TIOCSETP, &ttbuf);
    ret = write(fn, "\0\0\0\0\0\0\0\0\0\0\0\0", 8);
    ttbuf.sg_ospeed = sospeed;
    ret = ioctl(fn, TIOCSETP, &ttbuf);
    ret = write(fn, "@", 1);
    return;
}
#endif	V7

/*  T T C H K  --  Tell how many characters are waiting in tty input buffer  */

ttchk() {
    int x; long n;
#ifdef FIONREAD
    x = ioctl(ttyfd, FIONREAD, &n);	/* Berkeley and maybe some others */
    debug(F101,"ttchk","",n);
    return((x < 0) ? 0 : n);
#else
#ifdef	V7
    lseek(kmem[TTY], (long) qaddr[TTY], 0);
    x = read(kmem[TTY], &n, sizeof(int));
    return((x == sizeof(int))? n: 0);
#else	V7
#ifdef UXIII
    return(inbufc + (ungotn >= 0) );	
#else
#ifdef C70
    return(inbufc + (ungotn >= 0) );
#else
#ifdef PROVX1
    x = ioctl(ttyfd, TIOCQCNT, &ttbuf);
    n = ttbuf.sg_ispeed & 0377;
    return((x < 0) ? 0 : n);
#else
    return(0);
#endif
#endif
#endif
#endif
#endif
}


/*  T T X I N  --  Get n characters from tty input buffer  */

/*  Returns number of characters actually gotten, or -1 on failure  */

/*  Intended for use only when it is known that n characters are actually */
/*  Available in the input buffer.  */

ttxin(n,buf) int n; char *buf; {
    int x;
    CHAR c;

#ifdef MYREAD
    for( x = 0; (x > -1) && (x < n); buf[x++] = myread() );
#else
    debug(F101,"ttxin: n","",n);
    x = read(ttyfd,buf,n);
    debug(F101," x","",x);
#endif
    if (x > 0) buf[x] = '\0';
    if (x < 0) x = -1;
    return(x);
}

/*  T T O L  --  Similar to "ttinl", but for writing.  */

ttol(s,n) int n; char *s; {
    int x;
    if (ttyfd < 0) return(-1);		/* Not open. */
    x = write(ttyfd,s,n);
    debug(F111,"ttol",s,n);
    if (x < 0) debug(F101,"ttol failed","",x);
    return(x);
}


/*  T T O C  --  Output a character to the communication line  */

ttoc(c) char c; {
    if (ttyfd < 0) return(-1);		/* Not open. */
    return(write(ttyfd,&c,1));
}

/*  T T I N L  --  Read a record (up to break character) from comm line.  */
/*
  If no break character encountered within "max", return "max" characters,
  with disposition of any remaining characters undefined.  Otherwise, return
  the characters that were read, including the break character, in "dest" and
  the number of characters read as the value of function, or 0 upon end of
  file, or -1 if an error occurred.  Times out & returns error if not completed
  within "timo" seconds.
*/

ttinl(dest,max,timo,eol) int max,timo; char *dest; char eol; {
    int x, y;
    CHAR c;

    if (ttyfd < 0) return(-1);		/* Not open. */
    if (timo <= 0) {			/* Untimed read... */

#ifdef MYREAD
	for (x = y = 0; (x < max) && (c != eol); x++) {
	     while ((y = myread()) == -1) ;
	     if (y == -2) return(-1);
	     dest[x] = y & 0377;
	}
#else
	x = read(ttyfd,dest,max);	/* Try to read. */
#endif
	return(x);			/* Return the count. */
    }

/* Timed read... */

    signal(SIGALRM,timerh);		/* Set up timeout action. */
    alarm(timo);			/* Set the timer. */
    if (setjmp(sjbuf))			/* Do this if timer went off. */
    	x = -1;
    else if (kerld) {			/* Efficient Kermit line discipline */
	x = read(ttyfd,dest,max);	/* for 4.2bsd only... */
    } else {				/* Normal case... */
	for (x = c = y = 0; (x < max) && (c != eol); x++) {
#ifdef MYREAD
    	    while ((y = myread()) == -1) /* Use own buffering if we can */
	    	;
	    if (y == -2) y++;
	    c = y & 0377;
#else
	    while ((y = read(ttyfd,&c,1)) == 0) /* Else call system */
	    	;			/* ...for each character. */
#endif
	    if (y < 0) {
		alarm(0);		/* Error, turn off timer, */
		signal(SIGALRM,SIG_DFL); /* and associated interrupt. */
		return(y);		/* Return the error indication. */
	    }
    	    dest[x] = c;
	}
	x++;
    }
    alarm(0);				/* Success, turn off timer, */
    signal(SIGALRM,SIG_DFL);		/* and associated interrupt. */
    return(x);				/* Return the count. */
}

/*  T T I N C --  Read a character from the communication line  */

ttinc(timo) int timo; {
    int n = 0;
    CHAR ch = 0;

    if (ttyfd < 0) return(-1);		/* Not open. */
    if (timo <= 0) {			/* Untimed. */
#ifdef MYREAD
    	/* comm line failure returns -1 thru myread, so no &= 0377 */
    	while ((n = myread()) == -1) ;	/* Wait for a character... */
	if (n == -2) n++;
	return( n );
#else
	while ((n = read(ttyfd,&ch,1)) == 0) ; /* Wait for a character. */
	return( (n > 0) ? (ch & 0377) : n );
#endif
    }

    signal(SIGALRM,timerh);		/* Timed, set up timer. */
    alarm(timo);
    if (setjmp(sjbuf)) {
	n = -1;
    } else {
#ifdef MYREAD
    	while ((n = myread()) == -1) ;	/* If managing own buffer... */
	if (n == -2) {
	    n++;
	} else {
	    ch = n;
	    n = 1;	
	}
#else
    	n = read(ttyfd,&ch,1);		/* Otherwise call the system. */
#endif
    }
    alarm(0);				/* Turn off timer, */
    signal(SIGALRM,SIG_DFL);		/* and interrupt. */
    return( (n > 0) ? (ch & 0377) : n ); /* Return char or -1. */
}

/*  T T S N D B  --  Send a BREAK signal  */

ttsndb() {
    int x; long n; char spd;

    if (ttyfd < 0) return(-1);		/* Not open. */

#ifdef PROVX1
    gtty(ttyfd,&ttbuf);			/* Get current tty flags */
    spd = ttbuf.sg_ospeed;		/* Save speed */
    ttbuf.sg_ospeed = B50;		/* Change to 50 baud */
    stty(ttyfd,&ttbuf);			/*  ... */
    write(ttyfd,brnuls,3);		/* Send 3 nulls */
    ttbuf.sg_ospeed = spd;		/* Restore speed */
    stty(ttyfd,&ttbuf);			/*  ... */
    return(0);
#else
#ifdef UXIII
    if (ioctl(ttyfd,TCSBRK,(char *)0) < 0) {	/* Send a BREAK */
    	perror("Can't send BREAK");
	return(-1);
    }
    return(0);
#else
#ifdef ANYBSD
#if !defined(BSD4_4)
    n = FWRITE;				/* Flush output queue. */
#endif
    ioctl(ttyfd,TIOCFLUSH,&n); 		/* Ignore any errors.. */
    if (ioctl(ttyfd,TIOCSBRK,(char *)0) < 0) {	/* Turn on BREAK */
    	perror("Can't send BREAK");
	return(-1);
    }
    x = msleep(275);			/* Sleep for so many milliseconds */
    if (ioctl(ttyfd,TIOCCBRK,(char *)0) < 0) {	/* Turn off BREAK */
	perror("BREAK stuck!!!");
	doexit(1);			/* Get out, closing the line. */
					/*   with exit status = 1 */
    }
    return(x);
#else
#ifdef	V7
    genbrk(ttyfd);			/* Simulate a BREAK */
    return(x);
#endif
#endif
#endif
#endif
}

/*  M S L E E P  --  Millisecond version of sleep().  */

/*
 Intended only for small intervals.  For big ones, just use sleep().
*/

msleep(m) int m; {

#ifdef PROVX1
    sleep(-((m * 60 + 500) / 1000));
    return(0);
#endif

#ifdef ANYBSD
    int t1, t3, t4;
#ifdef BSD41
    if (ftime(&ftp) < 0) return(-1);	/* Get current time. */
    t1 = ((ftp.time & 0xff) * 1000) + ftp.millitm;
    while (1) {
	ftime(&ftp);			/* new time */
	t3 = (((ftp.time & 0xff) * 1000) + ftp.millitm) - t1;
	if (t3 > m) return (t3);
    }
#else
/* 2.9 and 4.1 BSD do it this way */
    if (gettimeofday(&tv, &tz) < 0) return(-1); /* Get current time. */
    t1 = tv.tv_sec;			/* Seconds */

    tv.tv_sec = 0;			/* Use select() */
    tv.tv_usec = m * 1000;
    return(select( 0, (int *)0, (int *)0, (int *)0, &tv) );
#endif
#endif

#ifdef UXIII
#ifdef XENIX
#define CLOCK_TICK 50			/* millisecs per clock tick */
#else
#define CLOCK_TICK 17			/* 1/60 sec */
#endif
    extern long times();
    long t1, t2, tarray[4];
    int t3;

    if ((t1 = times(tarray)) < 0) return(-1);
    while (1) {
	if ((t2 = times(tarray)) < 0) return(-1);
	t3 = ((int)(t2 - t1)) * CLOCK_TICK;
	if (t3 > m) return(t3);
    }
#endif

#ifdef TOWER1
    int t1, t3;
    if (ftime(&ftp) < 0) return(-1);		/* Get current time. */
    t1 = ((ftp.time & 0xff) * 1000) + ftp.millitm;
    while (1) {
	ftime(&ftp);				/* new time */
	t3 = (((ftp.time & 0xff) * 1000) + ftp.millitm) - t1;
	if (t3 > m) return (t3);
    }
#endif
}

/*  R T I M E R --  Reset elapsed time counter  */

rtimer() {
    tcount = time( (long *) 0 );
}


/*  G T I M E R --  Get current value of elapsed time counter in seconds  */

gtimer() {
    int x;
    x = (int) (time( (long *) 0 ) - tcount);
    rtimer();
    return( (x < 0) ? 0 : x );
}


/*  Z T I M E  --  Return date/time string  */

ztime(s) char **s; {

#ifdef UXIII
    extern long time();			/* Sys III/V way to do it */
    char *ctime();
    long clock_storage;

    clock_storage = time( (long *) 0 );
    *s = ctime( &clock_storage );
#endif

#ifdef PROVX1
    int utime[2];			/* Venix way */
    time(utime);
    *s = ctime(utime);
#endif

#ifdef ANYBSD
    char *asctime();			/* Berkeley way */
    struct tm *localtime();
    struct tm *tp;
#ifndef BSD41
    gettimeofday(&tv, &tz);		/* BSD 2.9, 4.2 ... */
    time(&tv.tv_sec);
    tp = localtime(&tv.tv_sec);
#else
    time(&clock);			/* BSD 4.1 ... ceb */
    tp = localtime(&clock);
#endif
    *s = asctime(tp);
#endif

#ifdef TOWER1
    char *asctime();			/* Tower way */
    struct tm *localtime();
    struct tm *tp;

    time(&clock);
    tp = localtime(&clock);
    *s = asctime(tp);
#endif
#ifdef V7
    char *asctime();			/* V7 way */
    struct tm *localtime();
    struct tm *tp;

    time(&clock);
    tp = localtime(&clock);
    *s = asctime(tp);
#endif
}

/*  C O N G M  --  Get console terminal modes.  */

/*
 Saves current console mode, and establishes variables for switching between 
 current (presumably normal) mode and other modes.
*/

congm() {
#ifndef UXIII
     gtty(0,&ccold);			/* Structure for restoring */
     gtty(0,&cccbrk);			/* For setting CBREAK mode */
     gtty(0,&ccraw);			/* For setting RAW mode */
#else
     ioctl(0,TCGETA,&ccold);
     ioctl(0,TCGETA,&cccbrk);
     ioctl(0,TCGETA,&ccraw);
#endif
     cgmf = 1;				/* Flag that we got them. */
}


/*  C O N C B --  Put console in cbreak mode.  */

/*  Returns 0 if ok, -1 if not  */

concb(esc) char esc; {
    int x;
    if (cgmf == 0) congm();		/* Get modes if necessary. */
    escchr = esc;			/* Make this available to other fns */
    ckxech = 1;				/* Program can echo characters */
#ifndef UXIII
    cccbrk.sg_flags |= CBREAK;		/* Set to character wakeup, */
    cccbrk.sg_flags &= ~ECHO;		/* no echo. */
    x = stty(0,&cccbrk);
#else
    cccbrk.c_lflag &= ~(ICANON|ECHO);
    cccbrk.c_cc[0] = 003;		/* interrupt char is control-c */
    cccbrk.c_cc[1] = escchr;		/* escape during packet modes */
    cccbrk.c_cc[4] = 1;
    cccbrk.c_cc[5] = 1;
    x = ioctl(0,TCSETAW,&cccbrk);  	/* set new modes . */
#endif
    if (x > -1) setbuf(stdout,NULL);	/* Make console unbuffered. */
#ifdef	V7
    if (kmem[CON] < 0) {
	qaddr[CON] = initrawq(0);
	if((kmem[CON] = open("/dev/kmem", 0)) < 0) {
	    fprintf(stderr, "Can't read /dev/kmem in concb.\n");
	    perror("/dev/kmem");
	    exit(1);
	}
    }
#endif	V7
    return(x);
}

/*  C O N B I N  --  Put console in binary mode  */

/*  Returns 0 if ok, -1 if not  */

conbin(esc) char esc; {
    if (cgmf == 0) congm();		/* Get modes if necessary. */
    escchr = esc;			/* Make this available to other fns */
    ckxech = 1;				/* Program can echo characters */
#ifndef UXIII
    ccraw.sg_flags |= (RAW|TANDEM);   	/* Set rawmode, XON/XOFF */
    ccraw.sg_flags &= ~(ECHO|CRMOD);  	/* Set char wakeup, no echo */
    return(stty(0,&ccraw));
#else
    ccraw.c_lflag &= ~(ISIG|ICANON|ECHO);
    ccraw.c_iflag |= (BRKINT|IGNPAR);
    ccraw.c_iflag &= ~(IGNBRK|INLCR|IGNCR|ICRNL|IUCLC|IXON|IXANY|IXOFF
			|INPCK|ISTRIP);
    ccraw.c_oflag &= ~OPOST;

/*** Kermit used to put the console in 8-bit raw mode, but some users have
 *** pointed out that this should not be done, since some sites actually
 *** use terminals with parity settings on their Unix systems, and if we
 *** override the current settings and stop doing parity, then their terminals
 *** will display blotches for characters whose parity is wrong.  Therefore,
 *** the following two lines are commented out (Larry Afrin, Clemson U):
 ***
 ***   ccraw.c_cflag &= ~(PARENB|CSIZE);
 ***   ccraw.c_cflag |= (CS8|CREAD);
 ***
 *** Sys III/V sites that have trouble with this can restore these lines.
 ***/
    ccraw.c_cc[4] = 1;
    ccraw.c_cc[5] = 1;
    return(ioctl(0,TCSETAW,&ccraw) );  	/* set new modes . */
#endif
}


/*  C O N R E S  --  Restore the console terminal  */

conres() {
    if (cgmf == 0) return(0);		/* Don't do anything if modes */
#ifndef UXIII				/* except for sIII, */
    sleep(1);				/*  not known! */
#endif					/*   (sIII does wait in ioctls) */
    ckxech = 0;				/* System should echo chars */
#ifndef UXIII
    return(stty(0,&ccold));		/* Restore controlling tty */
#else
    return(ioctl(0,TCSETAW,&ccold));
#endif
}

/*  C O N O C  --  Output a character to the console terminal  */

conoc(c) char c; {
    write(1,&c,1);
}

/*  C O N X O  --  Write x characters to the console terminal  */

conxo(x,s) char *s; int x; {
    write(1,s,x);
}

/*  C O N O L  --  Write a line to the console terminal  */

conol(s) char *s; {
    int len;
    len = strlen(s);
    write(1,s,len);
}

/*  C O N O L A  --  Write an array of lines to the console terminal */

conola(s) char *s[]; {
    int i;
    for (i=0 ; *s[i] ; i++) conol(s[i]);
}

/*  C O N O L L  --  Output a string followed by CRLF  */

conoll(s) char *s; {
    conol(s);
    write(1,"\r\n",2);
}

/*  C O N C H K  --  Return how many characters available at console  */

conchk() {
    int x; long n;

#ifdef PROVX1 
    x = ioctl(0, TIOCQCNT, &ttbuf);
    n = ttbuf.sg_ispeed & 0377;
    return((x < 0) ? 0 : n);
#else
#ifdef V7
    lseek(kmem[CON], (long) qaddr[CON], 0);
    x = read(kmem[CON], &n, sizeof(int));
    return((x == sizeof(int))? n: 0);
#else
#ifdef UXIII
    if (conesc) {			/* Escape typed */
	conesc = 0;
	signal(SIGQUIT,esctrp);		/* Restore escape */
	return(1);
    }
    return(0);
#else
#ifdef C70
    if (conesc) {			/* Escape typed */
	conesc = 0;
	signal(SIGQUIT,esctrp);		/* Restore escape */
	return(1);
    }
    return(0);
#else
#ifdef FIONREAD
    x = ioctl(0, FIONREAD, &n);		/* BSD and maybe some others */
    return((x < 0) ? 0 : n);
#else
    return(0);				/* Others can't do. */
#endif
#endif
#endif
#endif
#endif
}

/*  C O N I N C  --  Get a character from the console  */

coninc(timo) int timo; {
    int n = 0; char ch;
    if (timo <= 0 ) {			/* untimed */
	n = read(0, &ch, 1);		/* Read a character. */
	ch &= 0377;
	if (n > 0) return(ch); 		/* Return the char if read */
	else 
#ifdef UXIII
	    if (n < 0 && errno == EINTR) /* if read was interrupted by QUIT */
		return(escchr);		 /* user entered escape character */
	    else		    /* couldnt be ^c, sigint never returns */
#endif
		return(-1);  		/* Return the char, or -1. */
	}
    signal(SIGALRM,timerh);		/* Timed read, so set up timer */
    alarm(timo);
    if (setjmp(sjbuf)) n = -2;
    else {
	n = read(0, &ch, 1);
	ch &= 0377;
    }
    alarm(0);				/* Stop timing, we got our character */
    signal(SIGALRM,SIG_DFL);
    if (n > 0) return(ch);  
    else
#ifdef UXIII
        if (n == -1 && errno == EINTR)  /* If read interrupted by QUIT, */
	    return(escchr);		/* user entered escape character, */
        else		    		/* can't be ^c, sigint never returns */
#endif
	return(-1);
}
