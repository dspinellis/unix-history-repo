/*****************************************************************************
 *  Copyright 1990, 1992 Free Software Foundation, Inc.
 *
 *   This code was donated by Intel Corp.
 *
 * Intel hereby grants you permission to copy, modify, and 
 * distribute this software and its documentation.  Intel grants
 * this permission provided that the above copyright notice 
 * appears in all copies and that both the copyright notice and
 * this permission notice appear in supporting documentation.  In
 * addition, Intel grants this permission provided that you
 * prominently mark as not part of the original any modifications
 * made to this software or documentation, and that the name of 
 * Intel Corporation not be used in advertising or publicity 
 * pertaining to distribution of the software or the documentation 
 * without specific, written prior permission.  
 *
 * Intel Corporation does not warrant, guarantee or make any 
 * representations regarding the use of, or the results of the use
 * of, the software and documentation in terms of correctness, 
 * accuracy, reliability, currentness, or otherwise; and you rely
 * on the software, documentation and results solely at your own risk.
 *****************************************************************************/

static char rcsid[] =
	"Id: Onindy.c,v 1.1.1.1 1991/03/28 16:20:43 rich Exp $";

/******************************************************************************
 *
 *	 		NINDY INTERFACE ROUTINES
 *
 * This version of the NINDY interface routines supports NINDY versions
 * 2.13 and older.  The older versions used a hex communication protocol,
 * instead of the (faster) current binary protocol.   These routines have
 * been renamed by prepending the letter 'O' to their names, to avoid
 * conflict with the current version.  The old versions are kept only for
 * backward compatibility, and well disappear in a future release.
 *
 ******************************************************************************/

#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>	/* Needed by file.h on Sys V */
#include <sys/file.h>
#include <signal.h>
#include <sys/stat.h>
#include <fcntl.h>	/* Needed on Sys V */
#include "ttycntl.h"
#include "block_io.h"
#include "wait.h"
#include "env.h"


#ifdef USG
#	include <unistd.h>
#	include "sysv.h"
#else	/* BSD */
#	include "string.h"
#endif

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

#ifndef ERROR
#define ERROR	-1
#endif

#define REGISTER_BYTES ((36*4) + (4*8))

extern char *malloc();
extern void free ();

static int quiet;	/* TRUE => stifle unnecessary messages */
static int nindy_fd;	/* File descriptor of tty connected to 960/NINDY board*/
static OninStrGet();

		/****************************
		 *                          *
		 *  MISCELLANEOUS UTILTIES  *
		 *                          *
		 ****************************/


/******************************************************************************
 * fromhex:
 *	Convert a hex ascii digit h to a binary integer
 ******************************************************************************/
static
int
fromhex( h )
    int h;
{
	if (h >= '0' && h <= '9'){
		h -= '0';
	} else if (h >= 'a' && h <= 'f'){
		h -= 'a' - 10;
	} else {
		h = 0;
	}
	return (h & 0xff);
}


/******************************************************************************
 * hexbin:
 *	Convert a string of ASCII hex digits to a string of binary bytes.
 ******************************************************************************/
static
hexbin( n, hexp, binp )
    int n;		/* Number of bytes to convert (twice this many digits)*/
    char *hexp;		/* Get hex from here		*/
    char *binp;		/* Put binary here		*/
{
	while ( n-- ){
		*binp++ = (fromhex(*hexp) << 4) | fromhex(*(hexp+1));
		hexp += 2;
	}
}


/******************************************************************************
 * binhex:
 *	Convert a string of binary bytes to a string of ASCII hex digits
 ******************************************************************************/
static
binhex( n, binp, hexp )
    int n;              /* Number of bytes to convert   */
    char *binp;         /* Get binary from here         */
    char *hexp;         /* Place hex here               */
{
	static char tohex[] = "0123456789abcdef";

        while ( n-- ){
                *hexp++ = tohex[ (*binp >> 4) & 0xf ];
                *hexp++ = tohex[ *binp & 0xf ];
                binp++;
        }
}

/******************************************************************************
 * byte_order:
 *	If the host byte order is different from 960 byte order (i.e., the
 *	host is big-endian), reverse the bytes in the passed value;  otherwise,
 *	return the passed value unchanged.
 *
 ******************************************************************************/
static
long
byte_order( n )
    long n;
{
	long rev;
	int i;
	static short test = 0x1234;

	if (*((char *) &test) == 0x12) {
		/*
		 * Big-endian host, swap the bytes.
		 */
		rev = 0;
		for ( i = 0; i < sizeof(n); i++ ){
			rev <<= 8;
			rev |= n & 0xff;
			n >>= 8;
		}
		n = rev;
	}
	return n;
}

/******************************************************************************
 * say:
 *	This is a printf that takes at most two arguments (in addition to the
 *	format string) and that outputs nothing if verbose output has been
 *	suppressed.
 ******************************************************************************/
static
say( fmt, arg1, arg2 )
    char *fmt;
    int arg1, arg2;
{
	if ( !quiet ){
		printf( fmt, arg1, arg2 );
		fflush( stdout );
	}
}

/******************************************************************************
 * exists:
 *	Creates a full pathname by concatenating up to three name components
 *	onto a specified base name; optionally looks up the base name as a
 *	runtime environment variable;  and checks to see if the file or
 *	directory specified by the pathname actually exists.
 *
 *	Returns:  the full pathname if it exists, NULL otherwise.
 *		(returned pathname is in malloc'd memory and must be freed
 *		by caller).
 *****************************************************************************/
static
char *
exists( base, c1, c2, c3, env )
    char *base;		/* Base directory of path */
    char *c1, *c2, *c3;	/* Components (subdirectories and/or file name) to be
			 *	appended onto the base directory name.  One or
			 *	more may be omitted by passing NULL pointers.
			 */
    int env;		/* If 1, '*base' is the name of an environment variable
			 *	to be examined for the base directory name;
			 *	otherwise, '*base' is the actual name of the
			 *	base directory.
			 */
{
	struct stat buf;/* For call to 'stat' -- never examined */
	char *path;	/* Pointer to full pathname (malloc'd memory) */
	int len;	/* Length of full pathname (incl. terminator) */
	extern char *getenv();


	if ( env ){
		base = getenv( base );
		if ( base == NULL ){
			return NULL;
		}
	}

	len = strlen(base) + 4;
			/* +4 for terminator and "/" before each component */
	if ( c1 != NULL ){
		len += strlen(c1);
	}
	if ( c2 != NULL ){
		len += strlen(c2);
	}
	if ( c3 != NULL ){
		len += strlen(c3);
	}

	path = malloc( len );

	strcpy( path, base );
	if ( c1 != NULL ){
		strcat( path, "/" );
		strcat( path, c1 );
		if ( c2 != NULL ){
			strcat( path, "/" );
			strcat( path, c2 );
			if ( c3 != NULL ){
				strcat( path, "/" );
				strcat( path, c3 );
			}
		}
	}

	if ( stat(path,&buf) != 0 ){
		free( path );
		path = NULL;
	}
	return path;
}

		/*****************************
		 *                           *
		 *  LOW-LEVEL COMMUNICATION  *
		 *                           *
		 *****************************/

/******************************************************************************
 * readchar:
 *	Wait for a character to come in on the NINDY tty, and return it.
 ******************************************************************************/
static
readchar()
{
	unsigned char c;

	while (read(nindy_fd,&c,1) != 1){
		;
	}
	return c;
}


/******************************************************************************
 * getpkt:
 *	Read a packet from a remote NINDY, with error checking, and return
 *	it in the indicated buffer.
 ******************************************************************************/
static
getpkt (buf)
     char *buf;
{
	unsigned char recv;	/* Checksum received		*/
	unsigned char csum;	/* Checksum calculated		*/
	char *bp;		/* Poointer into the buffer	*/
	int c;

	while (1){
		csum = 0;
		bp = buf;
		while ( (c = readchar()) != '#' ){
			*bp++ = c;
			csum += c;
		}
		*bp = 0;

		recv = fromhex(readchar()) << 4;
		recv |= fromhex(readchar());
		if ( csum == recv ){
			break;
		}
	
		fprintf(stderr,
			"Bad checksum (recv=0x%02x; calc=0x%02x); retrying\r\n",
								recv, csum );
		write (nindy_fd, "-", 1);
	}

	write (nindy_fd, "+", 1);
}


/******************************************************************************
 * putpkt:
 *	Checksum and send a gdb command to a remote NINDY, and wait for
 *	positive acknowledgement.
 *
 ******************************************************************************/
static
putpkt( cmd )
    char *cmd;	/* Command to be sent, without lead ^P (\020)
		 * or trailing checksum
		 */
{
	char ack;	/* Response received from NINDY		*/
	char checksum[4];
	char *p;
	unsigned int s;
	char resend;

	for ( s='\020', p=cmd; *p; p++ ){
		s += *p;
	}
	sprintf( checksum, "#%02x",  s & 0xff );

	/* Send checksummed message over and over until we get a positive ack
	 */
	resend = TRUE;
	do {
		if ( resend ){
			write( nindy_fd, "\020", 1 );
			write( nindy_fd, cmd, strlen(cmd) );
			write( nindy_fd, checksum, strlen(checksum) );
		}
		if  ( read( nindy_fd, &ack, 1 ) != 1 ){
			fprintf(stderr,"oink\n");
		}
		if ( ack == '-' ){
			fprintf( stderr, "Remote NAK, resending\r\n" );
			resend = TRUE;
		} else if ( ack != '+' ){
			fprintf( stderr, "Bad ACK, ignored: <%c>\r\n", ack );
			resend = FALSE;
		}
	} while ( ack != '+' );
}



/******************************************************************************
 * send:
 *	Send a message to a remote NINDY and return the reply in the same
 *	buffer (clobbers the input message).  Check for error responses
 *	as indicated by the second argument.
 *
 ******************************************************************************/
static
send( buf, ack_required )
    char *buf;		/* Message to be sent to NINDY; replaced by
			 *	NINDY's response.
			 */
    int ack_required;	/* TRUE means NINDY's response MUST be either "X00" (no
			 *	error) or an error code "Xnn".
			 * FALSE means the it's OK as long as it doesn't
			 *	begin with "Xnn".
			 */
{
	int errnum;
	static char *errmsg[] = {
		"",						/* X00 */
		"Buffer overflow",				/* X01 */
		"Unknown command",				/* X02 */
		"Wrong amount of data to load register(s)",	/* X03 */
		"Missing command argument(s)",			/* X04 */
		"Odd number of digits sent to load memory",	/* X05 */
		"Unknown register name",			/* X06 */
		"No such memory segment",			/* X07 */
		"No breakpoint available",			/* X08 */
		"Can't set requested baud rate",		/* X09 */
	};
#	define NUMERRS	( sizeof(errmsg) / sizeof(errmsg[0]) )

	static char err0[] = "NINDY failed to acknowledge command: <%s>\r\n";
	static char err1[] = "Unknown error response from NINDY: <%s>\r\n";
	static char err2[] = "Error response %s from NINDY: %s\r\n";

	putpkt (buf);
	getpkt (buf);

	if ( buf[0] != 'X' ){
		if ( ack_required ){
			fprintf( stderr, err0, buf );
			abort();
		}

	} else if ( strcmp(buf,"X00") ){
		sscanf( &buf[1], "%x", &errnum );
		if ( errnum > NUMERRS ){
			fprintf( stderr, err1, buf );
		} else{
			fprintf( stderr, err2, buf, errmsg[errnum] );
		}
		abort();
	}
}

		/************************
		 *                      *
		 *  BAUD RATE ROUTINES  *
		 *                      *
		 ************************/

/* Table of baudrates known to be acceptable to NINDY.  Each baud rate
 * appears both as character string and as a Unix baud rate constant.
 */
struct baudrate {
	char *string;
	int rate;
};

static struct baudrate baudtab[] = {
	 "1200", B1200,
	 "2400", B2400,
	 "4800", B4800,
	 "9600", B9600,
	"19200", B19200,
	"38400", B38400,
	NULL,    0		/* End of table */
};
 

/******************************************************************************
 * parse_baudrate:
 *	Look up the passed baud rate in the baudrate table.  If found, change
 *	our internal record of the current baud rate, but don't do anything
 *	about the tty just now.
 *
 *	Return pointer to baudrate structure on success, NULL on failure.
 ******************************************************************************/
static
struct baudrate *
parse_baudrate(s)
    char *s;	/* Desired baud rate, as an ASCII (decimal) string */
{
	int i;

	for ( i=0; baudtab[i].string != NULL; i++ ){
		if ( !strcmp(baudtab[i].string,s) ){
			return &baudtab[i];
		}
	}
	return NULL;
}

/******************************************************************************
 * try_baudrate:
 *	Try speaking to NINDY via the specified file descriptor at the
 *	specified baudrate.  Assume success it we can send an empty command
 *	with a bogus checksum and receive a NAK (response of '-') back within
 *	one second.
 *
 *	Return 1 on success, 0 on failure.
 ******************************************************************************/

static int saw_alarm;

static void
alarm_handler()
{
	saw_alarm = 1;
}

static int
try_baudrate( fd, brp )
    int fd;
    struct baudrate *brp;
{
	TTY_STRUCT tty;
	char c;
	int n;
	void (*old_alarm)();    /* Save alarm signal handler here on entry */
	

	/* Set specified baud rate and flush all pending input */
	ioctl( fd, TIOCGETP, &tty );
	TTY_REMOTE( tty, brp->rate );
	ioctl( fd, TIOCSETP, &tty );
	tty_flush( fd );

	/* Send bogus command */
	write( fd, "\020#00", 4 );

	/* Wait until reponse comes back or one second passes */
	old_alarm = signal( SIGALRM,alarm_handler );
	saw_alarm = 0;
	alarm(1);
	do {
		n = 1;
		TTY_NBREAD(fd,n,&c);
	} while ( n<=0 && !saw_alarm );

	/* Turn off alarm */
	alarm(0);
	signal( SIGALRM,old_alarm );

	/* Did we get a '-' back ? */
	if ( (n > 0) && (c == '-') ){
		return 1;
	}
	return 0;
}

/******************************************************************************
 * autobaud:
 *	Get NINDY talking over the specified file descriptor at the specified
 *	baud rate.  First see if NINDY's already talking at 'baudrate'.  If
 *	not, run through all the legal baudrates in 'baudtab' until one works,
 *	and then tell NINDY to talk at 'baudrate' instead.
 ******************************************************************************/
static
autobaud( fd, brp )
    int fd;
    struct baudrate *brp;
{
	int i;
	TTY_STRUCT tty;


	say("NINDY at wrong baud rate? Trying to autobaud...\n");
	i = 0;
	while ( 1 ){
		say( "\r%s...   ", baudtab[i].string );
		if ( try_baudrate(fd,&baudtab[i]) ){
			break;
		}
		if ( baudtab[++i].string == NULL ){
			/* End of table -- wraparound */
			i = 0;
			say("\nAutobaud failed. Trying again...\n");
		}
	}

	/* Found NINDY's current baud rate;  now change it.
	 */
	say("Changing NINDY baudrate to %s\n", brp->string);
	OninBaud( brp->string );

	/* Change our baud rate back to rate to which we just set NINDY.
	 */
	ioctl( fd, TIOCGETP, &tty );
	TTY_REMOTE( tty, brp->rate );
	ioctl( fd, TIOCSETP, &tty );
}

		/**********************************
		 *				  *
		 *   NINDY INTERFACE ROUTINES	  *
		 *                            	  *
		 * ninConnect *MUST* be the first *
		 * one of these routines called.  *
		 **********************************/


/******************************************************************************
 * ninBaud:
 *	Ask NINDY to change the baud rate on its serial port.
 *	Assumes we know the baud rate at which NINDY's currently talking.
 ******************************************************************************/
OninBaud( baudrate )
    char *baudrate;	/* Desired baud rate, as a string of ASCII decimal
			 * digits.
			 */
{
	char buf[100];		/* Message buffer	*/
	char *p;		/* Pointer into buffer	*/
	unsigned char csum;	/* Calculated checksum	*/

	tty_flush( nindy_fd );

	/* Can't use putpkt() because after the baudrate change
	 * NINDY's ack/nak will look like gibberish.
	 */
	for ( p=baudrate, csum=020+'z'; *p; p++ ){
		csum += *p;
	}
	sprintf( buf, "\020z%s#%02x", baudrate, csum );
	write( nindy_fd, buf, strlen(buf) );
}


/******************************************************************************
 * ninBptDel:
 *	Ask NINDY to delete the specified type of *hardware* breakpoint at
 *	the specified address.  If the 'addr' is -1, all breakpoints of
 *	the specified type are deleted.
 ******************************************************************************/
OninBptDel( addr, data )
    long addr;	/* Address in 960 memory	*/
    int data;	/* '1' => data bkpt, '0' => instruction breakpoint */
{
	char buf[100];

	if ( addr == -1 ){
		sprintf( buf, "b%c", data ? '1' : '0' );
	} else {
		sprintf( buf, "b%c%x", data ? '1' : '0', addr );
	}
	return send( buf, FALSE );
}


/******************************************************************************
 * ninBptSet:
 *	Ask NINDY to set the specified type of *hardware* breakpoint at
 *	the specified address.
 ******************************************************************************/
OninBptSet( addr, data )
    long addr;	/* Address in 960 memory	*/
    int data;	/* '1' => data bkpt, '0' => instruction breakpoint */
{
	char buf[100];

	sprintf( buf, "B%c%x", data ? '1' : '0', addr );
	return send( buf, FALSE );
}


/******************************************************************************
 * ninConnect:
 *	Open the specified tty.  Get communications working at the specified
 *	Flush any pending I/O on the tty.
 *
 *	Return the file descriptor, or -1 on failure.
 ******************************************************************************/
int
OninConnect( name, baudrate, brk, silent )
    char *name;		/* "/dev/ttyXX" to be opened			*/
    char *baudrate;/* baud rate: a string of ascii decimal digits (eg,"9600")*/
    int brk;		/* 1 => send break to tty first thing after opening it*/
    int silent;		/* 1 => stifle unnecessary messages when talking to 
			 *	this tty.
			 */
{
	int i;
	char *p;
	struct baudrate *brp;

	/* We will try each of the following paths when trying to open the tty
	 */
	static char *prefix[] = { "", "/dev/", "/dev/tty", NULL };

	quiet = silent;		/* Make global to this file */

	for ( i=0; prefix[i] != NULL; i++ ){
		p = malloc(strlen(prefix[i]) + strlen(name) + 1 );
		strcpy( p, prefix[i] );
		strcat( p, name );
		nindy_fd = open(p,O_RDWR);
		if ( nindy_fd >= 0 ){
#ifdef TIOCEXCL
			/* Exclusive use mode (hp9000 does not support it) */
			ioctl(nindy_fd,TIOCEXCL,NULL);
#endif
			if ( brk ){
				send_break( nindy_fd );
			}

			brp = parse_baudrate( baudrate );
			if ( brp == NULL ){
				say("Illegal baudrate %s ignored; using 9600\n",
								baudrate);
				brp = parse_baudrate( "9600" );
			}

			if ( !try_baudrate(nindy_fd,brp) ){
				autobaud(nindy_fd,brp);
			}
			tty_flush( nindy_fd );
			say( "Connected to %s\n", p );
			free(p);
			break;
		}
		free(p);
	}
	return nindy_fd;
}



/******************************************************************************
 * ninDownload:
 *	Ask NINDY to start up it's COFF downloader. Invoke 'sx' to perform
 *	the XMODEM download from the host end.
 *
 *	Return 1 on success, 0 on failure.
 ******************************************************************************/

#define XMODEM	"sx"	/* Name of xmodem transfer utility	*/

int
OninDownload( fn, quiet )
    char *fn;		/* Stripped copy of object file			*/
    int quiet;
{
	char *p;	/* Pointer to full pathname of sx utility	*/
	int success;	/* Return value					*/
	int pid;	/* Process ID of xmodem transfer utility	*/
	WAITTYPE w;	/* xmodem transfer completion status		*/
	char buf[200];


	/* Make sure the xmodem utility is findable.  This must be done before
	 * we start up the NINDY end of the download (NINDY will hang if we
	 * don't complete the download).
	 */
	if ( ((p = exists("G960BIN",XMODEM,NULL,NULL,1)) == NULL)
	&&   ((p = exists("G960BASE","bin",XMODEM, NULL,1)) == NULL)
#ifdef HOST
	&&   ((p = exists(DEFAULT_BASE,HOST,"bin",XMODEM,0)) == NULL)
#endif
								      ){

		fprintf(stderr,"Can't find '%s' download utility\n",XMODEM);
		fprintf(stderr,"Check env variables G960BIN and G960BASE\n");
		return 0;
	}

	if ( !quiet ){
		printf( "Downloading %s\n", fn );
	}

	/* Reset NINDY,  wait until "reset-complete" ack,
	 * and start up the NINDY end of the download.
	 */
	OninReset();
	putpkt( "D" );

	/* Invoke x-modem transfer, a separate process.  DON'T
	 * use system() to do this -- under system V Unix, the
	 * redirection of stdin/stdout causes the nindy tty to
	 * lose all the transmission parameters we've set up.
	 */
	success = 0;

	pid = fork();
	if ( pid == -1 ){
		perror( "Can't fork process:" );

	} else if ( pid == 0 ){		/* CHILD */
		dup2( nindy_fd, 0 );	/* Redirect stdin */
		dup2( nindy_fd, 1 );	/* Redirect stout */
		if ( quiet ){
			execl( p, p, "-q", fn, (char*)0 );
		} else {
			execl( p, p, fn, (char*)0 );
		}
		/* Don't get here unless execl fails */
		sprintf( buf, "Can't exec %s", p );
		perror( buf );

	} else {			/* PARENT */
		if ( wait(&w) == -1 ){
			perror( "Wait failed" );
		} else if (WIFEXITED(w) && (WEXITSTATUS(w) == 0)){
			success = 1;
		}
	}
	return success;
}


/******************************************************************************
 * ninGdbExit:
 *	Ask NINDY to leave GDB mode and print a NINDY prompt.
 *	Since it'll no longer be in GDB mode, don't wait for a response.
 ******************************************************************************/
OninGdbExit()
{
        putpkt( "E" );
}


/******************************************************************************
 * ninGo:
 *	Ask NINDY to start or continue execution of an application program
 *	in it's memory at the current ip.
 ******************************************************************************/
OninGo( step_flag )
    int step_flag;	/* 1 => run in single-step mode */
{
	putpkt( step_flag ? "s" : "c" );
}


/******************************************************************************
 * ninMemGet:
 *	Read a string of bytes from NINDY's address space (960 memory).
 ******************************************************************************/
OninMemGet(ninaddr, hostaddr, len)
     long ninaddr;	/* Source address, in the 960 memory space	*/
     char *hostaddr;	/* Destination address, in our memory space	*/
     int len;		/* Number of bytes to read			*/
{
	char buf[2*BUFSIZE+20];	/* Buffer: hex in, binary out		*/
	int cnt;		/* Number of bytes in next transfer	*/

	for ( ; len > 0; len -= BUFSIZE ){
		cnt = len > BUFSIZE ? BUFSIZE : len;

		sprintf( buf, "m%x,%x", ninaddr, cnt );
		send( buf, FALSE );
		hexbin( cnt, buf, hostaddr );

		ninaddr += cnt;
		hostaddr += cnt;
	}
}


/******************************************************************************
 * ninMemPut:
 *	Write a string of bytes into NINDY's address space (960 memory).
 ******************************************************************************/
OninMemPut( destaddr, srcaddr, len )
     long destaddr;	/* Destination address, in NINDY memory space	*/
     char *srcaddr;	/* Source address, in our memory space		*/
     int len;		/* Number of bytes to write			*/
{
	char buf[2*BUFSIZE+20];	/* Buffer: binary in, hex out		*/
	char *p;		/* Pointer into buffer			*/
	int cnt;		/* Number of bytes in next transfer	*/

	for ( ; len > 0; len -= BUFSIZE ){
		cnt = len > BUFSIZE ? BUFSIZE : len;

		sprintf( buf, "M%x,", destaddr );
		p = buf + strlen(buf);
		binhex( cnt, srcaddr, p );
		*(p+(2*cnt)) = '\0';
		send( buf, TRUE );

		srcaddr += cnt;
		destaddr += cnt;
	}
}

/******************************************************************************
 * ninRegGet:
 *	Retrieve the contents of a 960 register, and return them as a long
 *	in host byte order.
 *
 *	THIS ROUTINE CAN ONLY BE USED TO READ THE LOCAL, GLOBAL, AND
 *	ip/ac/pc/tc REGISTERS.
 *
 ******************************************************************************/
long
OninRegGet( regname )
    char *regname;	/* Register name recognized by NINDY, subject to the
			 * above limitations.
			 */
{
	char buf[200];
	long val;

	sprintf( buf, "u%s", regname );
	send( buf, FALSE );
	hexbin( 4, buf, (char *)&val );
	return byte_order(val);
}

/******************************************************************************
 * ninRegPut:
 *	Set the contents of a 960 register.
 *
 *	THIS ROUTINE CAN ONLY BE USED TO SET THE LOCAL, GLOBAL, AND
 *	ip/ac/pc/tc REGISTERS.
 *
 ******************************************************************************/
OninRegPut( regname, val )
    char *regname;	/* Register name recognized by NINDY, subject to the
			 * above limitations.
			 */
    long val;		/* New contents of register, in host byte-order	*/
{
	char buf[200];

	sprintf( buf, "U%s,%08x", regname, byte_order(val) );
	send( buf, TRUE );
}

/******************************************************************************
 * ninRegsGet:
 *	Get a dump of the contents of the entire 960 register set.  The
 *	individual registers appear in the dump in the following order:
 *
 *		pfp  sp   rip  r3   r4   r5   r6   r7 
 *		r8   r9   r10  r11  r12  r13  r14  r15 
 *		g0   g1   g2   g3   g4   g5   g6   g7 
 *		g8   g9   g10  g11  g12  g13  g14  fp 
 *		pc   ac   ip   tc   fp0  fp1  fp2  fp3
 *
 *	Each individual register comprises exactly 4 bytes, except for
 *	fp0-fp3, which are 8 bytes.
 *
 * WARNING:
 *	Each register value is in 960 (little-endian) byte order.
 *
 ******************************************************************************/
OninRegsGet( regp )
    char *regp;		/* Where to place the register dump */
{
	char buf[(2*REGISTER_BYTES)+10];   /* Registers in ASCII hex */

	strcpy( buf, "r" );
	send( buf, FALSE );
	hexbin( REGISTER_BYTES, buf, regp );
}

/******************************************************************************
 * ninRegsPut:
 *	Initialize the entire 960 register set to a specified set of values.
 *	The format of the register value data should be the same as that
 *	returned by ninRegsGet.
 *
 * WARNING:
 *	Each register value should be in 960 (little-endian) byte order.
 *
 ******************************************************************************/
OninRegsPut( regp )
    char *regp;		/* Pointer to desired values of registers */
{
	char buf[(2*REGISTER_BYTES)+10];   /* Registers in ASCII hex */

	buf[0] = 'R';
	binhex( REGISTER_BYTES, regp, buf+1 );
	buf[ (2*REGISTER_BYTES)+1 ] = '\0';

	send( buf, TRUE );
}


/******************************************************************************
 * ninReset:
 *      Ask NINDY to perform a soft reset; wait for the reset to complete.
 ******************************************************************************/
OninReset()
{

	putpkt( "X" );
	while ( readchar() != '+' ){
		;
	}
}


/******************************************************************************
 * ninSrq:
 *	Assume NINDY has stopped execution of the 960 application program in
 *	order to process a host service request (srq).  Ask NINDY for the
 *	srq arguments, perform the requested service, and send an "srq
 *	complete" message so NINDY will return control to the application.
 *
 ******************************************************************************/
OninSrq()
{
	char buf[BUFSIZE];
	int retcode;
	unsigned char srqnum;
	char *p;
	char *argp;
	int nargs;
	int arg[MAX_SRQ_ARGS];


	/* Get srq number and arguments
	 */
	strcpy( buf, "!" );
	send( buf, FALSE );
	hexbin( 1, buf, (char *)&srqnum );

	/* Set up array of pointers the each of the individual
	 * comma-separated args
	 */
	nargs=0;
	argp = p = buf+2;
        while ( 1 ){
                while ( *p != ',' && *p != '\0' ){
                        p++;
                }
                sscanf( argp, "%x", &arg[nargs++] );
                if ( *p == '\0' || nargs == MAX_SRQ_ARGS ){
                        break;
                }
                argp = ++p;
        }

	/* Process Srq
	 */
	switch( srqnum ){
	case BS_CLOSE:
		/* args: file descriptor */
		if ( arg[0] > 2 ){
			retcode = close( arg[0] );
		} else {
			retcode = 0;
		}
		break;
	case BS_CREAT:
		/* args: filename, mode */
		OninStrGet( arg[0], buf );
		retcode = creat(buf,arg[1]);
		break;
	case BS_OPEN:
		/* args: filename, flags, mode */
		OninStrGet( arg[0], buf );
		retcode = open(buf,arg[1],arg[2]);
		break;
	case BS_READ:
		/* args: file descriptor, buffer, count */
		retcode = read(arg[0],buf,arg[2]);
		if ( retcode > 0 ){
			OninMemPut( arg[1], buf, retcode );
		}
		break;
	case BS_SEEK:
		/* args: file descriptor, offset, whence */
		retcode = lseek(arg[0],arg[1],arg[2]);
		break;
	case BS_WRITE:
		/* args: file descriptor, buffer, count */
		OninMemGet( arg[1], buf, arg[2] );
		retcode = write(arg[0],buf,arg[2]);
		break;
	default:
		retcode = ERROR;
		break;
	}

	/* Tell NINDY to continue
	 */
	sprintf( buf, "e%x", retcode );
	send( buf, TRUE );
}


/******************************************************************************
 * ninStopWhy:
 *	Assume the application program has stopped (i.e., a DLE was received
 *	from NINDY).  Ask NINDY for status information describing the
 *	reason for the halt.
 *
 *	Returns a non-zero value if the user program has exited, 0 otherwise.
 *	Also returns the following information, through passed pointers:
 *           - why: an exit code if program the exited; otherwise the reason
 *			why the program halted (see stop.h for values).
 *	    - contents of register ip (little-endian byte order)
 *	    - contents of register sp (little-endian byte order)
 *	    - contents of register fp (little-endian byte order)
 ******************************************************************************/
char
OninStopWhy( whyp, ipp, fpp, spp )
    char *whyp;	/* Return the 'why' code through this pointer	*/
    char *ipp;	/* Return contents of register ip through this pointer	*/
    char *fpp;	/* Return contents of register fp through this pointer	*/
    char *spp;	/* Return contents of register sp through this pointer	*/
{
	char buf[30];
	char stop_exit;

	strcpy( buf, "?" );
	send( buf, FALSE );
	hexbin( 1, buf, &stop_exit );
	hexbin( 1, buf+2, whyp );
	hexbin( 4, buf+4, ipp );
	hexbin( 4, buf+12, fpp );
	hexbin( 4, buf+20, spp );
	return stop_exit;
}

/******************************************************************************
 * ninStrGet:
 *	Read a '\0'-terminated string of data out of the 960 memory space.
 *
 ******************************************************************************/
static
OninStrGet( ninaddr, hostaddr )
     unsigned long ninaddr;	/* Address of string in NINDY memory space */
     char *hostaddr;		/* Address of the buffer to which string should
				 *	be copied.
				 */
{
	char buf[BUFSIZE];	/* String as 2 ASCII hex digits per byte */
	int numchars;		/* Length of string in bytes.		*/

	sprintf( buf, "\"%x", ninaddr );
	send( buf, FALSE );
	numchars = strlen(buf)/2;
	hexbin( numchars, buf, hostaddr );
	hostaddr[numchars] = '\0';
}

/******************************************************************************
 * ninVersion:
 *	Ask NINDY for version information about itself.
 *	The information is sent as an ascii string in the form "x.xx,<arch>",
 *	where,
 *		x.xx	is the version number
 *		<arch>	is the processor architecture: "KA", "KB", "MC", "CA" *
 *
 ******************************************************************************/
int
OninVersion( p )
     char *p;		/* Where to place version string */
{
	char buf[BUFSIZE];

	strcpy( buf, "v" );
	send( buf, FALSE );
	strcpy( p, buf );
	return strlen( buf );
}
