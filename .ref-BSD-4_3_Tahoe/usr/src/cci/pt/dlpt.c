/************************************************************************
 *									*
 *	dlpt	- download a CCI POWERTERMINAL or POWERTERMINAL II	*
 *									*
 ************************************************************************/

/************************************************************************
 *	Compilation Notes --						*
 *									*
 *	- The symbol  V7  must be defined when compiling for Version 7	*
 *	  or Berkeley 4.2.						*
 *	- No flags are needed when compiling for S3.			*
 *	- This file contains the changes to support the new Berkeley	*
 *	  4.2 signaling mechanism that were present at one point, then	*
 *	  deleted, and were restored in version 2.3.1.  These changes	*
 *	  are required to make timed-out reads work correctly.		*
 *	  Compile with NEWSEG set to get these changes.			*
 *									*
 ************************************************************************/

/************************************************************************
Version 1.2.1 changes:

MDS	3/13/84		Implemented logic to circumvent 5/20 CPU port
			  problem.  CPU port loses input characters
			  transmitted by the terminal at 9600 baud.
			  Therefore, if this program receives an invalid
			  DA response and it is talking to a cpu port,
			  then act as if a valid ROM DA response was
			  received.
			Implemented "force" option.  If specicified at
			  execution time the receipt of an invalid DA
			  response will be treated as a valid ROM DA
			  response.

*************************************************************************

Version 2.0.0 changes:

MDS	11/6/84		Implemented logic to support CCI POWERTERMINAL II.

*************************************************************************

Version 2.1.0 changes:

			Version 2.1 is an interim version of dlpt created
			by OSG.

*************************************************************************

Version 2.2.0 changes:

SP & BM @osg
MDS	2/20/85		1) -a prompt came up twice on PT1 when 'n'
			given.  Fixed.
			2) If tty is tty01 or console, and dlpt
			cannot determine terminal type, message is
			given for user to manually 'dlpt -f[12]'.
			3) Added argument check to -t option to prevent
			core dump if no arguments given with -t.
			4) In function whoisit() changed fputs to write.
			Fputs did not work on PERPOS-S.
			5) Removed CR1 from ttyinit() for System 3.
			CR1 caused a NULL,ACK to be output after CR
			in Terminal loaded message.
			6) Added XOFF/XON recognition to download by
			adding IXON to ttyinit().
			7) Changed source to default compile for S3
			instead of V7.  For V7 need to set V7 flag.

*************************************************************************

Version 2.3.0 changes:

BJM @osg
BJM	3/6/85		Added XON/XOFF recognition to download for
			Version 7 and Berkeley 4.2 version by setting
			port to CBREAK mode for 'normal' download and
			RAW mode for download in reliability mode.

			Added conditional compilation statements for
			19.2K baud.

*************************************************************************

Version 2.3.1 changes:

woe@ccicpg
WOE	6/17/85		Restored changes for new Berkeley 4.2 signalling
			mechanisms under the compile-time define NEWSIG.
			Timed-out reads require these changes since the
			read system call is "automatically" retried when
			interrupted by a SIGALRM (or any other handled/
			ignored signal).

*************************************************************************/


/************************************************************************
 *									*
 *	Include files.							*
 *									*
 ************************************************************************/

#include	"stdio.h"
#include	"signal.h"

#ifdef	V7	/********* if Version 7				*********/

#include 	"sgtty.h"

#else		/********* else assume System 3 		*********/

#include	"termio.h"

#endif		/********* end of System 3			*********/

#ifdef	NEWSIG			/* new signal mechanism */
#include	<setjmp.h>
#endif

/************************************************************************
 *									*
 *	Defines.							*
 *									*
 ************************************************************************/

#define	VERSION		"2.3.1"	/* version to be displayed by -v option	*/

typedef int BOOL;

#define PT1_DEFLD	"/lib/pt/DEFAULT.LD" /* default download file	*/
#define PT2_DEFLD	"/lib/pt/PT2DEF.LD" /* default download file	*/
#define TTY_PREFIX	"/dev/"		/* appended to -t field		*/

#define	STX		0x02	/* cntrl b */
#define	ENQ		0x05	/* cntrl e */
#define	ACK		0x06	/* cntrl f */
#define	CAN		0x18	/* cntrl x */
#define BELL		0x07	/* cntrl g */

#define INVRESP		0	/* signal no valid DA response from tty	*/
#define PT1ROM		1	/* signal that PT1 ROM responded to DA	*/
#define PT1RAM		2	/* signal that PT1 RAM responded to DA	*/
#define PT2ROM		3	/* signal that PT2 ROM responded to DA	*/
#define PT2RAM		4	/* signal that PT2 RAM responded to DA	*/

#define TRUE		1
#define FALSE		0

#define	STX_TIMES	3	/* Number of times to send STX block	*/
#define	NRETRIES	10	/* Number of retries for load block	*/
#define LD_TIMEOUT	2	/* Loading timeout period		*/

/************************************************************************
 *									*
 *	Local functions and variables.					*
 *									*
 ************************************************************************/

#ifdef	NEWSIG			/* new signal mechanism */
jmp_buf	jb;
#else
#define	timoread read
#define	timowrite write
#endif

char	stxrec [] = {STX,STX,STX,STX,STX,STX,STX,STX,STX,STX};
char	nulls [15];
char	can = CAN;
char	enq = ENQ;
char	ack = ACK;
char	devname[25] = TTY_PREFIX;

int	nullcnt = 14; 
int	timout;
int	bailout();
int	toterm;			/* to terminal file descriptor		*/
int	fromterm;		/* from terminal file descriptor	*/


#ifdef	V7	/********* if Version 7				*********/

char		baudrate;	/* baudrate , set by ttysave or main	*/

#else		/********* else assume System 3 		*********/

unsigned short	baudrate;	/* baudrate , set by ttysave or main	*/

#endif		/********* end of System 3			*********/


char		*myname;	/* name of this program			*/

BOOL	rflag = FALSE;		/* reliability mode			*/
char	fflag = 0;		/* force mode (used if inv DA response	*/
char	*intro[] = {
			"\n",
			"DO YOU WANT A TERMINAL LOAD SEQUENCE?",
			" ",
			"  If you do not answer within 15 seconds,",
			"  the terminal load sequence will begin.",
			" ",
			"ENTER: y <return>  or  <return>  to load ",
			"            n <return>  to avoid load :",
			NULL
		};

char	*wrong[] = {	"INVALID ANSWER",
			NULL
		};

char	*loading[]={
			"\n",
			"Terminal load in progress",
			NULL
		};


/************************************************************************
 *									*
 *	main	- Do it.						*
 *									*
 ************************************************************************/

int
main (argc, argv)
int	argc;
char	**argv;
{
	char	*pt1file;
	char	*pt2file;
	int	who;		/* who is responding			*/
	char	*ttyname();
	BOOL	aflag = FALSE;	/* TRUE if inquire wanted before load 	*/

	myname = *argv++;	/* get the name of this program 	*/
	--argc;

	ttysave();	/* save ioctl params and init default baudrate */

	pt1file = PT1_DEFLD;	/* set default hex file for load	*/
	pt2file = PT2_DEFLD;	/* set default hex file for load	*/

	while( argc-- )	/* while there are arguments to process */
	{
		if( **argv == '-' )	/* while flags left */
		{
			char	*fptr = *argv;	/* get ptr to flags */

			while( *++fptr )	/* while flags left */
			{
				char	atobaud();

				switch( *fptr )	/* process a flag  */
				{

				case 'a':	/* ask if load wanted */
					aflag = TRUE;
					break;

				case 'b':	/* specific baud rate */
					if( fptr[1] != '\0' )
					{
						baudrate = atobaud(++fptr);
						if(baudrate == (char) -1 )
						{
						  fprintf(stderr,
						    "%s: %s - invalid baud\n",
						    myname, fptr);
						  usage();
						}
						fptr = "x";	/* fake out */
						break;
					}
					else
					{
						if( argc < 1 )
							usage();
						baudrate = atobaud(*++argv);
						--argc;
						if(baudrate == (char) -1 )
						{
						   fprintf(stderr,
						     "%s: %s - invalid baud\n",
						     myname, *argv);
						   usage();
						}
					}
					break;

				case 'f':	/* force mode		*
						 * to force load if	*
						 * invalid DA response	*/
					/* get type '1' or '2' for	*
					 *   PT1 or PT2			*/
					fflag = *++fptr;
					break;

				case 'r':	/*reliability mode */
					rflag = TRUE;
					break;

				case 't':	/* specific port	*/
					if( fptr[1] != 0 )
					{
						strcat(devname,++fptr);
						fptr = "x"; /* fake out	*/
					}
					else
					{
						if( argc < 1 )
							usage();
						strcat(devname, *++argv);
						--argc;
					}
					if(freopen(devname,"w",stdout) ==
						(FILE *)NULL)
					{
					  fprintf(stderr,
					    "%s: cannot open port for write - %s\n",
					    myname,devname);
					  usage();
					}
					if(freopen(devname,"r",stdin) ==
						(FILE *)NULL)
					{
					  fprintf(stderr,
					    "%s: cannot open port for read - %s\n",
					    myname,devname);
					  usage();
					}
					break;

				case 'v':	/* display version */
					printf("%s: Version %s\n",myname,
							VERSION);
					exit(0);
					break;

				default:
					fprintf(stderr,
						"%s: %c - unknown flag\n",
						myname, *fptr);	
					usage();
					break;
				}
			}
		}
		else
		{
				pt1file = *argv;
				pt2file = *argv;
		}
		++argv;
	}	

	signal (SIGALRM, bailout);
	toterm = 1;			/* standard output		*/
	fromterm = 0;			/* standard input		*/

	who = whoisit();		/* find out who is there	*/

	if( who == INVRESP )	/* if an invalid or no response		*/
	    switch( fflag ) {	/* check for force wanted		*/
		case '1':		/* if PT1 force wanted		*/
			who = PT1ROM;	/* force a PT1 download		*/
			break;

		case '2':		/* if PT2 force wanted		*/
			who = PT2ROM;	/* force a PT2 download		*/
			break;

		default: {		/* else no force		*/
			char *devwho;
			devwho = ttyname(toterm); /* get name of port	*/
			if ((strcmp(devwho,"/dev/console") == 0) ||
			    (strcmp(devwho,"/dev/tty01") == 0)) {
				printf(
"%s: from console or tty01, enter 'dlpt -f#'\n-f1 for PT1, -f2 for PT2\n", myname);
				ttyrestore();
				exit(0);
			}
			break;
		}
	    }

	switch( who )		/* depending on who responded		*/
	{
	    case PT1RAM:		/* is a downloaded PT1 ( CT )	*/
	    case PT2RAM:		/* is a downloaded PT2		*/
		ttytalk();
		printf("\r\nTerminal loaded\r\n");
		break;

	    case PT1ROM:		/* is a non-downloaded PT1	*/
		if((!aflag) || ((aflag) && (wantptload())))
		{
			ttytalk();
			putmsg(loading, 0);
			dlpt1(pt1file);		/* load terminal	*/

			/************************************************
			 * NOTE: If terminal DTR is tied to CPU DCD	*
			 *   AND the terminal firmware re-initializes	*
			 *   the 8251 I/O chip, DCD is dropped which	*
			 *   causes this program to get killed at this	*
			 *   point.  This is the case for the PT	*
			 *   terminal as of 10/25/83.			*
			 ************************************************/

			sleep(3); /* wait for terminal to initialize	*/
			printf("Terminal download complete\r\n");
		}
		break;

	    case PT2ROM:		/* is a non-downloaded PT2	*/
		if((!aflag) || ((aflag) && (wantptload())))
		{
			ttytalk();
			putmsg(loading, 0);
			dlpt2(pt2file);		/* load terminal	*/
			sleep(3); /* wait for terminal to initialize	*/
			printf("Terminal download complete\r\n");
		}
		break;

	    default:
		break;
	}
	ttyrestore();
}

/************************************************************************
 *									*
 *	usage - print usage statement and abort				*
 *									*
 ************************************************************************/

usage()
{
	fprintf(stderr,
	"Usage: %s [-a] [-fTermNo] [-r] [-b BaudRate] [-t ttyXX] [load file]\n",
		myname);
	exit(1);
}


/************************************************************************
 *									*
 *	PT down-load rtn						*
 *									*
 ************************************************************************/
dlpt1(hexfile)

char *	hexfile;

{

	FILE *		in;		/* input file			*/
	int		i;		/* counter			*/
	int		rs;		/* record size			*/
	char		record [524];	/* record			*/
	int		cs;		/* checksum			*/
	char *		rp;		/* record pointer		*/
	char		csum [2];	/* received checksum		*/

	in = stdin;
	timout = 0;			/* Alarm time-out flag		*/

	if ((in = fopen (hexfile, "r")) == (FILE *)NULL)
		fatal ("Can't open input file.");

	ttyinit();	/* set up port for load 			*/

	for (i = STX_TIMES ; i > 0 ; i--)
		if (write (toterm, stxrec, sizeof(stxrec)) != sizeof(stxrec))
			fatal ("1. Can't write STX to terminal.");

	while ((rs = getrec1 (in, record)) > 0)
	{
	    for (cs = 0, i = rs, rp = record ; i > 0 ; i--)
		cs += *rp++;

	    for (i = NRETRIES ; i > 0 ; i--)
	    {
		if (write (toterm, record, rs) != rs)
				derror ("3. Can't write record to terminal.");

		if (write (toterm, nulls, nullcnt) != nullcnt)
			derror ("4. Can't write NULLS to terminal.");

		if(rflag==TRUE)
		{
			alarm(LD_TIMEOUT);  /*  time limit */
			timout = 0;
			if (timowrite (toterm, &ack, 1) != 1 || (timout))
			{
				derror ("6. Can't write ACK to terminal.");
				continue;
			}

			alarm(LD_TIMEOUT);  /* time limit */
			timout = 0;
			if (timoread (fromterm, csum, 1) != 1 || (timout))
			{
				derror ("7. Can't read ACK from terminal.");
				continue;
			}

			alarm(LD_TIMEOUT);  /* time limit */
			timout = 0;
			if (timoread (fromterm, csum + 1, 1) != 1 || (timout))
			{
				derror ("8. Can't read csum count from terminal.");
				continue;
			}

			if (csum [0] != ACK)
			{
				derror ("9. Protocol violation not ACK.");
				continue;
			}

			if (((cs & 0x3f)+0x20) == csum[1])
				break;

		} /* end of reliabilaty logic */

		else
			break;

	    }	/* end of for loop */

		if (i == 0)
			fatal ("10. Number of retries exceeded.");

		if (rs == 11)		/* Last record			*/
			return;

	}

	if (rs == 0)
		fatal ("11. End of file reached before end-of-load record.");
	else
		fatal ("12. Bad record read.");

	}

/************************************************************************
 *									*
 * TITLE:	dlpt2 - down load CCI POWERTERMINAL II			*
 *									*
 * RETURNS:	nothing							*
 *									*
 ************************************************************************/
dlpt2( hexfile )

char	*hexfile;

{
	FILE *		in;		/* input file			*/
	int		i;		/* counter			*/
	int		rs;		/* record size			*/
	char		record [524];	/* record			*/

	in = stdin;
	timout = 0;			/* Alarm time-out flag		*/

	if ((in = fopen (hexfile, "r")) == (FILE *)NULL)
		fatal ("Can't open input file.");

	ttyinit();	/* set up port for load 			*/

	ttyflush();	/* flush port before attempting com		*/

	for (i = STX_TIMES ; i > 0 ; i--)
		if (write (toterm, stxrec, sizeof(stxrec)) != sizeof(stxrec))
			fatal ("1. Can't write STX to terminal.");

	if( rflag == TRUE )	/* if rel mode wanted - tell PT2	*/
	{
		if( write( toterm, &enq, 1 ) != 1 )
		{
			d2error ("16. Can't write ENQ to terminal.");
		}
	}

	while( ( rs = getrec2(in, record) ) > 0 )
	{
	    for( i = NRETRIES ; i > 0 ; i-- )
	    {
		if( write ( toterm, record, rs ) != rs )
				d2error ("3. Can't write record to terminal.");

/*
		if( write ( toterm, nulls, nullcnt ) != nullcnt )
			d2error ("4. Can't write NULLS to terminal.");
*/

		if( rflag == TRUE )
		{
			char	resp;		/* resp char		*/

			alarm(LD_TIMEOUT);	/* time limit */
			timout = 0;
			if( (timoread( fromterm, &resp, 1 ) != 1 ) || (timout))
			{
				d2error ("17. Can't read ACK from terminal.");
				continue;
			}
			if( resp == ACK )
				break;		/* get txmit next char	*/

			d2error ("19. Protocol violation not ACK.");
			continue;

		} /* end of reliabilaty logic	*/

		else
			break;		/* no verification wanted	*/

	    }	/* end of error retry loop	*/

		if( i == 0 )
			fatal ("20. Number of retries exceeded.");

		/* a complete record has been transmitted		*/
		/* check if it was the last record			*/

		if( record[ 1 ] == '8' )	/* was it an S8 record?	*/
			return;			/* if so, done !!	*/

	}

	if( rs == 0 )
		fatal ("21. End of file reached before end-of-load record.");
	else
		fatal ("22. Bad record read.");

}

/************************************************************************
 *									*
 *	derror - have an error handle and return			*
 *									*
 ************************************************************************/

int
derror (s)

char *		s;

{

	alarm(0);	/* cancel alarms */
	write (toterm,&can,sizeof(can)); /* cancel */
	write (toterm,nulls,nullcnt); /* pad for next write */
	fputs(s, stderr);
	fputs("\r\n", stderr);
	return;

}

/************************************************************************
 *									*
 *	d2error - have an error handle and return			*
 *									*
 ************************************************************************/

int
d2error (s)

char *s;

{
	alarm(0);	/* cancel alarms */
	fputs(s, stderr);
	fputs("\r\n", stderr);
	return;
}

/************************************************************************
 *									*
 *	getrec1	- Get a PT1 (CT) record to transmit.			*
 *									*
 *	Returns:							*
 *		-1	Error in file format.				*
 *		0	End of file reached.				*
 *		> 0	Size of record to transmit.			*
 *									*
 ************************************************************************/
int
getrec1(fd, rp)

FILE *		fd;
char *		rp;

{

	int		c;
	int		nc;		/* number of characters		*/
	int		l;		/* length of record bytes	*/
	int		cc;		/* checksum			*/
	int		i;		/* counter			*/
	int		a;		/* address piece		*/

	while ((c = getc (fd)) != ':')	/* Look for start of record	*/
		if (c == EOF)
			return 0;	/* End of file			*/

	/*****	Start record off.					*/

/*%%%	printf("At record start\n");/*%%*/
	*rp++ = ':';
	nc = 1;

	/*****	Get number of hex bytes in record.			*/

	if ((l = getxb (fd, rp)) < 0)
		return -1;

/*%%%	printf("%d data bytes\n", l);/*%%*/
	cc = l;				/* Start checksum		*/
	rp += 2;			/* Past two characters		*/
	nc += 2;			/* Two more characters		*/

	/*****	Get address portion of data record.			*/

	for (i = 2 ; i > 0 ; i--)	/* Two hex bytes		*/
		{
		if ((a = getxb (fd, rp)) < 0)
			return -1;

		cc += a;		/* Figure address into checksum	*/
		rp += 2;		/* Past two characters		*/
		nc += 2;		/* Two more characters		*/
		}

	/*****	Read type byte, which should be zero.			*/

	if ((i = getxb (fd, rp)) != 0)
		return -1;

	cc += i;			/* Figure type into checksum	*/
	rp += 2;			/* Past two characters		*/
	nc += 2;			/* Two more characters		*/

	/*****	Read in the data record.				*/

	while (l-- > 0)			/* Count down			*/
		{
		if ((i = getxb (fd, rp)) < 0)
			return -1;

/*%%%	printf ("Read %d\n", i);/*%%*/
		cc += i;		/* Checksum			*/
		rp += 2;		/* Pointer			*/
		nc += 2;		/* Number of characters		*/
		}

	cc = (-cc) & 0377;		/* Two's complement byte	*/

/*%%%	printf ("Checksum = %x\n", cc);/*%%*/
	*rp = cc >> 4;
	*rp += *rp < 10 ? '0' : 'A' - 10;
	*++rp = cc & 0xF;
	*rp += *rp < 10 ? '0' : 'A' - 10;

/*%%%	printf ("record size = %d\n", nc + 2);/*%%*/
	return nc + 2;

}

/************************************************************************
 *									*
 * TITLE:	getrec2 - Get a POWERTERMINAL II record to transmit	*
 *									*
 * RETURNS:								*
 *		-1	Error in file format.				*
 *		0	End of file reached.				*
 *		> 0	Size of record to transmit.			*
 *									*
 ************************************************************************/
int
getrec2 (fd, rp)

FILE	*fd;
char	*rp;

{
	register char	*rrp;		/* register copy of record ptr	*/
	char		c;		/* temp store of read char	*/
	int		nc;		/* number of characters		*/
	int		l;		/* length of record bytes	*/

	rrp = rp;			/* get addr in a register	*/

	while(( c = getc( fd )) != 'S')	/* Look for start of record	*/
		if( c == EOF )
			return( 0 );	/* End of file			*/

	*rrp++ = c ;			/* put first char of record	*/
	nc = 1;				/* start the char counter	*/

	if( ( *rrp++ = getc( fd ) ) < 0 )	/* get S-record type	*/
		return( -1 );		/* if EOF, return		*/
	nc++;				/* incre num of chars		*/

	if( ( l = getxb(fd, rrp) ) < 0)	/* get num of bytes		*/
		return -1;
	rrp += 2;			/* Past two characters		*/
	nc += 2;			/* Two more characters		*/

	/* get address, data, and checksum fields			*/

	l <<= 1 ;			/* chg to num nibbles (times 2)	*/

	while( l-- > 0 ) {		/* l now contains num of nibbles*/
		if ( ( *rrp++ = getc( fd ) ) < 0)
			return( -1 );
		nc++;			/* bump char counter		*/
	}

	return( nc );			/* return num of chars in rec	*/

}

/************************************************************************
 *									*
 *	getxb	- Get a hex byte.					*
 *									*
 *	Returns:							*
 *		-1	Error in file format or end of file reached.	*
 *		>= 0	Integer value of byte.				*
 *									*
 ************************************************************************/

int
getxb (fd, sp)

FILE *		fd;
char *		sp;

{

	register int	i, j;

	if ((i = getxn (fd, sp++)) < 0 || (j = getxn (fd, sp)) < 0)
		return -1;

	return (i << 4) | j;

}

/************************************************************************
 *									*
 *	getxn	- Get a hex nibble.					*
 *									*
 *	Returns:							*
 *		-1	Error in file format or end of file reached.	*
 *		>= 0	Integer value of nibble.			*
 *									*
 ************************************************************************/

int
getxn (fd, sp)

FILE *		fd;
char *		sp;

{

	register int	c;

	*sp = c = getc (fd);

	if (c >= '0' && c <= '9')
		return c - '0';
	else if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	else if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;

	return -1;

}


#ifdef	NEWSIG		/* new signal mechanism */

/************************************************************************
 *									*
 *	timoread/timowrite - read/write with timeout			*
 *									*
 ************************************************************************/

timoread(fd, ptr, size)
	char *ptr;
{
	if(setjmp(jb) == 0)
		return read(fd, ptr, size);
	return -1;
}
timowrite(fd, ptr, size)
	char *ptr;
{
	if(setjmp(jb) == 0)
		return write(fd, ptr, size);
	return -1;
}
#endif


/************************************************************************
 *									*
 *	fatal - display error message and abort				*
 *									*
 ************************************************************************/

int
fatal (s)

char *		s;

{

	write(toterm,&can,sizeof(can));	/* write cancel to terminal */
	write(toterm,nulls,nullcnt);
	fputs(s, stderr);
	fputs("\r\n", stderr);
	ttyrestore();		/* put terminal back to orig state	*/
	exit(1);

}

/************************************************************************
 *									*
 *	wantptload - inquire if the user wants a download		*
 *									*
 ************************************************************************/

#define	BUFSIZE		80	/* size of answer buffers		*/
#define	WANT_WAIT	15	/* time to wait for answer 		*/

wantptload ()
{
	char	answer[BUFSIZE];

	ttytalk();		/* talk slow - do not lose chars    */
	while (1)
	{
		sleep(1);		/* allow time to prevent truncation */
		putmsg(intro, 0);	/* output msg, 0 pad null per line */
		alarm(WANT_WAIT);
		timout=0;
		*answer = '\0';
		timoread(0, answer, sizeof answer);
		if(timout)
			return(TRUE);
		else
		{
			alarm(0); /* reset the alarm */
			switch (*answer)
			{
			case 'y':
			case 'Y':
			case '\n':
				return(TRUE);

			case 'n':
			case 'N':
				return(FALSE);

			default:
				putmsg(wrong, 0);
			}
		}
	}
}

/************************************************************************
 *									*
 *	putmsg - put a msg to terminal with null padding at end of line	*
 *									*
 ************************************************************************/

putmsg(text,pad)
char **text;	/* pointer to list of string pointers */
int	pad;	/* number of nulls to output after each line */
{
	while( *text != NULL )
	{
		register int i;
		char *ptr = *text;

		while( *ptr )
		{
			putchar(*ptr++);
		}
		putchar('\n');
		for( i = 0; i < pad; i++)
			putchar('\0');
		text++;
	}
}

/************************************************************************
 *									*
 *	whoisit - Determine if the PT terminal is already downloaded	*
 *									*
 *		returns PT1ROM if ROM responds to DA command		*
 *		returns PT1RAM if RAM responds to DA command		*
 *		returns INVRESP if no or invalid response received	*
 *									*
 ************************************************************************/

#define DA "\033[c"		/* ANSI DA ESC seq to send to terminal	*/

/* Note that in the following expected DA response sequences, only the
	first 7 chars are used to match.  This allows for future
	changes in the software version number without having to change
	this program. (The PT ROM & RAM respond with version number(s)
	in addition to the following characters.)			
*/

#define RAM1RSP "\033[=0;1;" /* ANSI DA ESC response from PT (CT) RAM	*/
#define ROM1RSP "\033[=0;0;" /* ANSI DA ESC response from PT (CT) ROM	*/
#define RAM2RSP "\033[=1;1;" /* ANSI DA ESC response from PT2 RAM	*/
#define ROM2RSP "\033[=1;0;" /* ANSI DA ESC response from PT2 ROM	*/
#define DATERM	'c'	     /* ANSI DA ESC sequence terminator		*/

#define ASK_WAIT	3	/* time to wait for ESC response	*/
#define ASK_TIMES	3	/* number of times to send DA req block	*/
#define MAX_WRONG_CHARS	30	/* max number of chars to wait for ESC	*/

whoisit ()

{
	char response[MAX_WRONG_CHARS+1];
	int i,j;

	ttyinit();		/* make port raw for ESC sequence	*/

	for( j=0 ; j < ASK_TIMES ; j++ )	/* allow retries */
	{
		ttyflush();	/* flush port before attempting com	*/

		/* send DA cmd, is terminal loaded ?			*/
		write (toterm,DA,sizeof(DA)-1);

		alarm(ASK_WAIT);
		timout = 0;
		response[0] = '\0';

		/* Synchronize on first character of expected response	*/

		while (response[0] != *ROM1RSP)
		{
			if(timoread(0,&response[0],sizeof(char)) != sizeof(char)) 
			{
				if(timout)
					break;
				alarm(0);
				puts("\n\rDA ESC char read error\n\r");
				return(FALSE);
			}
		}

		if(timout)	/* time-out here means no good response	*/
			continue;	/* try again			*/

		alarm(0);		/* make sure alarm is cleared	*/

		/* At this point we have the first char of expected
		   response.  Now get the rest of it.			*/

		for( i=1 ; ((response[i-1] != DATERM) &&
			    (i < MAX_WRONG_CHARS-1))   ; i++ )
			{
			alarm(ASK_WAIT);
			timout=0;
			if(timoread(0,&response[i],sizeof(char)) != sizeof(char)) 
				{
					if(timout)
						break;
					alarm(0);
					puts("\n\rDA string read error\n\r");
					return(FALSE);
				}
			alarm(0);	/* make sure alarm is cleared	*/
			}
		if(timout)	/* if time-out, did not get enough chars*/
			continue;	/* retry DA command		*/

		/* At this point we have at least a matching first char
		   in response.  Now compare the complete string read in
		   with the expected response.				*/

		if(strncmp(ROM1RSP,response,sizeof(ROM1RSP)-1) == 0)
			return(PT1ROM);
		if(strncmp(RAM1RSP,response,sizeof(RAM1RSP)-1) == 0)
			return(PT1RAM);
		if(strncmp(ROM2RSP,response,sizeof(ROM2RSP)-1) == 0)
			return(PT2ROM);
		if(strncmp(RAM2RSP,response,sizeof(RAM2RSP)-1) == 0)
			return(PT2RAM);
	}
	return(INVRESP);
}

/************************************************************************
 *									*
 * atobaud - convert an ascii number to a baud rate ala B50, B1200, ...	*
 *	returns -1 if invalid baud rate is specified.			*
 *									*
 ************************************************************************/

struct baud {
	char	*abaud;		/* name of baud rate */
	char	ibaud;		/* baud rate for ioclt(2) */
};

struct baud baudtbl[] = {
	{ "110",	B110	},
	{ "300",	B300	},
	{ "600",	B600	},
	{ "1200",	B1200	},
	{ "2400",	B2400	},
	{ "4800",	B4800	},
	{ "9600",	B9600	},
#ifdef B19200
	{ "19200",	B19200	},
#endif
	{ NULL,		0	}
};

char
atobaud(string)
char	*string;
{
	struct baud *bptr = baudtbl;	/* index in baud rate table */

	while( bptr->abaud != NULL )
	{
		if( strcmp( bptr->abaud, string) == 0)
			{
			switch (atoi(string))
				{
				case 19200:	nullcnt=14;break;
				case  9600:	nullcnt= 9;break;
				case  4800:	nullcnt= 8;break;
				case  2400:	nullcnt= 6;break;
				case  1200:	nullcnt= 5;break;
				case   300:
				case   110:	nullcnt= 3;break;
				default:	nullcnt=14;break;
				}
			return( bptr->ibaud );
			}
		++bptr;
	}
	return( (char) -1 );
}

/************************************************************************
 *									*
 *	bailout - alarm interrupt handler				*
 *									*
 ************************************************************************/


bailout ()
{
#ifdef	NEWSIG	/* new signal mechanism */
	timout = 1;
	longjmp(jb, 1);
#else
	signal(SIGALRM,bailout);
	timout = 1;
	return;
#endif
}

/************************************************************************
 *									*
 *	tty utilities for performing get & put ioctl's			*
 *									*
 ************************************************************************/

#define	IOC_DELAY 1	/* Delay is needed after ioctl to allow baud-
			   rate change to take effect.  This requirement
			   discovered on 2/21/84 when xmit of the DA
			   seq would result in the first DA attempt to
			   be sent at 9600 baud (irregardless of
			   intended setting) and the second DA to go
			   out at the intended (set by ioctl) rate.	*/

#ifdef	V7	/********* if Version 7 tty routines		*********/

struct	sgttyb tp_new;
struct	sgttyb tp_orig;
struct	tchars tc_new = { '\177', '\034', '\021', '\023', '\004', '\377' };
struct	tchars tc_orig;

ttysave ()
{
	ioctl(0, TIOCGETP, &tp_orig);
	ioctl(0, TIOCGETC, &tc_orig);
	baudrate = tp_orig.sg_ospeed;
}

ttyrestore ()
{
	ioctl(0, TIOCSETP, &tp_orig);
	ioctl(0, TIOCSETC, &tc_orig);
	sleep(IOC_DELAY);		/* allow baud to settle		*/
}

ttyinit ()
{
	/*
	 *	Use CBREAK mode for 'normal' download so that the Dow Jones
	 *	mux which generates XON/XOFF will be happy.  XON/XOFF appears
	 *	to screw up reliability mode, however, so stick with RAW
	 *	mode for that.
	 */
	ttysetup((short) ANYP + ((rflag) ? RAW : CBREAK));
}

ttytalk ()
{
	ttysetup((short)ANYP+ECHO+CRMOD);
}

ttysetup (flags)
short flags;
{

	tp_new.sg_flags = flags;
	tp_new.sg_ispeed = baudrate;
	tp_new.sg_ospeed = baudrate;
	tp_new.sg_stopbits = tp_orig.sg_stopbits;
	ioctl(0, TIOCSETP, &tp_new);
	ioctl(0, TIOCSETC, &tc_new);
	sleep(IOC_DELAY);		/* allow baud to settle		*/

}

ttyflush()
{
	ioctl(0,TIOCFLUSH,0);
}

#else		/********* else assume System 3 tty routines	*********/

struct	termio	term_orig;	/* save area for orig port state	*/
struct	termio	term_new;	/* new port state			*/

int	count;		/* used for 19200 baud delay			*/

ttysave()
{
	ioctl(0, TCGETA, &term_orig);	/* save the initial port state	*/
	baudrate = term_orig.c_cflag & CBAUD;	/* get orig baud	*/
}

ttyrestore()
{
	ioctl(0, TCSETAF, &term_orig);	/* restore saved port state	*/
	sleep(IOC_DELAY);		/* allow baud to settle		*/
}

ttyinit()
{
	term_new.c_iflag = ICRNL|ISTRIP|IXON;
	term_new.c_oflag = ONLRET|OPOST;
	term_new.c_cflag = baudrate|CREAD|CS8;
	term_new.c_lflag = 0;
	term_new.c_line  = 0;
	term_new.c_cc[VINTR]  = CINTR;
	term_new.c_cc[VQUIT]  = CQUIT;
	term_new.c_cc[VERASE] = 0x08;
	term_new.c_cc[VKILL]  = CKILL;
	term_new.c_cc[VMIN]   = 1;
	term_new.c_cc[VTIME]  = 1;

	ioctl( 0, TCSETAF, &term_new );
	sleep(IOC_DELAY);		/* allow baud to settle		*/

}

ttytalk()
{
	term_new.c_iflag = ICRNL|IXON;
	term_new.c_oflag = ONLCR|OPOST;
	term_new.c_cflag = baudrate|CREAD|CS8;
	term_new.c_lflag = ECHO|ECHOE|ECHOK|ICANON|ISIG;

	ioctl( 0, TCSETAF, &term_new );
	sleep(IOC_DELAY);		/* allow baud to settle		*/

}

ttyflush()
{
	ioctl( 0, TCFLSH, 2 );		/* flush input & output queues	*/
}

#endif		/********* end of System 3 tty routines		*********/

