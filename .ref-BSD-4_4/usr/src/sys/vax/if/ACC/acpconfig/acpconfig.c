
#define	VERSION	"13-Nov-1989\n"

/*************************************************************************/
/*									 */
/*									 */
/*	 ________________________________________________________	 */
/*	/							 \	 */
/*     |	  AAA	       CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*     |	 AAAAA	      CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     |	AAAAAAA	      CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |       AAAA AAAA      CCCC		CCCC		  |	 */
/*     |      AAAA   AAAA     CCCC		CCCC		  |	 */
/*     |     AAAA     AAAA    CCCC		CCCC		  |	 */
/*     |    AAAA       AAAA   CCCC		CCCC		  |	 */
/*     |   AAAA	 AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |  AAAA	  AAAAAAAAAAA CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     | AAAA	   AAAAAAAAA   CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*	\________________________________________________________/	 */
/*									 */
/*	Copyright (c) 1988 by Advanced Computer Communications		 */
/*	720 Santa Barbara Street, Santa Barbara, California  93101	 */
/*	(805) 963-9431							 */
/*									 */
/*									 */
/*  File:		acpconfig.c					 */
/*									 */
/*  Author:		Clare E. Russ					 */
/*									 */
/*  Project:		Installation verification program for ACC	 */
/*			ACP 5100/6100 and ACP 5250/6250 network		 */
/*			interface drivers.   Acpconfig provides a user	 */
/*			interface which supports the configuration of a	 */
/*			network	 interface.				 */
/*									 */
/*  Function:								 */
/*	  Based on socket ioctls, provide user interface to Network	 */
/*	  Interface Drivers for the following ACC ACP devices:		 */
/*									 */
/*		    ACP 5100/6100  'acp' interface			 */
/*		    ACP 5250/6250  'dda' interface			 */
/*		    ACP 625	   'ddn' interface			 */
/*									 */
/*	  The ioctl indicates what type of message is to be sent to the	 */
/*	  front end device (ie, bring up the HDLC line in external	 */
/*	  loopback mode).						 */
/*									 */
/*  Components:		acpconfig.c, ioctl.h				 */
/*									 */
/*									 */
/*  Usage Notes:							 */
/*									 */
/*	   -A filename							 */
/*		Add the contents of the named file to the address	 */
/*		translation table for PDN service			 */
/*									 */
/*	   -a ipaddr x25addr						 */
/*		Add the specified address pair to the address		 */
/*		translation table for PDN service			 */
/*									 */
/*	   -b baud							 */
/*		Note that the baud rate values are different for the	 */
/*		ACP 625.  Table 1 contains the baud rates which apply	 */
/*		to the ACP 6100, ACP 5250/6250 products.  Table 2	 */
/*		contains the baud rates which apply to the ACP 625.	 */
/*									 */
/*	  Table 1:  Nominal baud rates for ACP 5100/6100, ACP 5250/6250	 */
/*									 */
/*			2.00M  2000K  2.0M  1.33M  1333K  1.3M		 */
/*			1.00M  1000K  1.0M  500K   250K	  100K		 */
/*			64K    64000  56K   56000  30K	  19.2K		 */
/*			9.6K   9600   4.8K  4800   2.4K	  2400		 */
/*			1.2K   1200					 */
/*									 */
/*	  Table 2:  Nominal baud rates for ACP 625			 */
/*									 */
/*			316000	153600	 115200	  76800	  76.8K		 */
/*			 57600	 57.6K	  38400	  38.4K	  28800		 */
/*			 28.8K	 19200	  19.2K	   9600	   9.6K		 */
/*			  4800	  4.8K	   2400	   2.4K	   2150		 */
/*			  1760	  1200					 */
/*									 */
/*		An argument of 0 or "external" to the -b option		 */
/*		specifies external clocking for the ACP 5100/6100	 */
/*		and ACP 5250/6250 interfaces. (The ACP 625 does not	 */
/*		support this feature.)	 All other values imply		 */
/*		internal clocking.					 */
/*		The M for megabits/second and K for kilobits/second	 */
/*		are optional.  Note that Table 1 allows for the entry	 */
/*		of 9.6 (with the assumed unit of Kilobits/second), but	 */
/*		also allows for the entry of 9600 (bits/second).  Thus	 */
/*		there is more than one entry for some of the baud rate	 */
/*		values.							 */
/*									 */
/*	   -c msgnum							 */
/*		Toggle the enable status of driver message number msgnum */
/*									 */
/*	   -d ipaddr							 */
/*		Delete the specified address entry from the address	 */
/*		translation table for PDN service			 */
/*									 */
/*	   -D								 */
/*		Delete all address entries form the address translation	 */
/*		table for PDN service					 */
/*									 */
/*	   -e size							    */
/*		set the firmware buffer size to size.  This also resets	 */
/*		the board.  size comes from a table of valid sizes and	 */
/*		may also be "default" to get the default size.		 */
/*									 */
/*	   -h mode							 */
/*		Read driver histogram and print on standard output. Each */
/*		entry is of the form sec.usec. The entries are as	 */
/*		follows:						 */
/*		0    : number of seconds the link was up		 */
/*		1    : starting time					 */
/*		2    : ending time					 */
/*		3    : current value for tmo_data_idle			 */
/*		4-69 : seconds n logical channels were open for		 */
/*		       n = 0 to 64 (126)				 */
/*									 */
/*		If mode is 0 then data is read.	 If mode is 1 then data	 */
/*		is read and the histogram is reinitialized.		 */
/*									 */
/*	   -f parameter status						 */
/*		Control flow control parameter negotiation where	 */
/*		parameter is either "packet" or "window" and		 */
/*		status is either "on" or "off".				 */
/*		Note:  incoming flow control parameter negotiation	 */
/*		if not effected by this option.				 */
/*									 */
/*	   -l								 */
/*	   -ln								 */
/*		List the currently active lcns.  The 'n' suffix		 */
/*		disables address to name lookups.			 */
/*									 */
/*	   -m message							 */
/*		Pass ``message'' to the FEP as a supervisory channel	 */
/*		message.  Message is a sequence of numbers separated	 */
/*		by white space.	 Numbers with leading ``0'' are taken	 */
/*		as octal, other numbers taken as HEX, decimal is not	 */
/*		supported.  Hex and octal may be intermixed, as in	 */
/*		``0140 0 0 2 89 017''.	The message is terminated by the */
/*		end of the argument list or by an argument beginning	 */
/*		with a dash ``-''.  Absolutely no checking is performed; */
/*		the bytes are written to the FEP as a supervisor	 */
/*		message.  The message is limited to MLEN = 112 bytes.	 */
/*									 */
/*	   -N network							 */
/*		Set network type					 */
/*		transpac - french transpac network addressing		 */
/*		default	 - normal x25 network				 */
/*		net15	 - normal network but with 15 digit addrs	 */
/*									 */
/*	   -n circuits							 */
/*		Set the number of logical channels to ``circuits'', which*/
/*		must be less than some implementation defined maximum.	 */
/*									 */
/*	   -q query-type						 */
/*		Query the board or driver and place results on stdout.	 */
/*									 */
/*	   -q 0	 Statistics query to FEP				 */
/*	   -q 1	 Driver operating mode query				 */
/*	   -q 2	 Reserved for ACC debugging				 */
/*									 */
/*	   -r count							 */
/*		Read the specified number of entries from the address	 */
/*		translation table for PDN service			 */
/*									 */
/*	   -r 0	 read all entries from the address translation table	 */
/*									 */
/*	   -s X.25 service						 */
/*		Select DDN standard X.25 service or basic X.25 service	 */
/*		or Public Data Network X.25 service			 */
/*									 */
/*	   -s standard	select standard X.25 service			 */
/*	   -s basic	select basic X.25 service			 */
/*	   -s pdn	select X.25 Public Data Network service		 */
/*									 */
/*	   -t sec	Set idle circuit timeout			 */
/*									 */
/*	   -u down  bring down the link					 */
/*	   -u dte   bring up link, no loopback, DTE			 */
/*	   -u dce   bring up link, no loopback, DCE			 */
/*	   -u ext   bring up link, external loopback mode		 */
/*	   -u int   bring up link, internal loopback mode		 */
/*									 */
/*	   -v variable value						 */
/*		Set the value of a driver variable symbolized by	 */
/*		"variable" to "value" decimal.	"Variables" understood	 */
/*		are "window" and "packet" to set the driver's notion of	 */
/*		negotiable window size, and negotiable packet size.	 */
/*									 */
/*	   -z  reset the specifed front-end device			 */
/*									 */
/*  Compile Notes:							 */
/*									 */
/*	The associated makefile builds the acpconfig executable image	 */
/*	for UNIX or the specified emulation/simulation such as		 */
/*	The Wollongong Group (TWG) software which runs under VAX/VMS.	 */
/*	The makefile must be invoked with the argument which specifies	 */
/*	the target host operating system, otherwise the makefile	 */
/*	defaults to creating an executable image for UNIX.		 */
/*									 */
/*		  make		       (compile for UNIX 4.nBSD)	 */
/*		  make clean						 */
/*		  make print						 */
/*									 */
/*									 */
/*  System Notes:							 */
/*									 */
/*	Create a socket of the AF_INET address family and of type	 */
/*	datagram, SOCK_DGRAM.  Use the socket to send a socket ioctl	 */
/*	to the network driver of the specified interface ('acp0' in	 */
/*	the usage notes).   Depending on the type of message specified	 */
/*	in the command line, send the appropriate socket ioctl to	 */
/*	specify what kind of action is to be taken.			 */
/*									 */
/*	For those drivers which support the Control Interface (CIF)	 */
/*	the message exchange conforms to the CIF which defines paired	 */
/*	command/response Control Interface Messages (CIMs) between the	 */
/*	host and the front end.	  Otherwise, the exchange of messages	 */
/*	is the pre-CIF protocol (i.e., that used  by the ddn interface). */
/*									 */
/*	Assignment of subcodes for the SIOCACPCONFIG ioctl:		 */
/*									 */
/*	1..14		-bRATE for RATE != 0				 */
/*	`0'..`4'	-u N (line control)				 */
/*	`a'		-A and -a commands (PDN)			 */
/*	`b'		-b0 (set external clock)			 */
/*	`c'		-c msgnum					 */
/*	`d'		-d command (PDN)				 */
/*	`e'		-e size						 */
/*	`h'		-h 0						 */
/*	`H'		-h 1						 */
/*	`m'		-m (message command)				 */
/*	`n'		-n circuits (limit lcns)			 */
/*	`p'		-q 0 (statistics query)				 */
/*	`q'		-q 1 (driver query)				 */
/*	`r'		-r command (PDN)				 */
/*	`t'		-t sec (timeout set)				 */
/*	`S'		-s 0						 */
/*	`T'		-s 1						 */
/*	`U'		-s 2						 */
/*	`V'		-v command (set variable)			 */
/*	`z'		-z command (reset)				 */
/*									 */
/*************************************************************************/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								    %%*/
/*%%		   INCLUDE FILES				    %%*/
/*%%								    %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*   There are alternate path names for The Wollongong Group (TWG)    */
/*   IPTCP and Eunice software which runs under VMS,		      */
/*   and for TGV, Incorporated's MultiNet software which also runs    */
/*   under VMS.							      */

#ifdef	VAXVMS
#  ifdef eunice
#    define WINS
#  else
#    define MULTINET
#  endif
#endif

#ifdef	vax11c
#  define	EXIT_ERR	0
#  define	EXIT_OK		1
#else
#  define	EXIT_ERR	1
#  define	EXIT_OK		0
#endif

#include <sys/param.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>
#include <nlist.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#ifdef	MULTINET
#include <vaxif/if_ddaioctl.h>		/* multinet doesn't fix ioctl.h */
#endif

#ifdef	VAXVMS				/* TWG or TGV */
#include <netinet/in.h>
#include <net/if.h>
#else
#include "/sys/netinet/in.h"
#include "/sys/net/if.h"
#endif

#ifdef	MULTINET
#ifdef	errno
#undef	errno
#endif	errno
#define errno	socket_errno		/* MultiNet runtime errors	*/
#define Perror(s) XPerror(s)		/* Keep different from perror() */
#define ioctl(s,c,d) socket_ioctl(s,c,d)/* MultiNet ioctl() routine	*/
#endif	MULTINET

#define NDDA		1
#define NONE		0		/* used by the -o option */
#define EXTENDED	1		/* used by -o option */

/* 'iface' interface definitions */
#define ACP_INTERFACE	0x01		/* for acp interface */
#define DDN_INTERFACE	0x02		/* for ddn interface */

#define BAUD_VAL	50		/* for baud rate flag, -b */
#define SERVICE_VAL	35		/* for X.25 service flag, -s */
#define ACTUAL_VAL	83		/* for X.25 service flag 'S' */

/* network values */
#define NET_STANDARD	0		/* on standard network */
#define NET_TRANSPAC	1		/* on transpac style network */

/* delay macro from /sys/vax/param.h */

#ifdef vax11c
#define NEW_DELAY(n)	sleep(n / 100000)
#else
#define NEW_DELAY(n)	{ register int N = (n); while (--N > 0); }
#endif


/* The baud_rate structure maps the user-supplied baud rate into the */
/* parameter used in the socket ioctl to the driver.  In some cases, */
/* more than one representation of a value is present to ease the    */
/* interface.  The manual page for acpconfig recommends that the     */
/* user give the baud rate with the assumed unit of Kilobits/second. */

struct	baud  {
	char	*b_rate;
	char	parameter;
}  baud_rate[] = {
	{ "2.00",	1 },		/* these are nominal baud rates */
	{ "2.00M",	1 },
	{ "2000K",	1 },
	{ "2000k",	1 },
	{ "2.0",	1 },
	{ "2.0M",	1 },
	{ "1.33",	2 },
	{ "1.33M",	2 },
	{ "1333K",	2 },
	{ "1333k",	2 },
	{ "1.3",	2 },
	{ "1.3M",	2 },
	{ "1.00",	3 },
	{ "1.00M",	3 },
	{ "1000K",	3 },
	{ "1000k",	3 },
	{ "1.0M",	3 },
	{ "1.0",	3 },
	{ "500",	4 },
	{ "500K",	4 },
	{ "500k",	4 },
	{ "250",	5 },
	{ "250K",	5 },
	{ "250k",	5 },
	{ "100",	6 },
	{ "100K",	6 },
	{ "100k",	6 },
	{ "64",		7 },
	{ "64K",	7 },
	{ "64k",	7 },
	{ "64000",	7 },
	{ "56",		8 },
	{ "56K",	8 },
	{ "56k",	8 },
	{ "56000",	8 },
	{ "30",		9 },
	{ "30K",	9 },
	{ "30k",	9 },
	{ "19.2",	10 },
	{ "19.2K",	10 },
	{ "19.2k",	10 },
	{ "9.6",	11 },
	{ "9.6K",	11 },
	{ "9.6k",	11 },
	{ "9600",	11 },
	{ "4.8",	12 },
	{ "4.8K",	12 },
	{ "4.8k",	12 },
	{ "4800",	12 },
	{ "2.4",	13 },
	{ "2.4K",	13 },
	{ "2.4k",	13 },
	{ "2400",	13 },
	{ "1.2",	14 },
	{ "1.2K",	14 },
	{ "1.2k",	14 },
	{ "1200",	14 },
	{ 0,		0 },
};


/* Table of baud rate values and the associated parameter for the Set	*/
/* System Parameters message, ddninit_msg.  The 'parameter1' is nonzero */
/* for valid baud rate divisors.  The user's manual gives both the	*/
/* actual and nominal baud rates, either one is accepted from the user, */
/* but the nominal baud rate is the figure which is closest to the rate */
/* set on the front end.						*/

struct	baud	ddnbaud_rate[] =	{
	{ "333333",	1 },		/* actual baud rate  */
	{ "316000",	1 },		/* nominal baud rate */
	{ "153846",	2 },		/* actual baud rate  */
	{ "153600",	2 },		/* nominal baud rate */
	{ "114285",	3 },		/* actual baud rate  */
	{ "115200",	3 },		/* nominal baud rate */
	{ "76923",	4 },		/* actual baud rate  */
	{ "76800",	4 },		/* nominal baud rate */
	{ "76.8K",	4 },		/* nominal baud rate */
	{ "76.8k",	4 },		/* nominal baud rate */
	{ "57971",	5 },		/* actual baud rate  */
	{ "57600",	5 },		/* nominal baud rate */
	{ "57.6K",	5 },		/* nominal baud rate */
	{ "57.6k",	5 },		/* nominal baud rate */
	{ "38461",	6 },		/* actual baud rate  */
	{ "38400",	6 },		/* nominal baud rate */
	{ "38.4K",	6 },		/* nominal baud rate */
	{ "38.4k",	6 },		/* nominal baud rate */
	{ "28776",	7 },		/* actual baud rate  */
	{ "28800",	7 },		/* nominal baud rate */
	{ "28.8K",	7 },		/* nominal baud rate */
	{ "28.8k",	7 },		/* nominal baud rate */
	{ "19230",	8 },		/* actual baud rate  */
	{ "19200",	8 },		/* nominal baud rate */
	{ "19.2K",	8 },		/* nominal baud rate */
	{ "19.2k",	8 },		/* nominal baud rate */
	{ "9592",	9 },		/* actual baud rate  */
	{ "9600",	9 },		/* nominal baud rate */
	{ "9.6K",	9 },		/* nominal baud rate */
	{ "9.6k",	9 },		/* nominal baud rate */
	{ "4801",	10 },		/* actual baud rate  */
	{ "4800",	10 },		/* nominal baud rate */
	{ "4.8K",	10 },		/* nominal baud rate */
	{ "4.8k",	10 },		/* nominal baud rate */
	{ "2400",	11 },		/* actual baud rate  */
	{ "2.4K",	11 },		/* actual baud rate  */
	{ "2.4k",	11 },		/* actual baud rate  */
	{ "2150",	12 },		/* nominal baud rate */
	{ "1760",	13 },		/* actual baud rate  */
	{ "1760",	13 },		/* actual baud rate  */
	{ "1200",	14 },		/* nominal baud rate */
	{ "1.2K",	14 },		/* nominal baud rate */
	{ "1.2k",	14 },		/* nominal baud rate */
	{ 0,		0 },
};

#ifndef MULTINET
char	*kmemf = "/dev/kmem";
int	kmem;
#endif	MULTINET


#include <strings.h>
#include <sys/time.h>

#ifdef VAXVMS
#  ifdef WINS
#    include <vaxif/if_ddareg.h>
#    include <vaxif/if_ddavar.h>
#  endif
#  ifdef MULTINET
#    include "[-.kernel.vaxif]if_ddareg.h"
#    include "[-.kernel.vaxif]if_ddavar.h"
#  endif
#else VAXVMS				/* must be unix or ultrix */
#  include "/sys/vaxif/if_ddareg.h"
#  include "/sys/vaxif/if_ddavar.h"
#endif VAXVMS

/* EXTERNAL FUNCTIONS */
extern	char *routename();
extern	off_t lseek();
extern	u_long inet_addr();
extern	char *inet_ntoa();
extern	u_long inet_network();

/*
 *	Weirdness for test-jig, re-map all ioctl and socket calls to go
 *	through driver simulator.
 */

#ifdef	SIMULATION
#define ioctl	fake_unix_ioctl
#define socket	fake_unix_socket
#define main	cf_main
#endif

/*
 *	Print contents of queues and various data structures
 */

char *state_tab[] = {
        "Down",
	"Rstrt",
	"Idle",
	"Call",
	"Open",
	"Clear"
	};
#define NSTATES 5




/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%			     MAIN()				    %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*								      */
/*  Purpose:							      */
/*								      */
/*	For the specified interface, create a socket for a socket     */
/*	ioctl to set the Internet address and configuration.  The     */
/*	ioctl kicks the appropriate driver, the value in ifr_data     */
/*	indicates the type of action to be taken.  ('b' indicates     */
/*	external clock, 1-14 indicates baud rate from the table of    */
/*	possible values, '0' - '4' indicates -u options, and 's','t', */
/*	'u' indicate -s options.)				      */
/*	Enhancement: 'a' indicates addition of an address table	      */
/*	entry, 'd' indicates deletion of an address table entry,      */
/*	and 'r' is a request to read address table entries.	      */
/*	Etc, etc (see comment at top for complete list)		      */
/*								      */
/*  Call:		main( argc, argv)			      */
/*  Arguments:		argc:	argument count			      */
/*			argv:	argument value			      */
/*  Returns:		nothing					      */
/*  Called by:		invoked by privileged user		      */
/*  Calls to:		socket()				      */
/*			perror()				      */
/*			strcpy()				      */
/*			strncpy()				      */
/*			sizeof()				      */
/*			usage()					      */
/*			long_usage()				      */
/*			get_bfrsize_index()			      */
/*			ioctl()					      */
/*			exit()					      */
/*			Perror()				      */
/*								      */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

struct	ifreq ifr;
struct	sockaddr_in sin = { AF_INET };
char	name[30];
int	s;			/* socket descriptor */
int	nflag;			/* control host name lookup (symbolic) */
struct ddactl ddactl;
struct trtab trtab;
struct lg2tab {			/* For use with packet sizes, so just the */
	int base2log;		/* legal packet sizes for X.25 are here */
	int binval;
} lg2tab[] = {
	4,	16,
	5,	32,
	6,	64,
	7,	128,
	8,	256,
	9,	512,
	10,	1024,
	11,	2048,
	12,	4096,
	0,	0,
};

char *fe_bfr_sizes[] =
{
    "default",		/* entry 0: Default Size */
    "256",		/* entry 1: 256 bytes */
    "512",		/* entry 2: 512 bytes */
    "1024",		/* entry 3: 1K bytes (currently same as default) */
    "2048",		/* entry 4: 2K bytes */
    "4096",		/* entry 5: 4K bytes */
    "8192",		/* entry 6: 8K bytes */
    "16384",		/* entry 7: 16K bytes */
    NULL
};

extern int errno;


/*
 * Structure used to hold the results of an nlist library
 * function call.
 */

struct nlist nl[] = {
	{ "_dda_softc" },
#define SOFTC	0
	"",
};


#ifndef VAXVMS
/*
 * Seek into the kernel for a value.
 * Used by the -l option of acpconfig.
 */
klseek(fd, base, off)
int fd;
off_t base;
int off;
{

	if (lseek(fd, base, off) == (off_t)-1 ){
		fprintf(stderr,"acpconfig:  Bad lseek fd=%d,bas=%x,off=%d ",fd,base,off);
		perror("");
		exit(1);
	}
}

klread(fd,buf,size)
int fd;
char *buf;
int size;
{
	if ( read(fd,buf,size) < 0 ){
		perror("Read");
		exit(2);
	}
}
#endif VAXVMS

/*
 *	Routine used in conjunction with the -l option.
 *	This function displays the status of each active
 *	lcn for the given unit.
 */

display(addr,nddach)
off_t addr;
int nddach;  /* number of circuits currently available */
{
	struct dda_softc dda_softc;
	register struct dda_cb *dc;
	struct sockaddr_in sin;
	char *p;
	int i;
	int header;

	if (addr == 0) {
		printf("acpconfig: nlist--symbol not defined\n");
		return;
	}
#ifdef	MULTINET
	klseek(addr);
	klread((char *)&dda_softc, sizeof(dda_softc));
#else	MULTINET
	klseek(kmem, addr, 0);
	klread(kmem, (char *)&dda_softc, sizeof(dda_softc));
#endif	MULTINET
	header = 0;
	for ( i=0; i<= nddach ; i++)
 	{
	    dc = &dda_softc.dda_cb[i];
	    if ((dc->dc_state != LC_IDLE) && (dc->dc_state != LC_DOWN))
	    {
		if (!header) 
		{
	    	    printf("\n\t\t\tACP 6250 / 5250 (dda%d) Status\n", 
			dda_softc.dda_if.if_unit);
		    printf("lcn Qlen Dropped Flags Timer State  Pin Pout Win Wout  IP Addr\n");
		}
		header++;
		printf("%3d ",i);
	    	printf("%4d ",dc->dc_oq.ifq_len);
	    	printf("%6d  ",dc->dc_oq.ifq_drops);
		switch (dc->dc_flags & DC_CLIENTS) {
		    case DC_X29:	p="Pad "; break;
		    case DC_X29W:	p="Host"; break;
		    case DC_RAW:	p="PI  "; break;
		    case DC_IP:		p="IP  "; break;
		    default:		p="bug!"; break;
		}
		printf("%c %s", (dc->dc_flags & DC_OBUSY ? 'B' : ' '), p);
	        printf(" %3d  ", dc->dc_timer);
		if (dc->dc_state > NSTATES)
		    printf ("%4x? ", dc->dc_state);
		else
		    printf("%-5s ", state_tab [dc->dc_state]);
		printf(dc->dc_pktsizein ? "%4d ":" ??? ",1<<dc->dc_pktsizein);
		printf(dc->dc_pktsizeout ?"%4d ":" ??? ",1<<dc->dc_pktsizeout);
		printf(dc->dc_wsizein ? "%3d ":"??? ",dc->dc_wsizein);
		printf(dc->dc_wsizeout ?"%3d ":"??? ",dc->dc_wsizeout);
		if ((dc->dc_flags & DC_CLIENTS) == DC_IP) 
		    printf("  %s",routename(dc->dc_inaddr));
		putchar('\n');
	    }
        }
	if ((!header) && (i == nddach + 1))
	    printf("lcns 0 through %d inactive\n",nddach);

	sin.sin_addr.s_addr = dda_softc.dda_ipaddr.s_addr;
	printf("Our addr: %s", routename(sin.sin_addr));
	(void)putchar('\n');
}

char *
routename(in)
	struct in_addr in;
{
	char *cp = 0;
	static char line[50];
	int lna, net;

	net = inet_netof(in);
	lna = inet_lnaof(in);
	if (!nflag) {
		if (lna == INADDR_ANY) {
			struct netent *np = getnetbyaddr(net, AF_INET);

			if (np)
				cp = np->n_name;
		} else {
			struct hostent *hp;

			hp = gethostbyaddr((char *)&in, sizeof (struct in_addr),
				AF_INET);
			if (hp)
				cp = hp->h_name;
		}
	}
	if (cp)
		(void)strcpy(line, cp);
	else {
		u_char *ucp = (u_char *)&in;
		if (lna == INADDR_ANY)
			(void)sprintf(line, "%u.%u.%u", ucp[0], ucp[1], ucp[2]);
		else
			(void)sprintf(line, "%u.%u.%u.%u", ucp[0], ucp[1],
				ucp[2], ucp[3]);
	}
	return (line);
}




get_bfrsize_index(str)
char *str;
{
	register int i;

	for(i = 0; fe_bfr_sizes[i]; i++)
		if(strcmp(str,fe_bfr_sizes[i]) == 0)
			return(i);
	fprintf(stderr,"Invalid size: %s\nBuffer sizes available are: ");
	for(i = 0; fe_bfr_sizes[i]; i++)
		fprintf(stderr,"%s ",fe_bfr_sizes[i]);
	fprintf(stderr,"\n");
	return(-1);
}

main(argc, argv)
	int argc;
	char *argv[];
{
    register struct baud *p;            /* baud rates */
    int	iface = 0;			/* indicate interface, i.e. acp */
    int tmp, top, offset;
    int unit;
    FILE *fp;
    char line[80], arg1[40], arg2[40];
    char *ap;
    char option_byte1 = 0;
    int o_flag = 0;

    if (argc == 1)
    {
	long_usage();
    }
    if (argc < 3) {
	fprintf(stderr,"\n acpconfig:  invalid number of arguments\n");
	usage();		/* display proper syntax for user */
	}
    s = socket(AF_INET, SOCK_DGRAM, 0);		/* try create socket */
    if (s < 0) {
	perror("acpconfig: socket");
	exit(1);
    }
    argc--, argv++;		/* interface name */
    (void)strcpy(name, *argv);
    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
    if ((*argv)[2] == 'n')	/* check for 'ddn' interface */
	iface |= DDN_INTERFACE;
    if ((*argv)[0] == 'a')	/* check for 'acp' interface */
	iface |= ACP_INTERFACE;

    if (ioctl(s, SIOCGIFFLAGS, (caddr_t)&ifr) < 0) {
	Perror("ioctl (SIOCGIFFLAGS)");
	exit(1);
    }
    unit = (*argv)[3] & 0x0F;
    while (--argc > 0) {
	argv++;
	if(**argv != '-')
	   setifaddr(*argv);		/* set interface address */
	else {
	   ifr.ifr_data = 0;
	   switch((*argv)[1]) 	/* process flag(s) */
	   {	
	   case 'b':			/* set baud rate or ext clocking */
		ap = *argv;
		if (ap[2] == '\0')
		{	/* -b N case */
			argc--, argv++, ap = *argv;
			if (ap == 0)
				ap = "???";
		}
		else
		    ap += 2;		/* -bN case, skip over the "-b" */

		/* ap points to purported number, regardless -bN or -b N */
		if ((*ap == '0') || (strcmp(ap, "external") == 0))
		{
			if (iface&DDN_INTERFACE)
			{
				fprintf (stderr,
					"\nacpconfig %s: -b 0 invalid\n",
						name);
				break;
			}
			arg1[0] = 'b';
			ifr.ifr_data = arg1;
		}
		else 	/* set baud rate, if N is valid */
		{	
			if (iface&DDN_INTERFACE)
				p = ddnbaud_rate;
			else
				p = baud_rate;
			while (p->b_rate)
			{
				if (strcmp (ap, p->b_rate) == 0)
					break;
				p++;
			}
			if (p->parameter)
				ifr.ifr_data = &(p->parameter);
		}
		if (setconfig(ifr.ifr_data, s))
		{
		    fprintf(stderr,"\nacpconfig: -b %s invalid\n", ap);
		    usage();
		}
		break;

	    case 'l':			/* monitor status of lcns */
		if ((*argv)[2] == 'n' && (*argv)[3] == '\0')
		    nflag = 1;		/* "-ln" - numeric */
		else if ((*argv)[2] == '\0')
		    nflag = 0;		/* "-l" - symbolic */
		else
		    usage();		/* other - wrong. */

		if (strlen(*argv) < 3) {
#ifdef VAXVMS
#  ifdef MULTINET
		    if(multinet_kernel_nlist("MULTINET_NETWORK_IMAGE:",nl) ==-1)
#  else  MULTINET	/* must be TWG instead */
		    char *vmsnlist();
		    kmemf = vmsnlist();
		    if(nlist(kmemf, nl) == -1)
#  endif MULTINET
#else VAXVMS
		    if(nlist("/vmunix", nl) == -1)
#endif VAXVMS
		    {
			fprintf(stderr,"acpconfig: could not open /vmunix\n");
			exit(1);
		    }
		    if (nl[SOFTC].n_value == 0) {
			fprintf(stderr, "acpconfig:  No namelist\n");
			exit(1);
		    }
#ifndef VAXVMS
		    kmem = open (kmemf, 0);
		    if (kmem < 0) {
			fprintf(stderr, "acpconfig:  cannot open ");
			perror(kmemf);
			exit(1);
		    }
#endif
		    ddactl.func = 'n';
		    ddactl.drval = 0;
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0)
		    {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		    display((off_t)(nl[SOFTC].n_value+unit * sizeof(struct dda_softc)),*(int *)ifr.ifr_data);
		}
		else
		    usage();
		(void)close(kmem);
		break;
	    case 'o':			/* collect X.25 1984 options */
		{
		    char *cp = *argv;
		    if (strlen (cp) > 2) /* handle -oNAME and -o NAME */
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
			if (cp == 0)  {
			  fprintf (stderr, "acpconfig: -o what?\n");
			  exit(1);
			}
		    }
		    if (strcmp(cp,"none") == 0)
			option_byte1 = NONE;
		    else
			if (strcmp(cp,"extended") == 0)
			    option_byte1 |= EXTENDED;
			else {
			    fprintf(stderr,"acpconfig: -o %s invalid\n",cp);
			    exit(1);
			}
		o_flag++;
		}
		break;
	    case 's':
		/* select DDN standard X.25 service  */
		/* or basic X.25 service, accept -sn */
		/* or -s n  */

		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"\n acpconfig:  '-s' flag invalid for");
		    fprintf(stderr," specified interface\n");
		    break;
		}
		ifr.ifr_data = 0; 		/* clear value */
		if (!(*argv)[2]) {
		    offset = 0;
		    argc--;
		    argv++;
		}
		else
		    offset = 2;

		if (((*argv)[offset] >='0' && (*argv)[offset]< '3')
		   && (*argv)[offset + 1] == 0) {
		    (*argv)[offset] += SERVICE_VAL;     /* match 'S','T','U' */
		    ifr.ifr_data = &((*argv)[offset]);
		}
		else
		    if ((strncmp(&(*argv)[offset],"standard",8)==0)
			 && (*argv)[offset + 8] == 0) {
			(*argv)[offset] = ACTUAL_VAL;	/* match 'S' */
			ifr.ifr_data = &((*argv)[offset]);
		    }
		    else
			if ((strncmp(&(*argv)[offset],"basic",5)==0)
			  && (*argv)[offset + 5] == 0) {
			    (*argv)[offset] = ACTUAL_VAL + 1;	/* match 'T' */
			    ifr.ifr_data = &((*argv)[offset]);
			}
			else
			    if ((strncmp(&(*argv)[offset],"pdn",3)==0)
				 && (*argv)[offset + 3]==0) {
				(*argv)[offset] = ACTUAL_VAL + 2; /* 'U' */
				ifr.ifr_data = &((*argv)[offset]);
			    }
			    else
			    {
				fprintf(stderr,
			       "\nacpconfig:  invalid X.25 service\n");
				usage();/* display proper syntax for user */
			    }

		if ((ifr.ifr_data == 0) || setconfig(ifr.ifr_data, s)) {
		    if (errno == EALREADY) {
			fprintf(stderr,"acpconfig: must shut down interface to\
 change modes between DDN and PDN\n");
			exit(1);
		    }
		}
		break;
	
	    case 'u':
		{
		    char *vp = *argv;	/* value to set it to. */
		    char *cp;
		    ifr.ifr_data = NULL;

		    if (vp[2] == '\0')
		    {   argc--;		/* value is next arg; fix pointers */
			argv++;
			vp = *argv;
		    }
		    else
			vp += 2;	/* value at 3rd char of this arg */

		    if (argc > 1) {
			fprintf(stderr, "-u command must be last on line\n");
			exit(1);
		    }

		    if (strcmp(vp, "down") == 0 || *vp == '0')
			ifr.ifr_data = "0";
		    if (strcmp(vp, "dte") == 0 || *vp == '1')
			ifr.ifr_data = "1";
		    if (strcmp(vp, "dce") == 0 || *vp == '2')
			ifr.ifr_data = "2";
		    if (strcmp(vp, "ext") == 0 || *vp == '3')
			ifr.ifr_data = "3";
		    if (strcmp(vp, "int") == 0 || *vp == '4')
			ifr.ifr_data = "4";

		    if (ifr.ifr_data == 0) {
			fprintf(stderr,"acpconfig: -u invalid mode '%s'\n",
				*vp);
			usage();    /* display proper syntax for user */
		    }
		    else {
			if ( setconfig(ifr.ifr_data, s) ) {
			    if (errno == EADDRNOTAVAIL) {
				fprintf(stderr,
					"acpconfig: no local X.25 address \
translation in table; cannot start up PDN mode\n");
				exit(1);
			    }
			    else {
				usage();    /* display proper syntax for user */
			    }
			}
		    }
		}
		break;
	

	    case 'f':		/* control initiation of flow control */
				/* parameters netgotiation. */
		{
		    char *cp;	/* keyword determining which parameter */
				/* to control*/
		    char *vp;	/* value to set it to. */

		    cp = *argv;
		    if (strlen (cp) > 2) /* handle -fNAME VAL and -f NAME VAL */
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
			if (cp == 0)  {
			  fprintf (stderr, "acpconfig: -f what?\n");
			  exit(1);
			}
		    }
		    argc--; argv++; vp = *argv;
		    if (vp == 0) {
			fprintf (stderr, "acpconfig: -f %s what?\n", cp);
			exit(1);
		    }
		    if (strcmp(cp,"packet") == 0)
			ddactl.msg[0] = 0;
		    else
			if (strcmp(cp,"window") == 0)
			    ddactl.msg[0] = 1;
			else {
			    fprintf(stderr,"acpconfig: -f %s invalid\n",cp);
			    exit(1);
			}
		    if (strcmp(vp,"on") == 0)
			ddactl.drval = 1;
		    else
			if (strcmp(vp,"off") == 0)
			    ddactl.drval = 0;
			else {
			    fprintf(stderr,"acpconfig: -f %s invalid\n",cp);
			    exit(1);
			}
		    ddactl.func = 'f';
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;
	    case 'h':		/* histogram information */
		{
		    char *cp = *argv;
		    struct timeval databuf[HISTSIZE];
		    register int i;

		    if (strlen (cp) > 2)
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
			if (cp == 0) {
				fprintf(stderr, "acpconfig: -h what?\n");
				exit(1);
			}
		    }
		    *((char *)databuf) = (*cp == '1') ? 'H' : 'h';
		    ifr.ifr_data = (caddr_t) databuf;
		    (void)strncpy (ifr.ifr_name, name, sizeof(ifr.ifr_name));
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		    if(*(cp+1) == 'r')
		    {
			printf("%ld.%06ld\t",databuf[H_LINK_UP].tv_sec,databuf[H_LINK_UP].tv_usec);
			printf("%ld.%06ld",databuf[H_START].tv_sec,databuf[H_START].tv_usec);
			printf("\t%ld.%06ld",databuf[H_END].tv_sec,databuf[H_END].tv_usec);
			printf("\t%ld.%06ld",databuf[H_TMO].tv_sec,databuf[H_TMO].tv_usec);
			for(i = 0; i < H_LINK_UP; i++)
				printf("\t%ld.%06ld",databuf[i].tv_sec,databuf[i].tv_usec);
			printf("\n");
		    }
		    else
			    hist_data_out(databuf);
		}
		break;

	    case 'm':		/* Send supervisory message to FEP */
		{
		    char *mp = ddactl.msg;
		    int i;
		    static char *xfmt = "%x";
		    static char *ofmt = "%o";

		    bzero (ddactl.msg, sizeof (ddactl.msg));
		    while (--argc > 0) {
			argv++;
			if (*argv[0] == '-') {	/* hit a non-number */
			    argc++;			/* fix these guys for */
			    argv--;			/* the main loop */
			    break;
			}
			if (mp > &ddactl.msg[sizeof (ddactl.msg)]) {
			    fprintf (stderr,"\nacpconfig: -m message too long\n");
			    exit(1);
			}
			(void)sscanf (*argv, (**argv == '0') ? ofmt : xfmt, &i);
			*mp++ = i;
		    } /* while */
		    ddactl.func = 'm';
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;

	    case 'n':		/* set SVC limit */
		{
		    char *ap = *argv;

		    if (ap[2] == '\0')
		    {   argc--;		/* value is next arg; fix pointers */
			argv++;
			ap = *argv;
		    }
		    else
			ap += 2;	/* value at 3rd char of this arg */
		    ddactl.func = 'n';
		    ddactl.drval = atoi(ap);
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0)
		    {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		    else if (*(int *)ifr.ifr_data != 0)
			printf("acpconfig: %s: %d circuits currently available.\n",name,*(int *)ifr.ifr_data);
		}
		break;
	    case 'q':		/* query board or driver for status */
		{
		    char *cp = *argv;
                    int   mode;

		    if (strlen (cp) > 2)
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
			if (cp == 0) {
			    fprintf(stderr, "acpconfig: -q what?\n");
			    exit(1);
			}
		    }
		    mode = atoi(cp);

		    ddactl.func   = 'q';
		    ddactl.msg[0] = mode;

		    ifr.ifr_data = (caddr_t) &ddactl;
		    (void)strncpy (ifr.ifr_name, name, sizeof(ifr.ifr_name));
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			Perror("ioctl (SIOCACPCONFIG) returns");
			exit(1);
		    }

	            switch (mode) {
		      case 0:	 fmtstats(ddactl.msg); break;
		      case 1:	 fmtmode(ddactl.msg);  break;
		      case 2:	 fmtsilo(ddactl.msg);  break;
		    }
		}
		break;

	    case 't':		/* set idle circuit timeout limit */
		{
		    char *ap = *argv;

		    if (ap[2] == '\0')
		    {
			argc--;		/* value is next arg; fix pointers */
			argv++;
			ap = *argv;
		    }
		    else
			ap += 2;	/* value at 3rd char of this arg */
		    ddactl.func = 't';
		    ddactl.drval = atoi(ap);
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0)
		    {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;

	    case 'c':		/* disable/enable driver console message */
		{
		    char *ap = *argv;
			    int val;

		    if (ap[2] == '\0')
		    {
			argc--;		/* value is next arg; fix pointers */
			argv++;
			ap = *argv;
		    }
		    else
			ap += 2;	/* value at 3rd char of this arg */
		    if ((ap == NULL) || (*ap == '-') || (*ap == '\0')) {
			fprintf(stderr, "\nacpconfig: no message number specified for -c\n");
			exit(1);
		    }
		    ddactl.func = 'c';
		    val = ddactl.drval = atoi(ap);
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0)
		    {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		    else
		    {
			printf("acpconfig: %s: Driver message #%d is now %s\n",name,val,*(u_char *)ifr.ifr_data ? "disabled" : "enabled");
		    }
		}
		break;
	    case 'N':		/* set net id */
		{
		    char *vp = *argv;	/* value to set it to. */
		    char *cp;
		    int  net_type = -1;

		    if (vp[2] == '\0')
		    {   argc--;		/* value is next arg; fix pointers */
			argv++;
			vp = *argv;
		    }
		    else
			vp += 2;	/* value at 3rd char of this arg */

		    if (strcmp(vp, "transpac") == 0)
			net_type = NET_TRANSPAC;
		    if (strcmp(vp, "0") == 0 || strcmp(vp, "standard") == 0)
			net_type = NET_STANDARD;
		
		    if (net_type != -1) {		
			ddactl.func  = 'N';
			ddactl.drval = net_type;
			ifr.ifr_data = (caddr_t) &ddactl;
			if (setconfig(&ifr.ifr_data, s))
			    exit(1);
		    }

		    if (net_type == NET_TRANSPAC || strcmp(vp, "net15") == 0) {
						/* allow 15 digit addrs */
			ddactl.func = 'm';
			bzero(ddactl.msg, sizeof (ddactl.msg));
			bcopy("\140\0\0\2\177\17", ddactl.msg, 6);
			ifr.ifr_data = (caddr_t) &ddactl;
			if (setconfig(&ifr.ifr_data, s))
			    exit(1);
		    }
		}
		break;

	    case 'v':		/* set variable */
		{
		    char *cp;	/* keyword determining what to set */
		    char *vp;	/* value to set it to. */
		    int i, val;
		    /*
		     * The ORDERING of this table must match the
		     * cases in the processing for this ioctl
		     * inside the driver (kludgy interface!)
		     */
		     /* debug, dbgunit, and log are obsolete */
		    static char *nametab[] = {
			"log", "debug", "dbgunit", "packet", "window", 0
		    };

		    cp = *argv;
		    if (strlen (cp) > 2) /* handle -vNAME VAL and -v NAME VAL */
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
			if (cp == 0)  {
			  fprintf (stderr, "acpconfig: -v what?\n");
			  exit(1);
			}
		    }
		    argc--; argv++; vp = *argv;
		    if (vp == 0) {
			fprintf (stderr, "acpconfig: -v %s what?\n", cp);
			exit(1);
		    }
		    for (i = 0; nametab[i] != 0; i++)
			if (0 == strcmp (nametab[i], cp))
			    goto strmatch;
		    fprintf (stderr, "acpconfig: -v: \"%s\" unknown\n", cp);
		    exit(1);

		strmatch:	/* hit the keyword, and "i" is its index */
		    if (i < 3)	/* log, debug: masks: read in hexadecimal */
			(void)sscanf (vp, "%x", &val);
		    else	/* window, packet: counts: read in decimal */
			(void)sscanf (vp, "%d", &val);
		    if (i == 3) {	/* packet size: take base 2 log */
			struct lg2tab *lp;

			/*
			 * The sad thing is that after putting in all this
			 * code to take the base 2 log, I realized that the
			 * FEP takes this argument in BYTES and does the
			 * log internally.  Left the code in, though, since
			 * it's the easiest way to check the argument.
			 * (The driver converts to bytes with a shift).
			 */
			for (lp = lg2tab; lp->base2log; ++lp)
			    if (lp->binval == val) {
				val = lp->base2log;
				goto goodsize;
			    }
			fprintf (stderr, "acpconfig: -v: bad packet size\n");
			exit(1);
		    }

		goodsize:
		    ddactl.drval = val;
		    ddactl.msg[0] = i;
		    ddactl.func = 'v';
		    (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;

	    case 'e':
		{
		char *cp = *argv;
		char databuf[10];
		register int i;

		if (strlen (cp) > 2)
		    cp += 2;
		else
		{
		    argc--; argv++; cp = *argv;
		}
		databuf[0] = 'e';
		i = get_bfrsize_index(cp);
		if(i == -1)
		    exit(1);  /* get_bfrsize_index will print error message */
		else
		    databuf[1] = i;
		ifr.ifr_data = (caddr_t) databuf;
		if (!setconfig(ifr.ifr_data, s))
		    fprintf(stderr,"\nacpconfig:  buffer reset in progress, wait 1 minute\n");
		break;
		}
	    case 'z':
		/* reset the specified front-end device*/

		ifr.ifr_data = &((*argv)[1]);
		if (!setconfig(ifr.ifr_data, s))
		    fprintf(stderr,"\nacpconfig:  reset in progress, wait 1 minute\n");
		break;

	    case 'x':

		ifr.ifr_data = &((*argv)[1]);
		if (setconfig(ifr.ifr_data, s))
		    fprintf(stderr,"acpconfig: Could not create UCBs\n");
		break;
	
	    case 'A':
		/* add a file of address translation table entries */
		/* accept "-A filename OR "-Afilename"             */

		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"acpconfig: '-A' flag invalid for specified \
interface\n");
		    exit(1);
		}
		ifr.ifr_data = 0;	/* clear value */
		if (strlen(*argv) > 2)
			fp = fopen(&((*argv)[2]),"r");
		else {
			argc--, argv++;
			fp = fopen(*argv,"r");
		}
		if (fp==NULL)
		{	fprintf(stderr,"acpconfig: cannot open file '%s'\n",
			  ((*argv)[0] == '-') ? &((*argv)[2]) : *argv);
			exit(1);
		}
		fprintf(stdout,
			"acpconfig: processing file '%s'; please wait...\n",
			  ((*argv)[0] == '-') ? &((*argv)[2]) : *argv);
		while (fgets(line, 80, fp) != NULL)
		{	if (line[0] == '#' || line[0] == '\n')
				continue;
			(void)sscanf (line, "%s %s", arg1, arg2);
/*
			fprintf(stdout,
				"acpconfig: processing translation %s ==> %s\n",
				arg1, arg2);
*/
			if ((top=strlen(arg2)) > (MAXADDRLEN-1) || top < (MINADDRLEN-1))
			{	fprintf(stderr,
				  "acpconfig: bad X.25 address length: '%s'\n",
				  arg2);
				exit(1);
			}
			for (tmp=0; tmp<top; tmp++)
			    if (arg2[tmp] > '9' || arg2[tmp] < '0')
			    {	fprintf(stderr,
				  "acpconfig: invalid X.25 address '%s'\n",
				  arg2);
				exit(1);
			    }
			getaddr(arg1,&sin);
			trtab.func = 'a';
			trtab.ipaddr = sin.sin_addr.s_addr;
			trtab.x25addr[0]=strlen(arg2);
			(void)strncpy((char *)&trtab.x25addr[1], arg2, MAXADDRLEN-1);
			ifr.ifr_data = (caddr_t)&trtab;
			(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
			if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0)
			{
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
			}
		}
		(void)fclose(fp);
		break;
	

	    case 'a':
		/* add an address translation table entry */
		/* accept "-a inet_addr x25_addr"         */
		/* OR "-ainet_addr x25_addr"              */

		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"acpconfig: '-a' flag invalid for specified \
interface\n");
		    exit(1);
		}
		ifr.ifr_data = 0;	/* clear value */
		if (strlen(*argv) > 2)
			getaddr(&((*argv)[2]),&sin);
		else {
			argc--, argv++;
			getaddr(*argv,&sin);
		}
		trtab.func = 'a';
		trtab.ipaddr = sin.sin_addr.s_addr;
		argc--, argv++;
		if ((top=strlen(*argv)) > (MAXADDRLEN-1) || top < (MINADDRLEN-1))
		{	fprintf(stderr,
			  "acpconfig: bad X.25 address length: '%s'\n", *argv);
			exit(1);
		}
		for (tmp=0; tmp<top; tmp++)
		    if ((*argv)[tmp] > '9' || (*argv)[tmp] < '0')
		    {	fprintf(stderr,
			  "acpconfig: invalid X.25 address '%s'\n",
			  *argv);
			exit(1);
		    }
		trtab.x25addr[0]=strlen(*argv);
		(void)strncpy((char *)&trtab.x25addr[1], *argv, MAXADDRLEN-1);
		ifr.ifr_data = (caddr_t)&trtab;
		(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			Perror("ioctl (SIOCACPCONFIG) returns");
			exit(1);
		}
		break;

	    case 'd':
		/* delete an address translation table entry */
		/* accept "-d inet_addr" OR "-dinet_addr"    */
		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"acpconfig: '-d' flag invalid for specified \
interface\n");
		    exit(1);
		}

		ifr.ifr_data = 0;	/* clear value */
		if (strlen(*argv) > 2)
			getaddr(&((*argv)[2]),&sin);
		else {
			argc--, argv++;
			getaddr(*argv,&sin);
		}
		trtab.func = 'd';
		trtab.ipaddr = sin.sin_addr.s_addr;
		ifr.ifr_data = (caddr_t)&trtab;
		(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			Perror("ioctl (SIOCACPCONFIG) returns");
			exit(1);
		}
		break;

	    case 'D':
		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"acpconfig: '-d' flag invalid for specified \
interface\n");
		    exit(1);
		}

		trtab.func = 'D';
		ifr.ifr_data = (caddr_t)&trtab;
		(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			Perror("ioctl (SIOCACPCONFIG) returns");
			exit(1);
		}
		break;

	    case 'r':
		/* read an address translation table entry */
		/* accept "-r length" OR "-rlength"        */
		if (iface & ACP_INTERFACE) { 	/* error if 'acp' device */
		    fprintf(stderr,"acpconfig: '-r' flag invalid for specified \
interface\n");
		    exit(1);
		}

		ifr.ifr_data = 0;	/* clear value */
		if (strlen(*argv) > 2)
			top = atoi(&((*argv)[2]));
		else {
			argc--, argv++;
			top = atoi(*argv);
		}
		if (top==0)
		    top = 30000;  /* (a big number) */
		for (tmp=0; tmp<top; tmp++) {
		  trtab.ipaddr = (u_long) tmp;
		  trtab.func = 'r';
		  ifr.ifr_data = (caddr_t)&trtab;
		  (void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		  if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
		    if (errno == EFAULT) {
			fprintf(stdout,"acpconfig: end of address table \
(%d entries max)\n", tmp);
			break;
		    }
		    else {
			Perror("ioctl (SIOCACPCONFIG) returns");
			exit(1);
		    }
		  }
		  /*
		   * The byte ordering "works" on the inet_ntoa() call.
		   * Probably only on VAXen, though (fine for us).
		   */
		  if (trtab.x25addr[0]) {
		    printf("acpconfig: address table entry %d: ", tmp);
		    printf ("%s ==> %01.16s\n",
			inet_ntoa (trtab.ipaddr), &trtab.x25addr[1]);
		  }
		}
		break;

	    default:
		usage();    /* display proper syntax for user */
	    }
	}
     }
/*
 *	Send ioctl for gathered -o options.
 *	Presently (08-31-87) the -o option supports only
 *	one byte of options.  If the number of firmware
 *	supported options reaches 9, then a second byte
 *	will be used.  The driver must be modified if a
 *	second byte is used.  The value of PKT_OPTIONS
 *	must be changed from 0x77 to 0xB7 to notify the
 *	FE that PKT_OPTIONS is a 2-Byte Valued ID.
 */
     if (o_flag) {	/* send ioctl for gathered options */
	ddactl.func = 'o';
	ddactl.msg[0] = option_byte1;
	(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	ifr.ifr_data = (caddr_t) &ddactl;
	if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
		Perror("ioctl (SIOCACPCONFIG) returns");
		exit(1);
	}
     }
/*
 *	if simulation, simply return to main loop of test jig
 */
#ifndef	SIMULATION
     exit(0);
#endif
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        SETIFADDR()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */
/*  Purpose:                                                          */
/*                                                                    */
/*      Set the interface address.                                    */
/*                                                                    */
/*  Call:            setifaddr(addr)                                  */
/*  Arguments:       addr:   internet address                         */
/*  Returns:         nothing                                          */
/*  Called by:       main()                                           */
/*  Calls to:        getaddr()                                        */
/*                   strncpy()                                        */
/*                   ioctl()                                          */
/*                   Perror()                                         */
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

setifaddr(addr)
	char *addr;
{
	getaddr(addr, (struct sockaddr_in *)&ifr.ifr_addr);
	(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCSIFADDR, (caddr_t)&ifr) < 0) {
		Perror("ioctl (SIOCSIFADDR)");
		exit(1);
	}
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                          GETADDR()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* THIS ROUTINE IS SLOW AS MOLASSES - - FIX IT SOONER OR LATER !!!    */
/*  Purpose:                                                          */
/*                                                                    */
/*      Get the interface address and stick it in the appropriate     */
/*      data structure.  This routine has four different methods for  */
/*      deriving the address.  If successful return nothing, else     */
/*      print error message and exit.                                 */
/*                                                                    */
/*                                                                    */
/*  Call:            getaddr(s, sin)                                  */
/*  Arguments:       s:    string for address                         */
/*                   sin:    socket address struct                    */
/*  Returns:         nothing for success, else error message exit     */
/*  Called by:       setifaddr()                                      */
/*  Calls to:        gethostbyname()                                  */
/*                   bcopy()                                          */
/*                   getnetbyname()                                   */
/*                   inet_makeaddr()                                  */
/*                   inet_addr()                                      */
/*                   inet_network()                                   */
/*                   fprintf()                                        */
/*                   exit()                                           */
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

struct in_addr inet_makeaddr();

getaddr(s, sin)
	char *s;
	struct sockaddr_in *sin;
{
	struct hostent *hp,
#ifdef VAXVMS
			*_gethtbyname();
#else VAXVMS
			*gethostbyname();
#endif VAXVMS
	struct netent *np;
	int val;

	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return;
	}
#ifdef VAXVMS
	hp = _gethtbyname(s); /* Use host table, not name server */
#else VAXVMS
# ifdef	SIMULATION
	hp = 0;			/* don't find our address by host name */
# else	SIMULATION
	hp = gethostbyname(s);
# endif	SIMULATION
#endif	VAXVMS
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *)&sin->sin_addr, hp->h_length);
		return;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr((int)np->n_net, INADDR_ANY);
		return;
	}
	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return;
	}
	val = inet_network(s);
	if (val != -1) {
		sin->sin_addr = inet_makeaddr(val, INADDR_ANY);
		return;
	}
	fprintf(stderr, "acpconfig: invalid internet address '%s'\n", s);
	exit(1);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                         SETCONFIG()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */
/*  Purpose:                                                          */
/*                                                                    */
/*      Set the configuration parameters by sending socket ioctl      */
/*      to the specified interface driver.                            */
/*                                                                    */
/*                                                                    */
/*  Call:            setconfig(action, s)                             */
/*  Arguments:       action    indicates action to be taken           */
/*                   s         socket descriptor                      */
/*  Returns:         0 for success, nonzero for failure               */
/*                   NOTE:  if no internet address, doesn't return,   */
/*                           program exits                            */
/*  Called by:       main()                                           */
/*  Calls to:        fprintf()                                        */
/*                   strncpy()                                        */
/*                   sizeof()                                         */
/*                   ioctl()                                          */
/*                   Perror()                                         */
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

setconfig(action, s)

	char	  *action;
	int 	  s;
{
	extern int errno;
	int saves;

	if (action == 0)
		return(1);
	saves = s;
	(void)strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
		if (errno == EINTR) {
			fprintf(stderr,"\nacpconfig:  command in progress\n");
			NEW_DELAY(3000000);	   /* wait for device pwrup diags */
			if (ioctl(saves, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
				Perror("ioctl (SIOCACPCONFIG) returns");
				return(1);
			}
			else
				return(0);
		}
		else {
			Perror("ioctl (SIOCACPCONFIG) returns");
			return(1);
		}
	}
	return(0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                          USAGE()                               %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */
/*  Purpose:                                                          */
/*                                                                    */
/*      Display the correct syntax for the acpconfig command line.    */
/*                                                                    */
/*  Call:            usage()                                          */
/*  Returns:         nothing                                          */
/*  Called by:       main()                                           */
/*  Calls to:        fprintf()                                        */
/*                   exit()                                           */
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static char *usemsg =
"Usage: acpconfig interface [address] [-A filename] [-a ipaddr x25addr]\n\
\t[-b baud] [-c msgnum] [-d ipaddr] [-D] [-e size] [-f facility status]\n\
\t[-h mode[r]] [-l] [-m message] [-n circuits] [-o option]\n\
\t[-q type] [-r count] [-s X.25_service] [-t secs]\n\
\t[-u mode] [-v variable value] [-z]\n";

usage()
{
       fprintf(stderr, usemsg);
       fprintf(stderr, "\nacpconfig generation %s\n", VERSION);
       exit(1);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                   LONG_USAGE()                                 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */
/*  Purpose:                                                          */
/*                                                                    */
/*      Display the correct syntax for the acpconfig command line.    */
/*      This routine is called when no arguments are passed to        */
/*      acpconfig, this generally indicates that the user want to see */
/*      the usage line as opposed to making a mistake in the          */
/*      arguments                                                     */
/*                                                                    */
/*  Call:            usage()                                          */
/*  Returns:         nothing                                          */
/*  Called by:       main()                                           */
/*  Calls to:        fprintf()                                        */
/*                   exit()                                           */
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static char *longusemsg =
"Usage: acpconfig interface [address] \n\
[-a ipaddr x25addr]  : add a single IP to X.25 address translation (PDN only)\n\
[-A filename]        : read IP to X.25 addr translations from file (PDN only)\n\
[-b baud]            : set baud rate to baud. 0 designates external clocking\n\
[-c msgnum]          : enable/disable driver console message\n\
[-d ipaddr]          : delete an IP to X.25 translation (PDN only)\n\
[-D]                 : delete all IP to X.25 translations (PDN only)\n\
[-e size]            : set front-end buffer size\n\
[-f facility status] : control initiation of flow control negotiation\n\
                       status is \"on\" or \"off\"; facility is \"packet\" or\n\
                       \"window\"\n\
[-h mode[r]]         : print statistics on logical circuit usage\n\
                       mode is 0 (read) or 1 (read and reset); \"r\"\n\
                       specifies raw mode\n\
[-l[n]]              : display status of each active logical channel\n\
		       the optional \"n\" argument will cause acpconfig\n\
[-m message]         : send specified supervisor message\n\
[-n circuits]        : set number of virtual circuits\n\
[-N net]             : set network type;  values for net are one of:\n\
                       transpac - the French Transpac network\n\
                       net15    - any network using 15 digit addresses\n\
                       default  - standard network configuration\n\
[-o option]          : set or disable extended clear handling; option is\n\
                       \"extended\" or \"none\"\n\
[-q type]            : query driver or front end status; type is 0 for\n\
                       front end, 1 for driver\n\
[-r count]           : read and print address translation table; count\n\
                       is number of translations to read; 0 means all\n\
[-s X.25_service]    : specify DDN standard, DDN basic or PDN service;\n\
                       X.25_service is: \"standard\", \"basic\" or \"pdn\"\n\
[-t secs]            : set the idle circuit timeout to secs seconds\n\
[-u mode]            : set interface mode to:\n\
                       down - disable interface\n\
                       dte  - online as DTE\n\
                       dce  - online as DCE\n\
                       ext  - external loopback\n\
                       int  - internal loopback\n\
[-v variable value]  : set driver variable; variable/values are one of:\n\
                       packet DDD - set X.25 packet size to DDD decimal\n\
                       window DDD - set X.25 window size to DDD decimal\n\
[-z]                 : reset the board\n";

long_usage()
{
       fprintf(stderr, longusemsg);
       fprintf(stderr, "\nacpconfig generation %s\n", VERSION);
       exit(1);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                            PERROR()                               %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */
/*  Purpose:                                                             */
/*                                                                       */
/*	print error message, based on errno                              */
/*									 */
/*  Call:  		Perror(cmd)                                      */
/*  Arguments:      	cmd:   error message                             */
/*  Returns:  		nothing, exits if no internet address is set     */
/*  Called by:  	main()                                           */
/*  Calls to:   	fprintf()                                        */
/*                	perror()                                         */
/*                                                                       */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

Perror(cmd)
	char *cmd;
{
	extern int errno;

	fprintf(stderr, "acpconfig: ");
	switch (errno)
	{

	case ENXIO:
		fprintf(stderr, "%s: ", cmd);
		fprintf(stderr, "no such interface\n");
		break;

	case EPERM:
		fprintf(stderr, "permission denied\n");
		break;

	case EDESTADDRREQ:
		fprintf(stderr, "%s: ", cmd);
		fprintf(stderr, "no internet address assigned to interface\n");
		usage();		/* display proper syntax for user */
		exit(1);
		break;

	case EINVAL:
		fprintf(stderr, "%s: ", cmd);
		fprintf(stderr, "invalid command\n");
		break;

	case EINTR:
		fprintf(stderr, "%s: ", cmd);
		fprintf(stderr, "device not operational\n");
		break;

	case EINPROGRESS:
		fprintf(stderr, "%s: ", cmd);
		fprintf (stderr, "Can't change parameters with link up\n");
		fprintf (stderr, "Bring the link down and try again\n");
		break;

	case ENOPROTOOPT:
		fprintf (stderr, "%s: ", cmd);
		fprintf (stderr, "Operation not supported by this version of the firmware.\n");
		break;

	default:
		perror(cmd);
	}
}

/*
 * from SAGE::PROJECT:[X25.ACP5250W.HCOMMON]NCP.H on
 */
struct stat_rsp {
	u_char	cmnd, path, var, count; /* message header */	
	u_long  uptime;		/* seconds since startup */
	u_long  elapsed;	/* tenths since last statistics response */
	u_long  frames_sent;	/* frames and... */
	u_long  bytes_sent;	/* bytes transmitted */
	u_long  frames_rcvd;	/* frames and... */
	u_long  bytes_rcvd;	/* bytes received */
	u_short SABMs_sent;	/* similarly for SABMs, */
	u_short SABMs_rcvd;
	u_short DISCs_sent;	/* DISCs */
	u_short DISCs_rcvd;
	u_short FRMRs_sent;	/* FRMRs */
	u_short FRMRs_rcvd;
	u_short RRs_sent;	/* RRs */
	u_short RRs_rcvd;
	u_short REJs_sent;	/* REJs */
	u_short REJs_rcvd;
	u_short RNRs_sent;	/* RNRs */
	u_short RNRs_rcvd;
	u_short crc_errors;	/* CRC errors received */
	u_short bad_frames;	/* invalid frames received */
	u_short rexmitted;	/* I-frames RE-transmitted */
	u_short UAs_sent;	/* UAs transmitted, received */
	u_short UAs_rcvd;
	u_short DMs_sent;	/* DMs transmitted, received */
	u_short DMs_rcvd;
	u_short I_frames_sent;	/* I frames transmitted, received */
	u_short I_frames_rcvd;
};

/*
 * A long time ago in a UNIBUS far, far away, there was a Z80 based
 * front end processor happily talking to its PDP-11 host.  They
 * exchanged many cheerful messages, some of which were statistics
 * queries.  The statistics query was defined in the only logical
 * format, the format shared by both the Z80 and its PDP-11 host.
 * Well, thousands of computer generations passed, and the little
 * Z80 grew up into a big, strong 68000 based front end.  His
 * friendly little PDP-11 host grew up too, into a big honkin'
 * VAX.  They still liked to exchange cheerful messages, though,
 * and some of those were still the same well-worn statistics
 * queries, which are still in the only "logical" data format.
 * So don't ask again why a VAX and a 68000 exchange data in Z80
 * byte ordering!
 */
#define z80toVAX(l)	(((l) << 16) | (((l) >> 16) & 0xFFFF))

#ifdef	__GNU__		/* gnu compiler prefers ANSII C */
#define pr(A) printf("%-10s%10d%10d\n", #A, sp->A##_sent, sp->A##_rcvd)
#else			/* idiot compiler */
#define pr(A) printf("%-10s%10d%10d\n", "A", sp->A/**/_sent, sp->A/**/_rcvd)
#endif

/*
 * internal routines for use by query commands
 */
fmtstats (buf)
    char *buf;
{
	register struct stat_rsp *sp = (struct stat_rsp *) buf;
	struct timeval t;
	char *cp;

	sp->elapsed = z80toVAX (sp->elapsed);
	sp->uptime = z80toVAX (sp->uptime);
	sp->frames_sent = z80toVAX (sp->frames_sent);
	sp->frames_rcvd = z80toVAX (sp->frames_rcvd);
	sp->bytes_sent = z80toVAX (sp->bytes_sent);
	sp->bytes_rcvd = z80toVAX (sp->bytes_rcvd);

	(void)gettimeofday (&t, (struct timezone *)0);
	cp = ctime (&t.tv_sec);
	cp[24] = '\0'; /* get rid of the embedded newline */
	printf ("\n----------------------------------------------\n");
	printf ("Front End Processor statistics (host time: %s)\n", cp);

	t.tv_sec -= sp->uptime;
	cp = ctime (&t.tv_sec);
	cp[24] = '\0';
	printf ("FEP up %d seconds (since %s)\n", sp->uptime, cp);

	t.tv_sec += sp->uptime;
	t.tv_sec -= sp->elapsed;
	cp = ctime (&t.tv_sec);
	cp[24] = '\0';
	printf ("%d seconds since last statistics (at %s)\n", sp->elapsed, cp);
/* ---- debugging stuff
	printf ("cmnd = %x path = %x var = %x count = %x\n",
		sp->cmnd, sp->path, sp->var, sp->count);
   ---- */

	printf ("\n%20s%10s\n", "SENT", "RCVD");
	pr (frames);
	pr (bytes);
	pr (I_frames);
	pr (SABMs);
	pr (DISCs);
	pr (FRMRs);
	pr (RRs);
	pr (REJs);
	pr (RNRs);
	pr (UAs);
	pr (DMs);

	printf ("\n%d CRC errors; %d bad frames; %d frames retransmitted\n",
		sp->crc_errors, sp->bad_frames, sp->rexmitted);
	printf ("--------------- END OF STATISTICS -----------------\n\n");
}

/*
 * This is disgustingly ugly.  The whole interface between acpconfig
 * and the driver has gotten completely out of hand.  Has to do with
 * the fact that we want this program to be compatible with both
 * ULTRIX and BSD and they have different include file structures AND
 * historically many of the structures weren't even IN include files!
 *
 * buf:
 *	dda_state, dda_init, dda_flags, dda_firmrev
 */
fmtmode (buf)
    char *buf;
{
    static char *brd_modes[] = {
	"DISABLED", "COMING UP", "UP", "GOING DOWN"
    };

    printf ("\n>>> Driver status for %s (Interface is %s) <<<\n",
		name, brd_modes[*buf & 3]);
    buf++;

    printf ("\tMode 0x%x ", (u_char)*buf);
    if (*buf & 0x1)
	printf ("<DDN_STANDARD>\n");
    if (*buf & 0x2)
	printf ("<DDN_BASIC>\n");
    if (*buf & 0x4)
	printf ("<PDN>\n");
    if (*buf & 0x40)
	printf ("\t<Packet Size Facility Negotiation Initiation Enabled>\n");
    if (*buf & 0x80)
	printf ("\t<Window Size Facility Negotiation Initiation Enabled>\n");
    buf++;

    if (*buf == 0)
	printf ("\tWarning, DDAF_OK is OFF <--------------\n");
    else
	printf ("\tInterface on-line (DDAF_OK is ON)\n");
    buf++;

    printf ("\tFirmware revision %d.%d installed\n",
	(*buf >> 4) & 0xF, *buf & 0xF);
    buf++;

    printf ("*** End of driver status information ***\n\n");
}

fmtsilo (buf)
unsigned char *buf;
{
    int i, j;

    for (j = 0; j < 256; j += 16) {
	for (i = 0; i < 16; i++) 
	    printf("%02x ", buf[j+i]);
	for (i = 0; i < 16; i++)
	    putchar(isalpha(buf[j+i]) ? buf[j+i] : '.');
	printf("\n");;
    }
}

/* print out data from the histogram request */

double
tval_dbl(tv)
struct timeval *tv;
{
	return((double)tv->tv_sec+(double)tv->tv_usec / (double)1000000.0);
}

hist_data_out(times)
struct timeval times[];
{
	register int i;
	double percents[HISTSIZE];
	double totaltime;
	char *p, *p1;

	totaltime = tval_dbl(times+H_END) - tval_dbl(times+H_START);

	for(i = 0; i <= H_LINK_UP; i++)
		percents[i] = tval_dbl(times+i) / totaltime;

	p1 = p = ctime(&times[H_START].tv_sec);
	while(*p)
	{
		if(*p == '\n')
			*p = ' ';
		*p++;
	}
	printf("START: %s",p1);
	p1 = p = ctime(&times[H_END].tv_sec);
	while(*p)
	{
		if(*p == '\n')
			*p = ' ';
		*p++;
	}
	printf("\t\tEND: %s\n",p1);
	printf("total time: %6.2f seconds",totaltime);
	printf("\ttime up: %ld seconds (%3.2f%%)\n",times[H_LINK_UP].tv_sec,
		percents[H_LINK_UP]*100.0);
	printf("idle circuit timeout: %ld seconds\n",times[H_TMO].tv_sec);
	for(i = 0; i < H_LINK_UP; i++)
	{
		if(percents[i] <= 0.0)
			continue;
		printf("%3d %6.2f\n",i,percents[i]*100.0);
	}
}

/*  Revision History:	

9-AUG-1987	Steph Price	V2.3
	Add new -f option for controlling the initiation of flow control
	parameter negotiation.  The default in the driver is no flow
	control parameter negotiation initiation.  To turn on packet or window
	size negotiation initiation, issue the new acpconfig commands:
		-f packet on
		-f window on
	To turn off initiation:
		-f packet off
		-f window off
	Note that these options are valid only on a per call basis.  If a
	circuit is already open for the requested destination, the status of
	flow control parameter negotiation initiation can't be altered until
	the circuit is cleared either by timing out or by a reset.

	Add "external" argument to the -b option.
	Add new arguments to the -s option.  They are as follows:
		-s standard (equivalent to -s 0)
		-s basic (equivalent to -s 1)
		-s pdn (equivalent to -s 2)
	SERVICE_VAL was modified to accomodate the new arguments.
	Previously the driver expected arguments of s, t, and u to be
	passed for the -s 0, -s 1, and -s 2 options.  Since an argument of v
	is already in use, use of the -s0/standard option will result in the
	use of 'S' as an argument to the driver's ioctl routine.
	Similarly, 'T', 'U', and 'V' will be passed for the -s1/basic,
	-s2/pdn, and -s3/class_b_c options.

	Add the -o option to enable root to select extended clear and extended
	clear confirmation packets as the first step toward X.25 1984
	compatability.

	Add the -l option to enable root to display the status of each active
	lcn.

	Modify reset case to check for returned value before printing a message
	saying that a reset is in progress.

	Fix bug in -n case.  The interface was not given.

25-OCT-87	Stephanie Price
	Modified -l option to used /dev/kmem instead of /dev/mem.  Removed
	mask of high order bit in klseek() routine.
6-NOV-87	Brad Engstrom
	Add -D flag
11-NOV-87	Brad Engstrom
	Add -h and -H histogram flags
18-NOV-87	Brad Engstrom
	Changed -A option handling so that acpconfig does not exit upon seeing
	an address already in use error.
18-DEC-87	Brad Engstrom
	Added -t options to set the idle circuit timeout value to a certain
	number of seconds.
21-APR-88	Brad Engstrom
	Added the -v dbgunit option to the -v flag to set the debug unit
	variable in the driver.
22-APR-88	Brad Engstrom
	Added the -c command to enable or disable driver console messages.  Each
	use of the command for a particular message will toggle the message
	status.
26-APR-88	Brad Engstrom
	Modified the -n command so that a command of the form "-n 0" will
	print the number of circuits currently available.  Also the -l
	command uses this information to print the number of circuits free.
	This allows the new driver to work with old firmware easier.
10-MAY-88	Brad Engstrom
	Made all references to the length of an X.25 address refer to theo
	constants MAXADDRLEN and MINADDRLEN which are defined in if_ddavar.h.
26-JAN-89	Steve Johnson
	Added -N option to support TRANSPAC net.
	Merged in extra TWG support.
15-FEB-89	Paul Traina
	Fixed bug with -N command parsing.
	Implemented an improved form of Brad's changes for Transpac.
17-FEB-89	Paul Traina
	Installed Multinet support.
10-MAR-89	Paul Traina
	Installed simulation support.
13-MAR-89	Paul Traina
	Installed support for undocumented -q 2 command.
31-MAY-89	Paul Traina
	Updated command parsing on -u fixed documentation.
20-JUN-89	Paul Traina
	Obsoleted -v debug and -v dbg_unit commands.  New driver is *NOT*
	compatible with the old crufty debugging format.
26-JUN-89	Paul Traina
	Removed the tolower()'s on -u and -N arguments.
	Reinstalled Charles' changes to 3.1 acpconfig for -ln
	Fixed things so -h w/o an argument doesn't core.
19-Jul-89	Paul Traina
	Changed version date id due to incompatible changes in the dc
	structure located in if_ddavar.h
13-Nov-89	Paul Traina
	Changed query ioctl interface.  Incompatible with old drivers.

End of Revision History
*/
