
/* 	acpconfig.c		V2.2 		30 June 1987             */

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*       ________________________________________________________        */
/*      /                                                        \       */
/*     |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*     |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |       AAAA AAAA      CCCC              CCCC              |      */
/*     |      AAAA   AAAA     CCCC              CCCC              |      */
/*     |     AAAA     AAAA    CCCC              CCCC              |      */
/*     |    AAAA       AAAA   CCCC              CCCC              |      */
/*     |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*      \________________________________________________________/       */
/*                                                                       */
/*  	Copyright (c) 1986 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		acpconfig.c                                      */
/*                                                                       */
/*  Author:		Clare E. Russ                                    */
/*                                                                       */
/*  Project:		Installation verification program for ACC        */
/*			ACP 5100/6100 and ACP 5250/6250 network          */
/* 			interface drivers.   Acpconfig provides a user   */
/*			interface which supports the configuration of a  */
/*			network  interface.                              */
/*                                                                       */
/*  Function:                                                            */
/*        Based on socket ioctls, provide user interface to Network      */
/*        Interface Drivers for the following ACC ACP devices:           */
/*                                                                       */
/*                  ACP 5100/6100  'acp' interface                       */
/*                  ACP 5250/6250  'dda' interface                       */
/*                  ACP 625        'ddn' interface                       */
/*                                                                       */
/*        The ioctl indicates what type of message is to be sent to the  */
/*        front end device (ie, bring up the HDLC line in external       */
/*        loopback mode).                                                */
/*                                                                       */
/*  Components:		acpconfig.c, ioctl.h                             */
/*                                                                       */
/*  Revision History:                                                    */
/*                                                                       */
/* 16-SEP-1985  Clare Russ:     first generated, cut and paste from      */
/*                              ifconfig.c                               */
/* 20-OCT-1985  Clare Russ:     modify for compatibility with TWG        */
/*                              software                                 */
/* 28-APR-1986  Clare Russ:     BETA RELEASE V1.2 (version number tracks */
/*                              ACC Software Library entry)              */
/*                              next version:  condense code in main(),  */
/*                              create acpconfig.h for includes and defs */
/* 14-MAY-1986  Clare Russ:     BETA RELEASE V1.3 (version number tracks */
/*                              ACC Software Library entry)              */
/*                              fix error in baud rate logic             */
/* 30-MAY-1986  Randy Graves    BETA RELEASE V1.4 (version number tracks */
/*                              ACC Software Library entry) make         */
/*                              changes to support Public Data Networks  */
/* 11-JUL-1986  Clare Russ:     BETA RELEASE V0.5 (version number tracks */
/*                              ACC Software Library entry) make         */
/*                              changes to support Public Data Networks  */
/*                              in_addr differences reconciled           */
/* 06-AUG-1986  Clare Russ:     V2.0 (version number tracks ACC Software */
/*                              Library entry) modify for compatibility  */
/*                              with ULTRIX-32(m) V1.2                   */
/* 10-SEP-1986  Clare Russ:     V2.1 (version number tracks ACC Software */
/*                              Library entry) add ability to reset the  */
/*                              front end.                               */
/* 18-FEB-1987  Jeff Berkowitz  V2.2 Add ability to pass baud rate	 */
/*				divisor to FEP.  New ioctl to query	 */
/*				service mode.  Add interface to		 */
/*				statistics query message.  Add general	 */
/*				configuration interface from user space. */
/*				Fix white space syntax on -b to allow	 */
/*				-b 9600.  Use acpconfig for debug/log	 */
/*				message control.  Add ioctl to configure */
/*				the number of LCNs.			 */
/*                                                                       */
/*  Usage Notes:                                                         */
/*                                                                       */
/*      acpconfig interface [address] [-A filename] [-a ipaddr x25addr]  */
/*		  [-b baud] [-d ipaddr] [-m message] 			 */
/*		  [-n #_lcns] [-q type] [-r count] [-s X.25_service]	 */
/*		  [-u mode] [-v variable value] [-z]			 */
/*                                                                       */
/*         -A filename                                                   */
/*              Add the contents of the named file to the address        */
/*              translation table for PDN service                        */
/*                                                                       */
/*         -a ipaddr x25addr                                             */
/*              Add the specified address pair to the address            */
/*              translation table for PDN service                        */
/*                                                                       */
/*         -b baud                                                       */
/*              Note that the baud rate values are different for the     */
/*              ACP 625.  Table 1 contains the baud rates which apply    */
/*              to the ACP 6100, ACP 5250/6250 products.  Table 2        */
/*              contains the baud rates which apply to the ACP 625.      */
/*                                                                       */
/*  	  Table 1:  Nominal baud rates for ACP 5100/6100, ACP 5250/6250  */
/*  			                                                 */
/*  			2.00M  2000K  2.0M  1.33M  1333K  1.3M           */
/*  			1.00M  1000K  1.0M  500K   250K   100K           */
/*  			64K    64000  56K   56000  30K    19.2K          */
/*  			9.6K   9600   4.8K  4800   2.4K   2400           */
/*  			1.2K   1200                                      */
/*  			                                                 */
/*  	  Table 2:  Nominal baud rates for ACP 625		         */
/*  			                                                 */
/*  			316000  153600   115200   76800   76.8K          */
/*			 57600   57.6K    38400   38.4K   28800          */ 
/*			 28.8K   19200    19.2K    9600    9.6K          */ 
/*			  4800    4.8K     2400    2.4K    2150          */ 
/*			  1760    1200                                   */
/*  			                                                 */
/*  	        The M for megabits/second and K for kilobits/second      */
/*  	        are optional.  Note that Table 1 allows for the entry    */
/*  	        of 9.6 (with the assumed unit of Kilobits/second), but   */
/*              also allows for the entry of 9600 (bits/second).  Thus   */
/*              there is more than one entry for some of the baud rate   */
/*		values.                                                  */
/*                                                                       */
/*         -d ipaddr                                                     */
/*              Delete the specified address entry from the address      */
/*              translation table for PDN service                        */
/*                                                                       */
/*	   -m message							 */
/*		Pass ``message'' to the FEP as a supervisory channel	 */
/*		message.  Message is a sequence of numbers separated	 */
/*		by white space.  Numbers with leading ``0'' are taken	 */
/*		as octal, other numbers taken as HEX, decimal is not	 */
/*		supported.  Hex and octal may be intermixed, as in	 */
/*		``0140 0 0 2 89 017''.  The message is terminated by the */
/*		end of the argument list or by an argument beginning	 */
/*		with a dash ``-''.  Absolutely no checking is performed; */
/*		the bytes are written to the FEP as a supervisor	 */
/*		message.  The message is limited to MLEN = 112 bytes.    */
/*									 */
/*	   -n #_lcns							 */
/*		Set the number of logical channels to ``#_lcns'', which  */
/*		must be less than some implementation defined maximum.	 */
/*									 */
/*	   -q query-type						 */
/*		Query the board or driver and place results on stdout.	 */
/*									 */
/*	   -q 0	 Statistics query to FEP				 */
/*	   -q 1	 Driver operating mode query				 */
/*                                                                       */
/*         -r count                                                      */
/*              Read the specified number of entries from the address    */
/*              translation table for PDN service                        */
/*                                                                       */
/*         -r 0  read all entries from the address translation table     */
/*                                                                       */
/*         -s X.25 service                                               */
/*              Select DDN standard X.25 service or basic X.25 service   */
/*              or Public Data Network X.25 service                      */
/*                                                                       */
/*         -s 0  select standard X.25 service                            */
/*         -s 1  select basic X.25 service                               */
/*         -s 2  select X.25 Public Data Network service                 */
/*                                                                       */
/*         -u mode                                                       */
/*              Bring the link up or down.                               */
/*                                                                       */
/*         -u 0  bring down the link                                     */
/*         -u 1  bring up link for normal operation, no loopback, DTE    */
/*         -u 2  bring up link for normal operation, no loopback, DCE    */
/*         -u 3  bring up the link in external loopback mode             */
/*         -u 4  bring up the link in internal loopback mode             */
/*                                                                       */
/*	   -v variable value						 */
/*		Set the value of a driver variable symbolized by	 */
/*		"variable" to "value" decimal.  "Variables" understood   */
/* 		are "log", "debug", "window" and "packet" to set the	 */
/* 		driver's notion of logging, debug flags, negotiable	 */
/*		window size, and negotiable packet size.		 */
/*									 */
/*         -z  reset the specifed front-end device                       */
/*                                                                       */
/*  Compile Notes:                                                       */
/*                                                                       */
/*      The associated makefile builds the acpconfig executable image    */
/*      for UNIX 4.2BSD or the specified emulation/simulation such as    */
/*      The Wollongong Group (TWG) software which runs under VAX/VMS.    */
/*      The makefile must be invoked with the argument which specifies   */
/*      the target host operating system, otherwise the makefile         */
/*      defaults to creating an executable image for UNIX 4.2BSD.        */
/*                                                                       */
/*                make                 (compile for UNIX 4.2BSD)         */
/*                make sysbsd          (compile for UNIX 4.2BSD)         */
/*                make systwg          (compile for TWG under VAX/VMS)   */
/*                                                                       */
/*                make clean                                             */
/*                make print                                             */
/*                                                                       */
/*                                                                       */
/*  System Notes:                                                        */
/*                                                                       */
/*      Create a socket of the AF_INET address family and of type        */
/*      datagram, SOCK_DGRAM.  Use the socket to send a socket ioctl     */
/*      to the network driver of the specified interface ('acp0' in      */
/*      the usage notes).   Depending on the type of message specified   */
/*      in the command line, send the appropriate socket ioctl to        */
/*      specify what kind of action is to be taken.                      */
/*                                                                       */
/*      For those drivers which support the Control Interface (CIF)      */
/*      the message exchange conforms to the CIF which defines paired    */
/*      command/response Control Interface Messages (CIMs) between the   */
/*      host and the front end.   Otherwise, the exchange of messages    */
/*      is the pre-CIF protocol (i.e., that used  by the ddn interface). */
/*                                                                       */
/*	Assignment of subcodes for the SIOCACPCONFIG ioctl:		 */
/*                                                                       */
/*	1..14		-bRATE for RATE != 0				 */
/*	`0'..`4'	-u N (line control)				 */
/*	`a'		-A and -a commands (PDN)			 */
/*	`b'		-b0 (set external clock)			 */
/*	`d'		-d command (PDN)				 */
/*	`m'		-m (message command)				 */
/*	`n'		-n #_lcns (limit lcns)				 */
/*	`p'		-q 0 (statistics query)				 */
/*	`q'		-q 1 (driver query)				 */
/*	`r'		-r command (PDN)				 */
/*	`s'		-s 0						 */
/*	`t'		-s 1						 */
/*	`u'		-s 2						 */
/*	`v'		-v command (set variable)		 	 */
/*	`z'		-z command (reset)				 */
/*									 */
/*************************************************************************/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                                %%*/
/*%%               INCLUDE FILES                                    %%*/
/*%%                                                                %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*   There are alternate path names for The Wollongong Group (TWG)    */
/*   IPTCP and Eunice software which runs under VMS                   */

#include <sys/types.h>
#include <sys/socket.h>

#ifdef TWG
#include </sys/twgtcp/kernel/h/ioctl.h>
#include </sys/twgtcp/kernel/netinet/in.h>
#include </sys/twgtcp/kernel/net/if.h>
#else
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#endif TWG

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

/* 'iface' interface definitions */
#define	ACP_INTERFACE	0x01		/* for acp interface */
#define	DDN_INTERFACE	0x02		/* for ddn interface */

#define	BAUD_VAL	50		/* for baud rate flag, -b */
#define	SERVICE_VAL	67		/* for X.25 service flag, -s */

/* Change these for PDN X.25 Addresses of other formats.  E.g., for no */
/* restriction change PDNX25AMIN to 1 and PDNBADDNIC to "Z" */

#define PDNX25AMIN 12		/* Minimum X.25 address length */
#define PDNBADDNIC "0000"	/* X.25 addr prefix to EXCLUDE */

/* delay macro from /sys/vax/param.h */

#define	DELAY(n)	{ register int N = (n); while (--N > 0); }


/* The baud_rate structure maps the user-supplied baud rate into the */
/* parameter used in the socket ioctl to the driver.  In some cases, */
/* more than one representation of a value is present to ease the    */
/* interface.  The manual page for acpconfig recommends that the     */
/* user give the baud rate with the assumed unit of Kilobits/second. */

struct 	baud  {
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

 
/* Table of baud rate values and the associated parameter for the Set   */
/* System Parameters message, ddninit_msg.  The 'parameter1' is nonzero */
/* for valid baud rate divisors.  The user's manual gives both the      */
/* actual and nominal baud rates, either one is accepted from the user, */
/* but the nominal baud rate is the figure which is closest to the rate */
/* set on the front end.                                                */

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

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                         MAIN()                                 %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */ 
/*  Purpose:                                                          */ 
/*                                                                    */
/*      For the specified interface, create a socket for a socket     */
/*      ioctl to set the Internet address and configuration.  The     */
/*      ioctl kicks the appropriate driver, the value in ifr_data     */
/*      indicates the type of action to be taken.  ('b' indicates     */
/*      external clock, 1-14 indicates baud rate from the table of    */
/*      possible values, '0' - '4' indicates -u options, and 's','t', */
/*      'u' indicate -s options.)                                     */
/*      Enhancement: 'a' indicates addition of an address table       */
/*      entry, 'd' indicates deletion of an address table entry,      */
/*      and 'r' is a request to read address table entries.           */
/*	Etc, etc (see comment at top for complete list)		      */
/*                                                                    */
/*  Call:               main( argc, argv)                             */ 
/*  Arguments:          argc:   argument count                        */
/*                      argv:   argument value                        */ 
/*  Returns:            nothing                                       */ 
/*  Called by:          invoked by privileged user                    */
/*  Calls to:           socket()                                      */ 
/*                      perror()                                      */ 
/*                      strcpy()                                      */ 
/*                      strncpy()                                     */ 
/*                      sizeof()                                      */ 
/*                      usage()                                       */ 
/*                      ioctl()                                       */ 
/*                      exit()                                        */ 
/*                      Perror()                                      */ 
/*                                                                    */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

struct	ifreq ifr;
struct	sockaddr_in sin = { AF_INET };
char	name[30];
int	s;			/* socket descriptor */
struct ddactl {			/* used by various ioctls */
	u_char func;		/* must be first */
	u_char nothing[3];	/* alignment */
	int drval;		/* used by -v, etc */
	char msg[112];		/* used by -m, -q: XXX 112 should be "MLEN" */
} ddactl;
struct trtab {			/* Address Translation Table Entry */
	u_char func;
	u_char x25addr[15];
	u_long ipaddr;
} trtab;
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

extern int errno;

main(argc, argv)
	int argc;
	char *argv[];
{
    register struct baud *p;            /* baud rates */
    int	iface = 0;			/* indicate interface, i.e. acp */
    int tmp, top;
    FILE *fp;
    char line[80], arg1[40], arg2[40];

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
    strcpy(name, *argv);
    strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
    if ((*argv)[2] == 'n')	/* check for 'ddn' interface */
        iface |= DDN_INTERFACE;
    if ((*argv)[0] == 'a')	/* check for 'acp' interface */
        iface |= ACP_INTERFACE;

    if (ioctl(s, SIOCGIFFLAGS, (caddr_t)&ifr) < 0) {
	Perror("ioctl (SIOCGIFFLAGS)");
	exit(1);
    }
    while (--argc > 0) {
  	argv++;
	if(**argv != '-')
	   setifaddr(*argv, 0);		/* set interface address */
	else {
	   ifr.ifr_data = 0;
  	   switch((*argv)[1]) {		/* process flag(s) */
           case 'b':			/* set baud rate or ext clocking */
		{			/* accept -bN or -b N for any N */
		char *ap = *argv;
		if (ap[2] == '\0') {	/* -b N case */
			argc--, argv++, ap = *argv;
			if (ap == 0)
				ap = "???";
		} else {
		    ap += 2;		/* -bN case, skip over the "-b" */
		}
		/* ap points to purported number, regardless -bN or -b N */
		if (*ap == '0') {	/* set external clocking */
			if (iface&DDN_INTERFACE) {
				fprintf (stderr,
					"\nacpconfig %s: -b 0 invalid\n",
						name);
				break;
			}
			*ap = 'b';	/* cookie for driver ioctl routine */
			ifr.ifr_data = ap;
		} else {		/* set baud rate, if N is valid */
			if (iface&DDN_INTERFACE)
				p = ddnbaud_rate;
			else
				p = baud_rate;
			while (p->b_rate) {
				if (strcmp (ap, p->b_rate) == 0)
					break;
				p++;
			}
			if (p->parameter)
				ifr.ifr_data = &(p->parameter);
		}
	        if (setconfig(ifr.ifr_data, s)) {
        	    fprintf(stderr,"\nacpconfig: -b %s invalid\n", ap);
		    usage();
	        }
       	        break;
		}

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
		if (((*argv)[2] >='0' && (*argv)[2]<= '2') && (*argv)[3] == 0) {
			(*argv)[2] += SERVICE_VAL;     /* match 's','t','u' */
			ifr.ifr_data = &((*argv)[2]);
		}
		else {
		  	argc--, argv++;
			if(((*argv)[0]>= '0'&&(*argv)[0]<='2')&&(*argv)[1]==0) {
				(*argv)[0] += SERVICE_VAL;
				ifr.ifr_data = &((*argv)[0]);
			}
		}
		if ((ifr.ifr_data == 0) || setconfig(ifr.ifr_data, s)) {
		    if (errno == EALREADY) {
        	      	fprintf(stderr,"acpconfig: must shut down interface to\
 change modes between DDN and PDN\n");
			exit(1);
		    }
		    else {
        	      	fprintf(stderr,"\n acpconfig:  invalid X.25 service\n");
	 		usage();    /* display proper syntax for user */
		    }
		}
		break;
	
            case 'u':
		/* bring up the line, specify loopback  */
		/* or DTE/DCE, accept -un or -u n       */

		if(((*argv)[2] >= '0' && (*argv)[2] <= '4') && (*argv)[3] == 0)
			ifr.ifr_data = &((*argv)[2]);
		else {
		  	argc--, argv++;
			if(((*argv)[0] >= '0'&& (*argv)[0]<='4')&&(*argv)[1]==0)
				ifr.ifr_data = &((*argv)[0]);
		}
		if (argc > 1) {	/* any more parameters? */
		    fprintf(stderr, " acpconfig: -u flag must be last\n");
		    exit(1);
		}
		if (ifr.ifr_data == 0) {
        	      	fprintf(stderr,"\n acpconfig:  invalid mode");
        		fprintf(stderr," '%s'\n", *argv);
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
			    fprintf ("\nacpconfig: -m message too long\n");
			    exit(1);
			}
#ifdef notdef
printf ("argument %s argc %d\n", *argv, argc);
#endif
			sscanf (*argv, (**argv == '0') ? ofmt : xfmt, &i);
			*mp++ = i;
		    } /* while */
		    ddactl.func = 'm';
		    strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
#ifdef notdef
printf ("before ioctl msg %x %x %x %x %x %x %x %x\n", 
    ddactl.msg[0], ddactl.msg[1], ddactl.msg[2], ddactl.msg[3], 
    ddactl.msg[4], ddactl.msg[5], ddactl.msg[6], ddactl.msg[7] );
sleep(1);
#endif
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;

	    case 'n':		/* set SVC limit */
	        {   char *ap = *argv;

		    if (ap[2] == '\0')
		    {   argc--;		/* value is next arg; fix pointers */
			argv++;
			ap = *argv;
		    }
		    else
			ap += 2;	/* value at 3rd char of this arg */
		    ddactl.func = 'n';
		    ddactl.drval = atoi(ap);
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;
	    case 'q':		/* query board or driver for status */
		{
		    char *cp = *argv;

		    if (strlen (cp) > 2)
			cp += 2;
		    else {
			argc--; argv++; cp = *argv;
		    }
		    doquery (atoi (cp));
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
		    static char *nametab[] = {
			"log", "debug", "packet", "window", 0
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
		    if (i < 2)	/* log, debug: masks: read in hexadecimal */
			sscanf (vp, "%x", &val);
		    else	/* window, packet: counts: read in decimal */
			sscanf (vp, "%d", &val);
		    if (i == 2) {	/* packet size: take base 2 log */
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
		    strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
		    ifr.ifr_data = (caddr_t) &ddactl;
		    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
			    Perror("ioctl (SIOCACPCONFIG) returns");
			    exit(1);
		    }
		}
		break;

            case 'z':
		/* reset the specified front-end device*/

		ifr.ifr_data = &((*argv)[1]);
	        setconfig(ifr.ifr_data, s);
        	fprintf(stderr,"\nacpconfig:  reset in progress, wait 1 minute\n");
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
		while (fgets(line, 124, fp) != NULL)
		{	if (line[0] == '#' || line[0] == '\n')
				continue;
			sscanf (line, "%s %s", arg1, arg2);
/*
        		fprintf(stdout,
				"acpconfig: processing translation %s ==> %s\n",
				arg1, arg2);
*/
 			if ((top=strlen(arg2)) > 14 || top < PDNX25AMIN)
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
			if (strncmp(arg2, PDNBADDNIC, 4) == 0)
        		{	fprintf(stderr,
				  "acpconfig: incomplete X.25 address '%s'\n",
				  arg2);
				exit(1);
			}
			getaddr(arg1,&sin);
			trtab.func = 'a';
			trtab.ipaddr = sin.sin_addr.s_addr;
			trtab.x25addr[0]=strlen(arg2);
			strncpy(&trtab.x25addr[1], arg2, 14);
			ifr.ifr_data = (caddr_t)&trtab;
			strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
			if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
	 	     		Perror("ioctl (SIOCACPCONFIG) returns");
	 	     		exit(1);
	 		}
		}
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
		if ((top=strlen(*argv)) > 14 || top < PDNX25AMIN)
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
		if (strncmp(*argv, PDNBADDNIC, 4) == 0)
        	{	fprintf(stderr,
			  "acpconfig: incomplete X.25 address '%s'\n", *argv);
			exit(1);
		}
		trtab.x25addr[0]=strlen(*argv);
		strncpy(&trtab.x25addr[1], *argv, 14);
		ifr.ifr_data = (caddr_t)&trtab;
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
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
		strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
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
		  strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
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
		    printf ("%s ==> %01.14s\n",
			inet_ntoa (trtab.ipaddr), &trtab.x25addr[1]);
		  }
		}
		break;

            default:
	 	usage();    /* display proper syntax for user */
            }
        }
     }
     exit(0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                        SETIFADDR()                             %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                    */ 
/*  Purpose:                                                          */ 
/*                                                                    */
/*      Set the interface address.                                    */
/*                                                                    */
/*  Call:            setifaddr(addr, param)                           */ 
/*  Arguments:       addr:   internet address                         */
/*                   param:  argument value                           */ 
/*  Returns:         nothing                                          */ 
/*  Called by:       main()                                           */
/*  Calls to:        getaddr()                                        */ 
/*                   strncpy()                                        */ 
/*                   ioctl()                                          */ 
/*                   Perror()                                         */ 
/*                                                                    */ 
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

setifaddr(addr, param)
	char *addr;
	int param;
{
	getaddr(addr, (struct sockaddr_in *)&ifr.ifr_addr);
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
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
	struct hostent *hp;
	struct netent *np;
	int val;

	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *)&sin->sin_addr, hp->h_length);
		return;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
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
	strncpy(ifr.ifr_name, name, sizeof (ifr.ifr_name));
	if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
		if (errno == EINTR) {
       			fprintf(stderr,"\nacpconfig:  command in progress\n");
			DELAY(3000000);	   /* wait for device pwrup diags */
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
\t[-b baud] [-d ipaddr] [-m message]\n\
\t[-n #_lcns] [-q type] [-r count] [-s X.25_service]\n\
\t[-u mode] [-v variable value] [-z]\n\
\n\
This version supports firmware revisions 1.0 and 2.0\n";

usage()
{
       fprintf(stderr, usemsg);
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
		fprintf (stderr, "Changing packet size, window size, or\n");
		fprintf (stderr, "SVC limit requires firmware\n");
		fprintf (stderr, "revision 2.0 or greater\n");
		break;

	default:
		perror(cmd);
	}
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%                            DOQUERY()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                       */ 
/*  Purpose:                                                             */ 
/*                                                                       */
/*	Query either FEP or driver for status, depending on argument	 */
/*									 */
/*  Call:  		doquery(n)					 */
/*  Arguments:      	n: 0 for statistics query, 1 for driver mode	 */
/*  Returns:  		nothing, exits if error return from ioctl call	 */
/*			Prints status display on stdout			 */
/*  Called by:  	main()                                           */
/*  Calls to:   	fprintf()                                        */ 
/*                	perror()                                         */ 
/*                                                                       */ 
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

doquery (mode)
    int mode;
{
    ddactl.func = (mode == 0) ? 'p' : 'q';
    ifr.ifr_data = (caddr_t) &ddactl;
    strncpy (ifr.ifr_name, name, sizeof(ifr.ifr_name));
    if (ioctl(s, SIOCACPCONFIG, (caddr_t)&ifr) < 0) {
	    Perror("ioctl (SIOCACPCONFIG) returns");
	    exit(1);
    }
    if (mode == 0)
	fmtstats(ddactl.msg);
    else
	fmtmode(ddactl.msg);
}

/*
 * from PROJECT:[X25.ACP5250W.HCOMMON]NCP.H on
 * VAX/Very Multiloquious System, node SAGE.
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
#include <sys/time.h>
#define z80toVAX(l)	(((l) << 16) | (((l) >> 16) & 0xFFFF))
#define pr(A) printf("%-10s%10d%10d\n", "A", sp->A/**/_sent, sp->A/**/_rcvd)

/*
 * internal routines for use by doquery()
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

	gettimeofday (&t, 0);
	cp = ctime (&t);
	cp[24] = '\0'; /* get rid of the embedded newline */
	printf ("\n----------------------------------------------\n");
	printf ("Front End Processor statistics (host time: %s)\n", cp);

	t.tv_sec -= sp->uptime;
	cp = ctime (&t);
	cp[24] = '\0';
	printf ("FEP up %d seconds (since %s)\n", sp->uptime, cp);

	t.tv_sec += sp->uptime;
	t.tv_sec -= sp->elapsed;
	cp = ctime (&t);
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

    printf ("\tMode 0x%x ", *buf);
    if (*buf & 0x1)
	printf ("<DDN_STANDARD>");
    if (*buf & 0x2)
	printf ("<DDN_BASIC>");
    if (*buf & 0x4)
	printf ("<PDN>");
    printf ("\n");
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

