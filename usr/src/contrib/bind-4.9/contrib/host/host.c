/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * Originally, this program came from Rutgers University, however it
 * is based on nslookup and other pieces of named tools, so it needs
 * that copyright notice.
 */

/*
 * Extensively modified by E. Wassenaar, Nikhef-H, <e07@nikhef.nl>
 *
 * The officially maintained source of this program is available
 * via anonymous ftp from machine 'ftp.nikhef.nl' [192.16.199.1]
 * in the directory '/pub/network' as 'host.tar.Z'
 *
 * Also available in this directory are patched versions of the
 * BIND 4.8.3 nameserver and resolver library which you may need
 * to fully exploit the features of this program, although they
 * are not mandatory. See the file 'README_FIRST' for details.
 *
 * You are kindly requested to report bugs and make suggestions
 * for improvements to the author at the given email address,
 * and to not re-distribute your own modifications to others.
 */

/*
 *			New features
 *
 * - Major overhaul of the whole code.
 * - Very rigid error checking, with more verbose error messages.
 * - Zone listing section completely rewritten.
 * - It is now possible to do recursive listings into subdomains.
 * - Maintain resource record statistics during zone listings.
 * - Maintain count of hosts during zone listings.
 * - Exploit multiple server addresses if available.
 * - Option to exploit only primary server for zone transfers.
 * - Option to exclude info from names that do not reside in a domain.
 * - Implement timeout handling during connect and read.
 * - Write resource record output to optional logfile.
 * - Special MB tracing by recursively expanding MR and MG records.
 * - Special mode to check SOA records at each nameserver for domain.
 * - Special mode to check inverse mappings of host addresses.
 * - Code is extensively documented.
 */

/*
 *			Publication history
 *
 * Revision:	910129
 *			Maintain count of hosts during domain listings.
 *			Check for hosts with same name as subdomain.
 *			Add -H option for special host count mode.
 *			Recognize obsolete T_MAILA.
 * Revision:	910415
 *			Improve finding of subdomain names.
 *			Allow subdomains not directly within domain.
 *			Check for unauthoritative glue records.
 *			Add -T option to print ttl when non-verbose.
 *			Improve connect timeout handling.
 *			Improve dotted quad parsing.
 *			Minimum ttl is now called default ttl.
 * Revision:	910905
 *			Improve counting of hosts within domain.
 *			Allow hosts not directly within domain.
 *			Increase (static) maximum number of hosts.
 * Revision:	910923
 *			Count gateway hosts (with multiple addresses).
 *			Add -G option to list gateway hosts.
 * Revision:	911010
 *			Don't recurse on cnames if querytype is cname.
 * Revision:	911201
 *			Option -T also prints MX preference value.
 *			Save name of longest hostname found (just for fun).
 *			Undocumented option -g to select long names (fun).
 * Revision:	920315
 *			Improve counting of hosts within domain.
 *			Discard glue records not directly within domain.
 *			Keep track of hosts with duplicate address.
 *			Add -D option to list duplicate hosts.
 *			Add -E option to list extrazone hosts.
 *			Miscellaneous casting and typing cleanup.
 *			Increase (static) number of possible subdomains.
 * Revision:	920616
 *			Allocate list of zonenames dynamically, not statically.
 *			Move and slightly modify the test for fake hosts.
 *			Suppress host count statistics during inverse listing.
 *			Miscellaneous documentation updates.
 * Revision:	920624
 *			Lookup server name before changing nameserver address.
 *			Handle possible truncation in zone transfers.
 *			Provide private simplified version of res_send().
 *			Add -u option to force virtual circuit connections.
 *			Move all socket I/O routines to separate send.c.
 * Revision:	920702
 *			Recognize alternative program call names.
 *			Distinguish between auth and non-auth NO_DATA.
 * Revision:	921005
 *			Anticipate ultrix specific resolv.h
 *			Miscellaneous declaration changes.
 *			Some reshuffling of code.
 * Revision:	930209
 *			Lookup server name with default resolver values.
 *			Check SOA records without nameserver recursion.
 *			Implement new RR types from RFC 1183 and 1348.
 */

#ifndef lint
static char Version[] = "@(#)host.c	e07@nikhef.nl (Eric Wassenaar) 930209";
#endif

/*
 *			Compilation options
 *
 * This program usually compiles without special compilation options,
 * but for some platforms you have to define the following settings:
 *
 * #if defined(_AIX)
 *	DEFS = -D_BSD -D_BSD_INCLUDES -U__STR__ -DBIT_ZERO_ON_LEFT
 *
 * #if defined(hpux)
 *	DEFS = -DSYSV_SETVBUF
 *
 * #if defined(ultrix)
 *	DEFS = -DULTRIX_RESOLV
 *	Only if you are using the default ultrix <resolv.h>
 */

/*
 *			Miscellaneous notes
 *
 * This program should be linked explicitly with the BIND resolver library
 * in case the default gethostbyname() or gethostbyaddr() routines use a
 * non-standard strategy for retrieving information. These functions in the
 * resolver library call on the nameserver, and fall back on the hosts file
 * only if no nameserver is running (ECONNREFUSED).
 *
 * You may also want to link this program with the BIND resolver library if
 * your default library has not been compiled with DEBUG printout enabled.
 *
 * The version of the resolver should be BIND 4.8.2 or later. The crucial
 * include files are <netdb.h>, (resolv.h>, <arpa/nameser.h>. These files
 * are assumed to be present in the /usr/include directory.
 *
 * The resolver code depends on the definition of the BSD pre-processor
 * variable. This variable is usually defined in the file <sys/param.h>.
 *
 * The definition of this variable determines the method how to handle
 * datagram connections. This may not work properly on all platforms
 * (e.g. sun). A fix for this is available (see above).
 *
 * The hostent struct defined in <netdb.h> is assumed to handle multiple
 * addresses in h_addr_list[]. Usually this is true if BSD >= 43.
 *
 * Your version of the nameserver may not handle queries about top-level
 * domains properly. It needs a patch if it appends the default domain
 * to single names for which it has no data cached. A fix for this is
 * available (see above).
 *
 * For smooth porting to both BSD and SYSV environments:
 * - Do not use the function value returned by sprintf().
 *   It is of different type in the two environments.
 * - Use <string.h> instead of <strings.h>.
 *
 * The treatment of TXT records has changed from 4.8.2 to 4.8.3. Formerly,
 * the data consisted simply of the text string. Now, the text string is
 * preceded by the character count with a maximum of 255, and multiple
 * strings are embedded if the total character count exceeds 255.
 * We handle only the new situation in this program, assuming that nobody
 * uses TXT records before 4.8.3.
 *
 * Note that in 4.8.3 PACKETSZ from nameser.h is still at 512, which is
 * the maximum possible packet size for datagrams, whereas MAXDATA from
 * db.h has increased from 256 to 2048.
 * The nameserver reads queries in a buffer of size BUFSIZ.
 *
 * The gethostbyname() routine in 4.8.3 interprets dotted quads (if not
 * terminated with a dot) and simulates a gethostbyaddr(), but we will
 * not rely on it, and handle dotted quads ourselves.
 *
 * On some systems a bug in the _doprnt() routine exists which prevents
 * printf("%.*s", n, string) to be printed correctly if n == 0.
 *
 * This program has not been optimized for speed. Especially the memory
 * management is simple and straightforward.
 */

/*
 *			Terminology used
 *
 * Gateway hosts.
 * These are hosts that have more that one address registered under
 * the same name. Obviously we cannot recognize a gateway host if it
 * has different names associated with its different addresses.
 *
 * Duplicate hosts.
 * These are non-gateway hosts of which the address was found earlier
 * but with a different name, possibly in a totally different domain.
 * Such hosts should not be counted again in the overall host count.
 * This situation notably occurs in e.g. the "ac.uk" domain which has
 * many names registered in both the long and the abbreviated form,
 * such as 'host.department.university.ac.uk' and 'host.dept.un.ac.uk'.
 * This is probably not an error per se. It is an error if some domain
 * has registered a foreign address under a name within its own domain.
 * To recognize duplicate hosts when traversing many zones, we have to
 * maintain a global list of host addresses. To simplify things, only
 * single address hosts are handled as such.
 *
 * Extrazone hosts.
 * These are hosts which belong to a domain but which are not residing
 * directly within the domain under consideration and which are not
 * glue records for a subdomain of the given domain. E.g. if we are
 * processing the domain 'bar' and find 'host.foo.bar' but 'foo.bar'
 * is not a registered subdomain of 'bar' then it is considered to be
 * an extrazone host. This is not necessarily an error, but it could be.
 */

/*
 *		Usage: host [options] name [server]
 *
 * Regular command line options:
 * ----------------------------
 *
 * -t type	specify query type; default is T_A for normal mode
 *
 * -a		specify query type T_ANY
 *
 * -v		print verbose messages (-vv is very verbose)
 *
 * -d		print debugging output (-dd prints even more)
 *
 * Special mode options.
 * --------------------
 *
 * -l		special mode to generate zone listing for domain
 *
 * -L level	do recursive domain listing/checking this levels deep
 *
 * -p		use primary nameserver of domain for zone transfers
 *
 * -S		print zone resource record statistics
 *
 * -H		special mode to count hosts residing in domain
 *
 * -G		same as -H but lists gateway hosts in addition
 *
 * -E		same as -H but lists extrazone hosts in addition
 *
 * -D		same as -H but lists duplicate hosts in addition
 *
 * -C		special mode to check SOA records for domain
 *
 * -A		special mode to check reverse mappings of host addresses
 *
 * Miscellaneous options.
 * ---------------------
 *
 * -T		print ttl value during non-verbose output
 *
 * -e		exclude info from names that do not reside in domain
 *
 * -f file	log resource record output also in given file
 *
 * -i		generate inverse in-addr.arpa query for dotted quad
 *
 * -q		be quiet about some non-fatal errors
 *
 * Seldom used options.
 * -------------------
 *
 * -c class	specify query class; default is C_IN
 *
 * -m		specify query type T_MAILB and trace MB records
 *
 * -r		do not use recursion when querying nameserver
 *
 * -s secs	specify timeout value in seconds; default is 2 * 5
 *
 * -u		use virtual circuit instead of datagram for queries
 *
 * -w		wait until nameserver becomes available
 *
 */

static char Usage[] =
"\
Usage:      host [-v] [-a] [-t querytype]  name  [server]\n\
Listing:    host [-v] [-a] [-t querytype]  -l domain  [server]\n\
Hostcount:  host [-v] -H [-D] [-E] [-G] domain\n\
Check soa:  host [-v] -C domain\n\
Addrcheck:  host [-v] -A host\n\
Special options: [-L level] [-S] [-p]\n\
Common  options: [-T] [-d] [-e] [-f logfile] [-i] [-q]\n\
Other   options: [-c class] [-m] [-r] [-s secs] [-u] [-w]\
";

#define justfun			/* this is only for fun */

#ifdef DEBUG
#define assert(condition)\
{\
	if (!(condition))\
	{\
		(void) fprintf(stderr, "assertion botch: ");\
		(void) fprintf(stderr, "%s(%d): ", __FILE__, __LINE__);\
		(void) fprintf(stderr, "%s\n", "condition");\
		exit(EX_SOFTWARE);\
	}\
}
#else
#define assert(condition)
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sysexits.h>
#include <netdb.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>

#define input			/* read-only input parameter */
#define output			/* modified output parameter */

typedef char	ptr_t;		/* generic pointer type; will become void */
typedef u_int	siz_t;		/* general size type; will become int */

typedef int	bool;		/* boolean type */
#define TRUE	1
#define FALSE	0

#include "type.h"		/* types should be defined in nameser.h */

#ifndef C_HS
#define C_HS	4
#endif

#ifndef NO_DATA
#define NO_DATA	NO_ADDRESS	/* used here only in case authoritative */
#endif

#define NO_RREC	5		/* used for non-authoritative NO_DATA */

#define T_NONE	0		/* yet unspecified resource record type */
#define T_FIRST	T_A		/* first possible type in resource record */
#define T_LAST	T_AXFR - 1	/* last  possible type in resource record */

#define MAXADDRS 35		/* max address count from gethostnamadr.c */

#define NOT_DOTTED_QUAD ((u_long)-1)
#define LOCALHOST_ADDR	((u_long)0x7f000001)

#define bitset(a,b)	(((a) & (b)) != 0)
#define sameword(a,b)	(strcasecmp(a,b) == 0)
#define samepart(a,b)	(strncasecmp(a,b,strlen(b)) == 0)
#define fakename(a)	(samepart(a,"localhost.") || samepart(a,"loopback."))
#define fakeaddr(a)	(((a) == 0) || ((a) == htonl(LOCALHOST_ADDR)))

#define newstr(a)	strcpy((char *)xalloc((ptr_t *)NULL, strlen(a)+1), a)
#define newblk(a,n,t)	(t *)xalloc((ptr_t *)a, (int)((n)*sizeof(t)))
#define xfree(a)	(void) free((ptr_t *)a)
#define incopy(a)	*((struct in_addr *)a)

#ifdef ULTRIX_RESOLV
#define nslist(i)	_res.ns_list[i].addr
#else
#define nslist(i)	_res.nsaddr_list[i]
#endif

#if PACKETSZ > 1024
#define MAXPACKET PACKETSZ
#else
#define MAXPACKET 1024
#endif

typedef union {
	HEADER header;
	u_char packet[MAXPACKET];
} querybuf;

#ifdef lint
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN int errno;
EXTERN int h_errno;		/* defined in gethostnamadr.c */
EXTERN struct state _res;	/* defined in res_init.c */

int record_stats[T_ANY+1];	/* count of resource records per type */

char cnamebuf[MAXDNAME+1];
char *cname = NULL;		/* name to which CNAME is aliased */

char mnamebuf[MAXDNAME+1];
char *mname = NULL;		/* name to which MR or MG is aliased */

char soanamebuf[MAXDNAME+1];
char *soaname = NULL;		/* domain name of SOA record */

char subnamebuf[MAXDNAME+1];
char *subname = NULL;		/* domain name of NS record */

char adrnamebuf[MAXDNAME+1];
char *adrname = NULL;		/* domain name of A record */

u_long address;			/* internet address of A record */

char servername[MAXDNAME+1];
char *server = NULL;		/* name of explicit server to query */

char *logfilename = NULL;	/* name of log file */
FILE *logfile = NULL;		/* default is stdout only */

char *queryname = NULL;		/* the name about which to query */
int querytype = T_NONE;		/* the type of the query */
int queryclass = C_IN;		/* the class of the query */

int debug = 0;			/* print resolver debugging output */
int verbose = 0;		/* verbose mode for extra output */
bool quiet = FALSE;		/* suppress some warning messages */
bool inverse = FALSE;		/* generate inverse in-addr.arpa queries */
bool primary = FALSE;		/* use primary server for zone transfers */
bool ttlprint = FALSE;		/* print ttl value in non-verbose mode */
bool waitmode = FALSE;		/* wait until server becomes available */
bool mailmode = FALSE;		/* trace MG and MR into MB records */
bool addrmode = FALSE;		/* check reverse mappings of addresses */
bool listmode = FALSE;		/* generate zone listing of domain */
bool hostmode = FALSE;		/* count real hosts residing within domain */
bool duplmode = FALSE;		/* list duplicate hosts within domain */
bool extrmode = FALSE;		/* list extrazone hosts within domain */
bool gatemode = FALSE;		/* list gateway hosts within domain */
bool checkmode = FALSE;		/* check SOA records at each nameserver */
int recursive = 0;		/* recursive listmode maximum level */
bool exclusive = FALSE;		/* exclude records that are not in domain */
bool statistics = FALSE;	/* print resource record statistics */
#ifdef justfun
int namelen = 0;		/* select records exceeding this length */
#endif

extern u_long inet_addr();	/* (char *) */
extern char *inet_ntoa();	/* (struct in_addr) */
extern char *strcpy();		/* (char *, char *) */
extern char *rindex();		/* (char *, char) */
extern char *index();		/* (char *, char) */
extern void exit();		/* (int) */

/* main.c */
int main();			/* (int, char **) */
bool execute();			/* (u_long) */
void set_server();		/* (char *) */
void fatal();			/* (char *, ...) */
void errmsg();			/* (char *, ...) */

/* info.c */
bool get_hostinfo();		/* (char *) */
bool get_domaininfo();		/* (char *, char*) */
int get_info();			/* (querybuf *, char *, int, int) */
int print_info();		/* (querybuf *, int, char *, int) */
void doprintf();		/* (char *, ...) */
u_char *print_rr();		/* (char *, u_char*, u_char*, u_char*, int) */
u_char *skip_qr();		/* (char *, u_char*, u_char*, u_char*) */

/* list.c */
bool list_domain();		/* (char *) */
int find_servers();		/* (char *) */
int get_servers();		/* (char *) */
int get_nsinfo();		/* (querybuf *, int, char *) */
bool transfer_zone();		/* (char *, int, struct in_addr) */
bool get_zone();		/* (char *, int, struct in_addr) */
char *get_primary();		/* (char *) */
bool check_domain();		/* (char *) */
int get_soainfo();		/* (querybuf *, int, char *) */
void check_soa();		/* (querybuf *, char *) */

/* addr.c */
bool check_addr();		/* (char *) */
bool check_name();		/* (u_long) */

/* util.c */
int parse_type();		/* (char *) */
int parse_class();		/* (char *) */
char *in_addr_arpa();		/* (char *) */
void print_host();		/* (char *, struct hostent *) */
void print_res();		/* (void) */
void print_statistics();	/* (char *, int) */
void clear_statistics();	/* (void) */
void print_types();		/* (char *, int) */
void ns_error();		/* (char *, int) */
char *decode_error();		/* (int) */
void print_status();		/* (querybuf *) */
void pr_error();		/* (char *, ...) */
void pr_warning();		/* (char *, ...) */
bool want_rr();			/* (int, int) */
bool indomain();		/* (char *, char *, bool) */
bool samedomain();		/* (char *, char *, bool) */
bool gluerecord();		/* (char *, char *, char **, int) */
char *pr_type();		/* (int) */
char *pr_class();		/* (int) */
int expand();			/* (char *, int, u_char*, u_char*, u_char*, char *) */
int check_size();		/* (char *, int, u_char*, u_char*, u_char*, int) */
ptr_t *xalloc();		/* (ptr_t *, int) */

/* send.c */
int res_send();			/* (char *, int, char *, int) */
int _res_connect();		/* (int, struct sockaddr_in *, int) */
int _res_write();		/* (int, char *, int) */
int _res_read();		/* (int, char *, int) */

/*
** MAIN -- Start of program host
** -----------------------------
**
**	Exits:
**		EX_OK		Operation successfully completed
**		EX_UNAVAILABLE	Could not obtain requested information
**		EX_CANTCREAT	Could not create specified logfile
**		EX_OSERR	Could not obtain resources
**		EX_USAGE	Improper parameter/option specified
**		EX_SOFTWARE	Assertion botch in DEBUG mode
*/

int
main(argc, argv)
int argc;
char *argv[];
{
	register char *option;
	struct state _new;		/* new resolver database */
	u_long addr;			/* explicit address of query */
	bool result;			/* result status of action taken */
	char *program;			/* name that host was called with */

/*
 * Synchronize stdout and stderr in case output is redirected.
 * Note that some SYSV implementations may have setlinebuf().
 */
#if defined(SYSV_SETVBUF)
	setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
#else
	setlinebuf(stdout);
#endif

/*
 * Initialize resolver. See print_res() for details.
 * Pickup current values and set new defaults.
 * Query for optional server is also done with new defaults.
 * Old defaults are (RES_RECURSE | RES_DEFNAMES | RES_DNSRCH)
 */
	(void) res_init();

	/* we handle default domains ourselves, thank you */
	_res.options |=  RES_DEFNAMES;
	_res.options &= ~RES_DNSRCH;

	_res.options |=  RES_RECURSE;
	_res.options &= ~RES_DEBUG;
	_res.options &= ~RES_USEVC;

	_res.retry = 2;		/* number  of retries, default = 4 */
	_res.retrans = 5;	/* timeout in seconds, default = 5 or 6 */

	/* save new defaults */
	_new = _res;

/*
 * Check if host was called with a different name.
 */
	program = rindex(argv[0], '/');
	if (program++ == NULL)
		program = argv[0];

	/* check for resource record names */
	querytype = parse_type(program);
	if (querytype < 0)
		querytype = T_NONE;

	/* check for zone listing abbreviation */
	if (sameword(program, "zone"))
		listmode = TRUE;

/*
 * Scan command line options and flags.
 */
	while (argc > 2 && argv[1][0] == '-')
	{
	    for (option = &argv[1][1]; *option != '\0'; option++)
	    {
		switch (*option)
		{
		    case 'w' :
			waitmode = TRUE;
			_new.retry = 2;
			_new.retrans = 5;
			break;

		    case 's' :
			_new.retry = 2;
			_new.retrans = atoi(argv[2]);
			if (_new.retrans <= 0)
				fatal("Invalid timeout value %s", argv[2]);
			argv++; argc--;
			break;

		    case 'r' :
			_new.options &= ~RES_RECURSE;
			break;

		    case 'u' :
			_new.options |= RES_USEVC;
			break;

		    case 'd' :
			debug++;		/* increment debugging level */
			_new.options |= RES_DEBUG;
			break;

		    case 'v' :
			verbose++;		/* increment verbosity level */
			break;

		    case 'q' :
			quiet = TRUE;
			break;

		    case 'i' :
			inverse = TRUE;
			break;

		    case 'p' :
			primary = TRUE;
			break;

		    case 'e' :
			exclusive = TRUE;
			break;

		    case 'S' :
			statistics = TRUE;
			break;

		    case 'T' :
			ttlprint = TRUE;
			break;

		    case 'A' :
			addrmode = TRUE;
			break;

		    case 'D':
		    case 'E':
		    case 'G':
			if (*option == 'D')
				duplmode = TRUE;
			if (*option == 'E')
				extrmode = TRUE;
			if (*option == 'G')
				gatemode = TRUE;
			/* fall through */

		    case 'H' :
			hostmode = TRUE;
			listmode = TRUE;
			if (querytype == T_NONE)
				querytype = -1;	/* suppress zone data output */
			break;

		    case 'C' :
			checkmode = TRUE;
			listmode = TRUE;
			if (querytype == T_NONE)
				querytype = -1;	/* suppress zone data output */
			break;

		    case 'l' :
			listmode = TRUE;
			break;

		    case 'L' :
			recursive = atoi(argv[2]);
			if (recursive <= 0)
				fatal("Invalid recursion level %s", argv[2]);
			argv++; argc--;
			break;

		    case 'f' :
			logfilename = argv[2];
			argv++; argc--;
			break;

		    case 'c' :
			queryclass = parse_class(argv[2]);
			if (queryclass < 0)
				fatal("Invalid query class %s", argv[2]);
			argv++; argc--;
			break;

		    case 't' :
			querytype = parse_type(argv[2]);
			if (querytype < 0)
				fatal("Invalid query type %s", argv[2]);
			argv++; argc--;
			break;

		    case 'a' :
			querytype = T_ANY;	/* filter anything available */
			break;

		    case 'm' :
			mailmode = TRUE;
			querytype = T_MAILB;	/* filter MINFO/MG/MR/MB data */
			break;
#ifdef justfun
		    case 'g' :
			namelen = atoi(argv[2]);
			if (namelen <= 0)
				fatal("Invalid length %s", argv[2]);
			argv++; argc--;
			break;
#endif
		    default:
			fatal(Usage);
		}
	    }

	    argv++; argc--;
	}

	/* must have at least one argument */
	if (argc < 2 || argv[1][0] == '-')
		fatal(Usage);

	/* check for nonsense input names */
	if (strlen(argv[1]) > MAXDNAME)
		fatal("Query name %s too long", argv[1]);

	if (argc > 2 && strlen(argv[2]) > MAXDNAME)
		fatal("Server name %s too long", argv[2]);

/*
 * Analyze name and type to be queried about.
 * In regular mode, the querytype is used to formulate the nameserver
 * query, and any response is filtered out when processing the answer.
 * In listmode, the querytype is used to filter out the proper records.
 */
	queryname = argv[1];
	if (queryname[0] == '\0')
		queryname = ".";

	if (sameword(queryname, "."))
		addr = NOT_DOTTED_QUAD;
	else
		addr = inet_addr(queryname);

	/* invert dotted quad if so requested */
	if ((addr != NOT_DOTTED_QUAD) && inverse)
	{
		queryname = in_addr_arpa(queryname);
		if (queryname == NULL)
			fatal("Invalid dotted quad %s", argv[1]);

		addr = NOT_DOTTED_QUAD;
	}
	else
		inverse = FALSE;

	/* set querytype for regular mode if unspecified */
	if ((querytype == T_NONE) && !listmode)
	{
		if ((addr != NOT_DOTTED_QUAD) || inverse)
			querytype = T_PTR;
		else
			querytype = T_A;
	}

	/* cannot have dotted quad in listmode */
	if (listmode && (addr != NOT_DOTTED_QUAD))
		fatal("Invalid query name %s", queryname);

	/* must have regular name or dotted quad in addrmode */
	if (addrmode && inverse)
		fatal("Invalid query name %s", queryname);

	/* show what we are going to query about */
	if (verbose)
		print_types(queryname, querytype);

/*
 * Check for possible alternative server.
 */
	if (argc > 2)
		set_server(argv[2]);

	/* set new resolver values changed by command options */
	_res.retry = _new.retry;
	_res.retrans = _new.retrans;
	_res.options = _new.options;

	/* show the new resolver database */
	if (debug > 1 || verbose > 1)
		print_res();

/*
 * Open log file if requested.
 */
	if (logfilename)
	{
		logfile = fopen(logfilename, "w");
		if (logfile == NULL)
		{
			perror(logfilename);
			exit(EX_CANTCREAT);
		}
	}

/*
 * All set. Perform requested function.
 */
	result = execute(addr);

	if (result == FALSE)
		exit(EX_UNAVAILABLE);

	exit(EX_OK);
	/*NOTREACHED*/
}

/*
** EXECUTE -- Perform the requested function
** -----------------------------------------
**
**	Returns:
**		TRUE if information was obtained successfully.
**		FALSE otherwise.
**
**	The whole environment has been set up and checked for
**	legality and consistency.
*/

bool
execute(addr)
input u_long addr;			/* explicit address of query */
{
	struct hostent *hp;
	char newnamebuf[MAXDNAME+1];
	char *newname = NULL;		/* name to which CNAME is aliased */
	int ncnames = 0;		/* count of CNAMEs in chain */
	bool result;			/* result status of action taken */

/*
 * Special mode to check inverse mappings of host addresses.
 */
	if (addrmode)
	{
		if (addr == NOT_DOTTED_QUAD)
			result = check_addr(queryname);
		else
			result = check_name(addr);
		return(result);
	}

/*
 * Special mode to list contents of specified domain.
 */
	if (listmode)
	{
		result = list_domain(queryname);
		return(result);
	}

/*
 * Regular mode to query about specified host.
 */
	result = FALSE;
	h_errno = TRY_AGAIN;

	/* retry until positive result or permanent failure */
	while (result == FALSE && h_errno == TRY_AGAIN)
	{
		if (addr == NOT_DOTTED_QUAD)
		{
			/* reset CNAME indicator */
			cname = NULL;

			/* lookup the name in question */
			if (newname == NULL)
				result = get_hostinfo(queryname);
			else
				result = get_hostinfo(newname);

			/* recurse on CNAMEs, but not too deep */
			if (cname && (querytype != T_CNAME))
			{
				newname = strcpy(newnamebuf, cname);

				ncnames++;
				if (ncnames > 5)
				{
					errmsg("Possible CNAME loop");
					return(FALSE);
				}

				result = FALSE;
				h_errno = TRY_AGAIN;
				continue;
			}
		}
		else
		{
			hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
			if (hp != NULL)
			{
				print_host("Name", hp);
				result = TRUE;
			}
		}

		/* only retry if so requested */
		if (!waitmode)
			break;
	}

	/* explain the reason of a failure */
	if (result == FALSE)
		ns_error(queryname, querytype);

	return(result);
}

/*
** SET_SERVER -- Override default nameserver with explicit server
** --------------------------------------------------------------
**
**	Returns:
**		None.
**		Aborts the program if an unknown host was given.
**
**	Side effects:
**		The global variable server is set to indicate
**		that an explicit server is being used.
**
**	The default nameserver addresses in the resolver database
**	which are initialized by res_init() from /etc/resolv.conf
**	are replaced with the (possibly multiple) addresses of an
**	explicitly named server host. If a dotted quad is given,
**	only that single address will be used.
*/

void
set_server(name)
input char *name;			/* name of server to be queried */
{
	register int i;
	struct hostent *hp;
	struct in_addr inaddr;
	u_long addr;			/* explicit address of server */

	addr = inet_addr(name);
	inaddr.s_addr = addr;
	if (addr == NOT_DOTTED_QUAD)
	{
		/* lookup all of its addresses; this must not fail */
		hp = gethostbyname(name);
		if (hp == NULL)
		{
			errmsg("Error in looking up server name");
			ns_error(name, T_A);
			exit(EX_UNAVAILABLE);
		}

		for (i = 0; i < MAXNS && hp->h_addr_list[i]; i++)
		{
			nslist(i).sin_family = AF_INET;
			nslist(i).sin_port = htons(NAMESERVER_PORT);
			nslist(i).sin_addr = incopy(hp->h_addr_list[i]);
		}
		_res.nscount = i;
	}
	else
	{
		/* lookup the name, but use only the given address */
		hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);

		nslist(0).sin_family = AF_INET;
		nslist(0).sin_port = htons(NAMESERVER_PORT);
		nslist(0).sin_addr = inaddr;
		_res.nscount = 1;
	}

	if (hp != NULL)
	{
		server = strcpy(servername, hp->h_name);
		print_host("Server", hp);
	}
	else
	{
		server = strcpy(servername, inet_ntoa(inaddr));
		printf("Server: %s\n\n", server);
	}
}

/*
** FATAL -- Abort program when illegal option encountered
** ------------------------------------------------------
**
**	Returns:
**		Aborts after issuing error message.
*/

/*VARARGS1*/
void
fatal(fmt, a, b, c, d)
input char *fmt;			/* format of message */
{
	(void) fprintf(stderr, fmt, a, b, c, d);
	(void) fprintf(stderr, "\n");
	exit(EX_USAGE);
}


/*
** ERRMSG -- Issue error message to error output
** ---------------------------------------------
**
**	Returns:
**		None.
*/

/*VARARGS1*/
void
errmsg(fmt, a, b, c, d)
input char *fmt;			/* format of message */
{
	(void) fprintf(stderr, fmt, a, b, c, d);
	(void) fprintf(stderr, "\n");
}

/*
** GET_HOSTINFO -- Principal routine to query about given name
** -----------------------------------------------------------
**
**	Returns:
**		TRUE if requested info was obtained successfully.
**		FALSE otherwise.
**
**	This is the equivalent of the resolver module res_search().
*/

bool
get_hostinfo(name)
input char *name;			/* name to query about */
{
	extern char *hostalias();
	register char **domain;
	register char *cp;
	int dot;			/* number of dots in query name */
	bool result;			/* result status of action taken */

/*
 * Single dot means root domain.
 */
	if (sameword(name, "."))
	{
		result = get_domaininfo(name, (char *)NULL);
		return(result);
	}

/*
 * Count number of dots.
 */
	for (dot = 0, cp = name; *cp != '\0'; cp++)
		if (*cp == '.')
			dot++;

/*
 * Check for aliases of single name.
 */
	if (dot == 0 && (cp = hostalias(name)) != NULL)
	{
		if (verbose)
			printf("Aliased to \"%s\"\n", cp);

		result = get_domaininfo(cp, (char *)NULL);
		return(result);
	}

/*
 * Trailing dot means absolute address.
 */
	if (dot != 0 && cp[-1] == '.')
	{
		cp[-1] = '\0';
		result = get_domaininfo(name, (char *)NULL);
		cp[-1] = '.';
		return(result);
	}

/*
 * Append own domain if appropriate.
 */
	if ((dot == 0 && bitset(RES_DEFNAMES, _res.options)) ||
	    (dot != 0 && bitset(RES_DNSRCH, _res.options)))
	{
		for (domain = _res.dnsrch; *domain; domain++)
		{
			result = get_domaininfo(name, *domain);
			if (result)
				return(result);

			/* in case nameserver not present */
			if (errno == ECONNREFUSED)
				return(FALSE);
		}
	}

/*
 * Single hostname lookup failed.
 */
	if (dot == 0)
	{
		/* set status in case we never queried */
		if (!bitset(RES_DEFNAMES, _res.options))
			h_errno = HOST_NOT_FOUND;

		return(FALSE);
	}

/*
 * Rest means fully qualified.
 */
	result = get_domaininfo(name, (char *)NULL);
	return(result);
}

/*
** GET_DOMAININFO -- Fetch and print desired info about name in domain
** -------------------------------------------------------------------
**
**	Returns:
**		TRUE if requested info was obtained successfully.
**		FALSE otherwise.
**
**	This is the equivalent of the resolver module res_querydomain().
*/

bool
get_domaininfo(name, domain)
input char *name;			/* name to query about */
input char *domain;			/* domain to which name is relative */
{
	char namebuf[2*MAXDNAME+2];	/* buffer to store full domain name */
	querybuf answer;
	int anslen;
	int result;

	if (verbose)
	{
		if (domain == NULL || domain[0] == '\0')
			printf("Trying %s ...\n", name);
		else
			printf("Trying %s within %s ...\n", name, domain);
	}

/*
 * Construct the actual domain name.
 * A null domain means the given name is already fully qualified.
 */
	if (domain == NULL || domain[0] == '\0')
		(void) sprintf(namebuf, "%.*s", MAXDNAME, name);
	else
		(void) sprintf(namebuf, "%.*s.%.*s", MAXDNAME, name, MAXDNAME, domain);
	name = namebuf;

/*
 * Fetch the desired info, and print any relevant data.
 */
	anslen = get_info(&answer, name, querytype, queryclass);
	if (anslen < 0)
		return(FALSE);

	result = print_info(&answer, anslen, name, T_ANY);
	return(result == NOERROR ? TRUE : FALSE);
}

/*
** GET_INFO -- Basic routine to issue a nameserver query
** -----------------------------------------------------
**
**	Returns:
**		Length of nameserver answer buffer, if obtained.
**		-1 if an error occurred (h_errno is set appropriately).
**
**	This is the equivalent of the resolver module res_query().
*/

int
get_info(answerbuf, name, type, class)
output querybuf *answerbuf;		/* address of buffer to store answer */
input char *name;			/* full name to query about */
input int type;				/* specific resource record type */
input int class;			/* specific resource record class */
{
	querybuf query;
	HEADER *bp;
	int ancount;
	register int n;

/*
 * Construct query, and send it to the nameserver.
 * res_send() will fail if no nameserver responded. In this case the possible
 * values for errno are ECONNREFUSED and ETIMEDOUT. If we did get an answer,
 * errno should be reset, since res_send() may have left an errno in case it
 * has used datagrams. Our private version of res_send() will leave also other
 * error statuses, and will clear errno if an answer was obtained.
 */
	errno = 0;	/* reset before querying nameserver */

	n = res_mkquery(QUERY, name, class, type, (char *)NULL, 0,
			(struct rrec *)NULL, (char *)&query, sizeof(querybuf));
	if (n < 0)
	{
		if (debug)
			(void) fprintf(stderr, "res_mkquery failed\n");
		h_errno = NO_RECOVERY;
		return(-1);
	}

	n = res_send((char *)&query, n, (char *)answerbuf, sizeof(querybuf));
	if (n < 0)
	{
		if (debug)
			(void) fprintf(stderr, "res_send failed\n");
		h_errno = TRY_AGAIN;
		return(-1);
	}

	errno = 0;	/* reset after we got an answer */

	if (n < sizeof(HEADER))
	{
		pr_error("answer length %d too short", n);
		h_errno = NO_RECOVERY;
		return(-1);
	}

/*
 * Analyze the status of the answer from the nameserver.
 */
	if (debug || verbose)
		print_status(answerbuf);

	bp = (HEADER *)answerbuf;
	ancount = ntohs(bp->ancount);

	if (bp->rcode != NOERROR || ancount == 0)
	{
		switch (bp->rcode)
		{
		    case NXDOMAIN:
			/* distinguish between authoritative or not */
			h_errno = bp->aa ? HOST_NOT_FOUND : TRY_AGAIN;
			break;

		    case NOERROR:
			/* distinguish between authoritative or not */
			h_errno = bp->aa ? NO_DATA : NO_RREC;
			break;

		    case SERVFAIL:
			h_errno = TRY_AGAIN;
			break;

		    case FORMERR:
		    case NOTIMP:
		    case REFUSED:
		    case NOCHANGE:
			h_errno = NO_RECOVERY;
			break;

		    default:
			h_errno = NO_RECOVERY;
			break;
		}
		return(-1);
	}

	h_errno = 0;
	return(n);
}

/*
** PRINT_INFO -- Check resource records in answer and print relevant data
** ----------------------------------------------------------------------
**
**	Returns:
**		NOERROR if answer buffer was processed successfully.
**		FORMERR otherwise.
**
**	Side effects:
**		Will recurse on MAILB records if appropriate.
**		See also side effects of the print_rr() routine.
*/

int
print_info(answerbuf, answerlen, name, filter)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* full name we are querying about */
input int filter;			/* type of records we want to see */
{
	HEADER *bp;
	int qdcount, ancount, nscount, arcount;
	u_char *msg, *eom;
	register u_char *cp;

	bp = (HEADER *)answerbuf;
	qdcount = ntohs(bp->qdcount);
	ancount = ntohs(bp->ancount);
	nscount = ntohs(bp->nscount);
	arcount = ntohs(bp->arcount);

	msg = (u_char *)answerbuf;
	eom = (u_char *)answerbuf + answerlen;
	cp  = (u_char *)answerbuf + sizeof(HEADER);

/*
 * Skip the query section in the response (present only in normal queries).
 */
	if (qdcount)
	{
		while (qdcount > 0 && cp < eom)
		{
			/* cp += dn_skipname(cp, eom) + QFIXEDSZ; */

			cp = skip_qr(name, cp, msg, eom);
			if (cp == NULL)
				return(FORMERR);
			qdcount--;
		}

		if (qdcount)
		{
			pr_error("invalid qdcount in response");
			return(FORMERR);
		}
	}

/*
 * Process the actual answer section in the response.
 * During zone transfers, this is the only section available.
 */
	if (ancount)
	{
		if (!listmode && verbose && !bp->aa)
			printf("The following answer is not authoritative:\n");

		while (ancount > 0 && cp < eom)
		{
			cp = print_rr(name, cp, msg, eom, filter);
			if (cp == NULL)
				return(FORMERR);
			ancount--;

		/*
		 * When we ask for address and there is a CNAME, it returns
		 * both the CNAME and the address.  Since we trace down the
		 * CNAME chain ourselves, we don't really want to print the
		 * address at this point.
		 */
			if (!listmode && !verbose && cname)
				return(NOERROR);

		/*
		 * Recursively expand MR or MG records into MB records.
		 */
			if (mailmode && !listmode && mname)
			{
				char newnamebuf[MAXDNAME+1];
				char *newname;

				newname = strcpy(newnamebuf, mname);
				mname = NULL;

				(void) get_hostinfo(newname);
			}
		}

		if (ancount)
		{
			pr_error("invalid ancount in response");
			return(FORMERR);
		}
	}

/*
 * The nameserver and additional info section are normally not processed.
 */
	if (!verbose || exclusive)
		return(NOERROR);

	if (nscount)
	{
		printf("Authoritative nameservers:\n");

		while (nscount > 0 && cp < eom)
		{
			cp = print_rr(name, cp, msg, eom, filter);
			if (cp == NULL)
				return(FORMERR);
			nscount--;
		}

		if (nscount)
		{
			pr_error("invalid nscount in response");
			return(FORMERR);
		}
	}

	if (arcount)
	{
		printf("Additional information:\n");

		while (arcount > 0 && cp < eom)
		{
			cp = print_rr(name, cp, msg, eom, filter);
			if (cp == NULL)
				return(FORMERR);
			arcount--;
		}

		if (arcount)
		{
			pr_error("invalid arcount in response");
			return(FORMERR);
		}
	}

	return(NOERROR);
}

/*
** DOPRINT -- Output resource record data if this record is wanted
** ---------------------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The global variable doprint is set by print_rr()
**		if we need to print the data.
*/

static bool doprint;		/* indicates whether or not to print */

/*VARARGS1*/
void
doprintf(fmt, a, b, c, d)
input char *fmt;			/* format of message */
{
	if (doprint)
	{
		printf(fmt, a, b, c, d);

		if (logfile)
			(void) fprintf(logfile, fmt, a, b, c, d);
	}
}

/*
** PRINT_RR -- Decode single resource record and output relevant data
** ------------------------------------------------------------------
**
**	Returns:
**		Pointer to position in answer buffer after current record.
**		NULL if there was a format error in the current record.
**
**	Outputs:
**		The global variable doprint is set appropriately
**		for use by doprintf().
**
**	Side effects:
**		Updates resource record statistics in record_stats[].
**		Sets soaname if this is an SOA record.
**		Sets subname if this is an NS record.
**		Sets adrname if this is an A record.
**		Sets address if this is an A record.
**		Sets cname if this is a valid CNAME record.
**		Sets mname if this is a valid MAILB record.
**		These variables must have been cleared before calling
**		print_info() and may be checked afterwards.
*/

u_char *
print_rr(name, cp, msg, eom, filter)
input char *name;			/* full name we are querying about */
register u_char *cp;			/* current position in answer buf */
input u_char *msg, *eom;		/* begin and end of answer buf */
input int filter;			/* type of records we want to see */
{
	char rname[MAXDNAME+1];		/* record name in LHS */
	char dname[MAXDNAME+1];		/* domain name in RHS */
	int type, class, ttl, dlen;	/* fixed values in every record */
	u_char *eor;			/* predicted position of next record */
	register int n;
	struct in_addr inaddr;
	struct protoent *protocol;
	struct servent *service;

/*
 * Pickup the standard values present in each resource record.
 */
	n = expand(name, T_NONE, cp, msg, eom, rname);
	if (n < 0)
		return(NULL);
	cp += n;

	n = 3*sizeof(u_short) + sizeof(u_long);
	if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
		return(NULL);

	type = _getshort(cp);
	cp += sizeof(u_short);

	class = _getshort(cp);
	cp += sizeof(u_short);

	ttl = _getlong(cp);
	cp += sizeof(u_long);

	dlen = _getshort(cp);
	cp += sizeof(u_short);

	eor = cp + dlen;

/*
 * Decide whether or not to print this resource record.
 */
	doprint = want_rr(type, filter);

#ifdef obsolete
	if (doprint && exclusive && !samedomain(rname, name, TRUE))
		doprint = FALSE;
#endif
	if (doprint && exclusive && !indomain(rname, name, TRUE))
		doprint = FALSE;

	if (doprint && exclusive && fakename(rname))
		doprint = FALSE;

#ifdef justfun
	if (namelen && (strlen(rname) < namelen))
		doprint = FALSE;
#endif

/*
 * Print name and common values, if appropriate.
 */
	if (verbose)
		doprintf("%-20s\t%d\t%s\t%s",
			rname, ttl, pr_class(class), pr_type(type));
	else if (ttlprint)
		doprintf("%-20s\t%d\t%s",
			rname, ttl, pr_type(type));
	else
		doprintf("%-20s\t%s",
			rname, pr_type(type));

/*
 * Update resource record statistics for zone listing.
 */
	if (type >= T_FIRST && type <= T_LAST)
		record_stats[type]++;

/*
 * Save the domain name of an SOA or NS or A record for zone listing.
 */
	if (type == T_A)
		adrname = strcpy(adrnamebuf, rname);
	else if (type == T_NS)
		subname = strcpy(subnamebuf, rname);
	else if (type == T_SOA)
		soaname = strcpy(soanamebuf, rname);

/*
 * Print type specific data, if appropriate.
 */
	switch (type)
	{
	    case T_A:
		switch (class)
		{
		    case C_IN:
		    case C_HS:
			if (dlen == 4)
			{
				bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
				address = inaddr.s_addr;
				doprintf("\t%s", inet_ntoa(inaddr));
				cp += dlen;
				break;
			}
			else if (dlen == 7)
			{
				bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
				address = inaddr.s_addr;
				doprintf("\t%s", inet_ntoa(inaddr));
				doprintf(", protocol = %d", cp[4]);
				doprintf(", port = %d", (cp[5] << 8) + cp[6]);
				cp += dlen;
				break;
			}
			address = 0;
			break;

		    default:
			address = 0;
			cp += dlen;
			break;
		}
		break;

	    case T_MX:
		if (check_size(rname, type, cp, msg, eor, sizeof(u_short)) < 0)
			break;
		if (verbose || ttlprint)
			doprintf("\t%ld ", _getshort(cp));
		else
			doprintf("\t");
		cp += sizeof(u_short);

		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("%s", dname);
		cp += n;
		break;

	    case T_NS:
	    case T_PTR:
	    case T_CNAME:
		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", dname);
		cp += n;
		break;

	    case T_SOA:
		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", dname);
		cp += n;

		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", dname);
		cp += n;

		n = 5*sizeof(u_long);
		if (check_size(rname, type, cp, msg, eor, n) < 0)
			break;
		doprintf(" (\n\t\t\t%ld\t;serial (version)", _getlong(cp));
		cp += sizeof(u_long);
		doprintf("\n\t\t\t%ld\t;refresh period", _getlong(cp));
		cp += sizeof(u_long);
		doprintf("\n\t\t\t%ld\t;retry refresh time", _getlong(cp));
		cp += sizeof(u_long);
		doprintf("\n\t\t\t%ld\t;expiration period", _getlong(cp));
		cp += sizeof(u_long);
		doprintf("\n\t\t\t%ld\t;default ttl\n\t\t\t)", _getlong(cp));
		cp += sizeof(u_long);
		break;

	    case T_WKS:
		if (check_size(rname, type, cp, msg, eor, sizeof(u_long)) < 0)
			break;
		bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
		doprintf("\t%s", inet_ntoa(inaddr));
		cp += sizeof(u_long);

		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		protocol = getprotobynumber(n);
		if (protocol)
			doprintf(" %s", protocol->p_name);
		else
			doprintf(" %d", n);

		doprintf(" (");
		n = 0;
		while (cp < eor)
		{
		    register int c;

		    c = *cp++;
		    do
		    {
 			if (c & 0200)
			{
			    if (protocol)
				    service = getservbyport(htons(n), protocol->p_name);
			    else
				    service = NULL;
			    if (service)
				    doprintf(" %s", service->s_name);
			    else
				    doprintf(" %d", n);
			}
 			c <<= 1;
		    } while (++n & 07);
		}
		doprintf(" )");
		break;

	    case T_HINFO:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%.*s", n, cp);
			cp += n;
		}

		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%.*s", n, cp);
			cp += n;
		}
		break;

	    case T_MINFO:
		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", dname);
		cp += n;

		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", dname);
		cp += n;
		break;

	    case T_MB:
	    case T_MG:
	    case T_MR:
	    case T_MD:
	    case T_MF:
		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", dname);
		cp += n;
		break;

#ifdef obsolete
	    case T_TXT:
		if (dlen > 0)
		{
			doprintf("\t%.*s", dlen, cp);
			cp += dlen;
		}
		break;
#endif
	    case T_TXT:
		while (cp < eor)
		{
			if (check_size(rname, type, cp, msg, eor, 1) < 0)
				break;
			n = *cp++;
			if (n > 0)
			{
				doprintf("\t%.*s", n, cp);
				cp += n;
			}
		}
		break;

	    case T_UINFO:
		if (dlen > 0)
		{
			doprintf("\t%.*s", dlen, cp);
			cp += dlen;
		}
		break;

	    case T_UID:
	    case T_GID:
		if (dlen == 4)
		{
			doprintf("\t%ld", _getlong(cp));
			cp += dlen;
		}
		break;

	    case T_UNSPEC:
	    case T_NULL:
		cp += dlen;
		break;

	    case T_RP:
		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", dname);
		cp += n;

		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", dname);
		cp += n;
		break;

	    case T_RT:
	    case T_AFSDB:
		if (check_size(rname, type, cp, msg, eor, sizeof(u_short)) < 0)
			break;
		if (verbose || ttlprint)
			doprintf("\t%ld ", _getshort(cp));
		else
			doprintf("\t");
		cp += sizeof(u_short);

		n = expand(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("%s", dname);
		cp += n;
		break;

	    case T_X25:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%.*s", n, cp);
			cp += n;
		}
		break;

	    case T_ISDN:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%.*s", n, cp);
			cp += n;
		}

		if (cp < eor)
		{
			if (check_size(rname, type, cp, msg, eor, 1) < 0)
				break;
			n = *cp++;
			if (n > 0)
			{
				doprintf("\t%.*s", n, cp);
				cp += n;
			}
		}
		break;

	    case T_NSAP:
	    case T_NSAPPTR:
		doprintf("\t(not yet implemented)");
		cp += dlen;
		break;

	    default:
		doprintf("\t???");
		cp += dlen;
		break;
	}

	doprintf("\n");

/*
 * Save the CNAME alias for cname chain tracing.
 * Save the MR or MG alias for MB chain tracing.
 */
	if ((type == T_CNAME) && n > 0 && cp == eor)
		cname = strcpy(cnamebuf, dname);
	else if ((type == T_MR || type == T_MG) && n > 0 && cp == eor)
		mname = strcpy(mnamebuf, dname);

/*
 * Check if we have reached the exact end of this record.
 */
	if (cp != eor)
	{
		pr_error("size error in %s record for %s, dlen = %d, off by = %d",
			pr_type(type), rname, dlen, (cp - eor));

		/* we believe value of dlen; should perhaps return(NULL) */
		cp = eor;
	}

	return(cp);
}

/*
** SKIP_QR -- Skip the query record in the nameserver answer buffer
** ----------------------------------------------------------------
**
**	Returns:
**		Pointer to position in answer buffer after current record.
**		NULL if there was a format error in the current record.
*/

u_char *
skip_qr(name, cp, msg, eom)
input char *name;			/* full name we are querying about */
register u_char *cp;			/* current position in answer buf */
input u_char *msg, *eom;		/* begin and end of answer buf */
{
	char rname[MAXDNAME+1];		/* record name in LHS */
	int type, class;		/* fixed values in query record */
	register int n;

	n = expand(name, T_NONE, cp, msg, eom, rname);
	if (n < 0)
		return(NULL);
	cp += n;

	n = 2*sizeof(u_short);
	if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
		return(NULL);

	type = _getshort(cp);
	cp += sizeof(u_short);

	class = _getshort(cp);
	cp += sizeof(u_short);

#ifdef lint
	if (verbose)
		printf("%-20s\t%s\t%s\n",
			rname, pr_class(class), pr_type(type));
#endif
	return(cp);
}


/*
 * Nameserver information.
 * Stores the names and addresses of all servers that are to be queried
 * for a zone transfer of the desired domain. Normally these are the
 * authoritative primary and/or secondary nameservers for the domain.
 */

#define MAXNSNAME 12		/* maximum count of nameservers per domain */
#define MAXIPADDR 10		/* maximum count of addresses per nameserver */

char nsname[MAXNSNAME][MAXDNAME+1];		/* nameserver hostname */
struct in_addr ipaddr[MAXNSNAME][MAXIPADDR];	/* nameserver addresses */
int naddrs[MAXNSNAME];				/* count of addresses */

#ifdef notyet
struct nsdata
{
	char nsname[MAXDNAME+1];		/* nameserver hostname */
	struct in_addr ipaddr[MAXIPADDR];	/* nameserver addresses */
	int naddrs;				/* count of addresses */
};

struct nsdata ns[MAXNSNAME];	/* nameserver info */
#endif

/*
 * Host information.
 * Stores names and (single) addresses encountered during the zone listing
 * of all A records that belong to the domain. Non-authoritative glue records
 * that do not belong to the domain are not stored. Glue records that belong
 * to a subdomain will be filtered out later during the host count scan.
 */

#define MAXHOSTS 25000		/* maximum count of hostnames per zone */

char *hostname[MAXHOSTS];	/* hostname of host in domain */
u_long hostaddr[MAXHOSTS];	/* first host address */
bool multaddr[MAXHOSTS];	/* set if this is a multiple address host */
int hostcount = 0;		/* count of hosts in domain */

#ifdef notyet
struct hsdata
{
	char *hostname;		/* hostname of host in domain */
	u_long hostaddr;	/* first host address */
	bool multaddr;		/* set if this is a multiple address host */
};

struct hsdata hs[MAXHOSTS];	/* info on hosts in domain */
int hostcount = 0;		/* count of hosts in domain */
#endif

#ifdef notyet
/*
 * The maximum count of subdomains per zone doesn't apply any more,
 * since the list of zonenames is now allocated dynamically, but the
 * host data could have been allocated dynamically instead of statically,
 * although this is less important since it is not saved across calls.
 */
#endif

/*
 * Subdomain information.
 * Stores the names of the subdomains encountered during the zone listing.
 */

char **zonename = NULL;		/* names of subdomains in domain */
int zonecount = 0;		/* count of subdomains in domain */

/*
 * Address information.
 * Stores the (single) addresses of hosts found in all domains traversed.
 * Used to search for duplicate hosts (same address but different name).
 */

u_long *addrlist = NULL;	/* global list of addresses */
int addrcount = 0;		/* count of global addresses */

/*
 * SOA record information.
 */

struct soa_data
{
	char sname[MAXDNAME+1];	/* name of primary server */
	char mname[MAXDNAME+1];	/* name of hostmaster mailbox */
	int serial;		/* serial (version) number */
	int refresh;		/* refresh time in seconds */
	int retry;		/* refresh retry time in seconds */
	int expire;		/* expiration time in seconds */
	int defttl;		/* default time_to_live */
};

struct soa_data soa;		/* buffer to store soa data */

/*
** LIST_DOMAIN -- Basic routine to do complete zone listing and checking
** ---------------------------------------------------------------------
**
**	Returns:
**		TRUE if the requested info was processed successfully.
**		FALSE otherwise.
*/

int total_calls = 0;		/* number of calls for possible zones */
int total_zones = 0;		/* number of zones successfully read */
int total_hosts = 0;		/* number of hosts in all subdomains */
int total_dupls = 0;		/* number of duplicates in all subdomains */

#ifdef justfun
char longname[MAXDNAME+1];	/* longest hostname found */
int longsize = 0;		/* size of longest hostname */
#endif

int recursion_level = 0;	/* current recursion level */

bool
list_domain(name)
input char *name;			/* name of domain to list */
{
	register int n;
	register int i;
	int nservers;			/* count of nameservers */
	int nzones;			/* count of subdomains */
	int nhosts;			/* count of real hostnames */
	int ndupls;			/* count of duplicate hosts */
	int nextrs;			/* count of extrazone hosts */
	int ngates;			/* count of gateway hosts */

	total_calls += 1;		/* new attempt */

/*
 * Normalize to not have trailing dot, unless it is the root domain.
 */
	n = strlen(name);
	if (n > 1 && name[n-1] == '.')
		name[n-1] = '\0';

/*
 * Indicate whether we are processing an "in-addr.arpa" inverse domain.
 * In this case we will suppress accumulating host count statistics.
 */
	inverse = indomain(name, "in-addr.arpa", FALSE);

/*
 * Find the nameservers for the given domain.
 */
	nservers = find_servers(name);
	if (nservers < 1)
	{
		errmsg("No nameservers for %s found", name);
		return(FALSE);
	}

/*
 * Make sure we have an address for at least one nameserver.
 */
	for (n = 0; n < nservers; n++)
		if (naddrs[n] > 0)
			break;

	if (n >= nservers)
	{
		errmsg("No addresses of nameservers for %s found", name);
		return(FALSE);
	}

/*
 * Check SOA records at each of the nameservers.
 * Temporarily save our current server info from the resolver database.
 * Turn off nameserver recursion and make sure answer is authoritative.
 */
	if (checkmode)
	{
		struct state save_res;	/* saved copy of resolver database */
		char *save_server;	/* saved copy of server name */

		/* save resolver database */
		save_res = _res;
		save_server = server;

		/* turn off nameserver recursion */
		_res.options &= ~RES_RECURSE;

		for (n = 0; n < nservers; n++)
		{
			if (naddrs[n] < 1)
				continue;	/* shortcut */

			server = nsname[n];
			for (i = 0; i < MAXNS && i < naddrs[n]; i++)
			{
				nslist(i).sin_family = AF_INET;
				nslist(i).sin_port = htons(NAMESERVER_PORT);
				nslist(i).sin_addr = ipaddr[n][i];
			}
			_res.nscount = i;

			if (check_domain(name) == FALSE)
				ns_error(name, T_SOA);
		}

		/* restore resolver database */
		_res = save_res;
		server = save_server;

		/* all done if maximum recursion level reached */
		if (!recursive || (recursion_level >= recursive))
			return(TRUE);
	}

/*
 * Ask zone transfer to the nameservers, until one responds.
 */
	for (n = 0; n < nservers; n++)
	{
	    for (i = 0; i < naddrs[n]; i++)
	    {
		if (verbose)
			printf("Trying server %s (%s) ...\n",
				inet_ntoa(ipaddr[n][i]), nsname[n]);

		if (transfer_zone(name, queryclass, ipaddr[n][i]))
			goto done;	/* double break */

		if (h_errno != TRY_AGAIN)
		{
			ns_error(name, T_AXFR);
			return(FALSE);
		}
#ifdef notyet
		/* in case nameserver not present */
		if (errno == ECONNREFUSED)
			break;
#endif
	    }
	}
done:
	if (n >= nservers)
	{
		ns_error(name, T_AXFR);
		errmsg("No nameservers for %s responded", name);
		return(FALSE);
	}

/*
 * Print resource record statistics if so requested.
 */
	if (statistics)
		print_statistics(name, querytype);

/*
 * Accumulate host count statistics for this domain.
 */
	nzones = zonecount;

	nhosts = 0, ndupls = 0, nextrs = 0, ngates = 0;

	for (n = 0; n < hostcount; n++)
	{
		/* skip fake hosts using a very rudimentary test */
		if (fakename(hostname[n]) || fakeaddr(hostaddr[n]))
			continue;
#ifdef justfun
		/* save longest hostname encountered so far */
		if (strlen(hostname[n]) > longsize)
		{
			longsize = strlen(hostname[n]);
			(void) strcpy(longname, hostname[n]);
		}
#endif
		/* skip apparent glue records */
		if (gluerecord(hostname[n], name, zonename, nzones))
		{
			if (verbose > 1)
				printf("%s is glue record\n", hostname[n]);
			continue;
		}

		/* otherwise count as host */
		nhosts++;

	/*
	 * Mark hosts not residing directly in domain as extrazone host.
	 */
		if (!samedomain(hostname[n], name, TRUE))
		{
			nextrs++;
			if (extrmode || (verbose > 1))
				printf("%s is extrazone host\n", hostname[n]);
		}

	/*
	 * Mark hosts with more than one address as gateway host.
	 * These are not checked for duplicate addresses.
	 */
		if (multaddr[n])
		{
			ngates++;
			if (gatemode || (verbose > 1))
				printf("%s is gateway host\n", hostname[n]);
			continue;
		}
		
	/*
	 * Compare single address hosts against global list of addresses.
	 * Multiple address hosts are too complicated to handle this way.
	 */
		for (i = 0; i < addrcount; i++)
			if (addrlist[i] == hostaddr[n])
				break;	/* duplicate */

		if (i < addrcount)
		{
			ndupls++;
			if (duplmode || (verbose > 1))
				printf("%s is duplicate host\n", hostname[n]);
		}

		if (i >= addrcount)
		{
			addrlist = newblk(addrlist, addrcount+1, u_long);
			addrlist[addrcount] = hostaddr[n];
			addrcount++;
		}
	}

/*
 * Print statistics for this domain.
 */
	if (verbose || statistics || hostmode)
	{
		printf("Found %d host%s within %s\n",
			nhosts, nhosts == 1 ? "" : "s", name);

	    if ((ndupls > 0) || duplmode || (verbose > 1))
		printf("Found %d duplicate host%s within %s\n",
			ndupls, ndupls == 1 ? "" : "s", name);

	    if ((nextrs > 0) || extrmode || (verbose > 1))
		printf("Found %d extrazone host%s within %s\n",
			nextrs, nextrs == 1 ? "" : "s", name);

	    if ((ngates > 0) || gatemode || (verbose > 1))
		printf("Found %d gateway host%s within %s\n",
			ngates, ngates == 1 ? "" : "s", name);
	}

	total_zones += 1;		/* update total zones processed */
	total_hosts += nhosts;		/* update total number of hosts */
	total_dupls += ndupls;		/* update total number of duplicates */

/*
 * The names of the hosts were allocated dynamically.
 */
	for (n = 0; n < hostcount; n++)
		xfree(hostname[n]);

/*
 * Do recursion on subdomains if requested and any were found.
 * Temporarily save subdomain list, and force allocation of new list.
 */
	if (verbose || statistics)
		printf("Found %d subdomain%s within %s\n",
			nzones, nzones == 1 ? "" : "s", name);

	if (recursive && (recursion_level < recursive))
	{
		for (n = 0; n < nzones; n++)
		{
			char **subdomain;	/* local copy of list */

			subdomain = zonename;
			zonename = NULL;	/* allocate new list */

			if (verbose || statistics || checkmode)
				printf("\n");

			if (verbose)
				printf("Entering subdomain %s\n", subdomain[n]);

			recursion_level++;
			(void) list_domain(subdomain[n]);
			recursion_level--;

			zonename = subdomain;
		}
	}

/*
 * The names of the subdomains were allocated dynamically.
 * The list of subdomain names was also allocated dynamically.
 */
	for (n = 0; n < nzones; n++)
		xfree(zonename[n]);

	if (zonename != NULL)
		xfree(zonename);

	zonename = NULL;

/*
 * Print final overall statistics.
 */
	if (recursive && (recursion_level == 0))
	{
		if (verbose || statistics || checkmode)
			printf("\n");

		if (verbose || statistics || hostmode)
			printf("Encountered %d host%s in %d domain%s within %s\n",
				total_hosts, total_hosts == 1 ? "" : "s",
				total_zones, total_zones == 1 ? "" : "s",
				name);

		if (verbose || statistics || hostmode)
			printf("Encountered %d duplicate host%s in %d domain%s within %s\n",
				total_dupls, total_dupls == 1 ? "" : "s",
				total_zones, total_zones == 1 ? "" : "s",
				name);

		if (verbose || statistics || checkmode)
			printf("Processed %d domain%s out of %d attempt%s\n",
				total_zones, total_zones == 1 ? "" : "s",
				total_calls, total_calls == 1 ? "" : "s");
#ifdef justfun
		if (verbose && (longsize > 0))
			printf("Longest hostname %s\t%d\n",
				longname, longsize);
#endif
	}

	return(TRUE);
}

/*
** FIND_SERVERS -- Fetch names and addresses of authoritative servers
** ------------------------------------------------------------------
**
**	Returns:
**		The number of servers found.
**		0 if no servers could be determined successfully.
**
**	Inputs:
**		The global variable server, if set, contains the name
**		of the explicit server to be contacted.
**		The global variable primary, if set, indicates that
**		we must use the primary nameserver for the domain.
**
**	Outputs:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
*/

int
find_servers(name)
input char *name;			/* name of domain to find servers for */
{
	struct hostent *hp;
	register int n;
	register int i;
	int nservers;			/* count of nameservers */

/*
 * Use the explicit server if given on the command line.
 * Its addresses are stored in the resolver state struct.
 * This server may not be authoritative for the given domain.
 */
	if (server)
	{
		(void) strcpy(nsname[0], server);
		for (i = 0; i < MAXIPADDR && i < _res.nscount; i++)
			ipaddr[0][i] = nslist(i).sin_addr;
		naddrs[0] = i;

		nservers = 1;
		return(nservers);
	}

/*
 * Fetch primary nameserver info if so requested.
 * Get its name from the SOA record for the domain,
 * and do a regular host lookup to fetch its addresses.
 */
	if (primary)
	{
		char *primaryname;

		primaryname = get_primary(name);
		if (primaryname == NULL)
		{
			ns_error(name, T_SOA);
			return(0);
		}

		hp = gethostbyname(primaryname);
		if (hp == NULL)
		{
			ns_error(primaryname, T_A);
			return(0);
		}

		(void) strcpy(nsname[0], hp->h_name);
		for (i = 0; i < MAXIPADDR && hp->h_addr_list[i]; i++)
			ipaddr[0][i] = incopy(hp->h_addr_list[i]);
		naddrs[0] = i;

		if (verbose)
			printf("Found %d address%s for %s\n",
				naddrs[0], naddrs[0] == 1 ? "  " : "es",
				nsname[0]);

		nservers = 1;
		return(nservers);
	}

/*
 * Otherwise we have to find the nameservers for the domain.
 */
	nservers = get_servers(name);
	if (nservers < 1)
	{
		ns_error(name, T_NS);
		return(0);
	}

/*
 * Usually we'll get addresses for all the servers in the additional
 * info section.  But in case we don't, look up their addresses.
 */
	for (n = 0; n < nservers; n++)
	{
	    if (naddrs[n] == 0)
	    {
		hp = gethostbyname(nsname[n]);
		if (hp != NULL)
		{
			for (i = 0; i < MAXIPADDR && hp->h_addr_list[i]; i++)
				ipaddr[n][i] = incopy(hp->h_addr_list[i]);
			naddrs[n] = i;
		}

		if (verbose)
			printf("Found %d address%s for %s by extra query\n",
				naddrs[n], naddrs[n] == 1 ? "  " : "es",
				nsname[n]);
	    }
	    else
	    {
		if (verbose)
			printf("Found %d address%s for %s\n",
				naddrs[n], naddrs[n] == 1 ? "  " : "es",
				nsname[n]);
	    }
	}

	return(nservers);
}

/*
** GET_SERVERS -- Fetch names and addresses of authoritative servers
** -----------------------------------------------------------------
**
**	Returns:
**		The number of servers found.
**		0 if no servers could be determined successfully.
**
**	Side effects:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
*/

int
get_servers(name)
input char *name;			/* name of domain to find servers for */
{
	querybuf answer;
	int anslen;
	int nservers;			/* count of nameservers */

	if (verbose)
		printf("Finding nameservers for %s ...\n", name);

	anslen = get_info(&answer, name, T_NS, queryclass);
	if (anslen < 0)
		return(0);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, T_ANY);

	nservers = get_nsinfo(&answer, anslen, name);
	return(nservers);
}

/*
** GET_NSINFO -- Extract nameserver data from nameserver answer buffer
** -------------------------------------------------------------------
**
**	Returns:
**		The number of servers found.
**		0 if no servers could be determined successfully.
**
**	Outputs:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
*/

int
get_nsinfo(answerbuf, answerlen, name)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* name of domain to find servers for */
{
	HEADER *bp;
	int qdcount, ancount, nscount, arcount;
	int rrcount;
	u_char *msg, *eom;
	register u_char *cp;
	register int i;
	int nservers = 0;		/* count of nameservers */

	bp = (HEADER *)answerbuf;
	qdcount = ntohs(bp->qdcount);
	ancount = ntohs(bp->ancount);
	nscount = ntohs(bp->nscount);
	arcount = ntohs(bp->arcount);

	msg = (u_char *)answerbuf;
	eom = (u_char *)answerbuf + answerlen;
	cp  = (u_char *)answerbuf + sizeof(HEADER);

	while (qdcount > 0 && cp < eom)
	{
		cp = skip_qr(name, cp, msg, eom);
		if (cp == NULL)
			return(0);
		qdcount--;
	}

/*
 * If the answer is authoritative, the names are found in the
 * answer section, and the nameserver section is empty.
 * If not, there may be duplicate names in both sections.
 * Addresses are found in the additional info section both cases.
 */
	rrcount = ancount + nscount + arcount;
	while (rrcount > 0 && cp < eom)
	{
		char rname[MAXDNAME+1];
		char dname[MAXDNAME+1];
		int type, class, ttl, dlen;
		register int n;
		struct in_addr inaddr;

		n = expand(name, T_NONE, cp, msg, eom, rname);
		if (n < 0)
			break;
		cp += n;

		n = 3*sizeof(u_short) + sizeof(u_long);
		if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
			break;

		type = _getshort(cp);
		cp += sizeof(u_short);

		class = _getshort(cp);
		cp += sizeof(u_short);

		ttl = _getlong(cp);
		cp += sizeof(u_long);

		dlen = _getshort(cp);
		cp += sizeof(u_short);
#ifdef lint
		if (verbose)
			printf("%-20s\t%d\t%s\t%s\n",
				rname, ttl, pr_class(class), pr_type(type));
#endif
		if ((type == T_NS) && sameword(rname, name))
		{
			n = expand(rname, type, cp, msg, eom, dname);
			if (n < 0)
				break;

			for (i = 0; i < nservers; i++)
				if (sameword(nsname[i], dname))
					break;	/* duplicate */

			if (i >= nservers && nservers < MAXNSNAME)
			{
				(void) strcpy(nsname[nservers], dname);
				naddrs[nservers] = 0;
				nservers++;
			}
		}
		else if ((type == T_A) && dlen == 4)
		{
			for (i = 0; i < nservers; i++)
				if (sameword(nsname[i], rname))
					break;	/* found */

			if (i < nservers && naddrs[i] < MAXIPADDR)
			{
				bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
				ipaddr[i][naddrs[i]] = inaddr;
				naddrs[i]++;
			}
		}

		cp += dlen;
		rrcount--;
	}

	return(nservers);
}

/*
** TRANSFER_ZONE -- Wrapper for get_zone() to hide administrative tasks
** --------------------------------------------------------------------
**
**	Returns:
**		See get_zone() for details.
**
**	Side effects:
**		See get_zone() for details.
**
**	This routine may be called repeatedly with different server
**	addresses, until one of the servers responds. Various items
**	must be reset on every try to continue with a clean slate.
*/

bool
transfer_zone(name, class, inaddr)
input char *name;			/* name of domain to do zone xfer for */
input int class;			/* specific resource record class */
input struct in_addr inaddr;		/* address of server to be queried */
{
	register int n;

/*
 * Reset the resource record statistics before each try.
 */
	clear_statistics();

/*
 * Perform the actual zone transfer.
 */
	if (get_zone(name, class, inaddr))
		return(TRUE);

/*
 * Failure to get the zone. Free any memory that may have been allocated.
 * On success it is the responsibility of the caller to free the memory.
 */
	for (n = 0; n < hostcount; n++)
		xfree(hostname[n]);

	for (n = 0; n < zonecount; n++)
		xfree(zonename[n]);

	if (zonename != NULL)
		xfree(zonename);

	zonename = NULL;

	return(FALSE);
}

/*
** GET_ZONE -- Perform a zone transfer from server at specific address
** -------------------------------------------------------------------
**
**	Returns:
**		TRUE if the zone data have been retrieved successfully.
**		FALSE if an error occurred (h_errno is set appropriately).
**		Set TRY_AGAIN wherever possible to try the next server.
**
**	Side effects:
**		Stores list of subdomains found in zonename[],
**		and the count of subdomains in zonecount.
**		Stores list of hostnames  found in hostname[],
**		and the count of hostnames in hostcount.
**		Updates resource record statistics in record_stats[].
**		This array must have been cleared before.
*/

bool
get_zone(name, class, inaddr)
input char *name;			/* name of domain to do zone xfer for */
input int class;			/* specific resource record class */
input struct in_addr inaddr;		/* address of server to be queried */
{
	querybuf query;
	querybuf answer;
	HEADER *bp;
	int ancount;
	int sock;
	struct sockaddr_in sin;
	register int n;
	register int i;
	int nrecords = 0;		/* number of records processed */
	int soacount = 0;		/* count of SOA records */

	zonecount = 0;			/* count of subdomains */
	hostcount = 0;			/* count of hostnames */

/*
 * Construct query, and connect to the given server.
 */
	errno = 0;

	n = res_mkquery(QUERY, name, class, T_AXFR, (char *)NULL, 0,
			(struct rrec *)NULL, (char *)&query, sizeof(querybuf));
	if (n < 0)
	{
		if (debug)
			(void) fprintf(stderr, "res_mkquery failed\n");
		h_errno = NO_RECOVERY;
		return(FALSE);
	}

	if (debug)
	{
		printf("get_zone()\n");
		p_query((char *)&query);
	}

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
	{
		perror("socket");
		h_errno = TRY_AGAIN;
		return(FALSE);
	}

	sin.sin_family = AF_INET;
	sin.sin_port = htons(NAMESERVER_PORT);
	sin.sin_addr = inaddr;

	if (_res_connect(sock, &sin, sizeof(sin)) < 0)
	{
		if (debug || verbose)
			perror("connect");
		(void) close(sock);
		h_errno = TRY_AGAIN;
		return(FALSE);
	}

	if (verbose)
		printf("Asking zone transfer for %s ...\n", name);

/*
 * Send the query buffer.
 */
	if (_res_write(sock, (char *)&query, n) < 0)
	{
		(void) close(sock);
		h_errno = TRY_AGAIN;
		return(FALSE);
	}

/*
 * Process all incoming records, each record in a separate packet.
 */
	while ((n = _res_read(sock, (char *)&answer, sizeof(querybuf))) != 0)
	{
		if (n < 0)
		{
			(void) close(sock);
			h_errno = TRY_AGAIN;
			return(FALSE);
		}

		if (n < sizeof(HEADER))
		{
			pr_error("answer length %d too short", n);
			(void) close(sock);
			h_errno = TRY_AGAIN;
			return(FALSE);
		}

		if (debug > 1)
		{
			printf("got answer:\n");
			p_query((char *)&answer);
		}

	/*
	 * Analyze the contents of the answer and check for errors.
	 */
		bp = (HEADER *)&answer;
		ancount = ntohs(bp->ancount);

		if (bp->rcode != NOERROR || ancount == 0)
		{
			if (verbose)
				print_status(&answer);

			switch (bp->rcode)
			{
			    case NXDOMAIN:
				/* distinguish between authoritative or not */
				h_errno = bp->aa ? HOST_NOT_FOUND : TRY_AGAIN;
				break;

			    case NOERROR:
				/* distinguish between authoritative or not */
				h_errno = bp->aa ? NO_DATA : NO_RREC;
				break;

			    default:
				h_errno = TRY_AGAIN;
				break;
			}

			(void) close(sock);
			return(FALSE);
		}

		h_errno = 0;

	/*
	 * Valid packet received. Print contents if appropriate.
	 */
		nrecords++;
		soaname = NULL;
		subname = NULL;
		adrname = NULL;

		(void) print_info(&answer, n, name, querytype);

	/*
	 * Terminate upon the second SOA record for this domain.
	 */
		if (soaname && sameword(soaname, name))
			if (soacount++)
				break;

		/* the nameserver balks on this one */
		if (soaname && !sameword(soaname, name))
			pr_warning("extraneous SOA record for %s within %s",
				soaname, name);

	/*
	 * Save encountered subdomain name for recursive listing.
	 */
		if (subname && indomain(subname, name, FALSE))
		{
			for (i = 0; i < zonecount; i++)
				if (sameword(zonename[i], subname))
					break;	/* duplicate */

			if (i >= zonecount)
			{
				zonename = newblk(zonename, zonecount+1, char *);
				zonename[zonecount] = newstr(subname);
				zonecount++;
			}
		}
#ifdef obsolete
		/* not sure whether this is illegal or not (no, it's not) */
		if (subname && !samedomain(subname, name, TRUE))
			pr_warning("extraneous NS record for %s within %s",
				subname, name);
#endif
		/* warn about strange subdomains */
		if (subname && !indomain(subname, name, TRUE))
			pr_warning("extraneous NS record for %s within %s",
				subname, name);

	/*
	 * Save encountered name of A record for hostname count.
	 */
		if (adrname && indomain(adrname, name, FALSE) && !inverse)
		{
			for (i = 0; i < hostcount; i++)
				if (sameword(hostname[i], adrname))
					break;	/* duplicate */

			if (i < hostcount && address != hostaddr[i])
				multaddr[i] = TRUE;

			if (i >= hostcount && hostcount < MAXHOSTS)
			{
				hostname[hostcount] = newstr(adrname);
				hostaddr[hostcount] = address;
				multaddr[hostcount] = FALSE;
				hostcount++;

				if (hostcount == MAXHOSTS)
					pr_error("maximum number of %d hostnames reached", hostcount);
			}
		}

		/* check for unauthoritative glue records */
		if (adrname && !indomain(adrname, name, TRUE))
			pr_warning("extraneous glue record for %s within %s",
				adrname, name);
	}

/*
 * End of zone transfer at second SOA record or zero length read.
 */
	(void) close(sock);

/*
 * Do extra check for hostnames also defined as subdomains.
 * They may have been defined in the child domain, and crept in
 * the parent domain, or may have been defined as glue records.
 * This is not necessarily an error, but the hostname count may
 * be actually wrong. Leave it in for the time being.
 */
	for (n = 0; n < hostcount; n++)
	{
	    for (i = 0; i < zonecount; i++)
	    {
		if (sameword(hostname[n], zonename[i]))
			pr_warning("extraneous A record for %s within %s",
				hostname[n], name);
	    }
	}

	if (verbose)
		printf("Transfer complete, %d records received for %s\n",
			nrecords, name);

	return(TRUE);
}

/*
** GET_PRIMARY -- Fetch name of primary nameserver for a domain
** ------------------------------------------------------------
**
**	Returns:
**		Pointer to the name of the primary server, if found.
**		NULL if the server could not be determined.
*/

char *
get_primary(name)
input char *name;			/* name of domain to get soa for */
{
	querybuf answer;
	int anslen;

	if (verbose)
		printf("Finding primary nameserver for %s ...\n", name);

	anslen = get_info(&answer, name, T_SOA, queryclass);
	if (anslen < 0)
		return(NULL);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, T_ANY);

	soaname = NULL;
	(void) get_soainfo(&answer, anslen, name);
	if (soaname == NULL)
		return(NULL);

	return(soa.sname);
}

/*
** CHECK_DOMAIN -- Fetch and analyze SOA record of a domain
** --------------------------------------------------------
**
**	Returns:
**		TRUE if the SOA record was found at the given server.
**		FALSE otherwise.
**
**	Inputs:
**		The global variable server must contain the name
**		of the server that was queried.
*/

bool
check_domain(name)
input char *name;			/* name of domain to get soa for */
{
	querybuf answer;
	int anslen;

	if (verbose)
		printf("Checking SOA for %s at server %s\n", name, server);
	else
		printf("%s (%s)\n", name, server);

	anslen = get_info(&answer, name, T_SOA, queryclass);
	if (anslen < 0)
		return(FALSE);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, T_ANY);

	soaname = NULL;
	(void) get_soainfo(&answer, anslen, name);
	if (soaname == NULL)
		return(FALSE);

	check_soa(&answer, name);
	return(TRUE);
}

/*
** GET_SOAINFO -- Extract SOA data from nameserver answer buffer
** -------------------------------------------------------------
**
**	Returns:
**		NOERROR if the SOA record was found successfully.
**		FORMERR otherwise.
**
**	Outputs:
**		The global struct soa is filled with the soa data.
**
**	Side effects:
**		Sets soaname if this is a valid SOA record.
**		This variable must have been cleared before calling
**		get_soainfo() and may be checked afterwards.
*/

int
get_soainfo(answerbuf, answerlen, name)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* name of domain to get soa for */
{
	HEADER *bp;
	int qdcount, ancount;
	u_char *msg, *eom;
	register u_char *cp;

	bp = (HEADER *)answerbuf;
	qdcount = ntohs(bp->qdcount);
	ancount = ntohs(bp->ancount);

	msg = (u_char *)answerbuf;
	eom = (u_char *)answerbuf + answerlen;
	cp  = (u_char *)answerbuf + sizeof(HEADER);

	while (qdcount > 0 && cp < eom)
	{
		cp = skip_qr(name, cp, msg, eom);
		if (cp == NULL)
			return(FORMERR);
		qdcount--;
	}

	if (qdcount)
	{
		pr_error("invalid qdcount in response");
		return(FORMERR);
	}

/*
 * Check answer section only.
 * The nameserver section may contain the nameservers for the domain,
 * and the additional section their addresses, but not guaranteed.
 */
	while (ancount > 0 && cp < eom)
	{
		char rname[MAXDNAME+1];
		int type, class, ttl, dlen;
		register int n;
		u_char *eor;

		n = expand(name, T_NONE, cp, msg, eom, rname);
		if (n < 0)
			return(FORMERR);
		cp += n;

		n = 3*sizeof(u_short) + sizeof(u_long);
		if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
			return(FORMERR);

		type = _getshort(cp);
		cp += sizeof(u_short);

		class = _getshort(cp);
		cp += sizeof(u_short);

		ttl = _getlong(cp);
		cp += sizeof(u_long);

		dlen = _getshort(cp);
		cp += sizeof(u_short);

		eor = cp + dlen;
#ifdef lint
		if (verbose)
			printf("%-20s\t%d\t%s\t%s\n",
				rname, ttl, pr_class(class), pr_type(type));
#endif
		switch (type)
		{
		    case T_SOA:
			n = expand(rname, type, cp, msg, eom, soa.sname);
			if (n < 0)
				break;
			cp += n;

			n = expand(rname, type, cp, msg, eom, soa.mname);
			if (n < 0)
				break;
			cp += n;

			n = 5*sizeof(u_long);
			if (check_size(rname, type, cp, msg, eor, n) < 0)
				break;
			soa.serial = _getlong(cp);
			cp += sizeof(u_long);
			soa.refresh = _getlong(cp);
			cp += sizeof(u_long);
			soa.retry = _getlong(cp);
			cp += sizeof(u_long);
			soa.expire = _getlong(cp);
			cp += sizeof(u_long);
			soa.defttl = _getlong(cp);
			cp += sizeof(u_long);

			/* valid complete soa record found */
			soaname = strcpy(soanamebuf, rname);
			break;

		    default:
			cp += dlen;
			break;
		}

		if (cp != eor)
		{
			pr_error("size error in %s record for %s, dlen = %d, off by = %d",
				pr_type(type), rname, dlen, (cp - eor));
			return(FORMERR);
		}

		ancount--;
	}

	if (ancount)
	{
		pr_error("invalid ancount in response");
		return(FORMERR);
	}

	return(NOERROR);
}

/*
** CHECK_SOA -- Analyze retrieved SOA records of a domain
** ------------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The global variable server must contain the name
**		of the server that was queried.
**		The global struct soa must contain the soa data.
*/

void
check_soa(answerbuf, name)
input querybuf *answerbuf;		/* address of answer buffer */
input char *name;			/* name of domain to check soa for */
{
	static char *oldname = NULL;	/* previous name of domain */
	static char *oldserver = NULL;	/* previous name of server */
	static struct soa_data oldsoa;	/* previous soa data */
	HEADER *bp;

/*
 * Print the various SOA fields in abbreviated form.
 */
	printf("%s\t%s\t(%d %d %d %d %d)\n", soa.sname, soa.mname,
		soa.serial, soa.refresh, soa.retry, soa.expire, soa.defttl);

/*
 * We are supposed to have queried an authoritative nameserver, and
 * nameserver recursion has been turned off. Answer must be authoritative.
 */
	bp = (HEADER *)answerbuf;
	if (!bp->aa)
		pr_error("SOA record for %s at %s is not authoritative",
			name, server);

/*
 * Compare various fields with those of the previous query, if any.
 * Different serial numbers may be present if secondaries have not yet
 * refreshed the data from the primary.
 */
	if (oldname && !sameword(name, oldname))
		oldname = NULL;

	if (oldname)
	{
		if (soa.serial != oldsoa.serial)
			pr_warning("%s has different serial than %s",
				server, oldserver);

		if (!sameword(soa.sname, oldsoa.sname))
			pr_error("%s has different primary than %s",
				server, oldserver);

		if (!sameword(soa.mname, oldsoa.mname))
			pr_error("%s has different hostmaster than %s",
				server, oldserver);
	}

	oldname = name;
	oldserver = server;
	oldsoa = soa;
}

/*
** CHECK_ADDR -- Check if reverse address mappings revert to host
** --------------------------------------------------------------
**
**	Returns:
**		TRUE if all addresses of host map back to host.
**		FALSE otherwise.
*/

bool
check_addr(name)
input char *name;			/* hostname to check addresses for */
{
	struct hostent *hp;
	register int i;
	struct in_addr inaddr[MAXADDRS];
	int naddr;
	char hnamebuf[MAXDNAME+1];
	char *hname;
	int matched;

/*
 * Look up the specified host to fetch its addresses.
 */
	hp = gethostbyname(name);
	if (hp == NULL)
	{
		ns_error(name, T_A);
		return(FALSE);
	}

	hname = strcpy(hnamebuf, hp->h_name);
	for (i = 0; i < MAXADDRS && hp->h_addr_list[i]; i++)
		inaddr[i] = incopy(hp->h_addr_list[i]);
	naddr = i;

	if (verbose)
		printf("Found %d address%s for %s\n",
			naddr, naddr == 1 ? "" : "es", hname);

/*
 * Map back the addresses found, and check if they revert to host.
 */
	for (matched = 0, i = 0; i < naddr; i++)
	{
		char *iname = inet_ntoa(inaddr[i]);
		u_long addr = inaddr[i].s_addr;

		if (verbose)
			printf("Checking %s address %s\n", hname, iname);

		hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
		if (hp == NULL)
			ns_error(iname, T_PTR);
		else if (!sameword(hp->h_name, hname))
			pr_error("address %s of %s maps to %s",
				iname, hname, hp->h_name);
		else
			matched++;
	}

	return(matched == naddr ? TRUE : FALSE);
}

/*
** CHECK_NAME -- Check if address belongs to host addresses
** --------------------------------------------------------
**
**	Returns:
**		TRUE if given address was found among host addresses.
**		FALSE otherwise.
*/

bool
check_name(addr)
input u_long addr;			/* address of host to check */
{
	struct hostent *hp;
	register int i;
	struct in_addr inaddr;
	char hnamebuf[MAXDNAME+1];
	char *hname;
	char inamebuf[MAXDNAME+1];
	char *iname;
	int matched;

/*
 * Check if the address is registered by fetching its hostname.
 */
	inaddr.s_addr = addr;
	iname = strcpy(inamebuf, inet_ntoa(inaddr));

	hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
	if (hp == NULL)
	{
		ns_error(iname, T_PTR);
		return(FALSE);
	}

	hname = strcpy(hnamebuf, hp->h_name);

	if (verbose)
		printf("Address %s maps to %s\n", iname, hname);

/*
 * Lookup the hostname found to fetch its addresses.
 * Check if the given address is listed among the known addresses.
 */
	hp = gethostbyname(hname);
	if (hp == NULL)
	{
		ns_error(hname, T_A);
		return(FALSE);
	}

	for (matched = 0, i = 0; hp->h_addr_list[i]; i++)
	{
		inaddr = incopy(hp->h_addr_list[i]);

		if (verbose)
			printf("Checking %s address %s\n",
				hname, inet_ntoa(inaddr));

		if (inaddr.s_addr == addr)
			matched++;
	}

	if (!matched)
		pr_error("address %s does not belong to %s", iname, hname);

	return(matched ? TRUE : FALSE);
}

/*
** PARSE_TYPE -- Decode rr type from input string
** ----------------------------------------------
**
**	Returns:
**		Value of resource record type.
**		-1 if specified record name is invalid.
**
**	Note.	T_MD, T_MF, T_MAILA are obsolete, but recognized.
**		T_AXFR is not allowed to be specified as query type.
*/

int
parse_type(str)
input char *str;			/* input string with record type */
{
	register int type;

	if (sameword(str, "A"))		return(T_A);
	if (sameword(str, "NS"))	return(T_NS);
	if (sameword(str, "MD"))	return(T_MD);		/* obsolete */
	if (sameword(str, "MF"))	return(T_MF);		/* obsolete */
	if (sameword(str, "CNAME"))	return(T_CNAME);
	if (sameword(str, "SOA"))	return(T_SOA);
	if (sameword(str, "MB"))	return(T_MB);
	if (sameword(str, "MG"))	return(T_MG);
	if (sameword(str, "MR"))	return(T_MR);
	if (sameword(str, "NULL"))	return(T_NULL);
	if (sameword(str, "WKS"))	return(T_WKS);
	if (sameword(str, "PTR"))	return(T_PTR);
	if (sameword(str, "HINFO"))	return(T_HINFO);
	if (sameword(str, "MINFO"))	return(T_MINFO);
	if (sameword(str, "MX"))	return(T_MX);
	if (sameword(str, "TXT"))	return(T_TXT);

	if (sameword(str, "RP"))	return(T_RP);
	if (sameword(str, "AFSDB"))	return(T_AFSDB);
	if (sameword(str, "X25"))	return(T_X25);
	if (sameword(str, "ISDN"))	return(T_ISDN);
	if (sameword(str, "RT"))	return(T_RT);
	if (sameword(str, "NSAP"))	return(T_NSAP);
	if (sameword(str, "NSAP-PTR"))	return(T_NSAPPTR);

	if (sameword(str, "UINFO"))	return(T_UINFO);
	if (sameword(str, "UID"))	return(T_UID);
	if (sameword(str, "GID"))	return(T_GID);
	if (sameword(str, "UNSPEC"))	return(T_UNSPEC);

	if (sameword(str, "AXFR"))	return(-1);		/* illegal */
	if (sameword(str, "MAILB"))	return(T_MAILB);
	if (sameword(str, "MAILA"))	return(T_MAILA);	/* obsolete */
	if (sameword(str, "ANY"))	return(T_ANY);
	if (sameword(str, "*"))		return(T_ANY);

	type = atoi(str);
	if (type >= T_FIRST && type <= T_LAST)
		return(type);

	return(-1);
}

/*
** PARSE_CLASS -- Decode rr class from input string
** ------------------------------------------------
**
**	Returns:
**		Value of resource class.
**		-1 if specified class name is invalid.
*/

int
parse_class(str)
input char *str;			/* input string with resource class */
{
	register int class;

	if (sameword(str, "IN"))	return(C_IN);
	if (sameword(str, "CHAOS"))	return(C_CHAOS);
	if (sameword(str, "HS"))	return(C_HS);

	if (sameword(str, "ANY"))	return(C_ANY);
	if (sameword(str, "*"))		return(C_ANY);

	class = atoi(str);
	if (class > 0)
		return(class);

	return(-1);
}

/*
** IN_ADDR_ARPA -- Convert dotted quad string to inverse in-addr.arpa
** ------------------------------------------------------------------
**
**	Returns:
**		Pointer to inverse in-addr.arpa. domain name
**		with trailing dot to force absolute domain name.
**		NULL in case of invalid dotted quad input string.
*/

char *
in_addr_arpa(dottedquad)
input char *dottedquad;			/* input string with dotted quad */
{
	static char addrbuf[32];
	u_int a[4];
	register int n;

	n = sscanf(dottedquad, "%u.%u.%u.%u", &a[0], &a[1], &a[2], &a[3]);
	switch (n)
	{
	    case 4:
		(void) sprintf(addrbuf, "%u.%u.%u.%u.in-addr.arpa.",
			a[3]&0xff, a[2]&0xff, a[1]&0xff, a[0]&0xff);
		break;

	    case 3:
		(void) sprintf(addrbuf, "%u.%u.%u.in-addr.arpa.",
			a[2]&0xff, a[1]&0xff, a[0]&0xff);
		break;

	    case 2:
		(void) sprintf(addrbuf, "%u.%u.in-addr.arpa.",
			a[1]&0xff, a[0]&0xff);
		break;

	    case 1:
		(void) sprintf(addrbuf, "%u.in-addr.arpa.",
			a[0]&0xff);
		break;

	    default:
		return(NULL);
	}

	while (--n >= 0)
		if (a[n] > 255)
			return(NULL);

	return(addrbuf);
}

/*
** PRINT_HOST -- Print hostname and address of hostent struct
** ----------------------------------------------------------
**
**	Returns:
**		None.
*/

void
print_host(heading, hp)
input char *heading;			/* header string */
input struct hostent *hp;		/* address of hostent struct */
{
	register char **ap;

	printf("%s: %s", heading, hp->h_name);

	for (ap = hp->h_addr_list; ap && *ap; ap++)
	{
		if (ap == hp->h_addr_list)
			printf("\nAddress:");

		printf(" %s", inet_ntoa(incopy(*ap)));
	}

	for (ap = hp->h_aliases; ap && *ap && **ap; ap++)
	{
		if (ap == hp->h_aliases)
			printf("\nAliases:");

		printf(" %s", *ap);
	}

	printf("\n\n");
}

/*
** PRINT_RES -- Print resolver database information
** ------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The resolver database _res is localized in the resolver.
*/

void
print_res()
{
	register int i;
	register char **domain;

/*
 * The default domain is defined by the "domain" entry in /etc/resolv.conf
 * if not overridden by the environment variable "LOCALDOMAIN".
 * If still not defined, gethostname() may yield a fully qualified hostname.
 */
	printf("Default domain:");
	if (_res.defdname[0] != '\0')
		printf(" %s", _res.defdname);
	printf("\n");

/*
 * The search domains are extracted from the default domain components,
 * but may be overridden by "search" directives in /etc/resolv.conf
 * since 4.8.3.
 */
	printf("Search domains:");
	for (domain = _res.dnsrch; *domain; domain++)
		printf(" %s", *domain);
	printf("\n");

/*
 * The routine res_send() will do _res.retry tries to contact each of the
 * _res.nscount nameserver addresses before giving up when using datagrams.
 * The first try will timeout after _res.retrans seconds. Each following
 * try will timeout after ((_res.retrans << try) / _res.nscount) seconds.
 * Note. When we contact an explicit server the addresses will be replaced
 * by the multiple addresses of the same server.
 * When doing a zone transfer _res.retrans is used for the connect timeout.
 */
	printf("Timeout per retry: %d secs\n", _res.retrans);
	printf("Number of retries: %d\n", _res.retry);

	printf("Number of addresses: %d\n", _res.nscount);
	for (i = 0; i < _res.nscount; i++)
		printf("%s\n", inet_ntoa(nslist(i).sin_addr));

/*
 * The resolver options are initialized by res_init() to contain the
 * defaults settings (RES_RECURSE | RES_DEFNAMES | RES_DNSRCH)
 * The various options have the following meaning:
 *
 *	RES_INIT	set after res_init() has been called
 *	RES_DEBUG	let the resolver modules print debugging info
 *	RES_AAONLY	want authoritative answers only (not implemented)
 *	RES_USEVC	use tcp virtual circuit instead of udp datagrams
 *	RES_PRIMARY	use primary nameserver only (not implemented)
 *	RES_IGNTC	ignore datagram truncation; don't switch to tcp
 *	RES_RECURSE	forward query if answer not locally available
 *	RES_DEFNAMES	add default domain to queryname without dot
 *	RES_STAYOPEN	keep tcp socket open for subsequent queries
 *	RES_DNSRCH	append search domains even to queryname with dot
 */
	printf("Options set:");
	if (bitset(RES_INIT,      _res.options)) printf(" INIT");
	if (bitset(RES_DEBUG,     _res.options)) printf(" DEBUG");
	if (bitset(RES_AAONLY,    _res.options)) printf(" AAONLY");
	if (bitset(RES_USEVC,     _res.options)) printf(" USEVC");
	if (bitset(RES_PRIMARY,   _res.options)) printf(" PRIMARY");
	if (bitset(RES_IGNTC,     _res.options)) printf(" IGNTC");
	if (bitset(RES_RECURSE,   _res.options)) printf(" RECURSE");
	if (bitset(RES_DEFNAMES,  _res.options)) printf(" DEFNAMES");
	if (bitset(RES_STAYOPEN,  _res.options)) printf(" STAYOPEN");
	if (bitset(RES_DNSRCH,    _res.options)) printf(" DNSRCH");
	printf("\n");

	printf("Options clr:");
	if (!bitset(RES_INIT,     _res.options)) printf(" INIT");
	if (!bitset(RES_DEBUG,    _res.options)) printf(" DEBUG");
	if (!bitset(RES_AAONLY,   _res.options)) printf(" AAONLY");
	if (!bitset(RES_USEVC,    _res.options)) printf(" USEVC");
	if (!bitset(RES_PRIMARY,  _res.options)) printf(" PRIMARY");
	if (!bitset(RES_IGNTC,    _res.options)) printf(" IGNTC");
	if (!bitset(RES_RECURSE,  _res.options)) printf(" RECURSE");
	if (!bitset(RES_DEFNAMES, _res.options)) printf(" DEFNAMES");
	if (!bitset(RES_STAYOPEN, _res.options)) printf(" STAYOPEN");
	if (!bitset(RES_DNSRCH,   _res.options)) printf(" DNSRCH");
	printf("\n");

	printf("\n");
}

/*
** PRINT_STATISTICS -- Print resource record statistics
** ----------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The record_stats[] counts have been updated by print_rr().
*/

void
print_statistics(name, filter)
input char *name;			/* name of domain we are listing */
input int filter;			/* type of records we want to see */
{
	register int type;
	int nrecords;

	for (type = T_FIRST; type <= T_LAST; type++)
	{
		nrecords = record_stats[type];
		if (nrecords > 0 || (filter != T_ANY && want_rr(type, filter)))
		{
			printf("Found %4d %-5s record%s within %s\n",
				nrecords, pr_type(type),
				nrecords == 1 ? " " : "s", name);
		}
	}
}


/*
** CLEAR_STATISTICS -- Clear resource record statistics
** ----------------------------------------------------
**
**	Returns:
**		None.
*/

void
clear_statistics()
{
	bzero((char *)record_stats, sizeof(record_stats));
}

/*
** PRINT_TYPES -- Print resource record types wanted
** -------------------------------------------------
**
**	Returns:
**		None.
*/

void
print_types(name, filter)
input char *name;			/* name we want to query about */
input int filter;			/* type of records we want to see */
{
	register int type;

	if (filter >= T_NONE)
	{
		printf("Query about %s for record types", name);
		if (filter == T_ANY)
			printf(" %s", pr_type(T_ANY));
		else
			for (type = T_FIRST; type <= T_LAST; type++)
				if (want_rr(type, filter))
					printf(" %s", pr_type(type));
		printf("\n");
	}
}

/*
** NS_ERROR -- Print error message from errno and h_errno
** ------------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The global variable server, if set, contains
**		the name of the server that was contacted.
*/

void
ns_error(name, type) 
input char *name;			/* full name we queried about */
input int type;				/* record type we queried about */
{
/*
 * If res_send() fails, it will leave errno in either of the first two
 * following states when using datagrams. Note that this depends on the
 * proper handling of connected datagram sockets, which is usually true
 * if BSD >= 43 (see res_send.c for details; it may need a patch).
 * Note. If it succeeds, it may leave errno in the state EAFNOSUPPORT
 * if it has disconnected a previously connected datagram socket, since
 * the dummy address used to disconnect does not have a proper family set.
 * Always clear errno after getting a reply, or patch res_send().
 * Our private version of res_send() will leave also other error statuses.
 */
	switch (errno)
	{
	    case ECONNREFUSED:
		/*
		 * The contacted host does not have a nameserver running.
		 * The standard res_send() also returns this if none of
		 * the intended hosts could be reached via datagrams.
		 */
		if (server)
			errmsg("Nameserver %s not running", server);
		else
			errmsg("Nameserver not running");
		break;

	    case ETIMEDOUT:
		/*
		 * The contacted host did not give any reply at all.
		 */
		if (server)
			errmsg("Nameserver %s not responding", server);
		else
			errmsg("Nameserver not responding");
		break;

	    case ENETDOWN:
	    case ENETUNREACH:
	    case EHOSTDOWN:
	    case EHOSTUNREACH:
		/*
		 * The host to be contacted or its network can not be reached.
		 * Our private res_send() also returns this using datagrams.
		 */
		if (server)
			errmsg("Nameserver %s not reachable", server);
		else
			errmsg("Nameserver not reachable");
		break;
	}

/*
 * Print the message associated with the particular nameserver error.
 */
	switch (h_errno)
	{
	    case HOST_NOT_FOUND:
		/*
		 * The specified name does definitely not exist at all.
		 * In this case the answer is always authoritative.
		 */
		errmsg("%s does not exist (Authoritative answer)", name);
		break;

	    case TRY_AGAIN:
		/*
		 * Some intermediate server failure, e.g. timeout.
		 */
		errmsg("%s %s record not found, try again",
			name, pr_type(type));
		break;

	    case NO_RECOVERY:
		/*
		 * Some irrecoverable format error, or server refusal.
		 */
		errmsg("%s %s record not found, no recovery",
			name, pr_type(type));
		break;

	    case NO_DATA:
		/*
		 * The name is valid, but the specified type does not exist.
		 * This status is here returned only in case authoritative.
		 */
		errmsg("%s has no %s record (Authoritative answer)",
			name, pr_type(type));
		break;

	    case NO_RREC:
		/*
		 * The specified type does not exist, but we don't know whether
		 * the name is valid or not. The answer was not authoritative.
		 * Perhaps recursion was off, and no data was cached locally.
		 */
		errmsg("%s %s record not present", name, pr_type(type));
		break;

	    default:
		/*
		 * Unknown cause for server failure.
		 */
		errmsg("%s %s record not found", name, pr_type(type));
		break;
	}
}

/*
** DECODE_ERROR -- Convert nameserver error code to error message
** --------------------------------------------------------------
**
**	Returns:
**		Pointer to appropriate error message.
*/

char *
decode_error(error)
input int error;			/* error code from bp->rcode */
{
	switch (error)
	{
	    case NOERROR: 	return("no error");
	    case FORMERR:	return("format error");
	    case SERVFAIL:	return("server failed");
	    case NXDOMAIN:	return("non-existent domain");
	    case NOTIMP:	return("not implemented");
	    case REFUSED:	return("query refused");
	    case NOCHANGE:	return("no change");
	}

	return("unknown error");
}

/*
** PRINT_STATUS -- Print result status after nameserver query
** ----------------------------------------------------------
**
**	Returns:
**		None.
**
**	Conditions:
**		The size of the answer buffer must have been
**		checked before to be of sufficient length,
**		i.e. to contain at least the buffer header.
*/

void
print_status(answerbuf)
input querybuf *answerbuf;		/* address of answer buffer */
{
	HEADER *bp;
	int ancount;
	bool failed;

	bp = (HEADER *)answerbuf;
	ancount = ntohs(bp->ancount);
	failed = (bp->rcode != NOERROR || ancount == 0);

	printf("Query %s, %d answer%s%s, %sstatus: %s\n",
		failed ? "failed" : "done",
		ancount, ancount == 1 ? "" : "s",
		bp->tc ? " (truncated)" : "",
		bp->aa ? "authoritative " : "",
		decode_error((int)bp->rcode));
}

/*
** PR_ERROR -- Print error message about encountered inconsistencies
** -----------------------------------------------------------------
**
**	We are supposed to have an error condition which is fatal
**	for normal continuation, and the message is always printed.
**
**	Returns:
**		None.
*/

/*VARARGS1*/
void
pr_error(fmt, a, b, c, d)
input char *fmt;			/* format of message */
{
	(void) fprintf(stderr, " *** ");
	(void) fprintf(stderr, fmt, a, b, c, d);
	(void) fprintf(stderr, "\n");
}


/*
** PR_WARNING -- Print warning message about encountered inconsistencies
** ---------------------------------------------------------------------
**
**	We are supposed to have an error condition which is non-fatal
**	for normal continuation, and the message is suppressed in case
**	quiet mode has been selected.
**
**	Returns:
**		None.
*/

/*VARARGS1*/
void
pr_warning(fmt, a, b, c, d)
input char *fmt;			/* format of message */
{
	if (!quiet)
	{
		(void) fprintf(stderr, " !!! ");
		(void) fprintf(stderr, fmt, a, b, c, d);
		(void) fprintf(stderr, "\n");
	}
}

/*
** WANT_RR -- Indicate whether the rr type matches the desired filter
** ------------------------------------------------------------------
**
**	Returns:
**		TRUE if the resource record type matches the filter.
**		FALSE otherwise.
**
**	In regular mode, the querytype is used to formulate the query,
**	and the filter is set to T_ANY to filter out any response.
**	In listmode, we get everything, so the filter is set to the
**	querytype to filter out the proper responses.
**	Note that T_NONE is the default querytype in listmode.
*/

bool
want_rr(type, filter)
input int type;				/* resource record type */
input int filter;			/* type of records we want to see */
{
	if (type == filter)
		return(TRUE);

	if (filter == T_ANY)
		return(TRUE);

	if (filter == T_NONE &&
	   (type == T_A || type == T_NS || type == T_PTR))
		return(TRUE);

	if (filter == T_MAILB &&
	   (type == T_MB || type == T_MR || type == T_MG || type == T_MINFO))
		return(TRUE);

	if (filter == T_MAILA &&
	   (type == T_MD || type == T_MF))
		return(TRUE);

	return(FALSE);
}

/*
** INDOMAIN -- Check whether a name belongs to a domain
** ----------------------------------------------------
**
**	Returns:
**		TRUE if the given name lies anywhere in the domain, or
**		if the given name is the same as the domain and may be so.
**		FALSE otherwise.
*/

bool
indomain(name, domain, equal)
input char *name;			/* the name under consideration */
input char *domain;			/* the name of the domain */
input bool equal;			/* set if name may be same as domain */
{
	register char *dot;

	if (sameword(name, domain))
		return(equal);

	if (sameword(domain, "."))
		return(TRUE);

	dot = index(name, '.');
	while (dot != NULL)
	{
		if (sameword(dot+1, domain))
			return(TRUE);

		dot = index(dot+1, '.');
	}

	return(FALSE);
}

/*
** SAMEDOMAIN -- Check whether a name belongs to a domain
** ------------------------------------------------------
**
**	Returns:
**		TRUE if the given name lies directly in the domain, or
**		if the given name is the same as the domain and may be so.
**		FALSE otherwise.
*/

bool
samedomain(name, domain, equal)
input char *name;			/* the name under consideration */
input char *domain;			/* the name of the domain */
input bool equal;			/* set if name may be same as domain */
{
	register char *dot;

	if (sameword(name, domain))
		return(equal);

	dot = index(name, '.');
	if (dot == NULL)
		return(sameword(domain, "."));

	if (sameword(dot+1, domain))
		return(TRUE);

	return(FALSE);
}

/*
** GLUERECORD -- Check whether a name is a glue record
** ---------------------------------------------------
**
**	Returns:
**		TRUE is this is a glue record.
**		FALSE otherwise.
**
**	The name is supposed to be the name of an address record.
**	If it lies directly in the given domain, it is considered
**	an ordinary host within that domain, and not a glue record.
**	If it does not belong to the given domain at all, is it
**	here considered to be a glue record.
**	If it lies in the given domain, but not directly, it is
**	considered a glue record if it belongs to any of the known
**	subdomains of the given domain.
**	In the root domain itself are no hosts, only glue records.
*/

bool
gluerecord(name, domain, zone, nzones)
input char *name;			/* the name under consideration */
input char *domain;			/* name of domain being processed */
input char *zone[];			/* list of known subdomains */
input int nzones;			/* number of known subdomains */
{
	register int n;

	if (sameword(domain, "."))
		return(TRUE);

	if (samedomain(name, domain, TRUE))
		return(FALSE);

	if (!indomain(name, domain, TRUE))
		return(TRUE);

	for (n = 0; n < nzones; n++)
		if (indomain(name, zone[n], TRUE))
			return(TRUE);

	return(FALSE);
}

/*
** PR_TYPE -- Return name of resource record type
** ----------------------------------------------
**
**	Returns:
**		Pointer to name of resource record type.
*/

char *
pr_type(type)
input int type;				/* resource record type */
{
	static char nbuf[20];

	switch (type)
	{
	    case T_A:       return("A");	/* internet address */
	    case T_NS:      return("NS");	/* authoritative server */
	    case T_MD:      return("MD");	/* mail destination */
	    case T_MF:      return("MF");	/* mail forwarder */
	    case T_CNAME:   return("CNAME");	/* canonical name */
	    case T_SOA:     return("SOA");	/* start of auth zone */
	    case T_MB:      return("MB");	/* mailbox domain name */
	    case T_MG:      return("MG");	/* mail group member */
	    case T_MR:      return("MR");	/* mail rename name */
	    case T_NULL:    return("NULL");	/* null resource record */
	    case T_WKS:     return("WKS");	/* well known service */
	    case T_PTR:     return("PTR");	/* domain name pointer */
	    case T_HINFO:   return("HINFO");	/* host information */
	    case T_MINFO:   return("MINFO");	/* mailbox information */
	    case T_MX:      return("MX");	/* mail routing info */
	    case T_TXT:     return("TXT");	/* descriptive text */

	    case T_RP:      return("RP");	/* responsible person */
	    case T_AFSDB:   return("AFSDB");	/* afs database location */
	    case T_X25:     return("X25");	/* x25 address */
	    case T_ISDN:    return("ISDN");	/* isdn address */
	    case T_RT:      return("RT");	/* route through host */
	    case T_NSAP:    return("NSAP");	/* nsap address */
	    case T_NSAPPTR: return("NSAP-PTR");	/* nsap pointer */

	    case T_UINFO:   return("UINFO");	/* user information */
	    case T_UID:     return("UID");	/* user ident */
	    case T_GID:     return("GID");	/* group ident */
	    case T_UNSPEC:  return("UNSPEC");	/* unspecified binary data */

	    case T_AXFR:    return("AXFR");	/* zone transfer */
	    case T_MAILB:   return("MAILB");	/* matches MB/MR/MG/MINFO */
	    case T_MAILA:   return("MAILA");	/* matches MD/MF */
	    case T_ANY:     return("ANY");	/* matches any type */

	    case T_NONE:    return("resource");	/* not yet determined */
	}

	(void) sprintf(nbuf, "%d", type);
	return(nbuf);
}

/*
** PR_CLASS -- Return name of resource record class
** ------------------------------------------------
**
**	Returns:
**		Pointer to name of resource record class.
*/

char *
pr_class(class)
input int class;			/* resource record class */
{
	static char nbuf[20];

	switch (class)
	{
	    case C_IN:      return("IN");	/* internet */
	    case C_CHAOS:   return("CHAOS");	/* chaosnet */
	    case C_HS:      return("HS");	/* hesiod */
	    case C_ANY:     return("ANY");	/* any class */
	}

	(void) sprintf(nbuf, "%d", class);
	return(nbuf);
}

/*
** EXPAND -- Expand compressed domain name in a recource record
** ------------------------------------------------------------
**
**	Returns:
**		Number of bytes advanced in answer buffer.
**		-1 if there was a format error.
*/

int
expand(name, type, cp, msg, eom, namebuf)
input char *name;			/* name of resource record */
input int type;				/* type of resource record */
input u_char *cp;			/* current position in answer buf */
input u_char *msg, *eom;		/* begin and end of answer buf */
output char *namebuf;			/* address of buf to expand name in */
{
	register int n;

	n = dn_expand(msg, eom, cp, (u_char *)namebuf, MAXDNAME);
	if (n < 0)
	{
		pr_error("expand error in %s record for %s, offset = %d",
			pr_type(type), name, (cp - msg));
		h_errno = NO_RECOVERY;
		return(-1);
	}

	if (namebuf[0] == '\0')
	{
		namebuf[0] = '.';
		namebuf[1] = '\0';
	}

	return(n);
}

/*
** CHECK_SIZE -- Check whether resource record is of sufficient length
** -------------------------------------------------------------------
**
**	Returns:
**		Requested size if current record is long enough.
**		-1 if current record does not have this many bytes.
**
**	Note that HINFO records are very often incomplete since only
**	one of the two data fields has been filled in and the second
**	field is missing. So we generate only a warning message.
*/

int
check_size(name, type, cp, msg, eor, size)
input char *name;			/* name of resource record */
input int type;				/* type of resource record */
input u_char *cp;			/* current position in answer buf */
input u_char *msg, *eor;		/* begin and end of answer buf */
input int size;				/* required record size remaining */
{
	if (cp + size > eor)
	{
		if (type != T_HINFO)
			pr_error("incomplete %s record for %s, offset = %d",
				pr_type(type), name, (cp - msg));
		else
			pr_warning("incomplete %s record for %s, offset = %d",
				pr_type(type), name, (cp - msg));
		h_errno = NO_RECOVERY;
		return(-1);
	}

	return(size);
}

/*
** XALLOC -- Allocate or reallocate additional memory
** --------------------------------------------------
**
**	Returns:
**		Pointer to (re)allocated buffer space.
**		Aborts if the requested memory could not be obtained.
*/

ptr_t *
xalloc(buf, size)
register ptr_t *buf;			/* current start of buffer space */
input int size;				/* number of bytes to allocate */
{
	extern ptr_t *malloc();
	extern ptr_t *realloc();

	if (buf == NULL)
		buf = malloc((siz_t)size);
	else
		buf = realloc(buf, (siz_t)size);

	if (buf == NULL)
	{
		errmsg("Out of memory");
		exit(EX_OSERR);
	}

	return(buf);
}
