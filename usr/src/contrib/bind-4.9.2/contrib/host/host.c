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
 * Rewritten by Eric Wassenaar, Nikhef-H, <e07@nikhef.nl>
 *
 * The officially maintained source of this program is available
 * via anonymous ftp from machine 'ftp.nikhef.nl' [192.16.199.1]
 * in the directory '/pub/network' as 'host.tar.Z'
 *
 * You are kindly requested to report bugs and make suggestions
 * for improvements to the author at the given email address,
 * and to not re-distribute your own modifications to others.
 */

#ifndef lint
static char Version[] = "@(#)host.c	e07@nikhef.nl (Eric Wassenaar) 930926";
#endif

#if defined(apollo) && defined(lint)
#define __attribute(x)
#endif

#define justfun			/* this is only for fun */
#undef  obsolete		/* old code left as a reminder */
#undef  notyet			/* new code for possible future use */

/*
 *			New features
 *
 * - Major overhaul of the entire code.
 * - Very rigid error checking, with more verbose error messages.
 * - Zone listing section completely rewritten.
 * - It is now possible to do recursive listings into delegated zones.
 * - Maintain resource record statistics during zone listings.
 * - Maintain count of hosts during zone listings.
 * - Check for various extraneous conditions during zone listings.
 * - Exploit multiple server addresses if available.
 * - Option to exploit only primary server for zone transfers.
 * - Option to exclude info from names that do not reside in a zone.
 * - Implement timeout handling during connect and read.
 * - Write resource record output to optional logfile.
 * - Special MB tracing by recursively expanding MR and MG records.
 * - Special mode to check SOA records at each nameserver for a zone.
 * - Special mode to check reverse mappings of host addresses.
 * - Extended syntax allows multiple arguments on command line or stdin.
 * - Configurable default options in HOST_DEFAULTS environment variable.
 * - Code is extensively documented.
 */

/*
 *			Publication history
 *
 * This information has been moved to the RELEASE_NOTES file.
 */

/*
 *			Compilation options
 *
 * This program usually compiles without special compilation options,
 * but for some platforms you may have to define special settings.
 * See the Makefile and the header file port.h for details.
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
 * datagram connections. This may not work properly on all platforms.
 *
 * The hostent struct defined in <netdb.h> is assumed to handle multiple
 * addresses in h_addr_list[]. Usually this is true if BSD >= 43.
 *
 * Your version of the nameserver may not handle queries about top-level
 * zones properly. It needs a patch if it appends the default domain
 * to single names for which it has no data cached. A fix for this is
 * available.
 *
 * The treatment of TXT records has changed from 4.8.2 to 4.8.3. Formerly,
 * the data consisted simply of the text string. Now, the text string is
 * preceded by the character count with a maximum of 255, and multiple
 * strings are embedded if the total character count exceeds 255.
 * We handle only the new situation in this program, assuming that nobody
 * uses TXT records before 4.8.3 (unfortunately this is not always true:
 * current vendor supplied software may sometimes be even pre-BIND 4.8.2).
 *
 * Note that in 4.8.3 PACKETSZ from nameser.h is still at 512, which is
 * the maximum possible packet size for datagrams, whereas MAXDATA from
 * db.h has increased from 256 to 2048. The resolver defines MAXPACKET
 * as 1024. The nameserver reads queries in a buffer of size BUFSIZ.
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
 * These are hosts that have more than one address registered under
 * the same name. Obviously we cannot recognize a gateway host if it
 * has different names associated with its different addresses.
 *
 * Duplicate hosts.
 * These are non-gateway hosts of which the address was found earlier
 * but with a different name, possibly in a totally different zone.
 * Such hosts should not be counted again in the overall host count.
 * This situation notably occurs in e.g. the "ac.uk" domain which has
 * many names registered in both the long and the abbreviated form,
 * such as 'host.department.university.ac.uk' and 'host.dept.un.ac.uk'.
 * This is probably not an error per se. It is an error if some domain
 * has registered a foreign address under a name within its own domain.
 * To recognize duplicate hosts when traversing many zones, we have to
 * maintain a global list of host addresses. To simplify things, only
 * single-address hosts are handled as such.
 *
 * Extrazone hosts.
 * These are hosts which belong to a zone but which are not residing
 * directly within the zone under consideration and which are not
 * glue records for a delegated zone of the given zone. E.g. if we are
 * processing the zone 'bar' and find 'host.foo.bar' but 'foo.bar' is not
 * an NS registered delegated zone of 'bar' then it is considered to be
 * an extrazone host. This is not necessarily an error, but it could be.
 *
 * Lame delegations.
 * If we query the SOA record of a zone at a supposedly authoritative
 * nameserver for that zone (listed in the NS records for the zone),
 * the SOA record should be present and the answer authoritative.
 * If not, we flag a lame delegation of the zone to that nameserver.
 * This may need refinement in some special cases.
 * A lame delegation is also flagged if we discover that a nameserver
 * mentioned in an NS record does not exist when looking up its address.
 */

/*
 *		Usage: host [options] name [server]
 *		Usage: host [options] -x [name ...]
 *		Usage: host [options] -X server [name ...]
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
 * -l		special mode to generate zone listing for a zone
 *
 * -L level	do recursive zone listing/checking this level deep
 *
 * -p		use primary nameserver of zone for zone transfers
 *
 * -S		print zone resource record statistics
 *
 * -H		special mode to count hosts residing in a zone
 *
 * -G		same as -H but lists gateway hosts in addition
 *
 * -E		same as -H but lists extrazone hosts in addition
 *
 * -D		same as -H but lists duplicate hosts in addition
 *
 * -C		special mode to check SOA records for a zone
 *
 * -A		special mode to check reverse mappings of host addresses
 *
 * Miscellaneous options.
 * ---------------------
 *
 * -e		exclude info from names that do not reside in the zone
 *
 * -f file	log resource record output also in given file
 *
 * -F file	same as -f, but exchange role of stdout and logfile
 *
 * -i		generate reverse in-addr.arpa query for dotted quad
 *
 * -q		be quiet about some non-fatal errors
 *
 * -T		print ttl value or MX pref during non-verbose output
 *
 * Seldom used options.
 * -------------------
 *
 * -c class	specify query class; default is C_IN
 *
 * -m		specify query type T_MAILB and trace MB records
 *
 * -o		suppress resource record output to stdout
 *
 * -r		do not use recursion when querying nameserver
 *
 * -R		repeatedly add search domains to qualify queryname
 *
 * -s secs	specify timeout value in seconds; default is 2 * 5
 *
 * -u		use virtual circuit instead of datagram for queries
 *
 * -w		wait until nameserver becomes available
 *
 * Undocumented options. (Experimental, subject to change)
 * --------------------
 *
 * -g length	only select names that are at least this long
 *
 * -B		enforce full BIND behaviour during DNSRCH
 *
 * -I chars	print illegal resource record names, but allow chars
 *
 * -M		special mode to list mailable delegated zones of zone
 */

static char Usage[] =
"\
Usage:      host [-v] [-a] [-t querytype] [options]  name  [server]\n\
Listing:    host [-v] [-a] [-t querytype] [options]  -l zone  [server]\n\
Hostcount:  host [-v] [options] -H [-D] [-E] [-G] zone\n\
Check soa:  host [-v] [options] -C zone\n\
Addrcheck:  host [-v] [options] -A host\n\
List options:    [-L level] [-S] [-p]\n\
Common options:  [-d] [-e] [-f logfile] [-F logfile] [-i] [-q] [-T]\n\
Other options:   [-c class] [-m] [-o] [-r] [-R] [-s secs] [-u] [-w]\n\
Extended usage:  [-x [name ...]] [-X server [name ...]]\
";

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <netdb.h>

#include <sys/types.h>		/* not always automatically included */
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#undef NOERROR			/* in <sys/streams.h> on solaris 2.x */
#include <arpa/nameser.h>
#include <resolv.h>

#include "type.h"		/* types should be in <arpa/nameser.h> */
#include "exit.h"		/* exit codes come from <sysexits.h> */
#include "port.h"		/* various portability definitions */

#define input			/* read-only input parameter */
#define output			/* modified output parameter */

typedef int	bool;		/* boolean type */
#define TRUE	1
#define FALSE	0

#ifndef NO_DATA
#define NO_DATA	NO_ADDRESS	/* used here only in case authoritative */
#endif

#define NO_RREC	(NO_DATA + 1)	/* used for non-authoritative NO_DATA */
#define NO_HOST	(NO_DATA + 2)	/* used for non-authoritative HOST_NOT_FOUND */

#define T_NONE	0		/* yet unspecified resource record type */
#define T_FIRST	T_A		/* first possible type in resource record */
#define T_LAST	(T_AXFR - 1)	/* last  possible type in resource record */

#define MAXADDRS 35		/* max address count from gethostnamadr.c */

#define NOT_DOTTED_QUAD ((ipaddr_t)-1)
#define LOCALHOST_ADDR	((ipaddr_t)0x7f000001)

#define is_space(c)	(isascii(c) && isspace(c))
#define is_alnum(c)	(isascii(c) && isalnum(c))
#define bitset(a,b)	(((a) & (b)) != 0)
#define sameword(a,b)	(strcasecmp(a,b) == 0)
#define samepart(a,b)	(strncasecmp(a,b,strlen(b)) == 0)
#define fakename(a)	(samepart(a,"localhost.") || samepart(a,"loopback."))
#define fakeaddr(a)	(((a) == 0) || ((a) == htonl(LOCALHOST_ADDR)))
#define incopy(a)	*((struct in_addr *)a)

#define newlist(a,n,t)	(t *)xalloc((ptr_t *)a, (siz_t)((n)*sizeof(t)))
#define newstring(a)	(char *)xalloc((ptr_t *)NULL, (siz_t)(strlen(a)+1))
#define newstr(a)	strcpy(newstring(a), a)
#define xfree(a)	(void) free((ptr_t *)a)
#define strlength(a)	(int)strlen(a)

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
EXTERN res_state_t _res;	/* defined in res_init.c */

int Errors = 0;			/* global error count */

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

ipaddr_t address;		/* internet address of A record */

char serverbuf[MAXDNAME+1];
char *server = NULL;		/* name of explicit server to query */

char realnamebuf[2*MAXDNAME+2];
char *realname = NULL;		/* the actual name that was queried */

char *logfilename = NULL;	/* name of log file */
FILE *logfile = NULL;		/* default is stdout only */
bool logexchange = FALSE;	/* exchange role of logfile and stdout */

char *illegal = NULL;		/* give warning about illegal domain names */

char *queryname = NULL;		/* the name about which to query */
int querytype = T_NONE;		/* the type of the query */
int queryclass = C_IN;		/* the class of the query */

int debug = 0;			/* print resolver debugging output */
int verbose = 0;		/* verbose mode for extra output */
#ifdef justfun
int namelen = 0;		/* select records exceeding this length */
#endif
int recursive = 0;		/* recursive listmode maximum level */

bool quiet = FALSE;		/* suppress non-fatal warning messages */
bool reverse = FALSE;		/* generate reverse in-addr.arpa queries */
bool primary = FALSE;		/* use primary server for zone transfers */
bool suppress = FALSE;		/* suppress resource record output */
bool ttlprint = FALSE;		/* print ttl value in non-verbose mode */
bool waitmode = FALSE;		/* wait until server becomes available */
bool mailmode = FALSE;		/* trace MG and MR into MB records */
bool addrmode = FALSE;		/* check reverse mappings of addresses */
bool listmode = FALSE;		/* generate zone listing of a zone */
bool hostmode = FALSE;		/* count real hosts residing within zone */
bool duplmode = FALSE;		/* list duplicate hosts within zone */
bool extrmode = FALSE;		/* list extrazone hosts within zone */
bool gatemode = FALSE;		/* list gateway hosts within zone */
bool checkmode = FALSE;		/* check SOA records at each nameserver */
bool mxrecmode = FALSE;		/* list MX records for each delegated zone */
bool exclusive = FALSE;		/* exclude records that are not in zone */
bool statistics = FALSE;	/* print resource record statistics */
bool bindcompat = FALSE;	/* enforce full BIND DNSRCH compatibility */

char **optargv = NULL;		/* argument list including default options */
int optargc = 0;		/* number of arguments in new argument list */

/* extern */
ipaddr_t inet_addr	PROTO((char *));
char *inet_ntoa		PROTO((struct in_addr));
char *hostalias		PROTO((char *));
char *getenv		PROTO((char *));
char *strcpy		PROTO((char *, char *));
char *rindex		PROTO((char *, char));
char *index		PROTO((char *, char));
ptr_t *malloc		PROTO((siz_t));
ptr_t *realloc		PROTO((ptr_t *, siz_t));
void exit		PROTO((int));

/* main.c */
int main		PROTO((int, char **));
void set_defaults	PROTO((char *, int, char **));
int process_argv	PROTO((int, char **));
int process_file	PROTO((FILE *));
int process_name	PROTO((char *));
int execute_name	PROTO((char *));
bool execute		PROTO((ipaddr_t));
void set_server		PROTO((char *));
void fatal		PROTO((char *, ...));
void errmsg		PROTO((char *, ...));

/* info.c */
bool get_hostinfo	PROTO((char *));
bool get_domaininfo	PROTO((char *, char *));
int get_info		PROTO((querybuf *, char *, int, int));
bool print_info		PROTO((querybuf *, int, char *, bool));
void doprintf		PROTO((char *, ...));
u_char *print_rrec	PROTO((char *, u_char *, u_char *, u_char *, bool));
u_char *skip_qrec	PROTO((char *, u_char *, u_char *, u_char *));

/* list.c */
bool list_zone		PROTO((char *));
bool find_servers	PROTO((char *));
bool get_servers	PROTO((char *));
bool get_nsinfo		PROTO((querybuf *, int, char *));
bool transfer_zone	PROTO((char *, int, struct in_addr, char *));
bool get_zone		PROTO((char *, int, struct in_addr, char *));
bool get_mxrec		PROTO((char *));
char *get_primary	PROTO((char *));
bool check_zone		PROTO((char *));
bool get_soainfo	PROTO((querybuf *, int, char *));
void check_soa		PROTO((querybuf *, char *));
bool check_dupl		PROTO((ipaddr_t));

/* addr.c */
bool check_addr		PROTO((char *));
bool check_name		PROTO((ipaddr_t));

/* util.c */
int parse_type		PROTO((char *));
int parse_class		PROTO((char *));
char *in_addr_arpa	PROTO((char *));
void print_host		PROTO((char *, struct hostent *));
void show_res		PROTO((void));
void print_statistics	PROTO((char *, int, int));
void clear_statistics	PROTO((void));
void show_types		PROTO((char *, int, int));
void ns_error		PROTO((char *, int, int));
char *decode_error	PROTO((int));
void print_status	PROTO((querybuf *));
void pr_error		PROTO((char *, ...));
void pr_warning		PROTO((char *, ...));
bool want_type		PROTO((int, int));
bool want_class		PROTO((int, int));
bool indomain		PROTO((char *, char *, bool));
bool samedomain		PROTO((char *, char *, bool));
bool gluerecord		PROTO((char *, char *, char **, int));
char *pr_dotname	PROTO((char *));
char *pr_type		PROTO((int));
char *pr_class		PROTO((int));
int expand_name		PROTO((char *, int, u_char *, u_char *, u_char *, char *));
int check_size		PROTO((char *, int, u_char *, u_char *, u_char *, int));
bool valid_name		PROTO((char *, bool));

/* misc.c */
ptr_t *xalloc		PROTO((ptr_t *, siz_t));
char *itoa		PROTO((int));
char *stoa		PROTO((u_char *, int));

/* send.c */
#ifdef HOST_RES_SEND
int res_send		PROTO((char *, int, char *, int));
#endif /*HOST_RES_SEND*/
int _res_connect	PROTO((int, struct sockaddr_in *, int));
int _res_write		PROTO((int, char *, int));
int _res_read		PROTO((int, char *, int));
void _res_setaddr	PROTO((struct sockaddr_in *, char *));
void _res_perror	PROTO((char *));

/* vers.c */
extern char *version;

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

/*
** MAIN -- Start of program host
** -----------------------------
**
**	Exits:
**		EX_OK		Operation successfully completed
**		EX_UNAVAILABLE	Could not obtain requested information
**		EX_CANTCREAT	Could not create specified logfile
**		EX_NOINPUT	No input arguments were found
**		EX_NOHOST	Could not lookup explicit server
**		EX_OSERR	Could not obtain resources
**		EX_USAGE	Improper parameter/option specified
**		EX_SOFTWARE	Assertion botch in DEBUG mode
*/

int
main(argc, argv)
input int argc;
input char *argv[];
{
	register char *option;
	res_state_t new_res;		/* new resolver database */
	int result;			/* result status of action taken */
	char *program;			/* name that host was called with */
	char *servername = NULL;	/* name of explicit server */
	bool extended = FALSE;		/* accept extended argument syntax */

	assert(sizeof(u_int) == 4);	/* paranoid */
	assert(sizeof(u_short) == 2);	/* paranoid */
	assert(sizeof(ipaddr_t) == 4);	/* but this is critical */

/*
 * Synchronize stdout and stderr in case output is redirected.
 */
	linebufmode(stdout);

/*
 * Initialize resolver. See show_res() for details.
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
	new_res = _res;

/*
 * Check if host was called with a different name.
 * Interpolate default options and parameters.
 */
	option = getenv("HOST_DEFAULTS");
	if (option != NULL)
	{
		set_defaults(option, argc, argv);
		argc = optargc; argv = optargv;
	}

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
	while (argc > 1 && argv[1][0] == '-')
	{
	    for (option = &argv[1][1]; *option != '\0'; option++)
	    {
		switch (*option)
		{
		    case 'w' :
			waitmode = TRUE;
			new_res.retry = 2;
			new_res.retrans = 5;
			break;

		    case 's' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing timeout value");
			new_res.retry = 2;
			new_res.retrans = atoi(argv[2]);
			if (new_res.retrans <= 0)
				fatal("Invalid timeout value %s", argv[2]);
			argv++; argc--;
			break;

		    case 'r' :
			new_res.options &= ~RES_RECURSE;
			break;

		    case 'B' :
			bindcompat = TRUE;
			/* fall through */

		    case 'R' :
			new_res.options |= RES_DNSRCH;
			break;

		    case 'u' :
			new_res.options |= RES_USEVC;
			break;

		    case 'd' :
			new_res.options |= RES_DEBUG;
			debug++;		/* increment debugging level */
			break;

		    case 'v' :
			verbose++;		/* increment verbosity level */
			break;

		    case 'q' :
			quiet = TRUE;
			break;

		    case 'i' :
			reverse = TRUE;
			break;

		    case 'p' :
			primary = TRUE;
			break;

		    case 'o' :
			suppress = TRUE;
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

		    case 'M' :
			mxrecmode = TRUE;
			listmode = TRUE;
			if (querytype == T_NONE)
				querytype = -1;	/* suppress zone data output */
			break;

		    case 'l' :
			listmode = TRUE;
			break;

		    case 'L' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing recursion level");
			recursive = atoi(argv[2]);
			if (recursive <= 0)
				fatal("Invalid recursion level %s", argv[2]);
			argv++; argc--;
			break;

		    case 'F' :
			logexchange = TRUE;
			/* fall through */

		    case 'f' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing logfile name");
			logfilename = argv[2];
			argv++; argc--;
			break;

		    case 'I' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing allowed chars");
			illegal = argv[2];
			argv++; argc--;
			break;

		    case 'c' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing query class");
			queryclass = parse_class(argv[2]);
			if (queryclass < 0)
				fatal("Invalid query class %s", argv[2]);
			argv++; argc--;
			break;

		    case 't' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing query type");
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
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing minimum length");
			namelen = atoi(argv[2]);
			if (namelen <= 0)
				fatal("Invalid minimum length %s", argv[2]);
			argv++; argc--;
			break;
#endif
		    case 'X' :
			if (argv[2] == NULL || argv[2][0] == '-')
				fatal("Missing server name");
			servername = argv[2];
			argv++; argc--;
			/* fall through */

		    case 'x' :
			extended = TRUE;
			break;

		    case 'V' :
			printf("Version %s\n", version);
			exit(EX_OK);

		    default:
			fatal(Usage);
		}
	    }

	    argv++; argc--;
	}

/*
 * Check the remaining arguments.
 */
	/* old syntax must have at least one argument */
	if (!extended && (argc < 2 || argc > 3))
		fatal(Usage);

	/* old syntax has explicit server as second argument */
	if (!extended && (argc > 2))
		servername = argv[2];

/*
 * Open log file if requested.
 * Exchange role of stdout and logfile if so specified.
 */
	if (logfilename != NULL)
	{
		logfile = fopen(logfilename, "w");
		if (logfile == NULL)
		{
			perror(logfilename);
			exit(EX_CANTCREAT);
		}

		if (logexchange)
		{
#ifdef SWAPFILE_HACK
			swapfile(logfile, stdout);
#else
			fatal("Option -F not supported");
#endif
		}
	}

/*
 * Check for possible alternative server.
 */
	if (servername != NULL)
		set_server(servername);

	/* set new resolver values changed by command options */
	_res.retry = new_res.retry;
	_res.retrans = new_res.retrans;
	_res.options = new_res.options;

	/* show the new resolver database */
	if (debug > 1 || verbose > 1)
		show_res();

	/* show customized default domain */
	option = getenv("LOCALDOMAIN");
	if (option != NULL && verbose > 1)
		printf("Explicit local domain %s\n\n", option);

/*
 * Process command line argument(s) depending on syntax.
 */
	if (!extended) /* only one argument */
		result = process_name(argv[1]);

	else if (argc < 2) /* no arguments */
		result = process_file(stdin);

	else /* multiple command line arguments */
		result = process_argv(argc, argv);

	exit(result);
	/*NOTREACHED*/
}

/*
** SET_DEFAULTS -- Interpolate default options and parameters in argv
** ------------------------------------------------------------------
**
**	Returns:
**		None.
**
**	Outputs:
**		Creates new optargv vector with optargc arguments.
*/

void
set_defaults(option, argc, argv)
input char *option;			/* option string */
input int argc;
input char *argv[];
{
	register char *p, *q;
	register int i;

/*
 * Allocate new argument vector.
 */
	optargv = newlist(NULL, 2, char *);
	optargv[0] = argv[0];
	optargc = 1;

/*
 * Construct argument list from option string.
 */
	for (p = newstr(option); *p != '\0'; p = q)
	{
		while (is_space(*p))
			p++;

		if (*p == '\0')
			break;

		for (q = p; *q != '\0' && !is_space(*q); q++)
			continue;

		if (*q != '\0')
			*q++ = '\0';

		optargv = newlist(optargv, optargc+2, char *);
		optargv[optargc] = p;
		optargc++;
	}

/*
 * Append command line arguments.
 */
	for (i = 1; i < argc; i++)
	{
		optargv = newlist(optargv, optargc+2, char *);
		optargv[optargc] = argv[i];
		optargc++;
	}

	/* and terminate */
	optargv[optargc] = NULL;
}

/*
** PROCESS_ARGV -- Process command line arguments
** ----------------------------------------------
**
**	Returns:
**		EX_OK if information was obtained successfully.
**		Appropriate exit code otherwise.
*/

int
process_argv(argc, argv)
input int argc;
input char *argv[];
{
	register int i;
	int result;			/* result status of action taken */
	int excode = EX_NOINPUT;	/* overall result status */

	for (i = 1; i < argc; i++)
	{
		/* process a single argument */
		result = process_name(argv[i]);

		/* maintain overall result */
		if (result != EX_OK || excode == EX_NOINPUT)
			excode = result;
	}

	/* return overall result */
	return(excode);
}

/*
** PROCESS_FILE -- Process arguments from input file
** -------------------------------------------------
**
**	Returns:
**		EX_OK if information was obtained successfully.
**		Appropriate exit code otherwise.
*/

int
process_file(fp)
input FILE *fp;				/* input file with query names */
{
	register char *p, *q;
	char buf[BUFSIZ];
	int result;			/* result status of action taken */
	int excode = EX_NOINPUT;	/* overall result status */

	while (fgets(buf, sizeof(buf), fp) != NULL)
	{
		p = index(buf, '\n');
		if (p != NULL)
			*p = '\0';

		/* extract names separated by whitespace */
		for (p = buf; *p != '\0'; p = q)
		{
			while (is_space(*p))
				p++;

			/* ignore comment lines */
			if (*p == '\0' || *p == '#' || *p == ';')
				break;

			for (q = p; *q != '\0' && !is_space(*q); q++)
				continue;

			if (*q != '\0')
				*q++ = '\0';

			/* process a single argument */
			result = process_name(p);

			/* maintain overall result */
			if (result != EX_OK || excode == EX_NOINPUT)
				excode = result;
		}
	}

	/* return overall result */
	return(excode);
}

/*
** PROCESS_NAME -- Process a single command line argument
** ------------------------------------------------------
**
**	Returns:
**		EX_OK if information was obtained successfully.
**		Appropriate exit code otherwise.
**
**	Wrapper for execute_name() to hide administrative tasks.
*/

int
process_name(name)
input char *name;			/* command line argument */
{
	int result;			/* result status of action taken */
	static int save_querytype;
	static bool save_reverse;
	static bool firstname = TRUE;

	/* separate subsequent pieces of output */
	if (!firstname && (verbose || debug || checkmode))
		printf("\n");

/*
 * Some global variables are redefined further on. Save their initial
 * values in the first pass, and restore them during subsequent passes.
 */
	if (firstname)
	{
		save_querytype = querytype;
		save_reverse = reverse;
		firstname = FALSE;
	}
	else
	{
		querytype = save_querytype;
		reverse = save_reverse;
	}

/*
 * Do the real work.
 */
	result = execute_name(name);
	return(result);
}

/*
** EXECUTE_NAME -- Process a single command line argument
** ------------------------------------------------------
**
**	Returns:
**		EX_OK if information was obtained successfully.
**		Appropriate exit code otherwise.
**
**	Outputs:
**		Defines queryname appropriately.
**
**	Side effects:
**		May redefine querytype and reverse if necessary.
*/

int
execute_name(name)
input char *name;			/* command line argument */
{
	ipaddr_t addr;			/* explicit address of query */
	bool result;			/* result status of action taken */

	/* check for nonsense input name */
	if (strlength(name) > MAXDNAME)
	{
		errmsg("Query name %s too long", name);
		return(EX_USAGE);
	}

/*
 * Analyze name and type to be queried about.
 * In regular mode, the querytype is used to formulate the nameserver
 * query, and any response is filtered out when processing the answer.
 * In listmode, the querytype is used to filter out the proper records.
 */
	queryname = name;
	if (queryname[0] == '\0')
		queryname = ".";

	if (sameword(queryname, "."))
		addr = NOT_DOTTED_QUAD;
	else
		addr = inet_addr(queryname);

	/* invert dotted quad if so requested */
	if ((addr != NOT_DOTTED_QUAD) && reverse)
	{
		queryname = in_addr_arpa(queryname);
		if (queryname == NULL)
		{
			errmsg("Invalid dotted quad %s", name);
			return(EX_USAGE);
		}

		addr = NOT_DOTTED_QUAD;
	}
	else
		reverse = FALSE;

	/* set querytype for regular mode if unspecified */
	if ((querytype == T_NONE) && !listmode)
	{
		if ((addr != NOT_DOTTED_QUAD) || reverse)
			querytype = T_PTR;
		else
			querytype = T_A;
	}

	/* cannot have dotted quad in listmode */
	if (listmode && (addr != NOT_DOTTED_QUAD))
	{
		errmsg("Invalid query name %s", queryname);
		return(EX_USAGE);
	}

	/* must have regular name or dotted quad in addrmode */
	if (addrmode && reverse)
	{
		errmsg("Invalid query name %s", queryname);
		return(EX_USAGE);
	}

	/* show what we are going to query about */
	if (verbose)
		show_types(queryname, querytype, queryclass);

/*
 * All set. Perform requested function.
 */
	result = execute(addr);
	return(result ? EX_OK : EX_UNAVAILABLE);
}

/*
** EXECUTE -- Perform the requested function
** -----------------------------------------
**
**	Returns:
**		TRUE if information was obtained successfully.
**		FALSE otherwise.
**
**	The whole environment has been set up and checked.
*/

bool
execute(addr)
input ipaddr_t addr;			/* explicit address of query */
{
	struct hostent *hp;
	char newnamebuf[MAXDNAME+1];
	char *newname = NULL;		/* name to which CNAME is aliased */
	int ncnames = 0;		/* count of CNAMEs in chain */
	bool result;			/* result status of action taken */

/*
 * Special mode to check reverse mappings of host addresses.
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
 * Special mode to list contents of specified zone.
 */
	if (listmode)
	{
		result = list_zone(queryname);
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
		/* reset before each query */
		realname = NULL;

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

				if (++ncnames > 5)
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

	/* use actual name if available */
	if (realname != NULL)
		queryname = realname;

	/* explain the reason of a failure */
	if (result == FALSE)
		ns_error(queryname, querytype, queryclass);

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
	ipaddr_t addr;			/* explicit address of server */

	/* check for nonsense input name */
	if (strlength(name) > MAXDNAME)
	{
		errmsg("Server name %s too long", name);
		exit(EX_USAGE);
	}

	addr = inet_addr(name);
	inaddr.s_addr = addr;
	if (addr == NOT_DOTTED_QUAD)
	{
		/* lookup all of its addresses; this must not fail */
		hp = gethostbyname(name);
		if (hp == NULL)
		{
			ns_error(name, T_A, C_IN);
			errmsg("Error in looking up server name");
			exit(EX_NOHOST);
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
		server = strcpy(serverbuf, hp->h_name);
		print_host("Server", hp);
	}
	else
	{
		server = strcpy(serverbuf, inet_ntoa(inaddr));
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

void /*VARARGS1*/
fatal(fmt, a, b, c, d)
input char *fmt;			/* format of message */
input char *a, *b, *c, *d;		/* optional arguments */
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
**
**	Side effects:
**		Increments the global error count.
*/

void /*VARARGS1*/
errmsg(fmt, a, b, c, d)
input char *fmt;			/* format of message */
input char *a, *b, *c, *d;		/* optional arguments */
{
	(void) fprintf(stderr, fmt, a, b, c, d);
	(void) fprintf(stderr, "\n");

	/* flag an error */
	Errors++;
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
**
**	In this program RES_DEFNAMES is always on, and RES_DNSRCH
**	is off by default. This means that single names without dot
**	are always, and only, tried within the own default domain,
**	and compound names are assumed to be already fully qualified.
**
**	The default BIND behaviour can be simulated by turning on
**	RES_DNSRCH with -R. The given name, whether or not compound,
**	is then	first tried within the possible search domains.
**
**	Note. In the latter case, the search terminates in case the
**	specified name exists but does not have the desired type.
**	The BIND behaviour is to continue the search. This can be
**	simulated with the undocumented option -B.
*/

bool
get_hostinfo(name)
input char *name;			/* name to query about */
{
	register char **domain;
	register char *cp;
	int dot;			/* number of dots in query name */
	bool result;			/* result status of action taken */
	char oldnamebuf[2*MAXDNAME+2];
	char *oldname;			/* saved actual name when NO_DATA */
	int nodata = 0;			/* NO_DATA status during DNSRCH */
	int nquery = 0;			/* number of extra search queries */

/*
 * Single dot means root zone.
 */
	if (sameword(name, "."))
	{
		result = get_domaininfo(name, (char *)NULL);
		return(result);
	}

/*
 * Count number of dots. Move to the end of the name.
 */
	for (dot = 0, cp = name; *cp != '\0'; cp++)
		if (*cp == '.')
			dot++;

/*
 * Check for aliases of single name.
 * Note that the alias is supposed to be fully qualified.
 */
	if (dot == 0 && (cp = hostalias(name)) != NULL)
	{
		if (verbose)
			printf("Aliased to \"%s\"\n", cp);

		result = get_domaininfo(cp, (char *)NULL);
		return(result);
	}

/*
 * Trailing dot means absolute (fully qualified) address.
 */
	if (dot != 0 && cp[-1] == '.')
	{
		cp[-1] = '\0';
		result = get_domaininfo(name, (char *)NULL);
		cp[-1] = '.';
		return(result);
	}

/*
 * Append own default domain and other search domains if appropriate.
 */
	if ((dot == 0 && bitset(RES_DEFNAMES, _res.options)) ||
	    (dot != 0 && bitset(RES_DNSRCH, _res.options)))
	{
		for (domain = _res.dnsrch; *domain; domain++)
		{
			result = get_domaininfo(name, *domain);
			if (result)
				return(result);

			/* keep count of extra search queries */
			nquery++;

			/* in case nameserver not present */
			if (errno == ECONNREFUSED)
				return(FALSE);

			/* if no further search desired (single name) */
	    		if (!bitset(RES_DNSRCH, _res.options))
				break;

			/* if name exists but has not requested type */
			if (h_errno == NO_DATA || h_errno == NO_RREC)
			{
				if (bindcompat)
				{
					/* remember status and search up */
					oldname = strcpy(oldnamebuf, realname);
					nodata = h_errno;
					continue;
				}

				return(FALSE);
			}

			/* retry only if name does not exist at all */
			if (h_errno != HOST_NOT_FOUND && h_errno != NO_HOST)
				break;
		}
	}

/*
 * Single name lookup failed.
 */
	if (dot == 0)
	{
		/* unclear what actual name should be */
		if (nquery != 1)
			realname = NULL;

		/* restore nodata status from search */
		if (bindcompat && nodata)
		{
			realname = strcpy(realnamebuf, oldname);
			h_errno = nodata;
		}

		/* set status in case we never queried */
		if (!bitset(RES_DEFNAMES, _res.options))
			h_errno = HOST_NOT_FOUND;

		return(FALSE);
	}

/*
 * Rest means fully qualified.
 */
	result = get_domaininfo(name, (char *)NULL);

	/* restore nodata status from search */
	if (!result && bindcompat && nodata)
	{
		realname = strcpy(realnamebuf, oldname);
		h_errno = nodata;
	}

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
**	Side effects:
**		Sets global variable realname to actual name queried.
**
**	This is the equivalent of the resolver module res_querydomain().
**
**	Things get a little complicated in case RES_DNSRCH is on.
**	If we get an answer but the data is corrupted, an error will be
**	returned and NO_RECOVERY will be set. This will terminate the
**	extra search loop, but a compound name will still be tried as-is.
**	The same holds if the query times out or we have a server failure,
**	in which case an error will be returned and TRY_AGAIN will be set.
**	For now we take this for granted. Normally RES_DNSRCH is disabled.
**	In this default case we do only one query and we have no problem.
*/

bool
get_domaininfo(name, domain)
input char *name;			/* name to query about */
input char *domain;			/* domain to which name is relative */
{
	char namebuf[2*MAXDNAME+2];	/* buffer to store full domain name */
	querybuf answer;
	int anslen;
	bool result;			/* result status of action taken */

/*
 * Show what we are about to query.
 */
	if (verbose)
	{
		if (domain == NULL || domain[0] == '\0')
			printf("Trying %s", name);
		else
			printf("Trying %s within %s", name, domain);

		if (server && (verbose > 1))
			printf(" at server %s", server);

		printf(" ...\n");
	}

/*
 * Construct the actual domain name.
 * A null domain means the given name is already fully qualified.
 * If the composite name is too long, res_mkquery() will fail.
 */
	if (domain == NULL || domain[0] == '\0')
		(void) sprintf(namebuf, "%.*s", MAXDNAME, name);
	else
		(void) sprintf(namebuf, "%.*s.%.*s",
				MAXDNAME, name, MAXDNAME, domain);
	name = namebuf;

/*
 * Fetch the desired info.
 */
	anslen = get_info(&answer, name, querytype, queryclass);
	result = anslen < 0 ? FALSE : TRUE;

/*
 * Print the relevant data.
 * If we got a positive answer, the data may still be corrupted.
 */
	if (result)
		result = print_info(&answer, anslen, name, FALSE);

/*
 * Remember the actual name that was queried.
 * Must be at the end to avoid clobbering during recursive calls.
 */
	realname = strcpy(realnamebuf, name);

	return(result);
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
			(rrec_data_t *)NULL, (char *)&query, sizeof(querybuf));
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
		pr_error("answer length %s too short", itoa(n));
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
			h_errno = bp->aa ? HOST_NOT_FOUND : NO_HOST;
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
**		TRUE if answer buffer was processed successfully.
**		FALSE otherwise.
**
**	Side effects:
**		Will recurse on MAILB records if appropriate.
**		See also side effects of the print_rrec() routine.
*/

bool
print_info(answerbuf, answerlen, name, listing)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* full name we are querying about */
input bool listing;			/* set if this is a zone listing */
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

			cp = skip_qrec(name, cp, msg, eom);
			if (cp == NULL)
				return(FALSE);
			qdcount--;
		}

		if (qdcount)
		{
			pr_error("invalid qdcount in response");
			h_errno = NO_RECOVERY;
			return(FALSE);
		}
	}

/*
 * Process the actual answer section in the response.
 * During zone transfers, this is the only section available.
 */
	if (ancount)
	{
		if (!listing && verbose && !bp->aa)
			printf("The following answer is not authoritative:\n");

		while (ancount > 0 && cp < eom)
		{
			cp = print_rrec(name, cp, msg, eom, listing);
			if (cp == NULL)
				return(FALSE);
			ancount--;

		/*
		 * When we ask for address and there is a CNAME, it returns
		 * both the CNAME and the address.  Since we trace down the
		 * CNAME chain ourselves, we don't really want to print the
		 * address at this point.
		 */
			if (!listmode && !verbose && cname)
				return(TRUE);

		/*
		 * Recursively expand MR or MG records into MB records.
		 */
			if (!listmode && mailmode && mname)
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
			h_errno = NO_RECOVERY;
			return(FALSE);
		}
	}

/*
 * The nameserver and additional info section are normally not processed.
 * Both sections shouldn't exist in zone transfers.
 */
	if (!verbose || exclusive)
		return(TRUE);

	if (nscount)
	{
		printf("Authoritative nameservers:\n");

		while (nscount > 0 && cp < eom)
		{
			cp = print_rrec(name, cp, msg, eom, FALSE);
			if (cp == NULL)
				return(FALSE);
			nscount--;
		}

		if (nscount)
		{
			pr_error("invalid nscount in response");
			h_errno = NO_RECOVERY;
			return(FALSE);
		}
	}

	if (arcount)
	{
		printf("Additional information:\n");

		while (arcount > 0 && cp < eom)
		{
			cp = print_rrec(name, cp, msg, eom, FALSE);
			if (cp == NULL)
				return(FALSE);
			arcount--;
		}

		if (arcount)
		{
			pr_error("invalid arcount in response");
			h_errno = NO_RECOVERY;
			return(FALSE);
		}
	}

	return(TRUE);
}

/*
** DOPRINTF -- Output resource record data if this record is wanted
** ----------------------------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The global variable doprint is set by print_rrec()
**		if we need to print the data.
*/

static bool doprint;		/* indicates whether or not to print */

void /*VARARGS1*/
doprintf(fmt, a, b, c, d)
input char *fmt;			/* format of message */
input char *a, *b, *c, *d;		/* optional arguments */
{
	if (doprint)
	{
		if (!suppress)
			printf(fmt, a, b, c, d);

		if (logfile != NULL)
			(void) fprintf(logfile, fmt, a, b, c, d);
	}
}

/*
** PRINT_RREC -- Decode single resource record and output relevant data
** --------------------------------------------------------------------
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

#define pr_name(x)	(listing ? pr_dotname(x) : x)

u_char *
print_rrec(name, cp, msg, eom, listing)
input char *name;			/* full name we are querying about */
register u_char *cp;			/* current position in answer buf */
input u_char *msg, *eom;		/* begin and end of answer buf */
input bool listing;			/* set if this is a zone listing */
{
	char rname[MAXDNAME+1];		/* record name in LHS */
	char dname[MAXDNAME+1];		/* domain name in RHS */
	int type, class, ttl, dlen;	/* fixed values in every record */
	u_char *eor;			/* predicted position of next record */
	bool classmatch;		/* set if we want to see this class */
	register int n;
	struct in_addr inaddr;
	struct protoent *protocol;
	struct servent *service;

/*
 * Pickup the standard values present in each resource record.
 */
	n = expand_name(name, T_NONE, cp, msg, eom, rname);
	if (n < 0)
		return(NULL);
	cp += n;

	n = 3*sizeof(u_short) + sizeof(u_int);
	if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
		return(NULL);

	type = _getshort(cp);
	cp += sizeof(u_short);

	class = _getshort(cp);
	cp += sizeof(u_short);

	ttl = _getlong(cp);
	cp += sizeof(u_int);

	dlen = _getshort(cp);
	cp += sizeof(u_short);

	eor = cp + dlen;

/*
 * Decide whether or not to print this resource record.
 */
	if (listing)
	{
		classmatch = want_class(class, queryclass);
		doprint = classmatch && want_type(type, querytype);
	}
	else
	{
		classmatch = want_class(class, C_ANY);
		doprint = classmatch && want_type(type, T_ANY);
	}

#ifdef obsolete
	if (doprint && exclusive && !samedomain(rname, name, TRUE))
		doprint = FALSE;
#endif
	if (doprint && exclusive && !indomain(rname, name, TRUE))
		doprint = FALSE;

	if (doprint && exclusive && fakename(rname))
		doprint = FALSE;

#ifdef justfun
	if (namelen && (strlength(rname) < namelen))
		doprint = FALSE;
#endif

/*
 * Print name and common values, if appropriate.
 */
	if (verbose)
		doprintf("%-20s\t%s\t%s\t%s",
			pr_name(rname), itoa(ttl), pr_class(class), pr_type(type));
	else if (ttlprint)
		doprintf("%-20s\t%s\t%s",
			pr_name(rname), itoa(ttl), pr_type(type));
	else
		doprintf("%-20s\t%s",
			pr_name(rname), pr_type(type));

/*
 * Update resource record statistics for zone listing.
 */
	if (listing && classmatch)
	{
		if (type >= T_FIRST && type <= T_LAST)
			record_stats[type]++;
	}

/*
 * Save the domain name of an SOA or NS or A record for zone listing.
 */
	if (listing && classmatch)
	{
		if (type == T_A)
			adrname = strcpy(adrnamebuf, rname);
		else if (type == T_NS)
			subname = strcpy(subnamebuf, rname);
		else if (type == T_SOA)
			soaname = strcpy(soanamebuf, rname);
	}

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
			if (dlen == sizeof(ipaddr_t))
			{
				bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
				address = inaddr.s_addr;
				doprintf("\t%s", inet_ntoa(inaddr));
				cp += dlen;
				break;
			}
			else if (dlen == sizeof(ipaddr_t) + 3)
			{
				bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
				address = inaddr.s_addr;
				doprintf("\t%s", inet_ntoa(inaddr));
				n = cp[4];
				doprintf(", protocol = %s", itoa(n));
				n = (cp[5] << 8) + cp[6];
				doprintf(", port = %s", itoa(n));
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
		n = _getshort(cp);
		if (verbose || ttlprint)
			doprintf("\t%s ", itoa(n));
		else
			doprintf("\t");
		cp += sizeof(u_short);

		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("%s", pr_name(dname));
		cp += n;
		break;

	    case T_NS:
	    case T_PTR:
	    case T_CNAME:
		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", pr_name(dname));
		cp += n;
		break;

	    case T_SOA:
		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", pr_name(dname));
		cp += n;

		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", pr_name(dname));
		cp += n;

		n = 5*sizeof(u_int);
		if (check_size(rname, type, cp, msg, eor, n) < 0)
			break;
		n = _getlong(cp);
		doprintf(" (\n\t\t\t%s\t;serial (version)", itoa(n));
		cp += sizeof(u_int);
		n = _getlong(cp);
		doprintf("\n\t\t\t%s\t;refresh period", itoa(n));
		cp += sizeof(u_int);
		n = _getlong(cp);
		doprintf("\n\t\t\t%s\t;retry refresh time", itoa(n));
		cp += sizeof(u_int);
		n = _getlong(cp);
		doprintf("\n\t\t\t%s\t;expiration period", itoa(n));
		cp += sizeof(u_int);
		n = _getlong(cp);
		doprintf("\n\t\t\t%s\t;default ttl\n\t\t\t)", itoa(n));
		cp += sizeof(u_int);
		break;

	    case T_WKS:
		if (check_size(rname, type, cp, msg, eor, sizeof(ipaddr_t)) < 0)
			break;
		bcopy((char *)cp, (char *)&inaddr, sizeof(inaddr));
		doprintf("\t%s", inet_ntoa(inaddr));
		cp += sizeof(ipaddr_t);

		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		protocol = getprotobynumber(n);
		if (protocol != NULL)
			doprintf(" %s", protocol->p_name);
		else
			doprintf(" %s", itoa(n));

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
			    if (protocol != NULL)
				    service = getservbyport(htons(n), protocol->p_name);
			    else
				    service = NULL;
			    if (service != NULL)
				    doprintf(" %s", service->s_name);
			    else
				    doprintf(" %s", itoa(n));
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
		doprintf("\t\"%s\"", stoa(cp, n));
		cp += n;

		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		doprintf("\t\"%s\"", stoa(cp, n));
		cp += n;
		break;

	    case T_MINFO:
		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", pr_name(dname));
		cp += n;

		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", pr_name(dname));
		cp += n;
		break;

	    case T_MB:
	    case T_MG:
	    case T_MR:
	    case T_MD:
	    case T_MF:
		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", pr_name(dname));
		cp += n;
		break;

#ifdef obsolete
	    case T_TXT:
		if (dlen > 0)
		{
			doprintf("\t%s", stoa(cp, dlen));
			cp += dlen;
		}
		break;
#endif
	    case T_TXT:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		doprintf("\t\"%s", stoa(cp, n));
		cp += n;

		while (cp < eor)
		{
			if (check_size(rname, type, cp, msg, eor, 1) < 0)
				break;
			n = *cp++;
			doprintf("%s", stoa(cp, n));
			cp += n;
		}

		doprintf("\"");
		break;

	    case T_UINFO:
		doprintf("\t\"%s\"", stoa(cp, dlen));
		cp += dlen;
		break;

	    case T_UID:
	    case T_GID:
		if (dlen == sizeof(int))
		{
			n = _getlong(cp);
			doprintf("\t%s", itoa(n));
			cp += dlen;
		}
		break;

	    case T_UNSPEC:
	    case T_NULL:
		cp += dlen;
		break;

	    case T_RP:
		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("\t%s", pr_name(dname));
		cp += n;

		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf(" %s", pr_name(dname));
		cp += n;
		break;

	    case T_RT:
	    case T_AFSDB:
		if (check_size(rname, type, cp, msg, eor, sizeof(u_short)) < 0)
			break;
		n = _getshort(cp);
		if (verbose || ttlprint)
			doprintf("\t%s ", itoa(n));
		else
			doprintf("\t");
		cp += sizeof(u_short);

		n = expand_name(rname, type, cp, msg, eom, dname);
		if (n < 0)
			break;
		doprintf("%s", pr_name(dname));
		cp += n;
		break;

	    case T_X25:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%s", stoa(cp, n));
			cp += n;
		}
		break;

	    case T_ISDN:
		if (check_size(rname, type, cp, msg, eor, 1) < 0)
			break;
		n = *cp++;
		if (n > 0)
		{
			doprintf("\t%s", stoa(cp, n));
			cp += n;
		}

		if (cp < eor)
		{
			if (check_size(rname, type, cp, msg, eor, 1) < 0)
				break;
			n = *cp++;
			if (n > 0)
			{
				doprintf("\t%s", stoa(cp, n));
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
 * Check validity of the name of this resource record.
 * Currently only during zone listing and if requested.
 */
	if (listing && illegal && !valid_name(rname, TRUE))
	{
		pr_warning("illegal %s record name %s",
			pr_type(type), rname);
	}

/*
 * Save the CNAME alias for cname chain tracing.
 * Save the MR or MG alias for MB chain tracing.
 */
	if (!listmode && classmatch)
	{
		if ((type == T_CNAME) && n > 0 && cp == eor)
			cname = strcpy(cnamebuf, dname);
		else if ((type == T_MR || type == T_MG) && n > 0 && cp == eor)
			mname = strcpy(mnamebuf, dname);
	}

/*
 * Check if we have reached the exact end of this record.
 */
	if (cp != eor)
	{
		pr_error("size error in %s record for %s, off by = %s",
			pr_type(type), rname, itoa(cp - eor));

		/* we believe value of dlen; should perhaps return(NULL) */
		cp = eor;
	}

	return(cp);
}

/*
** SKIP_QREC -- Skip the query record in the nameserver answer buffer
** ------------------------------------------------------------------
**
**	Returns:
**		Pointer to position in answer buffer after current record.
**		NULL if there was a format error in the current record.
*/

u_char *
skip_qrec(name, cp, msg, eom)
input char *name;			/* full name we are querying about */
register u_char *cp;			/* current position in answer buf */
input u_char *msg, *eom;		/* begin and end of answer buf */
{
	char rname[MAXDNAME+1];		/* record name in LHS */
	int type, class;		/* fixed values in query record */
	register int n;

	n = expand_name(name, T_NONE, cp, msg, eom, rname);
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
 * Stores names and addresses of all servers that are to be queried
 * for a zone transfer of the desired zone. Normally these are the
 * authoritative primary and/or secondary nameservers for the zone.
 */

#define MAXNSNAME 16		/* maximum count of nameservers per zone */
#define MAXIPADDR 10		/* maximum count of addresses per nameserver */

char nsname[MAXNSNAME][MAXDNAME+1];		/* nameserver hostname */
struct in_addr ipaddr[MAXNSNAME][MAXIPADDR];	/* nameserver addresses */
int naddrs[MAXNSNAME];				/* count of addresses */
int nservers = 0;				/* count of nameservers */

#ifdef notyet
typedef struct ns_data {
	char nsname[MAXDNAME+1];		/* nameserver hostname */
	struct in_addr ipaddr[MAXIPADDR];	/* nameserver addresses */
	int naddrs;				/* count of addresses */
} ns_data_t;

ns_data_t nsinfo[MAXNSNAME];	/* nameserver info */
#endif

bool authserver;		/* server is supposed to be authoritative */

/*
 * Host information.
 * Stores names and (single) addresses encountered during the zone listing
 * of all A records that belong to the zone. Non-authoritative glue records
 * that do not belong to the zone are not stored. Glue records that belong
 * to a delegated zone will be filtered out later during the host count scan.
 * The host names are allocated dynamically.
#ifdef notyet
 * The host data should have been allocated dynamically to avoid static
 * limits, but this is less important since it is not saved across calls.
#endif
 */

#define MAXHOSTS 25000		/* maximum count of hostnames per zone */

char *hostname[MAXHOSTS];	/* hostname of host in zone */
ipaddr_t hostaddr[MAXHOSTS];	/* first host address */
bool multaddr[MAXHOSTS];	/* set if this is a multiple address host */
int hostcount = 0;		/* count of hosts in zone */

#ifdef notyet
typedef struct host_data {
	char *hostname;		/* hostname of host in zone */
	ipaddr_t hostaddr;	/* first host address */
	bool multaddr;		/* set if this is a multiple address host */
} host_data_t;

host_data_t hostlist[MAXHOSTS];	/* info on hosts in zone */
#endif

/*
 * Delegated zone information.
 * Stores the names of the delegated zones encountered during the zone
 * listing. The names and the list itself are allocated dynamically.
 */

char **zonename = NULL;		/* names of delegated zones within zone */
int zonecount = 0;		/* count of delegated zones within zone */

/*
 * Address information.
 * Stores the (single) addresses of hosts found in all zones traversed.
 * Used to search for duplicate hosts (same address but different name).
 * The list of addresses is allocated dynamically, and remains allocated.
 * This has now been implemented as a hashed list, using the low-order
 * address bits as the hash key.
 */

#ifdef obsolete
ipaddr_t *addrlist = NULL;	/* global list of addresses */
int addrcount = 0;		/* count of global addresses */
#endif

typedef struct addr_data {
	ipaddr_t *addrlist;	/* global list of addresses */
	int addrcount;		/* count of global addresses */
} addr_data_t;

#define AHASHSIZE	0x2000
#define AHASHMASK	0x1fff

addr_data_t hlist[AHASHSIZE];	/* hash list of global addresses */

/*
 * SOA record information.
 */

typedef struct soa_data {
	char pname[MAXDNAME+1];	/* name of primary server */
	char mname[MAXDNAME+1];	/* name of hostmaster mailbox */
	int serial;		/* serial (version) number */
	int refresh;		/* refresh time in seconds */
	int retry;		/* refresh retry time in seconds */
	int expire;		/* expiration time in seconds */
	int defttl;		/* default time_to_live */
} soa_data_t;

soa_data_t soa;			/* buffer to store soa data */

/*
** LIST_ZONE -- Basic routine to do complete zone listing and checking
** -------------------------------------------------------------------
**
**	Returns:
**		TRUE if the requested info was processed successfully.
**		FALSE otherwise.
*/

int total_calls = 0;		/* number of calls for zone processing */
int total_check = 0;		/* number of zones successfully processed */
int total_tries = 0;		/* number of zone transfer attempts */
int total_zones = 0;		/* number of successful zone transfers */
int total_hosts = 0;		/* number of hosts in all traversed zones */
int total_dupls = 0;		/* number of duplicates in all zones */

#ifdef justfun
char longname[MAXDNAME+1];	/* longest hostname found */
int longsize = 0;		/* size of longest hostname */
#endif

int recursion_level = 0;	/* current recursion level */

bool
list_zone(name)
input char *name;			/* name of zone to process */
{
	register int n;
	register int i;
	int nzones;			/* count of delegated zones */
	int nhosts;			/* count of real hostnames */
	int ndupls;			/* count of duplicate hosts */
	int nextrs;			/* count of extrazone hosts */
	int ngates;			/* count of gateway hosts */

	total_calls += 1;		/* update zone processing calls */

/*
 * Normalize to not have trailing dot, unless it is the root zone.
 */
	n = strlength(name);
	if (n > 1 && name[n-1] == '.')
		name[n-1] = '\0';

/*
 * Indicate whether we are processing an "in-addr.arpa" reverse zone.
 * In this case we will suppress accumulating host count statistics.
 */
	reverse = indomain(name, "in-addr.arpa", FALSE);

/*
 * Find the nameservers for the given zone.
 */
	(void) find_servers(name);
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
 * Without an explicit server on the command line, the servers we
 * have looked up are supposed to be authoritative for the zone.
 */
	authserver = server ? FALSE : TRUE;

/*
 * Check SOA records at each of the nameservers.
 * Temporarily save our current server info from the resolver database.
 * Turn off nameserver recursion and make sure answer is authoritative.
 */
	if (checkmode)
	{
		res_state_t save_res;	/* saved copy of resolver database */
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

			if (check_zone(name))
				continue;

			/* SOA query failed */
			ns_error(name, T_SOA, queryclass);

			/* non-authoritative denial: assume lame delegation */
			if (h_errno == NO_RREC || h_errno == NO_HOST)
			{
				if (authserver)
					errmsg("%s has lame delegation to %s",
						name, server);
				continue;
			}

			/* authoritative denial: probably misconfiguration */
			if (h_errno == NO_DATA || h_errno == HOST_NOT_FOUND)
			{
				if (authserver)
					errmsg("%s has lame delegation to %s",
						name, server);
				continue;
			}
		}

		/* restore resolver database */
		_res = save_res;
		server = save_server;

		total_check += 1;	/* update zones processed */

		/* all done if maximum recursion level reached */
		if (!recursive || (recursion_level >= recursive))
			return(Errors == 0 ? TRUE : FALSE);
	}

/*
 * Ask zone transfer to the nameservers, until one responds.
 * If we have queried an authoritative server, it should respond positively.
 * If it responds with an error, we may have a lame delegation.
 * Always continue with the next server to avoid missing entire zones.
 */
	total_tries += 1;		/* update zone transfer attempts */

	for (n = 0; n < nservers; n++)
	{
	    for (i = 0; i < naddrs[n]; i++)
	    {
		if (verbose)
			printf("Trying server %s (%s) ...\n",
				inet_ntoa(ipaddr[n][i]), nsname[n]);

		if (transfer_zone(name, queryclass, ipaddr[n][i], nsname[n]))
			goto done;	/* double break */

		/* zone transfer failed */
		if (h_errno != TRY_AGAIN)
			ns_error(name, T_AXFR, queryclass);

		/* non-authoritative denial: assume lame delegation */
		if (h_errno == NO_RREC || h_errno == NO_HOST)
		{
			if (authserver)
				errmsg("%s has lame delegation to %s",
					name, nsname[n]);
			continue;
		}

		/* authoritative denial: probably misconfiguration */
		if (h_errno == NO_DATA || h_errno == HOST_NOT_FOUND)
		{
			if (authserver)
				errmsg("%s has lame delegation to %s",
					name, nsname[n]);
			continue;
		}

		/* terminate on irrecoverable errors */
		if (h_errno != TRY_AGAIN)
			return(FALSE);
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
		if (h_errno == TRY_AGAIN)
			ns_error(name, T_AXFR, queryclass);
		errmsg("No nameservers for %s responded", name);
		return(FALSE);
	}

	total_zones += 1;		/* update successful zone transfers */

/*
 * Print resource record statistics if so requested.
 */
	if (statistics)
		print_statistics(name, querytype, queryclass);

/*
 * Accumulate host count statistics for this zone.
 */
	nzones = zonecount;

	nhosts = 0, ndupls = 0, nextrs = 0, ngates = 0;

	i = (verbose || statistics || hostmode) ? 0 : hostcount;

	for (n = i; n < hostcount; n++)
	{
		struct in_addr inaddr;

		/* skip fake hosts using a very rudimentary test */
		if (fakename(hostname[n]) || fakeaddr(hostaddr[n]))
			continue;
#ifdef justfun
		/* save longest hostname encountered so far */
		if (strlength(hostname[n]) > longsize)
		{
			longsize = strlength(hostname[n]);
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
		inaddr.s_addr = hostaddr[n];

	/*
	 * Mark hosts not residing directly in the zone as extrazone host.
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
		if (check_dupl(hostaddr[n]))
		{
			ndupls++;
			if (duplmode || (verbose > 1))
				printf("%s is duplicate host with address %s\n",
					hostname[n], inet_ntoa(inaddr));
		}
	}

/*
 * Print statistics for this zone.
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

	total_hosts += nhosts;		/* update total number of hosts */
	total_dupls += ndupls;		/* update total number of duplicates */

	if (!checkmode)
		total_check += 1;	/* update zones processed */

	if (verbose || statistics)
		printf("Found %d delegated zone%s within %s\n",
			nzones, nzones == 1 ? "" : "s", name);

/*
 * The names of the hosts were allocated dynamically.
 */
	for (n = 0; n < hostcount; n++)
		xfree(hostname[n]);

/*
 * Check for mailable delegated zones within this zone.
 * This is based on ordinary MX lookup, and not on the MX info
 * which may be present in the zone listing, to reduce zone transfers.
 */
	if (mxrecmode)
	{
		if (recursion_level == 0)
		{
			if (verbose)
				printf("\n");

			if (!get_mxrec(name))
				ns_error(name, T_MX, queryclass);
		}

		for (n = 0; n < nzones; n++)
		{
			if (verbose)
				printf("\n");

			if (!get_mxrec(zonename[n]))
				ns_error(zonename[n], T_MX, queryclass);
		}
	}

/*
 * Do recursion on delegated zones if requested and any were found.
 * Temporarily save zonename list, and force allocation of new list.
 */
	if (recursive && (recursion_level < recursive))
	{
		for (n = 0; n < nzones; n++)
		{
			char **newzone;		/* local copy of list */

			newzone = zonename;
			zonename = NULL;	/* allocate new list */

			if (verbose || statistics || checkmode || hostmode)
				printf("\n");

			if (verbose)
				printf("Entering zone %s\n", newzone[n]);

			recursion_level++;
			(void) list_zone(newzone[n]);
			recursion_level--;

			zonename = newzone;	/* restore */
		}
	}

/*
 * The names of the delegated zones were allocated dynamically.
 * The list of delegated zone names was also allocated dynamically.
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
		if (verbose || statistics || checkmode || hostmode)
			printf("\n");

		if (verbose || statistics || hostmode)
			printf("Encountered %d host%s in %d zone%s within %s\n",
				total_hosts, total_hosts == 1 ? "" : "s",
				total_zones, total_zones == 1 ? "" : "s",
				name);

		if (verbose || statistics || hostmode)
			printf("Encountered %d duplicate host%s in %d zone%s within %s\n",
				total_dupls, total_dupls == 1 ? "" : "s",
				total_zones, total_zones == 1 ? "" : "s",
				name);

		if (verbose || statistics || checkmode)
			printf("Transferred %d zone%s out of %d attempt%s\n",
				total_zones, total_zones == 1 ? "" : "s",
				total_tries, total_tries == 1 ? "" : "s");

		if (verbose || statistics || checkmode)
			printf("Processed %d zone%s out of %d request%s\n",
				total_check, total_check == 1 ? "" : "s",
				total_calls, total_calls == 1 ? "" : "s");
#ifdef justfun
		if (verbose && (longsize > 0))
			printf("Longest hostname %s\t%d\n",
				longname, longsize);
#endif
	}

	/* indicate whether any errors were encountered */
	return(Errors == 0 ? TRUE : FALSE);
}

/*
** FIND_SERVERS -- Fetch names and addresses of authoritative servers
** ------------------------------------------------------------------
**
**	Returns:
**		TRUE if servers could be determined successfully.
**		FALSE otherwise.
**
**	Inputs:
**		The global variable server, if set, contains the name
**		of the explicit server to be contacted.
**		The global variable primary, if set, indicates that
**		we must use the primary nameserver for the zone.
**
**	Outputs:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
**		The count of nameservers is stored in nservers.
*/

bool
find_servers(name)
input char *name;			/* name of zone to find servers for */
{
	struct hostent *hp;
	register int n;
	register int i;

/*
 * Use the explicit server if given on the command line.
 * Its addresses are stored in the resolver state struct.
 * This server may not be authoritative for the given zone.
 */
	if (server)
	{
		(void) strcpy(nsname[0], server);
		for (i = 0; i < MAXIPADDR && i < _res.nscount; i++)
			ipaddr[0][i] = nslist(i).sin_addr;
		naddrs[0] = i;

		nservers = 1;
		return(TRUE);
	}

/*
 * Fetch primary nameserver info if so requested.
 * Get its name from the SOA record for the zone, and do a regular
 * host lookup to fetch its addresses. We are assuming here that the
 * SOA record is a proper one. This is not necessarily true.
 * Obviously this server should be authoritative.
 */
	if (primary)
	{
		char *primaryname;

		primaryname = get_primary(name);
		if (primaryname == NULL)
		{
			ns_error(name, T_SOA, queryclass);
			nservers = 0;
			return(FALSE);
		}

		hp = gethostbyname(primaryname);
		if (hp == NULL)
		{
			ns_error(primaryname, T_A, C_IN);
			nservers = 0;
			return(FALSE);
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
		return(TRUE);
	}

/*
 * Otherwise we have to find the nameservers for the zone.
 * These are supposed to be authoritative, but sometimes we
 * encounter lame delegations, perhaps due to misconfiguration.
 */
	if (!get_servers(name))
	{
		ns_error(name, T_NS, queryclass);
		nservers = 0;
		return(FALSE);
	}

/*
 * Usually we'll get addresses for all the servers in the additional
 * info section.  But in case we don't, look up their addresses.
 * If we get no addresses by extra query, and this is authoritative,
 * we flag a lame delegation to that server.
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

		if (hp == NULL)
		{
			/* server name lookup failed */
			ns_error(nsname[n], T_A, C_IN);

			/* authoritative denial: probably misconfiguration */
			if (h_errno == NO_DATA || h_errno == HOST_NOT_FOUND)
			{
				errmsg("%s has lame delegation to %s",
					name, nsname[n]);
			}
		}
	    }
	    else
	    {
		if (verbose)
			printf("Found %d address%s for %s\n",
				naddrs[n], naddrs[n] == 1 ? "  " : "es",
				nsname[n]);
	    }
	}

/*
 * Issue warning if only one server has been discovered.
 * This is not an error per se, but not much redundancy in that case.
 */
	if (nservers == 1)
		pr_warning("only one nameserver for %s found", name);

	return(nservers > 0);
}

/*
** GET_SERVERS -- Fetch names and addresses of authoritative servers
** -----------------------------------------------------------------
**
**	Returns:
**		TRUE if servers could be determined successfully.
**		FALSE otherwise.
**
**	Side effects:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
**		The count of nameservers is stored in nservers.
*/

bool
get_servers(name)
input char *name;			/* name of zone to find servers for */
{
	querybuf answer;
	int anslen;
	bool result;			/* result status of action taken */

	if (verbose)
		printf("Finding nameservers for %s ...\n", name);

	anslen = get_info(&answer, name, T_NS, queryclass);
	if (anslen < 0)
		return(FALSE);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, FALSE);

	result = get_nsinfo(&answer, anslen, name);
	return(result);
}

/*
** GET_NSINFO -- Extract nameserver data from nameserver answer buffer
** -------------------------------------------------------------------
**
**	Returns:
**		TRUE if servers could be determined successfully.
**		FALSE otherwise.
**
**	Outputs:
**		Names are stored in the nsname[] database.
**		Addresses are stored in the ipaddr[] database.
**		Address counts are stored in the naddrs[] database.
**		The count of nameservers is stored in nservers.
*/

bool
get_nsinfo(answerbuf, answerlen, name)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* name of zone to find servers for */
{
	HEADER *bp;
	int qdcount, ancount, nscount, arcount;
	int rrcount;
	u_char *msg, *eom;
	register u_char *cp;
	register int i;

	nservers = 0;			/* count of nameservers */

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
		cp = skip_qrec(name, cp, msg, eom);
		if (cp == NULL)
			return(FALSE);
		qdcount--;
	}

	if (qdcount)
	{
		pr_error("invalid qdcount in response");
		h_errno = NO_RECOVERY;
		return(FALSE);
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
		u_char *eor;
		register int n;
		struct in_addr inaddr;

		n = expand_name(name, T_NONE, cp, msg, eom, rname);
		if (n < 0)
			return(FALSE);
		cp += n;

		n = 3*sizeof(u_short) + sizeof(u_int);
		if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
			return(FALSE);

		type = _getshort(cp);
		cp += sizeof(u_short);

		class = _getshort(cp);
		cp += sizeof(u_short);

		ttl = _getlong(cp);
		cp += sizeof(u_int);

		dlen = _getshort(cp);
		cp += sizeof(u_short);

		eor = cp + dlen;
#ifdef lint
		if (verbose)
			printf("%-20s\t%d\t%s\t%s\n",
				rname, ttl, pr_class(class), pr_type(type));
#endif
		if ((type == T_NS) && sameword(rname, name))
		{
			n = expand_name(rname, type, cp, msg, eom, dname);
			if (n < 0)
				return(FALSE);
			cp += n;

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
		else if ((type == T_A) && dlen == sizeof(ipaddr_t))
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

			cp += dlen;
		}
		else
			cp += dlen;

		if (cp != eor)
		{
			pr_error("size error in %s record for %s, off by = %s",
				pr_type(type), rname, itoa(cp - eor));
			return(FALSE);
		}

		rrcount--;
	}

	if (rrcount)
	{
		pr_error("invalid rrcount in response");
		h_errno = NO_RECOVERY;
		return(FALSE);
	}

	return(TRUE);
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
transfer_zone(name, class, inaddr, host)
input char *name;			/* name of zone to do zone xfer for */
input int class;			/* specific resource record class */
input struct in_addr inaddr;		/* address of server to be queried */
input char *host;			/* name of server to be queried */
{
	register int n;

/*
 * Reset the resource record statistics before each try.
 */
	clear_statistics();

/*
 * Perform the actual zone transfer.
 */
	if (get_zone(name, class, inaddr, host))
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
**		Stores list of delegated zones found in zonename[],
**		and the count of delegated zones in zonecount.
**		Stores list of hostnames  found in hostname[],
**		and the count of hostnames in hostcount.
**		Updates resource record statistics in record_stats[].
**		This array must have been cleared before.
*/

bool
get_zone(name, class, inaddr, host)
input char *name;			/* name of zone to do zone xfer for */
input int class;			/* specific resource record class */
input struct in_addr inaddr;		/* address of server to be queried */
input char *host;			/* name of server to be queried */
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

	zonecount = 0;			/* count of delegated zones */
	hostcount = 0;			/* count of hostnames */

/*
 * Construct query, and connect to the given server.
 */
	errno = 0;

	n = res_mkquery(QUERY, name, class, T_AXFR, (char *)NULL, 0,
			(rrec_data_t *)NULL, (char *)&query, sizeof(querybuf));
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
		fp_query((char *)&query, stdout);
	}

	sin.sin_family = AF_INET;
	sin.sin_port = htons(NAMESERVER_PORT);
	sin.sin_addr = inaddr;
	_res_setaddr(&sin, host);

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
	{
		_res_perror("socket");
		h_errno = TRY_AGAIN;
		return(FALSE);
	}

	if (_res_connect(sock, &sin, sizeof(sin)) < 0)
	{
		if (debug || verbose)
			_res_perror("connect");
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
			pr_error("answer length %s too short", itoa(n));
			(void) close(sock);
			h_errno = TRY_AGAIN;
			return(FALSE);
		}

		if (debug > 1)
		{
			printf("got answer:\n");
			fp_query((char *)&answer, stdout);
		}

	/*
	 * Analyze the contents of the answer and check for errors.
	 * An error can be expected only in the very first packet.
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
				h_errno = bp->aa ? HOST_NOT_FOUND : NO_HOST;
				break;

			    case NOERROR:
				/* distinguish between authoritative or not */
				h_errno = bp->aa ? NO_DATA : NO_RREC;
				break;

			    default:
				h_errno = TRY_AGAIN;
				break;
			}

			if (nrecords != 0)
				pr_error("unexpected error in answer");

			(void) close(sock);
			return(FALSE);
		}

		h_errno = 0;

		i = ntohs(bp->nscount);
		if (i != 0)
			pr_error("nonzero nscount in answer");

		i = ntohs(bp->arcount);
		if (i != 0)
			pr_error("nonzero arcount in answer");

	/*
	 * Valid packet received. Print contents if appropriate.
	 */
		nrecords++;
		soaname = NULL;
		subname = NULL;
		adrname = NULL;

		(void) print_info(&answer, n, name, TRUE);

	/*
	 * Terminate upon the second SOA record for this zone.
	 */
		if (soaname && sameword(soaname, name))
			if (soacount++)
				break;

		/* the nameserver balks on this one */
		if (soaname && !sameword(soaname, name))
			pr_warning("extraneous SOA record for %s within %s",
				soaname, name);

	/*
	 * Save encountered delegated zone name for recursive listing.
	 */
		if (subname && indomain(subname, name, FALSE))
		{
			for (i = 0; i < zonecount; i++)
				if (sameword(zonename[i], subname))
					break;	/* duplicate */

			if (i >= zonecount)
			{
				zonename = newlist(zonename, zonecount+1, char *);
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
		/* warn about strange delegated zones */
		if (subname && !indomain(subname, name, TRUE))
			pr_warning("extraneous NS record for %s within %s",
				subname, name);

	/*
	 * Save encountered name of A record for hostname count.
	 */
		if (adrname && indomain(adrname, name, FALSE) && !reverse)
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
					pr_error("maximum number of %s hostnames reached", itoa(hostcount));
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
 * Check for the anomaly that the whole transfer consisted of the
 * SOA records only. Could occur if we queried the victim of a lame
 * delegation which happened to have the SOA record present.
 */
	if (nrecords <= soacount)
	{
		pr_error("empty transfer for %s from %s", name, host);
		h_errno = NO_RREC;
		return(FALSE);
	}

/*
 * Do extra check for hostnames also defined as delegated zones.
 * They may have been defined in the child zone, and crept in
 * the parent zone, or may have been defined as glue records.
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
** GET_MXREC -- Fetch MX records of a domain
** -----------------------------------------
**
**	Returns:
**		TRUE if MX records were found.
**		FALSE otherwise.
*/

bool
get_mxrec(name)
input char *name;			/* domain name to get mx for */
{
	querybuf answer;
	int anslen;

	if (verbose)
		printf("Finding MX records for %s ...\n", name);

	anslen = get_info(&answer, name, T_MX, queryclass);
	if (anslen < 0)
		return(FALSE);

	(void) print_info(&answer, anslen, name, FALSE);
	return(TRUE);
}

/*
** GET_PRIMARY -- Fetch name of primary nameserver for a zone
** ----------------------------------------------------------
**
**	Returns:
**		Pointer to the name of the primary server, if found.
**		NULL if the server could not be determined.
*/

char *
get_primary(name)
input char *name;			/* name of zone to get soa for */
{
	querybuf answer;
	int anslen;

	if (verbose)
		printf("Finding primary nameserver for %s ...\n", name);

	anslen = get_info(&answer, name, T_SOA, queryclass);
	if (anslen < 0)
		return(NULL);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, FALSE);

	soaname = NULL;
	(void) get_soainfo(&answer, anslen, name);
	if (soaname == NULL)
		return(NULL);

	return(soa.pname);
}

/*
** CHECK_ZONE -- Fetch and analyze SOA record of a zone
** ----------------------------------------------------
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
check_zone(name)
input char *name;			/* name of zone to get soa for */
{
	querybuf answer;
	int anslen;

	if (verbose)
		printf("Checking SOA for %s at server %s\n", name, server);
	else if (authserver)
		printf("%-20s\tNS\t%s\n", name, server);
	else
		printf("%s\t(%s)\n", name, server);

	anslen = get_info(&answer, name, T_SOA, queryclass);
	if (anslen < 0)
		return(FALSE);

	if (verbose > 1)
		(void) print_info(&answer, anslen, name, FALSE);

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
**		TRUE if the SOA record was found successfully.
**		FALSE otherwise.
**
**	Outputs:
**		The global struct soa is filled with the soa data.
**
**	Side effects:
**		Sets soaname if this is a valid SOA record.
**		This variable must have been cleared before calling
**		get_soainfo() and may be checked afterwards.
*/

bool
get_soainfo(answerbuf, answerlen, name)
input querybuf *answerbuf;		/* address of answer buffer */
input int answerlen;			/* length of answer buffer */
input char *name;			/* name of zone to get soa for */
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
		cp = skip_qrec(name, cp, msg, eom);
		if (cp == NULL)
			return(FALSE);
		qdcount--;
	}

	if (qdcount)
	{
		pr_error("invalid qdcount in response");
		h_errno = NO_RECOVERY;
		return(FALSE);
	}

/*
 * Check answer section only.
 * The nameserver section may contain the nameservers for the zone,
 * and the additional section their addresses, but not guaranteed.
 */
	while (ancount > 0 && cp < eom)
	{
		char rname[MAXDNAME+1];
		int type, class, ttl, dlen;
		u_char *eor;
		register int n;

		n = expand_name(name, T_NONE, cp, msg, eom, rname);
		if (n < 0)
			return(FALSE);
		cp += n;

		n = 3*sizeof(u_short) + sizeof(u_int);
		if (check_size(rname, T_NONE, cp, msg, eom, n) < 0)
			return(FALSE);

		type = _getshort(cp);
		cp += sizeof(u_short);

		class = _getshort(cp);
		cp += sizeof(u_short);

		ttl = _getlong(cp);
		cp += sizeof(u_int);

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
			n = expand_name(rname, type, cp, msg, eom, soa.pname);
			if (n < 0)
				return(FALSE);
			cp += n;

			n = expand_name(rname, type, cp, msg, eom, soa.mname);
			if (n < 0)
				return(FALSE);
			cp += n;

			n = 5*sizeof(u_int);
			if (check_size(rname, type, cp, msg, eor, n) < 0)
				return(FALSE);
			soa.serial = _getlong(cp);
			cp += sizeof(u_int);
			soa.refresh = _getlong(cp);
			cp += sizeof(u_int);
			soa.retry = _getlong(cp);
			cp += sizeof(u_int);
			soa.expire = _getlong(cp);
			cp += sizeof(u_int);
			soa.defttl = _getlong(cp);
			cp += sizeof(u_int);

			/* valid complete soa record found */
			soaname = strcpy(soanamebuf, rname);
			break;

		    default:
			cp += dlen;
			break;
		}

		if (cp != eor)
		{
			pr_error("size error in %s record for %s, off by = %s",
				pr_type(type), rname, itoa(cp - eor));
			return(FALSE);
		}

		ancount--;
	}

	if (ancount)
	{
		pr_error("invalid ancount in response");
		h_errno = NO_RECOVERY;
		return(FALSE);
	}

	return(TRUE);
}

/*
** CHECK_SOA -- Analyze retrieved SOA records of a zone
** ----------------------------------------------------
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
input char *name;			/* name of zone to check soa for */
{
	static char oldnamebuf[MAXDNAME+1];
	static char *oldname = NULL;	/* previous name of zone */
	static char *oldserver = NULL;	/* previous name of server */
	static soa_data_t oldsoa;	/* previous soa data */
	register int n;
	HEADER *bp;

/*
 * Print the various SOA fields in abbreviated form.
 * Values are actually unsigned, but we print them as signed integers.
 */
	printf("%s\t%s\t(%d %d %d %d %d)\n", soa.pname, soa.mname,
		soa.serial, soa.refresh, soa.retry, soa.expire, soa.defttl);

/*
 * We are supposed to have queried an authoritative nameserver, and since
 * nameserver recursion has been turned off, answer must be authoritative.
 */
	bp = (HEADER *)answerbuf;
	if (!bp->aa)
	{
		if (authserver)
			pr_error("SOA record for %s at %s is not authoritative",
				name, server);
		else
			pr_warning("SOA record for %s at %s is not authoritative",
				name, server);

		if (authserver)
			errmsg("%s has lame delegation to %s",
				name, server);
	}

/*
 * Check whether we are switching to a new zone.
 * The old name must have been saved in static storage.
 */
	if (oldname != NULL && !sameword(name, oldname))
		oldname = NULL;

/*
 * Make few timer consistency checks only for the first one in a series.
 * Compare the primary field against the list of authoritative servers.
 * Explicitly check the hostmaster field for illegal characters ('@').
 */
	if (oldname == NULL)
	{
		for (n = 0; n < nservers; n++)
			if (sameword(soa.pname, nsname[n]))
				break;	/* found */

		if (n >= nservers && authserver)
			pr_warning("SOA for %s has extraneous primary", name);

		if (!valid_name(soa.mname, FALSE))
			pr_warning("SOA for %s has illegal hostmaster", name);

		if (soa.serial < 0)
			pr_warning("SOA for %s has extraneous serial", name);

		if (soa.retry > soa.refresh)
			pr_warning("SOA for %s has retry exceeding refresh", name);

		if (soa.refresh + soa.retry > soa.expire)
			pr_warning("SOA for %s has refresh+retry exceeding expire", name);
	}

/*
 * Compare various fields with those of the previous query, if any.
 * Different serial numbers may be present if secondaries have not yet
 * refreshed the data from the primary. Issue only a warning in that case.
 */
	if (oldname != NULL)
	{
		if (!sameword(soa.pname, oldsoa.pname))
			pr_error("%s has different primary than %s",
				server, oldserver);

		if (!sameword(soa.mname, oldsoa.mname))
			pr_error("%s has different hostmaster than %s",
				server, oldserver);

		if (soa.serial != oldsoa.serial)
			pr_warning("%s has different serial than %s",
				server, oldserver);

		if (soa.refresh != oldsoa.refresh)
			pr_error("%s has different refresh than %s",
				server, oldserver);

		if (soa.retry != oldsoa.retry)
			pr_error("%s has different retry than %s",
				server, oldserver);

		if (soa.expire != oldsoa.expire)
			pr_error("%s has different expire than %s",
				server, oldserver);

		if (soa.defttl != oldsoa.defttl)
			pr_error("%s has different defttl than %s",
				server, oldserver);
	}

/*
 * Save the current information.
 */
	oldname = strcpy(oldnamebuf, name);
	oldserver = server;
	oldsoa = soa;
}

/*
** CHECK_DUPL -- Check global address list for duplicates
** ------------------------------------------------------
**
**	Returns:
**		TRUE if the given host address already exists.
**		FALSE otherwise.
**
**	Side effects:
**		Adds the host address to the list if not present.
*/

bool
check_dupl(addr)
input ipaddr_t addr;			/* address of host to check */
{
 	register int i;
	register addr_data_t *h;

	h = &hlist[ntohl(addr) & AHASHMASK];

	for (i = 0; i < h->addrcount; i++)
		if (h->addrlist[i] == addr)
			return(TRUE);	/* duplicate */

	h->addrlist = newlist(h->addrlist, h->addrcount+1, ipaddr_t);
	h->addrlist[h->addrcount] = addr;
	h->addrcount++;
	return(FALSE);
}


#ifdef obsolete

#define NETWORK_MASK	((ipaddr_t)0xffff0000)
#define nethash(a)	((a) & htonl(NETWORK_MASK))

bool
check_dupl(addr)
input ipaddr_t addr;			/* address of host to check */
{
	ipaddr_t network;
	register int n;
 	register int i;
	register net_data_t *h;

/*
 * Extract the (peudo) network part from the address.
 */
	network = nethash(addr);

/*
 * Check whether we already have a list for this network.
 * If not, allocate a new empty address list.
 */
	for (n = 0; n < netcount; n++)
		if (netlist[n].network == network)
			break;  /* network is known */
 
	if (n >= netcount)
	{
		netlist = newlist(netlist, netcount+1, net_data_t);
		netlist[netcount].network = network;
		netlist[netcount].addrlist = NULL;
		netlist[netcount].addrcount = 0;
		netcount++;
	}

	/* the hash list for this network */
	h = &netlist[n];

/*
 * Check whether the address exists on the list for that network.
 * If not, add it to the address list.
 */
	for (i = 0; i < h->addrcount; i++)
		if (h->addrlist[i] == addr)
			return(TRUE);	/* duplicate */

	h->addrlist = newlist(h->addrlist, h->addrcount+1, ipaddr_t);
	h->addrlist[h->addrcount] = addr;
	h->addrcount++;
	return(FALSE);
}


bool
check_dupl(addr)
input ipaddr_t addr;			/* address of host to check */
{
	register int i;

	for (i = 0; i < addrcount; i++)
		if (addrlist[i] == addr)
			return(TRUE);	/* duplicate */

	addrlist = newlist(addrlist, addrcount+1, ipaddr_t);
	addrlist[addrcount] = addr;
	addrcount++;
	return(FALSE);
}

#endif

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
		ns_error(name, T_A, C_IN);
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
		ipaddr_t addr = inaddr[i].s_addr;

		if (verbose)
			printf("Checking %s address %s\n", hname, iname);

		hp = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
		if (hp == NULL)
			ns_error(iname, T_PTR, C_IN);
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
input ipaddr_t addr;			/* address of host to check */
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
		ns_error(iname, T_PTR, C_IN);
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
		ns_error(hname, T_A, C_IN);
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
**
**	Note.	C_CSNET is obsolete, but recognized.
*/

int
parse_class(str)
input char *str;			/* input string with resource class */
{
	register int class;

	if (sameword(str, "IN"))	return(C_IN);
	if (sameword(str, "INTERNET"))	return(C_IN);
	if (sameword(str, "CS"))	return(C_CSNET);	/* obsolete */
	if (sameword(str, "CSNET"))	return(C_CSNET);	/* obsolete */
	if (sameword(str, "CH"))	return(C_CHAOS);
	if (sameword(str, "CHAOS"))	return(C_CHAOS);
	if (sameword(str, "HS"))	return(C_HS);
	if (sameword(str, "HESIOD"))	return(C_HS);

	if (sameword(str, "ANY"))	return(C_ANY);
	if (sameword(str, "*"))		return(C_ANY);

	class = atoi(str);
	if (class > 0)
		return(class);

	return(-1);
}

/*
** IN_ADDR_ARPA -- Convert dotted quad string to reverse in-addr.arpa
** ------------------------------------------------------------------
**
**	Returns:
**		Pointer to reverse in-addr.arpa. zone name
**		with trailing dot to force absolute domain name.
**		NULL in case of invalid dotted quad input string.
*/

char *
in_addr_arpa(dottedquad)
input char *dottedquad;			/* input string with dotted quad */
{
	static char addrbuf[32];
	unsigned int a[4];
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
** SHOW_RES -- Show resolver database information
** ----------------------------------------------
**
**	Returns:
**		None.
**
**	Inputs:
**		The resolver database _res is localized in the resolver.
*/

void
show_res()
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
**		The record_stats[] counts have been updated by print_rrec().
*/

void
print_statistics(name, filter, class)
input char *name;			/* name of zone we are listing */
input int filter;			/* type of records we want to see */
input int class;			/* class of records we want to see */
{
	register int type;
	int nrecords;

	for (type = T_FIRST; type <= T_LAST; type++)
	{
		nrecords = record_stats[type];
		if (nrecords > 0 || (filter != T_ANY && want_type(type, filter)))
		{
			printf("Found %4d %-5s record%s", nrecords,
				pr_type(type), nrecords == 1 ? " " : "s");
			if (class != C_IN)
				printf(" in class %s", pr_class(class));
			printf(" within %s\n", name);
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
** SHOW_TYPES -- Show resource record types wanted
** -----------------------------------------------
**
**	Returns:
**		None.
*/

void
show_types(name, filter, class)
input char *name;			/* name we want to query about */
input int filter;			/* type of records we want to see */
input int class;			/* class of records we want to see */
{
	register int type;

	if (filter >= T_NONE)
	{
		printf("Query about %s for record types", name);
		if (filter == T_ANY)
			printf(" %s", pr_type(T_ANY));
		else
			for (type = T_FIRST; type <= T_LAST; type++)
				if (want_type(type, filter))
					printf(" %s", pr_type(type));
		if (class != C_IN)
			printf(" in class %s", pr_class(class));
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
ns_error(name, type, class) 
input char *name;			/* full name we queried about */
input int type;				/* record type we queried about */
input int class;			/* record class we queried about */
{
	static char *auth = "Authoritative answer";

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
		 * The contacted server did not give any reply at all
		 * within the specified time frame.
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
		 * Nameserver status: NXDOMAIN
		 */
		if (class != C_IN)
			errmsg("%s does not exist in class %s (%s)",
				name, pr_class(class), auth);
		else
			errmsg("%s does not exist (%s)",
				name, auth);
		break;

	    case NO_HOST:
		/*
		 * The specified name does not exist, but the answer
		 * was not authoritative, so it is still undecided.
		 * Happens when querying unknown name for class C_ANY.
		 * Nameserver status: NXDOMAIN
		 */
		if (class != C_IN)
			errmsg("%s does not exist in class %s, try again",
				name, pr_class(class));
		else
			errmsg("%s does not exist, try again",
				name);
		break;

	    case TRY_AGAIN:
		/*
		 * Some intermediate server failure, e.g. timeout, or when
		 * the server is not authoritative for a specific class.
		 * Nameserver status: SERVFAIL
		 */
		if (class != C_IN)
			errmsg("%s %s record in class %s not found, try again",
				name, pr_type(type), pr_class(class));
		else
			errmsg("%s %s record not found, try again",
				name, pr_type(type));
		break;

	    case NO_RECOVERY:
		/*
		 * Some irrecoverable format error, or server refusal.
		 * Nameserver status: FORMERR NOTIMP REFUSED NOCHANGE
		 */
		if (class != C_IN)
			errmsg("%s %s record in class %s not found, no recovery",
				name, pr_type(type), pr_class(class));
		else
			errmsg("%s %s record not found, no recovery",
				name, pr_type(type));
		break;

	    case NO_DATA:
		/*
		 * The name is valid, but the specified type does not exist.
		 * This status is here returned only in case authoritative.
		 * Nameserver status: NOERROR
		 */
		if (class != C_IN)
			errmsg("%s has no %s record in class %s (%s)",
				name, pr_type(type), pr_class(class), auth);
		else
			errmsg("%s has no %s record (%s)",
				name, pr_type(type), auth);
		break;

	    case NO_RREC:
		/*
		 * The specified type does not exist, but we don't know whether
		 * the name is valid or not. The answer was not authoritative.
		 * Perhaps recursion was off, and no data was cached locally.
		 * Nameserver status: NOERROR
		 */
		if (class != C_IN)
			errmsg("%s %s record in class %s currently not present",
				name, pr_type(type), pr_class(class));
		else
			errmsg("%s %s record currently not present",
				name, pr_type(type));
		break;

	    default:
		/*
		 * Unknown cause for server failure.
		 */
		if (class != C_IN)
			errmsg("%s %s record in class %s not found",
				name, pr_type(type), pr_class(class));
		else
			errmsg("%s %s record not found",
				name, pr_type(type));
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
**
**	Side effects:
**		Increments the global error count.
*/

void /*VARARGS1*/
pr_error(fmt, a, b, c, d)
input char *fmt;			/* format of message */
input char *a, *b, *c, *d;		/* optional arguments */
{
	(void) fprintf(stderr, " *** ");
	(void) fprintf(stderr, fmt, a, b, c, d);
	(void) fprintf(stderr, "\n");

	/* flag an error */
	Errors++;
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

void /*VARARGS1*/
pr_warning(fmt, a, b, c, d)
input char *fmt;			/* format of message */
input char *a, *b, *c, *d;		/* optional arguments */
{
	if (!quiet)
	{
		(void) fprintf(stderr, " !!! ");
		(void) fprintf(stderr, fmt, a, b, c, d);
		(void) fprintf(stderr, "\n");
	}
}

/*
** WANT_TYPE -- Indicate whether the rr type matches the desired filter
** --------------------------------------------------------------------
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
want_type(type, filter)
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
** WANT_CLASS -- Indicate whether the rr class matches the desired filter
** ----------------------------------------------------------------------
**
**	Returns:
**		TRUE if the resource record class matches the filter.
**		FALSE otherwise.
**
**	In regular mode, the queryclass is used to formulate the query,
**	and the filter is set to C_ANY to filter out any response.
**	In listmode, we get everything, so the filter is set to the
**	queryclass to filter out the proper responses.
**	Note that C_IN is the default queryclass in listmode.
*/

bool
want_class(class, filter)
input int class;			/* resource record class */
input int filter;			/* class of records we want to see */
{
	if (class == filter)
		return(TRUE);

	if (filter == C_ANY)
		return(TRUE);

	return(FALSE);
}

/*
** INDOMAIN -- Check whether a name belongs to a zone
** --------------------------------------------------
**
**	Returns:
**		TRUE if the given name lies anywhere in the zone, or
**		if the given name is the same as the zone and may be so.
**		FALSE otherwise.
*/

bool
indomain(name, domain, equal)
input char *name;			/* the name under consideration */
input char *domain;			/* the name of the zone */
input bool equal;			/* set if name may be same as zone */
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
** SAMEDOMAIN -- Check whether a name belongs to a zone
** ----------------------------------------------------
**
**	Returns:
**		TRUE if the given name lies directly in the zone, or
**		if the given name is the same as the zone and may be so.
**		FALSE otherwise.
*/

bool
samedomain(name, domain, equal)
input char *name;			/* the name under consideration */
input char *domain;			/* the name of the zone */
input bool equal;			/* set if name may be same as zone */
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
**	If it lies directly in the given zone, it is considered
**	an ordinary host within that zone, and not a glue record.
**	If it does not belong to the given dzone at all, is it
**	here considered to be a glue record.
**	If it lies in the given zone, but not directly, it is
**	considered a glue record if it belongs to any of the known
**	delegated zones of the given zone.
**	In the root zone itself are no hosts, only glue records.
*/

bool
gluerecord(name, domain, zone, nzones)
input char *name;			/* the name under consideration */
input char *domain;			/* name of zone being processed */
input char *zone[];			/* list of known delegated zones */
input int nzones;			/* number of known delegated zones */
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
** PR_DOTNAME -- Return domain name with trailing dot
** --------------------------------------------------
**
**	Returns:
**		Pointer to new domain name.
*/

char *
pr_dotname(name)
input char *name;			/* domain name to append to */
{
	static char buf[MAXDNAME+2];	/* buffer to store new domain name */
	register int n;

	/* return original if trailing dot present */
	n = strlength(name);
	if (n > 0 && name[n-1] == '.')
		return(name);

	/* construct name with trailing dot */
	(void) sprintf(buf, "%.*s.", MAXDNAME, name);
	return(buf);
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
	static char buf[20];

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

	(void) sprintf(buf, "%d", type);
	return(buf);
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
	static char buf[20];

	switch (class)
	{
	    case C_IN:      return("IN");	/* internet */
	    case C_CSNET:   return("CS");	/* csnet */
	    case C_CHAOS:   return("CH");	/* chaosnet */
	    case C_HS:      return("HS");	/* hesiod */
	    case C_ANY:     return("ANY");	/* any class */
	}

	(void) sprintf(buf, "%d", class);
	return(buf);
}

/*
** EXPAND_NAME -- Expand compressed domain name in a recource record
** -----------------------------------------------------------------
**
**	Returns:
**		Number of bytes advanced in answer buffer.
**		-1 if there was a format error.
*/

int
expand_name(name, type, cp, msg, eom, namebuf)
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
		pr_error("expand error in %s record for %s, offset = %s",
			pr_type(type), name, itoa(cp - msg));
		h_errno = NO_RECOVERY;
		return(-1);
	}

	/* change root to single dot */
	if (namebuf[0] == '\0')
	{
		namebuf[0] = '.';
		namebuf[1] = '\0';
	}

#ifdef notyet
	if ((type != T_NONE) && illegal && !valid_name(namebuf, FALSE))
	{
		pr_warning("illegal name %s in %s record for %s",
			namebuf, pr_type(type), name);
	}
#endif
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
			pr_error("incomplete %s record for %s, offset = %s",
				pr_type(type), name, itoa(cp - msg));
		else
			pr_warning("incomplete %s record for %s, offset = %s",
				pr_type(type), name, itoa(cp - msg));
		h_errno = NO_RECOVERY;
		return(-1);
	}

	return(size);
}

/*
** VALID_NAME -- Check whether domain name contains invalid characters
** -------------------------------------------------------------------
**
**	Returns:
**		TRUE if the name is valid.
**		FALSE otherwise.
**
**	The total size of a compound name should not exceed MAXDNAME.
**	We assume that this is true. Its individual components between
**	dots should not be longer than 64. This is not checked here.
**	Only alphanumeric characters and dash '-' may be used (dash
**	only in the middle). We only check the individual characters.
**
**	The label '*' can in principle be used anywhere to indicate
**	wildcarding. It is valid only in the LHS resource record name.
**	In definitions in zone files only as the first component.
**	Used primarily in wildcard MX record definitions.
*/

bool
valid_name(name, wildcard)
input char *name;			/* domain name to check */
input bool wildcard;			/* set if wildcard is allowed */
{
	register char *p;

	for (p = name; *p != '\0'; p++)
	{
		if (is_alnum(*p) || (*p == '-'))
			continue;

		/* start of a new component */
		if (*p == '.')
			continue;

		/* allow '*' for use in wildcard names */
		if ((*p == '*') && wildcard)
			continue;

		/* silently allowed widespread exceptions */
		if (illegal && index(illegal, *p) != NULL)
			continue;

		return(FALSE);
	}
	return(TRUE);
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
input siz_t size;			/* number of bytes to allocate */
{
	if (buf == NULL)
		buf = malloc(size);
	else
		buf = realloc(buf, size);

	if (buf == NULL)
	{
		errmsg("Out of memory");
		exit(EX_OSERR);
	}

	return(buf);
}

/*
** ITOA -- Convert integer value to ascii string
** ---------------------------------------------
**
**	Returns:
**		Pointer to string.
*/

char *
itoa(n)
input int n;				/* value to convert */
{
	static char buf[20];

	(void) sprintf(buf, "%d", n);
	return(buf);
}


/*
** STOA -- Extract partial ascii string
** ------------------------------------
**
**	Returns:
**		Pointer to string.
*/

char *
stoa(str, n)
input u_char *str;			/* input string to extract from */
input int n;				/* number of characters to extract */
{
	static char buf[MAXPACKET+1];

	if (n > MAXPACKET)
		n = MAXPACKET;

	if (n > 0)
		(void) sprintf(buf, "%.*s", n, str);
	else
		(void) sprintf(buf, "%s", "");
	return(buf);
}
