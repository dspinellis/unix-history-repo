/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)res.h	5.6 (Berkeley) 7/23/88
 */

/*
 *******************************************************************************
 *
 *  res.h --
 *
 *	Definitions used by modules of the name server lookup program.
 *
 *	Copyright (c) 1985 
 *  	Andrew Cherenson
 *	U.C. Berkeley
 *  	CS298-26  Fall 1985
 *  
 *******************************************************************************
 */

#define TRUE	1
#define FALSE	0
typedef int Boolean;

/*
 *  Define return statuses in addtion to the ones defined in namserv.h
 *   let SUCCESS be a synonym for NOERROR
 *
 *	TIME_OUT	- a socket connection timed out.
 *	NO_INFO		- the server didn't find any info about the host.
 *	ERROR		- one of the following types of errors:
 *			   dn_expand, res_mkquery failed
 *			   bad command line, socket operation failed, etc.
 *	NONAUTH		- the server didn't have the desired info but
 *			  returned the name(s) of some servers who should.
 *
 */

#define  SUCCESS		0
#define  TIME_OUT		-1
#define  NO_INFO 		-2
#define  ERROR 			-3
#define  NONAUTH 		-4

/*
 *  Define additional options for the resolver state structure.
 *
 *   RES_DEBUG2		more verbose debug level 
 */

#define RES_DEBUG2	0x80000000

/*
 *  Maximum length of server, host and file names.
 */

#define NAME_LEN 80


/*
 * Modified struct hostent from <netdb.h>
 *
 * "Structures returned by network data base library.  All addresses
 * are supplied in host order, and returned in network order (suitable
 * for use in system calls)."
 */

typedef struct	{
	char	*name;		/* official name of host */
	char	**domains;	/* domains it serves */
	char	**addrList;	/* list of addresses from name server */
} ServerInfo;

typedef struct	{
	char	*name;		/* official name of host */
	char	**aliases;	/* alias list */
	char	**addrList;	/* list of addresses from name server */
	int	addrType;	/* host address type */
	int	addrLen;	/* length of address */
	ServerInfo **servers;
} HostInfo;


/*
 *  SockFD is the file descriptor for sockets used to connect with
 *  the name servers. It is global so the Control-C handler can close
 *  it. Likewise for filePtr, which is used for directing listings
 *  to a file.
 */

extern int sockFD;
extern FILE *filePtr;


/*
 *  External routines:
 */

extern int   Print_query();
extern char *Print_cdname();
extern char *Print_cdname2();	/* fixed width */
extern char *Print_rr();
extern char *DecodeType();	/* descriptive version of p_type */
extern char *DecodeError();
extern char *Calloc();
extern char *Malloc();
extern void NsError();
extern void PrintServer();
extern void PrintHostInfo();
extern void ShowOptions();
extern void FreeHostInfoPtr();
extern FILE *OpenFile();
extern char *inet_ntoa();
extern char *res_skip();
