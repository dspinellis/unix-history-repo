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
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.23 (Berkeley) 11/17/88";
#endif /* not lint */

/*
 *******************************************************************************
 *  
 *   main.c --
 *  
 *  	Main routine and some action routines for the name server
 *	lookup program.
 *
 *  	Andrew Cherenson 	
 *	U.C. Berkeley Computer Science Div.
 *	CS298-26, Fall 1985
 *  
 *******************************************************************************
 */

#include <stdio.h>
#include <strings.h>
#include <sys/param.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include "res.h"

/*
 *  Location of the help file.
 */

#ifndef HELPFILE
#define HELPFILE "/usr/local/nslookup.help"
#endif


/*
 *  Default Internet address of the current host.
 */

#if BSD < 43
#define LOCALHOST "127.0.0.1"
#endif


/*
 * Name of a top-level name server. Can be changed with 
 * the "set root" command.
 */

#ifndef ROOT_SERVER
#define		ROOT_SERVER "sri-nic.arpa."
#endif
char 		rootServerName[NAME_LEN];


/*
 *  Import the state information from the resolver library.
 */

extern struct state _res;


/*
 *  Info about the most recently queried host.
 */

HostInfo	curHostInfo;
int		curHostValid = FALSE;


/*
 *  Info about the default name server.
 */

HostInfo 	*defaultPtr = NULL;
char 		defaultServer[NAME_LEN];
struct in_addr	defaultAddr;


/*
 *  Initial name server query type is Address.
 */

int 		queryType = T_A;
int 		queryClass = C_IN;

/*
 * Stuff for Interrupt (control-C) signal handler.
 *  SockFD is the file descriptor for sockets used to
 *  connect with the name servers. It has to be global to
 *  allow the interrupt handler can close open sockets.
 */

extern int 	IntrHandler();
int 		sockFD = -1;
FILE 		*filePtr;
jmp_buf 	env;

static void CvtAddrToPtr();


/*
 *******************************************************************************
 *
 *  main --
 *
 *	Initializes the resolver library and determines the address
 *	of the initial name server. The yylex routine is used to
 *	read and perform commands.
 *
 *******************************************************************************
 */

main(argc, argv)
    int		argc;
    char	**argv;
{
    char	*wantedHost = NULL;
    Boolean	useLocalServer;
    int 	result;
    int		i;
    struct hostent	*hp;
    extern int	h_errno;

    /*
     *  Initialize the resolver library routines.
     */

    if (res_init() == -1) {
	fprintf(stderr,"*** Can't initialize resolver.\n");
	exit(1);
    }

    /*
     *  Allocate space for the default server's host info and
     *  find the server's address and name. If the resolver library
     *  already has some addresses for a potential name server,
     *  then use them. Otherwise, see if the current host has a server.
     *  Command line arguments may override the choice of initial server. 
     */

    defaultPtr = (HostInfo *) Calloc(1, sizeof(HostInfo));

    /*
     * Parse the arguments:
     *  no args =  go into interactive mode, use default host as server
     *	1 arg	=  use as host name to be looked up, default host will be server
     *		   non-interactive mode
     *  2 args	=  1st arg: 
     *		     if it is '-', then 
     *		        ignore but go into interactive mode
     *	 	     else 
     *		         use as host name to be looked up, 
     *			 go into non-interactive mode
     *  	   2nd arg: name or inet address of server
     *
     *	"Set" options are specified with a leading - and must come before
     *	any arguments. For example, to find the well-known services for
     *  a host, type "nslookup -query=wks host"
     */

    ++argv; --argc;		/* skip prog name */

    while (argc && *argv[0] == '-' && argv[0][1]) {
	(void) SetOption (&(argv[0][1]));
	++argv; --argc;
    }
    if (argc > 2) {
	Usage();
    } 
    if (argc && *argv[0] != '-') {
	wantedHost = *argv;	/* name of host to be looked up */
    }

    useLocalServer = FALSE;
    if (argc == 2) {

	/*
	 *	Set explicit name server address.
	 */ 

	_res.nscount = 1;
	_res.nsaddr.sin_addr.s_addr = inet_addr(*++argv);
	if (_res.nsaddr.sin_addr.s_addr == (unsigned)-1) {
	    hp = gethostbyname(*argv);
	    if (hp == NULL) {
		fprintf(stderr, "nslookup: %s: ", *argv);
		herror((char *)NULL);
		_res.nscount = 0;
		useLocalServer = TRUE;
	    } else {
#if BSD < 43
		bcopy(hp->h_addr, &_res.nsaddr.sin_addr, hp->h_length);
#else
		bcopy(hp->h_addr_list[0], &_res.nsaddr.sin_addr, hp->h_length);
#endif
	    } 
	}
    }


    if (_res.nscount == 0 || useLocalServer) {
	LocalServer(defaultPtr);
    } else {
	for (i = 0; i < _res.nscount; i++) {
	    if (_res.nsaddr_list[i].sin_addr.s_addr == INADDR_ANY) {
	        LocalServer(defaultPtr);
		break;
	    } else {
		result = FindHostInfo(&(_res.nsaddr_list[i].sin_addr), 
				    &(_res.nsaddr_list[i].sin_addr), 
				    sizeof(struct in_addr),
				    defaultPtr);
		if (result != SUCCESS) {
		    fprintf(stderr,
		    "*** Can't find server name for address %s: %s\n", 
		       inet_ntoa(_res.nsaddr_list[i].sin_addr), 
		       DecodeError(result));
		} else {
		    defaultAddr = _res.nsaddr_list[i].sin_addr;
		    break;
		}
	    }
	}

	/*
	 *  If we have exhausted the list, tell the user about the
	 *  command line argument to specify an address.
	 */

	if (i == _res.nscount) {
	    fprintf(stderr, 
	    "*** Default servers are not available\n");
	    exit(1);
	}

    }
    strcpy(defaultServer, defaultPtr->name);
    strcpy(rootServerName, ROOT_SERVER);


#ifdef DEBUG
#ifdef DEBUG2
    _res.options |= RES_DEBUG2;
#endif
    _res.options |= RES_DEBUG;
    _res.retry    = 2;
#endif DEBUG

    /*
     * If we're in non-interactive mode, look up the wanted host and quit.
     * Otherwise, print the initial server's name and continue with
     * the initialization.
     */

    if (wantedHost != (char *) NULL) {
	LookupHost(wantedHost, 0);
    } else {
	PrintHostInfo(stdout, "Default Server:", defaultPtr);

	/*
	 * Setup the environment to allow the interrupt handler to return here.
	 */

	(void) setjmp(env);

	/* 
	 * Return here after a longjmp.
	 */

	signal(SIGINT, IntrHandler);
	signal(SIGPIPE, SIG_IGN);

	/*
	 * Read and evaluate commands. The commands are described in commands.l
	 * Yylex returns 0 when ^D or 'exit' is typed. 
	 */

	printf("> ");
	while(yylex()) {
	    printf("> ");
	}
    }
    exit(0);
}


LocalServer(defaultPtr)
    HostInfo *defaultPtr;
{
    char	hostName[NAME_LEN];
    int		result;

    gethostname(hostName, sizeof(hostName));

#if BSD < 43
    defaultAddr.s_addr = inet_addr(LOCALHOST);
    result = GetHostInfo(&defaultAddr, C_IN, T_A, hostName, defaultPtr, 1);
    if (result != SUCCESS) {
	fprintf(stderr,
	"*** Can't find initialize address for server %s: %s\n",
			defaultServer, DecodeError(result));
	exit(1);
    }
#else
    defaultAddr.s_addr = htonl(INADDR_ANY);
    (void) GetHostInfo(&defaultAddr, C_IN, T_A, "0.0.0.0", defaultPtr, 1);
    free(defaultPtr->name);
    defaultPtr->name = Calloc(1, sizeof(hostName)+1);
    strcpy(defaultPtr->name, hostName);
#endif
}


/*
 *******************************************************************************
 *
 *  Usage --
 *
 *	Lists the proper methods to run the program and exits.
 *
 *******************************************************************************
 */

Usage()
{
    fprintf(stderr, "Usage:\n");
    fprintf(stderr,
"   nslookup [-opt ...]             # interactive mode using default server\n");
    fprintf(stderr,
"   nslookup [-opt ...] - server    # interactive mode using 'server'\n");
    fprintf(stderr,
"   nslookup [-opt ...] host        # just look up 'host' using default server\n");
    fprintf(stderr,
"   nslookup [-opt ...] host server # just look up 'host' using 'server'\n");
    exit(1);
}


/*
 *******************************************************************************
 *
 *  SetDefaultServer --
 *
 *	Changes the default name server to the one specified by
 *	the first argument. The command "server name" uses the current 
 *	default server to lookup the info for "name". The command
 *	"lserver name" uses the original server to lookup "name".
 *
 *  Side effects:
 *	This routine will cause a core dump if the allocation requests fail.
 *
 *  Results:
 *	SUCCESS 	The default server was changed successfully.
 *	NONAUTH		The server was changed but addresses of
 *			other servers who know about the requested server
 *			were returned.
 *	Errors		No info about the new server was found or
 *			requests to the current server timed-out.
 *
 *******************************************************************************
 */

int
SetDefaultServer(string, local)
    char	*string;
    Boolean	local;
{
    register HostInfo 	*newDefPtr;
    char 		newServer[NAME_LEN];
    int 		result;
    int 		i;

    /*
     *  Parse the command line. It maybe of the form "server name",
     *  "lserver name" or just "name".
     */

    if (local) {
	i = sscanf(string, " lserver %s", newServer);
    } else {
	i = sscanf(string, " server %s", newServer);
    }
    if (i != 1) {
	i = sscanf(string, " %s", newServer);
	if (i != 1) {
	    fprintf(stderr,"SetDefaultServer: invalid name: %s\n",  string);
	    return(ERROR);
	}
    }

    /*
     * Allocate space for a HostInfo variable for the new server. Don't
     * overwrite the old HostInfo struct because info about the new server
     * might not be found and we need to have valid default server info.
     */

    newDefPtr = (HostInfo *) Calloc(1, sizeof(HostInfo));


    /*
     *	A 'local' lookup uses the original server that the program was
     *  initialized with.
     */

    if (local) {
	result = GetHostInfo(&defaultAddr, C_IN, T_A, newServer, newDefPtr, 1);
    } else {

	/*
	 *  Check to see if we have the address of the server or the
	 *	address of a server who knows about this domain.
	 *
	 *  For now, just use the first address in the list.
	 */
	if (defaultPtr->addrList == NULL) {
	    result = GetHostInfo(
			(struct in_addr *) defaultPtr->servers[0]->addrList[0], 
			    C_IN, T_A, newServer, newDefPtr, 1);
	} else {
	    result = GetHostInfo((struct in_addr *) defaultPtr->addrList[0], 
			    C_IN, T_A, newServer, newDefPtr, 1);
	}
    }

    if (result == SUCCESS || result == NONAUTH) {
	    /*
	     *  Found info about the new server. Free the resources for
	     *  the old server.
	     */

	    FreeHostInfoPtr(defaultPtr);
	    free((char *)defaultPtr);
	    defaultPtr = newDefPtr;
	    strcpy(defaultServer, defaultPtr->name);
	    PrintHostInfo(stdout, "Default Server:", defaultPtr);
	    return(SUCCESS);
    } else {
	    fprintf(stderr, "*** Can't find address for server %s: %s\n",
		    newServer, DecodeError(result));
	    free((char *)newDefPtr);

	    return(result);
    }
}

/*
 *******************************************************************************
 *
 *  LookupHost --
 *
 *	Asks the default name server for information about the
 *	specified host or domain. The information is printed
 *	if the lookup was successful.
 *
 *  Results:
 *	SUCCESS		- the lookup was successful.
 *	ERROR		- the output file could not be opened.
 *	Misc. Errors	- an error message is printed if the lookup failed.
 *
 *******************************************************************************
 */

int
LookupHost(string, putToFile)
    char	*string;
    Boolean	putToFile;
{
    char	host[NAME_LEN];
    char	file[NAME_LEN];
    int		result;

    /*
     *  Invalidate the current host information to prevent Finger 
     *  from using bogus info.
     */

    curHostValid = FALSE;

    /*
     *	 Parse the command string into the host and
     *	 optional output file name.
     *
     */

    sscanf(string, " %s", host);	/* removes white space */
    if (!putToFile) {
	filePtr = stdout;
    } else {
	filePtr = OpenFile(string, file);
	if (filePtr == NULL) {
	    fprintf(stderr, "*** Can't open %s for writing\n", file);
	    return(ERROR);
	}
	fprintf(filePtr,"> %s\n", string);
    }

    PrintHostInfo(filePtr, "Server:", defaultPtr);

    /*
     *  Check to see if we have the address of the server or the
     *	address of a server who knows about this domain.
     *
     *  For now, just use the first address in the list.
     */

    if (queryType == T_PTR) {
	CvtAddrToPtr(host);
    }
    if (defaultPtr->addrList == NULL) {
	result = GetHostInfo(
		    (struct in_addr *) defaultPtr->servers[0]->addrList[0], 
			  queryClass, queryType, host, &curHostInfo, 0);
    } else {
	result = GetHostInfo((struct in_addr *) defaultPtr->addrList[0], 
			  queryClass, queryType, host, &curHostInfo, 0);
    }

    switch(result) {
	case SUCCESS:
	    /*
	     *  If the query was for an address, then the curHostInfo
	     *  variable can be used by Finger.
	     *  There's no need to print anything for other query types
	     *  because the info has already been printed.
	     */
	    if (queryType == T_A) {
		curHostValid = TRUE;
		PrintHostInfo(filePtr, "Name:", &curHostInfo);
	    }
	    break;

	/*
	 * No Authoritative answer was available but we got names
	 * of servers who know about the host.
	 */
	case NONAUTH:
	    PrintHostInfo(filePtr, "Name:", &curHostInfo);
	    break;

	case NO_INFO:
	    fprintf(stderr, "*** No %s information is available for %s\n", 
			DecodeType(queryType), host);
	    break;

	case TIME_OUT:
	    fprintf(stderr, "*** Request to %s timed-out\n", defaultServer);
	    break;

	default:
	    fprintf(stderr, "*** %s can't find %s: %s\n", defaultServer, host,
		    DecodeError(result));
    }
    if (putToFile) {
	fclose(filePtr);
	filePtr = NULL;
    }
    return(result);
}

/*
 *******************************************************************************
 *
 *  LookupHostWithServer --
 *
 *	Asks the name server specified in the second argument for 
 *	information about the host or domain specified in the first
 *	argument. The information is printed if the lookup was successful.
 *
 *	Address info about the requested name server is obtained
 *	from the default name server. This routine will return an
 *	error if the default server doesn't have info about the 
 *	requested server. Thus an error return status might not
 *	mean the requested name server doesn't have info about the
 *	requested host.
 *
 *	Comments from LookupHost apply here, too.
 *
 *  Results:
 *	SUCCESS		- the lookup was successful.
 *	ERROR		- the output file could not be opened.
 *	Misc. Errors	- an error message is printed if the lookup failed.
 *
 *******************************************************************************
 */

int
LookupHostWithServer(string, putToFile)
    char	*string;
    Boolean	putToFile;
{
    char 	file[NAME_LEN];
    char 	host[NAME_LEN];
    char 	server[NAME_LEN];
    int 	result;
    static HostInfo serverInfo;

    curHostValid = FALSE;

    sscanf(string, " %s %s", host, server);
    if (!putToFile) {
	filePtr = stdout;
    } else {
	filePtr = OpenFile(string, file);
	if (filePtr == NULL) {
	    fprintf(stderr, "*** Can't open %s for writing\n", file);
	    return(ERROR);
	}
	fprintf(filePtr,"> %s\n", string);
    }
    

    if (defaultPtr->addrList == NULL) {
	result = GetHostInfo(
			(struct in_addr *) defaultPtr->servers[0]->addrList[0], 
				C_IN, T_A, server, &serverInfo, 1);
    } else {
	result = GetHostInfo((struct in_addr *) defaultPtr->addrList[0], 
				C_IN, T_A, server, &serverInfo, 1);
    }

    if (result != SUCCESS) {
	fprintf(stderr,"*** Can't find address for server %s: %s\n", server,
		 DecodeError(result));
    } else {
	PrintHostInfo(filePtr, "Server:", &serverInfo);

	if (queryType == T_PTR) {
	    CvtAddrToPtr(host);
	}
	if (serverInfo.addrList == NULL) {
	    result = GetHostInfo(
			(struct in_addr *) serverInfo.servers[0]->addrList[0], 
			      queryClass, queryType, host, &curHostInfo, 0);
	} else {
	    result = GetHostInfo((struct in_addr *) serverInfo.addrList[0], 
			      queryClass, queryType, host, &curHostInfo, 0);
	}


	switch(result) {

	    case SUCCESS:
		if (queryType == T_A) {
		    curHostValid = TRUE;
		    PrintHostInfo(filePtr, "Name:", &curHostInfo);
		}
		break;

	    case NONAUTH:
		PrintHostInfo(filePtr, "Name:", &curHostInfo);
		break;

	    case NO_INFO:
		fprintf(stderr, "*** No %s information is available for %s\n", 
			DecodeType(queryType), host);
		break;

	    case TIME_OUT:
		fprintf(stderr, "*** Request to %s timed-out\n", server);
		break;

	    default:
		fprintf(stderr, "*** %s can't find %s: %s\n", server, host,
			DecodeError(result));
	}
    }
    if (putToFile) {
	fclose(filePtr);
	filePtr = NULL;
    }
    return(result);
}

/*
 *******************************************************************************
 *
 *  SetOption -- 
 *
 *	This routine is used to change the state information
 *	that affect the lookups. The command format is
 *	   set keyword[=value]
 *	Most keywords can be abbreviated. Parsing is very simplistic--
 *	A value must not be separated from its keyword by white space.
 *
 *	Valid keywords:		Meaning:
 *	[no]aaonly	  	authoritative query only or not (hidden).
 *	all			lists current values of options.
 *	ALL			lists current values of options, including
 *				  hidden options.
 *	[no]d2			turn on/off extra debugging mode (hidden).
 *	[no]debug 		turn on/off debugging mode.
 *	[no]defname	  	use/don't use default domain name.
 *	[no]search		use/don't use domain search list.
 *	domain=NAME		set default domain name to NAME.
 *	[no]ignore		ignore/don't ignore trunc. errors (hidden).
 *	[no]primary 		use/don't use primary server (hidden).
 *	query=value		set default query type to value,
 *				value is one of the query types in RFC883
 *				without the leading T_.	(e.g. A, HINFO)
 *	[no]recurse		use/don't use recursive lookup.
 *	retry=#			set number of retries to #.
 *	root=NAME		change root server to NAME.
 *	time=#			set timeout length to #.
 *	[no]vc			use/don't use virtual circuit.
 *
 *  Results:
 *	SUCCESS		the command was parsed correctly.
 *	ERROR		the command was not parsed correctly.
 *
 *******************************************************************************
 */

int
SetOption(option)
    register char *option;
{
    char 	type[NAME_LEN];
    char 	*ptr;

    while (isspace(*option))
	++option;
    if (strncmp (option, "set ", 4) == 0)
	option += 4;
    while (isspace(*option))
	++option;

    if (*option == 0) {
	fprintf(stderr, "*** Invalid set command\n");
	return(ERROR);
    } else {
	if (strncmp(option, "all", 3) == 0) {
	    ShowOptions(FALSE);
	} else if (strncmp(option, "ALL", 3) == 0) {
	    ShowOptions(TRUE);
	} else if (strncmp(option, "aa", 2) == 0) {	/* aaonly */
	    _res.options |= RES_AAONLY;
	} else if (strncmp(option, "noaa", 4) == 0) {
	    _res.options &= ~RES_AAONLY;
	} else if (strncmp(option, "d2", 2) == 0) {	/* d2 (more debug) */
	    _res.options |= (RES_DEBUG | RES_DEBUG2);
	} else if (strncmp(option, "nod2", 4) == 0) {
	    _res.options &= ~RES_DEBUG2;
	} else if (strncmp(option, "def", 3) == 0) {	/* defname */
	    _res.options |= RES_DEFNAMES;
	} else if (strncmp(option, "nodef", 5) == 0) {
	    _res.options &= ~RES_DEFNAMES;
	} else if (strncmp(option, "do", 2) == 0) {	/* domain */
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%s", _res.defdname);
		res_re_init();
	    }
	} else if (strncmp(option, "deb", 1) == 0) {	/* debug */
	    _res.options |= RES_DEBUG;
	} else if (strncmp(option, "nodeb", 3) == 0) {
	    _res.options &= ~(RES_DEBUG | RES_DEBUG2);
	} else if (strncmp(option, "i", 1) == 0) {	/* ignore */
	    _res.options |= RES_IGNTC;
	} else if (strncmp(option, "noi", 3) == 0) {
	    _res.options &= ~RES_IGNTC;
	} else if (strncmp(option, "p", 1) == 0) {	/* primary */
	    _res.options |= RES_PRIMARY;
	} else if (strncmp(option, "nop", 3) == 0) {
	    _res.options &= ~RES_PRIMARY;
	} else if (strncmp(option, "q", 1) == 0 ||	/* querytype */
	  strncmp(option, "ty", 2) == 0) {
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%s", type);
		queryType = StringToType(type, queryType);
	    }
	} else if (strncmp(option, "cl", 2) == 0) {	/* query class */
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%s", type);
		queryClass = StringToClass(type, queryClass);
	    }
	} else if (strncmp(option, "rec", 3) == 0) {	/* recurse */
	    _res.options |= RES_RECURSE;
	} else if (strncmp(option, "norec", 5) == 0) {
	    _res.options &= ~RES_RECURSE;
	} else if (strncmp(option, "ret", 3) == 0) {	/* retry */
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%d", &_res.retry);
	    }
	} else if (strncmp(option, "ro", 2) == 0) {	/* root */
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%s", rootServerName);
	    }
	} else if (strncmp(option, "sea", 3) == 0) {	/* search list */
	    _res.options |= RES_DNSRCH;
	} else if (strncmp(option, "nosea", 5) == 0) {
	    _res.options &= ~RES_DNSRCH;
	} else if (strncmp(option, "t", 1) == 0) {	/* timeout */
	    ptr = index(option, '=');
	    if (ptr != NULL) {
		sscanf(++ptr, "%d", &_res.retrans);
	    }
	} else if (strncmp(option, "v", 1) == 0) {	/* vc */
	    _res.options |= RES_USEVC;
	} else if (strncmp(option, "nov", 3) == 0) {
	    _res.options &= ~RES_USEVC;
	} else {
	    fprintf(stderr, "*** Invalid option: %s\n",  option);
	    return(ERROR);
	}
    }
    return(SUCCESS);
}

/*
 * Fake a reinitialization when the domain is changed.
 */
res_re_init()
{
    register char *cp, **pp;
    int n;

    /* find components of local domain that might be searched */
    pp = _res.dnsrch;
    *pp++ = _res.defdname;
    for (cp = _res.defdname, n = 0; *cp; cp++)
	if (*cp == '.')
	    n++;
    cp = _res.defdname;
    for (; n >= LOCALDOMAINPARTS && pp < _res.dnsrch + MAXDNSRCH; n--) {
	cp = index(cp, '.');
	*pp++ = ++cp;
    }
    *pp = 0;
    _res.options |= RES_INIT;
}

/*
 *******************************************************************************
 *
 *  ShowOptions --
 *
 *	Prints out the state information used by the resolver
 *	library and other options set by the user.
 *
 *******************************************************************************
 */

void
ShowOptions(special)
    int special;
{
    int i;
    register char **cp;

    PrintHostInfo(stdout, "Default Server:", defaultPtr);
    if (curHostValid) {
	PrintHostInfo(stdout, "Host:", &curHostInfo);
    }

    printf("Set options:\n");
    printf("  %sdebug  \t", (_res.options & RES_DEBUG) ? "" : "no");
    printf("  %sdefname\t", (_res.options & RES_DEFNAMES) ? "" : "no");
    printf("  %ssearch\t", (_res.options & RES_DNSRCH) ? "" : "no");
    printf("  %srecurse\t", (_res.options & RES_RECURSE) ? "" : "no");
    printf("  %svc\n", (_res.options & RES_USEVC) ? "" : "no");

    if (special) {
	printf("  %saa\t\t", (_res.options & RES_AAONLY) ? "" : "no");
	printf("  %sd2\t\t", (_res.options & RES_DEBUG2) ? "" : "no");
	printf("  %signoretc\t", (_res.options & RES_IGNTC) ? "" : "no");
	printf("  %sprimary\n", (_res.options & RES_PRIMARY) ? "" : "no");
    }

    printf("  querytype=%s\t", p_type(queryType));
    printf("  class=%s\t", p_class(queryClass));
    printf("  timeout=%d\t", _res.retrans);
    printf("  retry=%d\n", _res.retry);
    printf("  domain=%s\n", _res.defdname);
    printf("  search list: ");
    for (cp = _res.dnsrch; *cp; cp++)
	printf("%s ", *cp);
    printf("\n  root=%s\n", rootServerName);

    if (special) {
	printf("\n");
	printf("State info:\n");
	printf("  current packet id:       %d\n", (int)_res.id);
	printf("  number of name servers:  %d\n", _res.nscount);
	printf("  name server addresses:   %s\n",
				    inet_ntoa(_res.nsaddr_list[0].sin_addr));
	for (i = 1; i < _res.nscount; i++) {
	    printf("                           %s\n", 
		    inet_ntoa(_res.nsaddr_list[i].sin_addr));
	}
    }
}

/*
 *******************************************************************************
 *
 *  PrintHelp --
 *
 *	Prints out the help file.
*	(Code taken from Mail.)
 *
 *******************************************************************************
 */

void
PrintHelp()
{
	register int c;
	register FILE *helpFilePtr;

	if ((helpFilePtr = fopen(HELPFILE, "r")) == NULL) {
	    perror(HELPFILE);
	    return;
	} 
	while ((c = getc(helpFilePtr)) != EOF) {
	    putchar((char) c);
	}
	fclose(helpFilePtr);
}

/*
 *******************************************************************************
 *
 * CvtAddrToPtr --
 *
 *	Convert a dotted-decimal Internet address into the standard
 *	PTR format (reversed address with .in-arpa. suffix).
 *
 *******************************************************************************
 */

static void
CvtAddrToPtr(name)
    char *name;
{
    register char *p;
    int ip[4];
    unsigned long addr;

    if (index(name, '.') != NULL) {
	/*
	 * Look for the in-addr.arpa suffix with and without a trailing .
	 */
	p = index(name, 'i');
	if (p == NULL) {
	    p = index(name, 'I');
	}
	if (p == NULL || 
	    (strncasecmp(p, "in-addr.arpa", sizeof("in-addr.arpa")-1) != 0)) {

	    /*
	     * Canonicalize the address so, for example, 127.1 works.
	     */
	    addr = inet_addr(name);
	    if (addr != -1) {
		p = inet_ntoa(addr);
		if (sscanf(p, "%d.%d.%d.%d", 
			&ip[0], &ip[1], &ip[2], &ip[3]) == 4) {
		    sprintf(name, "%d.%d.%d.%d.in-addr.arpa.",
			    ip[3], ip[2], ip[1], ip[0]);
		}
	    }
	}
    }
}
