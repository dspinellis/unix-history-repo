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
static char sccsid[] = "@(#)subr.c	5.12 (Berkeley) 7/23/88";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  subr.c --
 *
 *	Miscellaneous subroutines for the name server 
 *	lookup program.
 *  
 *	Copyright (c) 1985 
 *  	Andrew Cherenson
 *	U.C. Berkeley
 *  	CS298-26  Fall 1985
 *
 *******************************************************************************
 */

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <signal.h>
#include <setjmp.h>
#include "res.h"



/*
 *******************************************************************************
 *
 *  IntrHandler --
 *
 *	This routine is called whenever a control-C is typed. 
 *	It performs three main functions:
 *	 - closes an open socket connection,
 *	 - closes an open output file (used by LookupHost, et al.),
 *	 - jumps back to the main read-eval loop.
 *		
 *	If a user types a ^C in the middle of a routine that uses a socket, 
 *	the routine would not be able to close the socket. To prevent an 
 *	overflow of the process's open file table, the socket and output 
 *	file descriptors are closed by the interrupt handler.
 *
 *  Side effects:
 *	If sockFD is valid, it is closed.
 *	If filePtr is valid, it is closed.
 *	Flow of control returns to the main() routine.
 *
 *******************************************************************************
 */

int
IntrHandler()
{
    extern jmp_buf env;

    if (sockFD >= 0) {
	close(sockFD);
	sockFD = -1;
    }
    if (filePtr != NULL && filePtr != stdout) {
	fclose(filePtr);
	filePtr = NULL;
    }
    printf("\n");
    longjmp(env, 1);
}


/*
 *******************************************************************************
 *
 *  Malloc --
 *  Calloc --
 *
 *      Calls the malloc library routine with SIGINT blocked to prevent
 *	corruption of malloc's data structures. We need to do this because 
 *	a control-C doesn't kill the program -- it causes a return to the 
 *	main command loop.
 *
 *	NOTE: This method doesn't prevent the pointer returned by malloc 
 *	from getting lost, so it is possible to get "core leaks".
 *
 *	If malloc fails, the program exits.
 *
 *  Results:
 *	(address)	- address of new buffer.
 *
 *******************************************************************************
 */

char *
Malloc(size)
    int size;
{
    char 	*ptr;
    int 	saveMask;
    extern char *malloc();

#ifdef SYSV
    sighold(SIGINT);
    ptr = malloc((unsigned) size);
    sigrelse(SIGINT);
#else
    saveMask = sigblock(sigmask(SIGINT));
    ptr = malloc((unsigned) size);
    (void) sigsetmask(saveMask);
#endif SYSV
    if (ptr == NULL) {
	fflush(stdout);
	fprintf(stderr, "malloc failed\n");
	fflush(stderr);
	abort();
	/*NOTREACHED*/
    } else {
	return(ptr);
    }
}

char *
Calloc(num, size)
    register int num, size;
{
    char *ptr = Malloc(num*size);
    bzero(ptr, num*size);
    return(ptr);
}


/*
 *******************************************************************************
 *
 *  PrintHostInfo --
 *
 *	Prints out the HostInfo structure for a host.
 *
 *******************************************************************************
 */

void
PrintHostInfo(file, title, hp)
	FILE 	*file;
	char 	*title;
	register HostInfo *hp;
{
	register char 		**cp;
	register ServerInfo 	**sp;
	char 			comma;
	int  			i;

	fprintf(file, "%-7s  %s", title, hp->name);

	if (hp->addrList != NULL) {
	    if (hp->addrList[1] != NULL) {
		fprintf(file, "\nAddresses:");
	    } else {
		fprintf(file, "\nAddress:");
	    }
	    comma = ' ';
	    i = 0;
	    for (cp = hp->addrList; cp && *cp; cp++) {
		i++;
		if (i > 4) {
		    fprintf(file, "\n\t");
		    comma = ' ';
		    i = 0;
		}
		fprintf(file,"%c %s", comma, inet_ntoa(*(struct in_addr *)*cp));
		comma = ',';
	    }
	}

	if (hp->aliases != NULL) {
	    fprintf(file, "\nAliases:");
	    comma = ' ';
	    i = 10;
	    for (cp = hp->aliases; cp && *cp && **cp; cp++) {
		i += strlen(*cp) + 2;
		if (i > 75) {
		    fprintf(file, "\n\t");
		    comma = ' ';
		    i = 10;
		}
		fprintf(file, "%c %s", comma, *cp);
		comma = ',';
	    }
	}

	if (hp->servers != NULL) {
	    fprintf(file, "\nServed by:\n");
	    for (sp = hp->servers; *sp != NULL ; sp++) {

		fprintf(file, "- %s\n\t",  (*sp)->name);

		comma = ' ';
		i = 0;
		for (cp = (*sp)->addrList; cp && *cp && **cp; cp++) {
		    i++;
		    if (i > 4) {
			fprintf(file, "\n\t");
			comma = ' ';
			i = 0;
		    }
		    fprintf(file, 
			"%c %s", comma, inet_ntoa(*(struct in_addr *)*cp));
		    comma = ',';
		}
		fprintf(file, "\n\t");

		comma = ' ';
		i = 10;
		for (cp = (*sp)->domains; cp && *cp && **cp; cp++) {
		    i += strlen(*cp) + 2;
		    if (i > 75) {
			fprintf(file, "\n\t");
			comma = ' ';
			i = 10;
		    }
		    fprintf(file, "%c %s", comma, *cp);
		    comma = ',';
		}
		fprintf(file, "\n");
	    }
	}

	fprintf(file, "\n\n");
}

/*
 *******************************************************************************
 *
 *  OpenFile --
 *
 *	Parses a command string for a file name and opens
 *	the file.
 *
 *  Results:
 *	file pointer	- the open was successful.
 *	NULL		- there was an error opening the file or
 *			  the input string was invalid.
 *
 *******************************************************************************
 */

FILE *
OpenFile(string, file)
    char *string;
    char *file;
{
	char 	*redirect;
	FILE 	*tmpPtr;

	/*
	 *  Open an output file if we see '>' or >>'.
	 *  Check for overwrite (">") or concatenation (">>").
	 */

	redirect = index(string, '>');
	if (redirect == NULL) {
	    return(NULL);
	}
	if (redirect[1] == '>') {
	    sscanf(redirect, ">> %s", file);
	    tmpPtr = fopen(file, "a+");
	} else {
	    sscanf(redirect, "> %s", file);
	    tmpPtr = fopen(file, "w");
	}

	if (tmpPtr != NULL) {
	    redirect[0] = '\0';
	}

	return(tmpPtr);
}

/*
 *******************************************************************************
 *
 *  DecodeError --
 *
 *	Converts an error code into a character string.
 *
 *******************************************************************************
 */

char *
DecodeError(result)
    int result;
{
	switch(result) {
	    case NOERROR: 	return("Success"); break;
	    case FORMERR:	return("Format error"); break;
	    case SERVFAIL:	return("Server failed"); break;
	    case NXDOMAIN:	return("Non-existent domain"); break;
	    case NOTIMP:	return("Not implemented"); break;
	    case REFUSED:	return("Query refused"); break;
	    case NOCHANGE:	return("No change"); break;
	    case NO_INFO: 	return("No information"); break;
	    case ERROR: 	return("Unspecified error"); break;
	    case TIME_OUT: 	return("Timed out"); break;
	    case NONAUTH: 	return("Non-authoritative answer"); break;
	    default: 		break;
	}
	return("BAD ERROR VALUE"); 
}

int
StringToClass(class, dflt)
    char *class;
    int dflt;
{
	if (strcasecmp(class, "IN") == 0)
		return(C_IN);
	if (strcasecmp(class, "CHAOS") == 0)
		return(C_CHAOS);
	if (strcasecmp(class, "ANY") == 0)
		return(C_ANY);
	fprintf(stderr, "unknown query class: %s\n", class);
	return(dflt);
}
/*
 *******************************************************************************
 *
 *  StringToType --
 *
 *	Converts a string form of a query type name to its 
 *	corresponding integer value.
 *
 *******************************************************************************
 */

int
StringToType(type, dflt)
    char *type;
    int dflt;
{
	if (strcasecmp(type, "A") == 0)
		return(T_A);
	if (strcasecmp(type, "NS") == 0)
		return(T_NS);			/* authoritative server */
	if (strcasecmp(type, "MX") == 0)
		return(T_MX);			/* mail exchanger */
	if (strcasecmp(type, "CNAME") == 0)
		return(T_CNAME);		/* canonical name */
	if (strcasecmp(type, "SOA") == 0)
		return(T_SOA);			/* start of authority zone */
	if (strcasecmp(type, "MB") == 0)
		return(T_MB);			/* mailbox domain name */
	if (strcasecmp(type, "MG") == 0)
		return(T_MG);			/* mail group member */
	if (strcasecmp(type, "MR") == 0)
		return(T_MR);			/* mail rename name */
	if (strcasecmp(type, "WKS") == 0)
		return(T_WKS);			/* well known service */
	if (strcasecmp(type, "PTR") == 0)
		return(T_PTR);			/* domain name pointer */
	if (strcasecmp(type, "HINFO") == 0)
		return(T_HINFO);		/* host information */
	if (strcasecmp(type, "MINFO") == 0)
		return(T_MINFO);		/* mailbox information */
	if (strcasecmp(type, "AXFR") == 0)
		return(T_AXFR);			/* zone transfer */
	if (strcasecmp(type, "MAILB") == 0)
		return(T_MAILB);		/* mail box */
	if (strcasecmp(type, "ANY") == 0)
		return(T_ANY);			/* matches any type */
	if (strcasecmp(type, "UINFO") == 0)
		return(T_UINFO);		/* user info */
	if (strcasecmp(type, "UID") == 0)
		return(T_UID);			/* user id */
	if (strcasecmp(type, "GID") == 0)
		return(T_GID);			/* group id */
	fprintf(stderr, "unknown query type: %s\n", type);
	return(dflt);
}

/*
 *******************************************************************************
 *
 *  DecodeType --
 *
 *	Converts a query type to a descriptive name.
 *	(A more verbose form of p_type.)
 *
 *
 *******************************************************************************
 */

static  char nbuf[20];

char *
DecodeType(type)
	int type;
{
	switch (type) {
	case T_A:
		return("address");
	case T_NS:
		return("name server");
	case T_MX:		
		return("mail exchanger");
	case T_CNAME:		
		return("cannonical name");
	case T_SOA:		
		return("start of authority zone");
	case T_MB:		
		return("mailbox domain name");
	case T_MG:		
		return("mail group member");
	case T_MR:		
		return("mail rename name");
	case T_NULL:		
		return("null resource record");
	case T_WKS:		
		return("well known service");
	case T_PTR:		
		return("domain name pointer");
	case T_HINFO:		
		return("host");
	case T_MINFO:		
		return("mailbox (MINFO)");
	case T_AXFR:		
		return("zone transfer");
	case T_MAILB:		
		return("mail box");
	case T_ANY:		
		return("any type");
	case T_UINFO:
		return("user info");
	case T_UID:
		return("user id");
	case T_GID:
		return("group id");
	default:
		(void) sprintf(nbuf, "%d", type);
		return (nbuf);
	}
}
