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
 *  	CS298-26  Fall 1985
 *
 *	Copyright (c) 1985 Regents of the University of California.
 *	All rights reserved.  The Berkeley software License Agreement
 *	specifies the terms and conditions for redistribution.
 *
 *******************************************************************************
 */

#ifndef lint
static char sccsid[] = "@(#)subr.c	5.3 (Berkeley) 5/1/86";
#endif not lint

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
 *	 - close an open socket connection.
 *	 - close an open output file (used by LookupHost, et al.)
 *	 - jump back to the main read-eval loop.
 *		
 *	Since a user may type a ^C in the middle of a routine that
 *	is using a socket, the socket would never get closed by the 
 *	routine. To prevent an overflow of the process's open file table,
 *	the socket and output file descriptors are closed by
 *	the interrupt handler.
 *
 *  Side effects:
 *	If sockFD is valid, its socket is closed.
 *	If filePtr is valid, its file is closed.
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
 *  Calloc --
 *
 *      Calls the calloc library routine with the interrupt
 *      signal blocked.  The interrupt signal blocking is done
 *      to prevent malloc from getting confused if a
 *      control-C arrives in the middle of its bookkeeping
 *      routines.  We need to do this because a control-C
 *      causes a return to the main command loop instead
 *      causing the program to die.
 *
 *	This method doesn't prevent the pointer returned
 *	by calloc from getting lost, so it is possible
 *	to get "core leaks".
 *
 *  Results:
 *	(address)	- address of new buffer.
 *	NULL		- calloc failed.
 *
 *******************************************************************************
 */

char *
Calloc(num, size)
    unsigned num, size;
{
	char 	*ptr;
	int 	saveMask;
	extern char *calloc();

	saveMask = sigblock(1 << (SIGINT-1));
	ptr = calloc(num, size);
	(void) sigsetmask(saveMask);
	if (ptr == NULL) {
	    fflush(stdout);
	    fprintf(stderr, "Calloc failed\n");
	    fflush(stderr);
	    abort();
	    /*NOTREACHED*/
	} else {
	    return(ptr);
	}
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

	fprintf(file, "%-7s  %s\n", title, hp->name);

	if (hp->addrList != NULL) {
	    if (hp->addrList[1] != NULL) {
		fprintf(file, "Addresses:");
	    } else {
		fprintf(file, "Address:");
	    }
	    comma = ' ';
	    i = 0;
	    for (cp = hp->addrList; cp && *cp && **cp; cp++) {
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
	    fprintf(file, "Served by:\n");
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
StringToType(type)
    char *type;
{
	if (strcmp(type, "A") == 0) {
		return(T_A);
	} else if (strcmp(type, "NS") == 0) {
		return(T_NS);			/* authoritative server */
	} else if (strcmp(type, "MX") == 0) {
		return(T_MX);			/* mail exchanger */
	} else if (strcmp(type, "CNAME") == 0) {
		return(T_CNAME);		/* canonical name */
	} else if (strcmp(type, "SOA") == 0) {
		return(T_SOA);			/* start of authority zone */
	} else if (strcmp(type, "MB") == 0) {
		return(T_MB);			/* mailbox domain name */
	} else if (strcmp(type, "MG") == 0) {
		return(T_MG);			/* mail group member */
	} else if (strcmp(type, "MR") == 0) {
		return(T_MR);			/* mail rename name */
	} else if (strcmp(type, "WKS") == 0) {
		return(T_WKS);			/* well known service */
	} else if (strcmp(type, "PTR") == 0) {
		return(T_PTR);			/* domain name pointer */
	} else if (strcmp(type, "HINFO") == 0) {
		return(T_HINFO);		/* host information */
	} else if (strcmp(type, "MINFO") == 0) {
		return(T_MINFO);		/* mailbox information */
	} else if (strcmp(type, "AXFR") == 0) {
		return(T_AXFR);			/* zone transfer */
	} else if (strcmp(type, "MAILB") == 0) {
		return(T_MAILB);		/* mail box */
	} else if (strcmp(type, "ANY") == 0) {
		return(T_ANY);			/* matches any type */
	} else if (strcmp(type, "UINFO") == 0) {
		return(T_UINFO);		/* user info */
	} else if (strcmp(type, "UID") == 0) {
		return(T_UID);			/* user id */
	} else if (strcmp(type, "GID") == 0) {
		return(T_GID);			/* group id */
	} else {
		return(T_A);
	}
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
extern  char *sprintf();

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
		return (sprintf(nbuf, "%d", type));
	}
}

printanswer(hp)
	register struct hostent *hp;
{
	register char **cp;
	int	i;
	char	comma;
	extern char *inet_ntoa();

	fprintf(stderr,"Name: %s\n", hp->h_name);
	fprintf(stderr,"Address: %s\n", 
	    inet_ntoa(*(struct in_addr *)hp->h_addr));
	if (hp->h_aliases != NULL) {
	    fprintf(stderr, "Aliases:");
	    comma = ' ';
	    i = 10;
	    for (cp = hp->h_aliases; cp && *cp && **cp; cp++) {
		i += strlen(*cp) + 2;
		if (i > 75) {
		    fprintf(stderr, "\n\t");
		    comma = ' ';
		    i = 10;
		}
		fprintf(stderr, "%c %s", comma, *cp);
		comma = ',';
	    }
	}
	fprintf(stderr,"\n\n");
}

hperror(errno) 
int errno;
{
switch(errno) {
	case HOST_NOT_FOUND:
		fprintf(stderr,"Host not found.\n");
		break;
	case TRY_AGAIN:
		fprintf(stderr,"Host not found, try again.\n");
		break;
	case NO_RECOVERY:
		fprintf(stderr,"No recovery, Host not found.\n");
		break;
	case NO_ADDRESS:
		fprintf(stderr,"No Address, look for MF record.\n");
		break;
	}
}

