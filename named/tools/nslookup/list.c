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
static char sccsid[] = "@(#)list.c	5.13 (Berkeley) 7/23/88";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  list.c --
 *
 *	Routines to obtain info from name and finger servers.
 *
 *	Adapted from 4.3BSD BIND ns_init.c and from /usr/src/ucb/finger.c
 *
 *******************************************************************************
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include "res.h"

/*
 *  Imported from res_debug.c
 */
extern char *_res_resultcodes[];

typedef union {
    HEADER qb1;
    char qb2[PACKETSZ];
} querybuf;

extern u_long 		inet_addr();
extern HostInfo 	*defaultPtr;
extern HostInfo 	curHostInfo;
extern int 		curHostValid;

/*
 *  During a listing to a file, hash marks are printed 
 *  every HASH_SIZE records.
 */

#define HASH_SIZE 50


/*
 *******************************************************************************
 *
 *  ListHosts --
 *
 *	Requests the name server to do a zone transfer so we
 *	find out what hosts it knows about.
 *
 *	There are five types of output:
 *	- internet addresses (default)
 *	- cpu type and operating system (-h option)
 *	- canonical and alias names  (-a option)
 *	- well-known service names  (-s option)
 *	- ALL records (-d option)
 *	
 *	To see all three types of information in sorted order, 
 *	do the following:
 *	  ls domain.edu > file
 *	  ls -a domain.edu >> file
 *	  ls -h domain.edu >> file
 *	  ls -s domain.edu >> file
 *	  view file
 *
 *  Results:
 *	SUCCESS		the listing was successful.
 *	ERROR		the server could not be contacted because 
 *			a socket could not be obtained or an error
 *			occured while receiving, or the output file
 *			could not be opened.
 *
 *******************************************************************************
 */

int
ListHosts(string, putToFile)
    char *string;
    int  putToFile;
{
	querybuf 		buf;
	struct sockaddr_in 	sin;
	HEADER 			*headerPtr;
	int 			queryType;
	int 			msglen;
	int 			amtToRead;
	int 			numRead;
	int 			i;
	int 			numAnswers = 0;
	int 			result;
	int 			soacnt = 0;
	u_short 		len;
	char 			*cp, *nmp;
	char 			name[NAME_LEN];
	char 			dname[2][NAME_LEN];
	char 			option[NAME_LEN];
	char 			file[NAME_LEN];
	char			*namePtr;
	static char		*answer = NULL;
	static int		answerLen = 0;
	enum {
	    NO_ERRORS, 
	    ERR_READING_LEN, 
	    ERR_READING_MSG,
	    ERR_PRINTING,
	} error = NO_ERRORS;

	/*
	 *  Parse the command line. It maybe of the form "ls domain",
	 *  "ls -a domain" or "ls -h domain".
	 */ 
	i = sscanf(string, " ls %s %s", option, name);
	if (putToFile && i == 2 && name[0] == '>') {
	    i--;
	}
	if (i == 2) {
	    if (strcmp("-a", option) == 0) {
		queryType = T_CNAME;
	    } else if (strcmp("-h", option) == 0) {
		queryType = T_HINFO;
	    } else if (strcmp("-m", option) == 0) {
		queryType = T_MX;
	    } else if (strcmp("-s", option) == 0) {
		queryType = T_WKS;
	    } else if (strcmp("-d", option) == 0) {
		queryType = T_ANY;
	    } else {
		queryType = T_A;
	    }
	    namePtr = name;
	} else if (i == 1) {
	    namePtr = option;
	    queryType = T_A;
	} else {
	    fprintf(stderr, "ListHosts: invalid request %s\n",string);
	    return(ERROR);
	}


	/*
	 *  Create a query packet for the requested domain name.
	 */
	msglen = res_mkquery(QUERY, namePtr, C_IN, T_AXFR,
				(char *)0, 0, (char *)0, 
				(char *) &buf, sizeof(buf));
	if (msglen < 0) {
	    if (_res.options & RES_DEBUG) {
		fprintf(stderr, "ListHosts: Res_mkquery failed\n");
	    }
	    return (ERROR);
	}

	bzero((char *)&sin, sizeof(sin));
	sin.sin_family	= AF_INET;
	sin.sin_port	=  htons(NAMESERVER_PORT);

	/*
	 *  Check to see if we have the address of the server or the
	 *  address of a server who knows about this domain.
	 *       
	 *  For now, just use the first address in the list.
	 */

	if (defaultPtr->addrList != NULL) {
	  sin.sin_addr = *(struct in_addr *) defaultPtr->addrList[0];
	} else {
	  sin.sin_addr = *(struct in_addr *)defaultPtr->servers[0]->addrList[0];
	}

	/*
	 *  Set up a virtual circuit to the server.
	 */
	if ((sockFD = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	    perror("ListHosts");
	    return(ERROR);
	}	
	if (connect(sockFD, &sin, sizeof(sin)) < 0) {
	    perror("ListHosts");
	    (void) close(sockFD);
	    sockFD = -1;
	    return(ERROR);
	}	

	/*
	 * Send length & message for zone transfer 
	 */

        len = htons(msglen);

        if (write(sockFD, (char *)&len, sizeof(len)) != sizeof(len) ||
            write(sockFD, (char *) &buf, msglen) != msglen) {
		perror("ListHosts");
		(void) close(sockFD);
		sockFD = -1;
		return(ERROR);
	}

	fprintf(stdout,"[%s]\n",
		(defaultPtr->addrList != NULL) ? defaultPtr->name : 
		 defaultPtr->servers[0]->name);

	if (!putToFile) {
	    filePtr = stdout;
	} else {
	    filePtr = OpenFile(string, file);
            if (filePtr == NULL) {
                fprintf(stderr, "*** Can't open %s for writing\n", file);
		(void) close(sockFD);
		sockFD = -1;
                return(ERROR);
            }
	    fprintf(filePtr, "> %s\n", string);
	    fprintf(filePtr,"[%s]\n",
		(defaultPtr->addrList != NULL) ? defaultPtr->name : 
		 defaultPtr->servers[0]->name);
	}

	fprintf(filePtr, "%-30s", "Host or domain name");
	switch(queryType) {
	    case T_ANY:
		    fprintf(filePtr, " %-30s\n", "Resource record info");
		    break;
	    case T_A:
		    fprintf(filePtr, " %-30s\n", "Internet address");
		    break;
	    case T_HINFO:
		    fprintf(filePtr, " %-10s %s\n", "CPU", "OS");
		    break;
	    case T_CNAME:
		    fprintf(filePtr, " %-30s\n", "Alias");
		    break;
	    case T_MX:
		    fprintf(filePtr, " %3s %s\n", "Metric", "Host");
		    break;
	    case T_WKS:
		    fprintf(filePtr, " %-4s %s\n", "Protocol", "Services");
	}


	while (1) {
	    unsigned short tmp;

	    /*
	     * Read the length of the response.
	     */

	    cp = (char *) &tmp;
	    amtToRead = sizeof(u_short);
	    while(amtToRead > 0 && (numRead = read(sockFD, cp, amtToRead)) > 0){
		cp 	  += numRead;
		amtToRead -= numRead;
	    }
	    if (numRead <= 0) {
		error = ERR_READING_LEN;
		break;
	    }	

	    if ((len = htons(tmp)) == 0) {
		break;	/* nothing left to read */
	    }

	    /*
	     * The server sent too much data to fit the existing buffer --
	     * allocate a new one.
	     */
	    if (len > answerLen) {
		if (answerLen != 0) {
		    free(answer);
		}
		answerLen = len;
		answer = Malloc(answerLen);
	    }	

	    /*
	     * Read the response.
	     */

	    amtToRead = len;
	    cp = answer;
	    while(amtToRead > 0 && (numRead = read(sockFD, cp, amtToRead)) > 0){
		cp += numRead;
		amtToRead -= numRead;
	    }
	    if (numRead <= 0) {
		error = ERR_READING_MSG;
		break;
	    }

	    result = PrintListInfo(filePtr, answer, cp, queryType);
	    if (result != SUCCESS) {
		error = ERR_PRINTING;
		break;
	    }

	    numAnswers++;
	    if (putToFile && ((numAnswers % HASH_SIZE) == 0)) {
		fprintf(stdout, "#");
		fflush(stdout);
	    }
	    cp = answer + sizeof(HEADER);
	    if (ntohs(((HEADER* )answer)->qdcount) > 0)
		cp += dn_skipname(cp, answer + len) + QFIXEDSZ;
	    nmp = cp;
	    cp += dn_skipname(cp, (u_char *)answer + len);
	    if ((_getshort(cp) == T_SOA)) {
		dn_expand(answer, answer + len, nmp, dname[soacnt],
			sizeof(dname[0]));
	        if (soacnt) {
		    if (strcmp(dname[0], dname[1]) == 0)
			break;
		} else
		    soacnt++;
	    }
	}

	if (putToFile) {
	    fprintf(stdout, "%sReceived %d record%s.\n", 
		(numAnswers >= HASH_SIZE) ? "\n" : "",
		numAnswers,
		(numAnswers != 1) ? "s" : "");
	}

	(void) close(sockFD);
	sockFD = -1;
	if (putToFile) {
	    fclose(filePtr);
	    filePtr = NULL;
	}

	switch (error) {
	    case NO_ERRORS:
		return (SUCCESS);

	    case ERR_READING_LEN:
		return(ERROR);

	    case ERR_PRINTING:
		fprintf(stderr,"*** Error during listing of %s: %s\n", 
				namePtr, DecodeError(result));
		return(result);

	    case ERR_READING_MSG:
		headerPtr = (HEADER *) answer;
		fprintf(stderr,"ListHosts: error receiving zone transfer:\n");
		fprintf(stderr,
	       "  result: %s, answers = %d, authority = %d, additional = %d\n", 
		    	_res_resultcodes[headerPtr->rcode], 
		    	ntohs(headerPtr->ancount), ntohs(headerPtr->nscount), 
			ntohs(headerPtr->arcount));
		return(ERROR);
	    default:
		return(ERROR);
	}
}


/*
 *******************************************************************************
 *
 *  PrintListInfo --
 *
 * 	Used by the ListInfo routine to print the answer 
 *	received from the name server. Only the desired 
 *	information is printed.
 *
 *  Results:
 *	SUCCESS		the answer was printed without a problem.
 *	NO_INFO		the answer packet did not contain an answer.
 *	ERROR		the answer was malformed.
 *      Misc. errors	returned in the packet header.
 *
 *******************************************************************************
 */

#define NAME_FORMAT " %-30s"
#define STRIP_DOMAIN(string) if((dot = index(string, '.')) != NULL) *dot = '\0'


PrintListInfo(file, msg, eom, queryType)
    FILE 	*file;
    char 	*msg, *eom;
    int 	queryType;
{
    register char 	*cp;
    HEADER 		*headerPtr;
    int 		type, class, dlen, nameLen;
    u_long		ttl;
    int 		n;
    struct in_addr 	inaddr;
    char 		name[NAME_LEN];
    char 		name2[NAME_LEN];
    char 		*dot;

    /*
     * Read the header fields.
     */
    headerPtr = (HEADER *)msg;
    cp = msg + sizeof(HEADER);
    if (headerPtr->rcode != NOERROR) {
	return(headerPtr->rcode);
    }

    /*
     *  We are looking for info from answer resource records.
     *  If there aren't any, return with an error. We assume
     *  there aren't any question records.
     */

    if (ntohs(headerPtr->ancount) == 0) {
	return(NO_INFO);
    } else {
	if (ntohs(headerPtr->qdcount) > 0) {
	    nameLen = dn_skipname(cp, eom);
	    if (nameLen < 0)
		return (ERROR);
	    cp += nameLen + QFIXEDSZ;
	}
	if ((nameLen = dn_expand(msg, eom, cp, name, sizeof(name))) < 0) {
	    return (ERROR);
	}
	cp += nameLen;
	type = _getshort(cp);
	cp += sizeof(u_short);
	class = _getshort(cp);
	cp += sizeof(u_short);
	ttl = _getlong(cp);
	cp += sizeof(u_long);
	dlen = _getshort(cp);
	cp += sizeof(u_short);
	if (name[0] == 0)
		strcpy(name, "(root)");

	/*
	 * QueryType is used to specify the type of desired information.
	 *  T_A   	- internet address
	 *  T_CNAME 	- aliases
	 *  T_HINFO	- cpu, OS type
	 *  T_MX	- mail routing
	 *  T_WKS	- well known service
	 *  T_ANY	- any
	 *
	 */
	switch (type) {

	    case T_A:
		if (queryType != T_A && queryType != T_ANY)
		    break;

		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		if (class == C_IN) {
		    bcopy(cp, (char *)&inaddr, sizeof(inaddr));
		    if (dlen == 4) {
			fprintf(file," %s", inet_ntoa(inaddr));
		    } else if (dlen == 7) {
			fprintf(file," %s", inet_ntoa(inaddr));
			fprintf(file," (%d, %d)", cp[4],(cp[5] << 8) + cp[6]);
		    } else
			fprintf(file, " (dlen = %d?)", dlen);
		    if (_res.options & RES_DEBUG)
			fprintf(file,"\t\t\t%lu", ttl);
		    fprintf(file,"\n");
		} else
		    goto other;
		break;
		
	    case T_CNAME:
		if (queryType != T_CNAME && queryType != T_ANY)
		    break;

		if ((_res.options & RES_DEBUG) == 0)
			STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		if ((nameLen = dn_expand(msg, eom, cp, name2, sizeof(name2))) < 0) {
		    fprintf(file, " ***\n");
		    return (ERROR);
		}
		/* 
		 * a bug -- cnames need not be in same domain!
		 * STRIP_DOMAIN(name2);
		 */

		fprintf(file, NAME_FORMAT, name2);
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu", ttl);
		fprintf(file,"\n");
		break;
		
	    case T_HINFO:
		if (queryType != T_HINFO && queryType != T_ANY)
		    break;

		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		if (n = *cp++) {
		    (void)sprintf(name,"%.*s", n, cp);
		    fprintf(file," %-10s", name);
		    cp += n;
		} else {
		    fprintf(file," %-10s", " ");
		}
		if (n = *cp++) {
		    fprintf(file," %.*s", n, cp);
		    cp += n;
		}
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t\t%lu", ttl);
		fprintf(file,"\n");
		break;

	    case T_MX:
		if (queryType != T_MX && queryType != T_ANY)
		    break;

		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));

		{
		    short pref;

		    pref = _getshort(cp);
		    cp += sizeof(u_short);
		    fprintf(file," %-3d ",pref);
		}
		if ((nameLen = dn_expand(msg, eom, cp, name2, sizeof(name2))) < 0) {
		    fprintf(file, " ***\n");
		    return (ERROR);
		}
		fprintf(file, " %s", name2);
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu", ttl);
		fprintf(file,"\n");

		break;


	    case T_NS:
	    case T_PTR:
		if (queryType != T_A && queryType != T_ANY)
		    break;
		/*
		 *  Found a name server or pointer record.
		 */
		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		fprintf(file," %s = ", type == T_PTR ? "host" : "server");
		cp = Print_cdname2(cp, msg, eom, file);
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu", ttl);
		fprintf(file,"\n");
		break;

	    case T_WKS:
		if (queryType != T_WKS && queryType != T_ANY)
		    break;

		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		if (class == C_IN) {
		    cp += 4; dlen -= 4;
		    {
			struct protoent *pp;
			struct servent *ss;
			u_short port;

			setprotoent(1);
			setservent(1);
			n = *cp & 0377;
			pp = getprotobynumber(n);
			if(pp == 0)  
			    fprintf(file," %-3d ", n);
			else
			    fprintf(file," %-3s ", pp->p_name);
			cp++; dlen--;

			port = 0;
			while(dlen-- > 0) {
			    n = *cp++;
			    do {
				if(n & 0200) {
				    ss = getservbyport((int)htons(port), pp->p_name);
				    if(ss == 0)  
					fprintf(file," %d", port);
				    else
					fprintf(file," %s", ss->s_name);
				}
				    n <<= 1;
			    } while(++port & 07);
			}
		    } 
		} else
		    goto other;
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu", ttl);
		fprintf(file,"\n");
		endprotoent();
		endservent();
		break;

	    case T_SOA:
	    case T_AXFR:
		if (queryType != T_ANY)
		    break;
		fprintf(file, NAME_FORMAT, name);
		if (queryType == T_ANY)
		    fprintf(file," %-5s", p_type(type));
		if ((nameLen = dn_expand(msg, eom, cp, name2, sizeof(name2))) < 0) {
		    fprintf(file, " ***\n");
		    return (ERROR);
		}
		cp += nameLen;
		fprintf(file, " %s", name2);
		if ((nameLen = dn_expand(msg, eom, cp, name2, sizeof(name2))) < 0) {
		    fprintf(file, " ***\n");
		    return (ERROR);
		}
		cp += nameLen;
		fprintf(file, " %s. (", name2);
		for (n = 0; n < 5; n++) {
		    u_long u;

		    u = _getlong(cp);
		    cp += sizeof(u_long);
		    fprintf(file,"%s%d", n? " " : "", u);
		}
		fprintf(file, ")");
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu", ttl);
		fprintf(file,"\n");
		break;

	    default:
		/*
		 * Unwanted answer type -- ignore it.
		 */
		if (queryType != T_ANY)
		    break;
		if ((_res.options & RES_DEBUG) == 0)
		    STRIP_DOMAIN(name);
		fprintf(file, NAME_FORMAT, name);
other:
		fprintf(file," type = %-5s", p_type(type));
		fprintf(file,", class = %-5s", p_class(class));
		if (_res.options & RES_DEBUG)
		    fprintf(file,"\t%lu\n", ttl);
		break;
	}
    }
    return(SUCCESS);
}


/*
 *******************************************************************************
 *
 *  ViewList --
 *
 *	A hack to view the output of the ls command in sorted
 *	order using more.
 *
 *******************************************************************************
 */

ViewList(string)
    char *string;
{
    char file[NAME_LEN];
    char command[NAME_LEN];

    sscanf(string, " view %s", file);
    (void)sprintf(command, "grep \"^ \" %s | sort | more", file);
    system(command);
}

/*
 *******************************************************************************
 *
 *   Finger --
 *
 *	Connects with the finger server for the current host
 *	to request info on the specified person (long form)
 *	who is on the system (short form).
 *
 *  Results:
 *	SUCCESS		the finger server was contacted.
 *	ERROR		the server could not be contacted because 
 *			a socket could not be obtained or connected 
 *			to or the service could not be found.
 *
 *******************************************************************************
 */

Finger(string, putToFile)
    char *string;
    int  putToFile;
{
	struct servent 		*sp;
	struct sockaddr_in 	sin;
	register FILE 		*f;
	register int 		c;
	register int 		lastc;
	char 			name[NAME_LEN];
	char 			file[NAME_LEN];

	/*
	 *  We need a valid current host info to get an inet address.
	 */
	if (!curHostValid) {
	    fprintf(stderr, "Finger: no current host defined.\n");
	    return (ERROR);
	}

	if (sscanf(string, " finger %s", name) == 1) {
	    if (putToFile && (name[0] == '>')) {
		name[0] = '\0';
	    }
	} else {
	    name[0] = '\0';
	}

	sp = getservbyname("finger", "tcp");
	if (sp == 0) {
	    fprintf(stderr, "Finger: unknown service\n");
	    return (ERROR);
	}

	bzero((char *)&sin, sizeof(sin));
	sin.sin_family	= curHostInfo.addrType;
	sin.sin_port	= sp->s_port;
	bcopy(curHostInfo.addrList[0], (char *)&sin.sin_addr, 
		curHostInfo.addrLen);

	/*
	 *  Set up a virtual circuit to the host.
	 */

	sockFD = socket(curHostInfo.addrType, SOCK_STREAM, 0);
	if (sockFD < 0) {
	    fflush(stdout);
	    perror("Finger");
	    return (ERROR);
	}

	if (connect(sockFD, (char *)&sin, sizeof (sin)) < 0) {
	    fflush(stdout);
	    perror("Finger");
	    close(sockFD);
	    sockFD = -1;
	    return (ERROR);
	}

	if (!putToFile) {
	    filePtr = stdout;
	} else {
	    filePtr = OpenFile(string, file);
	    if (filePtr == NULL) {
		fprintf(stderr, "*** Can't open %s for writing\n", file);
		close(sockFD);
		sockFD = -1;
		return(ERROR);
	    }
	    fprintf(filePtr,"> %s\n", string);
	}
	fprintf(filePtr, "[%s]\n", curHostInfo.name);

	if (name[0] != '\0') {
	    write(sockFD, "/W ", 3);
	}
	write(sockFD, name, strlen(name));
	write(sockFD, "\r\n", 2);
	f = fdopen(sockFD, "r");
	while ((c = getc(f)) != EOF) {
	    switch(c) {
		case 0210:
		case 0211:
		case 0212:
		case 0214:
			c -= 0200;
			break;
		case 0215:
			c = '\n';
			break;
	    }
	    putc(lastc = c, filePtr);
	}
	if (lastc != '\n') {
	    putc('\n', filePtr);
	}
	putc('\n', filePtr);

	close(sockFD);
	sockFD = -1;

	if (putToFile) {
	    fclose(filePtr);
	    filePtr = NULL;
	}
	return (SUCCESS);
}
