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
static char sccsid[] = "@(#)getinfo.c	5.18 (Berkeley) 6/18/88";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  getinfo.c --
 *
 *	Routines to create requests to name servers 
 *	and interpret the answers.
 *
 *	Adapted from 4.3BSD BIND gethostnamadr.c
 *
 *******************************************************************************
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <ctype.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include "res.h"

extern char *_res_resultcodes[];
extern char *res_skip();

#define	MAXALIASES	35
#define MAXADDRS	35
#define MAXDOMAINS	35
#define MAXSERVERS	10

static char *addr_list[MAXADDRS + 1];

static char *host_aliases[MAXALIASES];
static int   host_aliases_len[MAXALIASES];
static char  hostbuf[BUFSIZ+1];

typedef struct {
    char *name;
    char *domain[MAXDOMAINS];
    int   numDomains;
    char *address[MAXADDRS];
    int   numAddresses;
} ServerTable;

ServerTable server[MAXSERVERS];

typedef union {
    HEADER qb1;
    char qb2[PACKETSZ];
} querybuf;

typedef union {
    long al;
    char ac;
} align;

#define GetShort(cp)	_getshort(cp); cp += sizeof(unsigned short);


/*
 *******************************************************************************
 *
 *  GetAnswer --
 *
 *	Interprets an answer packet and retrieves the following
 *	information:
 *
 *  Results:
 *      SUCCESS         the info was retrieved.
 *      NO_INFO         the packet did not contain an answer.
 *	NONAUTH		non-authoritative information was found.
 *      ERROR           the answer was malformed.
 *      Other errors    returned in the packet header.
 *
 *******************************************************************************
 */

static int
GetAnswer(nsAddrPtr, queryType, msg, msglen, iquery, hostPtr, isserver)
    struct in_addr 	*nsAddrPtr;
    char 		*msg;
    int 		queryType;
    int 		msglen;
    Boolean 		iquery;
    register HostInfo	*hostPtr;
    Boolean		isserver;
{
    register HEADER 	*headerPtr;
    register char 	*cp;
    querybuf 		answer;
    char 		*eom, *bp, **aliasPtr;
    char 		**addrPtr;
    char		*namePtr;
    char		*dnamePtr;
    int 		type, class;
    int 		qdcount, ancount, arcount, nscount, buflen;
    int 		origClass;
    int 		numAliases = 0;
    int 		numAddresses = 0;
    int 		n, i, j;
    int 		len;
    int 		dlen;
    int 		status;
    int			numServers;
    Boolean 		haveanswer;


    /*
     *  If the hostPtr was used before, free up the calloc'd areas.
     */
    FreeHostInfoPtr(hostPtr);
	
    status = SendRequest(nsAddrPtr, msg, msglen, (char *) &answer, 
			    sizeof(answer), &n);

    if (status != SUCCESS) {
	    if (_res.options & RES_DEBUG2)
		    printf("SendRequest failed\n");
	    return (status);
    }
    eom = (char *) &answer + n;

    headerPtr = (HEADER *) &answer;
    qdcount = ntohs(headerPtr->qdcount);
    ancount = ntohs(headerPtr->ancount);
    arcount = ntohs(headerPtr->arcount);
    nscount = ntohs(headerPtr->nscount);

    if (headerPtr->rcode != NOERROR) {
	if (_res.options & RES_DEBUG && !isserver) {
	    printf("Failed: %s, num. answers = %d, ns = %d, additional = %d\n", 
		    _res_resultcodes[headerPtr->rcode], ancount, nscount,
		    arcount);
	}
	return (headerPtr->rcode);
    }

    /*
     * If there are no answer, n.s. or additional records 
     * then return with an error.
     */
    if (ancount == 0 && nscount == 0 && arcount == 0) {
	return (NO_INFO);
    }


    bp 	   = hostbuf;
    buflen = sizeof(hostbuf);
    cp 	   = (char *) &answer + sizeof(HEADER);

    /*
     * For inverse queries, the desired information is returned
     * in the question section. If there are no question records,
     * return with an error.
     *
     */
    if (qdcount != 0) {
	if (iquery) {
	    if ((n = dn_expand((char *)&answer, eom, cp, bp, buflen)) < 0) {
		return (ERROR);
	    }
	    cp += n + QFIXEDSZ;
	    len = strlen(bp) + 1;
	    hostPtr->name = Calloc(1, len);
	    bcopy(bp, hostPtr->name, len);
	} else {
	    cp += dn_skipname(cp, eom) + QFIXEDSZ;
	}
	while (--qdcount > 0) {
	    cp += dn_skipname(cp, eom) + QFIXEDSZ;
	}
    } else if (iquery) {
	return (NO_INFO);
    }

    aliasPtr 	= host_aliases;
    addrPtr	= addr_list;
    haveanswer 	= FALSE;

    /*
     * Scan through the answer resource records.
     * Answers for address query types are saved.
     * Other query type answers are just printed.
     */
    if (!isserver && !headerPtr->aa && headerPtr->ancount != 0) {
	printf("Non-authoritative answer:\n");
    }
    while (--ancount >= 0 && cp < eom) {
	if (queryType != T_A) {
	    if ((cp = Print_rr(cp, (char *) &answer, eom, stdout)) == NULL) {
		return(ERROR);
	    }
	    continue;
	} else {
	    if ((n = dn_expand((char *) &answer, eom, cp, bp, buflen)) < 0) {
		return(ERROR);
	    }
	    cp   += n;
	    type  = GetShort(cp);
	    class = GetShort(cp);
	    cp   += sizeof(u_long); 	/* skip TTL */
	    dlen  = GetShort(cp);
	    if (type == T_CNAME) {
		/*
		 * Found an alias.
		 */
		cp += dlen;
		if (aliasPtr >= &host_aliases[MAXALIASES-1]) {
		    continue;
		}
		*aliasPtr++ = bp;
		n = strlen(bp) + 1;
		host_aliases_len[numAliases] = n;
		numAliases++;
		bp += n;
		buflen -= n;
		continue;
	    } else if (type == T_PTR) {
		/*
		 *  Found a "pointer" to the real name.
		 */
		if((n= dn_expand((char *)&answer, eom, cp, bp,buflen)) < 0) {
		    cp += n;
		    continue;
		}
		cp += n;
		len = strlen(bp) + 1;
		hostPtr->name = Calloc(1, len);
		bcopy(bp, hostPtr->name, len);
		haveanswer = TRUE;
		break;
	    } else if (type != T_A) {
		cp += dlen;
		continue;
	    }
	    if (haveanswer) {
		/*
		 * If we've already got 1 address, we aren't interested
		 * in addresses with a different length or class.
		 */
		if (dlen != hostPtr->addrLen) {
		    cp += dlen;
		    continue;
		}
		if (class != origClass) {
		    cp += dlen;
		    continue;
		}
	    } else {
		/*
		 * First address: record its length and class so we
		 * only save additonal ones with the same attributes.
		 */
		hostPtr->addrLen = dlen;
		origClass = class;
		hostPtr->addrType = (class == C_IN) ? AF_INET : AF_UNSPEC;
		if (!iquery) {
		    len = strlen(bp) + 1;
		    hostPtr->name = Calloc(1, len);
		    bcopy(bp, hostPtr->name, len);
		}
	    }
	    bp += (((u_long)bp) % sizeof(align));

	    if (bp + dlen >= &hostbuf[sizeof(hostbuf)]) {
		if (_res.options & RES_DEBUG) {
		    printf("Size (%d) too big\n", dlen);
		}
		break;
	    }
	    bcopy(cp, *addrPtr++ = bp, dlen);
	    bp +=dlen;
	    cp += dlen;
	    numAddresses++;
	    haveanswer = TRUE; 
	}
    }

    if (queryType == T_A && haveanswer) {

	/*
	 *  Go through the alias and address lists and return them
	 *  in the hostPtr variable.
	 */

	if (numAliases > 0) {
	    hostPtr->aliases = (char **) Calloc(1 + numAliases, sizeof(char *));
	    for (i = 0; i < numAliases; i++) {
		hostPtr->aliases[i] = Calloc(1, host_aliases_len[i]);
		bcopy(host_aliases[i], hostPtr->aliases[i],host_aliases_len[i]);
	    }
	    hostPtr->aliases[i] = NULL;
	}
	if (numAddresses > 0) {
	    hostPtr->addrList = (char **)Calloc(1+numAddresses, sizeof(char *));
	    for (i = 0; i < numAddresses; i++) {
		hostPtr->addrList[i] = Calloc(1, hostPtr->addrLen);
		bcopy(addr_list[i], hostPtr->addrList[i], hostPtr->addrLen);
	    }
	    hostPtr->addrList[i] = NULL;
	}
#ifdef verbose
	if (headerPtr->aa || nscount == 0) {
	    hostPtr->servers = NULL;
	    return (SUCCESS);
	}
#else
	hostPtr->servers = NULL;
	return (SUCCESS);
#endif
    }

    /*
     * At this point, for the T_A query type, only empty answers remain.
     * For other query types, additional information might be found
     * in the additional resource records part.
     */

    cp = res_skip((char *) &answer, 2, eom);

    numServers = 0;
    while (--nscount >= 0 && cp < eom) {
	/*
	 *  Go through the NS records and retrieve the names of hosts 
	 *  that serve the requested domain.
	 */

	if ((n = dn_expand((char *) &answer, eom, cp, bp, buflen)) < 0) {
	    return(ERROR);
	}
	cp += n;
	len = strlen(bp) + 1;
	dnamePtr = Calloc(1, len);   /* domain name */
	bcopy(bp, dnamePtr, len);

	type  = GetShort(cp);
	class = GetShort(cp);
	cp   += sizeof(u_long); 	/* skip TTL */
	dlen  = GetShort(cp);

	if (type != T_NS) {
	    cp += dlen;
	} else {
	    Boolean	found;

	    if ((n = dn_expand((char *) &answer, eom, cp, bp, buflen)) < 0) {
		return(ERROR);
	    }
	    cp += n;
	    len = strlen(bp) + 1;
	    namePtr = Calloc(1, len); /* server host name */
	    bcopy(bp, namePtr, len);

	    /*
	    * Store the information keyed by the server host name.
	    */
	    found = FALSE;
	    for (j = 0; j < numServers; j++) {
		if (strcmp(namePtr, server[j].name) == 0) {
		    found = TRUE;
		    free(namePtr);
		    break;
		}
	    }
	    if (found) {
		server[j].numDomains++;
		if (server[j].numDomains <= MAXDOMAINS) {
		    server[j].domain[server[j].numDomains-1] = dnamePtr;
		}
	    } else {
		if (numServers >= MAXSERVERS) {
		    break;
		}
		server[numServers].name = namePtr;
		server[numServers].domain[0] = dnamePtr;
		server[numServers].numDomains = 1;
		server[numServers].numAddresses = 0;
		numServers++;
	    }
	}
    }

    if (!headerPtr->aa && (queryType != T_A) && arcount > 0) {
	printf("Authoritative answers can be found from:\n");
    }

    /*
     * Additional resource records contain addresses of servers.
     */
    cp = res_skip((char *) &answer, 3, eom);
    while (--arcount >= 0 && cp < eom) {
	/*
	 * If we don't need to save the record, just print it.
	 */
	if (queryType != T_A) {
	    if ((cp = Print_rr(cp, (char *) &answer, eom, stdout)) == NULL) {
		return(ERROR);
	    }
	    continue;

	} else {
	  if ((n = dn_expand((char *) &answer, eom, cp, bp, buflen)) < 0) {
	      break;
	  }
	  cp   += n;
	  type  = GetShort(cp);
	  class = GetShort(cp);
	  cp   += sizeof(u_long); 	/* skip TTL */
	  dlen  = GetShort(cp);

	  if (type != T_A)  {
	      cp += dlen;
	      continue;
	  } else {
	      for (j = 0; j < numServers; j++) {
	        if (strcmp(bp, server[j].name) == 0) {
		  server[j].numAddresses++;
		  if (server[j].numAddresses <= MAXADDRS) {
		   server[j].address[server[j].numAddresses-1] = Calloc(1,dlen);
		   bcopy(cp,server[j].address[server[j].numAddresses-1],dlen);
		   break;
		  }
	        }
	      }
	      cp += dlen;
	  }
      }
    }

    /*
     * If we are returning name server info, transfer it to the hostPtr.
     */
    if (numServers > 0) {
	hostPtr->servers = (ServerInfo **) 
		    		Calloc(numServers+1, sizeof(ServerInfo *));

	for (i = 0; i < numServers; i++) {
	    hostPtr->servers[i] = (ServerInfo *) Calloc(1, sizeof(ServerInfo));
	    hostPtr->servers[i]->name = server[i].name;


	    hostPtr->servers[i]->domains = (char **)
		    		Calloc(server[i].numDomains+1,sizeof(char *));
	    for (j = 0; j < server[i].numDomains; j++) {
		hostPtr->servers[i]->domains[j] = server[i].domain[j];
	    }
	    hostPtr->servers[i]->domains[j] = NULL;


	    hostPtr->servers[i]->addrList = (char **)
		  		Calloc(server[i].numAddresses+1,sizeof(char *));
	    for (j = 0; j < server[i].numAddresses; j++) {
		hostPtr->servers[i]->addrList[j] = server[i].address[j];
	    }
	    hostPtr->servers[i]->addrList[j] = NULL;

	}
	hostPtr->servers[i] = NULL;
    }


    if (queryType != T_A) {
	return(SUCCESS);
    } else {
	return(NONAUTH);
    }
}

/*
*******************************************************************************
*
*  GetHostInfo --
*
*	Retrieves host name, address and alias information
*	for a domain.
*
*  Results:
*	ERROR		- res_mkquery failed.
*	+ return values from GetAnswer()
*
*******************************************************************************
*/

int
GetHostInfo(nsAddrPtr, queryClass, queryType, name, hostPtr, isserver)
    struct in_addr	*nsAddrPtr;
    int 		queryClass;
    int 		queryType;
    char 		*name;
    HostInfo 		*hostPtr;
    Boolean		isserver;
{
    int 	 	n;
    int 	 	result;
    register char	*cp, **domain;
    extern char		*hostalias();

    /* catch explicit addresses */
    if (isdigit(*name) && (queryType == T_A)) {
	long ina;

	ina = inet_addr(name);

	if (ina == -1)
	    return(ERROR);

	hostPtr->name = Calloc(strlen(name)+3, 1);
	(void)sprintf(hostPtr->name,"[%s]",name);
	hostPtr->aliases = NULL;
	hostPtr->servers = NULL;
	hostPtr->addrType = AF_INET;
	hostPtr->addrLen = 4;
	hostPtr->addrList = (char **)Calloc(2, sizeof(char *));
	hostPtr->addrList[0] = Calloc(sizeof(long), sizeof(char));
	bcopy((char *)&ina,hostPtr->addrList[0],sizeof(ina));
	hostPtr->addrList[1] = 0;

	return(SUCCESS);
    }

    for (cp = name, n = 0; *cp; cp++)
	    if (*cp == '.')
		    n++;
    if ((n && *--cp == '.') ||
	(isserver == 0 && (_res.options & RES_DEFNAMES) == 0)) {
	    int defflag = _res.options & RES_DEFNAMES;

	    _res.options &= ~RES_DEFNAMES;
	    if (n && *cp == '.')
		    *cp = 0;
	    result = GetHostDomain(nsAddrPtr, queryClass, queryType,
		    name, (char *)NULL, hostPtr, isserver);
	    if (n && *cp == 0)
		    *cp = '.';
	    if (defflag)
		    _res.options |= RES_DEFNAMES;
	    return (result);
    }
    if (n == 0 && (cp = hostalias(name)))
	    return (GetHostDomain(nsAddrPtr, queryClass, queryType,
		    cp, (char *)NULL, hostPtr, isserver));
    for (domain = _res.dnsrch; *domain; domain++) {
	    result = GetHostDomain(nsAddrPtr, queryClass, queryType,
		    name, *domain, hostPtr, isserver);
	    if ((result != NXDOMAIN && result != NO_INFO) ||
		(_res.options & RES_DNSRCH) == 0)
		    return (result);
    }
    if (n)
	    return (GetHostDomain(nsAddrPtr, queryClass, queryType,
		    name, (char *)NULL, hostPtr, isserver));
    return (result);
}

GetHostDomain(nsAddrPtr, queryClass, queryType, name, domain, hostPtr, isserver)
    struct in_addr	*nsAddrPtr;
    int 		queryClass;
    int 		queryType;
    char 		*name, *domain;
    HostInfo		*hostPtr;
    Boolean		isserver;
{
    querybuf buf;
    char nbuf[2*MAXDNAME+2];
    int n;

    if (domain) {
	    (void)sprintf(nbuf, "%.*s.%.*s",
		    MAXDNAME, name, MAXDNAME, domain);
	    name = nbuf;
    }
    n = res_mkquery(QUERY, name, queryClass, queryType,
		    (char *)0, 0, (char *)0, (char *) &buf, sizeof(buf));
    if (n < 0) {
	if (_res.options & RES_DEBUG) {
	    printf("Res_mkquery failed\n");
	}
	return (ERROR);
    }

    n = GetAnswer(nsAddrPtr, queryType, (char *) &buf, n, 0, hostPtr,
	isserver);

    /*
     * GetAnswer didn't find a name, so set it to the specified one.
     */
    if (n == NONAUTH) {
	if (hostPtr->name == NULL) {
	    int len = strlen(name) + 1;
	    hostPtr->name = Calloc(len, sizeof(char));
	    bcopy(name, hostPtr->name, len);
	}
    }
    return(n);
}


/*
*******************************************************************************
*
*  FindHostInfo --
*
*	Performs an inverse query to find the host name
*	that corresponds to the given address.
*
*  Results:
*	ERROR		- res_mkquery failed.
*	+ return values from GetAnswer()
*
*******************************************************************************
*/

int
FindHostInfo(nsAddrPtr, address, len, hostPtr)
    struct in_addr 	*nsAddrPtr;
    struct in_addr	*address;
    int 		len;
    HostInfo 		*hostPtr;
{
    int 	 n;
    querybuf buf;

    n = res_mkquery(IQUERY, (char *)0, C_IN, T_A,
	    (char *)address, len, (char *)0, (char *) &buf, sizeof(buf));
    if (n < 0) {
	if (_res.options & RES_DEBUG) {
	    printf("Res_mkquery failed\n");
	}
	return (ERROR);
    }
    return(GetAnswer(nsAddrPtr, T_A, (char *) &buf, n, 1, hostPtr, 1));
}

/*
*******************************************************************************
*
*  FreeHostInfoPtr --
*
*	Deallocates all the calloc'd areas for a HostInfo variable.
*
*******************************************************************************
*/

void
FreeHostInfoPtr(hostPtr)
    register HostInfo *hostPtr;
{
    int i, j;

    if (hostPtr->name != NULL) {
	free(hostPtr->name);
	hostPtr->name = NULL;
    }

    if (hostPtr->aliases != NULL) {
	i = 0;
	while (hostPtr->aliases[i] != NULL) {
	    free(hostPtr->aliases[i]);
	    i++;
	}
	free((char *)hostPtr->aliases);
	hostPtr->aliases = NULL;
    }

    if (hostPtr->addrList != NULL) {
	i = 0;
	while (hostPtr->addrList[i] != NULL) {
	    free(hostPtr->addrList[i]);
	    i++;
	}
	free((char *)hostPtr->addrList);
	hostPtr->addrList = NULL;
    }

    if (hostPtr->servers != NULL) {
	i = 0;
	while (hostPtr->servers[i] != NULL) {

	    if (hostPtr->servers[i]->name != NULL) {
		free(hostPtr->servers[i]->name);
	    }

	    if (hostPtr->servers[i]->domains != NULL) {
		j = 0;
		while (hostPtr->servers[i]->domains[j] != NULL) {
		    free(hostPtr->servers[i]->domains[j]);
		    j++;
		}
		free((char *)hostPtr->servers[i]->domains);
	    }

	    if (hostPtr->servers[i]->addrList != NULL) {
		j = 0;
		while (hostPtr->servers[i]->addrList[j] != NULL) {
		    free(hostPtr->servers[i]->addrList[j]);
		    j++;
		}
		free((char *)hostPtr->servers[i]->addrList);
	    }
	    free((char *)hostPtr->servers[i]);
	    i++;
	}
	free((char *)hostPtr->servers);
	hostPtr->servers = NULL;
    }
}

/*
*******************************************************************************
*
*  GetHostList --
*
*	Performs a completion query when given an incomplete name.
*
*	Still under development.
*
*******************************************************************************
*/

#if  notdef

int
GetHostList(nsAddrPtr, queryType, name, defaultName, hostPtr)
    struct in_addr 	*nsAddrPtr;
    int 		queryType;
    char 		*name, *defaultName;
    HostInfo 		*hostPtr;
{
    int 	n;
    querybuf	buf;

    n = res_mkquery(CQUERYM, name, C_IN, T_A, defaultName, 0, (char *)0,
	    (char *) &buf, sizeof(buf));
    if (n < 0) {
	    if (_res.options & RES_DEBUG)
		    printf("Res_mkquery failed\n");
	    return (ERROR);
    }
    return(GetAnswer(nsAddrPtr, queryType, (char *)&buf, n, 0, hostPtr, 1));
}
#endif  notdef
