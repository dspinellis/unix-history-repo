/****************************************************************
 * check_soa -- Retrieve the SOA record from each name server   *
 *     for a given domain and print out the serial number.      *
 *                                                              *
 * usage: check_soa domain                                      *
 *                                                              *
 * The following errors are reported:                           *
 *     o There is no address for a server.                      *
 *     o There is no server running on this host.               *
 *     o There was no response from a server.                   *
 *     o The server is not authoritative for the domain.        *
 *     o The response had an error response code.               *
 *     o The response had more than one answer.                 *
 *     o The response answer did not contain an SOA record.     *
 *     o The expansion of a compressed domain name failed.      *
 ****************************************************************/

/* Various header files */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <string.h>

/* Error variables */
extern int h_errno;  /* for resolver errors */
extern int errno;    /* general system errors */

/* Our own routines; code included later in this chapter */
void nsError();            /* report resolver errors */
void findNameServers();    /* find a domain's name servers */
void queryNameServers();   /* grab SOA records from servers */
void returnCodeError();    /* report response packet errors */
int  skipToData();         /* skip to the resource record data */
int  skipName();           /* skip a compressed name */

/* Maximum number of name servers we will check */
#define NSLIMIT 20

main(argc, argv)
int argc;
char *argv[];
{
    char *nsList[NSLIMIT]; /* list of name servers */
    int  nsNum = 0;        /* number of name servers in list */

    /* sanity check: one (and only one) argument? */
    if(argc != 2){
        (void) fprintf(stderr, "usage: %s domain\n", argv[0]);
        exit(1);
    }

    (void) res_init();

    /* 
     * Find the name servers for the domain.
     * The name servers are written into nsList.
     */
    findNameServers(argv[1], nsList, &nsNum);

    /* 
     * Query each name server for the domain's SOA record.
     * The name servers are read from nsList.
     */
    queryNameServers(argv[1], nsList, nsNum);

    exit(0);
}

/****************************************************************
 * findNameServers -- find all of the name servers for the      *
 *     given domain and store their names in nsList.  nsNum is  *
 *     the number of servers in the nsList array.               *
 ****************************************************************/
void
findNameServers(domain, nsList, nsNum)
char *domain;
char *nsList[];
int  *nsNum;
{
    union {
        HEADER hdr;           /* defined in resolv.h */
        u_char buf[PACKETSZ]; /* defined in arpa/nameser.h */
    } response;               /* response buffers */
    int responseLen;          /* buffer length */

    u_char  *cp;       /* character pointer to parse DNS packet */
    u_char  *endOfMsg; /* need to know the end of the message */
    u_short class;     /* classes defined in arpa/nameser.h */
    u_short type;      /* types defined in arpa/nameser.h */
    u_long  ttl;       /* resource record time to live */
    u_short dlen;      /* size of resource record data */

    int i, count, dup; /* misc variables */

    /* 
     * Look up the NS records for the given domain name.
     * We expect the domain to be a fully qualified name, so
     * we use res_query().  If we wanted the resolver search 
     * algorithm, we would have used res_search() instead.
     */
    if((responseLen = 
           res_query(domain,      /* the domain we care about   */
                     C_IN,        /* Internet class records     */
                     T_NS,        /* Look up name server records*/
                     (u_char *)&response,      /*response buffer*/
                     sizeof(response)))        /*buffer size    */
                                        < 0){  /*If negative    */
        nsError(h_errno, domain); /* report the error           */
        exit(1);                  /* and quit                   */
    }

    /*
     * Keep track of the end of the message so we don't 
     * pass it while parsing the response.  responseLen is 
     * the value returned by res_query.
     */
    endOfMsg = response.buf + responseLen;

    /*
     * Set a pointer to the start of the question section, 
     * which begins immediately AFTER the header.
     */
    cp = response.buf + sizeof(HEADER);

    /*
     * Skip over the whole question section.  The question 
     * section is comprised of a name, a type, and a class.  
     * QFIXEDSZ (defined in arpa/nameser.h) is the size of 
     * the type and class portions, which is fixed.  Therefore, 
     * we can skip the question section by skipping the 
     * name (at the beginning) and then advancing QFIXEDSZ.
     * After this calculation, cp points to the start of the 
     * answer section, which is a list of NS records.
     */
    cp += skipName(cp, endOfMsg) + QFIXEDSZ;

    /*
     * Create a list of name servers from the response.
     * NS records may be in the answer section and/or in the
     * authority section depending on the DNS implementation.  
     * Walk through both.  The name server addresses may be in
     * the additional records section, but we will ignore them
     * since it is much easier to call gethostbyname() later
     * than to parse and store the addresses here.
     */
    count = ntohs(response.hdr.ancount) + 
            ntohs(response.hdr.nscount);
    while (    (--count >= 0)        /* still more records     */
            && (cp < endOfMsg)       /* still inside the packet*/
            && (*nsNum < NSLIMIT)) { /* still under our limit  */

        /* Skip to the data portion of the resource record */
        cp += skipToData(cp, &type, &class, &ttl, &dlen, endOfMsg);

        if (type == T_NS) {

            /*
             * Allocate storage for the name.  Like any good 
             * programmer should, we test malloc's return value, 
             * and quit if it fails.
             */
            nsList[*nsNum] = (char *) malloc (MAXDNAME);
            if(nsList[*nsNum] == NULL){
                (void) fprintf(stderr, "malloc failed\n");
                exit(1);
            }

            /* Expand the name server's name */
            if (dn_expand(response.buf, /* Start of the packet   */
                          endOfMsg,     /* End of the packet     */
                          cp,           /* Position in the packet*/
                          (u_char *)nsList[*nsNum], /* Result    */
                          MAXDNAME)     /* size of nsList buffer */
                                    < 0) { /* Negative: error    */
                (void) fprintf(stderr, "dn_expand failed\n");
                exit(1);
            }

            /*
             * Check the name we've just unpacked and add it to 
             * the list of servers if it is not a duplicate.
             * If it is a duplicate, just ignore it.
             */
            for(i = 0, dup=0; (i < *nsNum) && !dup; i++)
                dup = !strcasecmp(nsList[i], nsList[*nsNum]);
            if(dup) 
                free(nsList[*nsNum]);
            else
                (*nsNum)++;
        }

        /* Advance the pointer over the resource record data */
        cp += dlen;

    } /* end of while */
}

/****************************************************************
 * queryNameServers -- Query each of the name servers in nsList *
 *     for the SOA record of the given domain.  Report any      *
 *     errors encountered.  (e.g., a name server not running or *
 *     the response not being an authoritative response.)  If   *
 *     there are no errors, print out the serial number for the *
 *     domain.                                                  *
 ****************************************************************/
void
queryNameServers(domain, nsList, nsNum)
char *domain;
char *nsList[];
int nsNum;
{
    union {
        HEADER hdr;            /* defined in resolv.h */
        u_char buf[PACKETSZ];  /* defined in arpa/nameser.h */
    } query, response;         /* query and response buffers */
    int responseLen, queryLen; /* buffer lengths */

    u_char  *cp;       /* character pointer to parse DNS packet */
    u_char  *endOfMsg; /* need to know the end of the message */
    u_short class;     /* classes defined in arpa/nameser.h */
    u_short type;      /* types defined in arpa/nameser.h */
    u_long  ttl;       /* resource record time to live */
    u_short dlen;      /* size of resource record data */

    struct in_addr saveNsAddr[MAXNS];  /* addrs saved from _res */
    int nsCount;          /* count of addresses saved from _res */
    struct hostent *host; /* structure for looking up ns addr */
    int i;                /* counter variable */

    /*
     * Save the _res name server list since 
     * we will need to restore it later.
     */
    nsCount = _res.nscount;
    for(i = 0; i < nsCount; i++)
      saveNsAddr[i] = _res.nsaddr_list[i].sin_addr;

    /*
     * Turn off the search algorithm and turn off appending 
     * the default domain before we call gethostbyname(); the 
     * name server names will be fully qualified.
     */
    _res.options &= ~(RES_DNSRCH | RES_DEFNAMES);

    /*
     * Query each name server for an SOA record.
     */
    for(nsNum-- ; nsNum >= 0; nsNum--){

        /* 
         * First, we have to get the IP address of every server.
         * So far, all we have are names.  We use gethostbyname
         * to get the addresses, rather than anything fancy.
         * But first, we have to restore certain values in _res 
         * because _res affects gethostbyname().  (We altered
         * _res in the previous iteration through the loop.)
         *
         * We can't just call res_init() again to restore
         * these values since some of the _res fields are 
         * initialized when the variable is declared, not when 
         * res_init() is called.
         */
        _res.options |= RES_RECURSE;  /* recursion on (default) */
        _res.retry = 4;               /* 4 retries (default)    */
        _res.nscount = nsCount;       /* original name servers  */
        for(i = 0; i < nsCount; i++)
            _res.nsaddr_list[i].sin_addr = saveNsAddr[i];

        /* Look up the name server's address */
        host = gethostbyname(nsList[nsNum]);
        if (host == NULL) {
            (void) fprintf(stderr,"There is no address for %s\n",
                                              nsList[nsNum]);
            continue; /* nsNum for-loop */
        }

        /*
         * Now get ready for the real fun.  host contains IP 
         * addresses for the name server we're testing.
         * Store the first address for host in the _res 
         * structure.  Soon, we'll look up the SOA record...
         */
        (void) memcpy((void *)&_res.nsaddr_list[0].sin_addr,
           (void *)host->h_addr_list[0], (size_t)host->h_length);
        _res.nscount = 1;

        /*
         * Turn off recursion.  We don't want the name server
         * querying another server for the SOA record; this name 
         * server ought to be authoritative for this data.
         */
        _res.options &= ~RES_RECURSE;

        /*
         * Reduce the number of retries.  We may be checking
         * several name servers, so we don't want to wait too
         * long for any one server.  With two retries and only
         * one address to query, we'll wait at most 15 seconds.
         */
        _res.retry = 2;

        /*
         * We want to see the response code in the next
         * response, so we must make the query packet and 
         * send it ourselves instead of having res_query()
         * do it for us.  If res_query() returned -1, there
         * might not be a response to look at.  
         *
         * There is no need to check for res_mkquery() 
         * returning -1.  If the compression was going to 
         * fail, it would have failed when we called 
         * res_query() earlier with this domain name.
         */
        queryLen = res_mkquery(
                     QUERY,           /* regular query         */
                     domain,          /* the domain to look up */
                     C_IN,            /* Internet type         */
                     T_SOA,           /* Look up an SOA record */
                     (char *)NULL,    /* always NULL           */
                     0,               /* length of NULL        */
                     (struct rrec *)NULL, /* always NULL       */
                     (char *)&query,  /* buffer for the query  */
                     sizeof(query));  /* size of the buffer    */

        /*
         * Send the query packet.  If there is no name server
         * running on the target host, res_send() returns -1
         * and errno is ECONNREFUSED.  First, clear out errno.
         */
        errno = 0;
        if((responseLen = res_send((char *)&query, /* the query  */
                                    queryLen,      /* true length*/
                                    (char *)&response, /*buffer  */
                                    sizeof(response))) /*buf size*/
                                        < 0){          /* error  */
            if(errno == ECONNREFUSED) { /* no server on the host */
                (void) fprintf(stderr, 
                    "There is no name server running on %s\n",
                    nsList[nsNum]);
            } else {                    /* anything else: no response */
                (void) fprintf(stderr, 
                    "There was no response from %s\n", 
                    nsList[nsNum]);
            }
            continue; /* nsNum for-loop */
        }

        /*
         * Set up the pointers to parse the response.
         * We set up two pointers: one to the end of the message 
         * (so we can test for overruns) and one to the question 
         * section (which we'll move as we parse the response).
         */
        endOfMsg = response.buf + responseLen;
        cp = response.buf + sizeof(HEADER);             

        /*
         * If the response reports an error, issue a message
         * and proceed to the next server in the list.
         */
        if(response.hdr.rcode != NOERROR){
            returnCodeError((int)response.hdr.rcode, 
                                                  nsList[nsNum]);
            continue; /* nsNum for-loop */
        }

        /*
         * Did we receive an authoritative response?  Check the 
         * authoritative answer bit.  If the server isn't
         * authoritative, report it, and go on to the next server.
         */
        if(!response.hdr.aa){
            (void) fprintf(stderr, 
                "%s is not authoritative for %s\n",
                nsList[nsNum], domain);
            continue; /* nsNum for-loop */
        }

        /* 
         * The response should only contain one answer; if more,
         * report the error, and proceed to the next server.
         */
        if(ntohs(response.hdr.ancount) != 1){
            (void) fprintf(stderr, 
                "%s: expected 1 answer, got %d\n",
                nsList[nsNum], ntohs(response.hdr.ancount));
            continue; /* nsNum for-loop */
        }

        /* 
         * Skip the question section (we know what we asked, 
         * don't we?).  cp now points to the answer section.
         */
        cp += skipName(cp, endOfMsg) + QFIXEDSZ;

        /* 
         * cp is now pointing at a resource record in the answer 
         * section.  Skip to the data portion of this record;
         * in the process, extract the type, class, etc. 
         */
        cp += skipToData(cp, &type, &class, &ttl, &dlen, endOfMsg);

        /* 
         * We asked for an SOA record; if we got something else,
         * report the error and proceed to the next server.
         */
        if (type != T_SOA) {
            (void) fprintf(stderr, 
                "%s: expected answer type %d, got %d\n",
                nsList[nsNum], T_SOA, type);
            continue; /* nsNum for-loop */
        }

        /* 
         * Skip the SOA origin and mail address, which we don't
         * care about.  Both are standard "compressed names."
         */
        cp += skipName(cp, endOfMsg);
        cp += skipName(cp, endOfMsg);

        /* cp now points to the serial number; print it. */
        (void) printf("%s has serial number %d\n", 
            nsList[nsNum], _getlong(cp));

    } /* end of nsNum for-loop */
}

/****************************************************************
 * skipName -- This routine skips over a domain name.  If the   *
 *     domain name expansion fails, it reports an error and     *
 *     exits.  dn_skipname() is probably not on your manual     *
 *     page; it is similar to dn_expand() except that it just   *
 *     skips over the name.  dn_skipname() is in res_comp.c if  *
 *     you need to find it.                                     *
 ****************************************************************/
int
skipName(cp, endOfMsg)
u_char *cp;
u_char *endOfMsg;
{
    int n;

    if((n = dn_skipname(cp, endOfMsg)) < 0){
        (void) fprintf(stderr, "dn_skipname failed\n");
        exit(1);
    }
    return(n);
}

/****************************************************************
 * skipToData -- This routine advances the cp pointer to the    *
 *     start of the resource record data portion.  On the way,  *
 *     it fills in the type, class, ttl, and data length        *
 ****************************************************************/
int
skipToData(cp, type, class, ttl, dlen, endOfMsg)
u_char  *cp;
u_short *type;
u_short *class;
u_long  *ttl;
u_short *dlen;
u_char  *endOfMsg;
{
    u_char *tmp_cp = cp;  /* temporary version of cp */

    /* Skip the domain name; it matches the name we looked up */
    tmp_cp += skipName(tmp_cp, endOfMsg);

    /*
     * Grab the type, class, and ttl.  The routines called
     * _getshort() and _getlong() are also resolver routines 
     * you may not find in a manual page.  They are in 
     * res_comp.c if you want to see them.
     */
    *type = _getshort(tmp_cp);
    tmp_cp += sizeof(u_short);
    *class = _getshort(tmp_cp);
    tmp_cp += sizeof(u_short);
    *ttl = _getlong(tmp_cp);
    tmp_cp += sizeof(u_long);
    *dlen = _getshort(tmp_cp);
    tmp_cp += sizeof(u_short);

    return(tmp_cp - cp);
}

/****************************************************************
 * nsError -- Print an error message from h_errno for a failure *
 *     looking up NS records.  res_query() converts the DNS     *
 *     packet return code to a smaller list of errors and       *
 *     places the error value in h_errno.  There is a routine   *
 *     called herror() for printing out strings from h_errno    *
 *     like perror() does for errno.  Unfortunately, the        *
 *     herror() messages assume you are looking up address      *
 *     records for hosts.  In this program, we are looking up   *
 *     NS records for domains, so we need our own list of error *
 *     strings.                                                 *
 ****************************************************************/
void
nsError(error, domain)
int error;
char *domain;
{
    switch(error){
        case HOST_NOT_FOUND:
          (void) fprintf(stderr, "Unknown domain: %s\n", domain);
          break;
        case NO_DATA:
          (void) fprintf(stderr, "No NS records for %s\n", domain); 
          break;
        case TRY_AGAIN:
          (void) fprintf(stderr, "No response for NS query\n");
          break;
        default:
          (void) fprintf(stderr, "Unexpected error\n");
          break;
    }
}

/****************************************************************
 * returnCodeError -- print out an error message from a DNS     *
 *     response return code.                                    *
 ****************************************************************/
void
returnCodeError(rcode, nameserver)
int rcode;
char *nameserver;
{
    (void) fprintf(stderr, "%s: ", nameserver);
    switch(rcode){
        case FORMERR:
          (void) fprintf(stderr, "FORMERR response\n");
          break;
        case SERVFAIL:
          (void) fprintf(stderr, "SERVFAIL response\n");
          break;
        case NXDOMAIN:
          (void) fprintf(stderr, "NXDOMAIN response\n");
          break;
        case NOTIMP:
          (void) fprintf(stderr, "NOTIMP response\n");
          break;
        case REFUSED:
          (void) fprintf(stderr, "REFUSED response\n");
          break;
        default:
          (void) fprintf(stderr, "unexpected return code\n");
          break;
    }
}
