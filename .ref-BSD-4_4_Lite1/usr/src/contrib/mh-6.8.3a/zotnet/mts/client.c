/* client.c - connect to a server */
#ifndef	lint
static char ident[] = "@(#)$Id: client.c,v 1.21 1993/08/25 17:32:58 jromine Exp $";
#endif	/* lint */

#if	defined(SYS5) && defined(AUX)
#define	u_short	ushort
#define u_long  ulong
#endif


#if	defined(BSD42) || defined(SOCKETS)
#include "../h/strings.h"
#include <stdio.h>
#include "mts.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#ifndef	hpux
#include <arpa/inet.h>
#endif

#ifdef	HESIOD
#include <hesiod.h>
#endif

#ifdef KPOP
#include <krb.h>
#include <ctype.h>

static CREDENTIALS cred;
static MSG_DAT msg_data;
static KTEXT ticket = (KTEXT)NULL;
static Key_schedule schedule;
#endif	/* KPOP */

#define	NOTOK	(-1)
#define	OK	0
#define	DONE	1

#define	TRUE	1
#define	FALSE	0

#define	OOPS1	(-2)
#define	OOPS2	(-3)

#define	MAXARGS		1000
#define	MAXNETS		5
#define	MAXHOSTS	25

/*  */

extern int errno;
#ifndef	BSD44
extern int  sys_nerr;
extern char *sys_errlist[];
#endif


struct addrent {
    int     a_addrtype;		/* assumes AF_INET for inet_netof () */

    union {
	int	un_net;
	char    un_addr[14];
    } un;
#define	a_net	un.un_net
#define	a_addr	un.un_addr
};

static struct addrent *ne, *nz;
static struct addrent nets[MAXNETS];

static struct addrent *he, *hz;
static struct addrent hosts[MAXHOSTS];

#ifdef KPOP
char krb_realm[REALM_SZ];
char *PrincipalHostname();
static char *kservice;		/* "pop" if using kpop */
#endif /* KPOP */

#ifdef __STDC__
static int rcaux (struct servent *, struct hostent *, int, char *);
static int getport (int, int, char *);
static int inet (struct hostent *, int);
#else
static int rcaux (), getport (), inet ();
#endif


#if	defined(BIND) && !defined(h_addr)
#define	h_addr	h_addr_list[0]
#endif

#define	inaddr_copy(hp,sin) \
    bcopy ((hp) -> h_addr, (char *) &((sin) -> sin_addr), (hp) -> h_length)


struct hostent *gethostbystring ();


static int brkany ();
static char *getcpy (), **copyip (), **brkstring ();

/*  */

int	client (args, protocol, service, rproto, response)
char   *args,
       *protocol,
       *service,	/* "pop" or "pop/kpop" */
       *response;
int	rproto;
{
    int     sd;
    register char **ap;
    char   *arguments[MAXARGS];
    register struct hostent *hp;
#ifndef	BIND
    register struct netent *np;
#endif
    register struct servent *sp;
#ifdef	KPOP
    char *cp;

    if (cp = index (kservice = service, '/')) {	/* "pop/kpop" */
	*cp++ = '\0';		/* kservice = "pop" */
	service = cp;		/* service  = "kpop" */
    }
    else
	kservice = NULL;	/* not using KERBEROS */
#endif	/* KPOP */
    

    if ((sp = getservbyname (service, protocol)) == NULL) {
#ifdef	HESIOD
	if ((sp = hes_getservbyname (service, protocol)) == NULL) {
	    (void) sprintf (response, "%s/%s: unknown service",
		    protocol, service);
	    return NOTOK;
	}
#else
	(void) sprintf (response, "%s/%s: unknown service", protocol, service);
	return NOTOK;
#endif
    }

    ap = arguments;
    if (args != NULL && *args != 0)
	ap = copyip (brkstring (getcpy (args), " ", "\n"), ap);
    else
	if (servers != NULL && *servers != 0)
	    ap = copyip (brkstring (getcpy (servers), " ", "\n"), ap);
    if (ap == arguments) {
	*ap++ = getcpy ("localhost");
	*ap = NULL;
    }

    nz = (ne = nets) + sizeof nets / sizeof nets[0];
    hz = (he = hosts) + sizeof hosts / sizeof hosts[0];

    for (ap = arguments; *ap; ap++) {
	if (**ap == '\01') {
#ifndef	BIND
	    if (np = getnetbyname (*ap + 1)) {
		sethostent (1);
		while (hp = gethostent ())
		    if (np -> n_addrtype == hp -> h_addrtype
			    && inet (hp, np -> n_net)) {
			switch (sd = rcaux (sp, hp, rproto, response)) {
			    case NOTOK: 
				continue;
			    case OOPS1: 
				break;
			    case OOPS2: 
				return NOTOK;

			    default: 
				return sd;
			}
			break;
		    }
	    }
#endif
	    continue;
	}

	if (hp = gethostbystring (*ap)) {
	    switch (sd = rcaux (sp, hp, rproto, response)) {
		case NOTOK: 
		case OOPS1: 
		    break;
		case OOPS2: 
		    return NOTOK;

		default: 
		    return sd;
	    }
	    continue;
	}
    }

    (void) strcpy (response, "no servers available");
    return NOTOK;
}

/*  */

static int  rcaux (sp, hp, rproto, response)
register struct servent *sp;
register struct hostent *hp;
int	rproto;
register char *response;
{
    int     sd;
    struct in_addr  in;
    register struct addrent *ap;
    struct sockaddr_in  in_socket;
    register struct sockaddr_in *isock = &in_socket;
#ifdef KPOP
    int rem;
#endif	/* KPOP */

    for (ap = nets; ap < ne; ap++)
	if (ap -> a_addrtype == hp -> h_addrtype && inet (hp, ap -> a_net))
	    return NOTOK;

    for (ap = hosts; ap < he; ap++)
	if (ap -> a_addrtype == hp -> h_addrtype
		&& bcmp (ap -> a_addr, hp -> h_addr, hp -> h_length) == 0)
	    return NOTOK;

    if ((sd = getport (rproto, hp -> h_addrtype, response)) == NOTOK)
	return OOPS2;

    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = hp -> h_addrtype;
    inaddr_copy (hp, isock);
    isock -> sin_port = sp -> s_port;

    if (connect (sd, (struct sockaddr *) isock, sizeof *isock) == NOTOK)
	switch (errno) {
	    case ENETDOWN: 
	    case ENETUNREACH: 
		(void) close (sd);
		if (ne < nz) {
		    ne -> a_addrtype = hp -> h_addrtype;
		    bcopy (hp -> h_addr, (char *) &in, sizeof in);
		    ne -> a_net = inet_netof (in);
		    ne++;
		}
		return OOPS1;

	    case ETIMEDOUT: 
	    case ECONNREFUSED: 
	    default: 
		(void) close (sd);
		if (he < hz) {
		    he -> a_addrtype = hp -> h_addrtype;
		    bcopy (hp -> h_addr, he -> a_addr, hp -> h_length);
		    he++;
		}
		return NOTOK;
	}

#ifdef KPOP
    if (kservice) {	/* "pop" */
	ticket = (KTEXT)malloc( sizeof(KTEXT_ST) );
	rem = krb_sendauth(0L, sd, ticket, kservice, hp->h_name,
			   (char *) krb_realmofhost(hp->h_name),
			   (unsigned long)0, &msg_data, &cred, schedule,
			   (struct sockaddr_in *)NULL,
			   (struct sockaddr_in *)NULL,
			   "KPOPV0.1");
	if (rem != KSUCCESS) {
	    close(sd);
	    (void) strcpy(response, "Post office refused connection: ");
	    (void) strcat(response, krb_err_txt[rem]);
	    return OOPS2;
	}
    }
#endif	/* KPOP */

    return sd;
}

/*  */

static int getport (rproto, addrtype, response)
int	rproto,
	addrtype;
register char *response;
{
    int     sd,
            port;
    struct sockaddr_in  in_socket,
                       *isock = &in_socket;

    if (rproto && addrtype != AF_INET) {
	(void) sprintf (response, "reserved ports not supported for af=%d",
		addrtype);
	errno = ENOPROTOOPT;
	return NOTOK;
    }

    if ((sd = socket (AF_INET, SOCK_STREAM, 0)) == NOTOK) {
	(void) sprintf (response, "unable to create socket: %s",
		errno > 0 && errno < sys_nerr ? sys_errlist[errno]
		: "unknown error");
	return NOTOK;
    }
#ifdef KPOP
    if (kservice)	/* "pop" */
	return(sd);
#endif	/* KPOP */
    if (!rproto)
	return sd;

    bzero ((char *) isock, sizeof *isock);
    isock -> sin_family = addrtype;
    for (port = IPPORT_RESERVED - 1;;) {
	isock -> sin_port = htons ((u_short) port);
	if (bind (sd, (struct sockaddr *) isock, sizeof *isock) != NOTOK)
	    return sd;

	switch (errno) {
	    case EADDRINUSE: 
	    case EADDRNOTAVAIL: 
		if (--port <= IPPORT_RESERVED / 2) {
		    (void) strcpy (response, "ports available");
		    return NOTOK;
		}
		break;

	    default: 
		(void) sprintf (response, "unable to bind socket: %s",
			errno > 0 && errno < sys_nerr ? sys_errlist[errno]
			: "unknown error");
		return NOTOK;
	}
    }
}

/*  */

static int  inet (hp, net)
register struct hostent *hp;
int	net;
{
    struct in_addr  in;

    bcopy (hp -> h_addr, (char *) &in, sizeof in);
    return (inet_netof (in) == net);
}

/*  */

/* taken from ISODE's compat/internet.c */

#ifndef	DG
u_long	inet_addr ();
#else
struct in_addr inet_addr ();
#endif


static char *empty = NULL;
#ifdef	h_addr
static char *addrs[2] = { NULL };
#endif

struct hostent *gethostbystring (s)
char   *s;
{
    register struct hostent *h;
#ifndef	DG
    static u_long iaddr;
#else
    static struct in_addr iaddr;
#endif
    static struct hostent   hs;

    iaddr = inet_addr (s);
#ifndef	DG
    if (iaddr == NOTOK && strcmp (s, "255.255.255.255"))
#else
    if (iaddr.s_addr == NOTOK && strcmp (s, "255.255.255.255"))
#endif
	return gethostbyname (s);

    h = &hs;
    h -> h_name = s;
    h -> h_aliases = &empty;
    h -> h_addrtype = AF_INET;
    h -> h_length = sizeof (iaddr);
#ifdef	h_addr
    h -> h_addr_list = addrs;
    bzero ((char *) addrs, sizeof addrs);
#endif
    h -> h_addr = (char *) &iaddr;

    return h;
}

/*  */

/* static copies of three MH subroutines... (sigh) */

static char *broken[MAXARGS + 1];


static char **brkstring (strg, brksep, brkterm)
register char  *strg;
register char  *brksep,
               *brkterm;
{
    register int    bi;
    register char   c,
                   *sp;

    sp = strg;

    for (bi = 0; bi < MAXARGS; bi++) {
	while (brkany (c = *sp, brksep))
	    *sp++ = 0;
	if (!c || brkany (c, brkterm)) {
	    *sp = 0;
	    broken[bi] = 0;
	    return broken;
	}

	broken[bi] = sp;
	while ((c = *++sp) && !brkany (c, brksep) && !brkany (c, brkterm))
	    continue;
    }
    broken[MAXARGS] = 0;

    return broken;
}


static  brkany (chr, strg)
register char   chr,
               *strg;
{
    register char  *sp;

    if (strg)
	for (sp = strg; *sp; sp++)
	    if (chr == *sp)
		return 1;
    return 0;
}


static char **copyip (p, q)
register char **p,
              **q;
{
    while (*p)
	*q++ = *p++;
    *q = 0;

    return q;
}


static char *getcpy (str)
register char  *str;
{
    register char  *cp;

    if ((cp = malloc ((unsigned) (strlen (str) + 1))) == NULL)
	return NULL;

    (void) strcpy (cp, str);
    return cp;
}
#endif	/* BSD42 or SOCKETS */
