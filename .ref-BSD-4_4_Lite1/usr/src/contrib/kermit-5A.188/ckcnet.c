char *cknetv = "Network support, 5A(015) 23 Nov 92";

/*  C K C N E T  --  Network support  */
/*
  Authors:

  Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
    Columbia University Center for Computing Activities.
  netopen() routine for TCP/IP originally by Ken Yap, Rochester University
    (ken@cs.rochester.edu) (no longer at that address).
  Missing pieces for Excelan sockets library from William Bader, Moravian
    College <bader@moravian.edu>.
  TELNET protocol by Frank da Cruz.
  TGV MultiNet code by Frank da Cruz.
  MultiNet code adapted to WIN/TCP by Ray Hunter of TWG.
  MultiNet code adapted to DEC TCP/IP by Lee Tibbert of DEC and Frank da Cruz.
  SunLink X.25 support by Marcello Frutig, Catholic University,
    Rio de Janeiro, Brazil (frutig@rnp.impa.br) with fixes from
    Stefaan Eeckels, Eurokom, Luxembourg.
  Other contributions as indicated below.

  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/

#include "ckcdeb.h"
#include "ckcker.h"
#ifdef I386IX				/* Has to come before ckcnet.h in */
#include <errno.h>			/* this version, but after in others */
#endif /* I386IX */
#include "ckcnet.h"

#ifdef NETCONN
/* Don't need these if there is no network support. */

#ifdef WINTCP

#include <errno.h>
#include <setjmp.h>
#include <signal.h>
/*
 * The WIN/TCP code path is the same as that for Multinet.  Only the routine
 * names have changed ...
 */
#define socket_errno 	errno
#define socket_read 	netread
#define socket_ioctl	ioctl
#define socket_write 	netwrite
#define socket_perror   win$perror
#define socket_close 	netclose

#else /* Not WINTCP */
#ifndef I386IX
#include <errno.h>
#endif /* I386IX */
#include <signal.h>
#ifndef ZILOG
#include <setjmp.h>
#else
#include <setret.h>
#endif /* ZILOG */
#endif /* WINTCP */

#ifdef datageneral			/* Data General AOS/VS */
#include <:usr:include:vs_tcp_errno.h>
#include <:usr:include:sys:vs_tcp_types.h>
#include <:usr:include:sys:socket.h>
#include <:usr:include:netinet:in.h>
#include <:usr:include:netdb.h>
#endif /* datageneral */

extern SIGTYP (*saval)();		/* For saving alarm handler */

_PROTOTYP( VOID bgchk, (void) );

extern int				/* External variables */
  duplex, debses, seslog, ttyfd, quiet, msgflg;

#ifdef SVR4
/*
  These suggested by Rob Healey, rhealey@kas.helios.mn.org, to avoid
  bugs in Berkeley compatibility library on Sys V R4 systems, but untested
  by me (fdc).  Remove this bit if it gives you trouble.
  (Later corrected by Marc Boucher <mboucher@iro.umontreal.ca> because
  bzero/bcopy are not argument-compatible with memset/memcpy|memmove.)
*/
#define bzero(s,n) memset(s,0,n)
#define bcopy(h,a,l) memmove(a,h,l)
#else
#ifdef PTX				/* Sequent DYNIX PTX 1.3 */
#define bzero(s,n) memset(s,0,n)
#define bcopy(h,a,l) memcpy(a,h,l)
#endif /* PTX */
#endif /* SVR4 */

#define NAMECPYL 100			/* Local copy of hostname */
static char namecopy[NAMECPYL];

char ipaddr[20] = { '\0' };		/* Global copy of IP address */

/*
  VMSTCPIP means "DEC_TCPIP or MULTINET or WINTCP" (defined in ckcnet.h).
*/
#ifdef VMSTCPIP
/*
  General global variables, but so far used only by MultiNet and WIN/TCP.
  Kept within #ifdef MULTINET..#endif to keep strict compilers (and lint)
  from complaining about unused variables.
*/
static jmp_buf njbuf;			/* For timeout longjumps */
#endif /* VMSTCPIP */

#endif /* NETCONN */

int ttnet = NET_NONE;			/* Network type */
int ttnproto = NP_NONE;			/* Network virtual terminal protocol */
int tn_init = 0;			/* Telnet protocol initialized flag */
int tn_duplex = 1;			/* Initial echo status */
char *tn_term = NULL;			/* Terminal type override */
int tn_nlm = 1;				/* Telnet CR -> CR LF mode */

#ifndef NETCONN
/*
  Network support not defined.
  Dummy functions here in case #ifdef's forgotten elsewhere.
*/
int					/* Open network connection */
netopen(name, lcl, nett) char *name; int *lcl, nett; {
    return(-1);
}
int					/* Close network connection */
netclos() {
    return(-1);
}
int					/* Check network input buffer */
nettchk() {
    return(-1);
}
int					/* Flush network input buffer */
netflui() {
    return(-1);
}
int					/* Send network BREAK */
netbreak() {
    return(-1);
}
int					/* Input character from network */
netinc(timo) int timo; {
}
int					/* Output character to network */
#ifdef CK_ANSIC
nettoc(char c)
#else
nettoc(c) char c;
#endif /* CK_ANSIC */
/* nettoc */ {
    return(-1);
}
int
nettol(s,n) char *s; int n; {
    return(-1);
}

#else /* NETCONN is defined (rest of this module...) */

#ifdef VMSTCPIP

/* For buffered network reads... */
/*
  If the buffering code is written right, it shouldn't matter how long this
  buffer is -- it could even be shorter than a Kermit packet.
*/
#define TTIBUFL 8192			/* Maybe 8K?... */

CHAR 	ttibuf[TTIBUFL+1];
int 	ttibp = 0, ttibn = 0;
/*
  Read bytes from network into internal buffer ttibuf[].
  To be called when input buffer is empty, i.e. when ttibn == 0.

  Other network reading routines, like ttinc, ttinl, ttxin, should check the
  internal buffer first, and call this routine for a refill if necessary.

  Returns -1 on error, 0 if nothing happens.  When data is read successfully,
  returns number of bytes read, and sets global ttibn to that number and
  ttibp (the buffer pointer) to zero.
*/
int
ttbufr() {				/* TT Buffer Read */
    int count;

    if (ttnet != NET_TCPB) {		/* First make sure current net is */
	return(-1);			/* TCP/IP; if not, do nothing. */
    } else {
	if (ttibn > 0)			/* Out internal buffer is not empty, */
	  return(ttibn);		/* so keep using it. */
#ifdef WINTCP
	count = 512;			/* This works for WIN/TCP */
#else					/* Not WINTCP, i.e it's Multinet */
#ifdef DEC_TCPIP
	count = 512;			/* This works for WIN/TCP */
#else					/* Not WINTCP, i.e it's Multinet */
	count = nettchk();		/* Check network input buffer, */
	if (ttibn > 0) return(ttibn);	/* which can put a char there! */
	if (count < 0)			/* Read error */
	  return(-1);
	else if (count > TTIBUFL)	/* Too many to read */
	  count = TTIBUFL;
	else if (count == 0)		/* None, so force blocking read */
	  count = 1;
#endif /* DEC_TCPIP */
#endif /* WINTCP */
	debug(F101,"ttbufr count 1","",count);

#ifdef COMMENT
/*
 This is for nonblocking reads, which we don't do any more.  This code didn't
 work anyway, in the sense that a broken connection was never sensed.
*/
	if ((count = socket_read(ttyfd,ttibuf,count)) < 1) {
	    if (count == -1 && socket_errno == EWOULDBLOCK) {
		debug(F100,"ttbufr finds nothing","",0);
		return(0);
	    } else if (count == 0) {
		debug(F100,"ttbufr socket eof","",0);		
		return(-1);
	    } else {
		debug(F101,"ttbufr socket_read error","",socket_errno);
		return(-1);
	    }
	}
#else
/* This is for blocking reads */
	if ((count = socket_read(ttyfd,ttibuf,count)) < 1) {
	    debug(F101,"ttbufr socket_read","",count);
	    debug(F101,"ttbufr socket_errno","",socket_errno);
	    return(-1);
	}
#endif /* COMMENT */
	ttibp = 0;			/* Reset buffer pointer. */
	ttibn = count;
#ifdef DEBUG
	debug(F101,"ttbufr count 2","",count); /* Got some bytes. */
	if (count > 0) ttibuf[count] = '\0';
	debug(F111,"ttbufr ttibuf",ttibuf,ttibp);
#endif /* DEBUG */
	return(ttibn);			/* Return buffer count. */
    }
}
#endif /* VMSTCPIP */

/*
  C-Kermit network open/close functions for BSD-sockets.
  Much of this code shared by SunLink X.25, which also uses the socket library.
*/

/*  N E T O P E N  --  Open a network connection.  */

/*  Returns 0 on success, -1 on failure.  */

#define	TELNET_PORT	23	   /* Should do lookup, but it won't change */

/* This symbol is not known to, e.g., Ultrix 2.0 */
#ifndef TELOPT_TTYPE
#define TELOPT_TTYPE 24
#endif /* TELOPT_TTYPE */

/*  N E T O P N  --  Open a network connection.  */
/*
  Call with:
    name of host (or host:service),
    lcl - local-mode flag to be set if this function succeeds,
    network type - value defined in ckunet.h.
*/

#ifdef EXCELAN
/*
  Most other BSD sockets implementations define these in header files
  and libraries.
*/
struct servent {
    unsigned short s_port;
};

struct hostent {
    short h_addrtype;
    struct in_addr h_addr;
    int h_length;
};

struct servent *
getservbyname(service, connection) char *service,*connection; {
    static struct servent servrec;
    int port;

    port = 0;
    if (strcmp(service, "telnet") == 0) port = 23;
    else if (strcmp(service, "smtp") == 0) port = 25;
    else port = atoi(service);

    debug(F101,"getservbyname return port ","",port);

    if (port > 0) {
    	servrec.s_port = htons(port);
    	return( &servrec );
    }
    return( (struct servent *) NULL );
}

struct hostent *
gethostbyname(hostname) char *hostname; {
    return( (struct hostent *) NULL );
}

unsigned long
inet_addr(name) char *name; {
    unsigned long addr;

    addr = rhost(&name);
    debug(F111,"inet_addr ",name,(int)addr);
    return(addr);
}

char *
inet_ntoa(in) struct in_addr in; {
    static char name[80];
    sprintf(name, "%d.%d.%d.%d", in.s_net, in.s_host, in.s_lh, in.s_impno);
    return(name);
}
#endif /* EXCELAN */

/*  N E T O P E N  --  Open a network connection  */
/*
  Calling conventions same as ttopen(), except third argument is network
  type rather than modem type.  Designed to be called from within ttopen.
*/
int
netopen(name, lcl, nett) char *name; int *lcl, nett; {
    char *p;
#ifdef SO_OOBINLINE
    int on = 1;
#endif /* SO_OOBINLINE */
    int i, x;
    struct servent *service, servrec;
    struct hostent *host;
    struct sockaddr_in saddr;
#ifdef EXCELAN
    struct sockaddr_in send_socket;
#endif /* EXCELAN */

#ifdef SUNX25				/* Code for SunLink X.25 support */
#define X29PID 1			/* X.29 Protocol ID */
    VOID x25oobh();
    CONN_DB x25host;
    FACILITY_DB x25facil;
    static int needh = 1;
    PID_T pid;
    extern int linkid, lcn, x25ver;
    extern int revcall, closgr, cudata;
    extern char udata[MAXCUDATA];
#endif /* SUNX25 */

    debug(F101,"netopen nett","",nett);
    *ipaddr = '\0';			/* Initialize IP address string */

#ifdef SUNX25
    if (nett == NET_SX25) {		/* If network type is X.25 */
        netclos();			/* Close any previous net connection */
        ttnproto = NP_NONE;		/* No protocol selected yet */

        /* Set up host structure */
        bzero ((char *)&x25host,sizeof(x25host));
        if ((x25host.hostlen = pkx121 (name,x25host.host)) < 0) {
            fprintf (stderr,"Invalid X.121 host address %s\n",name);
            errno = 0;
            return (-1);
        }
        x25host.datalen = X29PIDLEN;
        x25host.data[0] = X29PID;

	/* Set call user data if specified */
        if (cudata) {
            strncpy(x25host.data+X29PIDLEN,udata,(int)strlen(udata));
            x25host.datalen += (int)strlen(udata);
        }

        /* Open SunLink X.25 socket */
        if ((ttyfd = socket (AF_X25, SOCK_STREAM, 0)) < 0) {
	    debug(F101,"netopen socket error","",errno);
            perror ("X.25 connect socket error");
            return (-1);
        }

        /* Setting X.25 out-of-band data handler */
        pid = getpid();
        if (ioctl(ttyfd,SIOCSPGRP,&pid)) {
            perror("Setting process group id");
            return(-1);
        }
        (VOID) signal(SIGURG,x25oobh);

        /* Set reverse charge call and closed user group if requested */
        bzero ((char *)&x25facil,sizeof(x25facil));
        if (revcall) x25facil.reverse_charge = revcall;
        if (closgr > -1) {
            x25facil.cug_req = 1;
            x25facil.cug_index = closgr;
        }
        if (ioctl(ttyfd,X25_WR_FACILITY,&x25facil) < 0) {
            perror ("Setting X.25 facilities");
            return (-1);
        }

        /*  Need X.25 header with bits Q and M */
        if (ioctl (ttyfd,X25_HEADER,&needh) < 0) {
            perror ("Setting X.25 header");
            return (-1);
        }

        /* Connects to remote host via SunLink X.25 */
        if (connect(ttyfd,&x25host,sizeof(x25host)) < 0) {
            debug(F101,"netopen connect errno","",errno);
            i = errno;
	    if (errno) {
                perror("netopen");
                x25diag();
            }
            (VOID) close (ttyfd);
            ttyfd = -1;
            errno = i;
            return (-1);
        }

        /* Get X.25 link identification used for the connection */
        if (ioctl(ttyfd,X25_GET_LINK,&linkid) < 0) {
            perror ("Getting X.25 link id");
            return (-1);
        }

        /* Get X.25 logical channel number used for the connection */
        if (ioctl(ttyfd,X25_RD_LCGN,&lcn) < 0) {
            perror ("Getting X.25 lcn");
            return (-1);
        }

        /* Get SunLink X.25 version */
        if (ioctl(ttyfd,X25_VERSION,&x25ver) < 0) {
            perror ("Getting SunLink X.25 version");
            return (-1);
        }
        ttnet = nett;                   /* Sunlink X.25 network */
        ttnproto = NP_X3;               /* PAD X.3, X.28, X.29 protocol */
        if (*lcl < 0) *lcl = 1;         /* Local mode */
        return(0);
    } else /* Note that SUNX25 support can coexist with TCP/IP support. */
#endif /* SUNX25 */
/*
  Add support for other networks here.
*/
    if (nett != NET_TCPB) return(-1);	/* BSD socket support */

    netclos();				/* Close any previous connection. */
    strncpy(namecopy, name, NAMECPYL);	/* Copy the hostname. */
    ttnproto = NP_NONE;			/* No protocol selected yet. */
    debug(F110,"netopen namecopy",namecopy,0);

    p = namecopy;			/* Was a service requested? */
    while (*p != '\0' && *p != ':') p++; /* Look for colon */
    if (*p == ':') {			/* Have a colon */
	*p++ = '\0';			/* Get service name or number */
    } else {				/* Otherwise use telnet */
	p = "telnet";
    }
    debug(F110,"netopen service requested",p,0);
    if (isdigit(*p)) {			/* Use socket number without lookup */
	service = &servrec;
	service->s_port = htons((unsigned short)atoi(p));
    } else {				/* Otherwise lookup the service name */
	service = getservbyname(p, "tcp");
    }
    if (!service) {
	fprintf(stderr, "Cannot find port for service %s\n", p);
#ifdef MULTINET
	debug(F101,"netopen can't get service","",socket_errno);
#else
	debug(F101,"netopen can't get service","",errno);
#endif /* MULTINET */
	errno = 0;			/* rather than mislead */
	return(-1);
    }
    /* Set up socket structure and get host address */

    bzero((char *)&saddr, sizeof(saddr));
    if ((host = gethostbyname(namecopy)) != NULL) {
	saddr.sin_family = host->h_addrtype;
	bcopy(host->h_addr, (caddr_t)&saddr.sin_addr, host->h_length);
    } else {
#ifdef INADDRX
/* inet_addr() is of type struct in_addr */
	struct in_addr ina;
	unsigned long uu;
#ifdef datageneral
        extern struct in_addr inet_addr();
#endif /* datageneral */
	ina = inet_addr(namecopy);
	uu = *(unsigned long *)&ina;
#else /* Not INADDRX */
/* inet_addr() is unsigned long */
	unsigned long uu;
	uu = inet_addr(namecopy);
#endif /* INADDRX */
	if ((saddr.sin_addr.s_addr = uu) != ((unsigned long)-1))
	  saddr.sin_family = AF_INET;
	else {
	  fprintf(stderr, "Can't get address for %s\n", namecopy);
#ifdef MULTINET
	  debug(F101,"netopen can't get address","",socket_errno);
#else
	  debug(F101,"netopen can't get address","",errno);
#endif /* MULTINET */
	  errno = 0;			/* rather than mislead */
	  return(-1);
      }
    }

    /* Get a file descriptor for the connection. */

    saddr.sin_port = service->s_port;
    sprintf(ipaddr,"%s", inet_ntoa(saddr.sin_addr));
    if (!quiet && *ipaddr) printf(" Trying %s...\n", ipaddr);

#ifdef EXCELAN
    send_socket.sin_family = AF_INET;
    send_socket.sin_addr.s_addr = 0;
    send_socket.sin_port = 0;
    if ((ttyfd = socket(SOCK_STREAM, (struct sockproto *)0,
		&send_socket, SO_REUSEADDR)) < 0)
#else
    if ((ttyfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
#endif /* EXCELAN */
      {
#ifdef EXCELAN
	experror("TCP socket error");
#else
#ifdef MULTINET
 	socket_perror("TCP socket error");
	debug(F101,"netopen socket error","",socket_errno);
#else
 	perror("TCP socket error");
	debug(F101,"netopen socket error","",errno);
#endif /* MULTINET */
#endif /* EXCELAN */
	return (-1);
    }
    errno = 0;

    /* Now connect to the socket on the other end. */

#ifdef EXCELAN
    if (connect(ttyfd, &saddr) < 0)
#else
    if (connect(ttyfd, (struct sockaddr *)&saddr, sizeof(saddr)) < 0)
#endif /* EXCELAN */
      {
	i = errno;			/* save error code */
	close(ttyfd);
	ttyfd = -1;
	errno = i;			/* and report this error */
#ifdef EXCELAN
	if (errno) experror("netopen connect");
#else
#ifdef MULTINET
	debug(F101,"netopen connect error","",socket_errno);
	if (errno) socket_perror("netopen connect");
#else
	debug(F101,"netopen connect errno","",errno);
#ifdef	WINTCP
	perror("netopen connect");
#endif	/* WINTCP */
#ifdef DEC_TCPIP
	perror("netopen connect");
#endif /* DEC_TCPIP */
#endif /* MULTINET */
#endif /* EXCELAN */
	return(-1);
    }
#ifdef SO_OOBINLINE
/*
  The symbol SO_OOBINLINE is not known to Ultrix 2.0.
  It means "leave out of band data inline".  The normal value is 0x0100,
  but don't try this on systems where the symbol is undefined.
*/
#ifdef datageneral
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE,(char *) &on, sizeof on);
#else
#ifdef BSD43
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE,(char *) &on, sizeof on);
#else
#ifdef OSF1
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE,(char *) &on, sizeof on);
#else
#ifdef POSIX
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE,(char *) &on, sizeof on);
#else
#ifdef SOLARIS
/*
  Maybe this applies to all SVR4 versions, but the other (else) way has been
  compiling and working fine on all the others, so best not to change it.
*/
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE,(char *) &on, sizeof on);
#else
    setsockopt(ttyfd, SOL_SOCKET, SO_OOBINLINE, &on, sizeof on);
#endif /* SOLARIS */
#endif /* POSIX */
#endif /* BSD43 */
#endif /* OSF1 */
#endif /* datageneral */
#endif /* SO_OOBINLINE */

    /* See if the service is TELNET. */
    if ((x = ntohs((unsigned short)service->s_port)) == TELNET_PORT)
      ttnproto = NP_TELNET;		/* Yes, set global flag. */
    debug(F101,"netopen service","",x);
    ttnet = nett;			/* TCP/IP (sockets) network */
    tn_init = 0;			/* Telnet protocol not init'd yet */
    if (*lcl < 0) *lcl = 1;		/* Local mode. */
    return(0);				/* Done. */
}

/*  N E T C L O S  --  Close current network connection.  */

int
netclos() {
    int x = 0;
    if (ttyfd < 0)			/* Was open? */
      return(0);			/* Wasn't. */
    if (ttyfd > -1)			/* Was. */
#ifdef VMSTCPIP
      x = socket_close(ttyfd);		/* Close it. */
#else
      x = close(ttyfd);
#endif /* VMSTCPIP */
    ttyfd = -1;				/* Mark it as closed. */
    tn_init = 0;			/* Remember about telnet protocol... */
    *ipaddr = '\0';			/* Zero the IP address string */
    return(x);
}

/*  N E T T C H K  --  Check if network up, and how many bytes can be read */
/*
  Returns number of bytes waiting, or -1 if connection has been dropped.
*/
int					/* Check how many bytes are ready */
nettchk() {				/* for reading from network */
#ifdef VMSTCPIP
    unsigned int count;
    int x, y;
    char c;

    debug(F101,"nettchk entry ttibn","",ttibn);
    debug(F101,"nettchk entry ttibp","",ttibp);
    socket_errno = 0;
/*
  Note: this socket_ioctl() call does NOT return an error if the
  connection has been broken.  (At least not in Multinet.)
*/
    if (socket_ioctl(ttyfd,FIONREAD,&count) < 0) {
	debug(F101,"nettchk socket_ioctl error","",socket_errno);
	if (ttibn < 1) return(-1);
	else return(ttibn);
    }
    debug(F101,"nettchk count","",count);

#ifndef DEC_TCPIP
/*
  Let's see if we can skip this for UCX, since it seems to cause trouble.
*/
    if (count == 0) {
/*
  Here we need to tell the difference between a 0 count on an active
  connection, and a 0 count because the remote end of the socket broke the
  connection.  There is no mechanism in TGV MultiNet (or WIN/TCP?) to query
  the status of the connection, so we have to do a read.  -1 means there was
  no data available (socket_errno == EWOULDBLOCK), 0 means the connection is
  down.  But if, by chance, we actually get a character, we have to put it
  where it won't be lost.
*/
	y = 1;				/* Turn on nonblocking reads */
	debug(F101,"nettchk before FIONBIO","",x);
	x = socket_ioctl(ttyfd,FIONBIO,&y);
	debug(F101,"nettchk FIONBIO","",x);
	x = socket_read(ttyfd,&c,1);	/* Returns -1 if no data */
	debug(F101,"nettchk socket_read","",x);
	y = 0;				/* Turn them back off */
	socket_ioctl(ttyfd,FIONBIO,&y);
	if (x == 0) return(-1);		/* Connection is broken. */
	if (x == 1) {			/* Oops, actually got a byte? */
	    debug(F101,"nettchk socket_read char","",c);
	    debug(F101,"nettchk ttibp","",ttibp);
	    debug(F101,"nettchk ttibn","",ttibn);
/*
  So put the byte we got into the buffer at the current position.
  Increment the buffer count, but DON'T increment the buffer pointer.
*/
	    ttibuf[ttibp+ttibn] = c;
	    ttibn++;
#ifdef DEBUG
	    ttibuf[ttibp+ttibn] = '\0';
	    debug(F111,"nettchk ttibn",ttibuf,ttibn);
#endif /* DEBUG */
	}
    }
#endif /* DEC_TCPIP */
    debug(F101,"nettchk returns","",count+ttibn);
    return(count + ttibn);

#else /* Not VMSTCPIP */
/*
  UNIX just uses ttchk(), in which the ioctl() calls on the file descriptor
  seem to work OK.
*/
    return(0);
#endif /* VMSTCPIP */
/*
  But what about X.25?
*/
}

/*  N E T I N C --  Input character from network */

int			
netinc(timo) int timo; {
#ifdef VMSTCPIP
    int x; unsigned char c;		/* The locals. */

    if (ttibn > 0) {			/* Something in internal buffer? */
	debug(F100,"netinc char in buf","",0); /* Yes. */
	x = 0;				/* Success. */
    } else {				/* Else must read from network. */
	x = -1;				/* Assume failure. */
#ifdef DEBUG
	debug(F101,"netinc goes to net, timo","",timo);
	ttibuf[ttibp+1] = '\0';
	debug(F111,"netinc ttibuf",ttibuf,ttibp);
#endif /* DEBUG */
	if (timo <= 0) {		/* Untimed case. */
	    while (1) {			/* Wait forever if necessary. */
		if (ttbufr() < 0)	/* Refill buffer. */
		  break;		/* Error, fail. */
		if (ttibn > 0) {	/* Success. */
		    x = 0;
		    break;
		}
	    }
	} else {			/* Timed case... */
	    saval = signal(SIGALRM,ttimoff); /* Enable timer interrupt */
	    alarm(timo);		/* for requested interval. */
	    if (setjmp(njbuf)) {	/* Timer went off? */
		x = -1;			/* Yes, fail. */
	    } else {
		while (1) {
		    if (ttbufr() < 0)	/* Keep trying to refill it. */
		      break;		/* Till we get an error. */
		    if (ttibn > 0) {	/* Or we get a character. */
			x = 0;
			break;
		    }
		}
	    }
	    ttimoff();			/* Timer off. */
	}
    }
    if (x < 0) {			/* Return -1 if we failed. */
	debug(F100,"netinc timed out","",0);
	return(-1);
    } else {				/* Otherwise */
	ttibn--;			/* Return what we got. */
	c = ttibuf[ttibp++];
	debug(F101,"netinc returning","",c);
	return((c & 0xff));
    }
#else /* Not MULTINET or WINTCP */
    return(-1);
#endif /* VMSTCPIP */
}

/*  N E T T O L  --  Output a string of bytes to the network  */
/*
  Call with s = pointer to string, n = length.
  Returns number of bytes actually written on success, or
  -1 on i/o error, -2 if called improperly.
*/
int
nettol(s,n) char *s; int n; {
#ifdef VMSTCPIP
    int count;
    if (ttnet == NET_TCPB) {
	if ((count = socket_write(ttyfd,s,n)) < 1) {
	    debug(F101,"nettol socket_write error","",socket_errno);
	    return(-1);
	}
	debug(F111,"nettol socket_write",s,count);
	return(count);
    } else return(-2);
#else
    debug(F100,"nettol VMSTCPIP not defined","",0);
    return(-2);
#endif /* VMSTCPIP */
}

/*  N E T T O C  --   Output character to network */
/*
  Call with character to be transmitted.
  Returns 0 if transmission was successful, or
  -1 upon i/o error, or -2 if called improperly.
*/
int			
#ifdef CK_ANSIC
nettoc(char c)
#else
nettoc(c) char c;
#endif /* CK_ANSIC */
/* nettoc */ {
#ifdef VMSTCPIP
    unsigned char cc;
    cc = c;
    if (ttnet == NET_TCPB) {
	debug(F101,"nettoc cc","",cc);
	if (socket_write(ttyfd,&cc,1) < 1) {
	    debug(F101,"nettoc socket_write error","",socket_errno);
	    return(-1);
	}
	debug(F101,"nettoc socket_write","", cc);
	return(0);
    } else return(-2);
#else
    return(-2);
#endif /* MULTINET */
}

/*  N E T F L U I  --  Flush network input buffer  */

int
netflui() {
    int n;
#ifdef VMSTCPIP
    ttibuf[ttibp+1] = '\0';
    debug(F111,"netflui 1",ttibuf,ttibn);
    ttibn = ttibp = 0;			/* Flush internal buffer *FIRST* */
    if ((n = nettchk()) > 0) {		/* Now see what's waiting on the net */
	if (n > TTIBUFL) n = TTIBUFL;	/* and sponge it up */
	debug(F101,"netflui 2","",n);	/* ... */
	n = socket_read(ttyfd,ttibuf,n) ; /* into our buffer */
	if (n >= 0) ttibuf[n] = '\0';
	debug(F111,"netflui 3",ttibuf,n);
	ttibuf[0] = '\0';
    }
#else
/*
  It seems the UNIX ioctl()s don't do the trick, so we have to read the
  stuff ourselves.  This should be pretty much portable, if not elegant.
*/
    if ((n = ttchk()) > 0) {
	debug(F101,"netflui","",n);
	while ((n--) && ttinc(0) > -1) ; /* Don't worry, it's buffered. */
    }
#endif /* VMSTCPIP */
    return(0);
}

#ifdef TNCODE				/* Compile in telnet support code */

/* TCP/IP Telnet negotiation support code */

static int sgaflg = 0;			/* telnet SGA flag */
static int wttflg = 0;			/* telnet terminal type flag */

#ifndef TELCMDS
char *telcmds[] = {
    "SE", "NOP", "DMARK", "BRK",  "IP",   "AO", "AYT",  "EC",
    "EL", "GA",  "SB",    "WILL", "WONT", "DO", "DONT", "IAC",
};
int ntelcmds = sizeof(telcmds) / sizeof(char *);
#endif /* TELCMDS */

#ifndef TELOPTS
char *telopts[] = {
	"BINARY", "ECHO", "RCP", "SUPPRESS GO AHEAD", "NAME",
	"STATUS", "TIMING MARK", "RCTE", "NAOL", "NAOP",
	"NAOCRD", "NAOHTS", "NAOHTD", "NAOFFD", "NAOVTS",
	"NAOVTD", "NAOLFD", "EXTEND ASCII", "LOGOUT", "BYTE MACRO",
	"DATA ENTRY TERMINAL", "SUPDUP", "SUPDUP OUTPUT",
	"SEND LOCATION", "TERMINAL TYPE", "END OF RECORD"
#ifdef TELOPT_TUID
	,"TACACS UID"
#ifdef TELOPT_OUTMRK
	,"OUTPUT MARKING"
#ifdef TELOPT_TTYLOC
	,"TTYLOC"
#ifdef TELOPT_3270REGIME
	,"3270 REGIME"
#ifdef TELOPT_X3PAD
	,"X.3 PAD"
#ifdef TELOPT_NAWS
	,"NAWS"
#ifdef TELOPT_TSPEED
	,"TSPEED"
#ifdef TELOPT_LFLOW
	,"LFLOW"
#ifdef TELOPT_LINEMODE
	,"LINEMODE"
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
};
#endif /* TELOPTS */

int ntelopts = sizeof(telopts) / sizeof(char *);
#endif /* TNCODE */


/* Send network BREAK */
/*
  Returns -1 on error, 0 if nothing happens, 1 if BREAK sent successfully.
*/
int
netbreak() {
    CHAR buf[3];
    if (ttnet == NET_TCPB) {
	if (ttnproto == NP_TELNET) {
#ifdef TNCODE
	    buf[0] = (CHAR) IAC; buf[1] = (CHAR) BREAK;
	    if (ttol(buf,2) < 2) return(-1);
	    debug(F101,"telnet BREAK ok","",BREAK);
	    return(1);
#else
	    debug(F100,"netbreak no TNCODE","",0);
	    return(0);
#endif /* TNCODE */
	}
	/* Insert other TCP/IP protocols here */
    }
    /* Insert other networks here */
    return(0);
}

/* Send a telnet option, avoid loops. */
/* Returns 1 if command was sent, 0 if not, -1 on error */

int
tn_sopt(cmd,opt) int cmd, opt; {	/* TELNET SEND OPTION */
    CHAR buf[4];
    int n;
    if (ttnet != NET_TCPB) return(0);	/* Must be TCP/IP */
    if (ttnproto != NP_TELNET) return(0); /* Must be telnet protocol */
    n = cmd - SE;
    if (n < 0 || n > ntelcmds) return(0);
#ifdef TNCODE
    buf[0] = (CHAR) IAC;
    buf[1] = (CHAR) cmd & 0xff;
    buf[2] = (CHAR) opt & 0xff;
    if (ttol(buf,3) < 3)
      return(-1);
    debug(F111,"telnet cmd >",telcmds[n],cmd);
    debug(F111,"telnet opt >",
	  (opt < ntelopts) ? telopts[opt] : "UNKNOWN", opt );
    if (debses && cmd != SB)
      printf("[%s %s]",telcmds[n],
	     (opt < ntelopts) ? telopts[opt] : "UNKNOWN");
    return(1);
#else
    debug(F100,"tn_sopt no TNCODE","",0);
    return(0);
#endif /* TNCODE */
}

/* Initialize a telnet connection. */
/* Returns -1 on error, 0 if nothing happens, 1 if init msgs sent ok */

int
tn_ini() {
#ifndef TNCODE
    debug(F100,"tn_ini no TNCODE","",0);
    return(0);
#else /* TELNET protocol support */
    debug(F101,"tn_ini ttnproto","",ttnproto);
    debug(F101,"tn_ini tn_init","",tn_init);

    if (ttnet != NET_TCPB)		/* Make sure connection is TCP/IP. */
      return(0);
    if (tn_init)			/* Have we done this already? */
      return(0);			/* Don't do it again. */
    debug(F101,"tn_ini tn_duplex","",tn_duplex);
    duplex = tn_duplex;			/* Assume local echo. */
    sgaflg = 0;				/* Assume Go-Ahead suppressed. */
    wttflg = 0;				/* Did not send WILL TERM TYPE yet. */
    if (ttnproto == NP_NONE) {		/* If not talking to a telnet port, */
	ttnproto = NP_TELNET;		/* pretend it's telnet anyway, */
	tn_init = 1;			/* but don't send initial options. */
	debug(F100,"tn_ini skipping telnet negotiations","",0);
	return(0);
    }
    /* Talking to telnet port, so send WILL TERMINAL TYPE and DO SGA */

    if (tn_sopt(WILL,TELOPT_TTYPE) < 0)	/* Will send terminal type. */
      return(-1);
    wttflg = 1;				/* Remember I said I would. */
    if (tn_sopt(DO,TELOPT_SGA) < 0)	/* Please suppress go-ahead. */
      return(-1);
#ifdef COMMENT
    if (tn_sopt(DO,TELOPT_ECHO) < 0)	/* Ask the server to echo, since */
      return(-1);			/* I'm assuming it will. */
#endif /* COMMENT */
    tn_init = 1;			/* Set telnet-initialized flag. */

    /* Don't send anthing else! */

    debug(F101,"tn_ini duplex","",duplex);
    return(1);
#endif /* TNCODE */
}

/*
  Process in-band Telnet negotiation characters from the remote host.
  Call with the telnet IAC character and the current duplex setting
  (0 = remote echo, 1 = local echo).
  Returns:
    3 if server has sent us a quoted IAC
    2 if local echo must be changed to remote
    1 if remote echo must be changed to local
    0 if nothing happens or no action necessary
   -1 on failure (= internal or i/o error)
*/

#define TSBUFSIZ 41
char sb[TSBUFSIZ];			/* Buffer for subnegotiations */

int
#ifdef CK_ANSIC				/* TELNET DO OPTION */
tn_doop( CHAR z, int echo, int (*fn)(int) )
#else
tn_doop(z, echo, fn) CHAR z; int echo; int (*fn)();
#endif /* CK_ANSIC */
/* tn_doop */ {
    int c, x, y, n, m, flag;

#ifndef TNCODE
    debug(F100,"tn_doop no TNCODE","",0);
    return(0);
#else
    if (z != (CHAR) IAC) {
	debug(F101,"tn_doop bad call","",z);
	return(-1);
    }
    if (ttnet != NET_TCPB) return(0);
    if (ttnproto != NP_TELNET) return(0); 	/* Check protocol */

/* Have IAC, read command character. */

    c = (*fn)(0) & 0xff;		/* Read command character */
    m = c - SE;				/* Check validity */
    if (m < 0 || m > ntelcmds) {
	debug(F101,"tn_doop bad cmd","",c);
	return(0);
    }
    if (seslog) {			/* Copy to session log, if any. */
	if (zchout(ZSFILE, (char) z) < 0) seslog = 0; /* Log IAC. */
	else if (zchout(ZSFILE, (char) c) < 0) seslog = 0; /* Log command */
    }
    debug(F111,"telnet cmd <",telcmds[m],c); /* Debug log. */

    if (c == (CHAR) IAC) return(3);	/* Quoted IAC */
    if (c < SB) return(0);		/* Other command with no arguments. */

/* SB, WILL, WONT, DO, or DONT need more bytes... */

    if ((x = (*fn)(0)) < 0) return(-1);	/* Get the option. */
    x &= 0xff;				/* Trim to 8 bits. */

    debug(F111,"telnet opt <",
	  (x < ntelopts) ? telopts[x] : "UNKNOWN", x );
    if (seslog)				/* Session log */
      if (zchout(ZSFILE, (char) x) < 0) seslog = 0;

    /* Now handle the command */

    if (debses && c != SB) 		/* Debug to screen. */
      printf("<%s %s>",telcmds[m],
	     (x < ntelopts) ? telopts[x] : "UNKNOWN" );
    switch (x) {
      case TELOPT_ECHO:			/* ECHO negotiation. */
	switch (c) {			/* Command */
	  case WILL:			/* Host says it will echo. */
	    if (echo)			/* Only reply if change required. */
	      return((tn_sopt(DO,x) < 0) ? -1 : 2); /* Please do. */
	    else return(0);		/* Otherwise no change. */
	  case WONT:			/* Host says it won't echo. */
	    if (!echo)			/* If I'm full duplex */
	      return ((tn_sopt(DONT,x) < 0) ? -1 : 1); /* Switch to half */
	    else return(0);		/* Otherwise, no change.  */
	  case DO:			/* Host wants me to echo */
	    if (tn_sopt(WONT,x) < 0)	/* but the client never echoes */
	      return(-1);		/* back to the server. */
	  default:			/* Don't reply to anything else */
	    return(0);
	}

      case TELOPT_SGA:			/* Suppress Go-Ahead */
	switch (c) {			/* Command... */
	  case WONT:			/* Host says it won't. */
	    if (!sgaflg) {
		sgaflg = 1;		/* Remember. */
		if (tn_sopt(DONT,x) < 0) /* acknowledge, */
		  return(-1);
	    }
	    return(echo ? 0 : 1);	/* Switch to half duplex */
	  case WILL:			/* Server says it will SGA */
	    if (sgaflg) {		/* ACK only if necessary */
		if (tn_sopt(DO,x) < 0)
		  return(-1);
		sgaflg = 0;		/* Remember new SGA state. */
	    }
	    return(0);			/* But don't change echo state. */
	}

#ifdef TELOPT_TTYPE
      case TELOPT_TTYPE:		/* Terminal Type */
	switch (c) {
	  case DO:			/* DO terminal type. */
	    if (wttflg == 0) {		/* If I haven't said so before, */
		if (tn_sopt((CHAR)WILL,x) < 0) /* say I'll send it if asked. */
		  return(-1);
		wttflg++;
	    }
	    return(0);
	  case SB:
	    debug(F100,"TELNET subnegotiation:","",0);
	    n = flag = 0;		/* Flag for when done reading SB */
	    while (n < TSBUFSIZ) {	/* Loop looking for IAC SE */
		if ((y = (*fn)(0)) < 0)	/* Read a byte */
		  return(-1);
		y &= 0xff;		/* Make sure it's just 8 bits. */
		sb[n++] = y;		/* Deposit in buffer. */
		if (seslog)		/* Take care of session log */
		  if (zchout(ZSFILE, (char) y) < 0)
		    seslog = 0;
		if (y == IAC) {		/* If this is an IAC */
		    if (flag) {		/* If previous char was IAC */
			n--;		/* it's quoted, keep one IAC */
			flag = 0;	/* and turn off the flag. */
		    } else flag = 1;	/* Otherwise set the flag. */
		} else if (flag) { 	/* Something else following IAC */
		    if (y != SE)	/* If not SE, it's a protocol error */
		      flag = 0;
		    break;
		}
	    }
	    if (!flag) {		/* Make sure we got a valid SB */
		debug(F100, "TELNET Subnegotian prematurely broken", "",0);
		return(-1);
	    }
	    if (debses) {		/* Debug to screen. */
		int i;
		printf("<SB %s ",telopts[TELOPT_TTYPE]);
		for (i = 0; i < n-2; i++) printf("%02x",sb[i]);
		printf(" IAC SE>");
	    }
	    debug(F101,"TELNET suboption<","",sb[0]);
	    if (sb[0] == 1) {		/* SEND terminal type? */
		if (tn_sttyp() < 0)	/* Yes, so send it. */
		  return(-1);
	    }
	  default:			/* Others, ignore */
	    return(0);
	}
#endif /* TELOPT_TTYPE */

      default:				/* All others: refuse */
	switch(c) {
	  case WILL:			/* You will? */
	    if (tn_sopt(DONT,x) < 0)	/* Please don't. */
	      return(-1);		/* (Could this cause a loop?) */
	    break;
	  case DO:			/* You want me to? */
	    if (tn_sopt(WONT,x) < 0)	/* I won't. */
	      return(-1);
	    break;
	  case DONT:			/* You don't want me to? */
	    if (tn_sopt(WONT,x) < 0)	/* I won't. */
	      return(-1);		/* (Could this cause a loop?) */
	  case WONT:			/* You won't? */
	    break;			/* I didn't want you to. */
	}				/* Anything else, treat as user data */
	return(0);
    }
#endif /* TNCODE */
}

/* Telnet send terminal type */
/* Returns -1 on error, 0 if nothing happens, 1 if type sent successfully */

int
tn_sttyp() {				/* Send telnet terminal type. */
#ifndef TNCODE
    debug(F100,"tn_sttyp no TNCODE","",0);
    return(0);
#else
    char *ttn; int ttl, i;		/* Name & length of terminal type. */

    if (ttnet != NET_TCPB) return(0);
    if (ttnproto != NP_TELNET) return(0);

    ttn = NULL;

    if (tn_term) {			/* Terminal type override? */
	debug(F110,"tn_sttyp",tn_term,0);
	if (*tn_term) ttn = tn_term;
    } else debug(F100,"tn_sttyp no term override","",0);
#ifndef datageneral
    if (!ttn)				/* If no override, */
      ttn = getenv("TERM");		/* get it from the environment. */
#endif /* datageneral */
    if ((ttn == ((char *)0)) || ((ttl = (int)strlen(ttn)) >= TSBUFSIZ)) {
	ttn = "UNKNOWN";
	ttl = 7;
    }
    sb[0] = IAC;			/* I Am a Command */
    sb[1] = SB;				/* Subnegotiation */
    sb[2] = TELOPT_TTYPE;		/* Terminal Type */
    sb[3] = (CHAR) 0;			/* Is... */
    for (i = 4; *ttn; ttn++,i++)	/* Copy and uppercase it */
      sb[i] = (islower(*ttn)) ? toupper(*ttn) : *ttn;
    ttn = sb;				/* Point back to beginning */
    sb[i++] = IAC;			/* End of Subnegotiation */
    sb[i++] = SE;			/* marked by IAC SE */
    if (ttol((CHAR *)sb,i) < 0)		/* Send it. */
      return(-1);
#ifdef DEBUG
    sb[i-2] = '\0';			/* For debugging */
    debug(F111,"telnet SB sent ttype",sb+4,ttl);
#endif /* DEBUG */
    if (debses)				/* Debug to screen. */
      printf("[SB TERMINAL TYPE 00 %s IAC SE]",sb+4);
    return(1);
#endif /* TNCODE */
}

#ifdef SUNX25
/*
  SunLink X.25 support by Marcello Frutig, Catholic University,
  Rio de Janeiro, Brazil, 1990.
*/

/* PAD X.3, X.28 and X.29 support */

static CHAR x29err [MAXPADPARMS+3] = { X29_ERROR, INVALID_PAD_PARM, '\0' };


/* Initialize PAD */

extern CHAR padparms[MAXPADPARMS+1];

VOID
initpad() {
  padparms[PAD_BREAK_CHARACTER]        = 0;  /* Break character */
  padparms[PAD_ESCAPE]                 = 1;  /* Escape permitted */
  padparms[PAD_ECHO]                   = 1;  /* Kermit PAD does echo */
  padparms[PAD_DATA_FORWARD_CHAR]      = 2;  /* forward character CR */
  padparms[PAD_DATA_FORWARD_TIMEOUT]   = 0;  /* no timeout forward condition */
  padparms[PAD_FLOW_CONTROL_BY_PAD]    = 0;  /* not used */
  padparms[PAD_SUPPRESSION_OF_SIGNALS] = 1;  /* allow PAD service signals */
  padparms[PAD_BREAK_ACTION]           = 21; /* brk action: INT pk + brk ind*/
  padparms[PAD_SUPPRESSION_OF_DATA]    = 0;  /* no supression of user data */
  padparms[PAD_PADDING_AFTER_CR]       = 0;  /* no padding after CR */
  padparms[PAD_LINE_FOLDING]           = 0;  /* no line fold */
  padparms[PAD_LINE_SPEED]             = 0;  /* line speed - don't care */
  padparms[PAD_FLOW_CONTROL_BY_USER]   = 0;  /* flow cont of PAD - not used */
  padparms[PAD_LF_AFTER_CR]            = 0;  /* no LF insertion after CR */
  padparms[PAD_PADDING_AFTER_LF]       = 0;  /* no padding after LF */
  padparms[PAD_EDITING]                = 1;  /* can edit */
  padparms[PAD_CHAR_DELETE_CHAR]       = 8;  /* character delete character */
  padparms[PAD_BUFFER_DELETE_CHAR]     = 21; /* buffer delete character */
  padparms[PAD_BUFFER_DISPLAY_CHAR]    = 18; /* buffer display character */
}


/* Set PAD parameters */

VOID
setpad(s,n) CHAR *s; int n; {
    int i;
    CHAR *ps = s;

    for (i = 0; i < n; i++) {
        if (*ps > MAXPADPARMS)
	  x29err[i+2] = *ps;
        else
	  padparms[*ps] = *(ps+1);
        ps += 2;
    }
}

/* Read PAD parameters */

VOID
readpad(s,n,r) CHAR *s; int n; CHAR *r; {
    int i;
    CHAR *ps = s;
    CHAR *pr = r;

    *pr++ = X29_PARAMETER_INDICATION;
    for (i = 0; i < n; i++, ps++) {
         if (*ps > MAXPADPARMS) {
             x29err[i+2] = *ps++;
         } else {
             *pr++ = *ps;
             *pr++ = padparms[*ps++];
         }
    }
}

int
qbitpkt(s,n) CHAR *s; int n; {
    CHAR *ps = s;
    int x29cmd = *ps;
    CHAR *psa = s+1;
    CHAR x29resp[(MAXPADPARMS*2)+1];

    switch (x29cmd) {

        case X29_SET_PARMS:
            setpad (ps+1,n/2);
            if ((int)strlen(x29err) > 2) {
                ttol (x29err,(int)strlen(x29err));
                x29err[2] = '\0';
            }
            return (-2);
        case X29_READ_PARMS:
            readpad (ps+1,n/2,x29resp);
            setqbit ();
            ttol (x29resp,n+1);
            if ((int)strlen(x29err) > 2) {
                ttol (x29err,(int)strlen(x29err));
                x29err[2] = '\0';
            }
            resetqbit();
            break;
        case X29_SET_AND_READ_PARMS:
            setpad (ps+1,n/2);
            readpad (ps+1,n/2,x29resp);
            setqbit();
            ttol (x29resp,n+1);
            if ((int)strlen(x29err) > 2) {
                ttol (x29err,(int)strlen(x29err));
                x29err [2] = '\0';
            }
            resetqbit();
            return (-2);
        case X29_INVITATION_TO_CLEAR:
            (VOID) x25clear();
            return (-1) ;
        case X29_INDICATION_OF_BREAK:
	    break;
    }
    return (0);
}

/* PAD break action processor */

VOID
breakact() {
    extern char x25obuf[MAXOX25];
    extern int obufl;
    extern int active;
    extern unsigned char tosend;
    static CHAR indbrk[3] = {
	X29_INDICATION_OF_BREAK,
	PAD_SUPPRESSION_OF_DATA,
	1
    };
    CHAR intudat, cause, diag;

    if (x25stat() < 0) return(0);   /* Ignore if no virtual call established */

    if (padparms[PAD_BREAK_ACTION] != 0) /* Forward condition */
        if (ttol(x25obuf,obufl) < 0) {
            perror ("\r\nCan't send characters");
            active = 0;
        } else {
            bzero (x25obuf,sizeof(x25obuf));
            obufl = 0;
            tosend = 0;
        };

    switch (padparms[PAD_BREAK_ACTION]) {

       case 0 : break;			/* do nothing */
       case 1 : /* send interrupt packet with interrupt user data field = 1 */
	        intudat = 1;
                x25intr (intudat);
                break;
       case 2 : /* send reset packet with cause and diag = 0 */
		cause = diag = 0;
                x25reset (cause,diag);
                break;
       case 5 : /* send interrupt packet with interrupt user data field = 0 */
		intudat = 0;
                x25intr (intudat) ;
                setqbit ();
	        /* send indication of break without a parameter field */
                ttoc(X29_INDICATION_OF_BREAK);
                resetqbit ();
                break;
       case 8 : active = 0;		/* leave data transfer */
                conol ("\r\n");
                break;
       case 21: /* send interrupt packet with interrupt user data field = 0 */
		intudat = 0;
                x25intr (intudat);
                setpad (indbrk+1,2);	/* set pad to discard input */
                setqbit ();
		/* send indication of break with parameter field */
                ttol (indbrk,sizeof(indbrk));
                resetqbit ();
                break;
     }
}

/* X.25 support functions */

X25_CAUSE_DIAG diag;

/*
  Convert a null-terminated string representing an X.121 address
  to a packed BCD form.
*/

int
pkx121(str,bcd) char *str; CHAR *bcd; {
    int i, j;
    u_char c;

    i = j = 0;
    while (str[i]) {
        if ( i >= 15 || str [i] < '0' || str [i] > '9' )
	  return (-1);
        c = str [i] - '0';
        if ( i & 1 )
	  bcd [j++] |= c;
        else
	  bcd [j] = c << 4;
        i++;
    }
    return (i);
}

/* Reads and prints X.25 diagnostic */

int
x25diag () {
    int i;

    bzero ((char *)&diag,sizeof(diag));
    if (ioctl(ttyfd,X25_RD_CAUSE_DIAG,&diag)) {
        perror ("Reading X.25 diagnostic");
        return(-1);
    }
    if (diag.datalen > 0) {
        printf ("X.25 Diagnostic :");
        for (i = 0; i < diag.datalen; i++) printf (" %02x",diag.data[i]);
        printf ("\r\n");
    }
    return(0);
}

/* X.25 Out-of-Band Signal Handler */

VOID
x25oobh() {
    int oobtype;
    u_char oobdata;

    (VOID) signal(SIGURG,x25oobh);
    do {
        if (ioctl(ttyfd,X25_OOB_TYPE,&oobtype)) {
            perror ("Getting signal type");
            return;
        }
        switch (oobtype) {
	  case INT_DATA:
	    if (recv(ttyfd,oobdata,1,MSG_OOB) < 0) {
		perror ("Receiving X.25 interrupt data");
		return;
	    }
	    printf ("\r\nInterrupt received, data = %d\r\n", oobdata);
	    break;
	  case VC_RESET:
	    printf ("\r\nVirtual circuit reset\r\n");
	    x25diag ();
	    break;
	  case N_RESETS:
	    printf ("\r\nReset timeout\r\n");
	    break;
	  case N_CLEARS:
	    printf ("\r\nClear timeout\r\n");
	    break;
	  case MSG_TOO_LONG:
	    printf ("\r\nMessage discarded, too long\r\n");
	    break;
	  default:
	    if (oobtype) printf("\r\nUnknown oob type %d\r\n",oobtype);
	    break;
	}
    } while (oobtype);
}

/* Send a X.25 interrupt packet */

int
#ifdef CK_ANSIC
x25intr(char intr)
#else
x25intr(intr) char intr;
#endif /* CK_ANSIC */
/* x25intr */ {
    if (send(ttyfd,&intr,1,MSG_OOB) < 0) return(-1);
    debug(F100,"X.25 intr","",0);
    return(0);
}

/* Reset X.25 virtual circuit */
int
#ifdef CK_ANSIC
x25reset(char cause, char diagn)
#else
x25reset(cause, diagn) char cause; char diagn;
#endif /* CK_ANSIC */
/* x25reset */ {
    bzero ((char *)&diag,sizeof(diag));
    diag.flags   = 0;
    diag.datalen = 2;
    diag.data[0] = cause;
    diag.data[1] = diagn;
    if (ioctl(ttyfd,X25_WR_CAUSE_DIAG,&diag) < 0)
      return(-1);
    debug(F100,"X.25 reset","",0);
    return(0);
}

/* Clear X.25 virtual circuit */
int
x25clear() {
    int i;
    debug(F100,"X.25 clear","",0);
    bzero ((char *)&diag,sizeof(diag));
    diag.flags = (1 << DIAG_TYPE);
    diag.datalen = 2;
    diag.data[0] = 0;
    diag.data[1] = 0;
    ioctl (ttyfd,X25_WR_CAUSE_DIAG,&diag); /* Send Clear Request */
    return(ttclos(0));			/* Close socket */
}

/* X.25 status */
int
x25stat() {
    if (ttyfd < 0) return (-1);
    return(0);
}

/* Set Q_BIT on */
VOID
setqbit() {
    static int qbiton = 1 << Q_BIT;
    ioctl (ttyfd,X25_SEND_TYPE,&qbiton);
}

/* Set Q_BIT off */
VOID
resetqbit() {
    static int qbitoff = 0;
    ioctl (ttyfd,X25_SEND_TYPE,&qbitoff);
}

/* Read n characters from X.25 circuit into buf */

int
x25xin(n,buf) int n; CHAR *buf; {
    register int x, c;
    int qpkt;

    do {
	x = read(ttyfd,buf,n);
	if (buf[0] & (1 << Q_BIT)) { /* If Q_BIT packet, process it */
	    /* If return -1 : invitation to clear; -2 : PAD changes */
	    if ((c=qbitpkt(buf+1,x-2)) < 0) return(c);
	    qpkt = 1;
	} else qpkt = 0;
    } while (qpkt);
    if (x > 0) buf[x] = '\0';
    if (x < 1) x = -1;
    debug(F101,"x25xin x","",x);

    return(x);
}

#ifdef COMMENT /* NO LONGER NEEDED! */
/* X.25 read a line */

int
#ifdef PARSENSE
#ifdef CK_ANSIC
x25inl(CHAR *dest, int max,int timo, CHAR eol, CHAR start)
#else
x25inl(dest,max,timo,eol,start) int max,timo; CHAR *dest, eol, start;
#endif /* CK_ANSIC */
#else /* not PARSENSE */
#ifdef CK_ANSIC
x25inl(CHAR *dest, int max,int timo, CHAR eol)
#else
x25inl(dest,max,timo,eol) int max,timo; CHAR *dest, eol;
#endif /* __SDTC__ */
#endif /* PARSENSE */
/* x25inl */ {
    CHAR *pdest;
    int pktype, goteol, rest, n;
    int i, flag = 0;
    extern int ttprty, ttpflg;
    int ttpmsk;

    ttpmsk = (ttprty) ? 0177 : 0377;	/* Set parity stripping mask */

    debug(F101,"x25inl max","",max);
    debug(F101,"x25inl eol","",eol);
    pdest  = dest;
    rest   = max;
    goteol = 0;
    do {
	n = read(ttyfd,pdest,rest);
	n--;
	pktype = *pdest & 0x7f;
	switch (pktype) {
	  case 1 << Q_BIT:
	    if (qbitpkt(pdest+1,--n) < 0) return(-2);
	    break;
	  default:
	    if (flag == 0) { /* if not in packet, search start */
		for (i = 1; (i < n) &&
		     !(flag = ((dest[i] & 0x7f) == start));
		     i++);
		if (flag == 0) { /* not found, discard junk */
		    debug(F101,"x25inl skipping","",n);
		    continue;
		} else {		/* found, discard junk before start */
		    int k;
		    n = n - i + 1;
		    for (k = 1; k <= n; k++, i++) dest[k] = dest[i];
		}
	    }
	    for (i = 0; (i < n) && /* search for eol */
		 !(goteol=(((*pdest = *(pdest+1)&ttpmsk)&0x7f)== eol));
		 i++,pdest++);
	    *pdest = '\0';
	    rest -= n;
	}
    } while ( (rest > 0) && (!goteol) );

    if (goteol) {
	n = max - rest;
	debug (F111,"x25inl X.25 got",(char *) dest,n);
	if (timo) ttimoff();
	if (ttpflg++ == 0 && ttprty == 0) {
	    if ((ttprty = parchk(dest,start,n)) > 0) {
		int j;
		debug(F101,"x25inl senses parity","",ttprty);
		debug(F110,"x25inl packet before",(char *)dest,0);
		ttpmsk = 0x7f;
		for (j = 0; j < n; j++)
		  dest[j] &= 0x7f; /* Strip parity from packet */
		debug(F110,"x25inl packet after ",dest,0);
	    } else {
		debug(F101,"parchk","",ttprty);
		if (ttprty < 0) { ttprty = 0; n = -1; }
	    }
	}
	ttimoff();
	return(n);
    }
    ttimoff();
    return(-1);
}
#endif /* COMMENT */
#endif /* SUNX25 */

#endif /* NETCONN */
