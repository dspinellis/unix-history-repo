/* $Id: nntpinit.c,v 3.0 1991/11/22 04:12:21 davison Trn $
*/
/* This software is Copyright 1992 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction or this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#undef	DECNET    /* If you want decnet support */
#undef	EXCELAN   /* Excelan EXOS 205 support */
#undef	NONETDB	  /* Define if you're missing netdb.h */

#include "EXTERN.h"
#include "common.h"
#include "nntpclient.h"

#ifdef USE_NNTP

#include <sys/socket.h>
#include <netinet/in.h>
#ifdef NONETDB
# define IPPORT_NNTP	((unsigned short) 119)
#else
# include <netdb.h>
#endif /* !EXCELAN */

#ifdef EXCELAN
int connect _((int, struct sockaddr *));
unsigned short htons _((unsigned short));
unsigned long rhost _((char **));
int rresvport p((int));
int socket _((int, struct sockproto *, struct sockaddr_in *, int));
#endif /* EXCELAN */

#ifdef DECNET
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif /* DECNET */

unsigned long inet_addr _((char *x));
int get_tcp_socket _((char *machine));

int
server_init(server)
char *server;
{
    char line2[NNTP_STRLEN];
    int sockt_rd, sockt_wr;
#ifdef DECNET
    char *cp;

    cp = index(server, ':');

    if (cp && cp[1] == ':') {
	*cp = '\0';
	sockt_rd = get_dnet_socket(server);
    } else
	sockt_rd = get_tcp_socket(server);
#else /* !DECNET */
    sockt_rd = get_tcp_socket(server);
#endif

    if (sockt_rd < 0)
	return -1;
    sockt_wr = dup(sockt_rd);

    /* Now we'll make file pointers (i.e., buffered I/O) out of
    ** the socket file descriptor.  Note that we can't just
    ** open a fp for reading and writing -- we have to open
    ** up two separate fp's, one for reading, one for writing. */
    if ((ser_rd_fp = fdopen(sockt_rd, "r")) == NULL) {
	perror("server_init: fdopen #1");
	return -1;
    }
    if ((ser_wr_fp = fdopen(sockt_wr, "w")) == NULL) {
	perror("server_init: fdopen #2");
	ser_rd_fp = NULL;
	return -1;
    }

    /* Now get the server's signon message */
    nntp_check(FALSE);

    /* Send a MODE READER command in case we're talking to innd.
    ** If understood, use that reply. */
    nntp_command("MODE READER");
    if (nntp_gets(line2, sizeof line2) < 0)
	return -1;
    if (atoi(line2) != NNTP_BAD_COMMAND_VAL)
	strcpy(ser_line, line2);
    return atoi(ser_line);
}

int
get_tcp_socket(server)
char *server;
{
    int s;
    struct sockaddr_in sin;
#ifdef NONETDB
    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = AF_INET;
#else
    struct servent *getservbyname(), *sp;
    struct hostent *gethostbyname(), *hp;
#ifdef h_addr
    int x = 0;
    register char **cp;
    static char *alist[1];
#endif /* h_addr */
    static struct hostent def;
    static struct in_addr defaddr;
    static char namebuf[ 256 ];

    if ((sp = getservbyname("nntp", "tcp")) ==  NULL) {
	fprintf(stderr, "nntp/tcp: Unknown service.\n");
	return -1;
    }
    /* If not a raw ip address, try nameserver */
    if (!isdigit(*server)
     || (long)(defaddr.s_addr = inet_addr(server)) == -1)
	hp = gethostbyname(server);
    else {
	/* Raw ip address, fake  */
	(void) strcpy(namebuf, server);
	def.h_name = namebuf;
#ifdef h_addr
	def.h_addr_list = alist;
#endif
	def.h_addr = (char *)&defaddr;
	def.h_length = sizeof(struct in_addr);
	def.h_addrtype = AF_INET;
	def.h_aliases = 0;
	hp = &def;
    }
    if (hp == NULL) {
	fprintf(stderr, "%s: Unknown host.\n", server);
	return -1;
    }

    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = hp->h_addrtype;
    sin.sin_port = sp->s_port;
#endif /* !NONETDB */

    /* The following is kinda gross.  The name server under 4.3
    ** returns a list of addresses, each of which should be tried
    ** in turn if the previous one fails.  However, 4.2 hostent
    ** structure doesn't have this list of addresses.
    ** Under 4.3, h_addr is a #define to h_addr_list[0].
    ** We use this to figure out whether to include the NS specific
    ** code... */
#ifdef h_addr
    /* get a socket and initiate connection -- use multiple addresses */
    for (cp = hp->h_addr_list; cp && *cp; cp++) {
	s = socket(hp->h_addrtype, SOCK_STREAM, 0);
	if (s < 0) {
	    perror("socket");
	    return -1;
	}
        bcopy(*cp, (char *)&sin.sin_addr, hp->h_length);
		
	if (x < 0)
	    fprintf(stderr, "trying %s\n", inet_ntoa(sin.sin_addr));
	x = connect(s, (struct sockaddr *)&sin, sizeof (sin));
	if (x == 0)
	    break;
        fprintf(stderr, "connection to %s: ", inet_ntoa(sin.sin_addr));
	perror("");
	(void) close(s);
    }
    if (x < 0) {
	fprintf(stderr, "giving up...\n");
	return -1;
    }
#else /* no name server */
#ifdef EXCELAN
    s = socket(SOCK_STREAM, (struct sockproto *)NULL, &sin, SO_KEEPALIVE);
    if (s < 0) {
	/* Get the socket */
	perror("socket");
	return -1;
    }
    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(IPPORT_NNTP);

    /* set up addr for the connect */
    if ((sin.sin_addr.s_addr = rhost(&server)) == -1) {
	fprintf(stderr, "%s: Unknown host.\n", server);
	return -1;
    }

    /* And then connect */
    if (connect(s, (struct sockaddr *)&sin) < 0) {
	perror("connect");
	(void) close(s);
	return -1;
    }
#else /* not EXCELAN */
    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	perror("socket");
	return -1;
    }

    /* And then connect */

    bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
    if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
	perror("connect");
	(void) close(s);
	return -1;
    }

#endif /* !EXCELAN */
#endif /* !h_addr */
    return s;
}

#ifdef DECNET
int
get_dnet_socket(server)
char *server;
{
    int s, area, node;
    struct sockaddr_dn sdn;
    struct nodeent *getnodebyname(), *np;

    bzero((char *) &sdn, sizeof(sdn));

    switch (s = sscanf(server, "%d%*[.]%d", &area, &node)) {
    case 1: 
	node = area;
	area = 0;
    case 2: 
	node += area*1024;
	sdn.sdn_add.a_len = 2;
	sdn.sdn_family = AF_DECnet;
	sdn.sdn_add.a_addr[0] = node % 256;
	sdn.sdn_add.a_addr[1] = node / 256;
	break;
    default:
	if ((np = getnodebyname(server)) == NULL) {
	    fprintf(stderr, "%s: Unknown host.\n", server);
	    return -1;
	} else {
	    bcopy(np->n_addr, (char *) sdn.sdn_add.a_addr, np->n_length);
	    sdn.sdn_add.a_len = np->n_length;
	    sdn.sdn_family = np->n_addrtype;
	}
	break;
    }
    sdn.sdn_objnum = 0;
    sdn.sdn_flags = 0;
    sdn.sdn_objnamel = strlen("NNTP");
    bcopy("NNTP", &sdn.sdn_objname[0], sdn.sdn_objnamel);

    if ((s = socket(AF_DECnet, SOCK_STREAM, 0)) < 0) {
	nerror("socket");
	return -1;
    }

    /* And then connect */
    if (connect(s, (struct sockaddr *) &sdn, sizeof(sdn)) < 0) {
	nerror("connect");
	close(s);
	return -1;
    }
    return s;
}
#endif /* DECNET */

#ifdef EXCELAN
/*
 * inet_addr for EXCELAN (which does not have it!)
 *
 */
unsigned long
inet_addr(cp)
register char   *cp;
{
	unsigned long val, base, n;
	register char c;
	unsigned long octet[4], *octetptr = octet;
#ifndef htonl
	extern  unsigned long   htonl();
#endif  /* htonl */
again:
	/*
	 * Collect number up to ``.''.
	 * Values are specified as for C:
	 * 0x=hex, 0=octal, other=decimal.
	 */
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		/*
		 * Internet format:
		 *      a.b.c.d
		 *      a.b.c   (with c treated as 16-bits)
		 *      a.b     (with b treated as 24 bits)
		 */
		if (octetptr >= octet + 4)
			return (-1);
		*octetptr++ = val, cp++;
		goto again;
	}
	/*
	 * Check for trailing characters.
	 */
	if (*cp && !isspace(*cp))
		return (-1);
	*octetptr++ = val;
	/*
	 * Concoct the address according to
	 * the number of octet specified.
	 */
	n = octetptr - octet;
	switch (n) {

	case 1:                         /* a -- 32 bits */
		val = octet[0];
		break;

	case 2:                         /* a.b -- 8.24 bits */
		val = (octet[0] << 24) | (octet[1] & 0xffffff);
		break;

	case 3:                         /* a.b.c -- 8.8.16 bits */
		val = (octet[0] << 24) | ((octet[1] & 0xff) << 16) |
			(octet[2] & 0xffff);
		break;

	case 4:                         /* a.b.c.d -- 8.8.8.8 bits */
		val = (octet[0] << 24) | ((octet[1] & 0xff) << 16) |
		      ((octet[2] & 0xff) << 8) | (octet[3] & 0xff);
		break;

	default:
		return (-1);
	}
	val = htonl(val);
	return (val);
}
#endif /* EXCELAN */

#endif /* USE_NNTP */
