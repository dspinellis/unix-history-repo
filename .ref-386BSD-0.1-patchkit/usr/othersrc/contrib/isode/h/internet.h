/* internet.h - TCP/IP abstractions */

/* 
 * $Header: /f/osi/h/RCS/internet.h,v 7.5 91/02/22 09:24:43 mrose Interim $
 *
 *
 * $Log:	internet.h,v $
 * Revision 7.5  91/02/22  09:24:43  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/07  12:39:26  mrose
 * update
 * 
 * Revision 7.3  90/10/15  22:54:08  mrose
 * typo
 * 
 * Revision 7.2  90/01/11  18:36:01  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/19  16:18:01  mrose
 * dgram
 * 
 * Revision 7.0  89/11/23  21:55:45  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef	_INTERNET_
#define	_INTERNET_

#ifndef	_MANIFEST_
#include "manifest.h"
#endif

/*    SOCKETS */

#include "sys/socket.h"

#ifndef	SOMAXCONN
#define	SOMAXCONN	5
#endif

#if	defined(WIN) || defined(WINTLI)
#include "sys/in.h"
#else
#include "netinet/in.h"
#endif


int	start_tcp_client ();
int	start_tcp_server ();

#ifdef	SOCKETS
int	join_tcp_client ();
int	join_tcp_server ();

#define	read_tcp_socket		read
#define	write_tcp_socket	write
#define	select_tcp_socket	selsocket
#endif

#ifdef	WINTLI
int	join_tcp_client ();
int	join_tcp_server ();

#define	select_tcp_socket	selsocket
#endif

#ifdef	EXOS

#ifdef	SYS5
#define	join_tcp_client(s,f) \
	(accept ((s), (struct sockaddr *) (f)) != NOTOK ? (s) : NOTOK)
#define	join_tcp_server(s,t)	connect ((s), (struct sockaddr *) (t))

#define	read_tcp_socket		read
#define	write_tcp_socket	write
#define	close_tcp_socket	close
#define	select_tcp_socket	selsocket
#endif
#endif

int	read_tcp_socket ();
int	write_tcp_socket ();
int	close_tcp_socket ();

int	select_tcp_socket ();

/*    UDP */

#ifdef	SOCKETS
#ifndef	_DGRAM_
#include "dgram.h"
#endif

int	start_udp_server ();
#define	start_udp_client	start_udp_server

#define	join_udp_server(fd,sock) \
		join_dgram_aux ((fd), (struct sockaddr *) (sock), 0)
#define	join_udp_client(fd,sock) \
		join_dgram_aux ((fd), (struct sockaddr *) (sock), 1)

#define	read_udp_socket		read_dgram_socket
#define	write_udp_socket	write_dgram_socket
#define	close_udp_socket	close_dgram_socket

#define	select_udp_socket	select_dgram_socket
#define	check_udp_socket	check_dgram_socket
#endif

/*    NETDB */

#if	defined(SOCKETS) || defined (WINTLI) || defined (WIN)
#include <netdb.h>
#endif


#if	defined(BIND) && !defined(h_addr)
#define	h_addr	h_addr_list[0]
#endif

#define	inaddr_copy(hp,sin) \
    bcopy ((hp) -> h_addr, (char *) &((sin) -> sin_addr), (hp) -> h_length)


#ifdef	EXOS
struct hostent {
    char   *h_name;		/* official name */
    char  **h_aliases;		/* alias list */
    int     h_addrtype;		/* address type: AF_INET */
    int     h_length;		/* address length: sizeof (u_long) == 4 */
    char   *h_addr;		/* address value: (struct in_addr *) */
};

struct servent {
    char   *s_name;		/* official name */
    char  **s_aliases;		/* alias list */
    int     s_port;		/* port number */
    char   *s_proto;		/* protocol beneath service */
};


struct hostent *gethostbyaddr (), *gethostbyname ();
struct servent *getservbyname ();
#endif


struct hostent *gethostbystring ();

/*    INET */

/* under BSD42, we could simply include <arpa/inet.h> instead.  However,
   the definition of inet_addr contained therein causes problems with some
   compilers. */

char   *inet_ntoa ();
#ifndef	DG
u_long	inet_addr ();
#ifndef	HPUX
u_long	inet_network ();
#else
int	inet_network ();
#endif
#else
struct in_addr inet_addr (), inet_network ();
#endif
#endif
