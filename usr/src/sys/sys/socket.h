/*	socket.h	4.5	81/11/14	*/

/*
 * Externally visible attributes of sockets.
 */

/*
 * Socket types.
 *
 * The kernel implement these abstract (session-layer) socket
 * services, with extra protocol on top of network services
 * if necessary.
 */
#define	SOCK_STREAM	0		/* stream socket */
#define	SOCK_DGRAM	1		/* datagram socket */
#define	SOCK_RAW	2		/* raw-protocol interface */
#define	SOCK_RDM	3		/* reliably-delivered message */

/*
 * Option flags per-socket.
 */
#define	SO_DEBUG	0x01		/* turn on debugging info recording */
#define	SO_ACCEPTCONN	0x02		/* willing to accept connection */
#define	SO_NBIO		0x04		/* don't block on this socket */
#define	SO_INTNOTIFY	0x08		/* interrupt when data available */
#define	SO_NEWFDONCONN	0x10		/* give new fd's for each connect */
