/*	socket.h	4.4	81/11/08	*/

/*
 * Externally visible attributes of sockets:
 * types and options.
 */
#define	SOCK_STREAM	0		/* stream socket */
#define	SOCK_DGRAM	1		/* datagram socket */
#define	SOCK_RAW	2		/* raw-protocol interface */
#define	SOCK_RDM	3		/* reliably-delivered message */
/* ... */

/*
 * Option flags per-socket.
 */
#define	SO_DEBUG	0x01		/* turn on debugging info recording */
#define	SO_ACCEPT	0x02		/* willing to accept connection */
#define	SO_NBIO		0x04		/* don't block on this socket */
#define	SO_INTR		0x08		/* interrupt when data available */
#define	SO_NEWFD	0x10		/* give new fd's for each connect */
