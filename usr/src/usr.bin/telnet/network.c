#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>

#include <errno.h>

#include <arpa/telnet.h>

#include "ring.h"

#include "defines.h"
#include "externs.h"

Ring	netoring, netiring;
char	netobuf[2*BUFSIZ], netibuf[BUFSIZ];

/*
 * Initialize internal network data structures.
 */

init_network()
{
    ring_init(&netoring, netobuf, sizeof netobuf);
    ring_init(&netiring, netibuf, sizeof netibuf);
    NetTrace = stdout;
}


/*
 * Check to see if any out-of-band data exists on a socket (for
 * Telnet "synch" processing).
 */

int
stilloob()
{
    static struct timeval timeout = { 0 };
    fd_set	excepts;
    int value;

    do {
	FD_ZERO(&excepts);
	FD_SET(net, &excepts);
	value = select(net+1, (fd_set *)0, (fd_set *)0, &excepts, &timeout);
    } while ((value == -1) && (errno == EINTR));

    if (value < 0) {
	perror("select");
	quit();
    }
    if (FD_ISSET(net, &excepts)) {
	return 1;
    } else {
	return 0;
    }
}


/*
 *  setneturg()
 *
 *	Sets "neturg" to the current location.
 */

void
setneturg()
{
    ring_mark(&netoring);
}


/*
 *  netflush
 *		Send as much data as possible to the network,
 *	handling requests for urgent data.
 *
 *		The return value indicates whether we did any
 *	useful work.
 */


int
netflush()
{
    int n;

    if ((n = ring_full_consecutive(&netoring)) > 0) {
	if (!ring_at_mark(&netoring)) {
	    n = send(net, netoring.consume, n, 0);	/* normal write */
	} else {
	    /*
	     * In 4.2 (and 4.3) systems, there is some question about
	     * what byte in a sendOOB operation is the "OOB" data.
	     * To make ourselves compatible, we only send ONE byte
	     * out of band, the one WE THINK should be OOB (though
	     * we really have more the TCP philosophy of urgent data
	     * rather than the Unix philosophy of OOB data).
	     */
	    n = send(net, netoring.consume, 1, MSG_OOB);/* URGENT data */
	}
    }
    if (n < 0) {
	if (errno != ENOBUFS && errno != EWOULDBLOCK) {
	    setcommandmode();
	    perror(hostname);
	    NetClose(net);
	    ring_clear_mark(&netoring);
	    longjmp(peerdied, -1);
	    /*NOTREACHED*/
	}
	n = 0;
    }
    if (netdata && n) {
	Dump('>', netoring.consume, n);
    }
    ring_consumed(&netoring, n);
    return n > 0;
}
