#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <errno.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/time.h>
#include "dsh.h"
#include "dbid.h"

#define MAXWAIT	90		/* maximum time to wait */
#define MINWAIT 30		/* minimum time to wait */

int	errno;			/* global error location */
bool	hflg;			/* TRUE if specific host called for */
char	*spechost;		/* the specific host */

struct bid	*thebid;	/* pointer to the bids */

char		*myhostname();
long		myhostaddr();
bool		aflg;

/*
 *	get bids from the machines 
 */
getbids (av, thehost)
char	*av[];			/* the command */
struct hostdef	*thehost;	/* the hosts to use */
{
    int			sock, port;
    char		portstr[10];
    int			waiting;
    int			fds, readfds;
    int			rv, nodes, responses;
    int			argc, ac;
    char		*argv[100];
    struct sockaddr_in addr;
    struct bidmsg	bm;
    struct hostdef	*hp;
    struct bid		*bp;
    bool		done;
    struct timeval	timeout;

    port = 0;
    sock = makedgsocket (&port);
    for (nodes = 0, hp = thehost; hp != 0; nodes++, hp = hp->h_next) {

	if (!hflg || (strcmp (spechost, hp->h_name) == 0)) {

	    /* build the command */
	    argc = 0;
	    argv[argc++] = BIDCMD;
	    argv[argc++] = "\"";
	    for (ac = 0; av[ac] != 0; ac++) {
		argv[argc++] = av[ac];
	    }
	    argv[argc++] = "\"";
	    argv[argc++] = myhostname();
	    sprintf (portstr, "%d", port);
	    argv[argc++] = portstr;
	    argv[argc++] = hp->h_name;
	    argv[argc] = 0;

	    /* start up the bidcommand */
	    rshell (hp, argv, FALSE, TRUE, FALSE, TRUE);
	}
    }

    /* wait for replies */
    waiting = responses = 0;
    thebid = 0;
    done = FALSE;
    do {
	readfds = 1<<sock;
	timeout.tv_sec = 2;
	timeout.tv_usec = 0;
	fds = select (20, &readfds, 0, 0, &timeout);
	if (fds == 0)
	    waiting += 2;
	if (readfds & (1<<sock)) {
	    rv = recvdg (sock, &bm, sizeof (struct bidmsg));
	    if (rv < 0) {
		warn ("receiving bid");
	    }
	    for (hp = thehost; hp != 0; hp = hp->h_next) {
		if (strcmp (bm.bm_host, hp->h_name) == 0) {
		    strcpy (hp->h_dir, bm.bm_dir);
		    bp = new (struct bid);
		    bp->b_next = thebid;
		    /*
		    printf ("%s %s %f %f\n", hp->h_name, hp->h_dir,
			hp->h_weight, bm.bm_bid);
		    */
		    bp->b_bid = bm.bm_bid * hp->h_weight;
		    bp->b_host = hp;
		    thebid = bp;
		    responses++;
		    break;
		}
	    }
	}
	if (!aflg) {
	    done = (waiting > MINWAIT && responses > 0) || (waiting > MAXWAIT)
		   || (nodes <= responses);
	} else {
	    done = (waiting > MAXWAIT) || (nodes <= responses);
	}
    } while (!done);
}


/*
 *	find the highest bidder
 */
struct hostdef *
highest()
{
    struct bid		*bp, *bpp;
    double		high;

    /* find the highest bid */
    high = 0.0;
    bpp = 0;
    for (bp = thebid; bp != 0; bp = bp->b_next) {
	if (bp->b_bid > high) {
	    high = bp->b_bid;
	    bpp = bp;
	}
    }

    if (bpp == 0) {
	return (0);
    } else {

	/* remove highest from the list */
	if (bpp == thebid) {
	    thebid = bpp->b_next;
	} else {
	    for (bp = thebid; bp->b_next != bpp; bp = bp->b_next);
	    bp->b_next = bpp->b_next;
	}
	return (bpp->b_host);
    }
}
