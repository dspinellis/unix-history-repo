/*
 *	Create a bid for the command on the command line 
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <errno.h>
#include <pwd.h>
#include "dsh.h"
#include "dbid.h"

#define TOKENSIZE 120

/*
 *	Get the next token
 */
char *
nexttoken (p, token)
char	*p;
char	*token;
{
    while (*p == ' ' || *p == '\t')
	p++;
    while (*p != ' ' && *p != '\t' && *p != 0)
	*token++ = *p++;
    *token = 0;
    return (p);
}

main (argc, argv)
int	argc;
char	*argv[];
{
    char	token[TOKENSIZE];
    char	*p;
    bool	allfound;
    double	la[3];
    int		fd, rv;
    int		pid, port;
    struct bidmsg	b;
    struct passwd	*pwd;
    struct passwd	*getpwuid();

    if (argc != 5) {
	fprintf (stderr, "dbid usage: dbid <command> <host> <port#> <host>\n");
	for (rv = 0; rv <argc; rv++) {
	    fprintf (stderr, "%s || ", argv[rv]);
	}
	fprintf (stderr, "\n");
	exit (-1);
    }

    /* find the elements of a pipe */
    getpath ();
    p = argv[1];
    strcpy (token, "|");
    allfound = TRUE;
    while (*p != 0) {
	if (*p != 0 && strcmp ("|", token) == 0) {
	    p = nexttoken (p, token);
	    if (*token != 0) {
		if (findcmd (token) != 0) {
		    allfound = FALSE;
		    break;
		}
	    }
	}
	p = nexttoken (p, token);
    }

    /* get the load average */
    getloadave (la);

    /* simple bidding */
    if (allfound) {
	b.bm_bid = 10.0 / la[0];
    } else {
	b.bm_bid = NOBID;
    }
    strcpy (b.bm_host, argv[4]);

    /* get the directory to use */
    pwd = getpwuid (getuid());
    pid = getpid();
    rv = getstringrc (".dshrc", "dir", token);
    if (rv < 0) {
	sprintf (b.bm_dir, "%s/dsh%d", pwd->pw_dir, pid);
    } else {
	if (token[0] == '/' || token[0] == '~') {
	    /* this is an absolute path */
	    sprintf (b.bm_dir, "%s/dsh%d", token, pid);
	} else {
	    /* a relative path */
	    sprintf (b.bm_dir, "%s/%s/dsh%d", pwd->pw_dir, token, pid);
	}
    }

    /* return bid */
    port = 0;				/* no particular port */
    fd = makedgsocket (&port);
    if (fd < 0) {
	error ("dbid: couldn't open socket to send over");
    }
    /*
    fprintf (stderr, "senddg (%d, %x, %d, %s, %d)\n",
        fd, &b, sizeof (struct bidmsg), argv[2], atoi(argv[3]));
    */
    rv = senddg (fd, &b, sizeof (struct bidmsg), argv[2], atoi(argv[3]));
    if (rv < 0) {
	error ("dbid: couldn't send bid");
    }
}
