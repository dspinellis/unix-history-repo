/*
 * NNTP client routines.
 */

#ifndef lint
static char	*sccsid = "@(#)clientlib.c	1.5	(Berkeley) 10/15/87";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#if defined(AF_DECnet) && defined(ultrix)
# ifndef DECNET
#  define DECNET
# endif
#endif

#ifdef DECNET
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif

#include "response_codes.h"

FILE	*ser_rd_fp = NULL;
FILE	*ser_wr_fp = NULL;

/*
 * getserverbyfile	Get the name of a server from a named file.
 *			Handle white space and comments.
 *			Use NNTPSERVER environment variable if set.
 *
 *	Parameters:	"file" is the name of the file to read.
 *
 *	Returns:	Pointer to static data area containing the
 *			first non-ws/comment line in the file.
 *			NULL on error (or lack of entry in file).
 *
 *	Side effects:	None.
 */

char *
getserverbyfile(file)
char	*file;
{
	register FILE	*fp;
	register char	*cp;
	static char	buf[256];
	char		*index();
	char		*getenv();

	if (cp = getenv("NNTPSERVER")) {
		(void) strcpy(buf, cp);
		return (buf);
	}

	if (file == NULL)
		return (NULL);

	fp = fopen(file, "r");
	if (fp == NULL)
		return (NULL);

	while (fgets(buf, sizeof (buf), fp) != NULL) {
		if (*buf == '\n' || *buf == '#')
			continue;
		cp = index(buf, '\n');
		if (cp)
			*cp = '\0';
		(void) fclose(fp);
		return (buf);
	}

	(void) fclose(fp);
	return (NULL);			 /* No entry */
}


/*
 * server_init  Get a connection to the remote news server.
 *
 *	Parameters:	"machine" is the machine to connect to.
 *
 *	Returns:	-1 on error
 *			server's initial response code on success.
 *
 *	Side effects:	Connects to server.
 *			"ser_rd_fp" and "ser_wr_fp" are fp's
 *			for reading and writing to server.
 */

server_init(machine)
char	*machine;
{
	int	sockt_rd, sockt_wr;
	char	line[256];
	char	*cp;
	char	*index();

#ifdef DECNET
	cp = index(machine, ':');

	if (cp && cp[1] == ':') {
		*cp = '\0';
		sockt_rd = get_dnet_socket(machine);
	} else
		sockt_rd = get_tcp_socket(machine);
#else
	sockt_rd = get_tcp_socket(machine);
#endif

	if (sockt_rd < 0)
		return (-1);

	/*
	 * Now we'll make file pointers (i.e., buffered I/O) out of
	 * the socket file descriptor.  Note that we can't just
	 * open a fp for reading and writing -- we have to open
	 * up two separate fp's, one for reading, one for writing.
	 */

	if ((ser_rd_fp = fdopen(sockt_rd, "r")) == NULL) {
		perror("server_init: fdopen #1");
		return (-1);
	}

	sockt_wr = dup(sockt_rd);
	if ((ser_wr_fp = fdopen(sockt_wr, "w")) == NULL) {
		perror("server_init: fdopen #2");
		ser_rd_fp = NULL;		/* from above */
		return (-1);
	}

	/* Now get the server's signon message */

	(void) get_server(line, sizeof(line));
	return (atoi(line));
}


/*
 * get_tcp_socket -- get us a socket connected to the news server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *
 *	Returns:	Socket connected to the news server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via perror.
 */

get_tcp_socket(machine)
char	*machine;
{
	int	s, x = 0;
	register char **cp;
	struct	sockaddr_in sin;
	struct	servent *getservbyname(), *sp;
	struct	hostent *gethostbyname(), *hp;

	if ((sp = getservbyname("nntp", "tcp")) ==  NULL) {
		fprintf(stderr, "nntp/tcp: Unknown service.\n");
		return (-1);
	}

	if ((hp = gethostbyname(machine)) == NULL) {
		fprintf(stderr, "%s: Unknown host.\n", machine);
		return (-1);
	}

	bzero((char *) &sin, sizeof(sin));
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;

	/*
	 * The following is kinda gross.  The name server under 4.3
	 * returns a list of addresses, each of which should be tried
	 * in turn if the previous one fails.  However, 4.2 hostent
	 * structure doesn't have this list of addresses.
	 * Under 4.3, h_addr is a #define to h_addr_list[0].
	 * We use this to figure out whether to include the NS specific
	 * code...
	 */

#ifdef	h_addr

	/* get a socket and initiate connection -- use multiple addresses */

	for (cp = hp->h_addr_list; cp && *cp; cp++) {
		s = socket(hp->h_addrtype, SOCK_STREAM, 0);
		if (s < 0) {
			perror("socket");
			return (-1);
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
		return (-1);
	}
#else	/* no name server */

	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) { /* Get the socket */
		perror("socket");
		return (-1);
	}

	/* And then connect */

	bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("connect");
		(void) close(s);
		return (-1);
	}

#endif

	return (s);
}

#ifdef DECNET
/*
 * get_dnet_socket -- get us a socket connected to the news server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *
 *	Returns:	Socket connected to the news server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via nerror.
 */

get_dnet_socket(machine)
char	*machine;
{
	int	s, area, node;
	struct	sockaddr_dn sdn;
	struct	nodeent *getnodebyname(), *np;

	bzero((char *) &sdn, sizeof(sdn));

	switch (s = sscanf( machine, "%d%*[.]%d", &area, &node )) {
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
			if ((np = getnodebyname(machine)) == NULL) {
				fprintf(stderr, 
					"%s: Unknown host.\n", machine);
				return (-1);
			} else {
				bcopy(np->n_addr, 
					(char *) sdn.sdn_add.a_addr, 
					np->n_length);
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
		return (-1);
	}

	/* And then connect */

	if (connect(s, (struct sockaddr *) &sdn, sizeof(sdn)) < 0) {
		nerror("connect");
		close(s);
		return (-1);
	}

	return (s);
}
#endif



/*
 * handle_server_response
 *
 *	Print some informative messages based on the server's initial
 *	response code.  This is here so inews, rn, etc. can share
 *	the code.
 *
 *	Parameters:	"response" is the response code which the
 *			server sent us, presumably from "server_init",
 *			above.
 *			"server" is the news server we got the
 *			response code from.
 *
 *	Returns:	-1 if the error is fatal (and we should exit).
 *			0 otherwise.
 *
 *	Side effects:	None.
 */

handle_server_response(response, server)
int	response;
char	*server;
{
    switch (response) {
	case OK_NOPOST:		/* fall through */
    		printf(
	"NOTE: This machine does not have permission to post articles.\n");
		printf(
	"      Please don't waste your time trying.\n\n");

	case OK_CANPOST:
		return (0);
		break;

	case ERR_ACCESS:
		printf(
   "This machine does not have permission to use the %s news server.\n",
		server);
		return (-1);
		break;

	default:
		printf("Unexpected response code from %s news server: %d\n",
			server, response);
		return (-1);
		break;
    }
	/*NOTREACHED*/
}


/*
 * put_server -- send a line of text to the server, terminating it
 * with CR and LF, as per ARPA standard.
 *
 *	Parameters:	"string" is the string to be sent to the
 *			server.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Talks to the server.
 *
 *	Note:		This routine flushes the buffer each time
 *			it is called.  For large transmissions
 *			(i.e., posting news) don't use it.  Instead,
 *			do the fprintf's yourself, and then a final
 *			fflush.
 */

void
put_server(string)
char *string;
{
#ifdef DEBUG
	fprintf(stderr, ">>> %s\n", string);
#endif
	fprintf(ser_wr_fp, "%s\r\n", string);
	(void) fflush(ser_wr_fp);
}


/*
 * get_server -- get a line of text from the server.  Strips
 * CR's and LF's.
 *
 *	Parameters:	"string" has the buffer space for the
 *			line received.
 *			"size" is the size of the buffer.
 *
 *	Returns:	-1 on error, 0 otherwise.
 *
 *	Side effects:	Talks to server, changes contents of "string".
 */

get_server(string, size)
char	*string;
int	size;
{
	register char *cp;
	char	*index();

	if (fgets(string, size, ser_rd_fp) == NULL)
		return (-1);

	if ((cp = index(string, '\r')) != NULL)
		*cp = '\0';
	else if ((cp = index(string, '\n')) != NULL)
		*cp = '\0';
#ifdef DEBUG
	fprintf(stderr, "<<< %s\n", string);
#endif

	return (0);
}


/*
 * close_server -- close the connection to the server, after sending
 *		the "quit" command.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Closes the connection with the server.
 *			You can't use "put_server" or "get_server"
 *			after this routine is called.
 */

void
close_server()
{
	char	ser_line[256];

	if (ser_wr_fp == NULL || ser_rd_fp == NULL)
		return;

	put_server("QUIT");
	(void) get_server(ser_line, sizeof(ser_line));

	(void) fclose(ser_wr_fp);
	(void) fclose(ser_rd_fp);
}
