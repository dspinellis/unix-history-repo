/*
 * NNTP client routines.
 *
 * %W%	(Berkeley) %G%
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "response_codes.h"

FILE	*ser_rd_fp;
FILE	*ser_wr_fp;

/*
 * server_init  Get a connection to the remote news server.
 *
 *	Parameters:	"machine" is the machine to connect to.
 *
 *	Returns:	-1 on error, 0 otherwise.
 *
 *	Side effects:	Connects to server.
 */

server_init(machine)
char	*machine;
{
	int	sockt_rd, sockt_wr;
	char	line[256];

	sockt_rd = getsocket(machine);	/* Get a socket to the */
	if (sockt_rd < 0) 		/* server, abort on */
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
		return (-1);
	}

	/* Now get the server's signon message */

	(void) get_server(line, sizeof(line));
	if (line[0] != CHAR_OK) {
		(void) close(sockt_rd);
		(void) close(sockt_wr);
		return (-1);		/* And abort if it's not good */
	}
	return (0);
}


/*
 * getsocket -- get us a socket connected to the news server.
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

getsocket(machine)
char	*machine;
{
	int	s;
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
	bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;

	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) { /* Get the socket */
		perror("socket");
		return (-1);
	}

	/* And then connect */

	if (connect(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("connect");
		return (-1);
	}

	return (s);
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
	/* fprintf(stderr, ">>> %s\n", string); */
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
	/* fprintf(stderr, "<<< %s\n", string); */

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

	put_server("QUIT");
	(void) get_server(ser_line, sizeof(ser_line));

	(void) fclose(ser_wr_fp);
	(void) fclose(ser_rd_fp);
}
