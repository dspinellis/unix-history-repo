/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char Version[] = "@(#)send.c	e07@nikhef.nl (Eric Wassenaar) 930915";
#endif

#if defined(apollo) && defined(lint)
#define __attribute(x)
#endif

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/time.h>

#include <sys/types.h>		/* not always automatically included */
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#undef NOERROR			/* in <sys/streams.h> on solaris 2.x */
#include <arpa/nameser.h>
#include <resolv.h>

#include "port.h"		/* various portability definitions */

#define input			/* read-only input parameter */
#define output			/* modified output parameter */

#define bitset(a,b)	(((a) & (b)) != 0)
#define setalarm(n)	(void) alarm((unsigned int)n)

extern int errno;
extern res_state_t _res;	/* defined in res_init.c */

static int timeout;		/* connection read timeout */

/* extern */
char *inet_ntoa		PROTO((struct in_addr));

/* send.c */
#ifdef HOST_RES_SEND
int res_send		PROTO((char *, int, char *, int));
#ifndef lint
void _res_close		PROTO((void));
#endif
static int send_stream	PROTO((struct sockaddr_in *, char *, int, char *, int));
static int send_dgram	PROTO((struct sockaddr_in *, char *, int, char *, int));
#endif /*HOST_RES_SEND*/
static sigtype_t timer	PROTO((int));
int _res_connect	PROTO((int, struct sockaddr_in *, int));
int _res_write		PROTO((int, char *, int));
int _res_read		PROTO((int, char *, int));
static int recvsock	PROTO((int, char *, int));
void _res_setaddr	PROTO((struct sockaddr_in *, char *));
void _res_perror	PROTO((char *));

#ifdef HOST_RES_SEND

/*
** RES_SEND -- Send nameserver query and retrieve answer
** -----------------------------------------------------
**
**	Returns:
**		Length of nameserver answer buffer, if obtained.
**		-1 if an error occurred (errno set appropriately).
**
**	This is a simplified version of the BIND 4.8.3 res_send().
**	- Always use connected datagrams to get proper error messages.
**	- Do not only return ETIMEDOUT or ECONNREFUSED in datagram mode.
**	- Never leave a connection open after we got an answer.
**	- No special ECONNRESET handling when using virtual circuits.
*/

int
res_send(query, querylen, answer, anslen)
input char *query;			/* address of formatted query buffer */
input int querylen;			/* length of query buffer */
output char *answer;			/* address of buffer to store answer */
input int anslen;			/* maximum size of answer buffer */
{
	HEADER *bp = (HEADER *)answer;
	struct sockaddr_in *sin;
	int v_circuit;			/* virtual circuit or datagram switch */
	register int try, ns;
	register int n;

	/* make sure resolver has been initialized */
	if (!bitset(RES_INIT, _res.options) && res_init() == -1)
		return(-1);

	if (bitset(RES_DEBUG, _res.options))
	{
		printf("res_send()\n");
		fp_query(query, stdout);
	}

	/* use virtual circuit if requested or if necessary */
	v_circuit = bitset(RES_USEVC, _res.options) || (querylen > PACKETSZ);

/*
 * Do _res.retry attempts for each of the _res.nscount addresses.
 */
	for (try = 0; try < _res.retry; try++)
	{
	    for (ns = 0; ns < _res.nscount; ns++)
	    {
		sin = &nslist(ns);
		_res_setaddr(sin, (char *)NULL);
retry:
		if (bitset(RES_DEBUG, _res.options))
			printf("Querying server (# %d) %s address = %s\n", ns+1,
			    v_circuit ? "tcp" : "udp", inet_ntoa(sin->sin_addr));

		if (v_circuit)
		{
			/* at most one attempt per server */
			try = _res.retry;

			/* connect via virtual circuit */
			n = send_stream(sin, query, querylen, answer, anslen);
		}
		else
		{
			/* set datagram read timeout for recvsock */
			timeout = (_res.retrans << try);
			if (try > 0)
				timeout /= _res.nscount;
			if (timeout <= 0)
				timeout = 1;

			/* connect via datagram */
			n = send_dgram(sin, query, querylen, answer, anslen);

			/* check truncation; use v_circuit with same server */
			if (n > 0 && bp->tc)
			{
				if (bitset(RES_DEBUG, _res.options))
					(void) fprintf(stderr, "truncated answer\n");

				if (!bitset(RES_IGNTC, _res.options))
				{
					v_circuit = 1;
					goto retry;
				}
			}
		}

		if (n <= 0)
			continue;

		if (bitset(RES_DEBUG, _res.options))
		{
			printf("got answer:\n");
			fp_query(answer, stdout);
		}

		/* we have an answer; clear possible error condition */
		errno = 0;
		return(n);
	    }
	}

	return(-1);
}


/*
 * Note that this private version of res_send() is not only called
 * directly by 'host' but also indirectly by gethostbyname() or by
 * gethostbyaddr() via their resolver interface routines.
 */


/*
 * Provide dummy routine to prevent the real res_send() to be loaded.
 * This one is actually only called by endhostent() to close a socket
 * that was requested to stay open. But in this version sockets are
 * always closed after use.
 */

#ifndef lint

void
_res_close()
{
}

#endif

/*
** SEND_STREAM -- Query nameserver via virtual circuit
** ---------------------------------------------------
**
**	Returns:
**		Length of nameserver answer buffer, if obtained.
**		-1 if an error occurred.
**
**	Note that connect() is the call that is allowed to fail
**	under normal circumstances. All other failures generate
**	an unconditional error message.
*/

static int
send_stream(addr, query, querylen, answer, anslen)
input struct sockaddr_in *addr;		/* the server address to connect to */
input char *query;			/* address of formatted query buffer */
input int querylen;			/* length of query buffer */
output char *answer;			/* address of buffer to store answer */
input int anslen;			/* maximum size of answer buffer */
{
	int sock;
	register int n;

/*
 * Setup a virtual circuit connection.
 */
	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
	{
		_res_perror("socket");
		return(-1);
	}

	if (_res_connect(sock, addr, sizeof(*addr)) < 0)
	{
		if (bitset(RES_DEBUG, _res.options))
			_res_perror("connect");
		(void) close(sock);
		return(-1);
	}

	if (bitset(RES_DEBUG, _res.options))
		printf("connected to %s\n", inet_ntoa(addr->sin_addr));

/*
 * Send the query buffer.
 */
	if (_res_write(sock, query, querylen) < 0)
	{
		(void) close(sock);
		return(-1);
	}

/*
 * Read the answer buffer.
 */
	n = _res_read(sock, answer, anslen);
	if (n <= 0)
	{
		(void) close(sock);
		return(-1);
	}

/*
 * Never leave the socket open.
 */
	(void) close(sock);
	return(n);
}

/*
** SEND_DGRAM -- Query nameserver via datagram
** -------------------------------------------
**
**	Returns:
**		Length of nameserver answer buffer, if obtained.
**		-1 if an error occurred.
**
**	Inputs:
**		The global variable timeout should have been
**		set with the desired timeout value in seconds.
**
**	Sending to a nameserver datagram port with no nameserver running
**	will cause an ICMP port unreachable message to be returned. If the
**	socket is connected, we get an ECONNREFUSED error on the next socket
**	operation, and select returns if the error message is received.
**	Also, we get ENETUNREACH or EHOSTUNREACH errors if appropriate.
**	We thus get a proper error status before timing out.
**	This method usually works only if BSD >= 43.
**
**	Note that send() and recv() are now the calls that are allowed
**	to fail under normal circumstances. All other failures generate
**	an unconditional error message.
*/

static int
send_dgram(addr, query, querylen, answer, anslen)
input struct sockaddr_in *addr;		/* the server address to connect to */
input char *query;			/* address of formatted query buffer */
input int querylen;			/* length of query buffer */
output char *answer;			/* address of buffer to store answer */
input int anslen;			/* maximum size of answer buffer */
{
	HEADER *qp = (HEADER *)query;
	HEADER *bp = (HEADER *)answer;
	int sock;
	register int n;

/*
 * Setup a connected datagram socket.
 */
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0)
	{
		_res_perror("socket");
		return(-1);
	}

	if (connect(sock, (struct sockaddr *)addr, sizeof(*addr)) < 0)
	{
		_res_perror("connect");
		(void) close(sock);
		return(-1);
	}

/*
 * Send the query buffer.
 */
	if (send(sock, query, querylen, 0) != querylen)
	{
		if (bitset(RES_DEBUG, _res.options))
			_res_perror("send");
		(void) close(sock);
		return(-1);
	}

/*
 * Wait for the arrival of a reply, timeout, or error message.
 */
wait:
	n = recvsock(sock, answer, anslen);
	if (n <= 0)
	{
		if (bitset(RES_DEBUG, _res.options))
			_res_perror("recvfrom");
		(void) close(sock);
		return(-1);
	}

/*
 * Make sure it is the proper response by checking the packet id.
 */
	if (qp->id != bp->id)
	{
		if (bitset(RES_DEBUG, _res.options))
		{
			printf("old answer:\n");
			fp_query(answer, stdout);
		}
		goto wait;
	}

/*
 * Never leave the socket open.
 */
	(void) close(sock);
	return(n);
}

#endif /*HOST_RES_SEND*/

/*
** _RES_CONNECT -- Connect to a stream (virtual circuit) socket
** ------------------------------------------------------------
**
**	Returns:
**		0 if successfully connected.
**		-1 in case of failure or timeout.
**
**	Note that we use _res.retrans to override the default
**	connect timeout value.
*/

static jmp_buf timer_buf;

static sigtype_t
/*ARGSUSED*/
timer(sig)
int sig;
{
	longjmp(timer_buf, 1);
}


int
_res_connect(sock, addr, addrlen)
input int sock;
input struct sockaddr_in *addr;		/* the server address to connect to */
input int addrlen;
{
	if (setjmp(timer_buf) != 0)
	{
		errno = ETIMEDOUT;
		setalarm(0);
		return(-1);
	}

	(void) signal(SIGALRM, timer);
	setalarm(_res.retrans);

	if (connect(sock, (struct sockaddr *)addr, addrlen) < 0)
	{
		if (errno == EINTR)
			errno = ETIMEDOUT;
		setalarm(0);
		return(-1);
	}

	setalarm(0);
	return(0);
}

/*
** _RES_WRITE -- Write the query buffer via a stream socket
** --------------------------------------------------------
**
**	Returns:
**		Length of buffer if successfully transmitted.
**		-1 in case of failure (error message is issued).
**
**	The query is sent in two steps: first a single word with
**	the length of the buffer, followed by the buffer itself.
*/

int
_res_write(sock, buf, bufsize)
input int sock;
input char *buf;			/* address of formatted query buffer */
input int bufsize;			/* length of query buffer */
{
	u_short len;

/*
 * Write the length of the query buffer.
 */
	len = htons(bufsize);

	if (write(sock, (char *)&len, sizeof(len)) != sizeof(len))
	{
		_res_perror("write query length");
		return(-1);
	}

/*
 * Write the query buffer itself.
 */
	if (write(sock, buf, bufsize) != bufsize)
	{
		_res_perror("write query");
		return(-1);
	}

	return(bufsize);
}

/*
** _RES_READ -- Read the answer buffer via a stream socket
** -------------------------------------------------------
**
**	Returns:
**		Length of buffer if successfully received.
**		-1 in case of failure (error message is issued).
**
**	The answer is read in two steps: first a single word which
**	gives the length of the buffer, followed by the buffer itself.
**	If the answer is too long to fit into the supplied buffer,
**	only the portion that fits will be stored, the residu will be
**	flushed, and the truncation flag will be set.
*/

int
_res_read(sock, buf, bufsize)
input int sock;
output char *buf;			/* address of buffer to store answer */
input int bufsize;			/* maximum size of answer buffer */
{
	u_short len;
	char *buffer;
	int buflen;
	int reslen;
	register int n;

	/* set stream timeout for recvsock */
	timeout = 60;

/*
 * Read the length of answer buffer.
 */
	buffer = (char *)&len;
	buflen = sizeof(len);

	while (buflen > 0 && (n = recvsock(sock, buffer, buflen)) > 0)
	{
		buffer += n;
		buflen -= n;
	}

	if (buflen != 0)
	{
		_res_perror("read answer length");
		return(-1);
	}

/*
 * Terminate if length is zero.
 */
	len = ntohs(len);
	if (len == 0)
		return(0);

/*
 * Check for truncation.
 */
	reslen = 0;
	if ((int)len > bufsize)
	{
		reslen = len - bufsize;
		len = bufsize;
	}

/*
 * Read the answer buffer itself.
 */
	buffer = buf;
	buflen = len;

	while (buflen > 0 && (n = recvsock(sock, buffer, buflen)) > 0)
	{
		buffer += n;
		buflen -= n;
	}

	if (buflen != 0)
	{
		_res_perror("read answer");
		return(-1);
	}

/*
 * Discard the residu to keep connection in sync.
 */
	if (reslen > 0)
	{
		HEADER *bp = (HEADER *)buf;
		char resbuf[PACKETSZ];

		buffer = resbuf;
		buflen = reslen < sizeof(resbuf) ? reslen : sizeof(resbuf);

		while (reslen > 0 && (n = recvsock(sock, buffer, buflen)) > 0)
		{
			reslen -= n;
			buflen = reslen < sizeof(resbuf) ? reslen : sizeof(resbuf);
		}

		if (reslen != 0)
		{
			_res_perror("read residu");
			return(-1);
		}

		if (bitset(RES_DEBUG, _res.options))
			(void) fprintf(stderr, "response truncated\n");

		/* set truncation flag */
		bp->tc = 1;
	}

	return(len);
}

/*
** RECVSOCK -- Read from stream or datagram socket with timeout
** ------------------------------------------------------------
**
**	Returns:
**		Length of buffer if successfully received.
**		-1 in case of failure or timeout.
**
**	Inputs:
**		The global variable timeout should have been
**		set with the desired timeout value in seconds.
*/

static int
recvsock(sock, buffer, buflen)
input int sock;
output char *buffer;			/* current buffer address */
input int buflen;			/* remaining buffer size */
{
	fd_set fds;
	struct timeval wait;
	register int n;

	wait.tv_sec = timeout;
	wait.tv_usec = 0;

	/* FD_ZERO(&fds); */
	bzero((char *)&fds, sizeof(fds));
	FD_SET(sock, &fds);

/*
 * Wait for the arrival of data, or timeout.
 */
	n = select(FD_SETSIZE, &fds, (fd_set *)NULL, (fd_set *)NULL, &wait);
	if (n <= 0)
	{
		if (n == 0)
			errno = ETIMEDOUT;
		return(-1);
	}

/*
 * Fake an error if nothing was actually read.
 */
	n = recv(sock, buffer, buflen, 0);
	if (n == 0)
		errno = ECONNRESET;
	return(n);
}

/*
** _RES_PERROR -- Issue perror message including host info
** -------------------------------------------------------
**
**	Returns:
**		None.
**
**	The address and name of the server host must be set
**	in advance via _res_setaddr().
*/

static struct in_addr curaddr;	/* current address of server host */
static char *curhost = NULL;	/* current name of server host */

void
_res_setaddr(sin, host)
input struct sockaddr_in *sin;		/* address of host to connect to */
input char *host;			/* name of host to connect to */
{
	curaddr = sin->sin_addr;
	curhost = host;
}


void
_res_perror(message)
input char *message;			/* perror message string */
{
	int save_errno;

	save_errno = errno;
	if (curaddr.s_addr)
		(void) fprintf(stderr, "%s ", inet_ntoa(curaddr));
	if (curhost)
		(void) fprintf(stderr, "(%s) ", curhost);
	errno = save_errno;
	perror(message);
}
