
/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)res_send.c	6.1 (Berkeley) %G%";
#endif not lint

/*
 * Send query to name server and wait for reply.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <errno.h>
#include <arpa/nameser.h>
#include <arpa/resolv.h>

extern int errno;

res_send(buf, buflen, answer, anslen)
	char *buf;
	int buflen;
	char *answer;
	int anslen;
{
	register int n;
	int s, retry, v_circuit, resplen, ns;
	u_short id, len;
	char *cp;
	int dsmask;
	struct timeval timeout;
	HEADER *hp = (HEADER *) buf;
	HEADER *anhp = (HEADER *) answer;

#ifdef DEBUG
	if (_res.options & RES_DEBUG) {
		printf("res_send()\n");
		p_query(buf);
	}
#endif DEBUG
	if (!(_res.options & RES_INIT))
		if (res_init() == -1) {
			return(-1);
		}
	s = -1;
	v_circuit = (_res.options & RES_USEVC) || buflen > PACKETSZ;
	id = hp->id;
	/*
	 * Send request, RETRY times, or until successful
	 */
	for (retry = _res.retry; --retry >= 0; ) {
	   for (ns = 0; ns < _res.nscount; ns++) {
#ifdef DEBUG
		if (_res.options & RES_DEBUG)
			printf("Querying server (# %d) address = %s\n", ns+1,
			      inet_ntoa(_res.nsaddr_list[ns].sin_addr.s_addr)); 
#endif DEBUG
		if (v_circuit) {
			/*
			 * Use virtual circuit.
			 */
			if (s < 0)
				s = socket(AF_INET, SOCK_STREAM, 0);
			if (connect(s, &(_res.nsaddr_list[ns]), 
			   sizeof(struct sockaddr)) < 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("connect failed %d\n", errno);
#endif DEBUG
				(void) close(s);
				s = -1;
				continue;
			}
			/*
			 * Send length & message
			 */
			len = htons(buflen);
			if (write(s, &len, sizeof(len)) != sizeof(len) ||
				    write(s, buf, buflen) != buflen) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("write failed %d\n", errno);
#endif DEBUG
				(void) close(s);
				s = -1;
				continue;
			}
			/*
			 * Receive length & response
			 */
			cp = answer;
			len = sizeof(short);
			while (len > 0 && (n = read(s, cp, len)) > 0) {
				cp += n;
				len -= n;
			}
			if (n <= 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("read failed %d\n", errno);
#endif DEBUG
				(void) close(s);
				s = -1;
				continue;
			}
			cp = answer;
			resplen = len = ntohs(*(short *)cp);
			while (len > 0 && (n = read(s, cp, len)) > 0) {
				cp += n;
				len -= n;
			}
			if (n <= 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("read failed %d\n", errno);
#endif DEBUG
				(void) close(s);
				s = -1;
				continue;
			}
		} else {
			/*
			 * Use datagrams.
			 */
			if (s < 0)
				s = socket(AF_INET, SOCK_DGRAM, 0);
			if (sendto(s, buf, buflen, 0, &_res.nsaddr_list[ns],
			    sizeof(struct sockaddr)) != buflen) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG) 
					printf("sendto errno = %d\n", errno);
#endif DEBUG
			}
			/*
			 * Wait for reply 
			 */
			timeout.tv_sec = 
				((_res.retrans * _res.retry) / _res.nscount);
			timeout.tv_usec = 0;
			dsmask = 1 << s;
			n = select(s+1, &dsmask, 0, 0, &timeout);
			if (n < 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("select errno = %d\n", errno);
#endif DEBUG
				continue;
			}
			if (n == 0) {
				/*
				 * timeout
				 */
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("timeout\n");
#endif DEBUG
				continue;
			}
			if ((resplen = recv(s, answer, anslen, 0)) <= 0) {
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("recvfrom, errno=%d\n", errno);
#endif DEBUG
				continue;
			}
			if (id != anhp->id) {
				/*
				 * response from old query, ignore it
				 */
#ifdef DEBUG
				if (_res.options & RES_DEBUG) {
					printf("old answer:\n");
					p_query(answer);
				}
#endif DEBUG
				continue;
			}
			if (!(_res.options & RES_IGNTC) && anhp->tc) {
				/*
				 * get rest of answer
				 */
#ifdef DEBUG
				if (_res.options & RES_DEBUG)
					printf("truncated answer\n");
#endif DEBUG
				(void) close(s);
				s = -1;
				retry = _res.retry;
				v_circuit = 1;
				continue;
			}
		}
#ifdef DEBUG
		if (_res.options & RES_DEBUG) {
			printf("got answer:\n");
			p_query(answer);
		}
#endif DEBUG
		(void) close(s);
		return (resplen);
	   }
	}
	(void) close(s);
	errno = ETIMEDOUT;
	return (-1);
}
