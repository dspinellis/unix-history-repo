/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)send.c	5.6 (Berkeley) 2/17/88";
#endif /* not lint */

/*
 *******************************************************************************
 *
 *  send.c --
 *
 *	Routine to send request packets to a name server.
 *
 *	Modified version of 4.3BSD BIND  res_send.c 5.5 (Berkeley) 9/14/85
 *
 *******************************************************************************
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include "res.h"

/*
 *  Initialize the socket address info struct.
 */

static struct sockaddr_in sin = { 
    AF_INET,  
};


/*
 *******************************************************************************
 *
 *   SendRequest --
 *
 *	Sends a request packet to a name server whose address
 *	is specified by the first argument and returns with
 *	the answer packet.
 *
 *  Results:
 *	SUCCESS		- the request was sent and an answer 
 *			  was received.
 *	TIME_OUT	- the virtual circuit connection timed-out 
 *			  or a reply to a datagram wasn't received.
 *
 *
 *******************************************************************************
 */

int
SendRequest(nsAddrPtr, buf, buflen, answer, anslen, trueLenPtr, printanswer)
	struct in_addr 	*nsAddrPtr;
	char 		*buf;
	int 		buflen;
	char 		*answer;
	int 		anslen;
	int 		*trueLenPtr;
	int		printanswer;
{
	struct timeval 	timeout;
	register int 	n;
	u_short 	packetId, len;
	short 		length;
	char 		*cp;
	int 		retry, v_circuit, resplen;
	int 		dsmask;

	int 		numTimeOuts	= 0;
	HEADER 		*requestPtr	= (HEADER *) buf;
	HEADER 		*answerPtr	= (HEADER *) answer;


	if (_res.options & RES_DEBUG2) {
	    printf("------------\nSendRequest(), len %d\n", buflen);
	    Print_query(buf, buf+buflen, 1);
	}
	sockFD = -1;

	/*
	 * See if a virtual circuit is required or desired.
	 */
	v_circuit = (_res.options & RES_USEVC) || buflen > PACKETSZ;

	packetId = requestPtr->id;

	sin.sin_port	= htons(NAMESERVER_PORT);
	sin.sin_addr	= *nsAddrPtr;

	/*
	 * Send request, RETRY times, or until successful
	 */
	for (retry = _res.retry; --retry >= 0; ) {
		if (v_circuit) {
			/*
			 * Use virtual circuit.
			 */
			if (sockFD < 0)
				sockFD = socket(AF_INET, SOCK_STREAM, 0);

			if (connect(sockFD, &sin, sizeof(sin)) < 0) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				(void) close(sockFD);
				sockFD = -1;
				continue;
			}
			/*
			 * Send length & message
			 */
			len = htons(buflen);
			if (write(sockFD, &len, sizeof(len)) != sizeof(len) ||
			    write(sockFD, buf, buflen) != buflen) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				(void) close(sockFD);
				sockFD = -1;
				continue;
			}
			/*
			 * Receive length & response
			 */
			cp = answer;
			length = sizeof(short);
			while(length > 0 && (n = read(sockFD, cp, length)) > 0){
				cp += n;
				length -= n;
			}
			if (n <= 0) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				(void) close(sockFD);
				sockFD = -1;
				continue;
			}
			cp = answer;
			resplen = length = ntohs(*(short *)cp);
			while(length > 0 && (n = read(sockFD, cp, length)) > 0){
				cp += n;
				length -= n;
			}
			if (n <= 0) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				(void) close(sockFD);
				sockFD = -1;
				continue;
			}
		} else {
			/*
			 * Use datagrams.
			 */
			if (sockFD < 0)
				sockFD = socket(AF_INET, SOCK_DGRAM, 0);

			if (sendto(sockFD, buf, buflen, 0, &sin,
 			    sizeof(sin)) != buflen) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
			}
			/*
			 * Wait for reply 
			 */
			timeout.tv_sec = _res.retrans;
			timeout.tv_usec = 0;
			dsmask = 1 << sockFD;
			n = select(sockFD+1, &dsmask, 0, 0, &timeout);
			if (n < 0) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				continue;
			}
			if (n == 0) {
				/*
				 * timeout
				 */
				if (_res.options & RES_DEBUG) {
				    printf("Timeout %d\n", ++numTimeOuts);
				}
				continue;
			}
 			if ((resplen = recv(sockFD, answer, anslen, 0)) <= 0) {
				if (_res.options & RES_DEBUG) {
				    perror("SendRequest");
				}
				continue;
			}
			if (packetId != answerPtr->id) {
				/*
				 * response from old query, ignore it
				 */
				if (_res.options & RES_DEBUG2) {
					printf("------------\nOld answer:\n");
					Print_query(answer, answer+resplen, 1);
				}
				continue;
			}
			if (!(_res.options & RES_IGNTC) && answerPtr->tc) {
				/*
				 * get rest of answer
				 */
				if (_res.options & RES_DEBUG) {
					printf("truncated answer\n");
				}
				(void) close(sockFD);
				sockFD = -1;
				retry = _res.retry;
				v_circuit = 1;
				continue;
			}
		}
		if (_res.options & RES_DEBUG && printanswer) {
		    if (_res.options & RES_DEBUG2)
			printf("------------\nGot answer (%d bytes):\n",
			    resplen);
		    else
			printf("------------\nGot answer:\n");
		    Print_query(answer, answer+resplen, 1);
		}
		(void) close(sockFD);
		sockFD = -1;
		*trueLenPtr = resplen;
		return (SUCCESS);
	}
	(void) close(sockFD);
	sockFD = -1;
	return (TIME_OUT);
}
