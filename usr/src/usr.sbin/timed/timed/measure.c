/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)measure.c	1.1 (Berkeley) %G%";
#endif not lint

#include "globals.h"
#include <protocols/timed.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>

#define BIASP	 	43199999
#define BIASN		-43200000
#define MODULO	 	86400000
#define PROCESSING_TIME	5 	/* ms. to reduce error in measurement */

#define PACKET_IN	1024
#define PACKET_OUT	160

extern int id;
int measure_delta;
extern int sock_raw;
extern struct sockaddr_in server;

/*
 * Measures the differences between machines' clocks using
 * ICMP timestamp messages.
 * Called by master with ckrange = 1, by clockdiff with ckrange = 0.
 */

measure(wait, ckrange)
struct timeval *wait;
int ckrange;
{
	int length;
	int status;
	int msgcount, trials, ntransmitted;
	int cc, count, ready, found;
	long sendtime, recvtime, histime;
	long min1, min2, diff;
	long delta1, delta2;
	struct timeval tv1, tv2, tout;
	struct sockaddr_in where;
	u_char packet[PACKET_IN];
	register struct icmp *icp = (struct icmp *) packet;
	
	min1 = min2 = 0x7fffffff;
	ntransmitted = 1;
	trials = 0;
	status = GOOD;
	measure_delta = HOSTDOWN;

/* empties the icmp input queue */
empty:
	tout.tv_sec = tout.tv_usec = 0;
	ready = 1<<sock_raw;
	found = select(20, &ready, (int *)0, (int *)0, &tout);
	if (found) {
		length = sizeof(struct sockaddr_in);
		cc = recvfrom(sock_raw, (char *)packet, PACKET_IN, 0, 
							&where, &length);
		if (cc < 0)
			return(-1);
		goto empty;
	}

	/*
	 * To measure the difference, select MSGS messages whose round-trip
	 * time is smaller than RANGE if ckrange is 1, otherwise simply
	 * select MSGS messages regardless of round-trip transmission time.
	 * Choose the smallest transmission time in each of the two directions.
	 * Use these two latter quantities to compute the delta between
	 * the two clocks.
	 */

	msgcount = 1;
	while(msgcount <= MSGS) {
		icp->icmp_type = ICMP_TSTAMP;
		icp->icmp_code = 0;
		icp->icmp_cksum = 0;
		icp->icmp_seq = ntransmitted++;
		icp->icmp_id = id;
		icp->icmp_rtime = 0;
		icp->icmp_ttime = 0;

		tout.tv_sec = wait->tv_sec;
		tout.tv_usec = wait->tv_usec;
		ready = 1<<sock_raw;

		where = server;

    		(void)gettimeofday (&tv1, (struct timezone *)0);
		sendtime = icp->icmp_otime = (tv1.tv_sec % (24*60*60)) * 1000 
							+ tv1.tv_usec / 1000;
		icp->icmp_cksum = in_cksum((u_short *)icp, PACKET_OUT);
	
		count = sendto(sock_raw, (char *)packet, PACKET_OUT, 0, 
				&where, sizeof(where));
		if (count < 0) {
			status = UNREACHABLE;
			return(-1);
		}
		found = select(20, &ready, (int *)0, (int *)0, &tout);
		if (found) {
			length = sizeof(struct sockaddr_in);
			cc = recvfrom(sock_raw, (char *)packet, PACKET_IN, 0, 
							&where, &length);
			(void)gettimeofday(&tv2, (struct timezone *)0);
			if (cc < 0)
				return(-1);
			if((icp->icmp_type == ICMP_TSTAMPREPLY) && 
					(icp->icmp_id == id)) {
				trials = 0;
				recvtime = (tv2.tv_sec % (24*60*60)) * 1000 + tv2.tv_usec / 1000;
				diff = recvtime - sendtime;
				/*
				 * diff can be less than 0 aroud midnight
				 */
				if ((diff > RANGE && ckrange) || diff < 0) {
					continue;
				}
				histime = ntohl((u_long)icp->icmp_rtime);
				if ((histime >> 32) == -1) {
					status = NONSTDTIME;
					break;
				}
				delta1 = histime - sendtime;
				/*
				 * Handles wrap-around to avoid that around 
				 * midnight small time differences appear 
				 * enormous. However, the two machine's clocks
				 * must be within 12 hours from each other.
				 */
				if (delta1 < BIASN)
					delta1 += MODULO;
				else if (delta1 > BIASP)
					delta1 -= MODULO;
				delta2 = recvtime - histime;
				if (delta2 < BIASN)
					delta2 += MODULO;
				else if (delta2 > BIASP)
					delta2 -= MODULO;
				if (delta1 < min1)  
					min1 = delta1;
				if (delta2 < min2)
					min2 = delta2;

				msgcount++;
			} else {
				/* empties the icmp input queue */
				tout.tv_sec = tout.tv_usec = 0;
				ready = 1<<sock_raw;
				found = select(20, &ready, (int *)0, 
							(int *)0, &tout);
				if (found) {
					length = sizeof(struct sockaddr_in);
					cc = recvfrom(sock_raw, (char *)packet,
						 PACKET_IN, 0, &where, &length);
					if (cc < 0)
						return(-1);
				}
				continue;
			}
		} else {
			if (++trials < TRIALS) {
				continue;
			} else {
				status = HOSTDOWN;
				break;
			}
		}
	}					/* end of while */

	/*
	 * If no answer is received for TRIALS consecutive times, 
	 * the machine is assumed to be down
	 */
	 if (status == GOOD) {
		measure_delta = (min1 - min2)/2 + PROCESSING_TIME;
	}
	return(status);
}
