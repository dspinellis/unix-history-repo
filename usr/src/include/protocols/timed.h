/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/*	@(#)timed.h	1.1	(Berkeley)	%G%	*/

#define NHOSTS		30	/* max number of hosts controlled by timed */

struct host {
	char *name;
	char *addr;
	int length;
	long delta;
	u_short seq;
};

/*
 * Time Synchronization Protocol
 */

#define	TSPVERSION	1
#define ANYADDR 	NULL

struct tsp {
	u_char	tsp_type;
	u_char	tsp_vers;
	short	tsp_seq;
	struct timeval tsp_time;
	char tsp_name[32];
};
 
/*
 * Command types.
 */
#define	TSP_ANY			0	/* match any types */
#define	TSP_ADJTIME		1	/* send adjtime */
#define	TSP_ACK			2	/* generic acknowledgement */
#define	TSP_MASTERREQ		3	/* ask for master's name */ 
#define TSP_MASTERACK		4	/* acknowledge master request */
#define	TSP_SETTIME		5	/* send network time */
#define TSP_MASTERUP		6	/* inform slaves that master is up */
#define	TSP_SLAVEUP		7	/* slave is up but not polled */
#define	TSP_ELECTION		8	/* advance candidature for master */
#define	TSP_ACCEPT		9	/* support candidature of master */
#define	TSP_REFUSE		10	/* reject candidature of master */
#define TSP_CONFLICT		11	/* two or more masters present */
#define	TSP_RESOLVE		12	/* masters' conflict resolution */
#define	TSP_QUIT		13	/* reject candidature if master is up */
#define	TSP_DATE		14	/* reset the time (date command) */
#define TSP_DATEREQ		15	/* remote request to reset the time */
#define TSP_DATEACK		16	/* acknowledge time setting  */
#define	TSP_TRACEON		17	/* turn tracing on */
#define	TSP_TRACEOFF		18	/* turn tracing off */
#define TSP_MSITE		19	/* find out master's site */
#define TSP_MSITEREQ		20	/* remote master's site request */
#define	TSP_TEST		21	/* for testing election algo */

#define	TSPTYPENUMBER		22

#ifdef TSPTYPES
char *tsptype[TSPTYPENUMBER] =
  { "#0", "ADJTIME", "ACK", "MASTERREQ", "MASTERACK", "SETTIME", "MASTERUP", 
  "SLAVEUP", "ELECTION", "ACCEPT", "REFUSE", "CONFLICT", "RESOLVE", "QUIT", 
  "DATE", "DATEREQ", "DATEACK", "TRACEON", "TRACEOFF", "MSITE", 
  "MSITEREQ", "TEST" };
#endif
