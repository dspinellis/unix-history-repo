/*
* This include file dkerr.h defines the NAK reason codes sent to the host 
* from a DATAKIT VCS node                                                 
*
*                 @(#)dkerr.h	2.2 DKHOST 85/01/08
*/

#define BUSY	1
#define NOCMC	2
#define NODEST	3
#define INTERT	4
#define REORT	5
#define EXIST	6
#define NOMSG	6
#define NOACC	7		/* Access denied */
#define DIRASS	8               /* Directory Assistance */

/* The next set of codes are NAK reasons sent
* from the auto-dialer process
*/

#define NEEDNUM	9               /* Need a valid number */
#define ADFAIL	10              /* Autodialer died */
#define WFAIL	11              /* Autodialer failed while dialing */
#define NOIDIAL	12              /* No initial dialtone */
#define NOSDIAL	13              /* No secondary dialtone */
#define LINBUSY	14              /* Dialed number is busy */
#define NOANS	15              /* RING...RING...RING...RING... */
#define NOCAR	16              /* RING...Hello??? No carrier! */
#define NOREAS	17              /* It failed. Why?  No clues! */

/* The next set of codes are NAK reasons sent
* from the DATAKIT host process
*/

#define NLINE	130
#define	NSRVTAB	133
#define	NCHROOT	134
