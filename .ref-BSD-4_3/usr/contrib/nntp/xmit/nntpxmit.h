#define	dprintf	if (Debug) fprintf

#define	TIMEOUT	3600	/* seconds to read timeout in sfgets */

/* in goodbye() wait (or not) for QUIT response */
#define	WAIT		1
#define	DONT_WAIT	0
#ifndef FAIL
#define	FAIL		(-1)
#endif
