/*
** Return codes from get_tcp_conn().
*/
#define FAIL		(-1)		/* routine failed */
#define	NOHOST		(FAIL-1)	/* no such host */
#define	NOSERVICE	(FAIL-2)	/* no such service */

#ifndef NULL
#define	NULL	0
#endif
