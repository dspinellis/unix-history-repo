
/*
 *temporary file implementing a setsockopt call
 * until I find out how Maryland intends to do it.
 */

#include <sys/types.h>
#include <stdio.h>
#include <sys/socket.h>

SetSPPoptions(s,stream,eom,attn)
	int s;		/* SPP socket */
	u_char stream;	/* datastream type */
	char eom;	/* Boolean EOM */
				/* can't set ATTN -- use MSG_OOB instead */
{
	/*
	setsockopt(s, SOL_PROTO, SPPOPT_DATASTREAMTYPE, &stream,
		   sizeof(stream));
	setsockopt(s, SOL_PROTO, SPPOPT_EOMBIT, &eom, sizeof(eom));
	*/
	fprintf(stderr, "SetSPPoptions called , now obsoleted\n");
	abort();
}
