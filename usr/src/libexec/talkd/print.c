#ifndef lint
static char sccsid[] = "@(#)print.c	1.2 (Berkeley) %G%";
#endif

/* debug print routines */

#include <stdio.h>
#include "ctl.h"

print_request(request)
	CTL_MSG *request;
{
	
	printf("type is %d, l_user %s, r_user %s, r_tty %s\n",
		request->type, request->l_name, request->r_name,
		request->r_tty);
	printf("		id = %d\n", request->id_num);
	fflush(stdout);
}

print_response(response)
	CTL_RESPONSE *response;
{
	printf("type is %d, answer is %d, id = %d\n\n", response->type, 
		response->answer, response->id_num);
	fflush(stdout);
}
