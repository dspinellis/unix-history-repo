/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/* debug print routines */

#include <stdio.h>
#include "ctl.h"

print_request(request)
	CTL_MSG *request;
{
    	extern FILE *debugout;
	
	fprintf(debugout
		, "type is %d, l_user %s, r_user %s, r_tty %s\n"
		, request->type, request->l_name, request->r_name
		, request->r_tty);
	fprintf(debugout, "		id = %d\n", request->id_num);
	fflush(debugout);
}

print_response(response)
	CTL_RESPONSE *response;
{
    	extern FILE *debugout;
	
	printf(debugout
		, "type is %d, answer is %d, id = %d\n\n", response->type
		, response->answer, response->id_num);
	fflush(debugout);
}
