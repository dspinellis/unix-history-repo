/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#ifndef lint
static char sccsid[] = "@(#)clnt_perror.c 1.1 85/02/08 Copyr 1984 Sun Micro";
#endif

/*
 * clnt_perror.c
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 */
#include "types.h"
#include "xdr.h"
#include "auth.h"
#include "clnt.h"
#include "rpc_msg.h"
#include <stdio.h>
extern char *sys_errlist[];

/*
 * Print reply error info
 */
void
clnt_perror(rpch, s)
	CLIENT *rpch;
	char *s;
{
	struct rpc_err e;
	void clnt_perrno();

	CLNT_GETERR(rpch, &e);
	fprintf(stderr, "%s: ", s);
	switch (e.re_status) {
		case RPC_SUCCESS:
		case RPC_CANTENCODEARGS:
		case RPC_CANTDECODERES:
		case RPC_TIMEDOUT:
		case RPC_PROGUNAVAIL:
		case RPC_PROCUNAVAIL:
		case RPC_CANTDECODEARGS:
			clnt_perrno(e.re_status);
			break;
		case RPC_CANTSEND:
			clnt_perrno(e.re_status);
			fprintf(stderr, "; errno = %s",
			    sys_errlist[e.re_errno]);
			break;
	
		case RPC_CANTRECV:
			clnt_perrno(e.re_status);
			fprintf(stderr, "; errno = %s",
			    sys_errlist[e.re_errno]);
			break;
	
		case RPC_VERSMISMATCH:
			clnt_perrno(e.re_status);
			fprintf(stderr, "; low version = %lu, high version = %lu", e.re_vers.low, e.re_vers.high);
			break;
	
		case RPC_AUTHERROR:
			clnt_perrno(e.re_status);
			fprintf(stderr, "; why = ");
			switch (e.re_why) {
			case AUTH_OK:
				fprintf(stderr, "AUTH_OK");
				break;
	
			case AUTH_BADCRED:
				fprintf(stderr, "AUTH_BOGUS_CREDENTIAL");
				break;
	
			case AUTH_REJECTEDCRED:
				fprintf(stderr, "AUTH_REJECTED_CREDENTIAL");
				break;
	
			case AUTH_BADVERF:
				fprintf(stderr, "AUTH_BOGUS_VERIFIER");
				break;
	
			case AUTH_REJECTEDVERF:
				fprintf(stderr, "AUTH_REJECTED_VERIFIER");
				break;
	
			case AUTH_TOOWEAK:
				fprintf(stderr, "AUTH_TOO_WEAK (remote error)");
				break;
	
			case AUTH_INVALIDRESP:
				fprintf(stderr, "AUTH_INVALID_RESPONSE");
				break;
	
			default:
				fprintf(stderr, "AUTH_UNKNOWN_FAILURE");
				break;
			}
			break;
	
		case RPC_PROGVERSMISMATCH:
			clnt_perrno(e.re_status);
			fprintf(stderr, "; low version = %lu, high version = %lu", e.re_vers.low, e.re_vers.high);
			break;
	
		default:
			fprintf(stderr, "RPC_UNKNOWN_FAILURE; s1 = %lu, s2 = %lu", e.re_lb.s1, e.re_lb.s2);
			break;
	}
	fprintf(stderr, "\n");
}

/*
 * This interface for use by clntrpc
 */
void
clnt_perrno(num)
	enum clnt_stat num;
{
	switch (num) {
		case RPC_SUCCESS:
			fprintf(stderr, "RPC_SUCCESS");
			break;
	
		case RPC_CANTENCODEARGS:
			fprintf(stderr, "RPC_CANT_ENCODE_ARGS");
			break;
	
		case RPC_CANTDECODERES:
			fprintf(stderr, "RPC_CANT_DECODE_RESULTS");
			break;
	
		case RPC_CANTSEND:
			fprintf(stderr, "RPC_CANT_SEND");
			break;
	
		case RPC_CANTRECV:
			fprintf(stderr, "RPC_CANT_RECV");
			break;
	
		case RPC_TIMEDOUT:
			fprintf(stderr, "RPC_TIMED_OUT");
			break;
	
		case RPC_VERSMISMATCH:
			fprintf(stderr, "RPC_VERSION_MISMATCH");
			break;
	
		case RPC_AUTHERROR:
			fprintf(stderr, "RPC_AUTH_ERROR");
			break;
	
		case RPC_PROGUNAVAIL:
			fprintf(stderr, "RPC_REMOTE_PROGRAM_UNAVAILABLE");
			break;
	
		case RPC_PROGVERSMISMATCH:
			fprintf(stderr, "RPC_PROGRAM_MISMATCH");
			break;
	
		case RPC_PROCUNAVAIL:
			fprintf(stderr, "RPC_UNKNOWN_PROCEDURE");
			break;
	
		case RPC_CANTDECODEARGS:
			fprintf(stderr, "RPC_CANT_DECODE_ARGS");
			break;
		case RPC_UNKNOWNHOST:
			fprintf(stderr, "RPC_UNKNOWNHOST");
			break;
		case RPC_PMAPFAILURE:
			fprintf(stderr, "RPC_PMAP_FAILURE");
			break;
		case RPC_PROGNOTREGISTERED:
			fprintf(stderr, "RPC_PROG_NOT_REGISTERED");
			break;
		case RPC_SYSTEMERROR:
			fprintf(stderr, "RPC_SYSTEM_ERROR");
			break;
	}
}

/*
 * A handle on why an rpc creation routine failed (returned NULL.)
 */
struct rpc_createerr rpc_createerr;

clnt_pcreateerror(s)
	char *s;
{

	fprintf(stderr, "%s: ", s);
	clnt_perrno(rpc_createerr.cf_stat);
	switch (rpc_createerr.cf_stat) {
		case RPC_PMAPFAILURE:
			fprintf(stderr, " - ");
			clnt_perrno(rpc_createerr.cf_error.re_status);
			break;

		case RPC_SYSTEMERROR:
			fprintf(stderr, " - %s", sys_errlist[rpc_createerr.cf_error.re_errno]);
			break;

	}
	fprintf(stderr, "\n");
}
