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

/*
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

/*
 * rpcinfo: ping a particular rpc program
 *     or dump the portmapper
 */

#include <rpc/rpc.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_clnt.h>

#define MAXHOSTLEN 256

main(argc, argv)
	char **argv;
{
	int ans;
	
	if (argc < 2) {
		usage();
		exit(1);
	}
	if (argv[1][0] == '-') {
		switch(argv[1][1]) {
			case 't':
				tcpping(argc-1, argv+1);
				break;
			case 'p':
				pmapdump(argc-1, argv+1);
				break;
			case 'u':
				udpping(argc-1, argv+1);
				break;
			default:
				usage();
				exit(1);
				break;
		}
	}
	else
		usage();
}
		
udpping(argc, argv)
	char **argv;
{
	int ans;
    
	if (argc != 4) {
		usage();
		exit(1);
	}
	ans = callrpc(argv[1], atoi(argv[2]), atoi(argv[3]), NULLPROC,
	    xdr_void, 0, xdr_void, 0);
	if (ans != 0) {
		clnt_perrno(ans);
		fprintf(stderr, "\n");
		printf("proc %d vers %d is not available\n", atoi(argv[2]),
		    atoi(argv[3]));
	}
	else
		printf("proc %d vers %d ready and waiting\n", atoi(argv[2]),
		    atoi(argv[3]));
}

tcpping(argc, argv)
	int argc;
	char **argv;
{
	struct timeval to;
	struct sockaddr_in addr;
	enum clnt_stat rpc_stat;
	CLIENT *client;
	int sock = -1;
	struct hostent *hp;

	if (argc != 4) {
		usage();
		exit(1);
	}
	if ((hp = gethostbyname(argv[1])) == NULL) {
	    fprintf(stderr, "can't find %s\n", argv[1]);
	    exit(1);
	}
	addr.sin_family = AF_INET;
	addr.sin_port = 0;
	addr.sin_addr.s_addr = *(int *)hp->h_addr;
	if ((client = clnttcp_create(&addr, atoi(argv[2]),
		atoi(argv[3]), &sock, 0, 0)) == NULL) {
			clnt_pcreateerror("");
			printf("proc %d vers %d is not available\n",
			    atoi(argv[2]), atoi(argv[3]));
			exit(1);
		}
	to.tv_usec = 0;
	to.tv_sec = 10;
	rpc_stat = clnt_call(client, 0, xdr_void, NULL, xdr_void, NULL, to);
	if (rpc_stat != RPC_SUCCESS) {
		clnt_perrno(rpc_stat);
		fprintf(stderr, "\n");
		printf("proc %d vers %d is not available\n", atoi(argv[2]),
		    atoi(argv[3]));
	}
	else
		printf("proc %d vers %d ready and waiting\n", atoi(argv[2]),
		    atoi(argv[3]));
}

pmapdump(argc, argv)
	int argc;
	char **argv;
{
	struct sockaddr_in server_addr;
	struct hostent *hp;
	struct pmaplist *head;
	char hoststr[MAXHOSTLEN];
	int socket = -1;
	struct timeval minutetimeout;
	char *hostnm;
	register CLIENT *client;
	enum clnt_stat rpc_stat;
	
	if (argc > 2) {
		usage();
		exit(1);
	}
	if (argc == 2) {
		hostnm = argv[1];
	} else {
		gethostname(hoststr, sizeof(hoststr));
		hostnm = hoststr;
	}
	if ((hp = gethostbyname(hostnm)) == NULL) {
		fprintf(stderr, "cannot get addr for '%s'\n", hostnm);
		exit(0);
	}
	bcopy(hp->h_addr, (caddr_t)&server_addr.sin_addr, hp->h_length);
	server_addr.sin_family = AF_INET;
	minutetimeout.tv_sec = 60;
	minutetimeout.tv_usec = 0;
	server_addr.sin_port = htons(PMAPPORT);
	if ((client = clnttcp_create(&server_addr, PMAPPROG,
	    PMAPVERS, &socket, 50, 500)) == NULL) {
		clnt_pcreateerror("rpcinfo: can't contact portmapper");
		exit(1);
	}
	if (clnt_call(client, PMAPPROC_DUMP, xdr_void, NULL,
	    xdr_pmaplist, &head, minutetimeout) != RPC_SUCCESS) {
		fprintf(stderr, "rpcinfo: can't contact portmapper");
		clnt_perrno(rpc_stat);
		fprintf(stderr, "\n");
		exit(1);
	}
	if (head == NULL) {
		printf("No remote programs registered.\n");
	} else {
		printf("[program, version, protocol, port]:\n\n");
		for (; head != (struct pmaplist *)NULL; head = head->pml_next) {
			printf("[%ld, %ld, %ld, %ld]\n",
			    head->pml_map.pm_prog,
			    head->pml_map.pm_vers,
			    head->pml_map.pm_prot,
			    head->pml_map.pm_port);
		}
	}
}

usage()
{
	fprintf(stderr, "Usage: rpcinfo -u host prognum versnum\n");
	fprintf(stderr, "       rpcinfo -t host prognum versnum\n");
	fprintf(stderr, "       rpcinfo -p [host]\n");
}
