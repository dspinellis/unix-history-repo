
/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)kpasswd.c	1.2 (Berkeley) 5/17/89";
#endif /* not lint */

/*
 * kpasswd - client program to update Kerberos password
 *
 * K. Fall
 * 12-Dec-88
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <netinet/in.h>
#include <netdb.h>
#include <kerberos/krb.h>
#include "kpasswd_proto.h"

KTEXT_ST	ticket;
long		authopts = 0L;
Key_schedule	random_schedule;
char		realm[REALM_SZ], krbhst[MAX_HSTNM];
static		struct	kpasswd_data	proto_data;
static		C_Block		okey;
static		Key_schedule	osched;
static struct timeval	timeout = { CLIENT_KRB_TIMEOUT, 0 };
int		sock;
char		*getpass();
int		sock;

int		finish();

#define		PROTO	"tcp"

main(argc, argv)
int	argc;
char	**argv;
{
	struct servent	*se;
	struct hostent	*host;
	struct sockaddr_in	sin;
	int		rval;
	char		password[255], *pass;
	fd_set		readfds;

	static struct	rlimit rl = { 0, 0 };

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGTSTP, SIG_IGN);

	if(setrlimit(RLIMIT_CORE, &rl) < 0) {
		perror("setrlimit");
		exit(1);
	}

	if((se = getservbyname(SERVICE, PROTO)) == NULL) {
		fprintf(stderr, "couldn't find entry for service %s/%s\n",
			SERVICE, PROTO);
		exit(1);
	}
	if((rval = krb_get_lrealm(realm,1)) != KSUCCESS) {
		fprintf(stderr, "couldn't get local Kerberos realm: %s\n",
			krb_err_txt[rval]);
		exit(1);
	}

	if((rval = krb_get_krbhst(krbhst, realm, 1)) != KSUCCESS) {
		fprintf(stderr, "couldn't get Kerberos host: %s\n",
			krb_err_txt[rval]);
		exit(1);
	}

	if((host = gethostbyname(krbhst)) == NULL) {
		fprintf(stderr, "couldn't get host entry for host %s\n",
			krbhst);
		exit(1);
	}

	sin.sin_family = host->h_addrtype;
	bcopy(host->h_addr, (char *) &sin.sin_addr, host->h_length);
	sin.sin_port = se->s_port;

	if((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
		perror("socket");
		exit(1);
	}

	if(connect(sock, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("connect");
		close(sock);
		exit(1);
	}

	rval = krb_sendauth(
		authopts,		/* NOT mutual */
		sock,
		&ticket,		/* (filled in) */
		SERVICE,
		krbhst,			/* instance (krbhst) */
		realm,			/* dest realm */
		(u_long) getpid(),	/* checksum */
		NULL,			/* msg data */
		NULL,			/* credentials */ 
		NULL,			/* schedule */
		NULL,			/* local addr */
		NULL,			/* foreign addr */
		"KPWDV0.1"
	);

	if(rval != KSUCCESS) {
		fprintf(stderr, "Kerberos sendauth error: %s\n",
			krb_err_txt[rval]);
		exit(1);
	}

	pass = getpass("Old Kerberos password:");
	string_to_key(pass, okey);
	key_sched(okey, osched);
	des_set_key(okey, osched);

	/* wait on the verification string */

	FD_ZERO(&readfds);
	FD_SET(sock, &readfds);

	rval =
	  select(sock + 1, &readfds, (fd_set *) 0, (fd_set *) 0, &timeout);
	if((rval < 1) || !FD_ISSET(sock, &readfds)) {
		if(rval == 0) {
			fprintf(stderr, "Timed out\n");
			cleanup();
			exit(1);
		}
		fprintf(stderr, "select failed\n");
		cleanup();
		exit(1);
	}

	/* read verification string */

	if(des_read(sock, &proto_data, sizeof(proto_data)) != sizeof(proto_data)) {
		fprintf(stderr,
		    "%s: couldn't read verification string (aborted)\n",
		    argv[0]
		);

		cleanup();
		exit(1);
	}

	signal(SIGHUP, finish);
	signal(SIGINT, finish);

	if(strcmp(SECURE_STRING, proto_data.secure_msg)) {
		cleanup();
		fprintf(stderr, "Sorry.\n");
		exit(1);
	}
	key_sched(proto_data.random_key, random_schedule);
	des_set_key(proto_data.random_key, random_schedule);
	pass = getpass("New Kerberos password:");
	strcpy(password, pass);
	pass = getpass("Retype new Kerberos password:");
	if(strcmp(password, pass)) {
		fprintf(stderr, "Password mismatch (aborted)\n");
		cleanup();
		exit(1);
	}
	send_update(sock, password, SECURE_STRING);

	/* wait for ACK */

	FD_ZERO(&readfds);
	FD_SET(sock, &readfds);

	rval =
	  select(sock + 1, &readfds, (fd_set *) 0, (fd_set *) 0, &timeout);
	if((rval < 1) || !FD_ISSET(sock, &readfds)) {
		if(rval == 0) {
			fprintf(stderr, "Timed out reading ACK\n");
			cleanup();
			exit(1);
		}
		fprintf(stderr, "select failed\n");
		cleanup();
		exit(1);
	}

	recv_ack(sock);
	cleanup();
	exit(0);
}

send_update(dest, pwd, str)
	int	dest;
	char	*pwd, *str;
{
	static struct	update_data	ud;
	strncpy(ud.secure_msg, str, MSGSIZE);
	strncpy(ud.pw, pwd, sizeof(ud.pw));
	if(des_write(dest, &ud, sizeof(ud)) != sizeof(ud)) {
		fprintf(stderr, "couldn't write pw update (abort)\n");
		bzero(ud, sizeof(ud));
		cleanup();
		exit(1);
	}
}

recv_ack(remote)
	int	remote;
{
	int	cc;
	char	buf[BUFSIZ];
	cc = des_read(remote, buf, sizeof(buf));
	if(cc <= 0) {
		fprintf(stderr, "error reading acknowledgement\n");
		cleanup();
		exit(1);
	}
	printf("%s", buf);
}

cleanup()
{
	bzero(&proto_data, sizeof(proto_data));
	bzero(okey, sizeof(okey));
	bzero(osched, sizeof(osched));
	bzero(random_schedule, sizeof(random_schedule));
}

finish()
{
	close(sock);
	exit(1);
}
