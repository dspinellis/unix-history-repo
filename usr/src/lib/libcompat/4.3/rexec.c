/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)rexec.c 4.2 %G%";

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <wellknown.h>

char	*index(), *malloc(), *getpass(), *getlogin();
static	struct sockaddr_in sin = { AF_INET };

rexec(ahost, port, cmd, name, pass)
	char **ahost, *cmd, *name, *pass;
{
	int rem, addr;

	addr = rhost(ahost);
	if (addr == -1) {
		printf("%s: unknown host\n", *ahost);
		return (-1);
	}
	sin.sin_port = port;
	sin.sin_addr.s_addr = addr;
	ruserpass(*ahost, &name, &pass);
	rem = socket(SOCK_STREAM, 0, 0, SO_DEBUG);
	if (rem < 0) {
		perror("socket");
		return (-1);
	}
	if (connect(rem, &sin) < 0) {
		perror(*ahost);
		close(rem);
		return (-1);
	}
	write(rem, name, strlen(name) + 1);
	/* should public key encypt the password here */
	write(rem, pass, strlen(pass) + 1);
	if (cmd)
		write(rem, cmd, strlen(cmd) + 1);
	return (rem);
}
