#ifndef lint
static char *rcsid_xinit_c = "$Header: xinit.c,v 10.3 86/02/01 16:24:54 tony Rel $";
#endif	lint
#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1986	*/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#define default_server "X"
#define default_display "0"
char *default_client[] = {"xterm", "=+1+1", "-n", "login",
#ifdef sun
			  "-C",
#endif
			  "unix:0", NULL};
char *server[100];
char *client[100];

main(argc, argv)
	int argc;
	register char **argv;
{
	register char **sptr = server;
	register char **cptr = client;
	register char **ptr;
	int serverpid, clientpid, pid;

	argv++;
	argc--;
	if (argc == 0 || (**argv != '/' && !isalpha(**argv)))
	    for (ptr = default_client; *ptr; )
		*cptr++ = *ptr++;
	while (argc && strcmp(*argv, "--")) {
	    *cptr++ = *argv++;
	    argc--;
	}
	*cptr = NULL;
	if (argc) {
	    argv++;
	    argc--;
	}
	if (argc == 0 || (**argv != '/' && !isalpha(**argv))) {
	    *sptr++ = default_server;
	    if (argc == 0 || !isdigit(**argv))
		*sptr++ = default_display;
	}
	while (--argc >= 0)
	    *sptr++ = *argv++;
	*sptr++ = "0";
	*sptr = NULL;
	if ((serverpid = fork()) == 0) {
	    close(0);
	    close(1);
	    execvp(server[0], server);
	    perror(server[0]);
	    exit(1);
	}
	if (serverpid < 0) {
	    perror(server[0]);
	    exit(1);
	}
	sleep(5);
	if ((clientpid = fork()) == 0) {
	    execvp(client[0], client);
	    perror(client[0]);
	    exit(1);
	}
	if (clientpid < 0) {
	    kill(serverpid, SIGKILL);
	    perror(client[0]);
	    exit(1);
	}
	while (((pid = wait(NULL)) != clientpid) && (pid != serverpid)) ;
	kill(clientpid, SIGKILL);
	if (!kill(serverpid, SIGHUP)) {
	    sleep(1);
	    kill(serverpid, SIGKILL);
	}
}
