char sccs_id[] = "@(#) poke_ns.c 1.4 92/08/31 @(#)";

/*
 * simple front-end for sending signals to the bind process
 * run setuid to root with appropriate group permission
 * for your installation
 */

/* Copyright (c) 1992 by Texas Internet Consulting
 * This code may be freely copied and used so long as this
 * copyright notice is attached.  This code may not be sold
 * without the express written permission of Texas Internet Consulting.
 * Texas Internet Consulting makes no warranty as to the correctness
 * nor the applicability of this code for any purpose.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#define PIDFILE "/etc/named.pid"
#define NAMED "/usr/etc/in.named"
#define DB_DUMP "/usr/tmp/named_dump.db"
#define DEBUG "/usr/tmp/named.run"

struct {
	char *name;	/* command name */
	int sig;	/* signal to send to bind */
} commands[] = {
	"restart",	SIGTERM,
	"reload",	SIGHUP,
	"debug",	SIGUSR1,
	"nodebug",	SIGUSR2,
	"dump",		SIGINT,
	"terminate",	SIGTERM,
	NULL,		0
};

main(argc, argv)
int argc;
char **argv;
{
	FILE *fd;
	int cmd, pid;
	struct stat status;
	void usage(), execute();
	int lookup();
	int uid, gid;

	if (argc != 2) {
		usage();
		exit(1);
	}

	/* match one of the commands */
	if ((cmd = lookup(argv[1])) == -1) {
		fprintf(stderr, "command %s not found\n", argv[1]);
		exit(1);
	}


	/* check permissions on /etc/named.pid */
	if (stat(PIDFILE, &status) == -1) {
		fprintf(stderr, "%s cannot stat\n", PIDFILE);
		exit(2);
	}
	if (status.st_uid != 0) {
		fprintf(stderr, "%s not owned by root\n", PIDFILE);
		exit(2);
	}
	if (status.st_nlink > 1) {
		fprintf(stderr, "%s has more than one link\n", PIDFILE);
		exit(2);
	}
	if (status.st_mode&(S_IWGRP|S_IWOTH)) {
		fprintf(stderr, "%s can be written by others\n", PIDFILE);
		exit(2);
	}

	/* if it is safe - then read pid */
	if ((fd = fopen(PIDFILE, "r")) == NULL) {
		fprintf(stderr, "%s cannot be read\n", PIDFILE);
		exit(3);
	}
	if (fscanf(fd, "%d", &pid) != 1) {
		fprintf(stderr, "%s does not contain an integer\n", PIDFILE);
		exit(3);
	}

	/* execute appropriate command */
	execute(cmd, pid);

	/* change ownership to real user of debugging files */
	uid = getuid();
	gid = getgid();
	chown(DB_DUMP, uid, gid);
	chown(DEBUG, uid, gid);
	exit(0);
}

void
usage()
{
	int i;

	fprintf(stderr, "usage: poke_ns ");
	for (i=0; commands[i].name != NULL; i++) {
		fprintf(stderr, commands[i].name);
		if (commands[i+1].name != NULL) {
			fprintf(stderr, " | ");
		}
		else {
			fprintf(stderr, "\n");
		}
	}
}
	
int
lookup(cmd)
char *cmd;
{
	int i;

	for (i=0; commands[i].name != NULL; i++) {
		if (strcmp(commands[i].name, cmd) == 0) {
			return i;
		}
	}
	return -1;
}

void
execute(cmd, pid)
int cmd;
int pid;
{
	int newpid, wstat;

	/* some sanity checking on pid */
	if (pid <= 2) {
		fprintf(stderr, "pid (%d) must be greater than 2\n", pid);
		exit(4);
	}

	/* send the signal to the process */
	if (kill(pid, commands[cmd].sig) == -1) {
		/* let restart work if no named process is running */
		if (!(cmd == 0 && errno == ESRCH)) {
			fprintf(stderr, "signal failed\n");
			exit(4);
		}
	}

	if (cmd != 0) {
		return;
	}
	/* special case of "restart" */

	/* wait and be sure process is dead */
	while (kill(pid, 0) != -1) {
		sleep(1);
	}
	/* restart named */
	newpid = fork();
	if (newpid == -1) {
		fprintf(stderr, "fork failed\n");
		exit(5);
	}
	if (newpid == 0) { /* child */
		execl(NAMED, "in.named", NULL);
		/* only if execl failed */
		fprintf(stderr, "execl failed\n");
		exit(5);
	}
	/* parent */
	wait (&wstat);
	if (WEXITSTATUS(wstat) != 0) {
		exit(5);
	}
	return;
}
