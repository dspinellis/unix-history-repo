#ifndef lint
static char sccsid[] = "@(#)cmds.c	4.3 (Berkeley) %G%";
#endif

/*
 * FTP User Program -- Command Routines.
 */
#include <sys/param.h>
#include <sys/socket.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <netdb.h>
#include <stat.h>

#include "ftp.h"
#include "ftp_var.h"

extern	char *globerr;
extern	char **glob();
extern	short gflag;

/*
 * Connect to peer server and
 * auto-login, if possible.
 */
setpeer(argc, argv)
	int argc;
	char *argv[];
{
	struct hostent *host, *hookup();
	int port;

	if (connected) {
		printf("Already connected to %s, use disconnect first.\n",
			hostname);
		return;
	}
	if (argc < 2) {
		strcat(line, " ");
		printf("(to) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc > 3) {
		printf("usage: %s host-name [port]\n", argv[0]);
		return;
	}
	port = sp->s_port;
	if (argc > 2) {
		port = atoi(argv[2]);
		if (port <= 0) {
			printf("%s: bad port number-- %s\n", argv[1], argv[2]);
			printf ("usage: %s host-name [port]\n", argv[0]);
			return;
		}
		port = htons(port);
	}
	host = hookup(argv[1], port);
	if (host) {
		connected = 1;
		if (autologin)
			login(host);
	}
}

struct	types {
	char	*t_name;
	char	*t_mode;
	int	t_type;
	char	*t_arg;
} types[] = {
	{ "ascii",	"A",	TYPE_A,	0 },
	{ "binary",	"I",	TYPE_I,	0 },
	{ "image",	"I",	TYPE_I,	0 },
	{ "ebcdic",	"E",	TYPE_E,	0 },
	{ "tenex",	"L",	TYPE_L,	bytename },
	0
};

/*
 * Set transfer type.
 */
settype(argc, argv)
	char *argv[];
{
	register struct types *p;
	int comret;

	if (argc > 2) {
		char *sep;

		printf("usage: %s [", argv[0]);
		sep = " ";
		for (p = types; p->t_name; p++) {
			printf("%s%s", sep, p->t_name);
			if (*sep == ' ')
				sep = " | ";
		}
		printf(" ]\n");
		return;
	}
	if (argc < 2) {
		printf("Using %s mode to transfer files.\n", typename);
		return;
	}
	for (p = types; p->t_name; p++)
		if (strcmp(argv[1], p->t_name) == 0)
			break;
	if (p->t_name == 0) {
		printf("%s: unknown mode\n", argv[1]);
		return;
	}
	if ((p->t_arg != NULL) && (*(p->t_arg) != '\0'))
		comret = command ("TYPE %s %s", p->t_mode, p->t_arg);
	else
		comret = command("TYPE %s", p->t_mode);
	if (comret == COMPLETE) {
		strcpy(typename, p->t_name);
		type = p->t_type;
	}
}

/*
 * Set binary transfer type.
 */
/*VARARGS*/
setbinary()
{

	call(settype, "type", "binary", 0);
}

/*
 * Set ascii transfer type.
 */
/*VARARGS*/
setascii()
{

	call(settype, "type", "ascii", 0);
}

/*
 * Set tenex transfer type.
 */
/*VARARGS*/
settenex()
{

	call(settype, "type", "tenex", 0);
}

/*
 * Set ebcdic transfer type.
 */
/*VARARGS*/
setebcdic()
{

	call(settype, "type", "ebcdic", 0);
}

/*
 * Set file transfer mode.
 */
setmode(argc, argv)
	char *argv[];
{

	printf("We only support %s mode, sorry.\n", modename);
}

/*
 * Set file transfer format.
 */
setform(argc, argv)
	char *argv[];
{

	printf("We only support %s format, sorry.\n", formname);
}

/*
 * Set file transfer structure.
 */
setstruct(argc, argv)
	char *argv[];
{

	printf("We only support %s structure, sorry.\n", structname);
}

/*
 * Send a single file.
 */
put(argc, argv)
	char *argv[];
{

	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	if (argc == 2)
		argc++, argv[2] = argv[1];
	if (argc < 2) {
		strcat(line, " ");
		printf("(local-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
usage:
		printf("%s local-file remote-file\n", argv[0]);
		return;
	}
	if (argc < 3) {
		strcat(line, " ");
		printf("(remote-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) 
		goto usage;
	if (!globulize(&argv[1]))
		return;
	sendrequest("STOR", argv[1], argv[2]);
}

/*
 * Send one or more files.
 */
mput(argc, argv)
	char *argv[];
{
	char **cpp, *dst;
	int i;

	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	if (argc < 2) {
		printf("%s local-file remote-file, or\n", argv[0]);
		printf("%s local-files\n", argv[0]);
		return;
	}
	if (argc == 3)  {
		if (!globulize(&argv[1]))
			return;
		sendrequest("STOR", argv[1], argv[2]);
		return;
	}
	/*
	 * Check for shell metacharacters which we might
	 * want to expand into a list of file names.
	 */
	for (i = 1; i < argc; i++) {
		if (!doglob) {
			if (!skip(argv[0], argv[i]))
				sendrequest("STOR", argv[i], argv[i]);
			continue;
		}
		cpp = glob(argv[i]);
		if (globerr != NULL) {
			printf("%s: %s\n", argv[i], globerr);
			if (cpp)
				blkfree(cpp);
			continue;
		}
		if (cpp == NULL) {
			printf("%s: no match\n", argv[i]);
			continue;
		}
		while (*cpp != NULL) {
			if (!skip(argv[0], *cpp))
				sendrequest("STOR", *cpp, *cpp);
			free(*cpp), cpp++;
		}
	}
}

/*
 * Receive one file.
 */
get(argc, argv)
	char *argv[];
{

	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	if (argc == 2)
		argc++, argv[2] = argv[1];
	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
usage:
		printf("%s remote-file [ local-file ]\n", argv[0]);
		return;
	}
	if (argc < 3) {
		strcat(line, " ");
		printf("(local-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) 
		goto usage;
	if (!globulize(&argv[2]))
		return;
	recvrequest("RETR", argv[2], argv[1]);
}

/*
 * Get multiple files.
 */
mget(argc, argv)
	char *argv[];
{
	char temp[16], *dst, dstpath[MAXPATHLEN], buf[MAXPATHLEN];
	FILE *ftemp;
	int madedir = 0, oldverbose;
	struct stat stb;

	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	if (argc == 2)
		argc++, argv[2] = argv[1];
	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-directory) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
usage:
		printf("%s remote-directory [ local-directory ], or\n",
			argv[0]);
		printf("%s remote-files local-directory\n", argv[0]);
		return;
	}
	if (argc < 3) {
		strcat(line, " ");
		printf("(local-directory) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) 
		goto usage;
	dst = argv[argc - 1];
	if (!globulize(&dst))
		return;
	/*
	 * If destination doesn't exist,
	 * try and create it.
	 */
	if (stat(dst, &stb) < 0) {
		if (mkdir(dst, 0777) < 0) {
			perror(dst);
			return;
		}
		madedir++;
	} else {
		if ((stb.st_mode & S_IFMT) != S_IFDIR) {
			printf("%s: not a directory\n", dst);
			return;
		}
	}
	/*
	 * Multiple files, just get each one without an nlst.
	 */
	if (argc > 3) {
		int i;

		for (i = 1; i < argc - 1; i++)
			recvrequest("RETR",
			  sprintf(dstpath, "%s/%s", dst, argv[i]), argv[i]);
		return;
	}
	/*
	 * Get a directory full of files.  Perform an
	 * nlst to find the file names, then retrieve
	 * each individually.  If prompting is on, ask
	 * before grabbing each file.
	 */
	strcpy(temp, "/tmp/ftpXXXXXX");
	mktemp(temp);
	oldverbose = verbose, verbose = 0;
	recvrequest("NLST", temp, argv[1]);
	verbose = oldverbose;
	ftemp = fopen(temp, "r");
	unlink(temp);
	if (ftemp == NULL) {
		printf("can't find list of remote files, oops\n");
		if (madedir)
			(void) rmdir(dst);
		return;
	}
	while (fgets(buf, sizeof (buf), ftemp) != NULL) {
		char *cp = index(buf, '\n');

		if (cp)
			*cp = '\0';
		if (skip(argv[0], buf))
			continue;
		recvrequest("RETR", sprintf(dstpath, "%s/%s", dst, buf), buf);
	}
	fclose(ftemp);
}

char *
onoff(bool)
	int bool;
{

	return (bool ? "on" : "off");
}

/*
 * Show status.
 */
status(argc, argv)
	char *argv[];
{

	if (connected)
		printf("Connected to %s.\n", hostname);
	else
		printf("Not connected.\n");
	printf("Mode: %s; Type: %s; Form: %s; Structure: %s\n",
		modename, typename, formname, structname);
	printf("Verbose: %s; Bell: %s; Prompting: %s; Globbing: %s\n", 
		onoff(verbose), onoff(bell), onoff(interactive),
		onoff(doglob));
}

/*
 * Set beep on cmd completed mode.
 */
/*VARARGS*/
setbell()
{

	bell = !bell;
	printf("Bell mode %s.\n", onoff(bell));
}

/*
 * Turn on packet tracing.
 */
/*VARARGS*/
settrace()
{

	trace = !trace;
	printf("Packet tracing %s.\n", onoff(trace));
}

/*
 * Turn on printing of server echo's.
 */
/*VARARGS*/
setverbose()
{

	verbose = !verbose;
	printf("Verbose mode %s.\n", onoff(verbose));
}

/*
 * Turn on interactive prompting
 * during mget, mput, and mdelete.
 */
/*VARARGS*/
setprompt()
{

	interactive = !interactive;
	printf("Interactive mode %s.\n", onoff(interactive));
}

/*
 * Toggle metacharacter interpretation
 * on local file names.
 */
/*VARARGS*/
setglob()
{
	
	doglob = !doglob;
	printf("Globbing %s.\n", onoff(doglob));
}

/*
 * Set debugging mode on/off and/or
 * set level of debugging.
 */
setdebug(argc, argv)
	char *argv[];
{
	int val;

	if (argc > 1) {
		val = atoi(argv[1]);
		if (val < 0) {
			printf("%s: bad debugging value.\n", argv[1]);
			return;
		}
	} else
		val = !debug;
	debug = val;
	if (debug)
		options |= SO_DEBUG;
	else
		options &= ~SO_DEBUG;
	printf("Debugging %s (debug=%d).\n", onoff(debug), debug);
}

/*
 * Set current working directory
 * on remote machine.
 */
cd(argc, argv)
	char *argv[];
{

	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-directory) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s remote-directory\n", argv[0]);
		return;
	}
	(void) command("CWD %s", argv[1]);
}

/*
 * Set current working directory
 * on local machine.
 */
lcd(argc, argv)
	char *argv[];
{
	char buf[MAXPATHLEN];
	extern char *home;

	if (argc < 2)
		argc++, argv[1] = home;
	if (argc != 2) {
		printf("%s local-directory\n", argv[0]);
		return;
	}
	if (!globulize(&argv[1]))
		return;
	if (chdir(argv[1]) < 0) {
		perror(argv[1]);
		return;
	}
	printf("Local directory now %s\n", getwd(buf));
}

/*
 * Delete a single file.
 */
delete(argc, argv)
	char *argv[];
{

	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s remote-file\n", argv[0]);
		return;
	}
	(void) command("DELE %s", argv[1]);
}

/*
 * Rename a remote file.
 */
renamefile(argc, argv)
	char *argv[];
{

	if (argc < 2) {
		strcat(line, " ");
		printf("(from-name) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
usage:
		printf("%s from-name to-name\n", argv[0]);
		return;
	}
	if (argc < 3) {
		strcat(line, " ");
		printf("(to-name) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) 
		goto usage;
	if (command("RNFR %s", argv[1]) == CONTINUE)
		(void) command("RNTO %s", argv[2]);
}

/*
 * Get a directory listing
 * of remote files.
 */
ls(argc, argv)
	char *argv[];
{
	char *cmd;

	if (argc < 2)
		argc++, argv[1] = NULL;
	if (argc < 3)
		argc++, argv[2] = "-";
	if (argc > 3) {
		printf("usage: %s remote-directory local-file\n", argv[0]);
		return;
	}
	cmd = argv[0][0] == 'l' ? "NLST" : "LIST";
	if (strcmp(argv[2], "-") && !globulize(&argv[2]))
		return;
	recvrequest(cmd, argv[2], argv[1]);
}

/*
 * Do a shell escape
 */
shell(argc, argv)
	char *argv[];
{

	printf("Sorry, this function is unimplemented.\n");
}

/*
 * Send new user information (re-login)
 */
user(argc, argv)
	int argc;
	char **argv;
{
	char acct[80], *getpass();
	int n;

	if (argc < 2) {
		strcat(line, " ");
		printf("(username) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc > 4) {
		printf("usage: %s username [password] [account]\n", argv[0]);
		return;
	}
	n = command("USER %s", argv[1]);
	if (n == CONTINUE) {
		if (argc < 3 )
			argv[2] = getpass("Password: "), argc++;
		n = command("PASS %s", argv[2]);
	}
	if (n == CONTINUE) {
		if (argc < 4) {
			printf("Account: "); (void) fflush(stdout);
			(void) fgets(acct, sizeof(acct) - 1, stdin);
			acct[strlen(acct) - 1] = '\0';
			argv[3] = acct; argc++;
		}
		n = command("ACCT %s", acct);
	}
	if (n != COMPLETE) {
		fprintf(stderr, "Login failed.\n");
		return (0);
	}
	return (1);
}

/*
 * Print working directory.
 */
/*VARARGS*/
pwd()
{
	if (!connected) {
		printf("Not connected.\n");
		return;
	}
	(void) command("XPWD");
}

/*
 * Make a directory.
 */
makedir(argc, argv)
	char *argv[];
{

	if (argc < 2) {
		strcat(line, " ");
		printf("(directory-name) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s directory-name\n", argv[0]);
		return;
	}
	(void) command("XMKD %s", argv[1]);
}

/*
 * Remove a directory.
 */
removedir(argc, argv)
	char *argv[];
{

	if (argc < 2) {
		strcat(line, " ");
		printf("(directory-name) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s directory-name\n", argv[0]);
		return;
	}
	(void) command("XRMD %s", argv[1]);
}

/*
 * Send a line, verbatim, to the remote machine.
 */
quote(argc, argv)
	char *argv[];
{
	int i;
	char buf[BUFSIZ];

	if (argc < 2) {
		strcat(line, " ");
		printf("(command line to send) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("usage: %s line-to-send\n", argv[0]);
		return;
	}
	strcpy(buf, argv[1]);
	for (i = 2; i < argc; i++) {
		strcat(buf, " ");
		strcat(buf, argv[i]);
	}
	(void) command(buf);
}

/*
 * Ask the other side for help.
 */
rmthelp(argc, argv)
	char *argv[];
{
	int oldverbose = verbose;

	verbose = 1;
	(void) command(argc == 1 ? "HELP" : "HELP %s", argv[1]);
	verbose = oldverbose;
}

/*
 * Terminate session and exit.
 */
/*VARARGS*/
quit()
{

	disconnect();
	exit(0);
}

/*
 * Terminate session, but don't exit.
 */
disconnect()
{
	extern FILE *cout;
	extern int data;

	if (!connected)
		return;
	(void) command("QUIT");
	(void) fclose(cout);
	cout = NULL;
	connected = 0;
	data = -1;
}

skip(cmd, file)
	char *cmd, *file;
{
	char line[BUFSIZ];

	if (!interactive)
		return (0);
	printf("%s %s? ", cmd, file);
	fflush(stdout);
	gets(line);
	return (*line == 'y');
}

fatal(msg)
	char *msg;
{

	fprintf(stderr, "ftp: %s\n");
	exit(1);
}

/*
 * Glob a local file name specification with
 * the expectation of a single return value.
 * Can't control multiple values being expanded
 * from the expression, we return only the first.
 */
globulize(cpp)
	char **cpp;
{
	char **globbed;

	if (!doglob)
		return (1);
	globbed = glob(*cpp);
	if (globerr != NULL) {
		printf("%s: %s\n", *cpp, globerr);
		if (globbed)
			blkfree(globbed);
		return (0);
	}
	if (globbed) {
		*cpp = *globbed++;
		/* don't waste too much memory */
		if (*globbed)
			blkfree(globbed);
	}
	return (1);
}
