#ifndef lint
static char RCSid[] = "$Header: cmds.c,v 2.5 87/04/01 09:32:37 ed Exp $";
#endif
/* $Log:	cmds.c,v $
 * Revision 2.5  87/04/01  09:32:37  ed
 * Fixed bug in setpeer if no host accompanies -F (from Chris Liebman)
 * 
 * Revision 2.4  87/01/09  16:52:00  ed
 * Use FilingSubset, if rejected attempt Filing
 * Allows user override with -F switch
 * Maintain FilingSubset mandatory attributes
 * User niceties:  echo file name/type on transfer commands
 * 		prompt on delete
 * guess type which will determine file type implied by content
 * New commands: (type related) Guess, Whatis
 * 	      (file transfer) Copy, Move, Rename
 * 
 * Revision 2.3  86/12/15  11:40:49  jqj
 * Added support for more ViewPoint file types (no other attributes, though)
 * 
 * Revision 2.2  86/12/11  06:11:30  jqj
 * Eliminated form, mode, and struct commands.  Started adding support for
 * more file types.
 * 
 * Revision 2.1  86/06/30  12:18:56  jqj
 * get proper order of args to allow mget to work right.
 * 
 * Revision 2.0  85/11/21  07:22:41  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/10/30  08:06:37  root
 * if prompt=off but verbose=on, print the name of each file during
 * multiplefile transfers.
 * 
 * Revision 1.1  85/05/27  06:30:51  jqj
 * Initial revision
 * 
 * based on Berkeley tcp/ftp
 */
/* static char sccsid[] = "@(#)cmds.c	4.9 (Berkeley) 7/26/83"; */

/*
 * FTP User Program -- Command Routines.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/file.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>

#include "ftp_var.h"

extern	char *globerr;
extern	char **glob();
extern	char *home;
extern	short gflag;
extern	char *remglob();
extern	char *getenv();
extern	char *index();
extern	char *rindex();

/*
 * Connect to peer server and
 * auto-login, if possible.
 */
setpeer(argc, argv)
	int argc;
	char *argv[];
{
	CourierConnection *hookup();

	if (connected != (CourierConnection*)0) {
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
	} else if (strcmp(argv[1], "-F") == 0) {
		usefiling= 1;
		if (argc < 3) {
			strcat(line, " ");
			printf("(to) ");
			gets(&line[strlen(line)]);
			makeargv();
			argc = margc;
			argv = margv;
		}
		argc--;
		argv[1]= argv[2];
	}

	if (argc > 2) {
		printf("usage: %s [-F] fileservice-name\n", argv[0]);
		return;
	}
	connected = hookup(argv[1]);
	if (connected != (CourierConnection*)0) {
		if (autologin) {
			login(0,0);
		}
	}
}

struct	types {
	char	*t_name;
	int	t_type;
} types[] = {
	{ "ascii",	TYPE_A },
	{ "binary",	TYPE_I },
	{ "image",	TYPE_I },
	{ "interpress",	TYPE_Interpress },
	{ "viewpoint",	TYPE_VP },
	{ "vpcanvas",	TYPE_VPCanvas },
	{ "vpdictionary",TYPE_VPDictionary },
	{ "vpmailnote",	TYPE_VPMailNote },
	{ "vpreference",TYPE_VPReference },
	{ "serialized",	TYPE_S },
	{ "guess",	TYPE_Guess },
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

	if (argc > 2 || (argc == 2 && strcmp(argv[1],"?") == 0)) {
		char *sep;

		printf("usage: %s [ <integer> ", argv[0]);
		sep = " | ";
		for (p = types; p->t_name; p++) {
			printf("%s%s", sep, p->t_name);
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
		int tmp = atoi(argv[1]);
		if (tmp == 0 && strcmp(argv[1],"0") != 0) {
			printf("%s: unknown mode (\"%s ?\" for help)\n",
				argv[1], argv[0]);
			return;
		}
		sprintf(typename,"%d",tmp);
		typevalue = tmp;
		return;
	}
	strcpy(typename, p->t_name);
	typevalue = p->t_type;
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
 * Set guess transfer type.
 *	basically make educated guess of file type as stored
 */
/*VARARGS*/
setguess()
{
    	call(settype, "type", "guess", 0);
}

put(argc, argv)
	int argc;
	char *argv[];
{
	char *cmd;

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
	cmd = (argv[0][0] == 'a') ? "APPE" : "STOR";

	sendrequest(cmd, argv[1], argv[2]);

}

/*
 * Send multiple files.
 */
mput(argc, argv)
	char *argv[];
{
	register int i;

	if (argc < 2) {
		strcat(line, " ");
		printf("(local-files) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s local-files\n", argv[0]);
		return;
	}
	for (i = 1; i < argc; i++) {
		register char **cpp, **gargs;

		if (!doglob) {
			if (confirm(argv[0], argv[i]))
				sendrequest("STOR", argv[i], argv[i]);
			continue;
		}
		gargs = glob(argv[i]);
		if (globerr != NULL) {
			printf("%s\n", globerr);
			if (gargs)
				blkfree(gargs);
			continue;
		}
		for (cpp = gargs; cpp && *cpp != NULL; cpp++)
			if (confirm(argv[0], *cpp))
				sendrequest("STOR", *cpp, *cpp);
		if (gargs != NULL)
			blkfree(gargs);
	}
}

/*
 * Receive one file.
 */
get(argc, argv)
	char *argv[];
{

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

	recvrequest("RETR", argv[2], argv[1], "w");
}
/*
 * given a remote file name, strip directory path and version number
 */
char *
getlocalname(cp)
	char *cp;
{
	char *result;
	static char buf[MAXPATHLEN];

	result = rindex(cp,'/'); /* find last component */
	if (result == NULL) result = cp;
	else result += 1;
	strcpy(buf, result);
	result = index(buf,'!'); /* strip version # if any */
	if (result != NULL && result != buf) *result = '\000';
	return(buf);
}

/*
 * Get multiple files.
 */
mget(argc, argv)
	char *argv[];
{
	char *cp;
	char *tailp;

	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-files) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s remote-files\n", argv[0]);
		return;
	}
	while ((cp = remglob(argc, argv)) != NULL)
		if (confirm(argv[0], cp)) {
			tailp = getlocalname(cp);
			recvrequest("RETR", tailp, cp, "w");
		}
}

/*
 * Deserialize a file to file/descendants]
 */
restore(argc, argv)
	int argc;
	char *argv[];
{
	char *cmd;

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
	cmd = "DSER";

	sendrequest(cmd, argv[1], argv[2]);

}

/*
 * Serialize a file/descendants.
 */
archive(argc, argv)
	char *argv[];
{

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

	recvrequest("SER", argv[2], argv[1], "w");
}

char *
remglob(argc, argv)
	char *argv[];
{
	char temp[16];
	static char buf[MAXPATHLEN];
	static FILE *ftemp = NULL;
	static char **args;
	int oldverbose, oldhash;
	char *cp, *mode;

	if (!doglob) {
		if (args == NULL)
			args = argv;
		if ((cp = *++args) == NULL)
			args = NULL;
		return (cp);
	}
	if (ftemp == NULL) {
		strcpy(temp, "/tmp/ftpXXXXXX");
		mktemp(temp);
		oldverbose = verbose, verbose = 0;
		oldhash = hash, hash = 0;
		for (mode = "w"; *++argv != NULL; mode = "a")
			recvrequest("NLST", temp, *argv, mode);
		verbose = oldverbose; hash = oldhash;
		ftemp = fopen(temp, "r");
		unlink(temp);
		if (ftemp == NULL) {
			printf("can't find list of remote files, oops\n");
			return (NULL);
		}
	}
	if (fgets(buf, sizeof (buf), ftemp) == NULL) {
		fclose(ftemp), ftemp = NULL;
		return (NULL);
	}
	if ((cp = index(buf, '\n')) != NULL)
		*cp = '\0';
	return (buf);
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
	printf("Type: %s\n", typename);
	printf("Verbose: %s; Bell: %s; Prompting: %s; Globbing: %s\n", 
		onoff(verbose), onoff(bell), onoff(interactive),
		onoff(doglob));
	printf("Hash mark printing: %s\n",
		onoff(hash));
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
 * Toggle hash mark printing during transfers.
 */
/*VARARGS*/
sethash()
{

	hash = !hash;
	printf("Hash mark printing %s", onoff(hash));
	if (hash)
		printf(" (1 packet/hash mark)");
	printf(".\n");
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
/*VARARGS*/
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
	printf("Debugging %s (debug=%d).\n", onoff(debug), debug);
}

/*
 * Set current working directory
 * on remote machine.
 */
cd(argc, argv)
	char *argv[];
{

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
	(void) docd(argv[1]);
}

/*
 * Set current working directory
 * on local machine.
 */
lcd(argc, argv)
	char *argv[];
{
	char buf[MAXPATHLEN];

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
	(void) dodelete( argv[1]);
}

/*
 * Delete multiple files.
 */
mdelete(argc, argv)
	char *argv[];
{
	char *cp;

	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-files) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 2) {
		printf("%s remote-files\n", argv[0]);
		return;
	}
	while ((cp = remglob(argc, argv)) != NULL)
		if (confirm(argv[0], cp))
			(void) dodelete( cp);
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
	dorename(argv[1], argv[2]);
}

/*
 * Copy a remote file.
 */
copyfile(argc, argv)
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
	docopy("COPY", argv[1], argv[2]);
}

/*
 * Move a remote file.
 */
movefile(argc, argv)
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
		printf("(to-directory) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) 
		goto usage;
	docopy("MOVE", argv[1], argv[2]);
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
		argc++, argv[1] = "*";
	if (argc < 3)
		argc++, argv[2] = "-";
	if (argc > 3) {
		printf("usage: %s remote-directory local-file\n", argv[0]);
		return;
	}
	cmd = argv[0][0] == 'l' ? "NLST" : "LIST";
	if (strcmp(argv[2], "-") && !globulize(&argv[2]))
		return;
	recvrequest(cmd, argv[2], argv[1], "w");
}

/*
 * Get a directory listing
 * of multiple remote files.
 */
mls(argc, argv)
	char *argv[];
{
	char *cmd, *mode, *cp, *dest;

	if (argc < 2) {
		strcat(line, " ");
		printf("(remote-files) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) {
		strcat(line, " ");
		printf("(local-file) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc < 3) {
		printf("%s remote-files local-file\n", argv[0]);
		return;
	}
	dest = argv[argc - 1];
	argv[argc - 1] = NULL;
	if (strcmp(dest, "-"))
		if (globulize(&dest) && confirm("local-file", dest))
			return;
	cmd = argv[0][1] == 'l' ? "NLST" : "LIST";
	for (mode = "w"; cp = remglob(argc, argv); mode = "a")
		if (confirm(argv[0], cp))
			recvrequest(cmd, argv[2], argv[1], "w");
}

/*
 * Unify Accesslists for a single file.
 */
unify(argc, argv)
	char *argv[];
{

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
	(void) dounify( argv[1]);
}

/*
 * Do a shell escape
 */
shell(argc, argv)
	char *argv[];
{
	int pid, status, (*old1)(), (*old2)();
	char shellnam[40], *shell, *namep;
	char **cpp, **gargs;

	old1 = signal (SIGINT, SIG_IGN);
	old2 = signal (SIGQUIT, SIG_IGN);
	if ((pid = fork()) == 0) {
		for (pid = 3; pid < 20; pid++)
			close(pid);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		if (argc <= 1) {
			shell = getenv("SHELL");
			if (shell == NULL)
				shell = "/bin/sh";
			namep = rindex(shell,'/');
			if (namep == NULL)
				namep = shell;
			strcpy(shellnam,"-");
			strcat(shellnam, ++namep);
			if (strcmp(namep, "sh") != 0)
				shellnam[0] = '+';
			if (debug) {
				printf ("%s\n", shell);
				fflush (stdout);
			}
			execl(shell, shellnam, 0);
			perror(shell);
			exit(1);
		}
		cpp = &argv[1];
		if (argc > 2) {
			if ((gargs = glob(cpp)) != NULL)
				cpp = gargs;
			if (globerr != NULL) {
				printf("%s\n", globerr);
				exit(1);
			}
		}
		if (debug) {
			register char **zip = cpp;

			printf("%s", *zip);
			while (*++zip != NULL)
				printf(" %s", *zip);
			printf("\n");
			fflush(stdout);
		}
		execvp(argv[1], cpp);
		perror(argv[1]);
		exit(1);
	}
	if (pid > 0)
		while (wait(&status) != pid)
			;
	signal(SIGINT, old1);
	signal(SIGQUIT, old2);
	if (pid == -1)
		perror("Try again later");
	return (0);
}

/*
 * Send new user information (re-login)
 */
user(argc, argv)
	int argc;
	char **argv;
{

	if (argc < 2) {
		login(0,0);
	}
	if (argc > 3) {
		printf("usage: %s username [password]\n", argv[0]);
		return (0);
	}
	login(argv[1], argv[2]);
	return (1);
}

/*
 * Print working directory.
 */
/*VARARGS*/
pwd()
{
	dopwd();
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
	domakedir(argv[1]);
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
	doremovedir(argv[1]);
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
	logout();
	CourierClose(connected);
	connected = (CourierConnection *)0;
}

confirm(cmd, file)
	char *cmd, *file;
{
	char line[BUFSIZ];

	if (!interactive) {
		if (verbose)
			printf("%s %s\n", cmd, file);
		return (1);
	}
	printf("%s %s? ", cmd, file);
	fflush(stdout);
	gets(line);
	return (*line != 'n' && *line != 'N');
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

whatis(argc, argv)
	int argc;
	char *argv[];
{
	FILE *fin, *fopen();
	int type;
	char *typetostring();

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
		printf("%s local-file\n", argv[0]);
		return;
	}

	if ( access(argv[1], R_OK | F_OK ) == -1 ) {
		perror("file error: ");
		return;
	}

	type= get_type(argv[1]);

	printf("file %s is assumed to be %s\n", argv[1], typetostring(type));
}
