static char sccsid[] = "@(#)nsh.c	4.3	(Berkeley)	%G%";

# include "defs.h"
/*
	nsh -c "comand to be executed"

	This pseudo-shell is executed over the network
	as the login shell of an acount "network", no passwd.
	It will only execute certain allowed commands.

	has these exit codes:
		EX_USAGE = 	wrong # arguments to nsh
		9 = 		command you execute may not take arguments
		10= 		the execl failed
		EX_UNAVAILABLE= could not find full path name for the command

	count is the # of arguments (= argc) allowed.
	a count of 0 turns off the command
*/

struct {
	char *app;
	char count;
	char *full;
	char *full1;
	} st[] = {
/* I assume these are the same for RAND */
	"mmail",	20,	"/usr/net/bin/mmail",	"/usr/net/bin/mmail",
	"mwrite",	20,	"/usr/net/bin/mwrite",	"/usr/net/bin/mwrite",
	"prmail",	20,	"/usr/net/bin/prmail",	"/usr/net/bin/prmail",
# ifndef NFREECMD
	"finger",	20,	"/usr/ucb/finger",	"/usr/bin/finger",
	"lpq",		20,	"/usr/ucb/lpq",		"/usr/bin/lpq",
# ifdef FREELPR
	"lpr",		20,	"/usr/ucb/lpr",		"/usr/bin/lpr",
# endif
	"netlog",	20,	"/usr/bin/netlog",	"/usr/ucb/netlog",
	"netq",		20,	"/usr/bin/netq",	"/usr/ucb/netq",
	"ps",		20,	"/bin/ps",		"/usr/bin/ps",
	"pstat",	20,	"/etc/pstat",		"/usr/bin/pstat",
	"vpq",		20,	"/usr/ucb/vpq",		"/usr/bin/vpq",
	"w",		20,	"/usr/ucb/w",		"/usr/bin/w",
	"wc",		20,	"/usr/bin/wc",		"/bin/wc",
	"who",		20,	"/bin/who",		"/usr/bin/who",
	"whom",		20,	"/usr/ucb/whom",	"/usr/bin/whom",
	"yank",		20,	"/usr/ucb/yank",	"/usr/bin/yank",
# endif
	0, 		0,		0,		0
	};
/* nsh -c cmd */
main(argc,argv)
  char **argv; {
	char *s, buf[500];
	int i, flg = 0;
	if(argc != 3){
		fprintf(stderr,"Wrong number of arguments to nsh.\n");
		exit(EX_USAGE);
	}
	s = argv[2];
	while (*s)
		if (*s == ';'
		 || *s == '|'
		 || *s == '&'
		 || *s == '?'
		 || *s == '*'
		 || *s == '['
		 || *s == '~'
		 || *s == '{'
		 || *s == '<'
		 || *s == '>'
		 || *s == '$'
		 || *s == '`') {
			fprintf(stderr, "Illegal shell metacharacter in command.\n");
			exit(9);
		} else
			++s;
	s = argv[2];
	while(*s && *s != ' ')s++;
	if(*s == ' ')flg++;
	*s = 0;
	if((i = mlookup(argv[2])) < 0){
		fprintf(stderr,
		"Command '%s' is not allowed if logged in as 'network'.\n",
			argv[2]);
		exit(11);
	}
	if(st[i].count == 0){
		fprintf(stderr,
		"The command '%s' is not allowed to have arguments.\n",argv[2]);
		exit(9);
		}
	if(stat(st[i].full,buf) >= 0)
		strcpy(buf,st[i].full);
	else strcpy(buf,st[i].full1);
	if(flg && st[i].count > 1){  /* some cmds don't allow parms */
		*s = ' ';
		strcat(buf,s);
		}
	/*
	fprintf(stderr,"%s\n",buf);
	*/
	execl(Bsh,"sh","-c",buf,0);
	fprintf(stderr,"Execute of shell failed.\n");
	exit(EX_UNAVAILABLE);
	}
mlookup(s)
  char *s; {
	int i;
	for(i = 0; st[i].app; i++)
		if(strcmp(st[i].app,s) == 0 || strcmp(st[i].full,s) == 0
		 || strcmp(st[i].full1,s) == 0)return(i);
	return(-1);
	}
