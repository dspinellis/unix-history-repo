/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
/*
	count is the # of arguments (= argc) allowed.
	a count of 0 turns off the command */
/* should get route # of Cory RCS lpr */
struct {
	char *app;
	char count;
	char *full;
	char *full1;
	} st[] = {
	"finger",	20,	"/usr/new/finger",	"/usr/bin/finger",
	"lpq",		20,	"/usr/bin/lpq",		"/bin/lpq",
	"mmail",	20,	"/usr/net/bin/mmail",	"/usr/net/bin/mmail",
	"mwrite",	20,	"/usr/net/bin/mwrite",	"/usr/net/bin/mwrite",
	"netq",		20,	"/usr/bin/netq",	"/usr/new/netq",
	"ps",		20,	"/bin/ps",		"/usr/bin/ps",
	"pstat",	20,	"/usr/bin/pstat",	"/bin/pstat",
	"rcs",		20,	"/usr/bin/rcs",		"/bin/rcs",
	"rcslog",	1,	"/usr/bin/rcslog",	"/bin/rcslog",
	"rcsq",		20,	"/usr/bin/rcsq",	"/bin/rcsq",
	"trq",		20,	"/usr/bin/trq",		"/bin/trq",
	"w",		20,	"/usr/bin/w",		"/bin/w",
	"where",	20,	"/usr/bin/where",	"/bin/where",
	"who",		20,	"/bin/who",		"/usr/bin/who",
	"whom",		20,	"/usr/new/whom",	"/usr/bin/whom",
	"write",	20,	"/usr/bin/write",	"/bin/write",
	"yank",		20,	"/usr/new/yank",	"/usr/bin/yank",
	0, 		0,		0,		0
	};
/* nsh -c cmd */
main(argc,argv)
  char **argv; {
	char *s, buf[500];
	int i, flg = 0;
	if(argc != 3)exit(8);
	s = argv[2];
	while(*s && *s != ' ')s++;
	if(*s == ' ')flg++;
	*s = 0;
	if((i = mlookup(argv[2])) >= 0){
		if(st[i].count == 0)exit(9);
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
		}
	exit(10);
	}
mlookup(s)
  char *s; {
	int i;
	for(i = 0; st[i].app; i++)
		if(strcmp(st[i].app,s) == 0 || strcmp(st[i].full,s) == 0
		 || strcmp(st[i].full1,s) == 0)return(i);
	return(-1);
	}
