/* Copyright (c) 1979 Regents of the University of California */
/*
 * netmail [-l ...] [-p ...] [-f] [-n] mach
 */
# include "defs.h"

main(argc,argv)
  char **argv; {
	char *hdir;
	char buf[BFS], buf1[BFS];
	char tomach;
	debugflg = DBV;
	tomach = getremote(local);
	hdir = getenv("HOME");
	if(hdir == 0 || strcmp(hdir,"/") == 0)hdir = ".";
	argc--; argv++;
	while(argc > 0 && argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'l':	harg(status.login,&argc,&argv); break;
		case 'p': 	harg(status.mpasswd,&argc,&argv); break;
		case 'f':	status.force++; break;
		case 'n':	status.nonotify++; break;
		default:
			fprintf(stderr,
			"Usage: netmail [-l login] [-p password] [-f] [-n] [mach]\n");
			exit(1);
		}
		argc--, argv++;
		}
	if(argc > 0) tomach = lookup(argv[0]);
	sprintf(buf,"-m%c",tomach);
	sprintf(buf1,"%s/mbox.%s",hdir,longname(tomach));
	if(tomach == local)
		mexecl(Bsh,"sh","-c",cmd,0);
	else kexecl(netcmd,"net",buf,"-r",buf1,"-c","netmail",PRMAIL,0);
	fprintf(stderr,"Network is down\n");
	exit(1);
	}
