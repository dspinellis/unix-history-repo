/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"

/* netcp - copy with remote machines */
main(argc,argv)
  char **argv;
	{
	register struct fd *ff, *tf, ffile, tfile;
	char buf[BFS], rcmd[BFS*2], acmd[BFS*2];
	argv[argc] = 0;
	if(argc < 3)goto usage;
	argv++, argc--;
	while(argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'l':	harg(status.login,&argc,&argv); break;
		case 'p':	harg(status.mpasswd,&argc,&argv); break;
		case 'n':	status.nonotify++; break;
		case 'f':	status.force++; break;
		default:	fprintf(stderr,"Unknown option %s\n",argv[0]);
				break;
		}
		argc--;argv++;
		}
	if(argc > 2)goto usage;
	ff = &ffile;
	tf = &tfile;
	sprintf(rcmd,"netcp %s %s",argv[0],argv[1]);
	analyze(argv[0],ff);
	analyze(argv[1],tf);
	sprintf(buf,"-m%c",tf->mach);
	if(ff->mach == local && tf->mach == local){
		fprintf(stderr,"Error: both files are on this machine\n");
		exit(1);
		}
	else if(ff->mach == local)
		kexecl(netcmd,"net",buf, "-o",tf->fn,"-s",ff->fn,
			"-c",rcmd,CATCMD,0);
	else if(tf->mach == local){
		buf[2] = ff->mach;
		kexecl(netcmd,"net",buf, "-r",tf->fn,"-i",ff->fn,
			"-c",rcmd,CATCMD,0);
		}
	/* remote for both */
	else if(tf->mach == ff->mach)
		kexecl(netcmd,"net",buf,"-c",rcmd,"cp",ff->fn,tf->fn,0);
	else {
		/* experimental */
		printf("Machines must be the same\n");
		fprintf(stderr,"To machine:\n");
		if(status.login[0] == 0 || status.mpasswd[0] == 0)
			promptlogin();
		sprintf(acmd,"%s -l %s -p %s %s %s",
			NETCPCMD,status.login,status.mpasswd,argv[0],argv[1]);
		buf[2] = ff->mach;
		fprintf(stderr,"From machine:\n");
		mexecl(netcmd,"net",buf,"-c",rcmd,acmd,0);
		}
	fprintf(stderr,"Network is down\n");
	exit(1);
usage:
	printf("Usage: netcp [-l ...] [-p ...] [-f] [-n] fromfile tofile\n");
	exit(1);
	}

