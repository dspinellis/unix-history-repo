/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
/* mmail -command -time user fmach user1 user2 ... usern */
/* command and time are optional */
main(argc,argv)
  char **argv; {
	int pip[2], n, sargc, ret, i;
	char *sargv[20], *cst = 0, buf[BUFSIZ], *s = 0;
	long timesent,el;
	int roption = 0;

# ifdef ROPTION
	/* only use roption if super-user or "network" */
	i = getuid();
	if(i == 0 || i == NUID)roption++;
# endif
	if(argv[1][0] == '-'){
		cst = argv[1] + 1;
		argv++;
		argc--;
		if(argv[1][0] == '-'){
			timesent = atol(argv[1] + 1);
			s = ctime(&timesent);
			s[strlen(s) - 6] = 0;
			el = gettime() - timesent;
			argv++;
			argc--;
			}
		}
	argv[argc] = 0;
	sargc = argc-2;
	sargv[sargc] = 0;

	if(roption){
		sargv[1] = "-r";
		sargv[2] = argv[2];
		sargv[3] = argv[1];
		for (i = 3; i < argc; i++)
			sargv[i+1] = argv[i];
		sargv[argc+1] = 0;
		}
	else {
		for(i=0; i< sargc; i++)
		sargv[i] = argv[i+2];
		}
	sargv[0] = "mail";
	pipe(pip);
	if(fork() == 0){
		close(pip[1]);
		close(0);
		dup(pip[0]);
		execv("/bin/mail", sargv);
		execv("/usr/bin/mail", sargv);
		exit(1);
		}
	close(pip[0]);
	close(1);
	dup(pip[1]);
	if(!roption)
		printf("(from %s on the %s machine)\n",argv[1],argv[2]);
	if(cst != 0){
		printf("(command: %s",cst);
		if(s != 0) printf(", sent %s, took %s",s,comptime(el));
		printf(")\n");
		}
	fflush(stdout);
	while((n = read(0,buf,512)) > 0)
		write(pip[1],buf,n);
	close(pip[1]);
	close(1);
	wait(&ret);
	fprintf(stderr,"Mail sent successfully.\n");
	exit(ret>>8);
	}
