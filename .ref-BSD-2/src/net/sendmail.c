/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"

/* sendmail - send remote mail */
/* sendmail mach:user1 user2 ... usern */
main(argc,argv)
  char **argv; {
	register struct fd *ff;
	struct fd ffile;
	char buf2[BFS], rem;
	int buf[BFS], buf1[BFS];
	char rcmd[2*BFS];
	int i;
	struct passwd *pwd;
	argc[argv] = 0;
	debugflg = DBV;
	ff = &ffile;
	if(argc < 2){
		fprintf(stderr,"Usage: sendmail m:user1 user2 ... usern\n");
		exit(1);
		}
	pwd = getpwuid(getuid());
	strcpy(buf2,pwd->pw_name);
	analyze(argv[1],ff);
	rem = getremote(local);
	if(ff->mach != 0)rem = ff->mach;
	sprintf(buf,"-m%c",rem);
	sprintf(buf1,"%s %s %s ",mailcmd,buf2,longname(local));
	strcpy(rcmd,"mail ");
	for(i=1;i<argc;i++){
		if(member(argv[i],':')){
			analyze(argv[i],ff);
			strcat(buf1,ff->fn);
			}
		else strcat(buf1,argv[i]);
		strcat(buf1," ");
		strcat(rcmd,argv[i]);
		strcat(rcmd," ");
		}
	rcmd[strlen(rcmd) -1] = 0;
	if(rem == local)
		fprintf(stderr,"Use mail to send to this machine.\n");
	else {
		mexecl(netcmd,"net",buf,"-l","network","-p","",
		"-","-c",rcmd,buf1,0);
		fprintf(stderr,"Network is down\n");
		}
	exit(1);
	}
