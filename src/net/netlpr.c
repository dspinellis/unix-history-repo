/* Copyright (c) 1979 Regents of the University of California */
/*
	netlpr [-m mach] [-l login] [-p password] [-f] [-n] [file1 ... filen]
*/
/* maybe a geteuid call will fix our root problem here ? */
/* also, netlpr should check the remote lpr's other user execute
   bit to see if it is down
*/
/* must run setuid root on CC machines only */
# include "defs.h"
/* netlpr remote line printer */
main(argc,argv)
  char **argv; {
	int rcode;
	char parms[BFS], dontpasswd, rem;
	static char buf[BFS], buf1[BFS], buf2[BFS];
	argv[argc] = 0;
	strcpy(parms," ");
	debugflg = DBV;
	argv++; argc--;
	while(argc > 0 && argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'm': harg(buf,&argc,&argv); rem = lookup(buf); break;
		case 'l': harg(status.login,&argc,&argv); break;
		case 'p': harg(status.mpasswd,&argc,&argv); break;
		case 'n': status.nonotify++; break;
		case 'f': status.force++; break;
		default: strcat(parms,argv[0]); strcat(parms," "); break;
		}
		argc--, argv++;
		}
	if(rem == 0)rem = getremote(local);
	dontpasswd = machtype[rem-'a']==M_CC&&machtype[local-'a']==M_CC;
	passwdent();
	if(!dontpasswd)promptlogin();
	do {
		sprintf(buf1,"netlpr %s",parms);
		if(dontpasswd)
			sprintf(cmd,"lpr -c%04.4d,%s %s",
				status.jobno,status.localname,parms);
		else sprintf(cmd,"lpr %s",parms);
		sprintf(buf,"-m%c",rem);
		sprintf(buf2,"%s%s",buf1,(argc > 0 ? argv[0] : ""));
		if(fork() == 0){
			if(argc > 0){
				if(access(argv[0],04) == -1){
					perror(argv[0]);
					exit(1);
					}
# ifdef CC
				if(dontpasswd){
					setuid(0);
					mexecl(netcmd,"net",buf,"-y",
					"-l",status.localname,
					"-s",argv[0],"-c",buf2,cmd,0);
					}
				else
# endif
				{
					kexecl(netcmd,"net",buf,
					"-s",argv[0],"-c",buf2,cmd,0);
					}
				}
			else {
# ifdef CC
				if(dontpasswd){
					setuid(0);
					mexecl(netcmd,"net",buf,"-y","-",
					"-l",status.localname,
					"-c",buf2,cmd,0);
					}
				else
# endif
				{
					kexecl(netcmd,"net",buf, "-",
					"-c",buf2,cmd,0);
					}
				}
			fprintf(stderr,"Network is down\n");
			exit(1);
			}
		wait(&rcode);
		argc--, argv++;
		} while(argc > 0);
	exit(0);
	}
