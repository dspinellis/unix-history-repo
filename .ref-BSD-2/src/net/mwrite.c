/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
/* mwrite name ttystr ltime fmach fuser command time */
/* ttystr is the full name, e.g. /dev/tty0 */
/* command and time is optional */
jmp_buf env;
main(argc,argv)
  char **argv; {
	long ltime, otime, timesent, el;
	int alarmint();
	FILE *file;
	int n;
	struct utmp utmpstr;
	char buf[BFS*2],*s,buf1[BFS];
	debugflg = DBV;
	argv[argc] = 0;
	otime = ltime = 0;
	errno = 0;
	setjmp(env);
	alarm(0);
	signal(SIGCLK,alarmint);
	if(errno != 100 && argv[2][0] && argv[2][8] != 'x'){
		alarm(100);
		/*
		debug("argv3 %s",argv[3]);
		*/
		sscanf(argv[3],"%lo",&ltime);
		file = fopen("/etc/utmp","r");
		if(file == NULL)error("bad utmp");
		while(fread(&utmpstr,1,sizeof utmpstr,file) == sizeof utmpstr){
# ifdef OLDTTY
			if(utmpstr.ut_tty == argv[2][8]){
# else
			if(strcmp(utmpstr.ut_line,argv[2]+5) == 0){
# endif
				otime = utmpstr.ut_time;
				break;
				}
			}
		fclose(file);
		/*
		debug("ltime %lo otime %lo",ltime,otime);
		*/
		if(otime != 0 && otime == ltime) {
			sprintf(buf,"%s",argv[2]);
			file = fopen(buf,"w");
			if(file != NULL && fstat(fileno(file),&statbuf) !=  -1
				&& (statbuf.st_mode&02)){
				fprintf(file,"\nMessage from %s on %s machine ...\n",
				argv[5],longname(argv[4][0]));
				if(argc > 6){
					fprintf(file,"(command: %s",argv[6]);
					if(argc > 7){
						timesent=atol(argv[7])+TIMEBASE;
						s = ctime(&timesent);
						s[strlen(s)-6] = 0;
						el = gettime() - timesent;
						fprintf(file,", sent %s, took %s",s,comptime(el));
						}
					fprintf(file,")\n");
					}
				while((n = fread(buf,1,512,stdin)) > 0){
					fwrite(buf,1,n,file);
					if(feof(stdin))break;
					}
				fprintf(file,"------\n");
				exit(0);
				}
			}
		}
	
	if(argc > 6){
		sprintf(buf,"-%s",argv[6]);
		if(argc > 7){
			timesent=atol(argv[7])+TIMEBASE;
			sprintf(buf1,"-%ld",timesent);
			mexecl(mailcmd,"mmail",buf,buf1,argv[5],
				longname(argv[4][0]), argv[1], 0);
			}
		else mexecl(mailcmd,"mmail",buf,argv[5],longname(argv[4][0]),
			argv[1], 0);
		}
	else mexecl(mailcmd,"mmail",argv[5],longname(argv[4][0]),argv[1],0);
	exit(1);
	}
alarmint(){
	errno = 100;
	signal(SIGCLK,SIG_IGN);		/* alarm off */
	longjmp(env,0);			/* ugh */
	}
/* returns number of bytes written, error returns WRITEFAIL (-3) */
