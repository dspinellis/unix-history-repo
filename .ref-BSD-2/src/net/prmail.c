/* Copyright (c) 1979 Regents of the University of California */
# include <stdio.h>
# include "mach.h"
# ifdef V7
# define MAILDIR "/usr/spool/mail"
# else
# define MAILDIR "/usr/mail"
# endif
main(){			/* prmail - print mail for user */
			/* should print of others too   */
	struct stat statbuf;
	FILE *f;
	char fn[100],buf[BUFSIZ],outbuf[BUFSIZ],*s;
	int i;
	setbuf(stdout,outbuf);
	s = getun(getuid());			/* always returns a str */
	if(strcmp(s,"UNKNOWN") == 0){
		perror("Unknown user");
		exit(1);
		}
	sprintf(fn,"%s/%s",MAILDIR,s);
	if(stat(fn,&statbuf) < 0 || getsize(&statbuf) == 0L){
		printf("No mail.\n");
		exit(0);
		}
	f = fopen(fn,"r");
	if(f == NULL){
		perror(fn);
		exit(1);
		}
	while((i = fread(buf,1,BUFSIZ,f)) > 0)
		fwrite(buf,1,i,stdout);
	fclose(f);
	exit(0);
	}
