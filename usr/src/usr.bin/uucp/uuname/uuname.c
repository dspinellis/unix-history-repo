#ifndef lint
static char sccsid[] = "@(#)uuname.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <signal.h>

/*******
 *      uuname  -  return list of all remote systems 
 *		   recognized by uucp, or  (with -l) the local
 *		   uucp name.
 *
 *      return codes: 0 | 1  (can't read)
 */
 
main(argc,argv)
char *argv[];
int argc;
{
	int i;
	int intrEXIT();
	FILE *np;
/* Increase buffers for s and prev.  cornell!pavel */
	char prev[1000];
	char s[1000];

	chdir(Spool);
	strcpy(Progname, "uuname");
	signal(SIGILL, intrEXIT);
	signal(SIGTRAP, intrEXIT);
	signal(SIGIOT, intrEXIT);
	signal(SIGEMT, intrEXIT);
	signal(SIGFPE, intrEXIT);
	signal(SIGBUS, intrEXIT);
	signal(SIGSEGV, intrEXIT);
	signal(SIGSYS, intrEXIT);
	signal(SIGINT, intrEXIT);
	signal(SIGHUP, intrEXIT);
	signal(SIGQUIT, intrEXIT);
	signal(SIGTERM, intrEXIT);

	if(argv[1][0] == '-' && argv[1][1] == 'l') {
		uucpname(s);
		printf("%s\n",s);
		exit(0);
	}
        if(argc != 1) {printf("Usage: uuname [-l]\n"); exit(1);}
	if((np = fopen(SYSFILE,"r")) == NULL) {
		printf("%s (name file) protected\n",SYSFILE);
		exit(1);
	}
	while ( cfgets(s,sizeof(s),np) != NULL ) {
		for(i=0; s[i]!=' ' && s[i]!='\t'; i++)
			;
		s[i]='\0';
		if (strcmp(s, prev) == SAME)
			continue;
		if(s[0]=='x' && s[1]=='x' && s[2]=='x')
			continue;
		printf("%s\n",s);
		strcpy(prev, s);
	}
 
	exit(0);
}
intrEXIT(inter)
{
	exit(inter);
}
