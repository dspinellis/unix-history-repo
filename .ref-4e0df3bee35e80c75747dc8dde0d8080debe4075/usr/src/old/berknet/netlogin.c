static char sccsid[] = "@(#)netlogin.c	4.1	(Berkeley)	%G%";

/* sccs id variable */
static char *netlogin_sid = "@(#)netlogin.c	1.2";

/*
	netlogin - specify login name and password in environment
*/
# include "defs.h"

/* global variables */
struct userinfo status;

main(argc,argv)
	char **argv;
{
	char mch, stemp[20], sencpasswd[30];
# ifdef V6
	fprintf(stderr,"The netlogin command doesn't work on Version 6 UNIX\n");
	exit(EX_UNAVAILABLE);
# endif
	mch = 0;
	status.login[0] = 0;
	status.mpasswd[0] = 0;
	while(argc > 1 && argv[1][0] == '-'){
		argc--;
		argv++;
		switch(argv[0][1]){
		case 'm':
			harg(stemp);
			mch = lookup(stemp);
			if(mch == 0){
				fprintf(stderr,"Bad machine %s\n",stemp);
				exit(EX_NOHOST);
			}
			break;
		case 'l':
			harg(status.login);
			break;
		default:
			usage();
			break;
		}
	}
	if(mch == 0)usage();
	passwdent();
	promptlogin(mch);
	if(strcmp(status.mpasswd,"\"\"") == 0)status.mpasswd[0] = 0;
	mkpwunclear(status.mpasswd,mch,sencpasswd);
	printf("%s,%s\n",status.login,sencpasswd);
	exit(EX_OK);
}
usage(){
	fprintf(stderr,"Usage: netlogin -m mach [-l username]\n");
	exit(EX_USAGE);
}
