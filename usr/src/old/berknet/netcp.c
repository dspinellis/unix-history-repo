static char sccsid[] = "@(#)netcp.c	4.1	(Berkeley)	9/12/82";

# include "defs.h"
/* sccs id variable */
static char *netcp_sid = "@(#)netcp.c	1.2";


/* global variables */
struct userinfo status;

/* netcp - copy with remote machines */
main(argc,argv)
  char **argv;
	{
	char rcmd[BUFSIZ], acmd[BUFSIZ], *sn;
	char mchto, mchfrom, sfnto[FNS], sfnfrom[FNS];

	argv[argc] = 0;
	debugflg = DBV;
	if(argc < 3)goto usage;
	argv++, argc--;
	while(argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'b':	status.nonotify++; break;
		case 'f':	status.force++; break;
		case 'l':	harg(status.login); break;
		case 'n':	status.nowrite++; break;
		case 'p':	harg(status.mpasswd); break;
		case 'q':	status.quiet++; break;
		default:	fprintf(stderr,"Unknown option %s\n",argv[0]);
				break;
		}
		argc--;argv++;
		}
	if(argc > 2)goto usage;
	sprintf(rcmd,"netcp %s %s",argv[0],argv[1]);
	mchfrom = analfile(sfnfrom,argv[0]);
	mchto =   analfile(sfnto,argv[1]);
	if(mchfrom  == 0 || mchto  == 0){
		fprintf(stderr,"Unknown machine\n");
		exit(EX_NOHOST);
	}
	if(sfnfrom[0] == 0 || sfnto[0] == 0){
		fprintf(stderr,"Must specify both file names\n");
		exit(EX_USAGE);
	}
	if(mchfrom == local && mchto == local){
		fprintf(stderr,"Error: both files are on this machine\n");
		exit(EX_USAGE);
		}
	else if(mchfrom == local)
		kexecl(netcmd,"net","-m",longname(mchto), "-o",sfnto,"-s",sfnfrom,
			"-c",rcmd,"cat",0);
	else if(mchto == local){
		kexecl(netcmd,"net","-m",longname(mchfrom), "-r",sfnto,"-i",sfnfrom,
			"-c",rcmd,"cat",0);
		}
	/* remote for both */
	else if(mchto == mchfrom)
		kexecl(netcmd,"net","-m",longname(mchto),"-c",rcmd,"cp",sfnfrom,sfnto,0);
	else {
		/* experimental - still needs debugging */
		fprintf(stderr,
			"Experimental - Machines normally must be the same\n");

		/* collect info on the "To Machine" */
		remote = mchto;
		/* get status.login and passwd from .netrc if poss. */
		commandfile();
		if(status.login[0] == 0 || status.mpasswd[0] == 0){
			sn = SnFromUid(getuid());
			if(sn == NULL){
				fprintf(stderr,"Unknown user\n");
				exit(EX_OSFILE);
			}
			strcpy(status.localname,sn);
			/* check environ */
			envloginpasswd(remote,status.login,status.mpasswd);
			/* prompt on terminal */
			promptlogin(remote);
		}
		/* should use -q option */
		sprintf(acmd,"%s -l %s -p %s %s %s",
			NETCPCMD,status.login,status.mpasswd,argv[0],argv[1]);

		/* send the netcp command to the "From" machine */
		remote = mchfrom;
		status.login[0] = status.mpasswd[0] = 0;
		mexecl(netcmd,"net","-m",longname(mchfrom),"-c",rcmd,acmd,0);
		}
	perror(netcmd);
	fprintf(stderr,"Network is down\n");
	exit(EX_UNAVAILABLE);
usage:
	printf("Usage: netcp [-l ...] [-p ...] [-f] [-n] fromfile tofile\n");
	exit(EX_USAGE);
	}

analfile(sfn,addr)
char *sfn;
char *addr;
{
	register char *file;
	char work[FNS], *s, c0,c1,c2,c3,c,colon=0,mch;
	mch = local;
	strcpy(work,addr);
	s = work;
	file = s;
	while(*s){
		if(*s == '/')break;
		if(*s == ':'){
			colon = 1;
			*s++ = 0;
			break;
		}
		s++;
	}
	if(colon){ /* name specified */
		mch = lookup(file);
		if(mch == 0){
			return(mch);
			}
		file = s;
		}
	else {
		s  = file;
		c0 = *s++;
		c1 = *s++;
		c2 = *s++;
		c3 = *s++;
		if(c0 == '/' && c1 != '/' && islower(c1))
			if(c3 == '/')mch = c1;		/* CC name */
	}
	strcpy(sfn,file);
	s = sfn;
	/* check for bad chars in file name */
	while(c = *s++)
		if(c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\b')
			err("Invalid character '%c'\n",c);
	return(mch);
	}
