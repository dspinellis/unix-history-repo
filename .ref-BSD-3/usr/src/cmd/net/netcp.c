# include "defs.h"

/* used by parser to parse filenames, e.g. mach:file */
struct fd {
	char mach;
	char *fn;
	};


/* netcp - copy with remote machines */
main(argc,argv)
  char **argv;
	{
	register struct fd *ff, *tf, ffile, tfile;
	char tomachstr[BUFSIZ], rcmd[BUFSIZ], acmd[BUFSIZ], *sn;
	argv[argc] = 0;
	if(argc < 3)goto usage;
	argv++, argc--;
	while(argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'b':	status.nonotify++; break;
		case 'f':	status.force++; break;
		case 'l':	harg(status.login,&argc,&argv); break;
		case 'n':	status.nowrite++; break;
		case 'p':	harg(status.mpasswd,&argc,&argv); break;
		case 'q':	status.quiet++; break;
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
	if(ff->mach  == 0 || tf->mach  == 0){
		fprintf(stderr,"Unknown machine\n");
		exit(1);
	}
	if(ff->fn[0] == 0 || tf->fn[0] == 0){
		fprintf(stderr,"Must specify both file names\n");
		exit(1);
	}
	sprintf(tomachstr,"-m%c",tf->mach);
	if(ff->mach == local && tf->mach == local){
		fprintf(stderr,"Error: both files are on this machine\n");
		exit(1);
		}
	else if(ff->mach == local)
		kexecl(netcmd,"net",tomachstr, "-o",tf->fn,"-s",ff->fn,
			"-c",rcmd,CATCMD,0);
	else if(tf->mach == local){
		tomachstr[2] = ff->mach;
		kexecl(netcmd,"net",tomachstr, "-r",tf->fn,"-i",ff->fn,
			"-c",rcmd,CATCMD,0);
		}
	/* remote for both */
	else if(tf->mach == ff->mach)
		kexecl(netcmd,"net",tomachstr,"-c",rcmd,"cp",ff->fn,tf->fn,0);
	else {
		/* experimental - still needs debugging */
		fprintf(stderr,
			"Experimental - Machines normally must be the same\n");

		/* collect info on the "To Machine" */
		remote = tf->mach;
		/* get status.login and passwd from .netrc if poss. */
		commandfile();
		if(status.login[0] == 0 || status.mpasswd[0] == 0){
			sn = SnCurrent();
			if(sn == NULL){
				fprintf(stderr,"Unknown user\n");
				exit(1);
			}
			strcpy(status.localname,sn);
			/* prompt on terminal */
			promptlogin(remote);
		}
		/* should use -q option */
		sprintf(acmd,"%s -l %s -p %s %s %s",
			NETCPCMD,status.login,status.mpasswd,argv[0],argv[1]);

		/* send the netcp command to the "From" machine */
		remote = tomachstr[2] = ff->mach;
		status.login[0] = status.mpasswd[0] = 0;
		mexecl(netcmd,"net",tomachstr,"-c",rcmd,acmd,0);
		}
	fprintf(stderr,"Network is down\n");
	exit(1);
usage:
	printf("Usage: netcp [-l ...] [-p ...] [-f] [-n] fromfile tofile\n");
	exit(1);
	}

analyze(S,f)
  char *S;
  register struct fd *f; {
	register char *file;
	char work[FNS], *s, c0,c1,c2,c3,c,colon=0;
	s = work;
	strcpy(s,S);
	f->mach = local;
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
		f->mach = lookup(file);
		if(f->mach == 0){
			return;
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
			if(c2 == '/')f->mach = 'y';		/* Cory name */
			else if(c3 == '/')f->mach = c1;		/* CC name */
		}
	f->fn = calloc(strlen(file)+1,1);
	strcpy(f->fn,file);
	s = f->fn;
	/* check for bad chars in file name */
	while(c = *s++)
		if(c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\b')
			err("Invalid character '%c'\n",c);
	}
