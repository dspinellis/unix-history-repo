static char sccsid[] = "@(#)net.c	4.2	(Berkeley)	9/12/82";

/* sccs id variable */
static char *net_sid = "@(#)net.c	1.8";

# include "defs.h"
/* must be setuid root */
/*
	net - -b -c cmd -f -i file -l name -mmach -n -o file -p passwd
		-r file -s file -u uid -w -x -y -z command
		
	-	take from standard input
	-b	never send anything back
	-c cmd	think of this as a "cmd" *
	-f	force prompting of user name and password
	-i file	remote stdin *
	-l name remote login name
	-m Mach	remote machine
	-n	do not write back anything, always mail them back
	-o file	remote stdout & stderr *
	-p pass remote password
	-q 	quiet option, send back only if rcode !=0 or if there is stdout
	-r file	local response file
	-s file	local stdin file *
	
	(super users only, always skip login/passwd check:)
	-u uid	net queue files should be owned by uid (16 bits)
	-w	this is a write/mail response cmd *
	-x	this is being forwarded through us to another machine *
	-y	skip login/password check *
	-z	this is a response file being returned *

	* = not documented in net(NEW)
	
*/
/*
	code	option	reason
	q		normal request
	w	-w	message to be written back
	 	-x	being forwarded through us
	y	-y	simply skips login check (used by netlpr)
	s	-z	normal response
*/
/* global variables */
struct userinfo status;

/* local variables */
static char dfname[]=		DFNAME;

main(argc, argv)
  char **argv; {
	register int i;
	int outerror(),uid;
	char localin[FNS], skey[30];
	char buf[BUFSIZ], suid[10];
	char sin =0, zopt = 0, wopt = 0, yopt = 0, xopt = 0;
	char *s,**sargv;
	long cnt = 0L, maxfile = MAXFILELARGE;
	FILE *file, *temp, *rfile;
	struct utmp *putmp;
	struct stat statbuf;
	struct header hd;

	debugflg = DBV;
	hd.hd_scmdact[0] = hd.hd_srespfile[0] = hd.hd_soutfile[0] = 0;
	hd.hd_sinfile[0] = hd.hd_scmdvirt[0] = hd.hd_sttyname[0] = 0;
	localin[0] = 0;
	suid[0] = 0;
	sargv = argv;

	if(isatty(0)) strcat(hd.hd_sttyname,ttyname(0));
	else if(isatty(2)) strcat(hd.hd_sttyname,ttyname(2));
	remote = 0;
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, outerror);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, outerror);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, outerror);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, outerror);

	while(argc > 1 && argv[1][0] == '-'){
		argc--; argv++;
		switch(argv[0][1]){
		case 0:   
			sin++; 
			break;
		case 'b': 
			status.nonotify++; 
			break;
		case 'c': 
			harg(hd.hd_scmdvirt); 
			break;
		case 'f': 
			status.force++; 
			break;
		case 'i': 
			harg(hd.hd_sinfile); 
			break;
		case 'l': 
			harg(status.login); 
			break;
		case 'm': 
			harg(buf); 
			remote = lookup(buf);
			if(remote == 0){
				fprintf(stderr,"Unknown machine %s\n",buf);
				exit(EX_NOHOST);
			}
			break;
		case 'n': 
			status.nowrite++; 
			break;
		case 'o': 
			harg(hd.hd_soutfile); 
			break;
		case 'p':
			harg(status.mpasswd);
			if(status.mpasswd[0] == 0)
			  	strcpy(status.mpasswd,"\n\n");
			break;
		case 'q': 
			status.quiet++; 
			break;
		case 'r': 
			harg(buf); 
			addir(hd.hd_srespfile,buf); 
			break;
		case 's': 
			harg(localin); 
			break;
		case 'u': 
			harg(suid); 
			break;
		case 'w': 
			wopt++; 
			break;
		case 'x': 
			xopt++; 
			break;
		case 'y': 
			yopt++; 
			break;
		case 'z': 
			zopt++; 
			break;
		default:
			fprintf(stderr,"Unknown option %s\n",argv[0]);
			break;
		}
		}
	while(argc > 1){
		argc--; argv++;
		strcat(hd.hd_scmdact,argv[0]);
		strcat(hd.hd_scmdact," ");
		}
	sargv[1] = 0;		/* so ps won't show passwd ??? */
	hd.hd_uidfrom = uid = getuid();
	hd.hd_gidfrom = getgid();
	hd.hd_code = 'q';
	if(zopt || wopt || yopt || xopt || suid[0] != 0){
		/* check z or w or y or x option permission */
# ifndef TESTING
		/* check effective user id ?? */
		if (uid != SUPERUSER && uid != NUID) {
			fprintf(stderr, "Error: Not super-user\n");
			fprintf(stderr,"zopt %d wopt %d yopt %d xopt %d suid[0] %s\n", zopt, wopt, yopt, xopt, suid);
			fprintf(stderr,"uid %d\n", uid);
			debugflg = 1;
			printhd(&hd);
			outerror(EX_UNAVAILABLE);
			}
# endif
		hd.hd_code = zopt? 's': 'w';
		hd.hd_code = yopt? 'y': hd.hd_code;
		if (status.mpasswd[0] == 0)	/* no passwd required */
			strcpy(status.mpasswd, "\n");
		debug("zopt %d wopt %d yopt %d xopt %d suid[0] %s\n", zopt, wopt, yopt, xopt, suid);
		debug("uid %d\n", uid);
		if(xopt)
			setuid(SUPERUSER);
	}
#ifdef CRN
	strcpy( status.jobno, MAGICCRN );	/* default (invalid) crn */
#else
	strcpy( status.jobno, "XYZZ");		/* default (invalid) crn */
#endif

	if(hd.hd_code == 'q' && !xopt){
		/* read passwd file, get status.localname & crn */
		passwdent();
	}

	/* sets remote,status.login,status.force,status.mpasswd,
		status.nonotify, status.nowrite */
	/* may read passwd file if getenv(HOME) reads it */
	commandfile();
	if(status.force)status.login[0] = status.mpasswd[0] = 0;

	/* look up login name and passwd in the environment */
	envloginpasswd(remote,status.login,status.mpasswd);


	if(remote == 0)remote = getremote(local);
# ifndef TESTING
	if(remote == local){
		fprintf(stderr,"Request sent to local machine - doesn't make sense\n");
		/* outerror(); */
		}
# endif
	strcat(status.defcmd," ");
	if(strlen(hd.hd_scmdact) == 0)strcpy(hd.hd_scmdact,status.defcmd);
	hd.hd_scmdact[strlen(hd.hd_scmdact)-1] = 0;
	do {
		mktemp(dfname); /* make until unique!! */
	} while(stat(dfname,&statbuf) >= 0);
	/* determine through machine */
	i = gothru(local,remote);
	if(i == 0){
		s = longname(remote);
		if(s != 0)fprintf(stderr,"No path to %s machine.\n",s);
		else fprintf(stderr,"Unknown machine\n");
		outerror(EX_NOHOST);
		}
	dfname[strlen(dfname)-11] = i;		/* set directory */
	dfname[strlen(dfname)-7] = i;		/* set file (unused) */
	/* check to see if data files are directories */
	if(isdirectory(hd.hd_srespfile) || isdirectory(hd.hd_sinfile) || isdirectory(hd.hd_soutfile)){
		fprintf(stderr,"%s is a directory, must be a file\n",
			isdirectory(hd.hd_srespfile)    ? hd.hd_srespfile :
			isdirectory(hd.hd_sinfile)  ? hd.hd_sinfile :
			hd.hd_soutfile);
		outerror(EX_USAGE);
	}
	if(suid[0] != 0)uid = atoi(suid);
	if(hd.hd_srespfile[0]){
		if(strcmp(hd.hd_srespfile,"/dev/tty") == 0){
		fprintf(stderr,"Can't have /dev/tty as response file.\n");
			outerror(EX_USAGE);
			}
		if(stat(hd.hd_srespfile,&statbuf) == -1){
			strcpy(buf,hd.hd_srespfile);
			s = &buf[0];
			s = s + strlen(buf) - 1;
			while(*s != '/' && s > &(buf[0]))s--;
			*s = 0;
			debug("chkdir %s",buf);
			if(strlen(buf) == 0)strcpy(buf,".");
			if(access(buf,2) == -1){
				perror(buf);
				outerror(EX_USAGE);
				}
			if((rfile=fopen(hd.hd_srespfile,"w")) == NULL){
				perror(hd.hd_srespfile);
				outerror(EX_USAGE);
				}
			chmod(hd.hd_srespfile,0600);
			fclose(rfile);
			mchown(hd.hd_srespfile,uid,hd.hd_gidfrom);
			}
		else if(access(hd.hd_srespfile,2) == -1){
			perror(hd.hd_srespfile);
			outerror(EX_USAGE);
			}
		else if(getsize(&statbuf) != 0L){
			fprintf(stderr,"%s must have 0-length or not exist\n",
				hd.hd_srespfile);
			outerror(EX_USAGE);
		}
	}
	/* go ahead and prompt for login name and passwd, if neccessary,
	   as long as the X option has not been specified */
	if(hd.hd_code == 'q' && !xopt)promptlogin(remote);

	/* at this point, we create the dfa... file */
	file = fopen(dfname,"w");
	if(file == NULL){
		perror(dfname);
		outerror(EX_OSERR);
		}
	chmod(dfname,0600);
	mchown(dfname,uid,getgid());
	if(xopt)goto stickit;
	if(status.mpasswd[0] == '\n')
		status.mpasswd[0] = 0;
	if(status.mpasswd[0] == 0 && hd.hd_code == 'q' &&
		strcmp(status.login,"network") != 0){
		fprintf(stderr,"Zero-length password not allowed\n");
		outerror(EX_USAGE);
		}
	if(hd.hd_code == 'q' && (streql(status.login,"root") == 0 ||
		streql(status.login,"ruut") == 0)){
		fprintf(stderr,"Can't login as root through the network\n");
		outerror(EX_USAGE);
		}
	makeuukey(skey,status.login,remote);
	nbsencrypt(status.mpasswd,skey,hd.hd_sencpasswd);
	enmask(status.mpasswd);
	hd.hd_lttytime = 0;
	if(hd.hd_sttyname[0] && status.nowrite == 0){
		putmp = getutmp(hd.hd_sttyname);
		if(putmp != NULL) hd.hd_lttytime = putmp->ut_time;
	}
/*
	debug("p:%s:\n",status.mpasswd);
*/
	/* write the header info onto 'file' */
	hd.hd_mchto = remote;
	hd.hd_mesgid.msg_mch = hd.hd_mchfrom = local;
	hd.hd_vmajor = VMAJOR;
	hd.hd_vminor = VMINOR;
	strcpy(hd.hd_snto,status.login);
	strcpy(hd.hd_snfrom,status.localname);
	strcpy(hd.hd_spasswd,status.mpasswd);
	strcpy(hd.hd_ijobno, status.jobno );
	hd.hd_mesgid.msg_ltime = hd.hd_ltimesent = gettime();
	hd.hd_fquiet = status.quiet;
	hd.hd_fnonotify = status.nonotify;
	hd.hd_mesgid.msg_pid = getpid();
	hd.hd_fcompressed = 0;
	/* handle account pairs, accounts which do not require
	   a passwd if you are logged in on the same one here */
	hd.hd_facctpair = fisacctpair(&hd);

	writehdfd(&hd,file);
	printhd(&hd);
stickit:
	if(sin)
		while((i = fread(buf,1,BUFSIZ,stdin)) > 0){
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				outerror(EX_OSFILE);
				}
			if((cnt += i) > maxfile)goto toobig;
			if(feof(stdin))break;
			}
	else if(localin[0]){
		if(access(localin,4) == -1){
			perror(localin);
			outerror(EX_OSFILE);
			}
		temp = fopen(localin,"r");
		if(temp == NULL){
			perror(localin);
			outerror(EX_OSFILE);
			}
		while((i = fread(buf,1,BUFSIZ,temp)) > 0){
			if((cnt += i) > maxfile)goto toobig;
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				outerror(EX_OSFILE);
				}
			}
		fclose(temp);
		}
	fclose(file);
	chmod(dfname,0400);
	dfname[strlen(dfname)-9] = 'c';
	file = fopen(dfname,"w");
	chmod(dfname,0400);
	fclose(file);
	mchown(dfname,uid,getgid());
	exit(EX_OK);
toobig:
	fprintf(stderr,"No more than %ld bytes can be sent\n",maxfile);
	outerror(EX_USAGE);		/* no return */
	}
/* 
   called if there is an error, makes sure that the files created
   are deleted and the terminal is reset to echo
*/
outerror(ret){
	register int i;
	struct sgttyb stt;
	signal(SIGHUP,SIG_IGN); signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_IGN); signal(SIGTERM,SIG_IGN);
	unlink(dfname);
	i = strlen(dfname) - 9;
	dfname[i] = (dfname[i] == 'c' ? 'd' : 'c');
	unlink(dfname);
	if(gtty(0,&stt) >= 0){
		stt.sg_flags |= ECHO;
		stty(0,&stt);
		}
	exit(ret);
	}
enmask(s)
  register char *s; {
	while(*s){
		*s &= 0177;		/* strip quote bites */
		*s++ ^= 040;		/* invert upper-lower */
		}
	}
addir(s,t)
  register char *s, *t; {
	if(t[0] == '/')strcpy(s,t);
	else {
		gwd(s);
		strcat(s,t);
		}
	}

/* returns true if phd is an account pair, false otherwise */
fisacctpair(phd)
register struct header *phd; 
{
	return(0);
}



static struct stat x;
static struct direct y;
static int off = -1;


/* these three routines gwd, cat, ckroot and 
   data structures x, y, off, do a pwd to string name */
#ifdef V6
static FILE *file;

gwd(name)
  register char *name; {
	*name = 0;
	for(;;){
		stat(".",&x);
		if((file = fopen("..","r")) == NULL)break;
		do {
			if(fread(&y,1,sizeof y,file) != sizeof y)break;
			} while(y.d_ino != x.st_ino);
		fclose(file);
		if(y.d_ino == ROOTINO){
			ckroot(name);
			break;
			}
		if(cat(name))break;
		chdir("..");
		}
	chdir(name);
	}
ckroot(name)
  char *name; {
	register int i;
	if(stat(y.d_name,&x) < 0)return;
	i = x.st_dev;
	if(chdir("/") < 0)return;
	if((file = fopen("/","r")) == NULL)return;
	do {
		if(fread(&y,1,sizeof y,file) != sizeof y)return;
		if(y.d_ino == 0)continue;
		if(stat(y.d_name,&x) < 0)return;
		} while(x.st_dev!=i || (x.st_mode&S_IFMT)!=S_IFDIR);
	if(strcmp(y.d_name,".") != 0 && strcmp(y.d_name,"..") != 0)
		if(cat(name))return;
	i = strlen(name);
	name[i+1] = 0;
	while(--i >= 0)name[i + 1] = name[i];
	name[0] = '/';
	return;
	}
#else
static DIR *file;
static struct stat xx;

gwd(name)
  register char *name;  {
	int rdev, rino;
	register int i;
	register struct direct *dp;

	*name = 0;
	stat("/", &x);
	rdev = x.st_dev;
	rino = x.st_ino;
	for (;;) {
		stat(".", &x);
		if (x.st_ino == rino && x.st_dev == rdev)
			break;
		if ((file = opendir("..")) == NULL)
			break;
		fstat(file->dd_fd, &xx);
		chdir("..");
		if (x.st_dev == xx.st_dev) {
			if (x.st_ino == xx.st_ino)
				break;
			do
				if ((dp = readdir(file)) == NULL)
					break;
			while (dp->d_ino != x.st_ino);
		}
		else do {
			if ((dp = readdir(file)) == NULL)
				break;
			stat(dp->d_name, &xx);
		} while (xx.st_ino != x.st_ino || xx.st_dev != x.st_dev);
		blkcpy(dp, &y, DIRSIZ(dp));
		closedir(file);
		if (cat(name))
			break;
	}
	i = strlen(name);
	name[i+1] = 0;
	while (--i >= 0) name[i+1] = name[i];
	name[0] = '/';
}
#endif

cat(name)
  register char *name; {		/* return 1 to exit */
	register int i,j;
	i = -1;
	while(y.d_name[++i] != 0);
	if((off+i+2) > 511)return(1);
	for(j = off +1; j >= 0; --j)name[j+i+1] = name[j];
	off = i + off + 1;
	name[i] = '/';
	for(--i; i>= 0; --i)name[i] = y.d_name[i];
	return(0);
	}


/*
	this function takes a file name and tells whether it is a 
	directory or on. Returns 1 if so, 0 otherwise.
	null strings etc. return 0.
*/
isdirectory(fn)
	char *fn;
{
	int i,ret=0;
	if(fn == NULL || *fn == 0)return(0);
	i = strlen(fn);
	if(i == 1){
		if(strcmp(fn,".")       == 0)ret = 1;
		if(strcmp(fn,"/")       == 0)ret = 1;
	}
	else if(i == 2){
		if(strcmp(fn,"..")      == 0)ret = 1;
		if(strcmp(fn,"/.")      == 0)ret = 1;
	}
	else {
		if(strcmp(fn+i-2,"/.")  == 0)ret = 1;
		if(strcmp(fn+i-3,"/..") == 0)ret = 1;
	}
	return(ret);
}

blkcpy(from, to, size)
	register *from, *to;
	register int size;
{
	while (size-- > 0)
		*to++ = *from++;
}
