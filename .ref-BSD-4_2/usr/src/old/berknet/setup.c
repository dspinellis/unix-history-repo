static char sccsid[] = "@(#)setup.c	4.2	(Berkeley)	10/3/82";

/*
	setup.c

	support procedures used in setting up the network

*/

# include "defs.h"

char logfile[] =	LOGFILE;

/* global variables */
struct daemonparms netd;

/*
	called in netdaemon and debugging software
	handles parameter lists to setup
	remote machine and pipes
*/
setupdaemon(argc,argv)
char **argv;{
	long timev;
	int timei;
	FILE *cfile;

	parseargs(argc,argv);

	cfile = fopen(INITFILE,"r");
	rdnetfile(cfile);
	fclose(cfile);
	err("remote %c local %c link %s inspeed %d outspeed %d length %d\n",
		remote,local,netd.dp_device,netd.dp_inspeed,
		netd.dp_outspeed,netd.dp_datasize);
	err("debug %d time %d count %d onlyuid %d usehispeed=%d hispeedlink='%s'\n",
		debugflg,netd.dp_oatime, netd.dp_maxbread,netd.dp_onlyuid,
		netd.dp_usehispeed, netd.dp_hispeedlink);
	err("sendonly %c rcvonly %c pipesim %c\n",
		chfromf(netd.dp_sndorcv < 0),chfromf(netd.dp_sndorcv > 0),
		chfromf(netd.dp_pipesim));
	setup(netd.dp_device);
	timev = gettime();
	timei = timev >> 16;
	srand(timei);
}
/*

see comment in netdaemon.c about the arguments

*/
parseargs(argc,argv)
  char **argv; {
	char stemp[30];
	remote = 0;
	while(argc > 1 && argv[1][0] == '-'){
		argc--; argv++;
		switch(argv[0][1]){
		case '8':
			netd.dp_use8bit = 1;
			break;
		case 'd':
			debugflg = 1;
			break;
		case 'h':
			netd.dp_usehispeed = 1;
			break;
		case 'l':
			netd.dp_trynetl = 0;
			break;
		case 'm':
			harg(stemp);
			remote = lookup(stemp);
			break;
		case 'o':		/* only */
			if(argv[0][2] == 's')		/* only send */
				netd.dp_sndorcv = -1;
			else if(argv[0][2] == 'r') 	/* only receive */
				netd.dp_sndorcv = 1;
			else if(argv[0][2] == 'u')	/* only uid num */
				netd.dp_onlyuid = atoi(argv[1]);
			break;
		case 'p':
			harg(stemp);
			netd.dp_datasize = atol(stemp);
			break;
		case 'r':
			harg(stemp);
			netd.dp_rdfile = fdopen(atoi(stemp),"r");
			netd.dp_pipesim++;
			break;
		case 'w':
			harg(stemp);
			netd.dp_pwritefd = atoi(stemp);
			netd.dp_pipesim++;
			break;
		/* ignore unknown options */
		}
	}
	if(remote == 0){
		fprintf(stderr,"Error- must specify machine - use -m option\n");
		exit(EX_USAGE);
	}
}
/*
	set the correct mode on the link device
*/
setup(str)
  char *str; {
	struct sgttyb stt;
# ifdef RAND
	struct {
		int     t_xflags;
		char    t_col;
		char	t_delct;
		char	t_outqc_cc;
		char	t_rawqc_cc;
	} exstt;
#define OUT8BIT 01              /* All 8 bits on output */
#define IN8BIT  02              /* All 8 bits on input  */
# endif

	initseqno();
	/* nothing to set up if we're simulating with pipes */
	if(netd.dp_pipesim)return;

	if(netd.dp_usehispeed){
		str = netd.dp_hispeedlink;
		netd.dp_datasize = SENDLEN - ACKLENGTH;
		}
	if(str == 0 || str[0] == 0){
		err("invalid net device\n");
		exit(EX_OSFILE);
		}
	netd.dp_linefd = open(str,2);
	if(netd.dp_linefd < 0){
		perror(str);
		exit(EX_OSERR);
		}
	/* set exclusive use for line */
#ifdef TIOCEXCL
#ifdef VAX
	(void)
#endif
	ioctl(netd.dp_linefd,TIOCEXCL,&stt);
#endif
	if(gtty(netd.dp_linefd,&stt) < 0){
		perror(str);
		exit(EX_OSERR);
		}
	stt.sg_ispeed = netd.dp_inspeed;	/* user set baud */
	stt.sg_ospeed = netd.dp_outspeed;  	/* user-set baud */
	stt.sg_erase = stt.sg_kill = 0;		/* erase and kill off */
	stt.sg_flags = ANYP;	/* even and odd parity, off everything else */
	if(stty(netd.dp_linefd,&stt) < 0){
		perror(str);
		exit(EX_OSERR);
		}
# ifdef RAND
	/* set device into 8-bit mode */
	if(gtty((2<<8)|netd.dp_linefd,&exstt) < 0){
		perror(str);
		exit(EX_OSERR);
		}
	exstt.t_xflags = OUT8BIT | IN8BIT;
	if(stty((2<<8)|netd.dp_linefd, &exstt) < 0){
		perror(str);
		exit(EX_OSERR);
		}
# endif
	/* set my own line discipline */
	/* NETLDISC is defined in sgtty.h on the CSVAX */
	/* setting the line discipline must be done AFTER the sttys */
# ifdef NETLDISC
	if(netd.dp_trynetl){
		netd.dp_linedis = NETLDISC;
		if(ioctl(netd.dp_linefd,TIOCSETD,&netd.dp_linedis) != 0){
			printf("error - line discipline\n");
			perror(str);
			printf("proceeding...\n");
			netd.dp_linedis = 0;
			}
		if(netd.dp_linedis){
			/* set the line into RAW mode */
			netd.dp_linedis = 0;
			ioctl(netd.dp_linefd,TIOCSETD,&netd.dp_linedis);
			netd.dp_linedis = NETLDISC;
			stt.sg_ispeed = netd.dp_inspeed;/* user set baud */
			stt.sg_ospeed = netd.dp_outspeed;  /* user-set baud */
			stt.sg_erase = stt.sg_kill = 0;		
			stt.sg_flags = ANYP|RAW;	/* in raw mode */
			if(stty(netd.dp_linefd,&stt) < 0){
				perror(str);
				exit(EX_OSERR);
			}
			ioctl(netd.dp_linefd,TIOCSETD,&netd.dp_linedis);
			printf("Using network line discipline.\n");
		}
	}
# endif
}
/*VARARGS0*/
error(s,a,b,c,d,e,f,g,h)
char *s; {
	char buf[10];
	if(remote != 0) sprintf(buf,"%s",longname(remote));
	else buf[0] = 0;
	fflush(stdout);
	if(debugflg){
		fprintf(stderr,s,a,b,c,d,e,f,g,h);
		putc('\n',stderr);
		}
	addtolog(remote,"Err %s: ",buf);
	addtolog(remote,s,a,b,c,d,e,f,g,h);
	addtolog(remote,"\n");
	}
/* this is really not right - we should use the rcslog format */
/* also, the user must be able to write on the
   public logfile to get error messages such as
   directory not found after he has
   setuid'd from root
*/
/*VARARGS0*/
addtolog(mach,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
char *s;
{
	static FILE *log = NULL;
	struct stat statbuf;
	logfile[strlen(logfile)-1] = mach;
	if(log == NULL){
		if(stat(logfile,&statbuf) < 0)return;
		log = fopen(logfile,"a");
		}
	if(log == NULL)return;
	fseek(log,0L,2);
	fprintf(log,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
	fflush(log);
	debug(s,a,b,c,d,e,f,g,h,i,h,k,l,m,n);
	}
