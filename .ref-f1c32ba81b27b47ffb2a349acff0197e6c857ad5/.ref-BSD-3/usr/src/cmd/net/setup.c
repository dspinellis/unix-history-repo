/*
	setup.c

	support procedures used in setting up the network

*/

# include "defs.h"

char logfile[] =	LOGFILE;

/* global variables */
int datasize = SIZE;		/* best if mult of 512 */
char vaxtovax = 0;
int linkspeed = LINKS;
char local;
char device[];
int debugflg;
int maxbread,atime;
int onlyuid;

short masterseqno;
FILE *readtty,*writetty;
int readfd, writefd, pipesim;
/*
	called in netdaemon and debugging software
	handles parameter lists to setup
	remote machine and pipes
*/
setupdaemon(argc,argv)
  char **argv; {
	remote = argc > 1 ? lookup(argv[1]) : getremote(local);
	if(argc == 4){	/* simulate using pipes */
		readfd = atoi(argv[2]);
		writefd = atoi(argv[3]);
		pipesim++;
		}
	initdaemon();
	}
/*
	set the correct mode on the link device
*/
setup(str)
  char *str; {
	struct sgttyb stt;
	static char readbuf[BUFSIZ],writebuf[BUFSIZ];
	if(str == 0 || str[0] == 0){
		err("invalid net device\n");
		exit(1);
		}
	masterseqno = 1;
	readtty = pipesim ? fdopen(readfd,"r") : fopen(str,"r");
	if(readtty == NULL){
		perror(str);
		exit(1);
		}
	writetty = pipesim ? fdopen(writefd,"w") : fopen(str,"w");
	if(writetty == NULL){
		perror(str);
		exit(1);
		}
	if(!pipesim){
		/* set exclusive use for line */
		if(ioctl(fileno(readtty),TIOCEXCL,&stt) != 0 ||
			gtty(fileno(readtty),&stt) < 0){
			perror(str);
			exit(1);
			}
		stt.sg_ispeed = stt.sg_ospeed = linkspeed;  /* user-set baud */
		stt.sg_erase = stt.sg_kill = 0;		/* erase and kill off */
		stt.sg_flags = ANYP;	/* even and odd parity, off everything else */
		if(stty(fileno(readtty),&stt) < 0){
			perror(str);
			exit(1);
			}
		}
	setbuf(readtty,readbuf);
	setbuf(writetty,writebuf);
	}
/*
	initialize various data structures and print banner
*/
initdaemon(){
	long timev;
	int timei;
	FILE *cfile;
	cfile = fopen(INITFILE,"r");
	rdnetfile(cfile);
	fclose(cfile);
	err("remote %c local %c link %s speed %d vtov %d length %d\n",
	remote,local,device,linkspeed,vaxtovax,datasize);
	err("debug %d time %d count %d onlyuid %d\n",debugflg,atime,
		maxbread,onlyuid);
	setup(device);
	timev = gettime();
	timei = timev >> 16;
	srand(timei);
# ifdef IMAGE
	if(machtype[local - 'a'] != M_OTHER)
# endif
# ifdef EECS40
	if(machtype[local - 'a'] != M_OTHER)
# endif
# ifdef OPTVAX
	if(machtype[local - 'a'] != M_OTHER)
# endif
# ifdef CSVAX
	if(machtype[local - 'a'] != M_VAX)
# endif
# ifdef CORY
	if(machtype[local - 'a'] != M_CORY)
# endif
# ifdef INGVAX
	if(machtype[local - 'a'] != M_INGRES)
# endif
# ifdef ING70
	if(machtype[local - 'a'] != M_INGRES)
# endif
# ifdef CC
	if(machtype[local -'a'] != M_CC && machtype[local - 'a'] != M_SRC)
# endif
		err("Machine type disagrees with local machine\n");
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
