
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vcmd.h>
#include <signal.h>
#include <errno.h>

char * LOCK 	= "/usr/spool/vad/lock";
char * DEVICE	= "/dev/va0";
char * DAEMON	= "/usr/lib/vad";
char * NAME	= "Varian";

extern char *ctime(),*getlogin();

#define BUFSIZE	16384
char vpbuf[BUFSIZE];

extern int errno;
extern quit();

int plotmd [] = { VPLOT, 0, 0 };
int prtmd  [] = { VPRINT, 0, 0 };
int vp;
FILE *f;
struct stat stbuf;
long clock;
int running;
char *name;
char **argv;
int argc;
int offline = 0;

main(count,v)
int count;
char **v;
{
     int i,k;
     argv = v;
     argc = count;
     name = argv[1];

     if(argc < 2) {
	fprintf(stderr,"Usage: %s bit_file [banner [message]] [-W]\n",argv[0]);
	exit(1);
	}

     if( (f=fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"Can't open %s\n",argv[1]);
	exit(1);
	}
     
     for(i=0;i<16;i++)
	signal(i,quit);

     signal(SIGHUP,SIG_IGN);
     signal(SIGINT,SIG_IGN);

     if( '-' == argv[argc-1][0]) {
	switch(argv[argc-1][1]) {
		case 'W':
			LOCK   = "/usr/spool/vpd/lock";
			DEVICE = "/dev/vp0";
			NAME   = "Versatec";
			DAEMON = "/usr/lib/vpd";
			break;
		case 'V':
			break;
		default:
			fprintf(stderr,"%s: unknown option - quit\n",argv[argc-1]);
			quit(0);
		}
	argc--;
	}

     while( stat(LOCK, &stbuf) >= 0) {
	sleep(30);
	}

     if( creat(LOCK, 0666) < 0) {
	fprintf(stderr,"Can't create %s\n",LOCK);
	quit(0);
	}
     
     while(1) {
	if( (vp = open(DEVICE, 1)) >= 0) break;
	if( errno != EIO ) {
	    perror(DEVICE);
	    quit(0);
	    }
	if(offline == 0) {
	    fprintf(stderr,"%s is offline\n",NAME);
	    offline = 1;
	    }
	sleep(30);
	}

     /*
     if( (vp = open(DEVICE, 1)) < 0) {
	printf("Put the %s on line\n",NAME);
	for( k=0; k<24 && vp == NULL; k++) {
		sleep(10);
		vp = open(DEVICE,1);
		}
	if( vp == NULL ) {
		fprintf(stderr,"Can't open %s\n",NAME);
		unlink(LOCK);
		quit(0);
		}
	}
	*/

     ioctl(vp, VSETSTATE, prtmd);
     running = 1;

     clock = time(0);
     if(argc < 3)
	sprintf(vpbuf, "%s: %s\n",getlogin(),ctime(&clock));
      else
     	sprintf(vpbuf, "%s: %s%s\n",getlogin(),ctime(&clock),argv[2]);
     write(vp, vpbuf, BUFSIZE);

     ioctl( vp, VSETSTATE, plotmd);
     while( (i=read(fileno(f),vpbuf, BUFSIZE)) > 0)
		write(vp,vpbuf,i);
     quit(-1);
     }

quit(n)
int n;
{
    if(running) {
	ioctl(vp, VSETSTATE, prtmd);
	write(vp, "\f\f", 2);
	}
    close(vp);
    unlink(LOCK);
    unlink(name);
    if(n < 0) {
    	if(argc > 2)  fprintf(stderr,"%s\n",argv[3]);
	}
      else fprintf(stderr,"Plotting Aborted\n");
    execl( DAEMON, DAEMON, 0 );
    exit(0);
    }
