/*  VDMP: version 4.2				updated 4/23/81
 *
 *  reads raster file created by cifplot and dumps it onto the
 *  Varian or Versatec plotter.
 *  Must be called with vcontrol or by vpd/vad daemon since
 *  it assumes plotter is already opened as device 3.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/vcmd.h>

#define BUFSIZE		16384

extern char *ctime();
extern long time();

char *Sid = "@(#)vdmp.c	4.2\t4/23/81";
int	plotmd[]	= { VPLOT, 0, 0};
int	prtmd[]		= { VPRINT, 0, 0};
char *name = "";
char *banner = "";

char vpbuf[BUFSIZE];

int	in;

#define VARIAN	1
#define VERSATEC 2

int device = VARIAN;	/* Indicate which device */
int BytesPerLine = 264;	/* Number of bytes per raster line of the output device */

main(argc, argv)
char **argv;
{
	extern int onintr();
	int b;

	for(b=1; argv[b][0] == '-';b++) {
	    switch(argv[b][1]) {
		case 'W':
			device = VERSATEC;
			BytesPerLine = 880;
			break;
		case 'V':
			device = VARIAN;
			BytesPerLine = 264;
			break;
		case 'n':
			if(argv[++b] != 0)
				name = argv[b];
			break;
		case 'b':
			if(argv[++b] != 0)
				banner = argv[b];
			break;
		}
	    }
	/* page feed */
	if(device == VARIAN) {
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,"\f",2);
	    }
	if(device == VERSATEC) {
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n",16);
	    }
	/* write header */
	{
	    char str[512];
	    long clock;
	    clock = time(0);
	    sprintf(str,"%s:%s%s\n \0",name,ctime(&clock),banner);
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,str,strlen(str) & 0xfffffffe); /*makes strlen even*/
	    }
	while (argc>b) {
		in = open(argv[b++], 0);
		if(in == NULL) {
		    char str[128];
		    sprintf(str,"%s: No such file\n\n\n",argv[b-1]);
		    ioctl(3, VSETSTATE,prtmd);
		    write(3,str, strlen(str));
		    exit(-1);
		    }
		  else putplot();
		  }
	if(device == VERSATEC) {
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,"\n\n\n\n\n\n\n",8);
	    }
	exit(0);
	}


putplot()
{
     register int i;
     register char *buf;

     buf = &(vpbuf[0]);
     /* vpd has already opened the Versatec as device 3 */
     ioctl(3, VSETSTATE, plotmd);
     while( (i=read(in,buf, BUFSIZE)) > 0)
		write(3,buf,i);
    }
