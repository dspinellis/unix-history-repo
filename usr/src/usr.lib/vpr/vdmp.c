/*  VDMP: version 4.4				updated 7/12/81
 *
 *  reads raster file created by cifplot and dumps it onto the
 *  Varian or Versatec plotter.
 *  Must be called with vcontrol or by vpd/vad daemon since
 *  it assumes plotter is already opened as device 3.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/vcmd.h>

#define MAGIC_WORD	0xA5CF4DFA

#define BUFSIZE		1024*128
#define BLOCK		1024

extern char *ctime();
extern long time();

char *Sid = "@(#)vdmp.c	4.4\t7/12/81";
int	plotmd[]	= { VPLOT, 0, 0};
int	prtmd[]		= { VPRINT, 0, 0};
char *name = "";
char *banner = "";

int inbuf[BLOCK/sizeof(int)];
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

	for(b=0; argv[1][0] == '-';b++) {
	    switch(argv[1][1]) {
		case 'W':
			device = VERSATEC;
			BytesPerLine = 880;
			break;
		case 'V':
			device = VARIAN;
			BytesPerLine = 264;
			break;
		case 'n':
			argc--; argv++;
			if(argv[1] != 0)
				name = argv[1];
			break;
		case 'b':
			argc--; argv++;
			if(argv[1] != 0)
				banner = argv[b];
			break;
		}
	    argc--; argv++;
	    }
	if(argc < 2) exit(-1);
	/* page feed */
	if(device == VARIAN) {
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,"\f",2);
	    }
	if(device == VERSATEC) {
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n",16);
	    }
	/* open dump file */
	in = open(argv[1], 0);
	if(in == NULL) {
	    char str[128];
	    sprintf(str,"%s: No such file\n\n\n",argv[1]);
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,str, strlen(str));
	    exit(-1);
	    }
	/* write header */
	{   char str[512];
	    long clock;
	    clock = time(0);
	    sprintf(str,"%s:%s%s",name,ctime(&clock),banner);
	    ioctl(3, VSETSTATE,prtmd);
	    write(3,str,(strlen(str)+1) & 0xfffffffe); /*makes strlen even*/
	    }
	/* open file for reading */
	b=read(in,inbuf,BLOCK);
	if(inbuf[0] == MAGIC_WORD && b == BLOCK) {
	    /* we have a formatted dump file */
	    inbuf[(BLOCK/sizeof(int))-1] = 0;  /* make sure string terminates */
	    write(3,&(inbuf[4]),(strlen(&(inbuf[4]))+1) & 0xfffe);
	    ioctl(3, VSETSTATE,prtmd);
	    write(3," \n",2);
	    putplot();
	    close(in);
	    }
	  else { 			/* dump file not formatted */
	    /* reset in's seek pointer and plot */
	    close(in);
	    in = open(argv[1], 0);
	    putplot();
	    close(in);
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
		if(write(3,buf,i)!=i) exit(1);
    }
