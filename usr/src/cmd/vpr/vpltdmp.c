/*  VPLTDMP: version 4.1				updated 2/12/81
 *
 *  reads raster file created by vplot and dumps it onto the
 *  Varian or Versatec plotter.
 *  Must be called with vcontrol or by vpd/vad daemon since
 *  it assumes plotter is already opened as device 3.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/vcmd.h>

#define NB	88
#define BSIZ	512

extern char *ctime();
extern long time();

char *Sid = "@(#)vpltdmp.c	4.1\t2/12/81";
int	plotmd[]	= { VPLOT, 0, 0};
int	prtmd[]		= { VPRINT, 0, 0};
char	blocks	[NB][BSIZ];
int	obuf[220*64];		/* output buffer; 64 lines of output */
char *name = "";
char *banner = "";

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
	/* write header */
	{
	    char str[512];
	    long clock;
	    clock = time(0);
	    sprintf(str,"%s:%s%s\n    ",name,ctime(&clock),banner);
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
		    }
		  else putpict();
		  }
	}

int	f; /* versatec file number */

putpict()
{
    register x, *ip, *op;
    int i,y;

    f = 3; /*vpd has already opened the Versatec as device 3 */
    ioctl(f, VSETSTATE, plotmd);
    lseek(in, 0L, 0);
    for (i=0; i<32; i++) {
	read(in, blocks[0], 32*BSIZ);
	for (y=0; y<64; y++) {
	    op = &(obuf[(y*BytesPerLine)/4]);
	    for (x=0; x<32; x++)  {
		ip = (int *)&blocks[x][y<<3];
		*op++ = *ip++;
		*op++ = *ip++;
		}
	    }
	write(f, (char *)obuf, BytesPerLine*64);
	}
    }
