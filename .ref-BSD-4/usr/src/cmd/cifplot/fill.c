/*******************************************************************
*                                                                  *
*    File: CIFPLOT/fill.c                                          *
*    Written by Dan Fitzpatrick & Dennis Wellborne                 *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "out_structs.h"
#include "masks.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/vcmd.h>
#include <signal.h>
#include <errno.h>

extern int errno;

int plotmd [] = { VPLOT, 0, 0 };
int prtmd  [] = { VPRINT, 0, 0 };
struct stat stbuf;
int offline;
int Lock = 0;
char *tail = "a";	/* letter appended to temp file name for uniqueness */
int NormalSize;
double StartNextPlot;

IMPORT char *Concat();
IMPORT char *mktemp();
IMPORT char *getlogin();
IMPORT char *ctime();
IMPORT time();

#define VLIMIT  2112
#define VMAX	2000
#define VEXTRA	2100
#define VMIN	0
#define VRESOL	200.0
#define VSPLFL	"/usr/spool/vad/dfcXXXXXX"
#define VDAEMON "/vb/grad/fitz/graphics/vad"

#define WLIMIT	7040
#define	WMAX	7000
#define	WEXTRA	7030
#define WMIN	0
#define WRESOL	200.0
#define WSPLFL	"/usr/spool/vpd/dfcXXXXXX"
#define WDAEMON "/vb/grad/fitz/graphics/vpd"

#define HLIMIT  704
#define HMAX	700
#define HMIN	0
#define HRESOL  10.0

#define LOCK   "/usr/spool/vpd/lock"
#define DEVICE "/dev/vp0"
#define NAME   "Versatec"
#define DAEMON "/vb/grad/fitz/graphics/vpd"
#define VDUMP	"/usr/ucb/vdump"
#define VDMP	"/vb/grad/fitz/graphics/vdmp"

int xmax,xlimit,xlimitDiv32,xmin;
real resolution = 200;
char *heading,*name;

#ifdef  SHIFT
#define FROMMASK(from)	( 0xffffff00 | (0xff >> (from & 0x07))) << (from & 0x18)

#define TOMASK(to)		(~((0xffffff00 | (0xff >> (to & 0x07))) << (to & 0x18)))
#else
#define FROMMASK(from)  fromMask[from&0x1f]
#define TOMASK(to)      toMask[to&0x1f]
#endif
#define DOTMASK(x)	dotMask[x&0x1f]

char *outfile;
int filedesc;
int *OutBuf,*ScanLine;
int NoLine;


Fill(line,from,to,pattern)
int from,to;
register int pattern;
{
    int start,end;
    register int i;

    to++;
    if(to < from || xlimit < to || from < 0) { 
	/* fprintf(stderr,"Out of range in fill(%d,%d)\n",from,to); */
	return;
	}
    start = (from >> 5) + ((line - Bottom) % OUT_BUF_SIZE)*xlimitDiv32;
    end = (to >> 5) + ((line - Bottom) % OUT_BUF_SIZE)*xlimitDiv32;

    if (start == end)
		OutBuf[start] |= pattern & (FROMMASK(from)) & (TOMASK(to));
	else {
		OutBuf[start] |= pattern & (FROMMASK(from));

		for (i=start+1; i<end;)
			OutBuf[i++] |= pattern;
		
		OutBuf[end] |= pattern & TOMASK(to);
		}
}

#define ORDER(x,y)	if(x > y) { temp=x; x=y;y=temp;}

FillTrap(xLeft,xRight,deltaxLeft,deltaxRight,yBottom,yTop,patNo)
real xLeft,xRight,deltaxLeft,deltaxRight;
int yTop,yBottom;
int patNo;
{
    register int xStart,xEnd,from,to;
    register int i,pattern;
    int y,yStart,yEnd;
    int n,temp;

    if(yBottom > yTop) Error("yBottom > yTop in FillTrap",INTERNAL);
    yStart = (yBottom - Bottom) % OUT_BUF_SIZE;
    yEnd = (yTop - yBottom) + yStart;
    if(yEnd > OUT_BUF_SIZE || yStart < 0) Error("Trap bigger than buf size",INTERNAL);

    if(!(deltaxLeft == 0.0 && deltaxRight ==  0.0)) {
	for(y=yStart; y<yEnd; y++) {
	    from = xLeft;
	    to = xRight+1;
	    if(to < from || xlimit < to || from < 0) { 
		/* fprintf(stderr,"Out of range in fill(%d,%d)\n",from,to); */
		return;
		}
	    xStart = (from >> 5) + (y*xlimitDiv32);
	    xEnd = (to >> 5) + (y*xlimitDiv32);
	    pattern = Pats[patNo][y%NO_PAT_LINE];
	    if(pattern != 0) {
		if(xStart == xEnd)
		    OutBuf[xStart] |= pattern & (FROMMASK(from)) & (TOMASK(to));
		  else {
		    OutBuf[xStart] |= pattern & (FROMMASK(from));
		    for(i=xStart+1; i<xEnd;)
			OutBuf[i++] |= pattern;
		    OutBuf[xEnd] |= pattern & (TOMASK(to));
		    }
		}
	    if(outline || pattern == 0xffffffff) {
		to = xLeft + deltaxLeft;
		ORDER(from,to); to++;
		xStart = (from >> 5) + (y*xlimitDiv32);
		n = (to >> 5) + (y*xlimitDiv32);
		if(xStart == n)
		    OutBuf[xStart] |= 0xffffffff & (FROMMASK(from)) & (TOMASK(to));
		  else {
		    OutBuf[xStart] |= 0xffffffff & (FROMMASK(from));
		    for(i=xStart+1; i<n;)
			OutBuf[i++] |= 0xffffffff;
		    OutBuf[n] |= 0xffffffff & (TOMASK(to));
		    }
		to = xRight;
		from = xRight + deltaxRight;
		ORDER(from,to); to++;
		n = (from >> 5) + (y*xlimitDiv32);
		xEnd = (to >> 5) + (y*xlimitDiv32);
		if(n == xEnd)
		    OutBuf[n] |= 0xffffffff & (FROMMASK(from)) & (TOMASK(to));
		  else {
		    OutBuf[n] |= 0xffffffff & (FROMMASK(from));
		    for(i=n+1; i<xEnd;)
			OutBuf[i++] |= 0xffffffff;
		    OutBuf[xEnd] |= 0xffffffff & (TOMASK(to));
		    }
		}
	    xLeft += deltaxLeft;
	    xRight += deltaxRight;
	    }
	}
      else {
	from = xLeft;
	to = xRight+1;
	if(to < from || xlimit < to || from < 0) { 
	    /* fprintf(stderr,"Out of range in fill(%d,%d)\n",from,to); */
	    return;
	    }
	xStart = (from >> 5) + (yStart*xlimitDiv32);
	xEnd = (to >> 5) + (yStart*xlimitDiv32);
	for(y=yStart; y<yEnd; y++) {
	    pattern = Pats[patNo][y%NO_PAT_LINE];
	    if(pattern != 0) {
		if(xStart == xEnd)
		    OutBuf[xStart] |= pattern & (FROMMASK(from)) & (TOMASK(to));
		  else {
		    OutBuf[xStart] |= pattern & (FROMMASK(from));
		    for(i=xStart+1; i<xEnd;)
			OutBuf[i++] |= pattern;
			OutBuf[xEnd] |= pattern & (TOMASK(to));
		    }
		}
	    if(outline || pattern == 0xffffffff) {
		OutBuf[xStart] |= DOTMASK(from);
		OutBuf[xEnd] |= DOTMASK(to);
		}
	    xEnd += xlimitDiv32;
	    xStart += xlimitDiv32;
	    }
	}
    }

blank(from,to,line)
int from,to;
{
    int start,end;
    register int i;

    to++;
    if(to < from || xlimit < to || from < 0) { 
	/* fprintf(stderr,"Out of range in blank(%d,%d)\n",from,to); */
	return;
	}
    start = (from >> 5) + ((line - Bottom) % OUT_BUF_SIZE)*xlimitDiv32;
    end = (to >> 5) + ((line - Bottom) % OUT_BUF_SIZE)*xlimitDiv32;

    if (start == end)
		OutBuf[start] &= ~((FROMMASK(from)) & (TOMASK(to)));
	else {
		OutBuf[start] &= ~(FROMMASK(from));

		for (i=start+1; i<end;)
			OutBuf[i++] = 0;
		
		OutBuf[end] &=  ~(TOMASK(to));
		}
}

match(pat,pos,line)
int pat,pos,line;
{
    int pat1,pat2,offset;

    line = (line-Bottom) % OUT_BUF_SIZE;
    offset = (line*xlimitDiv32);
    pat &= 0xff;
    /*
    pat1 = ((0xff >> (pos & 07)) & 0xff) << (pos & 0x18);
    OutBuf[(pos>>5)+offset] &= ~pat1;
    */
    pat1 = ((pat >> (pos & 07)) & 0xff) << (pos & 0x18);
    OutBuf[(pos>>5)+offset] |= pat1;
    pos += 8;
    /*
    pat2 = ((0xff00 >> (pos & 07)) & 0xff) << (pos & 0x18);
    OutBuf[(pos>>5)+offset] &= ~pat2;
    */
    pat = pat << 8;
    pat2 = ((pat >> (pos & 07)) & 0xff) << (pos & 0x18);
    OutBuf[(pos>>5)+offset] |= pat2;
    }

/*
ClearLine(){
    register int i,limit;
    limit = (OUT_BUF_SIZE*xlimit)/32;
    for(i=0; i<limit ; OutBuf[i++] = 0);
    }
    */

Clear(lp, nbytes)
	int *lp;
	int nbytes;
{

	asm("movc5 $0,(sp),$0,8(ap),*4(ap)");

}

DumpBuf(x)
int x;
{
    int i;
    for(i=xLast; i < x; i++) Text(i);
    if(plot) {
	if(write(filedesc, OutBuf, (OUT_BUF_SIZE*xlimit)/8) != (OUT_BUF_SIZE*xlimit/8)) {
	    perror(outfile);
	    if(output != VERSATEC)
	    	Error("Bad Write on temp file",RUNTIME);
	      else
		Error("Looks like someone turned off the Versatec",RUNTIME);
	    }
	Clear(OutBuf,(OUT_BUF_SIZE*xlimit)/8);
	xLast = x;
	}
    }

/*
DumpLine()
{
    NoLine++;
    if(NoLine == OUT_BUF_SIZE) {
       if(plot)
    	if(write( filedesc, OutBuf, (OUT_BUF_SIZE*xlimit)/8) != (OUT_BUF_SIZE*xlimit/8)) {
	    perror(outfile);
	    if(output != VERSATEC)
	    	Error("Bad Write on temp file",RUNTIME);
	      else
		Error("Looks like someone turned off the Versatec",RUNTIME);
	    }
    	Clear(OutBuf,(OUT_BUF_SIZE*xlimit)/8);
	NoLine = 0;
	}
    ScanLine = &(OutBuf[(NoLine*xlimit)/32]);
    }
    */

FlushLine()
{
    NoLine++;
    if(plot)
      if(write( filedesc, &(OutBuf[0]), (NoLine*xlimit)/8) != (NoLine*xlimit)/8) {
        perror(outfile);
        Error("Bad Write on temp file",RUNTIME);
        }
    Clear(OutBuf,(OUT_BUF_SIZE*xlimit)/8);
    NoLine = 0;
    ScanLine = OutBuf;
    }

InitFill(){
    switch(output) {
	case VARIAN:
		xlimit = VLIMIT;
		NoPixcels = NormalSize = VMAX;
		xmax = VEXTRA;
		xmin = VMIN;
		resolution = VRESOL;
		break;
	case VSPOOL:
	case VERSATEC:
		xlimit = WLIMIT;
		NoPixcels = NormalSize = WMAX;
		xmax = WEXTRA;
		xmin = WMIN;
		resolution = WRESOL;
		break;;
	case HP2648A:
		xlimit = HLIMIT;
		NoPixcels = NormalSize = xmax = HMAX;
		xmin = HMIN;
		resolution = HRESOL;
		break;;
	default:
		Error("Unknown output device in InitFill",INTERNAL);
	}
    OutBuf = (int *) alloc((int) ((OUT_BUF_SIZE * xlimit)/8));
    ScanLine = OutBuf;
    NoLine = 0;
    xlimitDiv32 = xlimit/32;
    }


vopen()
{
    char s[256];
    int i;
    long clock;
  
    name = getlogin();
    if(Window.ymax != Window.ymin)	/* Check for divide by zero */
    sprintf(s,"%s Window: %d %d %d %d --- Scale: 1 micron is %f inches",
	programName,(int)Window.xmin,(int)Window.xmax,
	(int)Window.ymin,(int)Window.ymax,scale);
    heading = Concat(s,"\n",banner,0);


    if((output==VARIAN || output==HP2648A) && plot) {
   	outfile = (char *) mktemp("/usr/tmp/cifXXXXXX");
	outfile = Concat(outfile,tail,0);
	*tail += 1;	/* insures uniqueness */
   	if((filedesc = creat(outfile,0666)) < 0) {
		perror(outfile);
		Error("Can't create temp file",RUNTIME);
		}
    	fileopen = 1;
	}

    if(output==VSPOOL && plot) {
   	outfile = (char *) mktemp("/usr/tmp/cifXXXXXX");
	outfile = Concat(outfile,tail,0);
	*tail += 1;	/* insures uniqueness */
   	if((filedesc = creat(outfile,0666)) < 0) {
		perror(outfile);
		Error("Can't create temp file",RUNTIME);
		}
    	fileopen = 1;
	}

     if(output==VERSATEC && plot) {
	/* Drive the Versatec directly */
     	while( stat(LOCK, &stbuf) >= 0) {
		sleep(30);
		}

     	if( (i = creat(LOCK, 0666)) < 0) {
		perror(LOCK);
		Error("Can't create lock file",RUNTIME);
		}
	 if( close(i) < 0) {
		perror(LOCK);
		Error("Can't close lock file",RUNTIME);
		}
    	 
	Lock = 1;
     	while(1) {
		if( (filedesc = open(DEVICE, 1)) >= 0) break;
		if( errno != EIO ) {
	    		perror(DEVICE);
			Error("I/O Error",RUNTIME);
	    		}
		if(offline == 0) {
	    		fprintf(stderr,"%s is offline\n",NAME);
	    		offline = 1;
	    		}
		sleep(30);
		}
    	fileopen = 1;
	fprintf(stderr,"Plotting\n");
	ioctl(filedesc,VSETSTATE,prtmd);
	clock = time(0);
	sprintf(s,"%s: %s",getlogin(),ctime(&clock));
	if(write(filedesc,s,LengthString(s)) != LengthString(s) ||
	   write(filedesc,heading,LengthString(heading)) != LengthString(heading)) {
		perror("");
		Error("Can't write on Versatec",RUNTIME);
		}
	write(filedesc,"\n\n",2);
	ioctl(filedesc,VSETSTATE,plotmd);
        }
    }

vclose()
{
    int i;
    fileopen = 0;
    FlushLine();
    if(plot && close(filedesc) < 0) {
	perror("outfile");
	Error("Can't close temp file",RUNTIME);
	}

    if(output==VERSATEC) {
	ioctl(filedesc,VSETSTATE,prtmd);
	for(i=0; i < 10; i++)
	    write(filedesc,"\n\n\n\n\n\n\n\n\n\n",10);
	unlock();
	Lock = 0;
	fprintf(stderr,"Plotting Done\n");
	}

    if(plot && (output==VARIAN)) /* || output==VSPOOL)) */
  	if(vfork() == 0)
	    if(! 1) {
		char *tmpFile;
		char *spoolFile;
		FILE *tmp;
		tmpFile = mktemp("/usr/tmp/cpltXXXXXX");
		if(output==VARIAN)
		    spoolFile = mktemp(VSPLFL);
		  else
		    spoolFile = mktemp(WSPLFL);
		spoolFile = Concat(spoolFile,tail,0);
		if((tmp = fopen(tmpFile,"w")) == NULL) {
		    perror(tmpFile);
		    Error("Can't open temp spool file",RUNTIME);
		    }
		fprintf(tmp,"L%s\n",name);
		fprintf(tmp,"B%s\n",heading);
		fprintf(tmp,"B%s\n",banner);
		fprintf(tmp,"C%s\n",outfile);
		fprintf(tmp,"U%s\n",outfile);
		fclose(tmp);
		link(tmpFile,spoolFile);
		unlink(tmpFile);
		if(output==VARIAN)
			execl(VDAEMON,"vad",0);
		    else
			execl(WDAEMON,"vpd",0);
		Error("Can't start up daemon",RUNTIME);
		}
	      else {
		if(execl(VDUMP,"vdump",outfile,heading,"Plotting Done",0) < 0) {
		    perror(VDUMP);
		    Error("Can't start up plotting routine",RUNTIME);
		    }
		}

    if(plot && output==VSPOOL)
  	if(vfork() == 0)
	    if(!debug) {
		if(execl(VDUMP,"vdump",outfile,heading,"","-W",0) < 0) {
		    perror(VDUMP);
		    Error("Can't start up plotting routine",RUNTIME);
		    }
		}
	      else {
		if(execl(VDUMP,"vdump",outfile,heading,"Plotting Done","-W",0) < 0) {
		    perror(VDUMP);
		    Error("Can't start up plotting routine",RUNTIME);
		    }
		}
  }

unlock()
{
    if(output==VERSATEC && Lock) {
    	if(unlink(LOCK) < 0) {
		perror(LOCK);
		Error("Can't remove lock",RUNTIME);
		}
	if(vfork()==0) {
	    if(execl(DAEMON,DAEMON) < 0) {
  	    	perror(DAEMON);
  	    	Error("Can't start up daemon",RUNTIME);
	    	}
	    }
	}
    }

float
PlotSize()
{
    return(((float) (Top-Bottom))/(resolution * 12.0));
    }

float
PlotScale()
{
   if(Window.ymax == Window.ymin) return(0.0);
   return((100.0 * ((real) xmax))/(resolution * (Window.ymax-Window.ymin)));
   }

FixScale()
/* Set up the correct conversion factor for chip to plotter conversion*/
{
    if(SetScale && scale != 0.0) {
	ConvertFactor = (scale * resolution)/100.0;
	StartNextPlot = GWindow.ymin;
	NoPixcels = xmax;
	}
      else {
   	if(GWindow.ymax == GWindow.ymin) {
	    ConvertFactor = 0.0;
	    scale = 0.0;
	    }
	  else {
	    ConvertFactor = NormalSize/(GWindow.ymax - GWindow.ymin);
	    scale = 100.0 * ((real) NoPixcels)/(resolution * (GWindow.ymax-GWindow.ymin));
	    }
	StartNextPlot = GWindow.ymin;
	}
    }

AdjustWindow()
{
	Window.xmin = GWindow.xmin;
	Window.xmax = GWindow.xmax;
	Window.ymin = StartNextPlot;
	Window.ymax = NoPixcels/ConvertFactor + Window.ymin;
	StartNextPlot = NormalSize/ConvertFactor + Window.ymin;
	if(Window.ymax < GWindow.ymax-1) {
	    MoreToPlot = ((GWindow.ymax - Window.ymax)*ConvertFactor)/NormalSize + 0.9999;
	    }
	  else {
	    MoreToPlot = 0;
	    Window.ymax = GWindow.ymax;
	    NoPixcels = CONVERT(Window.ymax) + 1;
	    }
    }

