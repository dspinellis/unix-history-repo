
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
IMPORT char *getlogin();
IMPORT char *ctime();
IMPORT time();

#define NOLINES	32

#define VLIMIT  2112
#define VMAX	2000
#define VEXTRA	2100
#define VMIN	0
#define VRESOL	200.0

#define WLIMIT	7040
#define	WMAX	7000
#define	WEXTRA	7030
#define WMIN	0
#define WRESOL	200.0

#define HLIMIT  704
#define HMAX	700
#define HMIN	0
#define HRESOL  10.0

#define LOCK   "/usr/spool/vpd/lock"
#define DEVICE "/dev/vp0"
#define NAME   "Versatec"
#define DAEMON "/usr/lib/vpd"
#define VDUMP	"/vb/grad/fitz/cif/newprogs/vdump"

int xmax,xlimit,xmin;
real resolution;
char *heading;

#define FromMask	( 0xffffff00 | (0xff >> (from & 0x07))) << (from & 0x18)

#define ToMask		(~((0xffffff00 | (0xff >> (to & 0x07))) << (to & 0x18)))

char *outfile;
int filedesc;
int *OutBuf,*ScanLine;
int NoLine;


main() {
    int from,to;

    printf("int fromMask[32] = {\n");
    for(from=0; from<32;from++)
	printf("\t0x%x,\n",FromMask);
    printf("\t};\n\n");


    printf("int toMask[32] = {\n");
    for(to=0; to<32;to++)
	printf("\t0x%x,\n",ToMask);
    printf("\t};\n\n");
    }
