/*******************************************************************
*                                                                  *
*    File: CIFPLOT/main.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "scanner.h"
#include "alloc.h"

IMPORT InitInput();
IMPORT InitError();
IMPORT InitInter();
IMPORT InitText();
IMPORT yyparser();
IMPORT string Concat();
IMPORT ErrorSummary();
IMPORT AllocSummary();
IMPORT InterSummary();
IMPORT float PlotSize();
IMPORT float PlotScale();
IMPORT transform *MakeTransform();
IMPORT transform *Rotate();
IMPORT point *MakePoint();

extern int FatalError;

main(n,v)
int n;
char **v;
{
    int b;
    char ch;
    InitGet();
    InitError();
    InitInter();
    b = options(1,n,v);
    if(extractor) InitExtractor();
    InitInput(b,n,v);
    InitText();
    if(parser()) 
	Error("Unrecoverable Syntax Error-Quit\n",FATAL);
    Summary();
    if(FatalError) exit(1);
    if(output == NOPLOT) Intermediate();
    InitFill();
    FixScale();
    AdjustWindow();
    InitPlotter();
    Grid();
    fprintf(stderr,"Window %d %d %d %d\n",(int) GWindow.xmin,(int) GWindow.xmax,
				  (int) GWindow.ymin,(int) GWindow.ymax);
    fprintf(stderr,"Scale: 1 micron is %f inches\n",scale);
    if(MoreToPlot == 0)
        fprintf(stderr,"The plot will be %4.2f feet\n",PlotSize());
      else
        fprintf(stderr,"The plot will be %d pages, %4.2f feet each\n",
		MoreToPlot+1,PlotSize());
    if(ask) {
    	fprintf(stderr,"Do you want a plot?  ");
    	ch = getchar();
    	if(!(ch == 'y' || ch == 'Y' || ch == EOF)) exit(0);
    	if(ch == EOF) {
		fprintf(stderr,"\n");
		}
	}
    if(extractor) OpenExtractor();
    Plot();
    while(MoreToPlot) {
	AdjustWindow();
	ReStartPlotter();
	Grid();
	Plot();
	}
    if(debug) {
	AllocSummary();
    	SortSummary();
	CheckUnAct();
	}
    if(extractor) Extract();
    }

options(b,n,v)
int b,n;
char **v;
{
    programName = "cifplot*";
    debug = 0;
    depth = 0;
    list = 0;
    standard = 0;
    banner = "";
    outline = 1;
    SetScale = 0;
    plot = 1;
    extractor = 0;
    baseName = "";
    symbox = 1;
    circle = 8;
    RetCmt = 1;
    text = 1;
    printSymbolName = 1;
    grid = 0;
    ask = 1;
    output = VARIAN;
    NoPixcels = 2000;
    GlobalTransform = MakeTransform();
    while (v[b][0] == '-') {
	switch(v[b][1]) {
		case 'a':
			if('0' <= v[b][2] && v[b][2] <= '9') {
				sscanf(&v[b][2],"%d",&circle);
				if(circle < 4) circle = 8;
			  	}
			      else {
				sscanf(v[++b],"%d",&circle);
				if(circle < 4) circle = 8;
				}
			break;
		case 'B':
			symbox = 0;
			break;
		case 'b':
			banner = v[++b];
			if(b >= n) {
				fprintf(stderr,"Banner must appear after the -b option\n");
				abort();
				}
			break;
		case 'C':
			RetCmt = 0;
			break;
		case 'd':
			if(v[b][2] != '\0') depth = atoi(&v[b][2]);
				else depth = atoi(v[++b]);
			break;
		case 'D':
			debug = 1;
			if(v[b][2] != '\0') debug += v[b][2] - '0';
			break;
		case 'f':
			outline = 0;
			break;
		case 'F':
			if(v[b][2] != '\0')
				fontfile = &(v[b][2]);
			    else
				fontfile = v[++b];
			break;
		case 'g':
			if(v[b][2] != '\0')
				grid = atoi(&(v[b][2]));
			    else
				grid = atoi(v[++b]);
			break;
		case 'H':
			output = HP2648A;
			break;
		case 'i':
			circle = 0;
			break;
		case 'I':
			ask = 0;
			break;
		case 'l':
			if(v[b][2] != '\0') Invisible(&v[b][2]);
				else Invisible(v[++b]);
			break;
		case 'L':
			list = 1;
			break;
		case 'r':
		   {    transform *temp;
			point pt;
			pt.x = 0;   pt.y = -1;
			temp = Rotate(&pt,GlobalTransform);
			FreeTransform(GlobalTransform);
			GlobalTransform = temp;
			}
			break;
		case 's':
			SetScale = 1;
			if(v[b][2] != '\0') {
			    if(EOF == sscanf(&(v[b][2]),"%f",&scale)) {
				fprintf(stderr,"bad format on -s option\n");
				abort();
				}
			    }
			else if(EOF == sscanf(v[++b],"%f",&scale)) {
				fprintf(stderr,"bad format on -s option\n");
				abort();
				}
			break;
		case 't':
			text = 0;
			break;
		case 'e':
			standard = 1;
			break;
		case 'O':
			output = NOPLOT;
			if(v[b][2] != '\0') outfile = &(v[b][2]);
				else outfile = v[++b];
			if(b >= n) {
				fprintf(stderr,"file name must appear after -O option\n");
				abort();
				}
			break;
		case 'p':
			if(v[b][2] != '\0') {
				readpat(&(v[b][2]));
				break;
				}
			if(b+2 >= n) {
				fprintf(stderr,"usage: %s -p file.stipple file.cif\n",v[0]);
				abort();
				}
			readpat(v[++b]);
			break;
		case 'P':
			output = LINEPRINTER;
			break;
		case 'S':
			output = VSPOOL;
			break;
		case 'V':
			output = VARIAN;
			break;
		case 'w':
			if(b+5 >= n) {
				fprintf(stderr,"usage: %s -w xmin xmax ymin ymax file.cif\n",v[0]);
				abort();
				}
			GWindow.xmin = atoi(v[++b]);
			GWindow.xmax = atoi(v[++b]);
			GWindow.ymin = atoi(v[++b]);
			GWindow.ymax = atoi(v[++b]);
			if(GWindow.xmin > GWindow.xmax) {
				fprintf(stderr,"error: xmin must be less than xmax\n");
				abort();
				}
			if(GWindow.ymin > GWindow.ymax) {
				fprintf(stderr,"error: ymin must be less than ymax\n");
				abort();
				}
			break;
		case 'W':
			output = VERSATEC;
			break;
		case 'n':
			plot = 0;
			break;
		case 'X':
			plot = 0;
			extractor = 1;
			SetScale = 1;
			scale = 0.02;
			baseName = v[++b];
			break;
		default:
			fprintf(stderr,"%s: Unknown option-Ignored\n",v[b]);
			break;
		}
	b++;
	}
    file1 = Concat(v[b],0);
    return(b);
    }

Summary()
{
    ErrorSummary();
    if(debug) AllocSummary();
    InterSummary();
    }
