/*******************************************************************
*                                                                  *
*    File: CIFPLOT/fonts.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include <vfont.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "out_structs.h"
#include "alloc.h"

IMPORT char *Concat();
IMPORT alloc();
IMPORT Error();

FORWARD real TCompare();

struct header header;
struct dispatch dispatch[256];
char *bits;

char *fontfile = "R.6";

InitText()
{
    char *s;
    int fonts;
    int i;

    /* Get the font file */
    s = Concat("/usr/lib/vfont/",fontfile,0);
    if((fonts = open(s,0)) == -1) {
	perror(s);
	Error("Can't get font file",RUNTIME);
	}
    /* Get the header and check magic number */
    if(read(fonts,&header,sizeof(header)) != sizeof(header)) {
	perror(s);
	Error("Bad read in font file",RUNTIME);
	}
    if(header.magic != 0436) {
	Error("Bad magic numer in font file",RUNTIME);
	}
    /* Get dispatches */
    if(read(fonts,dispatch,sizeof(dispatch)) != sizeof(dispatch)) {
	perror(s);
	Error("Bad read in font file",RUNTIME);
	}
    /* Allocate space for bit map and read in bits */
    bits = (char *) alloc(header.size);
    if(read(fonts,bits,header.size) != header.size) {
	perror(s);
	Error("Can't read bit map in font file",RUNTIME);
	}
    /* Close font file */
    if(close(fonts) != 0) {
	perror(s);
	Error("Can't close font file",RUNTIME);
	}
    /* Find the possible range up and down in which the text fits */
    TextUp = TextDown = 0;
    for(i=0;i<256;i++) {
	if(dispatch[i].nbytes && TextUp < dispatch[i].up)
		TextUp = dispatch[i].up;
	if(dispatch[i].nbytes && TextDown < dispatch[i].down)
		TextDown = dispatch[i].down;
	}
    }
    
DrawText(s,i,ypos,xpos)
char *s;
int i,ypos,xpos;
{
    if(!text) return;
    /* For all characters in string call DrawChar offset each character
     * by the width of the previous one */
    for(;*s;s++) {
	ypos += DrawChar(*s,i,ypos,xpos);
	}
    }

int
DrawChar(ch,i,ypos,xpos)
char ch;
int i,ypos,xpos;
{
    int nbytes,j;
    char *addr;

    /* It white space return an offset */
    if(ch == ' ' || ch == '\t' ) return((int) dispatch['t'].width);

    blank(ypos-1,ypos+1+dispatch[ch].width,xpos);
    /* If the line lines in appropriate range fill in bits for character */
    if(-dispatch[ch].down < i && i <= dispatch[ch].up) {
        addr = bits+dispatch[ch].addr;
        nbytes = (dispatch[ch].right + dispatch[ch].left + 7)/8;
        addr += (dispatch[ch].up - i)*nbytes;
        for(j=0; j<nbytes; j++) 
	    match(addr[j],j*8 - dispatch[ch].left + ypos,xpos);
	}
    return((int) dispatch[ch].width);
    }

TextLength(s)
char *s;
{
    int len;
    len = 0;
    for(;*s;s++) 
	len += TextWidth(*s);
    return(len);
    }

TextWidth(ch)
char ch;
{
    return( (ch == ' ' || ch == '\t') ? dispatch['t'].width : dispatch[ch].width);
    }

ClipText(s,rx,ry,adjust)
char *s;
real rx,ry;
char adjust;
{
    int x,y;
    int len;
    char *p;
    TextStruct *t;

    if(!text) return;
    switch(adjust) {
	case 'l':
		/* Left adjust */
		x = CONVERT(rx);
		y = CONVERT(ry);
		break;
	case 'c':
	case 'C':
		/* Center adjust */
		x = CONVERT(rx) + TextUp/2;
		y = CONVERT(ry) - TextLength(s)/2;
		break;
	case 'r':
	case 'R':
		/* Right adjust */
		x = CONVERT(rx);
		y = CONVERT(ry) - TextLength(s);
		break;
	case 'S':
		/* Adjust for symbol name */
		if(!printSymbolName) return;
		x = CONVERT(rx) + TextUp + 7;
		y = CONVERT(ry) + 5;
		break;
	case 'T':
		/* Adjust for point name */
		x = CONVERT(rx) + TextUp+1;
		y = CONVERT(ry)+1;
		break;
	default:
		Error("Unknown text adjustment in ClipText",INTERNAL);
	}
    for(;*s && y < 0;s++) y += TextWidth(*s);
    len = y;
    for(p = s; *p && len < NoPixcels; p++) len += TextWidth(*p);
    *p = '\0';
    if(*s == '\0') return;
    t = GetTextStruct();
    t->str = s;
    t->xpos = x;	t->ypos = y;
    PutList(t,&TextList,TCompare);
    return;
    }

/*
testfont()
{
    int nbytes;
    char ch;
    char *addr;
    int i;
    int j;

    vopen();
    for(i = 40; i > -20; i--) {
	DrawText("abcDE2@q Now is the time for all good men to come to the aid ... !@#$%^&*()_+}{|~`':::;<>.,",i,13);
	}
    vclose();
    }

display(i)
int i;
{
    int k;
    for(k=0;k<8;k++) {
	if(i & 0x80) putchar('x');
		else putchar(' ');
	i = i << 1;
	}
    }
    */
