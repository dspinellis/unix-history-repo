/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)rain.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <sgtty.h>
#include <signal.h>
/* rain 11/3/1980 EPS/CITHEP */
/* cc rain.c -o rain -O -ltermlib */
#define cursor(col,row) tputs(tgoto(CM,col,row),1,outc)
outc(c)
{
	putchar(c);
}
extern char *UP;
extern short ospeed;
struct sgttyb old_tty;
char *LL, *TE, *TI;
main(argc,argv)
int argc;
char *argv[];
{
    extern fputchar();
    char *malloc();
    char *getenv();
    char *tgetstr(), *tgoto();
    float ranf();
    int onsig();
    register int x, y, j;
    static int xpos[5], ypos[5];
    register char *CM, *BC, *DN, *ND;
    char *tcp;
    register char *term;
    char tcb[100];
    struct sgttyb sg;
    setbuf(stdout,malloc(BUFSIZ));
    if (!(term=getenv("TERM"))) {
	fprintf(stderr,"%s: TERM: parameter not set\n",*argv);
	exit(1);
    }
    if (tgetent(malloc(1024),term)<=0) {
	fprintf(stderr,"%s: %s: unknown terminal type\n",*argv,term);
	exit(1);
    }
    tcp=tcb;
    if (!(CM=tgetstr("cm",&tcp))) {
	fprintf(stderr,"%s: terminal not capable of cursor motion\n",*argv);
	exit(1);
    }
    if (!(BC=tgetstr("bc",&tcp))) BC="\b";
    if (!(DN=tgetstr("dn",&tcp))) DN="\n";
    if (!(ND=tgetstr("nd",&tcp))) ND=" ";
    TE=tgetstr("te",&tcp);
    TI=tgetstr("ti",&tcp);
    UP=tgetstr("up",&tcp);
    if (!(LL=tgetstr("ll",&tcp))) strcpy(LL=malloc(10),tgoto(CM,0,23));
    gtty(1, &sg);
    ospeed=sg.sg_ospeed;
    for (j=SIGHUP;j<=SIGTERM;j++)
	if (signal(j,SIG_IGN)!=SIG_IGN) signal(j,onsig);
    gtty(1, &old_tty);	/* save tty bits for exit */
    gtty(1, &sg);
    sg.sg_flags&=~(CRMOD|ECHO);
    stty(1, &sg);
    if (TI) fputs(TI,stdout);
    tputs(tgetstr("cl",&tcp),1,fputchar);
    fflush(stdout);
    for (j=5;--j>=0;) {
	xpos[j]=(int)(76.*ranf())+2;
	ypos[j]=(int)(20.*ranf())+2;
    }
    for (j=0;;) {
	x=(int)(76.*ranf())+2;
	y=(int)(20.*ranf())+2;
	cursor(x,y); fputchar('.');
	cursor(xpos[j],ypos[j]); fputchar('o');
	if (j==0) j=4; else --j;
	cursor(xpos[j],ypos[j]); fputchar('O');
	if (j==0) j=4; else --j;
	cursor(xpos[j],ypos[j]-1);
	fputchar('-');
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputs("|.|",stdout);
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputchar('-');
	if (j==0) j=4; else --j;
	cursor(xpos[j],ypos[j]-2); fputchar('-');
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputs("/ \\",stdout);
	cursor(xpos[j]-2,ypos[j]);
	fputs("| O |",stdout);
	cursor(xpos[j]-1,ypos[j]+1);
	fputs("\\ /",stdout);
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputchar('-');
	if (j==0) j=4; else --j;
	cursor(xpos[j],ypos[j]-2); fputchar(' ');
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputchar(' '); fputs(ND,stdout); fputchar(' ');
	cursor(xpos[j]-2,ypos[j]);
	fputchar(' '); fputs(ND,stdout); fputchar(' ');
	fputs(ND,stdout); fputchar(' ');
	cursor(xpos[j]-1,ypos[j]+1);
	fputchar(' '); fputs(ND,stdout); fputchar(' ');
	fputs(DN,stdout); fputs(BC,stdout); fputs(BC,stdout);
	fputchar(' ');
	xpos[j]=x; ypos[j]=y;
	fflush(stdout);
    }
}
onsig(n)
int n;
{
    struct sgttyb sg;
    fputs(LL, stdout);
    if (TE) fputs(TE, stdout);
    fflush(stdout);
    stty(1, &old_tty);
    kill(getpid(),n);
    _exit(0);
}
fputchar(c)
char c;
{
    putchar(c);
}
float ranf() {
    return((float)rand()/2147483647.);
}
