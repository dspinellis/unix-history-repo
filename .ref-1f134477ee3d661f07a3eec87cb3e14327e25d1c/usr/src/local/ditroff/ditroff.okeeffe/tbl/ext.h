/*	@(#)ext.h	1.3 (CWI) 86/11/13	*/
/*	t..c	4.2	83/10/13	*/

/* t..c : external declarations */

#include <stdio.h>
#include <ctype.h>

extern int nlin, ncol, iline, nclin, nslin, qcol;

extern char oldname[];
extern int style[MAXHEAD][MAXCOL];
extern int ctop[MAXHEAD][MAXCOL];
extern char font[MAXHEAD][MAXCOL][2];
extern char csize[MAXHEAD][MAXCOL][4];
extern char vsize[MAXHEAD][MAXCOL][4];
extern char cll[MAXCOL][CLLEN];
extern int stynum[];
extern int F1, F2;
extern int lefline[MAXHEAD][MAXCOL];
extern int fullbot[];
extern char *instead[];
extern int expflg;
extern int ctrflg;
extern int evenflg;
extern int evenup[];
extern int boxflg;
extern int dboxflg;
extern int linsize;
extern int tab;
extern int pr1403;
extern int linsize, delim1, delim2;
extern int allflg;
extern int textflg;
extern int left1flg;
extern int rightl;
extern struct colstr *table[];
extern char *cspace, *cstore;
extern char *exstore, *exlim;
extern int sep[];
extern int used[], lused[], rused[];
extern int linestop[];
extern int leftover;
extern char *last, *ifile;
extern char texname;
extern int texct;
extern char texstr[];
extern int linstart;
extern FILE *tabin;
extern int pr1403;
extern int dbg;
extern char *reg();
extern device;
