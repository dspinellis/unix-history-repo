#ifndef lint
static char sccsid[] = "@(#)writecntl.c	1.2 (CWI) 85/10/02";
#endif lint

/*
 * control to write table entries
 */

#include "defs.h"
#include "ext.h"

#define realsplit	((ct == 'a' || ct == 'n') && table[ldata][c].rcol)

runout(){
	register int i;

	if(boxflg || allflg || dboxflg)
		need();
	if(ctrflg){
		printf(".nr #I \\n(.i\n");
		printf(".in +(\\n(.lu-\\n(TWu-\\n(.iu)/2u\n");
	}
	printf(".fc %c %c\n", F1, F2);
	printf(".nr #T 0-1\n");
	deftail();
	for(i = 0; i < nlin; i++)
		putline(i, i);
	if(leftover)
		yetmore();
	printf(".fc\n");
	printf(".nr T. 1\n");
	printf(".T# 1\n");
	if(ctrflg)
		printf(".in \\n(#Iu\n");
}

runtabs(lform, ldata){
	int c, ct, vforml, lf;

	printf(".ta ");
	for(c = 0; c < ncol; c++){
		vforml = lform;
		for(lf = prev(lform); lf >= 0 && vspen (table[lf][c].col);
								lf = prev (lf))
			vforml = lf;
		if(fspan(vforml, c))
			continue;
		switch(ct = ctype(vforml, c)){

		case 'n': 
		case 'a': 
			if(table[ldata][c].rcol)
				if(lused[c]){
 					/*
					 * Zero field width
					 */
					printf("\\n(%2su ",
						reg(c, CMID));
				}
		case 'c': 
		case 'l': 
		case 'r': 
			if(realsplit ? rused[c] : (used[c] + lused[c]))
				printf("\\n(%2su ", reg(c, CRIGHT));
			continue;
		case 's': 
			if(lspan(lform, c))
				printf("\\n(%2su ", reg(c, CRIGHT));
			continue;
		}
	}
	printf("\n");
}

ifline(s)
char   *s;
{
	if(!point(s))
		return(0);
	if(s[0] == '\\')
		s++;
	if(s[1])
		return(0);
	if(s[0] == '_')
		return('-');
	if(s[0] == '=')
		return('=');
	return(0);
}

need(){
	int texlin, horlin, i;

	for(texlin = horlin = i = 0; i < nlin; i++){
		if(fullbot[i] != 0)
			horlin++;
		else if(instead[i] != 0)
				continue;
		else
			texlin++;
	}
	printf(".ne %dv+%dp\n", texlin, 2 * horlin);
}

deftail(){
	int i, c, lf, lwid;

	for(i = 0; i < MAXHEAD; i++)
		if(linestop[i])
			printf(".nr #%c 0-1\n", linestop[i] + 'a' - 1);
	printf(".nr #a 0-1\n");
	printf(".eo\n");
	printf(".de T#\n");
	printf(".ds #d .d\n");
	printf(".if \\(ts\\n(.z\\(ts\\(ts .ds #d nl\n");
	printf(".mk ##\n");
	printf(".nr ## -1v\n");
	printf(".ls 1\n");
	for(i = 0; i < MAXHEAD; i++)
		if(linestop[i])
			printf(".if \\n(#T>=0 .nr #%c \\n(#T\n",
							linestop[i] + 'a' - 1);
	/*
	 * bottom of table line
	 */
	if(boxflg || allflg || dboxflg){
		if(fullbot[nlin - 1] == 0){
			if(!pr1403)
				printf(".if \\n(T. .vs \\n(.vu-\\n(.sp\n");
			printf(".if \\n(T. ");
			drawline(nlin, 0, ncol, dboxflg ? '=' : '-', 1, 0);
			printf("\n.if \\n(T. .vs\n");
			/*
			 * T. is really an argument to a macro but because
			 * of eqn we don't dare pass it as an argument and
			 * reference by $1
			 */
		}
	}
	for(c = 0; c < ncol; c++){
		if((lf = left(nlin - 1, c, &lwid)) >= 0){
			printf(".if \\n(#%c>=0 .sp -1\n",
						linestop[lf] + 'a' - 1);
			printf(".if \\n(#%c>=0 ",
						linestop[lf] + 'a' - 1);
			tohcol(c);
			drawvert(lf, nlin - 1, c, lwid);
			printf("\\h'|\\n(TWu'\n");
		}
	}
	/*
	 * right hand line
	 */
	if(boxflg || allflg || dboxflg){
		printf(".if \\n(#a>=0 .sp -1\n");
		printf(".if \\n(#a>=0 \\h'|\\n(TWu'");
		drawvert(0, nlin - 1, ncol, dboxflg ? 2 : 1);
		printf("\n");
	}
	printf(".ls\n");
	printf("..\n");
	printf(".ec\n");
}
