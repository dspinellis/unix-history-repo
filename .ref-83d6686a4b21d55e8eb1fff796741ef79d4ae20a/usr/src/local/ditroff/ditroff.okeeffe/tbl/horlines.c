#ifndef lint
static char sccsid[] = "@(#)horlines.c	1.2 (CWI) 85/10/02";
#endif lint


/*
 * draw horizontal lines
 */

#include "defs.h"
#include "ext.h"

makeline(i, c, lintype){
	int cr, type, shortl;

	type = thish(i, c);
	if(type == 0)
		return;
	cr = c;
	shortl = (table[i][c].col[0] == '\\');
	if(c > 0 && !shortl && thish(i, c - 1) == type)
		return;
	if(shortl == 0)
		for(cr = c;
		    cr < ncol && (ctype(i, cr) == 's' || type == thish (i, cr));
		      cr++)
			;
	else
		for(cr = c + 1; cr < ncol && ctype(i, cr) == 's'; cr++)
			;
	drawline(i, c, cr - 1, lintype, 0, shortl);
}

fullwide(i, lintype)
{
	int cr, cl;

	if(!pr1403)
		printf(".nr %d \\n(.v\n.vs \\n(.vu-\\n(.sp\n", SVS);
	cr = 0;
	while(cr < ncol){
		cl = cr;
		while(i > 0 && vspand(prev (i), cl, 1))
			cl++;
		for(cr = cl; cr < ncol; cr++)
			if(i > 0 && vspand(prev (i), cr, 1))
				break;
		if(cl < ncol)
			drawline(i, cl, (cr < ncol ? cr - 1 : cr), lintype,
									1, 0);
	}
	printf("\n");
	if(!pr1403)
		printf(".vs \\n(%du\n", SVS);
}

drawline(i, cl, cr, lintype, noheight, shortl)
{
	char *exhr, *exhl;
	int lcount, ln, linpos, oldpos, nodata, lnch;

	lcount = 0;
	exhr = exhl = "";
	switch(lintype){

	case '-': 
		lcount = 1;
		break;
	case '=': 
		lcount = pr1403 ? 1 : 2;
		break;
	case SHORTLINE: 
		lcount = 1;
		break;
	}
	if(lcount <= 0)
		return;
	nodata = cr - cl >= ncol || noheight || allh(i);
	if(!nodata)
		printf("\\v'-.5m'");
	for(ln = oldpos = 0; ln < lcount; ln++){
		linpos = 2 * ln - lcount + 1;
		if(linpos != oldpos)
			printf("\\v'%dp'", linpos - oldpos);
		oldpos = linpos;
		if(shortl == 0){
			tohcol(cl);
			if(lcount > 1){
				switch(interv(i, cl)){
					case TOP: 
						exhl = ln == 0 ? "1p" : "-1p";
						break;
					case BOT: 
						exhl = ln == 1 ? "1p" : "-1p";
						break;
					case THRU: 
						exhl = "1p";
						break;
				}
				if(exhl[0])
					printf("\\h'%s'", exhl);
			} else {
				if(lcount == 1){
					switch(interv(i, cl)){

					case TOP: 
					case BOT: 
						exhl = "-1p";
						break;
					case THRU: 
						exhl = "1p";
						break;
					}
					if(exhl[0])
						printf("\\h'%s'", exhl);
				}
			}
			if(lcount > 1){
				switch(interv(i, cr + 1)){

				case TOP: 
					exhr = ln == 0 ? "-1p" : "+1p";
					break;
				case BOT: 
					exhr = ln == 1 ? "-1p" : "+1p";
					break;
				case THRU: 
					exhr = "-1p";
					break;
				}
			} else {
				if(lcount == 1){
					switch(interv(i, cr + 1)){

					case TOP: 
					case BOT: 
						exhr = "+1p";
						break;
					case THRU: 
						exhr = "-1p";
						break;
					}
				}
			}
		} else
			printf("\\h'|\\n(%du'", reg(cl, CLEFT));
		printf("\\s\\n(%d", LSIZE);
		printf("\\v'-\\n(#|n/100u'");
		if(shortl)
			printf("\\l'|\\n(%du'", reg(cr, CRIGHT));
		else {
			lnch = (int) "\\(ul";
			if(pr1403)
				lnch = lintype == 2 ? (int) "=" : (int) "\\(ru";
			if(cr + 1 >= ncol)
				printf("\\l'|\\n(TWu%s%s'", exhr, lnch);
			else
				printf("\\l'(|\\n(%2su+|\\n(%2su)/2u%s%s'",
						reg(cr, CRIGHT),
						reg(cr + 1, CLEFT), exhr, lnch);
		}
		printf("\\v'\\n(#|n/100u'");
		printf("\\s0");
	}
	if(oldpos != 0)
		printf("\\v'%dp'", -oldpos);
	if(!nodata)
		printf("\\v'+.5m'");
}
