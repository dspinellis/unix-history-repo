#ifndef lint
static char sccsid[] = "@(#)write1line.c	1.2 (CWI) 85/10/02";
#endif lint

/*
 * write out one line of output table
 */

#include "defs.h"
#include "ext.h"
 
#define realsplit	((ct == 'a' || ct == 'n') && table[nl][c].rcol)

int     watchout;
int     once;
int     topat[MAXCOL];

/*
 * i is line number for deciding format
 * nl is line number for finding data
 * usually identical
 */
putline(i, nl){
	int c, lf, ct, form, lwid, vspf, ip, cmidx, exvspen, vforml;
	int vct, chfont, uphalf;
	char   *s, *size, *fn;

	dprint(".\\\" -- putline( %d, %d)\n", i, nl);

	watchout = vspf = exvspen = 0;
	if(i == 0)
		once = 0;
	if(i == 0 && (allflg || boxflg || dboxflg))
		fullwide(0, dboxflg ? '=' : '-');
	if(instead[nl] == 0 && fullbot[nl] == 0){
		for(c = 0; c < ncol; c++){
			s = table[nl][c].col;
			if(s == 0)
				continue;
			if(vspen(s)){
				for(ip = nl; ip < nlin; ip = next(ip))
					if(!vspen(s = table[ip][c].col))
						break;
				if((int) s > 0 && (int) s < 128)
					printf(".ne \\n(%c|u+\\n(.Vu\n", s);
				continue;
			}
			if(point(s))
				continue;
			printf(".ne \\n(%c|u+\\n(.Vu\n", s);
			watchout = 1;
		}
	}
	if(linestop[nl])
		printf(".mk #%c\n", linestop[nl] + 'a' - 1);
	lf = prev(nl);
	if(instead[nl]){
		puts(instead[nl]);
		return;
	}
	if(fullbot[nl]){
		switch(ct = fullbot[nl]){
			case '=': 
			case '-': 
				fullwide(nl, ct);
		}
		return;
	}
	for(c = 0; c < ncol; c++){
		if(instead[nl] == 0 && fullbot[nl] == 0){
			if(vspen(table[nl][c].col))
				vspf = 1;
		}
		if(lf >= 0){
			if(vspen(table[lf][c].col))
				vspf = 1;
		}
	}
	if(vspf){
		printf(".nr #^ \\n(\\*(#du\n");
 		/*
		 * current line position relative to bottom
		 */
		printf(".nr #- \\n(#^\n");
	}
	vspf = 0;
	chfont = 0;
	for(c = 0; c < ncol; c++){
		s = table[nl][c].col;
		if(s == 0)
			continue;
		chfont |= (int) (font[stynum[nl]][c]);
		if(point(s))
			continue;
		lf = prev(nl);
		if(lf >= 0 && vspen(table[lf][c].col))
			printf(
	".if (\\n(%c|+\\n(^%c-1v)>\\n(#- .nr #- +(\\n(%c|+\\n(^%c-\\n(#--1v)\n",
							s, 'a' + c, s, 'a' + c);
		else
			printf(
	".if (\\n(%c|+\\n(#^-1v)>\\n(#- .nr #- +(\\n(%c|+\\n(#^-\\n(#--1v)\n",
								s, s);
	}
	if(allflg && once > 0)
		fullwide(i, '-');
	once = 1;
	runtabs(i, nl);
	if(allh(i) && !pr1403){
		printf(".nr %d \\n(.v\n", SVS);
		printf(".vs \\n(.vu-\\n(.sp\n");
	}
	if(chfont)
		printf(".nr %2d \\n(.f\n", S1);
	printf(".nr 35 1m\n");
	printf("\\&");
	vct = 0;
	for(c = 0; c < ncol; c++){
		uphalf = 0;
		if(watchout == 0 && i + 1 < nlin
				 && (lf = left(i, c, &lwid)) >= 0){
			tohcol(c);
			drawvert(lf, i, c, lwid);
			vct += 2;
		}
		if(rightl && c + 1 == ncol)
			continue;
		vforml = i;
		for(lf = prev(nl); lf >= 0 && vspen (table[lf][c].col);
								lf = prev (lf))
			vforml = lf;
		form = ctype(vforml, c);
		if(form != 's'){
			ct = (int) reg(c, CLEFT);
			if(form == 'a')
				ct = (int) reg(c, CMID);
			if(form == 'n' && table[nl][c].rcol && lused[c] == 0)
				ct = (int) reg(c, CMID);
			printf("\\h'|\\n(%2su'", ct);
		}
		s = table[nl][c].col;
		fn = font[stynum[vforml]][c];
		size = csize[stynum[vforml]][c];
		if(*size == 0)
			size = 0;
		if((ctop[stynum[nl]][c] & HALFUP) != 0 && pr1403 == 0)
			uphalf = 1;
		switch(ct = ctype(vforml, c)){

		case 'n': 
		case 'a': 
			if(table[nl][c].rcol){
				if(lused[c]){
 					/*
					 * Zero field width
					 */
					ip = prev(nl);
					if(ip >= 0){
/*
 * Indentation is getting complete out of hand here, let's shift to
 * the left
 */
if(vspen(table[ip][c].col)){
	if(exvspen == 0){
		printf("\\v'-(\\n(\\*(#du-\\n(^%cu", c + 'a');
		if(cmidx)
			printf("-((\\n(#-u-\\n(^%cu)/2u)", c + 'a');

		vct++;
		if(pr1403){
			/*
			 * Must round to whole lines (bwk?)
			 */
			printf("/1v*1v");
		}
		printf("'");
		exvspen = 1;
	}
}
/*
 * Restore layout of this source
 */
					}
					printf("%c%c", F1, F2);
					if(uphalf)
						printf("\\u");
					puttext(s, fn, size);
					if(uphalf)
						printf("\\d");
					printf("%c", F1);
				}
				s = table[nl][c].rcol;
				form = 1;
				break;
			}
		case 'c': 
			form = 3;
			break;
		case 'r': 
			form = 2;
			break;
		case 'l': 
			form = 1;
			break;
		case '-': 
		case '=': 
			if(real(table[nl][c].col)){
				fprintf(stderr,
				 "%s: line %d: Data ignored on table line %d\n",
						ifile, iline - 1, i + 1);
			}
			makeline(i, c, ct);
			continue;
		default: 
			continue;
		}
		if(realsplit ? rused[c] : used[c]){
 			/*
			 * Zero field width
			 */
			/*
			 * form: 1 left, 2 right, 3 center adjust
			 */
			if(ifline(s)){
				makeline(i, c, ifline (s));
				continue;
			}
			if(filler(s)){
				printf("\\l'|\\n(%2su\\&%s'", reg(c, CRIGHT),
									s + 2);
				continue;
			}
			ip = prev(nl);
			cmidx = (ctop[stynum[nl]][c] & (CTOP | CDOWN)) == 0;
			if(ip >= 0)
				if(vspen(table[ip][c].col)){
					if(exvspen == 0){
						printf(
						   "\\v'-(\\n(\\*(#du-\\n(^%cu",
								c + 'a');
						if(cmidx)
							printf(
					"-((\\n(#-u-\\n(^%cu)/2u)", c + 'a');
						vct++;
						if(pr1403){
							/*
							 * Round to
							 * whole lines
							 */
							 printf("/1v*1v");
						}
						printf("'");
					}
				}
			printf("%c", F1);
			if(form != 1)
				printf("%c", F2);
			if(vspen(s))
				vspf = 1;
			else
				puttext(s, fn, size);
			if(form != 2)
				printf("%c", F2);
			printf("%c", F1);
		}
		ip = prev(nl);	/*
				 * Julian Onion mod (system III)
				 */
		if(ip >= 0){
			if(vspen(table[ip][c].col)){
				exvspen = (c + 1 < ncol)
					  && vspen(table[ip][c + 1].col)
					  && (topat[c] == topat[c + 1])
					  && (cmidx == ((ctop[stynum[nl]][c+1]
							   & (CTOP|CDOWN)) == 0))
					  && (left(i, c + 1, &lwid) < 0);
  /*
   * IS THIS WRONG? SHOULD IT BE
   * (cmidx = ...
  && (cmidx = (ctop[stynum[nl]][c+1] &(CTOP|CDOWN) == 0))
   */
				if(exvspen == 0){
					printf("\\v'(\\n(\\*(#du-\\n(^%cu",
								c + 'a');
					if(cmidx)
						printf(
						     "-((\\n(#-u-\\n(^%cu)/2u)",
								c + 'a');
					vct++;
					if(pr1403){
						/*
						 * Round to
						 * whole lines
						 */
						 printf("/1v*1v");
					}
					printf("'");
				}
			}
			else
				exvspen = 0;
		}
		/*
		 * if lines need to be split for gcos
		 * here is the place for a backslash
		 */
		if(vct > 7 && c < ncol){
			printf("\n.sp-1\n\\&");
			vct = 0;
		}
	}
	printf("\n");
	if(allh(i) && !pr1403)
		printf(".vs \\n(%du\n", SVS);
	if(watchout)
		funnies(i, nl);
	if(vspf){
		for(c = 0; c < ncol; c++){
			if(vspen(table[nl][c].col)
				&& (nl == 0 || (lf = prev (nl)) < 0
						     || !vspen(table[lf][c].col
				   )		)){
				printf(".nr ^%c \\n(#^u\n", 'a' + c);
				topat[c] = nl;
			}
		}
	}
}

puttext(s, fn, size)
char *s, *size, *fn;
{
	if(point(s)){
		putfont(fn);
		putsize(size);
		printf("%s", s);
		if(*fn > 0)
			printf("\\f\\n(%2d", S1);
		if(size != 0)
			putsize("0");
	}
}

/*
 * write out funny diverted things
 */
funnies(stl, lin){
	int c, s, pl, lwid, dv, lf, ct;
	char *fn;
	extern char *reg();

	dprint(".\\\" -- funnies\n");
 	/*
	 * remember current vertical position
	 */
	printf(".mk ##\n");
	/*
	 * bottom position
	 */
	printf(".nr %d \\n(##\n", S1);
	for(c = 0; c < ncol; c++){
		s = (int)table[lin][c].col;
		if(point(s))
			continue;
		if(s == 0)
			continue;
		printf(".sp |\\n(##u-1v\n");
		printf(".nr %d ", SIND);
		for(pl = stl; pl >= 0 && !isalpha(ct = ctype (pl, c));
								pl = prev (pl))
			;
		switch(ct){

		case 'n': 
		case 'c': 
			printf("(\\n(%2su+\\n(%2su-\\n(%c-u)/2u\n",reg(c,CLEFT),
					reg(c - 1 + ctspan(lin, c), CRIGHT), s);
			break;
		case 'l': 
			printf("\\n(%2su\n", reg(c, CLEFT));
			break;
		case 'a': 
			printf("\\n(%2su\n", reg(c, CMID));
			break;
		case 'r': 
			printf("\\n(%2su-\\n(%c-u\n", reg(c, CRIGHT), s);
			break;
		}
		printf(".in +\\n(%du\n", SIND);
		fn = font[stynum[stl]][c];
		putfont(fn);
		pl = prev(stl);
		if(stl > 0 && pl >= 0 && vspen(table[pl][c].col)){
			printf(".sp |\\n(^%cu\n", 'a' + c);
			if((ctop[stynum[stl]][c]&(CTOP|CDOWN)) == 0){
				printf(".nr %d \\n(#-u-\\n(^%c-\\n(%c|+1v\n",
							TMP, 'a' + c, s);
				printf(".if \\n(%d>0 .sp \\n(%du/2u",TMP,TMP);
				/*
				 * Round
				 */
				if(pr1403)
					printf("/1v*1v");
				printf("\n");
			}
		}
		printf(".%c+\n", s);
		printf(".in -\\n(%du\n", SIND);
		if(*fn > 0)
			putfont("P");
		printf(".mk %d\n", S2);
		printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", S2, S1, S1, S2);
	}
	printf(".sp |\\n(%du\n", S1);
	for(c = dv = 0; c < ncol; c++){
		if(stl + 1 < nlin && (lf = left(stl, c, &lwid)) >= 0){
			if(dv++ == 0)
				printf(".sp -1\n");
			tohcol(c);
			dv++;
			drawvert(lf, stl, c, lwid);
		}
	}
	if(dv)
		printf("\n");
}

putfont(fn)
char *fn;
{
	if(fn && *fn)
		printf(fn[1] ? "\\f(%.2s" : "\\f%.2s", fn);
}

putsize(s)
char *s;
{
	if(s && *s)
		printf("\\s%s", s);
}
