#ifndef lint
static char sccsid[] = "@(#)vertlines.c	1.2 (CWI) 85/10/02";
#endif lint

/*
 * draw vertical lines
 */

#include "defs.h"
#include "ext.h"

drawvert(start, end, c, lwid){
	char *exb = 0, *ext = 0;
	int tp = 0, sl, ln, pos, epb, ept, vm;

	end++;
	vm = 'v';
	/*
	 * note: nr 35 has value of 1m outside of linesize
	 */
	while(instead[end])
		end++;
	for(ln = 0; ln < lwid; ln++){
		epb = ept = 0;
		pos = 2 * ln - lwid + 1;
		if(pos != tp)
			printf("\\h'%dp'", pos - tp);
		tp = pos;
		if(end < nlin){
			if(fullbot[end] || (!instead[end] && allh(end)))
				epb = 2;
			else {
				switch(midbar(end, c)){

				case '-': 
					exb = "1v-.5m";
					break;
				case '=': 
					exb = "1v-.5m";
					epb = 1;
					break;
				}
			}
		}
		if(lwid > 1){
			switch(interh(end, c)){

			case THRU: 
				epb -= 1;
				break;
			case RIGHT: 
				epb += (ln == 0 ? 1 : -1);
				break;
			case LEFT: 
				epb += (ln == 1 ? 1 : -1);
				break;
			}
		}
		if(lwid == 1){
			switch(interh(end, c)){

			case THRU: 
				epb -= 1;
				break;
			case RIGHT: 
			case LEFT: 
				epb += 1;
				break;
			}
		}
		if(start > 0){
			sl = start - 1;
			while(sl >= 0 && instead[sl])
				sl--;
			if(sl >= 0 && (fullbot[sl] || allh(sl)))
				ept = 0;
			else {
				if(sl >= 0){
					switch(midbar(sl, c)){

					case '-': 
						ext = ".5m";
						break;
					case '=': 
						ext = ".5m";
						ept = -1;
						break;
					default: 
						vm = 'm';
						break;
					}
				} else
					ept = -4;
			}
		} else {
			if(start == 0 && allh(0)){
				ept = 0;
				vm = 'm';
			}
		}
		if(lwid > 1){
			switch(interh(start, c)){

			case THRU: 
				ept += 1;
				break;
			case LEFT: 
				ept += (ln == 0 ? 1 : -1);
				break;
			case RIGHT: 
				ept += (ln == 1 ? 1 : -1);
				break;
			}
		} else {
			if(lwid == 1){
				switch(interh(start, c)){

				case THRU: 
					ept += 1;
					break;
				case LEFT: 
				case RIGHT: 
					ept -= 1;
					break;
				}
			}
		}
		if(exb)
			printf("\\v'%s'", exb);
		if(epb)
			printf("\\v'%dp'", epb);
		printf("\\s\\n(%d", LSIZE);
		printf("\\v'-\\n(%dp/6u'", LSIZE);
		printf("\\h'-\\n(#~n/100u'");
		
		printf("\\L'|\\n(#%cu-%s",
			linestop[start] + 'a' -1, vm == 'v' ? "1v" : "\\n(35u");
		if(ext)
			printf("-(%s)", ext);
		if(exb)
			printf("-(%s)", exb);
		pos = ept - epb;
		if(pos)
			printf("%s%dp", pos >= 0 ? "+" : "", pos);
		printf("'");
		/*
		 * the string #d is either "nl" or ".d" depending on diversions;
	   	 * on GCOS not the same
		 */
		printf("\\h'-\\n(#~n/100u'");
		printf("\\s0\\v'\\n(\\*(#du-\\n(#%cu+%s",
			linestop[start] + 'a' -1, vm == 'v' ? "1v" : "\\n(35u");
		if(ext)
			printf("+%s", ext);
		if(ept)
			printf("%s%dp", (-ept) > 0 ? "+" : "", (-ept));
		printf("'");
		printf("\\v'\\n(%dp/6u'", LSIZE);
	}
}

midbar(i, c){
	int k;

	k = midbcol(i, c);
	if(k == 0 && c > 0)
		k = midbcol(i, c - 1);
	return(k);
}

midbcol(i, c){
	int ct;

	while((ct = ctype(i, c)) == 's')
		c--;
	if(ct == '-' || ct == '=')
		return(ct);
	if(ct = barent(table[i][c].col))
		return(ct);
	return(0);
}

barent(s)
char *s;
{
	if(s == 0)
		return(1);
	if(!point(s))
		return(1);
	if(s[0] == '\\')
		s++;
	if(s[1] != 0)
		return(0);
	switch(s[0]){

	case '_': 
		return('-');
	case '=': 
		return('=');
	}
	return(0);
}
