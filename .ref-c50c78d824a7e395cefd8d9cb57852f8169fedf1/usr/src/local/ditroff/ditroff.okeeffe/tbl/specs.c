#ifndef lint
static char sccsid[] = "@(#)specs.c	1.2 (CWI) 85/10/02";
#endif lint

/*
 * read table specification
 */

#include "defs.h"
#include "ext.h"

int     oncol;

getspec(){
	int icol, i;

	qcol = findcol() + 1;
	/*
	 * Must allow one extra for line at right.
	 */
	/*
	 * clear out and default every specification
	 * for each column ...
	 */
	for(icol = 0; icol < MAXCOL; icol++){
		sep[icol] = -1;
		evenup[icol] = 0;
		cll[icol][0] = 0;
		/*
		 * ... and each specification "line".
		 */
		for(i = 0; i < MAXHEAD; i++){
			csize[i][icol][0] = 0;
			vsize[i][icol][0] = 0;
			font[i][icol][0] = lefline[i][icol] = 0;
			ctop[i][icol] = 0;
			style[i][icol] = 'l';
		}
	}
	nclin = ncol = 0;
	oncol = 0;
	left1flg = rightl = 0;

	readspec();

	printf(".rm");

	for(i = 0; i < ncol; i++)
		printf(" %2s", reg(i, CRIGHT));
	printf("\n");
}

/*
 * Read specification part of table description
 */
readspec(){
	int icol, c, sawchar, stopc, i;
	char sn[10], *snp, *temp;

	sawchar = icol = 0;
	while(c = get1char()){
		switch(c){

		default: 
			if(c != tab)
				error("bad table specification character");
			/*FALLTHROUGH*/
		case ' ':
			/*
			 * note this is also case tab
			 */
			continue;
		case '\n': 
			if(sawchar == 0)	/* empty line? (jna) */
				continue;
			/*FALTHROUGH*/
		case ',': 
		case '.':
			/*
			 * end of table specification
			 */
			ncol = max(ncol, icol);
			if(lefline[nclin][ncol] > 0){
				ncol++;
				rightl++;
			}
			if(sawchar)
				nclin++;
			if(nclin >= MAXHEAD)
				error("too many lines in specification");
			icol = 0;
			if(ncol == 0 || nclin == 0)
				error("no specification");
			if(c == '.'){
				while((c = get1char()) && c != '\n')
					if(c != ' ' && c != '\t')
						error("dot not last character on format line");
				/*
				 * fix up sep - default is 3 except
			   	 * at edge
				 */
				for(icol = 0; icol < ncol; icol++)
					if(sep[icol] < 0)
						sep[icol] = icol + 1 < ncol ? 3 : 1;
				if(oncol == 0)
					oncol = ncol;
				else
					if(oncol + 2 < ncol)
						error("tried to widen table in T&, not allowed");
				return;
			}
			sawchar = 0;
			continue;
		case 'C': 
		case 'S': 
		case 'R': 
		case 'N': 
		case 'L': 
		case 'A': 
			c += ('a' - 'A');
		case '_': 
			if(c == '_')
				c = '-';
		case '=': 
		case '-': 
		case '^': 
		case 'c': 
		case 's': 
		case 'n': 
		case 'r': 
		case 'l': 
		case 'a': 
			style[nclin][icol] = c;
			if(c == 's' && icol <= 0)
				error("first column can not be S-type");
			if(c == 's' && style[nclin][icol - 1] == 'a'){
				printf(".tm warning: can't span a-type cols, changed to l\n");
				style[nclin][icol - 1] = 'l';
			}
			if(c == 's' && style[nclin][icol - 1] == 'n'){
				printf(".tm warning: can't span n-type cols, changed to c\n");
				style[nclin][icol - 1] = 'c';
			}
			icol++;
			if(c == '^' && nclin <= 0)
				error("first row can not contain vertical span");
			if(icol >= MAXCOL)
				error("too many columns in table");
			sawchar = 1;
			continue;
		case 'b': 
		case 'i': 
			c += 'A' - 'a';
		case 'B': 
		case 'I': 
			if(icol == 0)
				continue;
			snp = font[nclin][icol - 1];
			snp[0] = (c == 'I' ? '2' : '3');
			snp[1] = 0;
			continue;
		case 't': 
		case 'T': 
			if(icol > 0)
				ctop[nclin][icol - 1] |= CTOP;
			continue;
		case 'd': 
		case 'D': 
			if(icol > 0)
				ctop[nclin][icol - 1] |= CDOWN;
			continue;
		case 'f': 
		case 'F': 
			if(icol == 0)
				continue;
			snp = font[nclin][icol - 1];
			snp[0] = snp[1] = stopc = 0;
			for(i = 0; i < 2; i++){
				c = get1char();
				if(i == 0 && c == '('){
					stopc = ')';
					c = get1char();
				}
				if(c == 0)
					break;
				if(c == stopc){
					stopc = 0;
					break;
				}
				if(stopc == 0)
					if(c == ' ' || c == tab)
						break;
				if(c == '\n'){
					un1getc(c);
					break;
				}
				snp[i] = c;
				if(c >= '0' && c <= '9')
					break;
			}
			if(stopc)
				if(get1char() != stopc)
					error("Nonterminated font name");
			continue;
		case 'P': 
		case 'p': 
			if(icol <= 0)
				continue;
			temp = snp = csize[nclin][icol - 1];
			while(c = get1char()){
				if(c == ' ' || c == tab || c == '\n')
					break;
				if(c == '-' || c == '+') {
					if(snp > temp)
						break;
					else
						*snp++ = c;
				} else {
					if(isdigit(c))
						*snp++ = c;
					else
						break;
				}
				if(snp - temp > 4)
					error("point size too large");
			}
			*snp = 0;
			if(atoi(temp) > 36)
				error("point size unreasonable");
			un1getc(c);
			continue;
		case 'V': 
		case 'v': 
			if(icol <= 0)
				continue;
			temp = snp = vsize[nclin][icol - 1];
			while(c = get1char()){
				if(c == ' ' || c == tab || c == '\n')
					break;
				if(c == '-' || c == '+') {
					if(snp > temp)
						break;
					else
						*snp++ = c;
				} else {
					if(isdigit(c))
						*snp++ = c;
					else
						break;
				}
				if(snp - temp > 4)
					error("vertical spacing value too large");
			}
			*snp = 0;
			un1getc(c);
			continue;
		case 'w': 
		case 'W': 
			snp = cll[icol - 1];
			/*
			 * Dale Smith didn't like this check - possible to
			 * have two text blocks of different widths now ....
			if(*snp) {
				printf("Ignored second width specification");
				continue;
			}
			/*
		 	 * end commented out code ...
			 */
			stopc = 0;
			while(c = get1char()){
				if(snp == cll[icol - 1] && c == '('){
					stopc = ')';
					continue;
				}
				if(!stopc && (c > '9' || c < '0'))
					break;
				if(stopc && c == stopc)
					break;
				*snp++ = c;
			}
			*snp = 0;
			if(snp - cll[icol - 1] > CLLEN)
				error("column width too long");
			if(!stopc)
				un1getc(c);
			continue;
		case 'e': 
		case 'E': 
			if(icol < 1)
				continue;
			evenup[icol - 1] = 1;
			evenflg = 1;
			continue;
		case 'z':
		case 'Z':
			/*
			 * Zero width - ignore width this item
			 */
			if(icol < 1)
				continue;
			ctop[nclin][icol - 1] |= ZEROW;
			continue;
		case 'u':
		case 'U':
			/*
			 * Halfline up
			 */
			if(icol < 1)
				continue;
			ctop[nclin][icol - 1] |= HALFUP;
			continue;
		case '0': 
		case '1': 
		case '2': 
		case '3': 
		case '4': 
		case '5': 
		case '6': 
		case '7': 
		case '8': 
		case '9': 
			sn[0] = c;
			snp = sn + 1;
			while(isdigit(*snp++ = c = get1char ()))
				;
			un1getc(c);
			sep[icol - 1] = max(sep[icol - 1], atoi(sn));
			continue;
		case '|': 
			lefline[nclin][icol]++;
			if(icol == 0)
				left1flg = 1;
			continue;
		}
	}
	error("EOF reading table specification");
}

#define FLNLIM 200

/*
 * findcol counts the number of columns and then puts the line back
 */
static
findcol()
{
	char *s, line[FLNLIM + 2], *p;
	int c, n = 0, inpar = 0;

	while((c = get1char()) != EOF && c == ' ')
		;
	if(c != '\n')
		un1getc(c);
	for(s = line; *s = c = get1char(); s++){
		if(c == ')')
			inpar = 0;
		if(inpar)
			continue;
		if(c == '\n' || c == EOF || c == '.' || c == ',')
			break;
		else if(c == '(')
			inpar = 1;
		else if(s >= line + FLNLIM)
			error("too long spec line");
	}
	for(p = line; p < s; p++){
		switch(c = *p){

		case 'l': 
		case 'r': 
		case 'c': 
		case 'n': 
		case 'a': 
		case 's': 
		case 'L': 
		case 'R': 
		case 'C': 
		case 'N': 
		case 'A': 
		case 'S': 
		case '-': 
		case '=': 
		case '_': 
			n++;
		}
	}
	while(p >= line)
		un1getc(*p--);
	return(n);
}
