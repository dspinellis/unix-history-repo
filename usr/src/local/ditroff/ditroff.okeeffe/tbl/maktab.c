#ifndef lint
static char sccsid[] = "@(#)maktab.c	1.3 (CWI) 86/11/13";
#endif lint


/*
 * compute tab stops
 */

#define tx(a)		(a>0 && a<128)

#define FN(i,c)		font[stynum[i]][c]
#define SZ(i,c)		csize[stynum[i]][c]

#include "defs.h"
#include "ext.h"

/*
 * define the tab stops of the table
 */
maktab()
{
	int icol, ilin, tsep, k, ik, vforml, il, text;
	int doubled[MAXCOL], acase[MAXCOL];
	char *s;

	for(icol = 0; icol < ncol; icol++){
		doubled[icol] = acase[icol] = 0;
		printf(".nr %2s 0\n", reg(icol, CRIGHT));
		for(text = 0; text < 2; text++){
			if(text)
				printf(".%2s\n.rm %2s\n", reg(icol, CRIGHT),
							reg(icol, CRIGHT));
			for(ilin = 0; ilin < nlin; ilin++){
				if(instead[ilin] || fullbot[ilin]){
					continue;
				}
				vforml = ilin;
				for(il = prev(ilin);
				    il >= 0 && vspen(table[il][icol].col);
				    il = prev(il))
					vforml = il;
				if(fspan(vforml, icol)){
					continue;
				}
				if(filler(table[ilin][icol].col)){
					continue;
				}
				if((ctop[stynum[ilin]][icol] & ZEROW) != 0){
					continue;
				}
				switch(ctype(vforml, icol)){

				case 'a': 
					acase[icol] = 1;
					s = table[ilin][icol].col;
					if(s > 0 && s < (char *)128 && text){
						if(doubled[icol] == 0)
							printf(
							 ".nr %d 0\n.nr %d 0\n",
							 S1, S2);
						doubled[icol] = 1;
						printf(
					  ".if \\n(%c->\\n(%d .nr %d \\n(%c-\n",
								s, S2, S2, s);
					}
				case 'n': 
					if(table[ilin][icol].rcol != 0){
						if(doubled[icol] == 0
							 && text == 0)
							printf(
							 ".nr %d 0\n.nr %d 0\n",
							 S1, S2);
						doubled[icol] = 1;
						if(real(s=table[ilin][icol].col)
						    && !vspen(s)){
							if(tx((int)s) != text)
								continue;
							printf(".nr %d ", TMP);
							wide(s,
							      FN(vforml, icol),
							      SZ(vforml, icol));
							printf("\n");
							printf(
					   ".if \\n(%d<\\n(%d .nr %d \\n(%d\n",
							      S1, TMP, S1, TMP);
						}
						if(text == 0
						   && real(s=table[ilin][icol].rcol)
						   && !vspen(s) && !barent(s)){
							printf(
							   ".nr %d \\w%c%s%c\n",
							  	TMP, F1, s, F1);
							printf(
					    ".if \\n(%d<\\n(%d .nr %d \\n(%d\n",
							      S2, TMP, S2, TMP);
						}
						continue;
					}
				case 'r': 
				case 'c': 
				case 'l': 
					if(real(s = table[ilin][icol].col)
					   && !vspen(s)){
						if(tx((int)s) != text)
							continue;
						printf(".nr %d ", TMP);
						wide(s,
							FN(vforml, icol),
							SZ(vforml, icol));
						printf("\n");
						printf(
	   ".if \\n(%2s<\\n(%d .nr %2s \\n(%d\n", reg(icol, CRIGHT), TMP,
							reg(icol, CRIGHT), TMP);
					}
				}
			}
		}
		if(acase[icol]){
			printf(".if \\n(%d>=\\n(%2s .nr %2s \\n(%du+2n\n",
				S2, reg(icol, CRIGHT), reg(icol, CRIGHT), S2);
		}
		if(doubled[icol]){
			printf(".nr %2s \\n(%d\n", reg(icol, CMID), S1);
			printf(".nr %d \\n(%2s+\\n(%d\n", TMP, reg(icol, CMID),
									S2);
			printf(".if \\n(%d>\\n(%2s .nr %2s \\n(%d\n", TMP,
				    reg(icol, CRIGHT), reg(icol, CRIGHT), TMP);
			printf(
			     ".if \\n(%d<\\n(%2s .nr %2s +(\\n(%2s-\\n(%d)/2\n",
			     TMP, reg(icol, CRIGHT), reg(icol, CMID),
			     				reg(icol, CRIGHT), TMP);
		}
		if(cll[icol][0]){
			printf(".nr %d %sn\n", TMP, cll[icol]);
			printf(".if \\n(%2s<\\n(%d .nr %2s \\n(%d\n",
				reg(icol, CRIGHT), TMP, reg(icol, CRIGHT), TMP);
		}
		for(ilin = 0; ilin < nlin; ilin++)
			if(k = lspan(ilin, icol)){
				s = table[ilin][icol - k].col;
				if(!real(s) || barent(s) || vspen(s))
					continue;
				printf(".nr %d ", TMP);
				wide(table[ilin][icol - k].col,
				      FN(ilin, icol - k), SZ(ilin, icol - k));
				for(ik = k; ik >= 0; ik--){
					printf("-\\n(%2s", reg(icol-ik,CRIGHT));
					if(!expflg && ik > 0)
						printf( "-%dn", sep[icol - ik]);
				}
				printf("\n");
				printf(".if \\n(%d>0 .nr %d \\n(%d/%d\n",
							TMP, TMP, TMP, k);
				printf(".if \\n(%d<0 .nr %d 0\n",
							TMP, TMP);
				for(ik = 1; ik <= k; ik++){
					if(doubled[icol - k + ik]){
						printf(".nr %2s +\\n(%d/2\n",
							reg(icol - k + ik,CMID),
								TMP);
					}
					printf(".nr %2s +\\n(%d\n",
						reg(icol - k + ik, CRIGHT),TMP);
				}
			}
	}
	if(textflg)
		untext();
	/*
	 * if even requested, make all columns widest width
	 */

#define TMP1 S1
#define TMP2 S2

	if(evenflg){
		printf(".nr %d 0\n", TMP);
		for(icol = 0; icol < ncol; icol++){
			if(evenup[icol] == 0)
				continue;
			printf(".if \\n(%2s>\\n(%d .nr %d \\n(%2s\n",
				reg(icol, CRIGHT), TMP, TMP, reg(icol, CRIGHT));
		}
		for(icol = 0; icol < ncol; icol++){
 			/*
			 * if column not evened just retain old interval
			 */
			if(evenup[icol] == 0)
				continue;
			if(doubled[icol])
				printf(
				   ".nr %2s (100*\\n(%2s/\\n(%2s)*\\n(%d/100\n",
				   	reg(icol, CMID), reg(icol, CMID),
				   		reg(icol, CRIGHT), TMP);
			/*
		 	* that nonsense with the 100's and parens tries to avoid
		 	* overflow while proportionally shifting the middle of
		 	* the number
		 	*/
			printf(".nr %2s \\n(%d\n", reg(icol, CRIGHT), TMP);
		}
	}
	/*
	 * now adjust for total table width
	 */
	for(tsep = icol = 0; icol < ncol; icol++)
		tsep += sep[icol];
	if(expflg){
		printf(".nr %d 0", TMP);
		for(icol = 0; icol < ncol; icol++)
			printf("+\\n(%2s", reg(icol, CRIGHT));
		printf("\n");
		/*
		 * Bug fix: Most users expect the expand to take place
		 * over the line length minus the current indentation
		 * (I do as well, a bit ugly to see the table creeping
		 * in the right margin (jna))
		 */
		printf(".nr %d \\n(.l-\\n(.i-\\n(%d\n", TMP, TMP);
		if(boxflg || dboxflg || allflg)
			tsep += 1;
		else
			tsep -= sep[ncol - 1];
		printf(".nr %d \\n(%d/%d\n", TMP, TMP, tsep);
		printf(".if \\n(%d<0 .nr %d 0\n", TMP, TMP);
	} else
		printf(".nr %d 1n\n", TMP);
	printf(".nr %2s 0\n", reg(-1,CRIGHT));
	tsep = (boxflg || allflg || dboxflg || left1flg) ? 1 : 0;
	for(icol = 0; icol < ncol; icol++){
		printf(".nr %2s \\n(%2s+(%d*\\n(%d)\n", reg(icol, CLEFT),
					reg(icol -1, CRIGHT), tsep, TMP);
		printf(".nr %2s +\\n(%2s\n",reg(icol, CRIGHT),reg(icol, CLEFT));
		if(doubled[icol]){
			/*
			 * the next line is last-ditch effort to avoid
			 * zero field width
			 */
			/*
			printf(".if \\n(%2s=0 .nr %2s 1\n", reg(icol,CMID),
		   				reg(icol,CMID));
			 */
			printf(".nr %2s +\\n(%2s\n", reg(icol, CMID),
							reg(icol, CLEFT));
			/*
			printf(".if n .if \\n(%2s%%24>0 .nr %2s +12u\n",
					reg(icol, CMID), reg(icol, CMID));
			 */
		}
		tsep = sep[icol];
	}
	if(rightl)
		printf(".nr %2s (\\n(%2s+\\n(%2s)/2\n", reg(ncol-1, CRIGHT),
							reg(ncol-1, CLEFT),
							reg(ncol-2, CRIGHT));
	printf(".nr TW \\n(%2s\n", reg(ncol-1, CRIGHT));
	if(boxflg || allflg || dboxflg)
		printf(".nr TW +%d*\\n(%d\n", sep[ncol - 1], TMP);
	printf(
".if t .if(\\n(TW+\\n(.o)>\\n(Tw .tm Table at line %d file %s is too wide - \\n(TW units\n",
		iline - 1, strlen(oldname) ? oldname : ifile);
	return;
}

/*
 * tell troff to calculate width of an entry
 */
wide(s, fn, size)
char *s, *size, *fn;
{
	/*
	 * if s is a pointer to a string, calculate the with of that string
	 */
	if(point(s)){
		printf("\\w%c", F1);
		if(*fn > 0)
			putfont(fn);
		if(*size)
			putsize(size);
		printf("%s", s);
		if(*fn > 0)
			putfont("P");
		if(*size)
			putsize("0");
		printf("%c", F1);
	} else
		/*
		 * it is the name of a diversion, so we know the witdh
		 * is in <diversionname>-
		 */
		printf("\\n(%c-", s);
}

filler(s)
char *s;
{
	return(point(s) && s[0] == '\\' && s[1] == 'R');
}
