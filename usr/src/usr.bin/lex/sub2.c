#ifndef lint
static char sccsid[] = "@(#)sub2.c	4.1 (Berkeley) 8/11/83";
#endif

# include "ldefs.c"
cfoll(v)
	int v;
	{
	register int i,j,k;
	char *p;
	i = name[v];
	if(i < NCH) i = 1;	/* character */
	switch(i){
		case 1: case RSTR: case RCCL: case RNCCL: case RNULLS:
			for(j=0;j<tptr;j++)
				tmpstat[j] = FALSE;
			count = 0;
			follow(v);
# ifdef PP
			padd(foll,v);		/* packing version */
# endif
# ifndef PP
			add(foll,v);		/* no packing version */
# endif
			if(i == RSTR) cfoll(left[v]);
			else if(i == RCCL || i == RNCCL){	/* compress ccl list */
				for(j=1; j<NCH;j++)
					symbol[j] = (i==RNCCL);
				p = left[v];
				while(*p)
					symbol[*p++] = (i == RCCL);
				p = pcptr;
				for(j=1;j<NCH;j++)
					if(symbol[j]){
						for(k=0;p+k < pcptr; k++)
							if(cindex[j] == *(p+k))
								break;
						if(p+k >= pcptr)*pcptr++ = cindex[j];
						}
				*pcptr++ = 0;
				if(pcptr > pchar + pchlen)
					error("Too many packed character classes");
				left[v] = p;
				name[v] = RCCL;	/* RNCCL eliminated */
# ifdef DEBUG
				if(debug && *p){
					printf("ccl %d: %d",v,*p++);
					while(*p)
						printf(", %d",*p++);
					putchar('\n');
					}
# endif
				}
			break;
		case CARAT:
			cfoll(left[v]);
			break;
		case STAR: case PLUS: case QUEST: case RSCON: 
			cfoll(left[v]);
			break;
		case BAR: case RCAT: case DIV: case RNEWE:
			cfoll(left[v]);
			cfoll(right[v]);
			break;
# ifdef DEBUG
		case FINAL:
		case S1FINAL:
		case S2FINAL:
			break;
		default:
			warning("bad switch cfoll %d",v);
# endif
		}
	return;
	}
# ifdef DEBUG
pfoll()
	{
	register int i,k,*p;
	int j;
	/* print sets of chars which may follow positions */
	printf("pos\tchars\n");
	for(i=0;i<tptr;i++)
		if(p=foll[i]){
			j = *p++;
			if(j >= 1){
				printf("%d:\t%d",i,*p++);
				for(k=2;k<=j;k++)
					printf(", %d",*p++);
				putchar('\n');
				}
			}
	return;
	}
# endif
add(array,n)
  int **array;
  int n; {
	register int i, *temp;
	register char *ctemp;
	temp = nxtpos;
	ctemp = tmpstat;
	array[n] = nxtpos;		/* note no packing is done in positions */
	*temp++ = count;
	for(i=0;i<tptr;i++)
		if(ctemp[i] == TRUE)
			*temp++ = i;
	nxtpos = temp;
	if(nxtpos >= positions+maxpos)
		error("Too many positions %s",(maxpos== MAXPOS?"\nTry using %p num":""));
	return;
	}
follow(v)
  int v;
	{
	register int p;
	if(v >= tptr-1)return;
	p = parent[v];
	if(p == 0) return;
	switch(name[p]){
			/* will not be CHAR RNULLS FINAL S1FINAL S2FINAL RCCL RNCCL */
		case RSTR:
			if(tmpstat[p] == FALSE){
				count++;
				tmpstat[p] = TRUE;
				}
			break;
		case STAR: case PLUS:
			first(v);
			follow(p);
			break;
		case BAR: case QUEST: case RNEWE:
			follow(p);
			break;
		case RCAT: case DIV: 
			if(v == left[p]){
				if(nullstr[right[p]])
					follow(p);
				first(right[p]);
				}
			else follow(p);
			break;
		case RSCON: case CARAT: 
			follow(p);
			break;
# ifdef DEBUG
		default:
			warning("bad switch follow %d",p);
# endif
		}
	return;
	}
first(v)	/* calculate set of positions with v as root which can be active initially */
  int v; {
	register int i;
	register char *p;
	i = name[v];
	if(i < NCH)i = 1;
	switch(i){
		case 1: case RCCL: case RNCCL: case RNULLS: case FINAL: case S1FINAL: case S2FINAL:
			if(tmpstat[v] == FALSE){
				count++;
				tmpstat[v] = TRUE;
				}
			break;
		case BAR: case RNEWE:
			first(left[v]);
			first(right[v]);
			break;
		case CARAT:
			if(stnum % 2 == 1)
				first(left[v]);
			break;
		case RSCON:
			i = stnum/2 +1;
			p = right[v];
			while(*p)
				if(*p++ == i){
					first(left[v]);
					break;
					}
			break;
		case STAR: case QUEST: case PLUS:  case RSTR:
			first(left[v]);
			break;
		case RCAT: case DIV:
			first(left[v]);
			if(nullstr[left[v]])
				first(right[v]);
			break;
# ifdef DEBUG
		default:
			warning("bad switch first %d",v);
# endif
		}
	return;
	}
cgoto(){
	register int i, j, s;
	int npos, curpos, n;
	int tryit;
	char tch[NCH];
	int tst[NCH];
	char *q;
	/* generate initial state, for each start condition */
	if(ratfor){
		fprintf(fout,"blockdata\n");
		fprintf(fout,"common /Lvstop/ vstop\n");
		fprintf(fout,"define Svstop %d\n",nstates+1);
		fprintf(fout,"integer vstop(Svstop)\n");
		}
	else fprintf(fout,"int yyvstop[] ={\n0,\n");
	while(stnum < 2 || stnum/2 < sptr){
		for(i = 0; i<tptr; i++) tmpstat[i] = 0;
		count = 0;
		if(tptr > 0)first(tptr-1);
		add(state,stnum);
# ifdef DEBUG
		if(debug){
			if(stnum > 1)
				printf("%s:\n",sname[stnum/2]);
			pstate(stnum);
			}
# endif
		stnum++;
		}
	stnum--;
	/* even stnum = might not be at line begin */
	/* odd stnum  = must be at line begin */
	/* even states can occur anywhere, odd states only at line begin */
	for(s = 0; s <= stnum; s++){
		tryit = FALSE;
		cpackflg[s] = FALSE;
		sfall[s] = -1;
		acompute(s);
		for(i=0;i<NCH;i++) symbol[i] = 0;
		npos = *state[s];
		for(i = 1; i<=npos; i++){
			curpos = *(state[s]+i);
			if(name[curpos] < NCH) symbol[name[curpos]] = TRUE;
			else switch(name[curpos]){
			case RCCL:
				tryit = TRUE;
				q = left[curpos];
				while(*q){
					for(j=1;j<NCH;j++)
						if(cindex[j] == *q)
							symbol[j] = TRUE;
					q++;
					}
				break;
			case RSTR:
				symbol[right[curpos]] = TRUE;
				break;
# ifdef DEBUG
			case RNULLS:
			case FINAL:
			case S1FINAL:
			case S2FINAL:
				break;
			default:
				warning("bad switch cgoto %d state %d",curpos,s);
				break;
# endif
			}
		}
# ifdef DEBUG
		if(debug){
			printf("State %d transitions on:\n\t",s);
			charc = 0;
			for(i = 1; i<NCH; i++){
				if(symbol[i]) allprint(i);
				if(charc > LINESIZE){
					charc = 0;
					printf("\n\t");
					}
				}
			putchar('\n');
			}
# endif
		/* for each char, calculate next state */
		n = 0;
		for(i = 1; i<NCH; i++){
			if(symbol[i]){
				nextstate(s,i);		/* executed for each state, transition pair */
				xstate = notin(stnum);
				if(xstate == -2) warning("bad state  %d %o",s,i);
				else if(xstate == -1){
					if(stnum >= nstates)
						error("Too many states %s",(nstates == NSTATES ? "\nTry using %n num":""));
					add(state,++stnum);
# ifdef DEBUG
					if(debug)pstate(stnum);
# endif
					tch[n] = i;
					tst[n++] = stnum;
					}
				else {		/* xstate >= 0 ==> state exists */
					tch[n] = i;
					tst[n++] = xstate;
					}
				}
			}
		tch[n] = 0;
		tst[n] = -1;
		/* pack transitions into permanent array */
		if(n > 0) packtrans(s,tch,tst,n,tryit);
		else gotof[s] = -1;
		}
	ratfor ? fprintf(fout,"end\n") : fprintf(fout,"0};\n");
	return;
	}
	/*	Beware -- 70% of total CPU time is spent in this subroutine -
		if you don't believe me - try it yourself ! */
nextstate(s,c)
  int s,c; {
	register int j, *newpos;
	register char *temp, *tz;
	int *pos, i, *f, num, curpos, number;
	/* state to goto from state s on char c */
	num = *state[s];
	temp = tmpstat;
	pos = state[s] + 1;
	for(i = 0; i<num; i++){
		curpos = *pos++;
		j = name[curpos];
		if(j < NCH && j == c
		|| j == RSTR && c == right[curpos]
		|| j == RCCL && member(c,left[curpos])){
			f = foll[curpos];
			number = *f;
			newpos = f+1;
			for(j=0;j<number;j++)
				temp[*newpos++] = 2;
			}
		}
	j = 0;
	tz = temp + tptr;
	while(temp < tz){
		if(*temp == 2){
			j++;
			*temp++ = 1;
			}
		else *temp++ = 0;
		}
	count = j;
	return;
	}
notin(n)
  int n;	{	/* see if tmpstat occurs previously */
	register int *j,k;
	register char *temp;
	int i;
	if(count == 0)
		return(-2);
	temp = tmpstat;
	for(i=n;i>=0;i--){	/* for each state */
		j = state[i];
		if(count == *j++){
			for(k=0;k<count;k++)
				if(!temp[*j++])break;
			if(k >= count)
				return(i);
			}
		}
	return(-1);
	}
packtrans(st,tch,tst,cnt,tryit)
  int st, *tst, cnt,tryit;
  char *tch; {
	/* pack transitions into nchar, nexts */
	/* nchar is terminated by '\0', nexts uses cnt, followed by elements */
	/* gotof[st] = index into nchr, nexts for state st */

	/* sfall[st] =  t implies t is fall back state for st */
	/*	        == -1 implies no fall back */

	int cmin, cval, tcnt, diff, p, *ast;
	register int i,j,k;
	char *ach;
	int go[NCH], temp[NCH], c;
	int swork[NCH];
	char cwork[NCH];
	int upper;

	rcount += cnt;
	cmin = -1;
	cval = NCH;
	ast = tst;
	ach = tch;
	/* try to pack transitions using ccl's */
	if(!optim)goto nopack;		/* skip all compaction */
	if(tryit){	/* ccl's used */
		for(i=1;i<NCH;i++){
			go[i] = temp[i] = -1;
			symbol[i] = 1;
			}
		for(i=0;i<cnt;i++){
			go[tch[i]] = tst[i];
			symbol[tch[i]] = 0;
			}
		for(i=0; i<cnt;i++){
			c = match[tch[i]];
			if(go[c] != tst[i] || c == tch[i])
				temp[tch[i]] = tst[i];
			}
		/* fill in error entries */
		for(i=1;i<NCH;i++)
			if(symbol[i]) temp[i] = -2;	/* error trans */
		/* count them */
		k = 0;
		for(i=1;i<NCH;i++)
			if(temp[i] != -1)k++;
		if(k <cnt){	/* compress by char */
# ifdef DEBUG
			if(debug) printf("use compression  %d,  %d vs %d\n",st,k,cnt);
# endif
			k = 0;
			for(i=1;i<NCH;i++)
				if(temp[i] != -1){
					cwork[k] = i;
					swork[k++] = (temp[i] == -2 ? -1 : temp[i]);
					}
			cwork[k] = 0;
# ifdef PC
			ach = cwork;
			ast = swork;
			cnt = k;
			cpackflg[st] = TRUE;
# endif
			}
		}
	for(i=0; i<st; i++){	/* get most similar state */
				/* reject state with more transitions, state already represented by a third state,
					and state which is compressed by char if ours is not to be */
		if(sfall[i] != -1) continue;
		if(cpackflg[st] == 1) if(!(cpackflg[i] == 1)) continue;
		p = gotof[i];
		if(p == -1) /* no transitions */ continue;
		tcnt = nexts[p];
		if(tcnt > cnt) continue;
		diff = 0;
		k = 0;
		j = 0;
		upper = p + tcnt;
		while(ach[j] && p < upper){
			while(ach[j] < nchar[p] && ach[j]){diff++; j++; }
			if(ach[j] == 0)break;
			if(ach[j] > nchar[p]){diff=NCH;break;}
			/* ach[j] == nchar[p] */
			if(ast[j] != nexts[++p] || ast[j] == -1 || (cpackflg[st] && ach[j] != match[ach[j]]))diff++;
			j++;
			}
		while(ach[j]){
			diff++;
			j++;
			}
		if(p < upper)diff = NCH;
		if(diff < cval && diff < tcnt){
			cval = diff;
			cmin = i;
			if(cval == 0)break;
			}
		}
	/* cmin = state "most like" state st */
# ifdef DEBUG
	if(debug)printf("select st %d for st %d diff %d\n",cmin,st,cval);
# endif
# ifdef PS
	if(cmin != -1){ /* if we can use st cmin */
		gotof[st] = nptr;
		k = 0;
		sfall[st] = cmin;
		p = gotof[cmin]+1;
		j = 0;
		while(ach[j]){
			/* if cmin has a transition on c, then so will st */
			/* st may be "larger" than cmin, however */
			while(ach[j] < nchar[p-1] && ach[j]){
				k++;
				nchar[nptr] = ach[j];
				nexts[++nptr] = ast[j];
				j++;
				}
			if(nchar[p-1] == 0)break;
			if(ach[j] > nchar[p-1]){
				warning("bad transition %d %d",st,cmin);
				goto nopack;
				}
			/* ach[j] == nchar[p-1] */
			if(ast[j] != nexts[p] || ast[j] == -1 || (cpackflg[st] && ach[j] != match[ach[j]])){
				k++;
				nchar[nptr] = ach[j];
				nexts[++nptr] = ast[j];
				}
			p++;
			j++;
			}
		while(ach[j]){
			nchar[nptr] = ach[j];
			nexts[++nptr] = ast[j++];
			k++;
			}
		nexts[gotof[st]] = cnt = k;
		nchar[nptr++] = 0;
		}
	else {
# endif
nopack:
	/* stick it in */
		gotof[st] = nptr;
		nexts[nptr] = cnt;
		for(i=0;i<cnt;i++){
			nchar[nptr] = ach[i];
			nexts[++nptr] = ast[i];
			}
		nchar[nptr++] = 0;
# ifdef PS
		}
# endif
	if(cnt < 1){
		gotof[st] = -1;
		nptr--;
		}
	else
		if(nptr > ntrans)
			error("Too many transitions %s",(ntrans==NTRANS?"\nTry using %a num":""));
	return;
	}
# ifdef DEBUG
pstate(s)
  int s; {
	register int *p,i,j;
	printf("State %d:\n",s);
	p = state[s];
	i = *p++;
	if(i == 0) return;
	printf("%4d",*p++);
	for(j = 1; j<i; j++){
		printf(", %4d",*p++);
		if(j%30 == 0)putchar('\n');
		}
	putchar('\n');
	return;
	}
# endif
member(d,t)
  int d;
  char *t;	{
	register int c;
	register char *s;
	c = d;
	s = t;
	c = cindex[c];
	while(*s)
		if(*s++ == c) return(1);
	return(0);
	}
# ifdef DEBUG
stprt(i)
  int i; {
	register int p, t;
	printf("State %d:",i);
	/* print actions, if any */
	t = atable[i];
	if(t != -1)printf(" final");
	putchar('\n');
	if(cpackflg[i] == TRUE)printf("backup char in use\n");
	if(sfall[i] != -1)printf("fall back state %d\n",sfall[i]);
	p = gotof[i];
	if(p == -1) return;
	printf("(%d transitions)\n",nexts[p]);
	while(nchar[p]){
		charc = 0;
		if(nexts[p+1] >= 0)
			printf("%d\t",nexts[p+1]);
		else printf("err\t");
		allprint(nchar[p++]);
		while(nexts[p] == nexts[p+1] && nchar[p]){
			if(charc > LINESIZE){
				charc = 0;
				printf("\n\t");
				}
			allprint(nchar[p++]);
			}
		putchar('\n');
		}
	putchar('\n');
	return;
	}
# endif
acompute(s)	/* compute action list = set of poss. actions */
  int s; {
	register int *p, i, j;
	int cnt, m;
	int temp[300], k, neg[300], n;
	k = 0;
	n = 0;
	p = state[s];
	cnt = *p++;
	if(cnt > 300)
		error("Too many positions for one state - acompute");
	for(i=0;i<cnt;i++){
		if(name[*p] == FINAL)temp[k++] = left[*p];
		else if(name[*p] == S1FINAL){temp[k++] = left[*p];
			if (left[*p] >NACTIONS) error("Too many right contexts");
			extra[left[*p]] = 1;
			}
		else if(name[*p] == S2FINAL)neg[n++] = left[*p];
		p++;
		}
	atable[s] = -1;
	if(k < 1 && n < 1) return;
# ifdef DEBUG
	if(debug) printf("final %d actions:",s);
# endif
	/* sort action list */
	for(i=0; i<k; i++)
		for(j=i+1;j<k;j++)
			if(temp[j] < temp[i]){
				m = temp[j];
				temp[j] = temp[i];
				temp[i] = m;
				}
	/* remove dups */
	for(i=0;i<k-1;i++)
		if(temp[i] == temp[i+1]) temp[i] = 0;
	/* copy to permanent quarters */
	atable[s] = aptr;
# ifdef DEBUG
	if(!ratfor)fprintf(fout,"/* actions for state %d */",s);
# endif
	putc('\n',fout);
	for(i=0;i<k;i++)
		if(temp[i] != 0){
			ratfor ? fprintf(fout,"data vstop(%d)/%d/\n",aptr,temp[i]) : fprintf(fout,"%d,\n",temp[i]);
# ifdef DEBUG
			if(debug)
				printf("%d ",temp[i]);
# endif
			aptr++;
			}
	for(i=0;i<n;i++){		/* copy fall back actions - all neg */
		ratfor ? fprintf(fout,"data vstop(%d)/%d/\n",aptr,neg[i]) : fprintf(fout,"%d,\n",neg[i]);
		aptr++;
# ifdef DEBUG
		if(debug)printf("%d ",neg[i]);
# endif
		}
# ifdef DEBUG
	if(debug)putchar('\n');
# endif
	ratfor ? fprintf(fout,"data vstop (%d)/0/\n",aptr) : fprintf(fout,"0,\n");
	aptr++;
	return;
	}
# ifdef DEBUG
pccl() {
	/* print character class sets */
	register int i, j;
	printf("char class intersection\n");
	for(i=0; i< ccount; i++){
		charc = 0;
		printf("class %d:\n\t",i);
		for(j=1;j<NCH;j++)
			if(cindex[j] == i){
				allprint(j);
				if(charc > LINESIZE){
					printf("\n\t");
					charc = 0;
					}
				}
		putchar('\n');
		}
	charc = 0;
	printf("match:\n");
	for(i=0;i<NCH;i++){
		allprint(match[i]);
		if(charc > LINESIZE){
			putchar('\n');
			charc = 0;
			}
		}
	putchar('\n');
	return;
	}
# endif
mkmatch(){
	register int i;
	char tab[NCH];
	for(i=0; i<ccount; i++)
		tab[i] = 0;
	for(i=1;i<NCH;i++)
		if(tab[cindex[i]] == 0)
			tab[cindex[i]] = i;
	/* tab[i] = principal char for new ccl i */
	for(i = 1; i<NCH; i++)
		match[i] = tab[cindex[i]];
	return;
	}
layout(){
	/* format and output final program's tables */
	register int i, j, k;
	int  top, bot, startup, omin;
	startup = 0;
	for(i=0; i<outsize;i++)
		verify[i] = advance[i] = 0;
	omin = 0;
	yytop = 0;
	for(i=0; i<= stnum; i++){	/* for each state */
		j = gotof[i];
		if(j == -1){
			stoff[i] = 0;
			continue;
			}
		bot = j;
		while(nchar[j])j++;
		top = j - 1;
# if DEBUG
		if (debug)
			{
			printf("State %d: (layout)\n", i);
			for(j=bot; j<=top;j++)
				{
				printf("  %o", nchar[j]);
				if (j%10==0) putchar('\n');
				}
			putchar('\n');
			}
# endif
		while(verify[omin+ZCH]) omin++;
		startup = omin;
# if DEBUG
		if (debug) printf("bot,top %d, %d startup begins %d\n",bot,top,startup);
# endif
		if(chset){
			do {
				++startup;
				if(startup > outsize - ZCH)
					error("output table overflow");
				for(j = bot; j<= top; j++){
					k=startup+ctable[nchar[j]];
					if(verify[k])break;
					}
				} while (j <= top);
# if DEBUG
			if (debug) printf(" startup will be %d\n",startup);
# endif
			/* have found place */
			for(j = bot; j<= top; j++){
				k = startup + ctable[nchar[j]];
				if (ctable[nchar[j]]<=0)
				 printf("j %d nchar %d ctable.nch %d\n",j,nchar[j],ctable[nchar[k]]);
				verify[k] = i+1;			/* state number + 1*/
				advance[k] = nexts[j+1]+1;		/* state number + 1*/
				if(yytop < k) yytop = k;
				}
			}
		else {
			do {
				++startup;
				if(startup > outsize - ZCH)
					error("output table overflow");
				for(j = bot; j<= top; j++){
					k = startup + nchar[j];
					if(verify[k])break;
					}
				} while (j <= top);
			/* have found place */
# if DEBUG
	if (debug) printf(" startup going to be %d\n", startup);
# endif
			for(j = bot; j<= top; j++){
				k = startup + nchar[j];
				verify[k] = i+1;			/* state number + 1*/
				advance[k] = nexts[j+1]+1;		/* state number + 1*/
				if(yytop < k) yytop = k;
				}
			}
		stoff[i] = startup;
		}

	/* stoff[i] = offset into verify, advance for trans for state i */
	/* put out yywork */
	if(ratfor){
		fprintf(fout, "define YYTOPVAL %d\n", yytop);
		rprint(verify,"verif",yytop+1);
		rprint(advance,"advan",yytop+1);
 		shiftr(stoff, stnum); 
		rprint(stoff,"stoff",stnum+1);
 		shiftr(sfall, stnum); upone(sfall, stnum+1);
		rprint(sfall,"sfall",stnum+1);
		bprint(extra,"extra",casecount+1);
		bprint(match,"match",NCH);
 		shiftr(atable, stnum);
		rprint(atable,"atable",stnum+1);
		return;
		}
	fprintf(fout,"# define YYTYPE %s\n",stnum+1 > NCH ? "int" : "char");
	fprintf(fout,"struct yywork { YYTYPE verify, advance; } yycrank[] ={\n");
	for(i=0;i<=yytop;i+=4){
		for(j=0;j<4;j++){
			k = i+j;
			if(verify[k])
				fprintf(fout,"%d,%d,\t",verify[k],advance[k]);
			else
				fprintf(fout,"0,0,\t");
			}
		putc('\n',fout);
		}
	fprintf(fout,"0,0};\n");

	/* put out yysvec */

	fprintf(fout,"struct yysvf yysvec[] ={\n");
	fprintf(fout,"0,\t0,\t0,\n");
	for(i=0;i<=stnum;i++){	/* for each state */
		if(cpackflg[i])stoff[i] = -stoff[i];
		fprintf(fout,"yycrank+%d,\t",stoff[i]);
		if(sfall[i] != -1)
			fprintf(fout,"yysvec+%d,\t", sfall[i]+1);	/* state + 1 */
		else fprintf(fout,"0,\t\t");
		if(atable[i] != -1)
			fprintf(fout,"yyvstop+%d,",atable[i]);
		else fprintf(fout,"0,\t");
# ifdef DEBUG
		fprintf(fout,"\t\t/* state %d */",i);
# endif
		putc('\n',fout);
		}
	fprintf(fout,"0,\t0,\t0};\n");

	/* put out yymatch */
	
	fprintf(fout,"struct yywork *yytop = yycrank+%d;\n",yytop);
	fprintf(fout,"struct yysvf *yybgin = yysvec+1;\n");
	if(optim){
		fprintf(fout,"char yymatch[] ={\n");
		if (chset==0) /* no chset, put out in normal order */
			{
			for(i=0; i<NCH; i+=8){
				for(j=0; j<8; j++){
					int fbch;
					fbch = match[i+j];
					if(printable(fbch) && fbch != '\'' && fbch != '\\')
						fprintf(fout,"'%c' ,",fbch);
					else fprintf(fout,"0%-3o,",fbch);
					}
				putc('\n',fout);
				}
			}
		else
			{
			int *fbarr;
			fbarr = myalloc(2*NCH, sizeof(*fbarr));
			if (fbarr==0)
				error("No space for char table reverse",0);
			for(i=0; i<ZCH; i++)
				fbarr[i]=0;
			for(i=0; i<NCH; i++)
				fbarr[ctable[i]] = ctable[match[i]];
			for(i=0; i<ZCH; i+=8)
				{
				for(j=0; j<8; j++)
					fprintf(fout, "0%-3o,",fbarr[i+j]);
				putc('\n',fout);
				}
			cfree(fbarr, 2*NCH, 1);
			}
		fprintf(fout,"0};\n");
		}
	/* put out yyextra */
	fprintf(fout,"char yyextra[] ={\n");
	for(i=0;i<casecount;i+=8){
		for(j=0;j<8;j++)
			fprintf(fout, "%d,", i+j<NACTIONS ?
				extra[i+j] : 0);
		putc('\n',fout);
		}
	fprintf(fout,"0};\n");
	return;
	}
rprint(a,s,n)
  char *s;
  int *a, n; {
	register int i;
	fprintf(fout,"block data\n");
	fprintf(fout,"common /L%s/ %s\n",s,s);
	fprintf(fout,"define S%s %d\n",s,n);
	fprintf(fout,"integer %s (S%s)\n",s,s);
	for(i=1; i<=n; i++)
		{
		if (i%8==1) fprintf(fout, "data ");
		fprintf(fout, "%s (%d)/%d/",s,i,a[i]);
		fprintf(fout, (i%8 && i<n) ? ", " : "\n");
		}
	fprintf(fout,"end\n");
	}
shiftr(a, n)
	int *a;
{
int i;
for(i=n; i>=0; i--)
	a[i+1]=a[i];
}
upone(a,n)
	int *a;
{
int i;
for(i=0; i<=n ; i++)
	a[i]++;
}
bprint(a,s,n)
 char *s,  *a;
 int  n; {
	register int i, j, k;
	fprintf(fout,"block data\n");
	fprintf(fout,"common /L%s/ %s\n",s,s);
	fprintf(fout,"define S%s %d\n",s,n);
	fprintf(fout,"integer %s (S%s)\n",s,s);
	for(i=1;i<n;i+=8){
		fprintf(fout,"data %s (%d)/%d/",s,i,a[i]);
		for(j=1;j<8;j++){
			k = i+j;
			if(k < n)fprintf(fout,", %s (%d)/%d/",s,k,a[k]);
			}
		putc('\n',fout);
		}
	fprintf(fout,"end\n");
	}
# ifdef PP
padd(array,n)
  int **array;
  int n; {
	register int i, *j, k;
	array[n] = nxtpos;
	if(count == 0){
		*nxtpos++ = 0;
		return;
		}
	for(i=tptr-1;i>=0;i--){
		j = array[i];
		if(j && *j++ == count){
			for(k=0;k<count;k++)
				if(!tmpstat[*j++])break;
			if(k >= count){
				array[n] = array[i];
				return;
				}
			}
		}
	add(array,n);
	return;
	}
# endif
