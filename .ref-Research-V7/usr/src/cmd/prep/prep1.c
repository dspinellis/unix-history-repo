# include <stdio.h>
# include "prep.h"
# define SKIP 0
# define COLLECT 1
# define SKIP2 2

int	tlno = 1;

coll()
{
	cs = COLLECT;
	temp[t1].beg = &line[l];
	return;
}

save()
{
	extern	only;
	char	*pt1,*pt2,cbuf[30];
	int	val;

	if(cs != COLLECT) {
		cs = SKIP;
		return;
	}
	cs = SKIP;
	line[l] = '\0';
	temp[t1].ct = &line[l] - temp[t1].beg;
	pt1 = temp[t1].beg-1;
	pt2 = cbuf-1;

	wdnum++;
	while(*++pt2 = *++pt1)
		if(*pt2 >= 'A' && *pt2 <= 'Z')
			*pt2 |= 040;

	if(igflg){
		val = search(cbuf,temp[t1].ct,&itab,0);
		if(!val == !only) goto yes;

		line[l] = c;
		return;
	}
yes:
	temp[t1++].wdno = wdnum;
	line[l] = c;
	return;
}

ctout()
{
	register int	ct;
	register char	*t2;
	int	t3;
	long	xxx;

	if(lflag) {
		fprintf(stderr, "line too long:  line %d\n",lno);
		lflag = 0;
	}
	if(cs == COLLECT)	save();

	t3 = t1;
	t1 = -1;
	while(++t1 < t3) {
		if(wdflg){
			xxx = temp[t1].wdno;
			conf(xxx, WIDTH, num);
			put(num,WIDTH+1);
		}
		ct = temp[t1].ct;
		t2 = temp[t1].beg - 1;
/*		fprintf(stderr, "out: %s	%d\n", temp[t1].beg, ct);	/*DEBUG*/
		while(ct--)
			if(*++t2 >= 'A' && *t2 <= 'Z')
				*t2 |= 040;

		ct = temp[t1].ct;
		while(*--t2 == '\'' && san == 0)
			ct--;

		put(temp[t1].beg, ct);
		put("\n", 1);
	}
	t1 = 0;
	l = -1;
	lno += tlno;
	tlno = 1;
	cs = SKIP;
	return;
}

conf(n,width,buf) 
	long n;
	char	*buf;
{
	long	a;
	auto i;

	i = width;
	while(i--)	buf[i] = ' ';

	a = n/10;
	if(a)
		width = conf(a, --width, buf);
	else
		width--;
	buf[width] = n%10 + '0';

	return(++width);
}

hyphen()
{
/*	fprintf(stderr, "hyphen\n");	/*DEBUG*/
	flag[++fl] = hyp1;
	return(1);
}

hyp1()
{
/*	fprintf(stderr, "hyp1 c = %o\n",c);	/*DEBUG*/
	if(c !=  '\n') {
		fl--;
		l--;
		save();
		l++;
		punc();
		return(0);
	} else {
		l -= 2;
		flag[fl] = hyp2;
		hsw = 1;
		return(1);
	}
}

hyp2()
{
	extern	(*acts[])();
/*	fprintf(stderr, "hyp2 c = %o l = %d\n",c,l);	/*DEBUG*/
	if(hsw && (tab[2][c] == 0)) {
		l--;
		if(c == '\n')	tlno++;
		return(1);
	}
	hsw = 0;
	if(c == '\n'){
		l--;
		return(1);
	}
	if(tab[cs][c]) {
		line[l] = '\n';
		(*acts[OUT])();
		fl--;
		return(0);
	}
	return(1);
}

gobble2()
{
	static	ct2;

	if(cs == COLLECT)	save();

	if(flag[fl] != gobble2) {
		ct2 = 1;
		flag[++fl] = gobble2;
		return(1);
	}
	if(ct2--)	return(1);

	fl--;
	cs = SKIP;
	return(1);
}

bslash()
{
	if(cs == COLLECT)	save();
	cs = SKIP2;
	return(1);
}

bsp()
{
	flag[++fl] = bsp1;
	return(1);
}

bsp1()
{
	fl--;
	if(c == '"')	return(1);

	line[--l] = c;
	return(0);
}

punc()
{

	if(cs == COLLECT)
		save();
	if(puncfl) {
		temp[t1].beg = &line[l];
		temp[t1].ct = 1;
		temp[t1++].wdno = 0;
	}
}

search(symbol,length,params,install)
	char	*symbol;
	int	length;
	struct	htab	*params;
	int	install;
{
	char	*sp,*p;
	static	int	*hptr,hsiz,nsym;
	static	int	ssiz;
	static	int	curb;
	static	char	*symt;
	auto	h,i,j;

	if(hptr != params->hptr) {
		hptr = params->hptr;
		hsiz = params->hsiz;
		symt = params->symt;
		ssiz = params->ssiz;
		curb = params->curb;
		nsym = params->nsym;
	}

	symbol[length] = '\0';
/*fprintf(stderr, "ssiz = %d; nsym = %d; %s\n", ssiz, nsym, symbol);/*DEBUG*/
	sp = symbol;

	i = length;
	h = 1;
	while(i--)
		h *= *sp++;

	if(h == 0100000) {
		h = 1;
	} else {
		h = h<0?(-h)%hsiz:h%hsiz;
	}
	if(h == 0)	h++;
/*		fprintf(stderr, "%s %d\n",symbol,h);	/*DEBUG*/

	while((p = &symt[hptr[h]]) > symt) {
		j = length + 2;
		sp = symbol;
		while(--j) {
			if(*p++ != *sp++)	goto no;
		}
		return(*p);
no:
		h++;
		if(h >= hsiz)	h -= hsiz;
	}
	if(install) {
		if(++nsym >= hsiz) {
			fprintf(stderr, "Too many symbols in ignore/only file.\n");
			exit(1);
		}

		hptr[h] = curb;
		length++;
		if((curb + length) >= ssiz) {
			fprintf(stderr, "i/o file too big; ssiz = %d\n", ssiz);
			exit(1);
		}

		while(length--)
			symt[curb++] = *symbol++;
		symt[curb++] = install;
		params->curb = curb;
		params->nsym = nsym;
	}
	return(0);
}
