#
# include "econs.h"
# include "ecmn.h"
# define SKIP 0
# define COLLECT 1
# define SKIP2 2

char	mone	-1;
int	tlno	1;

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
	int	a,tt,val;

	cs = SKIP;
	line[l] = '\0';
	temp[t1].ct = &line[l] - temp[t1].beg;
	if(!count)
		if(temp[t1].ct == 1)	goto no;
	pt1 = temp[t1].beg-1;
	pt2 = cbuf-1;

	while(*++pt2 = *++pt1)
		if(*pt2 >= 'A' && *pt2 <= 'Z')
			*pt2 =| 040;

	if(count)
		goto yes;
	val = search(cbuf,temp[t1].ct,&itab,0);

	if(!val == !only)	goto yes;
no:
	line[l] = c;
	return(0);
yes:
	if(count == 0) {
		tt = t1;
		while(tt)
			if(comp(temp[t1].beg,temp[--tt].beg))	goto no;
	}
	temp[t1++].term = c;
	return(1);
}

out()
{
	auto	i,ct,t2;
	char	*b,*e;
	if(cs == COLLECT)	save();
	ct = t1;
	while(ct--)
		temp[ct].beg[temp[ct].ct] = temp[ct].term;
	t2 = t1 - 1;
	while(t1--) {
/*printf("t1 = %d  beg = %o  ct = %d\n",t1,temp[t1].beg,temp[t1].ct); /* DEBUG */
		if((ct = temp[t1].ct) >= 15) {
			ct = 15;
		}
		put(temp[t1].beg,ct);
		if(count) {
			put("\n", 1);
			continue;
		}

		put("\t",1);

		if(!page) {
			put(curf,curfl);
			conf(lno,4,curs);
			put(curs,4);
		} else {
			conf(pn,4,curs);
			put(curs,4);
		}
		if(word == 0) {
			put("\t",1);
			if(t1 >= 1)
				b = temp[t1-1].beg;
			else
				b =  line;
			if(t2 > t1)
				e = temp[t1+1].beg + temp[t1+1].ct;
			else
			e = &line[l];
/*printf("e = %o	b = %o\n",e,b);	/*DEBUG*/
			put(b,e-b);
		}
		put("\n",1);
	}
	t1 = 0;
	l = -1;
	lno =+ tlno;
	tlno = 1;
	cs = SKIP;
	return;
}


search(symbol,length,params,install)
	char	*symbol;
	int	length;
	struct	htab	*params;
	int	install;
{
	char	*sp,*p;
	static	int	*hptr,hsiz,nsym;
	static	char	*ssiz;
	static	int	curb;
	static	char	*symt;
	auto	h,i,j,k;

	if(hptr != params->hptr) {
		hptr = params->hptr;
		hsiz = params->hsiz;
		symt = params->symt;
		ssiz = params->ssiz;
		curb = params->curb;
		nsym = params->nsym;
	}

	symbol[length] = '\0';
/*printf("ssiz = %d; nsym = %d; %s\n", ssiz, nsym, symbol);/*DEBUG*/
	sp = symbol;

	i = length;
	h = 1;
	while(i--)
		h =* *sp++;

	if(h == 0100000) {
		h = 1;
	} else {
		h = h<0?(-h)%hsiz:h%hsiz;
	}
	if(h == 0)	h++;
/*		printf("%s %d\n",symbol,h);	/*DEBUG*/

	while((p = &symt[hptr[h]]) > symt) {
		j = length + 2;
		sp = symbol;
		while(--j) {
			if(*p++ != *sp++)	goto no;
		}
		return(*p);
no:
		h = (h + h)%hsiz;
	}
	if(install) {
		if(++nsym >= hsiz) {
			printf("Too many symbols in ignore/only file.\n");
			dexit();
		}

		hptr[h] = curb;
		length++;
		if((curb + length) >= ssiz) {
			printf("i/o file too big; ssiz = %d\n", ssiz);
			dexit();
		}

		while(length--)
			symt[curb++] = *symbol++;
		symt[curb++] = install;
		params->curb = curb;
		params->nsym = nsym;
	}
	return(0);
}

conf(n,width,buf) 
	char	*buf;
{
	auto	i,a;

	i = width;
	while(i--)	buf[i] = ' ';

	buf[(a = n/10)?conf(a,--width,buf):--width] = n%10 + '0';

	return(++width);
}

comp(a,b)
	char	*a;
	char	*b;
{
	a--;
	b--;
	while(*++a == *++b)
		if(*a == '\0')	return(1);
	return(0);
}


hyphen()
{
/*	printf("hyphen\n");	/*DEBUG*/
	if(gch[fl] == 0)
		flag[++fl] = &hyp1;
	return(1);
}

hyp1()
{
/*	printf("hyp1 c = %o\n",c);	/*DEBUG*/
	if(c !=  '\n') {
		fl--;
		return(0);
	} else {
		l =- 2;
		flag[fl] = &hyp2;
		hsw = 1;
		return(1);
	}
}

hyp2()
{
/*	printf("hyp2 c = %o l = %d\n",c,l);	/*DEBUG*/
	if(hsw && (tab[2].cl[c] == 0)) {
		l--;
		if(c == 3)	pno();
		if(c == '\n')	tlno++;
		return(1);
	}
	hsw = 0;
	if(tab[cs].cl[c]) {
		line[l] = '\n';
		out();
		fl--;
		return(0);
	}
	return(1);
}

pno()
{
	if(flag[fl] != &pno) {
		flag[++fl] = &pno;
		pn = 0;
		return(1);
	}
	if(c == '\n') {
		fl--;
		out();
		return(1);
	}
	pn = pn*10 + c - '0';
	return(1);
}
