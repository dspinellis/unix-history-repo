#
# include "../mcons.h"
# include "../ccmn.h"
# define SKIP 0
# define COLLECT 1
# define SKIP2 2

int	cross	1;
int	order	1;
char	mone	-1;

coll()
{
	cs = COLLECT;
	temp[t1].beg = &line[l];
	return;
}

save()
{
	extern	wspace();

	line[l] = '\0';
	temp[t1].ct = &line[l] - temp[t1].beg;
	temp[t1].term = c;

	if((c == ' ' || c == '\t') && cflag) {
		gch[++fl] = mone;
		flag[fl] = &wspace;
	} else {
		sav1();
	}
	return;
}

sav1()
{
	extern	only;
	struct tempent	*ptr;
	int	a,tt,val;

	if(cflag && c == '(' && level == 0)	csym();

	cs = SKIP;

	ptr = &temp[t1];
	val = search(ptr->beg,ptr->ct,&itab,0);

	if(xsw) {
	    switch(val) {
		case 0:
			if((!level&&!hlevel)||(c == '(')||xtrn
				|| ssw) {
				search(ptr->beg,ptr->ct,&xtab,1);
				goto yes;
			} else {
				if(search(ptr->beg,ptr->ct,&xtab,0))
					goto yes;
			}
			goto no;

		case 1:
			goto no;

		case 2:
			xtrn = 1;
			goto no;

		case 3:
			if(hlevel)	type = 1;
			if(!level&&!hlevel)	ssw = 1;
			goto no;

		case 4:
			if(hlevel)	type = 1;
			goto no;
	    }
	}

	if(hlevel && (val == 4 || val == 3))	type = 1;
	if(!val == !only)	goto yes;
no:
	*(ptr->beg + ptr->ct) = ptr->term;
	return(0);
yes:
	tt = t1;
	while(tt)
		if(comp(ptr->beg,temp[--tt].beg))	goto no;
	t1++;
	return(1);
}

out()
{
	auto	i,ct;
	if(cs == COLLECT)	save();

	ct = t1;
	while(ct--)
		temp[ct].beg[temp[ct].ct] = temp[ct].term;

	while(t1--) {
/*printf("t1 = %d  beg = %d  ct = %d\n",t1,temp[t1].beg,temp[t1].ct); /* DEBUG */

		switch(order) {

			case 1:
				if(utmp)
					i = 0;
				else
					i = dfile(temp[t1].beg);

				if((ct = temp[t1].ct) >= 8) {
					ct = 8;
					*curf = -1;
				} else {
					*curf = '\t';
				}

				put(i,temp[t1].beg,ct);
				put(i,curf,curfl);
				if(cross)	put(i,curs,cursl);
				conf(lno,4,lbuf);
				put(i,lbuf,5);
				break;

			case 2:
				i = dfile(curf+1);
				put(i,curf+1,curfl-1);
				if(cross)	put(i,curs,cursl);
				else {
					conf(lno,4,lbuf);
					put(i,lbuf,5);
				}
				if((ct = temp[t1].ct) >= 8) {
					put(i,temp[t1].beg,8);
					put(i,&mone,1);
				} else {
					put(i,temp[t1].beg,ct);
					put(i,"\t",1);
				}
				if(cross) {
					conf(lno,4,lbuf);
					put(i,lbuf,5);
				}
				break;

			case 3:
				i = dfile(curs);
				put(i,curs,cursl);
				if((ct = temp[t1].ct) >= 8) {
					put(i,temp[t1].beg,8);
					*curf = -1;
				} else {
					put(i,temp[t1].beg,ct);
					*curf = '\t';
				}
				put(i,curf,curfl);
				conf(lno,4,lbuf);
				put(i,lbuf,5);
		}
		put(i,line,l + 1);

	}
	t1 = 0;
	l = -1;
	lno++;
	cs = SKIP;

	return;
}


asym()
{
	int	i;
	char	*p;

	if(cs == COLLECT) {
		if(cross) {
			p = temp[t1].beg;
			cursl = &line[l] - p;
			cursl = cursl>8?8:cursl;
			i = -1;
			while(++i < cursl)
				curs[i] = *p++;
			if(cursl < 8)
				curs[cursl++] = '\t';
			else
				curs[cursl++] = -1;
		}
		save();
	}
	cs = SKIP;
}

asw()
{
	switch(gch[fl]) {

		case 0:
			if(cs == COLLECT)	save();
			cs = SKIP;
			flag[++fl] = &asw;
			gch[fl] = c;
			return(1);

		case '\'':
			if(c == '\\') {
				gch[fl] = c;
				return(1);
			}
			break;

		case '"':
			gch[fl] = '\'';

			if(c == '\\') {
				flag[++fl] = &asw;
				gch[fl] = c;
				return(1);
			}
			return(1);

		case '<':
			if(c == '\n')	out();
			if(c == '\\') {
				flag[++fl] = &asw;
				gch[fl] = c;
				return(1);
			}
			if(c != '>')	return(1);
			break;

		case '/':
			if(c != '\n')	return(1);

		case '\\': 
			if(c == '\n')	out();

	}
	fl--;
	return(1);

}

csw()
{
	if(cs == COLLECT)	save();

	switch(gch[fl]) {

		case 0:
			if(c == '*')
				if(line[l - 1] != '/')
					return(1);
			gch[++fl] = c;
			flag[fl] = &csw;
			return(1);

		case '*':
			if(c == '\n')	out();
			if(c == '/' && line[l - 1] == '*')
				break;
			return(1);

		case '\'':
		case '"':
			if(c == gch[fl])
				break;
			if(c == '\\') {
				gch[++fl] = c;
				flag[fl] = &csw;
			}
			return(1);

		case '\\':
			break;
		}
		fl--;
		return(1);
}

incl()
{
/*	printf("incl: l = %d hl = %d dl = %d\n",level,hlevel,dlevel);/*DEBUG*/
	if(cs == COLLECT)	save();
	if(hlevel) {
		hlevel = 0;
		level++;
	} else {
		dlevel++;
	}

	cs = SKIP;
}

decl()
{
/*	printf("decl: l = %d hl = %d dl = %d\n",level,hlevel,dlevel);/*DEBUG*/
	if(cs == COLLECT)	save();
	cs = SKIP;
	if(dlevel) {
		dlevel--;
		return;
	}
	if(--level > 0)	return;
	curs[0] = '_';
	curs[1] = '\t';
	cursl = 2;
	level = 0;
	return;
}

csym()
{
	int	i;
	char	*p;

/*	printf("csym: l = %d hl = %d dl = %d\n",level,hlevel,dlevel);/*DEBUG*/
	p = temp[t1].beg;
	if(cs == COLLECT && level == 0) {
		if(cross) {
			cursl = temp[t1].ct;
			cursl = cursl>8?8:cursl;
			i = -1;
			while(++i < cursl)
				curs[i] = *p++;
			if(cursl < 8)
				curs[cursl++] = '\t';
			else
				curs[cursl++] = -1;
		}
		hlevel = 1;
	}
	cs = SKIP;
}

dfile(a)
	char	*a;
{
	if(*a < 'c')	return(0);
	if(*a < 'h')	return(1);
	if(*a < 'r')	return(2);
	return(3);
}


sk2()
{
	cs = SKIP2;
}

sk()
{
	cs = SKIP;
}

tabs()
{
	if(l == 0)	l = -1;
}


search(symbol,length,params,install)
	char	*symbol;
	int	length;
	struct	htab	*params;
	int	install;
{
	char	*sp,*p;
	static	int	curb,*hptr,hsiz,nsym,ssiz;
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
			printf("Too many symbols.\n");
			dexit();
		}

		hptr[h] = curb;
		length++;
		if((curb + length) >= ssiz) {
			printf("Too many characters in symbols.\n");
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

semi()
{
	if(cs == COLLECT)	save();
	if(only)	return;
	xtrn = 0;
	if(!level) {
		ssw = 0;
		if(!type) {
			hlevel = 0;
			curs[0] = '_';
			curs[1] = '\t';
			cursl = 2;
		}
		type = 0;
	}
	cs = SKIP;
}

wspace()
{
	if(c == ' ' || c == '\t')
		return(1);
	sav1();
	fl--;
	return(0);
}

