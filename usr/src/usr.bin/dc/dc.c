#ifndef lint
static char sccsid[] = "@(#)dc.c	4.7	(Berkeley)	%G%";
#endif not lint

#include <paths.h>
#include <sys/signal.h>
#include <stdio.h>
#include "dc.h"

main(argc,argv)
int argc;
char *argv[];
{
	init(argc,argv);
	commnds();
}
commnds(){
	register int c;
	register struct blk *p,*q;
	long l;
	int sign;
	struct blk **ptr,*s,*t;
	struct sym *sp;
	int sk,sk1,sk2;
	int n,d;

	while(1){
		if(((c = readc())>='0' && c <= '9')|| (c>='A' && c <='F') || c == '.'){
			unreadc(c);
			p = readin();
			pushp(p);
			continue;
		}
		switch(c){
		case ' ':
		case '\n':
		case 0377:
		case EOF:
			continue;
		case 'Y':
			sdump("stk",*stkptr);
			printf("all %ld rel %ld headmor %ld\n",all,rel,headmor);
			printf("nbytes %ld\n",nbytes);
			continue;
		case '_':
			p = readin();
			savk = sunputc(p);
			chsign(p);
			sputc(p,savk);
			pushp(p);
			continue;
		case '-':
			subt();
			continue;
		case '+':
			if(eqk() != 0)continue;
			binop('+');
			continue;
		case '*':
			arg1 = pop();
			EMPTY;
			arg2 = pop();
			EMPTYR(arg1);
			sk1 = sunputc(arg1);
			sk2 = sunputc(arg2);
			binop('*');
			p = pop();
			sunputc(p);
			savk = n = sk1+sk2;
			if(n>k && n>sk1 && n>sk2){
				sk = sk1;
				if(sk<sk2)sk = sk2;
				if(sk<k)sk = k;
				p = removc(p,n-sk);
				savk = sk;
			}
			sputc(p,savk);
			pushp(p);
			continue;
		case '/':
casediv:
			if(dscale() != 0)continue;
			binop('/');
			if(irem != 0)release(irem);
			release(rem);
			continue;
		case '%':
			if(dscale() != 0)continue;
			binop('/');
			p = pop();
			release(p);
			if(irem == 0){
				sputc(rem,skr+k);
				pushp(rem);
				continue;
			}
			p = add0(rem,skd-(skr+k));
			q = add(p,irem);
			release(p);
			release(irem);
			sputc(q,skd);
			pushp(q);
			continue;
		case 'v':
			p = pop();
			EMPTY;
			savk = sunputc(p);
			if(length(p) == 0){
				sputc(p,savk);
				pushp(p);
				continue;
			}
			if((c = sbackc(p))<0){
				error("sqrt of neg number\n");
			}
			if(k<savk)n = savk;
			else{
				n = k*2-savk;
				savk = k;
			}
			arg1 = add0(p,n);
			arg2 = sqrt(arg1);
			sputc(arg2,savk);
			pushp(arg2);
			continue;
		case '^':
			neg = 0;
			arg1 = pop();
			EMPTY;
			if(sunputc(arg1) != 0)error("exp not an integer\n");
			arg2 = pop();
			EMPTYR(arg1);
			if(sfbeg(arg1) == 0 && sbackc(arg1)<0){
				neg++;
				chsign(arg1);
			}
			if(length(arg1)>=3){
				error("exp too big\n");
			}
			savk = sunputc(arg2);
			p = exp(arg2,arg1);
			release(arg2);
			rewind(arg1);
			c = sgetc(arg1);
			if(sfeof(arg1) == 0)
				c = sgetc(arg1)*100 + c;
			d = c*savk;
			release(arg1);
			if(neg == 0){
				if(k>=savk)n = k;
				else n = savk;
				if(n<d){
					q = removc(p,d-n);
					sputc(q,n);
					pushp(q);
				}
				else {
					sputc(p,d);
					pushp(p);
				}
			}
			else {
				sputc(p,d);
				pushp(p);
			}
			if(neg == 0)continue;
			p = pop();
			q = salloc(2);
			sputc(q,1);
			sputc(q,0);
			pushp(q);
			pushp(p);
			goto casediv;
		case 'z':
			p = salloc(2);
			n = stkptr - stkbeg;
			if(n >= 100){
				sputc(p,n/100);
				n %= 100;
			}
			sputc(p,n);
			sputc(p,0);
			pushp(p);
			continue;
		case 'Z':
			p = pop();
			EMPTY;
			n = (length(p)-1)<<1;
			fsfile(p);
			sbackc(p);
			if(sfbeg(p) == 0){
				if((c = sbackc(p))<0){
					n -= 2;
					if(sfbeg(p) == 1)n += 1;
					else {
						if((c = sbackc(p)) == 0)n += 1;
						else if(c > 90)n -= 1;
					}
				}
				else if(c < 10) n -= 1;
			}
			release(p);
			q = salloc(1);
			if(n >= 100){
				sputc(q,n%100);
				n /= 100;
			}
			sputc(q,n);
			sputc(q,0);
			pushp(q);
			continue;
		case 'i':
			p = pop();
			EMPTY;
			p = scalint(p);
			release(inbas);
			inbas = p;
			continue;
		case 'I':
			p = copy(inbas,length(inbas)+1);
			sputc(p,0);
			pushp(p);
			continue;
		case 'o':
			p = pop();
			EMPTY;
			p = scalint(p);
			sign = 0;
			n = length(p);
			q = copy(p,n);
			fsfile(q);
			l = c = sbackc(q);
			if(n != 1){
				if(c<0){
					sign = 1;
					chsign(q);
					n = length(q);
					fsfile(q);
					l = c = sbackc(q);
				}
				if(n != 1){
					while(sfbeg(q) == 0)l = l*100+sbackc(q);
				}
			}
			logo = log2(l);
			obase = l;
			release(basptr);
			if(sign == 1)obase = -l;
			basptr = p;
			outdit = bigot;
			if(n == 1 && sign == 0){
				if(c <= 16){
					outdit = hexot;
					fw = 1;
					fw1 = 0;
					ll = 70;
					release(q);
					continue;
				}
			}
			n = 0;
			if(sign == 1)n++;
			p = salloc(1);
			sputc(p,-1);
			t = add(p,q);
			n += length(t)*2;
			fsfile(t);
			if((c = sbackc(t))>9)n++;
			release(t);
			release(q);
			release(p);
			fw = n;
			fw1 = n-1;
			ll = 70;
			if(fw>=ll)continue;
			ll = (70/fw)*fw;
			continue;
		case 'O':
			p = copy(basptr,length(basptr)+1);
			sputc(p,0);
			pushp(p);
			continue;
		case '[':
			n = 0;
			p = salloc(0);
			while(1){
				if((c = readc()) == ']'){
					if(n == 0)break;
					n--;
				}
				sputc(p,c);
				if(c == '[')n++;
			}
			pushp(p);
			continue;
		case 'k':
			p = pop();
			EMPTY;
			p = scalint(p);
			if(length(p)>1){
				error("scale too big\n");
			}
			rewind(p);
			k = sfeof(p)?0:sgetc(p);
			release(scalptr);
			scalptr = p;
			continue;
		case 'K':
			p = copy(scalptr,length(scalptr)+1);
			sputc(p,0);
			pushp(p);
			continue;
		case 'X':
			p = pop();
			EMPTY;
			fsfile(p);
			n = sbackc(p);
			release(p);
			p = salloc(2);
			sputc(p,n);
			sputc(p,0);
			pushp(p);
			continue;
		case 'Q':
			p = pop();
			EMPTY;
			if(length(p)>2){
				error("Q?\n");
			}
			rewind(p);
			if((c =  sgetc(p))<0){
				error("neg Q\n");
			}
			release(p);
			while(c-- > 0){
				if(readptr == &readstk[0]){
					error("readstk?\n");
				}
				if(*readptr != 0)release(*readptr);
				readptr--;
			}
			continue;
		case 'q':
			if(readptr <= &readstk[1])exit(0);
			if(*readptr != 0)release(*readptr);
			readptr--;
			if(*readptr != 0)release(*readptr);
			readptr--;
			continue;
		case 'f':
			if(stkptr == &stack[0])printf("empty stack\n");
			else {
				for(ptr = stkptr; ptr > &stack[0];){
					print(*ptr--);
				}
			}
			continue;
		case 'p':
			if(stkptr == &stack[0])printf("empty stack\n");
			else{
				print(*stkptr);
			}
			continue;
		case 'P':
			p = pop();
			EMPTY;
			sputc(p,0);
			printf("%s",p->beg);
			release(p);
			continue;
		case 'd':
			if(stkptr == &stack[0]){
				printf("empty stack\n");
				continue;
			}
			q = *stkptr;
			n = length(q);
			p = copy(*stkptr,n);
			pushp(p);
			continue;
		case 'c':
			while(stkerr == 0){
				p = pop();
				if(stkerr == 0)release(p);
			}
			continue;
		case 'S':
			if(stkptr == &stack[0]){
				error("save: args\n");
			}
			c = readc() & 0377;
			sptr = stable[c];
			sp = stable[c] = sfree;
			sfree = sfree->next;
			if(sfree == 0)goto sempty;
			sp->next = sptr;
			p = pop();
			EMPTY;
			if(c >= ARRAYST){
				q = copy(p,PTRSZ);
				for(n = 0;n < PTRSZ-1;n++)sputc(q,0);
				release(p);
				p = q;
			}
			sp->val = p;
			continue;
sempty:
			error("symbol table overflow\n");
		case 's':
			if(stkptr == &stack[0]){
				error("save:args\n");
			}
			c = readc() & 0377;
			sptr = stable[c];
			if(sptr != 0){
				p = sptr->val;
				if(c >= ARRAYST){
					rewind(p);
					while(sfeof(p) == 0)release(getwd(p));
				}
				release(p);
			}
			else{
				sptr = stable[c] = sfree;
				sfree = sfree->next;
				if(sfree == 0)goto sempty;
				sptr->next = 0;
			}
			p = pop();
			sptr->val = p;
			continue;
		case 'l':
			load();
			continue;
		case 'L':
			c = readc() & 0377;
			sptr = stable[c];
			if(sptr == 0){
				error("L?\n");
			}
			stable[c] = sptr->next;
			sptr->next = sfree;
			sfree = sptr;
			p = sptr->val;
			if(c >= ARRAYST){
				rewind(p);
				while(sfeof(p) == 0){
					q = getwd(p);
					if(q != 0)release(q);
				}
			}
			pushp(p);
			continue;
		case ':':
			p = pop();
			EMPTY;
			q = scalint(p);
			fsfile(q);
			c = 0;
			if((sfbeg(q) == 0) && ((c = sbackc(q))<0)){
				error("neg index\n");
			}
			if(length(q)>2){
				error("index too big\n");
			}
			if(sfbeg(q) == 0)c = c*100+sbackc(q);
			if(c >= MAXIND){
				error("index too big\n");
			}
			release(q);
			n = readc() & 0377;
			sptr = stable[n];
			if(sptr == 0){
				sptr = stable[n] = sfree;
				sfree = sfree->next;
				if(sfree == 0)goto sempty;
				sptr->next = 0;
				p = salloc((c+PTRSZ)*PTRSZ);
				zero(p);
			}
			else{
				p = sptr->val;
				if(length(p)-PTRSZ < c*PTRSZ){
					q = copy(p,(c+PTRSZ)*PTRSZ);
					release(p);
					p = q;
				}
			}
			seekc(p,c*PTRSZ);
			q = lookwd(p);
			if (q!=NULL) release(q);
			s = pop();
			EMPTY;
			salterwd(p,s);
			sptr->val = p;
			continue;
		case ';':
			p = pop();
			EMPTY;
			q = scalint(p);
			fsfile(q);
			c = 0;
			if((sfbeg(q) == 0) && ((c = sbackc(q))<0)){
				error("neg index\n");
			}
			if(length(q)>2){
				error("index too big\n");
			}
			if(sfbeg(q) == 0)c = c*100+sbackc(q);
			if(c >= MAXIND){
				error("index too big\n");
			}
			release(q);
			n = readc() & 0377;
			sptr = stable[n];
			if(sptr != 0){
				p = sptr->val;
				if(length(p)-PTRSZ >= c*PTRSZ){
					seekc(p,c*PTRSZ);
					s = getwd(p);
					if(s != 0){
						q = copy(s,length(s));
						pushp(q);
						continue;
					}
				}
			}
			q = salloc(PTRSZ);
			putwd(q, (struct blk *)0);
			pushp(q);
			continue;
		case 'x':
execute:
			p = pop();
			EMPTY;
			if((readptr != &readstk[0]) && (*readptr != 0)){
				if((*readptr)->rd == (*readptr)->wt)
					release(*readptr);
				else{
					if(readptr++ == &readstk[RDSKSZ]){
						error("nesting depth\n");
					}
				}
			}
			else readptr++;
			*readptr = p;
			if(p != 0)rewind(p);
			else{
				if((c = readc()) != '\n')unreadc(c);
			}
			continue;
		case '?':
			if(++readptr == &readstk[RDSKSZ]){
				error("nesting depth\n");
			}
			*readptr = 0;
			fsave = curfile;
			curfile = stdin;
			while((c = readc()) == '!')command();
			p = salloc(0);
			sputc(p,c);
			while((c = readc()) != '\n'){
				sputc(p,c);
				if(c == '\\')sputc(p,readc());
			}
			curfile = fsave;
			*readptr = p;
			continue;
		case '!':
			if(command() == 1)goto execute;
			continue;
		case '<':
		case '>':
		case '=':
			if(cond(c) == 1)goto execute;
			continue;
		default:
			printf("%o is unimplemented\n",c);
		}
	}
}
struct blk *
div(ddivd,ddivr)
struct blk *ddivd,*ddivr;
{
	int divsign,remsign,offset,divcarry;
	int carry, dig,magic,d,dd;
	long c,td,cc;
	struct blk *ps;
	register struct blk *p,*divd,*divr;

	rem = 0;
	p = salloc(0);
	if(length(ddivr) == 0){
		pushp(ddivr);
		printf("divide by 0\n");
		return((struct blk *)1);
	}
	divsign = remsign = 0;
	divr = ddivr;
	fsfile(divr);
	if(sbackc(divr) == -1){
		divr = copy(ddivr,length(ddivr));
		chsign(divr);
		divsign = ~divsign;
	}
	divd = copy(ddivd,length(ddivd));
	fsfile(divd);
	if(sfbeg(divd) == 0 && sbackc(divd) == -1){
		chsign(divd);
		divsign = ~divsign;
		remsign = ~remsign;
	}
	offset = length(divd) - length(divr);
	if(offset < 0)goto ddone;
	seekc(p,offset+1);
	sputc(divd,0);
	magic = 0;
	fsfile(divr);
	c = sbackc(divr);
	if(c<10)magic++;
	c = c*100 + (sfbeg(divr)?0:sbackc(divr));
	if(magic>0){
		c = (c*100 +(sfbeg(divr)?0:sbackc(divr)))*2;
		c /= 25;
	}
	while(offset >= 0){
		fsfile(divd);
		td = sbackc(divd)*100;
		dd = sfbeg(divd)?0:sbackc(divd);
		td = (td+dd)*100;
		dd = sfbeg(divd)?0:sbackc(divd);
		td = td+dd;
		cc = c;
		if(offset == 0)td += 1;
		else cc += 1;
		if(magic != 0)td = td<<3;
		dig = td/cc;
		rewind(divr);
		rewind(divxyz);
		carry = 0;
		while(sfeof(divr) == 0){
			d = sgetc(divr)*dig+carry;
			carry = d / 100;
			salterc(divxyz,d%100);
		}
		salterc(divxyz,carry);
		rewind(divxyz);
		seekc(divd,offset);
		carry = 0;
		while(sfeof(divd) == 0){
			d = slookc(divd);
			d = d-(sfeof(divxyz)?0:sgetc(divxyz))-carry;
			carry = 0;
			if(d < 0){
				d += 100;
				carry = 1;
			}
			salterc(divd,d);
		}
		divcarry = carry;
		sbackc(p);
		salterc(p,dig);
		sbackc(p);
		if(--offset >= 0){
			if(d > 0){
				sbackc(divd);
				dd=sbackc(divd);
				salterc(divd,dd+100);
			}
			divd->wt--;
		}
	}
	if(divcarry != 0){
		salterc(p,dig-1);
		salterc(divd,-1);
		ps = add(divr,divd);
		release(divd);
		divd = ps;
	}

	rewind(p);
	divcarry = 0;
	while(sfeof(p) == 0){
		d = slookc(p)+divcarry;
		divcarry = 0;
		if(d >= 100){
			d -= 100;
			divcarry = 1;
		}
		salterc(p,d);
	}
	if(divcarry != 0)salterc(p,divcarry);
	fsfile(p);
	while(sfbeg(p) == 0){
		if(sbackc(p) == 0)truncate(p);
		else break;
	}
	if(divsign < 0)chsign(p);
	fsfile(divd);
	while(sfbeg(divd) == 0){
		if(sbackc(divd) == 0)truncate(divd);
		else break;
	}
ddone:
	if(remsign<0)chsign(divd);
	if(divr != ddivr)release(divr);
	rem = divd;
	return(p);
}
dscale(){
	register struct blk *dd,*dr;
	register struct blk *r;
	int c;

	dr = pop();
	EMPTYS;
	dd = pop();
	EMPTYSR(dr);
	fsfile(dd);
	skd = sunputc(dd);
	fsfile(dr);
	skr = sunputc(dr);
	if(sfbeg(dr) == 1 || (sfbeg(dr) == 0 && sbackc(dr) == 0)){
		sputc(dr,skr);
		pushp(dr);
		errorrt("divide by 0\n");
	}
	c = k-skd+skr;
	if(c < 0)r = removr(dd,-c);
	else {
		r = add0(dd,c);
		irem = 0;
	}
	arg1 = r;
	arg2 = dr;
	savk = k;
	return(0);
}
struct blk *
removr(p,n)
struct blk *p;
{
	int nn;
	register struct blk *q,*s,*r;

	rewind(p);
	nn = (n+1)/2;
	q = salloc(nn);
	while(n>1){
		sputc(q,sgetc(p));
		n -= 2;
	}
	r = salloc(2);
	while(sfeof(p) == 0)sputc(r,sgetc(p));
	release(p);
	if(n == 1){
		s = div(r,tenptr);
		release(r);
		rewind(rem);
		if(sfeof(rem) == 0)sputc(q,sgetc(rem));
		release(rem);
		irem = q;
		return(s);
	}
	irem = q;
	return(r);
}
struct blk *
sqrt(p)
struct blk *p;
{
	struct blk *t;
	struct blk *r,*q,*s;
	int c,n,nn;

	n = length(p);
	fsfile(p);
	c = sbackc(p);
	if((n&1) != 1)c = c*100+(sfbeg(p)?0:sbackc(p));
	n = (n+1)>>1;
	r = salloc(n);
	zero(r);
	seekc(r,n);
	nn=1;
	while((c -= nn)>=0)nn+=2;
	c=(nn+1)>>1;
	fsfile(r);
	sbackc(r);
	if(c>=100){
		c -= 100;
		salterc(r,c);
		sputc(r,1);
	}
	else salterc(r,c);
	while(1){
		q = div(p,r);
		s = add(q,r);
		release(q);
		release(rem);
		q = div(s,sqtemp);
		release(s);
		release(rem);
		s = copy(r,length(r));
		chsign(s);
		t = add(s,q);
		release(s);
		fsfile(t);
		nn = sfbeg(t)?0:sbackc(t);
		if(nn>=0)break;
		release(r);
		release(t);
		r = q;
	}
	release(t);
	release(q);
	release(p);
	return(r);
}
struct blk *
exp(base,ex)
struct blk *base,*ex;
{
	register struct blk *r,*e,*p;
	struct blk *e1,*t,*cp;
	int temp,c,n;
	r = salloc(1);
	sputc(r,1);
	p = copy(base,length(base));
	e = copy(ex,length(ex));
	fsfile(e);
	if(sfbeg(e) != 0)goto edone;
	temp=0;
	c = sbackc(e);
	if(c<0){
		temp++;
		chsign(e);
	}
	while(length(e) != 0){
		e1=div(e,sqtemp);
		release(e);
		e = e1;
		n = length(rem);
		release(rem);
		if(n != 0){
			e1=mult(p,r);
			release(r);
			r = e1;
		}
		t = copy(p,length(p));
		cp = mult(p,t);
		release(p);
		release(t);
		p = cp;
	}
	if(temp != 0){
		if((c = length(base)) == 0){
			goto edone;
		}
		if(c>1)create(r);
		else{
			rewind(base);
			if((c = sgetc(base))<=1){
				create(r);
				sputc(r,c);
			}
			else create(r);
		}
	}
edone:
	release(p);
	release(e);
	return(r);
}
init(argc,argv)
int argc;
char *argv[];
{
	register struct sym *sp;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT,onintr);
	setbuf(stdout,(char *)NULL);
	svargc = --argc;
	svargv = argv;
	while(svargc>0 && svargv[1][0] == '-'){
		switch(svargv[1][1]){
		default:
			dbg=1;
		}
		svargc--;
		svargv++;
	}
	ifile=1;
	if(svargc<=0)curfile = stdin;
	else if((curfile = fopen(svargv[1],"r")) == NULL){
		printf("can't open file %s\n",svargv[1]);
		exit(1);
		}
	scalptr = salloc(1);
	sputc(scalptr,0);
	basptr = salloc(1);
	sputc(basptr,10);
	obase=10;
	log10=log2(10L);
	ll=70;
	fw=1;
	fw1=0;
	tenptr = salloc(1);
	sputc(tenptr,10);
	obase=10;
	inbas = salloc(1);
	sputc(inbas,10);
	sqtemp = salloc(1);
	sputc(sqtemp,2);
	chptr = salloc(0);
	strptr = salloc(0);
	divxyz = salloc(0);
	stkbeg = stkptr = &stack[0];
	stkend = &stack[STKSZ];
	stkerr = 0;
	readptr = &readstk[0];
	k=0;
	sp = sptr = &symlst[0];
	while(sptr < &symlst[TBLSZ-1]){
		sptr->next = ++sp;
		sptr++;
	}
	sptr->next=0;
	sfree = &symlst[0];
	return;
}
void
onintr(){

	signal(SIGINT,onintr);
	while(readptr != &readstk[0]){
		if(*readptr != 0){release(*readptr);}
		readptr--;
	}
	curfile = stdin;
	commnds();
}
pushp(p)
struct blk *p;
{
	if(stkptr == stkend){
		printf("out of stack space\n");
		return;
	}
	stkerr=0;
	*++stkptr = p;
	return;
}
struct blk *
pop(){
	if(stkptr == stack){
		stkerr=1;
		return(0);
	}
	return(*stkptr--);
}
struct blk *
readin(){
	register struct blk *p,*q;
	int dp,dpct;
	register int c;

	dp = dpct=0;
	p = salloc(0);
	while(1){
		c = readc();
		switch(c){
		case '.':
			if(dp != 0){
				unreadc(c);
				break;
			}
			dp++;
			continue;
		case '\\':
			readc();
			continue;
		default:
			if(c >= 'A' && c <= 'F')c = c - 'A' + 10;
			else if(c >= '0' && c <= '9')c -= '0';
			else goto gotnum;
			if(dp != 0){
				if(dpct >= 99)continue;
				dpct++;
			}
			create(chptr);
			if(c != 0)sputc(chptr,c);
			q = mult(p,inbas);
			release(p);
			p = add(chptr,q);
			release(q);
		}
	}
gotnum:
	unreadc(c);
	if(dp == 0){
		sputc(p,0);
		return(p);
	}
	else{
		q = scale(p,dpct);
		return(q);
	}
}
struct blk *
add0(p,ct)
int ct;
struct blk *p;
{
		/* returns pointer to struct with ct 0's & p */
	register struct blk *q,*t;

	q = salloc(length(p)+(ct+1)/2);
	while(ct>1){
		sputc(q,0);
		ct -= 2;
	}
	rewind(p);
	while(sfeof(p) == 0){
		sputc(q,sgetc(p));
	}
	release(p);
	if(ct == 1){
		t = mult(tenptr,q);
		release(q);
		return(t);
	}
	return(q);
}
struct blk *
mult(p,q)
struct blk *p,*q;
{
	register struct blk *mp,*mq,*mr;
	int sign,offset,carry;
	int cq,cp,mt,mcr;

	offset = sign = 0;
	fsfile(p);
	mp = p;
	if(sfbeg(p) == 0){
		if(sbackc(p)<0){
			mp = copy(p,length(p));
			chsign(mp);
			sign = ~sign;
		}
	}
	fsfile(q);
	mq = q;
	if(sfbeg(q) == 0){
		if(sbackc(q)<0){
			mq = copy(q,length(q));
			chsign(mq);
			sign = ~sign;
		}
	}
	mr = salloc(length(mp)+length(mq));
	zero(mr);
	rewind(mq);
	while(sfeof(mq) == 0){
		cq = sgetc(mq);
		rewind(mp);
		rewind(mr);
		mr->rd += offset;
		carry=0;
		while(sfeof(mp) == 0){
			cp = sgetc(mp);
			mcr = sfeof(mr)?0:slookc(mr);
			mt = cp*cq + carry + mcr;
			carry = mt/100;
			salterc(mr,mt%100);
		}
		offset++;
		if(carry != 0){
			mcr = sfeof(mr)?0:slookc(mr);
			salterc(mr,mcr+carry);
		}
	}
	if(sign < 0){
		chsign(mr);
	}
	if(mp != p)release(mp);
	if(mq != q)release(mq);
	return(mr);
}
chsign(p)
struct blk *p;
{
	register int carry;
	register char ct;

	carry=0;
	rewind(p);
	while(sfeof(p) == 0){
		ct=100-slookc(p)-carry;
		carry=1;
		if(ct>=100){
			ct -= 100;
			carry=0;
		}
		salterc(p,ct);
	}
	if(carry != 0){
		sputc(p,-1);
		fsfile(p);
		sbackc(p);
		ct = sbackc(p);
		if(ct == 99){
			truncate(p);
			sputc(p,-1);
		}
	}
	else{
		fsfile(p);
		ct = sbackc(p);
		if(ct == 0)truncate(p);
	}
	return;
}
readc(){
loop:
	if((readptr != &readstk[0]) && (*readptr != 0)){
		if(sfeof(*readptr) == 0)return(lastchar = sgetc(*readptr));
		release(*readptr);
		readptr--;
		goto loop;
	}
	lastchar = getc(curfile);
	if(lastchar != EOF)return(lastchar);
	if(readptr != &readptr[0]){
		readptr--;
		if(*readptr == 0)curfile = stdin;
		goto loop;
	}
	if(curfile != stdin){
		fclose(curfile);
		curfile = stdin;
		goto loop;
	}
	exit(0);
}
unreadc(c)
char c;
{

	if((readptr != &readstk[0]) && (*readptr != 0)){
		sungetc(*readptr,c);
	}
	else ungetc(c,curfile);
	return;
}
binop(c)
char c;
{
	register struct blk *r;

	switch(c){
	case '+':
		r = add(arg1,arg2);
		break;
	case '*':
		r = mult(arg1,arg2);
		break;
	case '/':
		r = div(arg1,arg2);
		break;
	}
	release(arg1);
	release(arg2);
	sputc(r,savk);
	pushp(r);
	return;
}
print(hptr)
struct blk *hptr;
{
	int sc;
	register struct blk *p,*q,*dec;
	int dig,dout,ct;

	rewind(hptr);
	while(sfeof(hptr) == 0){
		if(sgetc(hptr)>99){
			rewind(hptr);
			while(sfeof(hptr) == 0){
				printf("%c",sgetc(hptr));
			}
			printf("\n");
			return;
		}
	}
	fsfile(hptr);
	sc = sbackc(hptr);
	if(sfbeg(hptr) != 0){
		printf("0\n");
		return;
	}
	count = ll;
	p = copy(hptr,length(hptr));
	sunputc(p);
	fsfile(p);
	if(sbackc(p)<0){
		chsign(p);
		OUTC('-');
	}
	if((obase == 0) || (obase == -1)){
		oneot(p,sc,'d');
		return;
	}
	if(obase == 1){
		oneot(p,sc,'1');
		return;
	}
	if(obase == 10){
		tenot(p,sc);
		return;
	}
	create(strptr);
	dig = log10*sc;
	dout = ((dig/10) + dig) /logo;
	dec = getdec(p,sc);
	p = removc(p,sc);
	while(length(p) != 0){
		q = div(p,basptr);
		release(p);
		p = q;
		(*outdit)(rem,0);
	}
	release(p);
	fsfile(strptr);
	while(sfbeg(strptr) == 0)OUTC(sbackc(strptr));
	if(sc == 0){
		release(dec);
		printf("\n");
		return;
	}
	create(strptr);
	OUTC('.');
	ct=0;
	do{
		q = mult(basptr,dec);
		release(dec);
		dec = getdec(q,sc);
		p = removc(q,sc);
		(*outdit)(p,1);
	}while(++ct < dout);
	release(dec);
	rewind(strptr);
	while(sfeof(strptr) == 0)OUTC(sgetc(strptr));
	printf("\n");
	return;
}

struct blk *
getdec(p,sc)
struct blk *p;
{
	int cc;
	register struct blk *q,*t,*s;

	rewind(p);
	if(length(p)*2 < sc){
		q = copy(p,length(p));
		return(q);
	}
	q = salloc(length(p));
	while(sc >= 1){
		sputc(q,sgetc(p));
		sc -= 2;
	}
	if(sc != 0){
		t = mult(q,tenptr);
		s = salloc(cc = length(q));
		release(q);
		rewind(t);
		while(cc-- > 0)sputc(s,sgetc(t));
		sputc(s,0);
		release(t);
		t = div(s,tenptr);
		release(s);
		release(rem);
		return(t);
	}
	return(q);
}
tenot(p,sc)
struct blk *p;
{
	register int c,f;

	fsfile(p);
	f=0;
	while((sfbeg(p) == 0) && ((p->rd-p->beg-1)*2 >= sc)){
		c = sbackc(p);
		if((c<10) && (f == 1))printf("0%d",c);
		else printf("%d",c);
		f=1;
		TEST2;
	}
	if(sc == 0){
		printf("\n");
		release(p);
		return;
	}
	if((p->rd-p->beg)*2 > sc){
		c = sbackc(p);
		printf("%d.",c/10);
		TEST2;
		OUTC(c%10 +'0');
		sc--;
	}
	else {
		OUTC('.');
	}
	if(sc > (p->rd-p->beg)*2){
		while(sc>(p->rd-p->beg)*2){
			OUTC('0');
			sc--;
		}
	}
	while(sc > 1){
		c = sbackc(p);
		if(c<10)printf("0%d",c);
		else printf("%d",c);
		sc -= 2;
		TEST2;
	}
	if(sc == 1){
		OUTC(sbackc(p)/10 +'0');
	}
	printf("\n");
	release(p);
	return;
}
oneot(p,sc,ch)
struct blk *p;
char ch;
{
	register struct blk *q;

	q = removc(p,sc);
	create(strptr);
	sputc(strptr,-1);
	while(length(q)>0){
		p = add(strptr,q);
		release(q);
		q = p;
		OUTC(ch);
	}
	release(q);
	printf("\n");
	return;
}
hexot(p,flg)
struct blk *p;
{
	register int c;
	rewind(p);
	if(sfeof(p) != 0){
		sputc(strptr,'0');
		release(p);
		return;
	}
	c = sgetc(p);
	release(p);
	if(c >= 16){
		printf("hex digit > 16");
		return;
	}
	sputc(strptr,c<10?c+'0':c-10+'A');
	return;
}
bigot(p,flg)
struct blk *p;
{
	register struct blk *t,*q;
	register int l;
	int neg;

	if(flg == 1)t = salloc(0);
	else{
		t = strptr;
		l = length(strptr)+fw-1;
	}
	neg=0;
	if(length(p) != 0){
		fsfile(p);
		if(sbackc(p)<0){
			neg=1;
			chsign(p);
		}
		while(length(p) != 0){
			q = div(p,tenptr);
			release(p);
			p = q;
			rewind(rem);
			sputc(t,sfeof(rem)?'0':sgetc(rem)+'0');
			release(rem);
		}
	}
	release(p);
	if(flg == 1){
		l = fw1-length(t);
		if(neg != 0){
			l--;
			sputc(strptr,'-');
		}
		fsfile(t);
		while(l-- > 0)sputc(strptr,'0');
		while(sfbeg(t) == 0)sputc(strptr,sbackc(t));
		release(t);
	}
	else{
		l -= length(strptr);
		while(l-- > 0)sputc(strptr,'0');
		if(neg != 0){
			sunputc(strptr);
			sputc(strptr,'-');
		}
	}
	sputc(strptr,' ');
	return;
}
struct blk *
add(a1,a2)
struct blk *a1,*a2;
{
	register struct blk *p;
	register int carry,n;
	int size;
	int c,n1,n2;

	size = length(a1)>length(a2)?length(a1):length(a2);
	p = salloc(size);
	rewind(a1);
	rewind(a2);
	carry=0;
	while(--size >= 0){
		n1 = sfeof(a1)?0:sgetc(a1);
		n2 = sfeof(a2)?0:sgetc(a2);
		n = n1 + n2 + carry;
		if(n>=100){
			carry=1;
			n -= 100;
		}
		else if(n<0){
			carry = -1;
			n += 100;
		}
		else carry = 0;
		sputc(p,n);
	}
	if(carry != 0)sputc(p,carry);
	fsfile(p);
	if(sfbeg(p) == 0){
		while(sfbeg(p) == 0 && (c = sbackc(p)) == 0);
		if(c != 0)salterc(p,c);
		truncate(p);
	}
	fsfile(p);
	if(sfbeg(p) == 0 && sbackc(p) == -1){
		while((c = sbackc(p)) == 99){
			if(c == EOF)break;
		}
		sgetc(p);
		salterc(p,-1);
		truncate(p);
	}
	return(p);
}
eqk(){
	register struct blk *p,*q;
	register int skp;
	int skq;

	p = pop();
	EMPTYS;
	q = pop();
	EMPTYSR(p);
	skp = sunputc(p);
	skq = sunputc(q);
	if(skp == skq){
		arg1=p;
		arg2=q;
		savk = skp;
		return(0);
	}
	else if(skp < skq){
		savk = skq;
		p = add0(p,skq-skp);
	}
	else {
		savk = skp;
		q = add0(q,skp-skq);
	}
	arg1=p;
	arg2=q;
	return(0);
}
struct blk *
removc(p,n)
struct blk *p;
{
	register struct blk *q,*r;

	rewind(p);
	while(n>1){
		sgetc(p);
		n -= 2;
	}
	q = salloc(2);
	while(sfeof(p) == 0)sputc(q,sgetc(p));
	if(n == 1){
		r = div(q,tenptr);
		release(q);
		release(rem);
		q = r;
	}
	release(p);
	return(q);
}
struct blk *
scalint(p)
struct blk *p;
{
	register int n;
	n = sunputc(p);
	p = removc(p,n);
	return(p);
}
struct blk *
scale(p,n)
struct blk *p;
{
	register struct blk *q,*s,*t;

	t = add0(p,n);
	q = salloc(1);
	sputc(q,n);
	s = exp(inbas,q);
	release(q);
	q = div(t,s);
	release(t);
	release(s);
	release(rem);
	sputc(q,n);
	return(q);
}
subt(){
	arg1=pop();
	EMPTYS;
	savk = sunputc(arg1);
	chsign(arg1);
	sputc(arg1,savk);
	pushp(arg1);
	if(eqk() != 0)return(1);
	binop('+');
	return(0);
}
command(){
	int c;
	char line[100],*sl;
	register int pid, rpid;
	sig_t savint;
	int retcode;

	switch(c = readc()){
	case '<':
		return(cond(NL));
	case '>':
		return(cond(NG));
	case '=':
		return(cond(NE));
	default:
		sl = line;
		*sl++ = c;
		while((c = readc()) != '\n')*sl++ = c;
		*sl = 0;
		if((pid = fork()) == 0){
			execl(_PATH_BSHELL,"sh","-c",line,0);
			exit(0100);
		}
		savint = signal(SIGINT, SIG_IGN);
		while((rpid = wait(&retcode)) != pid && rpid != -1);
		signal(SIGINT,savint);
		printf("!\n");
		return(0);
	}
}
cond(c)
char c;
{
	register struct blk *p;
	register char cc;

	if(subt() != 0)return(1);
	p = pop();
	sunputc(p);
	if(length(p) == 0){
		release(p);
		if(c == '<' || c == '>' || c == NE){
			readc();
			return(0);
		}
		load();
		return(1);
	}
	else {
		if(c == '='){
			release(p);
			readc();
			return(0);
		}
	}
	if(c == NE){
		release(p);
		load();
		return(1);
	}
	fsfile(p);
	cc = sbackc(p);
	release(p);
	if((cc<0 && (c == '<' || c == NG)) ||
		(cc >0) && (c == '>' || c == NL)){
		readc();
		return(0);
	}
	load();
	return(1);
}
load(){
	register int c;
	register struct blk *p,*q;
	struct blk *t,*s;
	c = readc() & 0377;
	sptr = stable[c];
	if(sptr != 0){
		p = sptr->val;
		if(c >= ARRAYST){
			q = salloc(length(p));
			rewind(p);
			while(sfeof(p) == 0){
				s = getwd(p);
				if(s == 0){putwd(q, (struct blk *)NULL);}
				else{
					t = copy(s,length(s));
					putwd(q,t);
				}
			}
			pushp(q);
		}
		else{
			q = copy(p,length(p));
			pushp(q);
		}
	}
	else{
		q = salloc(1);
		sputc(q,0);
		pushp(q);
	}
	return;
}
log2(n)
long n;
{
	register int i;

	if(n == 0)return(0);
	i=31;
	if(n<0)return(i);
	while((n= n<<1) >0)i--;
	return(--i);
}

struct blk *
salloc(size)
int size;
{
	register struct blk *hdr;
	register char *ptr;
	all++;
	nbytes += size;
	ptr = malloc((unsigned)size);
	if(ptr == 0){
		garbage("salloc");
		if((ptr = malloc((unsigned)size)) == 0)
			ospace("salloc");
	}
	if((hdr = hfree) == 0)hdr = morehd();
	hfree = (struct blk *)hdr->rd;
	hdr->rd = hdr->wt = hdr->beg = ptr;
	hdr->last = ptr+size;
	return(hdr);
}
struct blk *
morehd(){
	register struct blk *h,*kk;
	headmor++;
	nbytes += HEADSZ;
	hfree = h = (struct blk *)malloc(HEADSZ);
	if(hfree == 0){
		garbage("morehd");
		if((hfree = h = (struct blk *)malloc(HEADSZ)) == 0)
			ospace("headers");
	}
	kk = h;
	while(h<hfree+(HEADSZ/BLK))(h++)->rd = (char *)++kk;
	(--h)->rd=0;
	return(hfree);
}
/*
sunputc(hptr)
struct blk *hptr;
{
	hptr->wt--;
	hptr->rd = hptr->wt;
	return(*hptr->wt);
}
*/
struct blk *
copy(hptr,size)
struct blk *hptr;
int size;
{
	register struct blk *hdr;
	register unsigned sz;
	register char *ptr;

	all++;
	nbytes += size;
	sz = length(hptr);
	ptr = nalloc(hptr->beg, (unsigned)size);
	if(ptr == 0){
		garbage("copy");
		if((ptr = nalloc(hptr->beg, (unsigned)size)) == NULL){
			printf("copy size %d\n",size);
			ospace("copy");
		}
	}
	if((hdr = hfree) == 0)hdr = morehd();
	hfree = (struct blk *)hdr->rd;
	hdr->rd = hdr->beg = ptr;
	hdr->last = ptr+size;
	hdr->wt = ptr+sz;
	ptr = hdr->wt;
	while(ptr<hdr->last)*ptr++ = '\0';
	return(hdr);
}
sdump(s1,hptr)
char *s1;
struct blk *hptr;
{
	char *p;
	printf("%s %o rd %o wt %o beg %o last %o\n",s1,hptr,hptr->rd,hptr->wt,hptr->beg,hptr->last);
	p = hptr->beg;
	while(p < hptr->wt)printf("%d ",*p++);
	printf("\n");
}
seekc(hptr,n)
struct blk *hptr;
{
	register char *nn,*p;

	nn = hptr->beg+n;
	if(nn > hptr->last){
		nbytes += nn - hptr->last;
		p = realloc(hptr->beg, (unsigned)n);
		if(p == 0){
			hptr->beg = realloc(hptr->beg, (unsigned)(hptr->last-hptr->beg));
			garbage("seekc");
			if((p = realloc(hptr->beg, (unsigned)n)) == 0)
				ospace("seekc");
		}
		hptr->beg = p;
		hptr->wt = hptr->last = hptr->rd = p+n;
		return;
	}
	hptr->rd = nn;
	if(nn>hptr->wt)hptr->wt = nn;
	return;
}
salterwd(hptr,n)
struct wblk *hptr;
struct blk *n;
{
	if(hptr->rdw == hptr->lastw)more(hptr);
	*hptr->rdw++ = n;
	if(hptr->rdw > hptr->wtw)hptr->wtw = hptr->rdw;
	return;
}
more(hptr)
struct blk *hptr;
{
	register unsigned size;
	register char *p;

	if((size=(hptr->last-hptr->beg)*2) == 0)size=1;
	nbytes += size/2;
	p = realloc(hptr->beg, (unsigned)size);
	if(p == 0){
		hptr->beg = realloc(hptr->beg, (unsigned)(hptr->last-hptr->beg));
		garbage("more");
		if((p = realloc(hptr->beg,(unsigned)size)) == 0)
			ospace("more");
	}
	hptr->rd = hptr->rd-hptr->beg+p;
	hptr->wt = hptr->wt-hptr->beg+p;
	hptr->beg = p;
	hptr->last = p+size;
	return;
}
ospace(s)
char *s;
{
	printf("out of space: %s\n",s);
	printf("all %ld rel %ld headmor %ld\n",all,rel,headmor);
	printf("nbytes %ld\n",nbytes);
	sdump("stk",*stkptr);
	abort();
}
garbage(s)
char *s;
{
	int i;
	struct blk *p, *q;
	struct sym *tmps;
	int ct;

/*	printf("got to garbage %s\n",s);	*/
	for(i=0;i<TBLSZ;i++){
		tmps = stable[i];
		if(tmps != 0){
			if(i < ARRAYST){
				do {
					p = tmps->val;
					if(((int)p->beg & 01)  != 0){
						printf("string %o\n",i);
						sdump("odd beg",p);
					}
					redef(p);
					tmps = tmps->next;
				} while(tmps != 0);
				continue;
			}
			else {
				do {
					p = tmps->val;
					rewind(p);
					ct = 0;
					while((q = getwd(p)) != NULL){
						ct++;
						if(q != 0){
							if(((int)q->beg & 01) != 0){
								printf("array %o elt %d odd\n",i-ARRAYST,ct);
printf("tmps %o p %o\n",tmps,p);
								sdump("elt",q);
							}
							redef(q);
						}
					}
					tmps = tmps->next;
				} while(tmps != 0);
			}
		}
	}
}
redef(p)
struct blk *p;
{
	register offset;
	register char *newp;

	if ((int)p->beg&01) {
		printf("odd ptr %o hdr %o\n",p->beg,p);
		ospace("redef-bad");
	}
	newp = realloc(p->beg, (unsigned)(p->last-p->beg));
	if(newp == NULL)ospace("redef");
	offset = newp - p->beg;
	p->beg = newp;
	p->rd += offset;
	p->wt += offset;
	p->last += offset;
}

release(p)
register struct blk *p;
{
	rel++;
	nbytes -= p->last - p->beg;
	p->rd = (char *)hfree;
	hfree = p;
	free(p->beg);
}

struct blk *
getwd(p)
struct blk *p;
{
	register struct wblk *wp;

	wp = (struct wblk *)p;
	if (wp->rdw == wp->wtw)
		return(NULL);
	return(*wp->rdw++);
}

putwd(p, c)
struct blk *p, *c;
{
	register struct wblk *wp;

	wp = (struct wblk *)p;
	if (wp->wtw == wp->lastw)
		more(p);
	*wp->wtw++ = c;
}

struct blk *
lookwd(p)
struct blk *p;
{
	register struct wblk *wp;

	wp = (struct wblk *)p;
	if (wp->rdw == wp->wtw)
		return(NULL);
	return(*wp->rdw);
}
char *
nalloc(p,nbytes)
register char *p;
unsigned nbytes;
{
	char *malloc();
	register char *q, *r;
	q = r = malloc(nbytes);
	if(q==0)
		return(0);
	while(nbytes--)
		*q++ = *p++;
	return(r);
}
