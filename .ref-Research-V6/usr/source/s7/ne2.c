# include "ne.h"

fromto(p1,p2,p3) int p1,p2,p3; {
	int w,h,b,h1,b1;
	yyval = oalloc();
	h1 = eht[yyval] = eht[p1];
	b1 = ebase[p1];
	b = 0;
	w = ewid[p1];
	if( p2>0 ){
		w = max(w, ewid[p2]);
		eht[yyval] =+ eht[p2];
		b = eht[p2];
	}
	if( p3>0 ){
		w = max(w, ewid[p3]);
		eht[yyval] =+ eht[p3];
	}
	ewid[yyval] = w;
	printf(".ds %d ", yyval);	/* bottom of middle box */
	if( p2>0 ){
		down(eht[p2]-ebase[p2]+b1);
		fwd((w-ewid[p2])/2);
		printf("\\*(%d", p2);
		back((w+ewid[p2])/2);
		up(eht[p2]-ebase[p2]+b1);
	}
	fwd((w-ewid[p1])/2);
	printf("\\*(%d", p1);
	fwd((1+w-ewid[p1])/2);
	if( p3>0 ){
		up(h1-b1+ebase[p3]);
		back((w+ewid[p3])/2);
		printf("\\*(%d", p3);
		fwd((w-ewid[p3])/2);
		down(h1-b1+ebase[p3]);
	}
	putchar('\n');
	ebase[yyval] = b + b1;
	if(dbg)printf(".\tfrom to: S%d <- %d f %d t %d; h=%d b=%d,w=%d\n",
		yyval,p1,p2,p3,eht[yyval], ebase[yyval], ewid[yyval]);
	ofree(p1);
	if( p2>0 ) ofree(p2);
	if( p3>0 ) ofree(p3);
}

paren(leftc,p1,rightc) int p1, leftc, rightc; {
	int n,h1,b1;
	yyval = oalloc();
	h1 = eht[p1]; ebase[yyval] = b1 = ebase[p1];
	n = max(b1+1, h1-b1-1);
	eht[yyval] = 2*n;
	if( eht[yyval] > h1 )
		++ebase[yyval];
	printf(".ds %d ", yyval);
	brack(n,'|');
	printf("\\*(%d", p1);
	if( rightc )
		brack(n,'|');
	putchar('\n');
	ewid[yyval] = ewid[p1] + 1 + (rightc ? 1 : 0);
	if(dbg)printf(".\tcurly: h=%d b=%d n=%d w=%d l=%c,r=%c\n",
		eht[yyval],ebase[yyval],n,ewid[yyval],leftc,rightc);
	ofree(p1);
}

brack(n,c) int n,c; {
	int j;
	down(n-1);
	for( j=0; j < n; j++ ){
		putchar(c);
		back(1);
		up(2);
	}
	down(n+1);
	fwd(1);
}

diacrit(p1,type) int p1,type; {
	int c,t;
	c = oalloc();
	switch(type){
		case 'H':
			printf(".ds %d ^\n",c);
			ewid[c] = 1;
			break;
		case 'T':
			printf(".ds %d ~\n",c);
			ewid[c] = 1;
			break;
		case 'D':
			printf(".ds %d ", c);
			up(2);
			putchar('.');
			down(2);
			putchar('\n');
			ewid[c] = 1;
			break;
		case 'U':
			printf(".ds %d ", c);
			up(2);
			printf("..");
			down(2);
			putchar('\n');
			ewid[c] = 2;
			break;
		case 'B':
			printf(".ds %d ", c);
			up(eht[p1]);
			line(ewid[p1]);
			down(eht[p1]);
			putchar('\n');
			ewid[c] = ewid[p1];
			break;
		case 'N':
			printf(".ds %d ", c);
			down(ebase[p1]);
			line(ewid[p1]);
			up(ebase[p1]);
			putchar('\n');
			ewid[c] = ewid[p1];
			break;
		}
	yyval = oalloc();
	printf(".ds %d \\*(%d", yyval, p1);
	back((ewid[p1]+ewid[c])/2);
	printf("\\*(%d", c);
	fwd(abs(ewid[p1]-ewid[c])/2);
	putchar('\n');
	ewid[yyval] = max(ewid[p1], ewid[c]);
	ebase[yyval] = ebase[p1];
	eht[yyval] = eht[p1];
	if( type != 'N' )
		eht[yyval]++;
	if(dbg)printf(".\t%c diacrit: S%d <- %d; h=%d,b=%d,w=%d\n",
		type, yyval, p1, eht[yyval], ebase[yyval], ewid[yyval]);
	ofree(p1); ofree(c);
}

move(dir, amt, p) int dir, amt; char *p; {
	yyval = p;
	printf(".ds %d ", yyval);
	if( dir==0 ) fwd(amt);
	else if( dir==1 ) up(amt);
	else if( dir==2 ) back(amt);
	else if( dir==3 ) down(amt);
	printf("\\*(%d\n", p);
	if( dir==1 )
		ebase[yyval] =- amt;
	else if( dir==3 )
		ebase[yyval] =+ amt;
	if(dbg)printf(".\tmove %d dir %d amt %d; h=%d b=%d\n",
		p,dir,amt,eht[yyval],ebase[yyval]);
}
