# include "ne.h"

boverb(p1,p2) int p1,p2; {
	int h,b,w,treg;
	yyval = oalloc();
	eht[yyval] = eht[p1] + eht[p2] ;
	ebase[yyval] = eht[p2] - 1;
	ewid[yyval] = w = max(ewid[p1],ewid[p2]);
	if(dbg)printf(".\tb:bob: S%d <- S%d over S%d; b=%d,h=%d,w=%d\n",
		yyval, p1, p2, ebase[yyval], eht[yyval], ewid[yyval]);
	printf(".ds %d ", yyval);
	down(eht[p2]-ebase[p2]-1);
	fwd((ewid[yyval]-ewid[p2])/2);
	printf("\\*(%d", p2);
	back((ewid[yyval]+ewid[p2])/2);
	up(eht[p2]-ebase[p2]+ebase[p1]);
	fwd((ewid[yyval]-ewid[p1])/2);
	printf("\\*(%d", p1);
	back((ewid[yyval]+ewid[p1])/2);
	down(ebase[p1]);
	line(ewid[yyval]);
	down(1);
	putchar('\n');
	ofree(p1);
	ofree(p2);
}

bshiftb(p1,dir,p2) int p1,dir,p2; {
	int shval, d1, h1, b1,h2,b2;
	yyval = oalloc();
	ewid[yyval] = ewid[p1] + ewid[p2];
	h1 = eht[p1]; b1 = ebase[p1];
	h2 = eht[p2]; b2 = ebase[p2];
	printf(".ds %d \"\\*(%d", yyval, p1);
	if( dir > 0 ){	/* subscript */
		shval = h2-b2-1;
		if( shval < b1 )
			shval = b1;
		down(shval);
		printf("\\*(%d", p2);
		up(shval);
		ebase[yyval] = max(b1, h2-1);
		eht[yyval] = h1 + max(0,h2-b1-1);
	} else {	/* superscript */
		ebase[yyval]  = b1;
		shval = b2+1;
		if( shval+h2 < h1-b1 )
			shval = h1-b1-h2;
		up(shval);
		printf("\\*(%d", p2);
		down(shval);
		eht[yyval] = max(h1, b1+1+h2);
	}
	putchar('\n');
	if(dbg)printf(".\tb:b shift b: S%d <- S%d vert %d S%d vert %d; b=%d,h=%d,w=%d\n",
		yyval,p1,shval,p2,-shval,ebase[yyval],eht[yyval],ewid[yyval]);
	ofree(p1);
	ofree(p2);
}

eqnbox(p1,p2) int p1,p2; {
	int b,h;
	if( p1==0 ){
		yyval = p2; return;
	}
	yyval = oalloc();
	b = max(ebase[p1], ebase[p2]);
	eht[yyval] = h = b + max(eht[p1]-ebase[p1],
		eht[p2]-ebase[p2]);
	ebase[yyval] = b;
	ewid[yyval] = ewid[p1] + ewid[p2];
	if(dbg)printf(".\te:eb: S%d <- S%d S%d; b=%d,h=%d,w=%d\n",
		yyval,p1,p2,b,h,ewid[yyval]);
	printf(".ds %d \\*(%d\\*(%d\n", yyval, p1, p2);
	ofree(p1);
	ofree(p2);
}

size(p1,p2) int p1,p2; {
	yyval = p2;
}

numb(p1) char *p1; {
	int i, n, c;
	for(i=n=0; (c=p1[i++])!='\0'; )
		if( c>='0' && c<='9' )
			n = n*10 + c-'0';
	if(dbg)printf(".\tnumb: %s %d\n",p1,n);
	return( n );
}

font(p1,p2) int p1,p2; {
	yyval = p2;
}

shift(p1) int p1; {
	yyval = p1;
	if(dbg)printf(".\tshift: %d\n",yyval);
}

sqrt(p2) int p2; {
	int nps, h, i;
	yyval = oalloc();
	h = eht[p2];
	eht[yyval] = h+1;
	nps = h-1;
	ebase[yyval] = ebase[p2];
	ewid[yyval] = ewid[p2] + 2;
	if(dbg)printf(".\tsqrt: S%d <- S%d;b=%d,h=%d,w=%d\n",
		yyval,p2,ebase[yyval],eht[yyval],ewid[yyval]);
	printf(".ds %d \\e|", yyval);
	for( i=2; i<=nps; i++ ){
		back(1);
		up(1);
		putchar('|');
	}
	up(2);
	line(ewid[p2]);
	back(ewid[p2]);
	down(h);
	printf("\\*(%d\n", p2);
	ofree(p2);
}

lpile(type,p1,p2) int type,p1,p2; {
	int w,bi,hi,i,gap,h,b,j, nlist, nlist2, mid;
	yyval = oalloc();
	gap = type == '-' ? 0 : 1;
	nlist = p2 - p1;
	nlist2 = (nlist+1)/2;
	mid = p1 + nlist2 -1;
	h = b = w = 0;
	for( i=p1; i<p2; i++ ){
		h =+ eht[lp[i]];
		w = max(w, ewid[lp[i]]);
	}
	eht[yyval] = h + (nlist-1)*gap;
	ewid[yyval] = w;
	for( i=p2-1; i>mid; i-- )
		b =+ eht[lp[i]] + gap;
	ebase[yyval] = (nlist%2) ? b + ebase[lp[mid]] : b - gap -1;
	if(dbg){
		printf(".\tS%d <- %c pile of:", yyval, type);
		for( i=p1; i<p2; i++)
			printf("  S%d", lp[i]);
		printf(";h=%d b=%d,w=%d\n",eht[yyval],ebase[yyval],ewid[yyval]);
	}
	printf(".ds %d \\\n", yyval);
	down(ebase[yyval]);
	if( type=='R' )
		fwd(ewid[yyval]);
	for(i = p2-1; i >=p1; i--){
		hi = eht[lp[i]]; 
		bi = ebase[lp[i]];
	switch(type){

	case 'L':
		up(bi);
		printf("\\*(%d", lp[i]);
		back(ewid[lp[i]]);
		up(hi-bi+gap);
		printf("\\\n");
		continue;
	case 'R':
		up(bi);
		back(ewid[lp[i]]);
		printf("\\*(%d", lp[i]);
		up(hi-bi+gap);
		printf("\\\n");
		continue;
	case 'C':
	case '-':
		up(bi);
		fwd((ewid[yyval]-ewid[lp[i]])/2);
		printf("\\*(%d", lp[i]);
		back((ewid[yyval]+ewid[lp[i]])/2);
		up(hi-bi+gap);
		printf("\\\n");
		continue;
		}
	}
	down(eht[yyval]-ebase[yyval]+gap);
	if( type!='R' )
		fwd(ewid[yyval]);
	putchar('\n');
	for( i=p1; i<p2; i++ ) ofree(lp[i]);
}

shift2(p1,p2,p3) int p1,p2,p3;{
	int h1,h2,h3,b1,b2,b3,subsh,d1,supsh;
	yyval = oalloc();
	h1 = eht[p1]; b1 = ebase[p1];
	h2 = eht[p2]; b2 = ebase[p2];
	h3 = eht[p3]; b3 = ebase[p3];
	d1 = 1;
	subsh = -d1+h2-b2;
	if( d1+b1 > h2 )
		subsh = b1-b2;
	supsh = b3 + 1;
	if( supsh+h3 < h1-b1 )
		supsh = h1-b1-h3;
	eht[yyval] = h1 + max(0,h3-1) + max(0,h2-b1-d1);
	ebase[yyval] = b1+max(0,h2-b1-d1);
	ewid[yyval] = ewid[p1] + max(ewid[p2], ewid[p3]);
	printf(".ds %d \\*(%d", yyval, p1);
	down(subsh);
	printf("\\*(%d", p2);
	back(ewid[p2]);
	up(subsh+supsh);
	printf("\\*(%d", p3);
	down(supsh);
	if(ewid[p3] < ewid[p2] )
		fwd(ewid[p2]-ewid[p3]);
	putchar('\n');
	if(dbg)printf(".\tshift2 s%d <- %d %d %d",yyval,p1,p2,p3);
	if(dbg)printf(" h=%d,b=%d,w=%d\n", eht[yyval],ebase[yyval],ewid[yyval]);
	ofree(p1); ofree(p2); ofree(p3);
}
