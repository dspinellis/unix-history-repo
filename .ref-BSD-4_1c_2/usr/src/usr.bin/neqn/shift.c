# include "e.h"
#include "e.def"

bshiftb(p1, dir, p2) int p1, dir, p2; {
	int shval, d1, h1, b1, h2, b2;
	yyval = p1;
	h1 = eht[p1];
	b1 = ebase[p1];
	h2 = eht[p2];
	b2 = ebase[p2];
	if( dir == SUB ) {	/* subscript */
		d1 = VERT(1);
		shval = - d1 + h2 - b2;
		if( d1+b1 > h2 ) /* move little sub down */
			shval = b1-b2;
		ebase[yyval] = b1 + max(0, h2-b1-d1);
		eht[yyval] = h1 + max(0, h2-b1-d1);
	} else {	/* superscript */
		d1 = VERT(1);
		ebase[yyval] = b1;
		shval = -VERT(1) - b2;
		if( VERT(1) + h2 < h1-b1 )	/* raise little super */
			shval = -(h1-b1) + h2-b2 - d1;
		eht[yyval] = h1 + max(0, h2 - VERT(1));
	}
	if(dbg)printf(".\tb:b shift b: S%d <- S%d vert %d S%d vert %d; b=%d, h=%d\n", 
		yyval, p1, shval, p2, -shval, ebase[yyval], eht[yyval]);
	printf(".as %d \\v'%du'\\*(%d\\v'%du'\n", 
		yyval, shval, p2, -shval);
	ofree(p2);
}

shift(p1) int p1; {
	ps -= deltaps;
	yyval = p1;
	if(dbg)printf(".\tshift: %d;ps=%d\n", yyval, ps);
}

shift2(p1, p2, p3) int p1, p2, p3; {
	int effps, h1, h2, h3, b1, b2, b3, subsh, d1, d2, supsh;
	int treg;

	treg = oalloc();
	yyval = p1;
	if(dbg)printf(".\tshift2 s%d <- %d %d %d\n", yyval, p1, p2, p3);
	effps = EFFPS(ps+deltaps);
	h1 = eht[p1]; b1 = ebase[p1];
	h2 = eht[p2]; b2 = ebase[p2];
	h3 = eht[p3]; b3 = ebase[p3];
	d1 = VERT(1);
	subsh = -d1+h2-b2;
	if( d1+b1 > h2 ) /* move little sub down */
		subsh = b1-b2;
	supsh = - VERT(1) - b3;
	d2 = VERT(1);
	if( VERT(1)+h3 < h1-b1 )
		supsh = -(h1-b1) + (h3-b3) - d2;
	eht[yyval] = h1 + max(0, h3-VERT(1)) + max(0, h2-b1-d1);
	ebase[yyval] = b1+max(0, h2-b1-d1);
	nrwid(p2, effps, p2);
	nrwid(p3, effps, p3);
	printf(".nr %d \\n(%d\n", treg, p3);
	printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", p2, treg, treg, p2);
	printf(".as %d \\v'%du'\\*(%d\\h'-\\n(%du'\\v'%du'\\\n", 
		p1, subsh, p2, p2, -subsh+supsh);
	printf("\\*(%d\\h'-\\n(%du+\\n(%du'\\v'%du'\n", 
		p3, p3, treg, -supsh);
	ps += deltaps;
	ofree(p2); ofree(p3); ofree(treg);
}
