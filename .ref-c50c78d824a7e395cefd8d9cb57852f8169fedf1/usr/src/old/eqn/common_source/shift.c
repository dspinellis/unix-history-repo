#ifndef lint
static char sccsid[] = "@(#)shift.c	4.3 %G%";
#endif

# include "e.h"
#include "e.def"

bshiftb(p1, dir, p2) int p1, dir, p2; {
	int shval, d1, h1, b1, h2, b2;
#ifndef NEQN
	int diffps, effps, effps2;
	char *sh1, *sh2;
#endif NEQN

	yyval = p1;
	h1 = eht[p1];
	b1 = ebase[p1];
	h2 = eht[p2];
	b2 = ebase[p2];
#ifndef NEQN
	effps = EFFPS(ps);
	effps2 = EFFPS(ps+deltaps);
	diffps = deltaps;
	sh1 = sh2 = "";
#endif NEQN
	if( dir == SUB ) {	/* subscript */
#ifndef NEQN
		/* top 1/2m above bottom of main box */
		d1 = VERT( (effps2*6)/2 );
#else NEQN
		d1 = VERT(1);
#endif NEQN
		shval = - d1 + h2 - b2;
		if( d1+b1 > h2 ) /* move little sub down */
			shval = b1-b2;
		ebase[yyval] = b1 + max(0, h2-b1-d1);
		eht[yyval] = h1 + max(0, h2-b1-d1);
#ifndef NEQN
		if (rfont[p1] == ITAL && lfont[p2] == ROM)
			sh1 = "\\|";
		if (rfont[p2] == ITAL)
			sh2 = "\\|";
#endif NEQN
	} else {	/* superscript */
#ifndef NEQN
		/* 4/10 up main box */
		d1 = VERT( (effps*6*2)/10 );
#else NEQN
		d1 = VERT(1);
#endif NEQN
		ebase[yyval] = b1;
#ifndef NEQN
		shval = -VERT( (4 * (h1-b1)) / 10 ) - b2;
		if( VERT(4*(h1-b1)/10) + h2 < h1-b1 )	/* raise little super */
#else NEQN
		shval = -VERT(1) - b2;
		if( VERT(1) + h2 < h1-b1 )	/* raise little super */
#endif NEQN
			shval = -(h1-b1) + h2-b2 - d1;
#ifndef NEQN
		eht[yyval] = h1 + max(0, h2-VERT((6*(h1-b1))/10));
		if (rfont[p1] == ITAL)
			sh1 = "\\|";
		if (rfont[p2] == ITAL)
			sh2 = "\\|";
#else NEQN
		eht[yyval] = h1 + max(0, h2 - VERT(1));
#endif NEQN
	}
	if(dbg)printf(".\tb:b shift b: S%d <- S%d vert %d S%d vert %d; b=%d, h=%d\n", 
		yyval, p1, shval, p2, -shval, ebase[yyval], eht[yyval]);
#ifndef NEQN
	printf(".as %d \\v'%du'\\s-%d%s\\*(%d\\s+%d%s\\v'%du'\n", 
		yyval, shval, diffps, sh1, p2, diffps, sh2, -shval);
	ps += deltaps;
	if (rfont[p2] == ITAL)
		rfont[p1] = 0;
	else
		rfont[p1] = rfont[p2];
#else NEQN
	printf(".as %d \\v'%du'\\*(%d\\v'%du'\n", 
		yyval, shval, p2, -shval);
#endif NEQN
	ofree(p2);
}

shift(p1) int p1; {
	ps -= deltaps;
	yyval = p1;
	if(dbg)printf(".\tshift: %d;ps=%d\n", yyval, ps);
}

shift2(p1, p2, p3) int p1, p2, p3; {
	int effps, h1, h2, h3, b1, b2, b3, subsh, d1, d2, supsh, treg;
#ifndef NEQN
	int effps2;
#endif NEQN

	treg = oalloc();
	yyval = p1;
	if(dbg)printf(".\tshift2 s%d <- %d %d %d\n", yyval, p1, p2, p3);
	effps = EFFPS(ps+deltaps);
#ifndef NEQN
	eht[p3] = h3 = VERT( (eht[p3] * effps) / EFFPS(ps) );
	ps += deltaps;
	effps2 = EFFPS(ps+deltaps);
#endif NEQN
	h1 = eht[p1]; b1 = ebase[p1];
	h2 = eht[p2]; b2 = ebase[p2];
#ifndef NEQN
	b3 = ebase[p3];
	d1 = VERT( (effps2*6)/2 );
#else NEQN
	h3 = eht[p3]; b3 = ebase[p3];
	d1 = VERT(1);
#endif NEQN
	subsh = -d1+h2-b2;
	if( d1+b1 > h2 ) /* move little sub down */
		subsh = b1-b2;
#ifndef NEQN
	supsh = -VERT( (4*(h1-b1))/10 ) - b3;
	d2 = VERT( (effps*6*2)/10 );
	if( VERT(4*(h1-b1)/10)+h3 < h1-b1 )
#else NEQN
	supsh = - VERT(1) - b3;
	d2 = VERT(1);
	if( VERT(1)+h3 < h1-b1 )
#endif NEQN
		supsh = -(h1-b1) + (h3-b3) - d2;
#ifndef NEQN
	eht[yyval] = h1 + max(0, h3-VERT( (6*(h1-b1))/10 )) + max(0, h2-b1-d1);
#else NEQN
	eht[yyval] = h1 + max(0, h3-VERT(1)) + max(0, h2-b1-d1);
#endif NEQN
	ebase[yyval] = b1+max(0, h2-b1-d1);
#ifndef NEQN
	if (rfont[p1] == ITAL && lfont[p2] == ROM)
		printf(".ds %d \\|\\*(%d\n", p2, p2);
	if (rfont[p2] == ITAL)
		printf(".as %d \\|\n", p2);
#endif NEQN
	nrwid(p2, effps, p2);
#ifndef NEQN
	if (rfont[p1] == ITAL && lfont[p3] == ROM)
		printf(".ds %d \\|\\|\\*(%d\n", p3, p3);
	else
		printf(".ds %d \\|\\*(%d\n", p3, p3);
#endif NEQN
	nrwid(p3, effps, p3);
	printf(".nr %d \\n(%d\n", treg, p3);
	printf(".if \\n(%d>\\n(%d .nr %d \\n(%d\n", p2, treg, treg, p2);
#ifndef NEQN
	printf(".as %d \\v'%du'\\s%d\\*(%d\\h'-\\n(%du'\\v'%du'\\\n", 
		p1, subsh, effps, p2, p2, -subsh+supsh);
	printf("\\s%d\\*(%d\\h'-\\n(%du+\\n(%du'\\s%d\\v'%du'\n", 
		effps, p3, p3, treg, effps2, -supsh);
#else NEQN
	printf(".as %d \\v'%du'\\*(%d\\h'-\\n(%du'\\v'%du'\\\n", 
		p1, subsh, p2, p2, -subsh+supsh);
	printf("\\*(%d\\h'-\\n(%du+\\n(%du'\\v'%du'\n", 
		p3, p3, treg, -supsh);
#endif NEQN
	ps += deltaps;
#ifndef NEQN
	if (rfont[p2] == ITAL)
		rfont[yyval] = 0;	/* lie */
#endif NEQN
	ofree(p2); ofree(p3); ofree(treg);
}
