#ifndef lint
static char sccsid[] = "@(#)tab3.c	4.1 6/7/82";
#endif lint

#define BYTE 8
#define PAIR(A,B) (A|(B<<BYTE))
/*
character name and code tables
default width tables
modified for BTL special font version 4
and Commercial II
*/

int chtab [] = {
PAIR('h','y'), 0200,	/*hyphen*/
PAIR('b','u'), 0201,	/*bullet*/
PAIR('s','q'), 0202,	/*square*/
PAIR('e','m'), 0203,	/*3/4em*/
PAIR('r','u'), 0204,	/*rule*/
PAIR('1','4'), 0205,	/*1/4*/
PAIR('1','2'), 0206,	/*1/2*/
PAIR('3','4'), 0207,	/*3/4*/
PAIR('m','i'), 0302,	/*equation minus*/
PAIR('f','i'), 0211,	/*fi*/
PAIR('f','l'), 0212,	/*fl*/
PAIR('f','f'), 0213,	/*ff*/
PAIR('F','i'), 0214,	/*ffi*/
PAIR('F','l'), 0215,	/*ffl*/
PAIR('d','e'), 0216,	/*degree*/
PAIR('d','g'), 0217,	/*dagger*/
PAIR('s','c'), 0220,	/*section*/
PAIR('f','m'), 0221,	/*foot mark*/
PAIR('a','a'), 0222,	/*acute accent*/
PAIR('g','a'), 0223,	/*grave accent*/
PAIR('u','l'), 0224,	/*underrule*/
PAIR('s','l'), 0225,	/*slash (longer)*/
PAIR('*','a'), 0230,	/*alpha*/
PAIR('*','b'), 0231,	/*beta*/
PAIR('*','g'), 0232,	/*gamma*/
PAIR('*','d'), 0233,	/*delta*/
PAIR('*','e'), 0234,	/*epsilon*/
PAIR('*','z'), 0235,	/*zeta*/
PAIR('*','y'), 0236,	/*eta*/
PAIR('*','h'), 0237,	/*theta*/
PAIR('*','i'), 0240,	/*iota*/
PAIR('*','k'), 0241,	/*kappa*/
PAIR('*','l'), 0242,	/*lambda*/
PAIR('*','m'), 0243,	/*mu*/
PAIR('*','n'), 0244,	/*nu*/
PAIR('*','c'), 0245,	/*xi*/
PAIR('*','o'), 0246,	/*omicron*/
PAIR('*','p'), 0247,	/*pi*/
PAIR('*','r'), 0250,	/*rho*/
PAIR('*','s'), 0251,	/*sigma*/
PAIR('*','t'), 0252,	/*tau*/
PAIR('*','u'), 0253,	/*upsilon*/
PAIR('*','f'), 0254,	/*phi*/
PAIR('*','x'), 0255,	/*chi*/
PAIR('*','q'), 0256,	/*psi*/
PAIR('*','w'), 0257,	/*omega*/
PAIR('*','A'), 0101,	/*Alpha*/
PAIR('*','B'), 0102,	/*Beta*/
PAIR('*','G'), 0260,	/*Gamma*/
PAIR('*','D'), 0261,	/*Delta*/
PAIR('*','E'), 0105,	/*Epsilon*/
PAIR('*','Z'), 0132,	/*Zeta*/
PAIR('*','Y'), 0110,	/*Eta*/
PAIR('*','H'), 0262,	/*Theta*/
PAIR('*','I'), 0111,	/*Iota*/
PAIR('*','K'), 0113,	/*Kappa*/
PAIR('*','L'), 0263,	/*Lambda*/
PAIR('*','M'), 0115,	/*Mu*/
PAIR('*','N'), 0116,	/*Nu*/
PAIR('*','C'), 0264,	/*Xi*/
PAIR('*','O'), 0117,	/*Omicron*/
PAIR('*','P'), 0265,	/*Pi*/
PAIR('*','R'), 0120,	/*Rho*/
PAIR('*','S'), 0266,	/*Sigma*/
PAIR('*','T'), 0124,	/*Tau*/
PAIR('*','U'), 0270,	/*Upsilon*/
PAIR('*','F'), 0271,	/*Phi*/
PAIR('*','X'), 0130,	/*Chi*/
PAIR('*','Q'), 0272,	/*Psi*/
PAIR('*','W'), 0273,	/*Omega*/
PAIR('s','r'), 0274,	/*square root*/
PAIR('t','s'), 0275,	/*terminal sigma*/
PAIR('r','n'), 0276,	/*root en*/
PAIR('>','='), 0277,	/*>=*/
PAIR('<','='), 0300,	/*<=*/
PAIR('=','='), 0301,	/*identically equal*/
PAIR('~','='), 0303,	/*approx =*/
PAIR('a','p'), 0304,	/*approximates*/
PAIR('!','='), 0305,	/*not equal*/
PAIR('-','>'), 0306,	/*right arrow*/
PAIR('<','-'), 0307,	/*left arrow*/
PAIR('u','a'), 0310,	/*up arrow*/
PAIR('d','a'), 0311,	/*down arrow*/
PAIR('e','q'), 0312,	/*equation equal*/
PAIR('m','u'), 0313,	/*multiply*/
PAIR('d','i'), 0314,	/*divide*/
PAIR('+','-'), 0315,	/*plus-minus*/
PAIR('c','u'), 0316,	/*cup (union)*/
PAIR('c','a'), 0317,	/*cap (intersection)*/
PAIR('s','b'), 0320,	/*subset of*/
PAIR('s','p'), 0321,	/*superset of*/
PAIR('i','b'), 0322,	/*improper subset*/
PAIR('i','p'), 0323,	/*  " superset*/
PAIR('i','f'), 0324,	/*infinity*/
PAIR('p','d'), 0325,	/*partial derivative*/
PAIR('g','r'), 0326,	/*gradient*/
PAIR('n','o'), 0327,	/*not*/
PAIR('i','s'), 0330,	/*integral sign*/
PAIR('p','t'), 0331,	/*proportional to*/
PAIR('e','s'), 0332,	/*empty set*/
PAIR('m','o'), 0333,	/*member of*/
PAIR('p','l'), 0334,	/*equation plus*/
PAIR('r','g'), 0335,	/*registered*/
PAIR('c','o'), 0336,	/*copyright*/
PAIR('b','r'), 0337,	/*box vert rule*/
PAIR('c','t'), 0340,	/*cent sign*/
PAIR('d','d'), 0341,	/*dbl dagger*/
PAIR('r','h'), 0342,	/*right hand*/
PAIR('l','h'), 0343,	/*left hand*/
PAIR('*','*'), 0344,	/*math * */
PAIR('b','s'), 0345,	/*bell system sign*/
PAIR('o','r'), 0346,	/*or*/
PAIR('c','i'), 0347,	/*circle*/
PAIR('l','t'), 0350,	/*left top (of big curly)*/
PAIR('l','b'), 0351,	/*left bottom*/
PAIR('r','t'), 0352,	/*right top*/
PAIR('r','b'), 0353,	/*right bot*/
PAIR('l','k'), 0354,	/*left center of big curly bracket*/
PAIR('r','k'), 0355,	/*right center of big curly bracket*/
PAIR('b','v'), 0356,	/*bold vertical*/
PAIR('l','f'), 0357,	/*left floor (left bot of big sq bract)*/
PAIR('r','f'), 0360,	/*right floor (rb of ")*/
PAIR('l','c'), 0361,	/*left ceiling (lt of ")*/
PAIR('r','c'), 0362,	/*right ceiling (rt of ")*/
0,0};

char codetab[256-32] = {	/*cat codes*/
00,	/*space*/
0145,	/*!*/
0230,	/*"*/
0337,	/*#*/
0155,	/*$*/
053,	/*%*/
050,	/*&*/
032,	/*' close*/
0132,	/*(*/
0133,	/*)*/
0122,	/***/
0143,	/*+*/
047,	/*,*/
040,	/*- hyphen*/
044,	/*.*/
043,	/*/*/
0110,	/*0*/
0111,	/*1*/
0112,	/*2*/
0113,	/*3*/
0114,	/*4*/
0115,	/*5*/
0116,	/*6*/
0117,	/*7*/
0120,	/*8*/
0121,	/*9*/
0142,	/*:*/
023,	/*;*/
0303,	/*<*/
0140,	/*=*/
0301,	/*>*/
0147,	/*?*/
0222,	/*@*/
0103,	/*A*/
075,	/*B*/
070,	/*C*/
074,	/*D*/
072,	/*E*/
0101,	/*F*/
065,	/*G*/
060,	/*H*/
066,	/*I*/
0105,	/*J*/
0107,	/*K*/
063,	/*L*/
062,	/*M*/
061,	/*N*/
057,	/*O*/
067,	/*P*/
055,	/*Q*/
064,	/*R*/
076,	/*S*/
056,	/*T*/
0106,	/*U*/
071,	/*V*/
0104,	/*W*/
0102,	/*X*/
077,	/*Y*/
073,	/*Z*/
0134,	/*[*/
0241,	/*\*/
0135,	/*]*/
0336,	/*^*/
0240,	/*_*/
030,	/*` open*/
025,	/*a*/
012,	/*b*/
027,	/*c*/
011,	/*d*/
031,	/*e*/
014,	/*f*/
045,	/*g*/
001,	/*h*/
006,	/*i*/
015,	/*j*/
017,	/*k*/
005,	/*l*/
004,	/*m*/
003,	/*n*/
033,	/*o*/
021,	/*p*/
042,	/*q*/
035,	/*r*/
010,	/*s*/
002,	/*t*/
016,	/*u*/
037,	/*v*/
041,	/*w*/
013,	/*x*/
051,	/*y*/
007,	/*z*/
0332,	/*{*/
0151,	/*|*/
0333,	/*}*/
0342,	/*~*/
00,	/*narrow space*/
040,	/*hyphen*/
0146,	/*bullet*/
0154,	/*square*/
022,	/*3/4 em*/
026,	/*rule*/
034,	/*1/4*/
036,	/*1/2*/
046,	/*3/4*/
0123,	/*minus*/
0124,	/*fi*/
0125,	/*fl*/
0126,	/*ff*/
0131,	/*ffi*/
0130,	/*ffl*/
0136,	/*degree*/
0137,	/*dagger*/
0355,	/*section*/
0150,	/*foot mark*/
0334,	/*acute accent*/
0335,	/*grave accent*/
0240,	/*underrule*/
0304,	/*slash (longer)*/
00,	/*half nar sp*/
00,	/**/
0225,	/*alpha*/
0212,	/*beta*/
0245,	/*gamma*/
0211,	/*delta*/
0231,	/*epsilon*/
0207,	/*zeta*/
0214,	/*eta*/
0202,	/*theta*/
0206,	/*iota*/
0217,	/*kappa*/
0205,	/*lambda*/
0204,	/*mu*/
0203,	/*nu*/
0213,	/*xi*/
0233,	/*omicron*/
0221,	/*pi*/
0235,	/*rho*/
0210,	/*sigma*/
0237,	/*tau*/
0216,	/*upsilon*/
0215,	/*phi*/
0227,	/*chi*/
0201,	/*psi*/
0251,	/*omega*/
0265,	/*Gamma*/
0274,	/*Delta*/
0256,	/*Theta*/
0263,	/*Lambda*/
0302,	/*Xi*/
0267,	/*Pi*/
0276,	/*Sigma*/
00,	/**/
0306,	/*Upsilon*/
0255,	/*Phi*/
0242,	/*Psi*/
0257,	/*Omega*/
0275,	/*square root*/
0262,	/*terminal sigma (was root em)*/
0261,	/*root en*/
0327,	/*>=*/
0326,	/*<=*/
0330,	/*identically equal*/
0264,	/*equation minus*/
0277,	/*approx =*/
0272,	/*approximates*/
0331,	/*not equal*/
0354,	/*right arrow*/
0234,	/*left arrow*/
0236,	/*up arrow*/
0223,	/*down arrow*/
0232,	/*equation equal*/
0323,	/*multiply*/
0324,	/*divide*/
0325,	/*plus-minus*/
0260,	/*cup (union)*/
0305,	/*cap (intersection)*/
0270,	/*subset of*/
0271,	/*superset of*/
0350,	/*improper subset*/
0246,	/* improper superset*/
0244,	/*infinity*/
0273,	/*partial derivative*/
0253,	/*gradient*/
0307,	/*not*/
0266,	/*integral sign*/
0247,	/*proportional to*/
0343,	/*empty set*/
0341,	/*member of*/
0353,	/*equation plus*/
0141,	/*registered*/
0153,	/*copyright*/
0346,	/*box rule (was parallel sign)*/
0127,	/*cent sign*/
0345,	/*dbl dagger*/
0250,	/*right hand*/
0340,	/*left hand*/
0347,	/*math * */
0243,	/*bell system sign*/
0226,	/*or (was star)*/
0351,	/*circle*/
0311,	/*left top (of big curly)*/
0314,	/*left bottom*/
0315,	/*right top*/
0317,	/*right bot*/
0313,	/*left center of big curly bracket*/
0316,	/*right center of big curly bracket*/
0312,	/*bold vertical*/
0321,	/*left floor (left bot of big sq bract)*/
0320,	/*right floor (rb of ")*/
0322,	/*left ceiling (lt of ")*/
0310};	/*right ceiling (rt of ")*/

/*modified for Commercial II*/
char W1[256-32] = {	/*Times Roman widths*/
12,	 /*space*/
12,	 /*!*/
0,	 /*"*/
0,	 /*#*/
19,	 /*$*/
29,	 /*%*/
28,	 /*&*/
12,	 /*' close*/
16,	 /*(*/
16,	 /*)*/
16,	 /***/
36,	 /*+*/
12,	 /*,*/
13,	 /*- hyphen*/
10,	 /*.*/
17,	 /*/*/
19+0200, /*0*/
19+0200, /*1*/
19+0200, /*2*/
19+0200, /*3*/
19+0200, /*4*/
19+0200, /*5*/
19+0200, /*6*/
19+0200, /*7*/
19+0200, /*8*/
19+0200, /*9*/
10,	 /*:*/
12,	 /*;*/
0,	 /*<*/
36,	 /*=*/
0,	 /*>*/
20,	 /*?*/
0,	 /*@*/
29+0200, /*A*/
23+0200, /*B*/
26+0200, /*C*/
30+0200, /*D*/
24+0200, /*E*/
23+0200, /*F*/
30+0200, /*G*/
29+0200, /*H*/
13+0200, /*I*/
16+0200, /*J*/
28+0200, /*K*/
24+0200, /*L*/
35+0200, /*M*/
29+0200, /*N*/
27+0200, /*O*/
22+0200, /*P*/
27+0300, /*Q*/
27+0200, /*R*/
20+0200, /*S*/
24+0200, /*T*/
29+0200, /*U*/
27+0200, /*V*/
36+0200, /*W*/
28+0200, /*X*/
27+0200, /*Y*/
23+0200, /*Z*/
14,	 /*[*/
0,	 /*\*/
14,	 /*]*/
0,	 /*^*/
0,	 /*_*/
12,	 /*` open*/
17,	 /*a*/
20+0200, /*b*/
16,	 /*c*/
20+0200, /*d*/
18,	 /*e*/
13+0200, /*f*/
18+0100, /*g*/
21+0200, /*h*/
10+0200, /*i*/
9+0300, /*j*/
20+0200, /*k*/
10+0200, /*l*/
32,	 /*m*/
21,	 /*n*/
20,	 /*o*/
19+0100, /*p*/
19+0100, /*q*/
14,	 /*r*/
15,	 /*s*/
12+0200, /*t*/
21,	 /*u*/
20,	 /*v*/
26,	 /*w*/
20,	 /*x*/
18+0100, /*y*/
17,	 /*z*/
0,	 /*{*/
2,	 /*|*/
0,	 /*}*/
0,	 /*~*/
6,	 /*narrow space*/
13,	 /*hyphen*/
27,	 /*bullet*/
27,	 /*square*/
36,	 /*3/4 em*/
18,	 /*rule*/
29,	 /*1/4*/
29,	 /*1/2*/
29,	 /*3/4*/
36,	 /*minus*/
21,	 /*fi*/
21,	 /*fl*/
24,	 /*ff*/
32,	 /*ffi*/
32,	 /*ffl*/
15,	 /*degree*/
20,	 /*dagger*/
0,	 /*section*/
8,	 /*foot mark*/
0,	 /*'*/
0,	 /*`*/
0,	 /*_*/
0,
3,	/*half nar sp*/
0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,
20,	 /*registered*/
20,	 /*copyright*/
0,
19,	 /*cent*/
};

char W2[256-32] = {	/*Times Italic widths*/
12,	 /*space*/
13,	 /*!*/
0,	 /*"*/
0,	 /*#*/
19,	 /*$*/
27,	 /*%*/
26,	 /*&*/
11,	 /*' close*/
15,	 /*(*/
15,	 /*)*/
16,	 /***/
36,	 /*+*/
11,	 /*,*/
13,	 /*- hyphen*/
11,	 /*.*/
9,	 /*/*/
19+0200, /*0*/
19+0200, /*1*/
19+0200, /*2*/
19+0200, /*3*/
19+0200, /*4*/
19+0200, /*5*/
19+0200, /*6*/
19+0200, /*7*/
19+0200, /*8*/
19+0200, /*9*/
11,	 /*:*/
11,	 /*;*/
0,	 /*<*/
36,	 /*=*/
0,	 /*>*/
20,	 /*?*/
0,	 /*@*/
25+0200, /*A*/
24+0200, /*B*/
26+0200, /*C*/
27+0200, /*D*/
23+0200, /*E*/
21+0200, /*F*/
27+0200, /*G*/
29+0200, /*H*/
14+0200, /*I*/
16+0200, /*J*/
28+0200, /*K*/
24+0200, /*L*/
34+0200, /*M*/
27+0200, /*N*/
27+0200, /*O*/
22+0200, /*P*/
27+0300, /*Q*/
27+0200, /*R*/
20+0200, /*S*/
23+0200, /*T*/
28+0200, /*U*/
25+0200, /*V*/
36+0200, /*W*/
24+0200, /*X*/
24+0200, /*Y*/
25+0200, /*Z*/
13,	 /*[*/
0,	 /*\*/
13,	 /*]*/
0,	 /*^*/
0,	 /*_*/
11,	 /*` open*/
19,	 /*a*/
18+0200, /*b*/
15,	 /*c*/
18+0200, /*d*/
16,	 /*e*/
11+0200, /*f*/
17+0100, /*g*/
19+0200, /*h*/
9+0200, /*i*/
9+0300, /*j*/
19+0200, /*k*/
9+0200, /*l*/
28,	 /*m*/
19,	 /*n*/
18,	 /*o*/
17+0100, /*p*/
18+0100, /*q*/
13,	 /*r*/
14,	 /*s*/
10+0200, /*t*/
19,	 /*u*/
16,	 /*v*/
24,	 /*w*/
18,	 /*x*/
16+0100, /*y*/
14,	 /*z*/
0,	 /*{*/
2,	 /*|*/
0,	 /*}*/
0,	 /*~*/
6,	 /*narrow space*/
13,	 /*hyphen*/
27,	 /*bullet*/
27,	 /*square*/
36,	 /*3/4 em*/
18,	 /*rule*/
27,	 /*1/4*/
27,	 /*1/2*/
27,	 /*3/4*/
36,	 /*minus*/
21,	 /*fi*/
21,	 /*fl*/
21,	 /*ff*/
31,	 /*ffi*/
31,	 /*ffl*/
15,	 /*degree*/
19,	 /*dagger*/
16,	 /*section*/
7,	 /*foot mark*/
0,	 /*'*/
0,	 /*`*/
0,	 /*_*/
0,
3,	/*half nar sp*/
0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,
20,	 /*registered*/
20,	 /*copyright*/
0,
19,	 /*cent*/
};
char W3[256-32] = {	/*Times Bold widths*/
12,	 /*space*/
13,	 /*!*/
0,	 /*"*/
0,	 /*#*/
18,	 /*$*/
28,	 /*%*/
27,	 /*&*/
12,	 /*' close*/
16,	 /*(*/
16,	 /*)*/
18,	 /***/
36,	 /*+*/
12,	 /*,*/
14,	 /*- hyphen*/
12,	 /*.*/
18,	 /*/*/
19+0200, /*0*/
19+0200, /*1*/
19+0200, /*2*/
19+0200, /*3*/
19+0200, /*4*/
19+0200, /*5*/
19+0200, /*6*/
19+0200, /*7*/
19+0200, /*8*/
19+0200, /*9*/
13,	 /*:*/
13,	 /*;*/
0,	 /*<*/
36,	 /*=*/
0,	 /*>*/
22,	 /*?*/
0,	 /*@*/
28+0200, /*A*/
26+0200, /*B*/
26+0200, /*C*/
29+0200, /*D*/
25+0200, /*E*/
23+0200, /*F*/
28+0200, /*G*/
32+0200, /*H*/
16+0200, /*I*/
21+0200, /*J*/
28+0200, /*K*/
25+0200, /*L*/
36+0200, /*M*/
30+0200, /*N*/
29+0200, /*O*/
25+0200, /*P*/
29+0300, /*Q*/
28+0200, /*R*/
23+0200, /*S*/
25+0200, /*T*/
29+0200, /*U*/
27+0200, /*V*/
36+0200, /*W*/
27+0200, /*X*/
28+0200, /*Y*/
27+0200, /*Z*/
12,	 /*[*/
0,	 /*\*/
12,	 /*]*/
0,	 /*^*/
0,	 /*_*/
12,	 /*` open*/
19,	 /*a*/
19+0200, /*b*/
16,	 /*c*/
19+0200, /*d*/
17,	 /*e*/
13+0200, /*f*/
18+0100, /*g*/
22+0200, /*h*/
12+0200, /*i*/
12+0300, /*j*/
23+0200, /*k*/
12+0200, /*l*/
32,	 /*m*/
22,	 /*n*/
18,	 /*o*/
20+0100, /*p*/
19+0100, /*q*/
15,	 /*r*/
17,	 /*s*/
13+0200, /*t*/
21,	 /*u*/
19,	 /*v*/
27,	 /*w*/
21,	 /*x*/
19+0100, /*y*/
17,	 /*z*/
0,	 /*{*/
2,	 /*|*/
0,	 /*}*/
0,	 /*~*/
6,	 /*narrow space*/
14,	 /*hyphen*/
27,	 /*bullet*/
27,	 /*square*/
36,	 /*3/4 em*/
18,	 /*rule*/
28,	 /*1/4*/
28,	 /*1/2*/
28,	 /*3/4*/
36,	 /*minus*/
22,	 /*fi*/
22,	 /*fl*/
23,	 /*ff*/
33,	 /*ffi*/
33,	 /*ffl*/
15,	 /*degree*/
20,	 /*dagger*/
0,	 /*section*/
9,	 /*foot mark*/
0,	 /*'*/
0,	 /*`*/
0,	 /*_*/
0,
3,	/*half nar sp*/
0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,
20,	 /*registered*/
20,	 /*copyright*/
0,
19,	 /*cent*/
};

/*
Modified for Commercial II
and with +, -, and = for equations
*/
char W4[256-32] = {	/*Special font widths*/
0,0,	 	/*.=Sw+042-40*/
13,	 /*"*/
29,	 /*#*/
0,0,0,0,	 	/*.=Sw+074-40*/
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,
36,	 /*<*/
0,	 	/*.=Sw+076-40*/
36,	 /*>*/
0,	 	/*.=Sw+100-40*/
36,	 /*@*/
0,0,0,0,0,0,0,	/*.=Sw+134-40*/
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,
15,	 /*\\*/
0,	 	/*.=Sw+136-40*/
15,	 /*^*/
18,	 /*_ underrule*/
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,
14,	 /*{*/
0,	 	/*.=Sw+175-40*/
14,	 /*}*/
15,	 /*~*/
0,	 	/*.=Sw+220-40*/
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
17,	 /*section*/
0,		/*.=Sw+222-40*/
10,	 /*acute accent*/
10,	 /*grave accent*/
18,	 /*underrule*/
15,	 /*slash (longer)*/
0,	 /**/
0,	 /**/
24,	 /*alpha*/
23+0300, /*beta*/
23+0100, /*gamma*/
19+0200, /*delta*/
18,	 /*epsilon*/
18+0300, /*zeta*/
23+0100, /*eta*/
19+0200, /*theta*/
13,	 /*iota*/
21,	 /*kappa*/
22+0200, /*lambda*/
25+0100, /*mu*/
20,	 /*nu*/
20+0300, /*xi*/
20,	 /*omicron*/
27,	 /*pi*/
21+0100, /*rho*/
27,	 /*sigma*/
20,	 /*tau*/
21,	 /*upsilon*/
25+0300, /*phi*/
22+0100, /*chi*/
24+0300, /*psi*/
25,	 /*omega*/
24+0200, /*Gamma*/
26+0200, /*Delta*/
28+0200, /*Theta*/
28+0200, /*Lambda*/
27+0200, /*Xi*/
29+0200, /*Pi*/
25+0200, /*Sigma*/
0,	 /**/
28+0200, /*Upsilon*/
29+0200, /*Phi*/
32+0200, /*Psi*/
36+0200, /*Omega*/
30,	 /*square root*/
18+0100, /*terminal sigma*/
18,	 /*root en*/
36,	 /*>=*/
36,	 /*<=*/
36,	 /*identically equal*/
27,	 /*minus*/
36,	 /*approx =*/
36,	 /*approximates*/
36,	 /*not equal*/
36,	 /*right arrow*/
36,	 /*left arrow*/
18,	 /*up arrow*/
18,	 /*down arrow*/
27,	 /*equal*/
27,	 /*multiply*/
27,	 /*divide*/
36,	 /*plus-minus*/
36,	 /*cup (union)*/
36,	 /*cap (intersection)*/
36,	 /*subset of*/
36,	 /*superset of*/
36,	 /*improper subset*/
36,	 /*improper superset*/
34,	 /*infinity*/
21,	 /*partial derivative*/
36+0200, /*gradient*/
22,	 /*not*/
24,	 /*integral sign*/
27,	 /*proportional to*/
28,	 /*empty set*/
27,	 /*member of*/
27,	 /*plus*/
0,
0,
0,	 /*box vert rule (was 2.)*/
0,
17,	 /*dbl dagger*/
42,	 /*right hand*/
42,	 /*left hand*/
16,	 /*math * */
41,	 /*bell system sign*/
9,	 /*or*/
27,	 /*circle*/
9,	 /*left top (of big curly)*/
9,	 /*left bottom*/
9,	 /*right top*/
9,	 /*right bot*/
9,	 /*left center of big curly bracket*/
9,	 /*right center of big curly bracket*/
9,	 /*bold vertical*/
9,	 /*left floor (left bot of big sq bract)*/
9,	 /*right floor (rb of ")*/
9,	 /*left ceiling (lt of ")*/
9 }; /*right ceiling (rt of ")*/
