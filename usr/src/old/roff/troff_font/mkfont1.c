#ifndef lint
static char mkfont1sccsid[] = "@(#)mkfont1.c	4.2 %G%";
#endif lint


struct {
	int name;
	int ctval;
	} font[102] = {
'h',2,
't',2,
'n',0,
'm',0,
'l',2,
'i',2,
'z',0,
's',0,
'd',2,
'b',2,
'x',0,
'f',2,
'j',3,
'u',0,
'k',2,
'p',1,
'em',0,
';',0,
'a',0,
'ru',0,
'c',0,
'`',0,
'e',0,
'\'',0,
'o',0,
'14',0,
'r',0,
'12',0,
'v',0,
'hy',0,
'w',0,
'q',1,
'/',0,
'.',0,
'g',1,
'34',0,
',',0,
'&',0,
'y',1,
'%',0,
'Q',3,
'T',2,
'O',2,
'H',2,
'N',2,
'M',2,
'L',2,
'R',2,
'G',2,
'I',2,
'P',2,
'C',2,
'V',2,
'E',2,
'Z',2,
'D',2,
'B',2,
'S',2,
'Y',2,
'F',2,
'X',2,
'A',2,
'W',2,
'J',2,
'U',2,
'K',2,
'0',2,
'1',2,
'2',2,
'3',2,
'4',2,
'5',2,
'6',2,
'7',2,
'8',2,
'9',2,
'*',0,
'--',0,
'fi',0,
'fl',0,
'ff',0,
'ct',0,
'Fl',0,
'Fi',0,
'(',0,
')',0,
'[',0,
']',0,
'de',0,
'dg',0,
'=',0,
'rg',0,
':',0,
'+',0,
'!',0,
'bu',0,
'?',0,
'fm',0,
'|',0,
'co',0,
'sq',0,
'$',0};
char *nametab[256-32] = {
"/*space*/",
"/*!*/",
"/*\"*/",
"/*#*/",
"/*$*/",
"/*%*/",
"/*&*/",
"/*' close*/",
"/*(*/",
"/*)*/",
"/***/",
"/*+*/",
"/*,*/",
"/*- hyphen*/",
"/*.*/",
"/*/*/",
"/*0*/",
"/*1*/",
"/*2*/",
"/*3*/",
"/*4*/",
"/*5*/",
"/*6*/",
"/*7*/",
"/*8*/",
"/*9*/",
"/*:*/",
"/*;*/",
"/*<*/",
"/*=*/",
"/*>*/",
"/*?*/",
"/*@*/",
"/*A*/",
"/*B*/",
"/*C*/",
"/*D*/",
"/*E*/",
"/*F*/",
"/*G*/",
"/*H*/",
"/*I*/",
"/*J*/",
"/*K*/",
"/*L*/",
"/*M*/",
"/*N*/",
"/*O*/",
"/*P*/",
"/*Q*/",
"/*R*/",
"/*S*/",
"/*T*/",
"/*U*/",
"/*V*/",
"/*W*/",
"/*X*/",
"/*Y*/",
"/*Z*/",
"/*[*/",
"/*\\*/",
"/*]*/",
"/*^*/",
"/*_*/",
"/*` open*/",
"/*a*/",
"/*b*/",
"/*c*/",
"/*d*/",
"/*e*/",
"/*f*/",
"/*g*/",
"/*h*/",
"/*i*/",
"/*j*/",
"/*k*/",
"/*l*/",
"/*m*/",
"/*n*/",
"/*o*/",
"/*p*/",
"/*q*/",
"/*r*/",
"/*s*/",
"/*t*/",
"/*u*/",
"/*v*/",
"/*w*/",
"/*x*/",
"/*y*/",
"/*z*/",
"/*{*/",
"/*|*/",
"/*}*/",
"/*~*/",
"/*narrow space*/",
"/*hyphen*/",
"/*bullet*/",
"/*square*/",
"/*3/4 em*/",
"/*rule*/",
"/*1/4*/",
"/*1/2*/",
"/*3/4*/",
"/*minus*/",
"/*fi*/",
"/*fl*/",
"/*ff*/",
"/*ffi*/",
"/*ffl*/",
"/*degree*/",
"/*dagger*/",
"/*section*/",
"/*foot mark*/",
"/*'*/",
"/*`*/",
"/*_*/",
0,
"/*half nar sp*/",
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
"/*registered*/",
"/*copywrite*/",
0,
"/*cent*/",
};
int chtab [] = {
'--', 0210,	/*font minus*/
'hy', 0200,	/*hyphen*/
'bu', 0201,	/*bullet*/
'sq', 0202,	/*square*/
'em', 0203,	/*3/4em*/
'ru', 0204,	/*rule*/
'14', 0205,	/*1/4*/
'12', 0206,	/*1/2*/
'34', 0207,	/*3/4*/
'mi', 0302,	/*equation minus*/
'fi', 0211,	/*fi*/
'fl', 0212,	/*fl*/
'ff', 0213,	/*ff*/
'Fi', 0214,	/*ffi*/
'Fl', 0215,	/*ffl*/
'de', 0216,	/*degree*/
'dg', 0217,	/*dagger*/
'sc', 0220,	/*section*/
'fm', 0221,	/*foot mark*/
'aa', 0222,	/*acute accent*/
'ga', 0223,	/*grave accent*/
'ul', 0224,	/*underrule*/
'sl', 0225,	/*slash (longer)*/
'*a', 0230,	/*alpha*/
'*b', 0231,	/*beta*/
'*g', 0232,	/*gamma*/
'*d', 0233,	/*delta*/
'*e', 0234,	/*epsilon*/
'*z', 0235,	/*zeta*/
'*y', 0236,	/*eta*/
'*h', 0237,	/*theta*/
'*i', 0240,	/*iota*/
'*k', 0241,	/*kappa*/
'*l', 0242,	/*lambda*/
'*m', 0243,	/*mu*/
'*n', 0244,	/*nu*/
'*c', 0245,	/*xi*/
'*o', 0246,	/*omicron*/
'*p', 0247,	/*pi*/
'*r', 0250,	/*rho*/
'*s', 0251,	/*sigma*/
'*t', 0252,	/*tau*/
'*u', 0253,	/*upsilon*/
'*f', 0254,	/*phi*/
'*x', 0255,	/*chi*/
'*q', 0256,	/*psi*/
'*w', 0257,	/*omega*/
'*A', 0101,	/*Alpha*/
'*B', 0102,	/*Beta*/
'*G', 0260,	/*Gamma*/
'*D', 0261,	/*Delta*/
'*E', 0105,	/*Epsilon*/
'*Z', 0132,	/*Zeta*/
'*Y', 0110,	/*Eta*/
'*H', 0262,	/*Theta*/
'*I', 0111,	/*Iota*/
'*K', 0113,	/*Kappa*/
'*L', 0263,	/*Lambda*/
'*M', 0115,	/*Mu*/
'*N', 0116,	/*Nu*/
'*C', 0264,	/*Xi*/
'*O', 0117,	/*Omicron*/
'*P', 0265,	/*Pi*/
'*R', 0120,	/*Rho*/
'*S', 0266,	/*Sigma*/
'*T', 0124,	/*Tau*/
'*U', 0270,	/*Upsilon*/
'*F', 0271,	/*Phi*/
'*X', 0130,	/*Chi*/
'*Q', 0272,	/*Psi*/
'*W', 0273,	/*Omega*/
'sr', 0274,	/*square root*/
'ts', 0275,	/*terminal sigma*/
'rn', 0276,	/*root en*/
'>=', 0277,	/*>=*/
'<=', 0300,	/*<=*/
'==', 0301,	/*identically equal*/
'~=', 0303,	/*approx =*/
'ap', 0304,	/*approximates*/
'!=', 0305,	/*not equal*/
'->', 0306,	/*right arrow*/
'<-', 0307,	/*left arrow*/
'ua', 0310,	/*up arrow*/
'da', 0311,	/*down arrow*/
'eq', 0312,	/*equation equal*/
'mu', 0313,	/*multiply*/
'di', 0314,	/*divide*/
'+-', 0315,	/*plus-minus*/
'cu', 0316,	/*cup (union)*/
'ca', 0317,	/*cap (intersection)*/
'sb', 0320,	/*subset of*/
'sp', 0321,	/*superset of*/
'ib', 0322,	/*improper subset*/
'ip', 0323,	/*  " superset*/
'if', 0324,	/*infinity*/
'pd', 0325,	/*partial derivative*/
'gr', 0326,	/*gradient*/
'no', 0327,	/*not*/
'is', 0330,	/*integral sign*/
'pt', 0331,	/*proportional to*/
'es', 0332,	/*empty set*/
'mo', 0333,	/*member of*/
'pl', 0334,	/*equation plus*/
'rg', 0335,	/*registered*/
'co', 0336,	/*copyright*/
'br', 0337,	/*box vert rule*/
'ct', 0340,	/*cent sign*/
'dd', 0341,	/*dbl dagger*/
'rh', 0342,	/*right hand*/
'lh', 0343,	/*left hand*/
'**', 0344,	/*math * */
'bs', 0345,	/*bell system sign*/
'or', 0346,	/*or*/
'ci', 0347,	/*circle*/
'lt', 0350,	/*left top (of big curly)*/
'lb', 0351,	/*left bottom*/
'rt', 0352,	/*right top*/
'rb', 0353,	/*right bot*/
'lk', 0354,	/*left center of big curly bracket*/
'rk', 0355,	/*right center of big curly bracket*/
'bv', 0356,	/*bold vertical*/
'lf', 0357,	/*left floor (left bot of big sq bract)*/
'rf', 0360,	/*right floor (rb of ")*/
'lc', 0361,	/*left ceiling (lt of ")*/
'rc', 0362,	/*right ceiling (rt of ")*/
0,0};
