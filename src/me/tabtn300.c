#define INCH 240
/*
Terminet300
nroff driving tables
width and code tables
*/

struct {
	int bset;
	int breset;
	int Hor;
	int Vert;
	int Newline;
	int Char;
	int Em;
	int Halfline;
	int Adj;
	char *twinit;
	char *twrest;
	char *twnl;
	char *hlr;
	char *hlf;
	char *flr;
	char *bdon;
	char *bdoff;
	char *ploton;
	char *plotoff;
	char *up;
	char *down;
	char *right;
	char *left;
	char *codetab[256-32];
	int zzz;
	} t {
/*bset*/	0,
/*breset*/	0,
/*Hor*/		INCH/10,
/*Vert*/	INCH/6,
/*Newline*/	INCH/6,
/*Char*/	INCH/10,
/*Em*/		INCH/10,
/*Halfline*/	INCH/12,
/*Adj*/		INCH/10,
/*twinit*/	"",
/*twrest*/	"",
/*twnl*/	"\n",
/*hlr*/		"",
/*hlf*/		"",
/*flr*/		"",
/*bdon*/	"",
/*bdoff*/	"",
/*ploton*/	"",
/*plotoff*/	"",
/*up*/		"",
/*down*/	"",
/*right*/	"",
/*left*/	"",
/*codetab*/
"\001 ",	/*space*/
"\001!",	/*!*/
"\001\"",	/*"*/
"\001#",	/*#*/
"\001$",	/*$*/
"\001%",	/*%*/
"\001&",	/*&*/
"\001'",	/*' close*/
"\001(",	/*(*/
"\001)",	/*)*/
"\001*",	/***/
"\001+",	/*+*/
"\001,",	/*,*/
"\001-",	/*- hyphen*/
"\001.",	/*.*/
"\001/",	/*/*/
"\2010",	/*0*/
"\2011",	/*1*/
"\2012",	/*2*/
"\2013",	/*3*/
"\2014",	/*4*/
"\2015",	/*5*/
"\2016",	/*6*/
"\2017",	/*7*/
"\2018",	/*8*/
"\2019",	/*9*/
"\001:",	/*:*/
"\001;",	/*;*/
"\001<",	/*<*/
"\001=",	/*=*/
"\001>",	/*>*/
"\001?",	/*?*/
"\001@",	/*@*/
"\201A",	/*A*/
"\201B",	/*B*/
"\201C",	/*C*/
"\201D",	/*D*/
"\201E",	/*E*/
"\201F",	/*F*/
"\201G",	/*G*/
"\201H",	/*H*/
"\201I",	/*I*/
"\201J",	/*J*/
"\201K",	/*K*/
"\201L",	/*L*/
"\201M",	/*M*/
"\201N",	/*N*/
"\201O",	/*O*/
"\201P",	/*P*/
"\201Q",	/*Q*/
"\201R",	/*R*/
"\201S",	/*S*/
"\201T",	/*T*/
"\201U",	/*U*/
"\201V",	/*V*/
"\201W",	/*W*/
"\201X",	/*X*/
"\201Y",	/*Y*/
"\201Z",	/*Z*/
"\001[",	/*[*/
"\001\\",	/*\*/
"\001]",	/*]*/
"\001^",	/*^*/
"\001_",	/*_ dash*/
"\001`",	/*` open*/
"\201a",	/*a*/
"\201b",	/*b*/
"\201c",	/*c*/
"\201d",	/*d*/
"\201e",	/*e*/
"\201f",	/*f*/
"\201g",	/*g*/
"\201h",	/*h*/
"\201i",	/*i*/
"\201j",	/*j*/
"\201k",	/*k*/
"\201l",	/*l*/
"\201m",	/*m*/
"\201n",	/*n*/
"\201o",	/*o*/
"\201p",	/*p*/
"\201q",	/*q*/
"\201r",	/*r*/
"\201s",	/*s*/
"\201t",	/*t*/
"\201u",	/*u*/
"\201v",	/*v*/
"\201w",	/*w*/
"\201x",	/*x*/
"\201y",	/*y*/
"\201z",	/*z*/
"\001{",	/*{*/
"\001|",	/*|*/
"\001}",	/*}*/
"\001~",	/*~*/
"\000\0",	/*nar sp*/
"\001-",	 /*hyphen*/
"\001o\b+",	 /*bullet*/
"\002[]",	 /*square*/
"\001-",	 /*3/4 em*/
"\001_",	 /*rule*/
"\0031/4",	 /*1/4*/
"\0031/2",	 /*1/2*/
"\0033/4",	 /*3/4*/
"\001-",	 /*minus*/
"\202fi",	 /*fi*/
"\202fl",	 /*fl*/
"\202ff",	 /*ff*/
"\203ffi",	 /*ffi*/
"\203ffl",	 /*ffl*/
"\001o",	 /*degree*/
"\001|\b-",	 /*dagger*/
"\002ss",	 /*section*/
"\001'",	 /*foot mark*/
"\001'",	 /*acute accent*/
"\001`",	 /*grave accent*/
"\001_",	 /*underrule*/
"\001/",	 /*slash (longer)*/
"\000\0",	/*half narrow space*/
"\001 ",	/*unpaddable space*/
"\001 ", /*alpha*/
"\001 ", /*beta*/
"\001 ", /*gamma*/
"\001 ", /*delta*/
"\001 ", /*epsilon*/
"\001 ", /*zeta*/
"\001 ", /*eta*/
"\001 ", /*theta*/
"\201i",	 /*iota*/
"\201k",	 /*kappa*/
"\001 ", /*lambda*/
"\001 ", /*mu*/
"\001 ", /*nu*/
"\001 ", /*xi*/
"\201o",	 /*omicron*/
"\001 ", /*pi*/
"\001 ", /*rho*/
"\001 ", /*sigma*/
"\001 ", /*tau*/
"\201v",	 /*upsilon*/
"\001 ", /*phi*/
"\201x",	 /*chi*/
"\001 ", /*psi*/
"\001 ", /*omega*/
"\001 ", /*Gamma*/
"\001 ", /*Delta*/
"\001 ", /*Theta*/
"\001 ", /*Lambda*/
"\001 ",	 /*Xi*/
"\001 ", /*Pi*/
"\001 ", /*Sigma*/
"\000",	 /**/
"\201Y",	 /*Upsilon*/
"\001 ", /*Phi*/
"\001 ", /*Psi*/
"\001 ", /*Omega*/
"\001 ",	 /*square root*/
"\001 ",	 /*terminal sigma*/
"\001 ",	 /*root en*/
"\001>\b_",	 /*>=*/
"\001<\b_",	 /*<=*/
"\001=\b_",	 /*identically equal*/
"\001-",	 /*equation minus*/
"\001=\b~",	 /*approx =*/
"\001~",	 /*approximates*/
"\002!=",	 /*not equal*/
"\002->",	 /*right arrow*/
"\002<-",	 /*left arrow*/
"\001|\b^",	 /*up arrow*/
"\001 ",	 /*down arrow*/
"\001=",	 /*equation equal*/
"\001x",	 /*multiply*/
"\001/",	 /*divide*/
"\001+\b_",	 /*plus-minus*/
"\001U",	 /*cup (union)*/
"\001 ",	 /*cap (intersection)*/
"\001 ",	 /*subset of*/
"\001 ",	 /*superset of*/
"\001 ",	 /*improper subset*/
"\001 ",	 /* improper superset*/
"\002oo",	 /*infinity*/
"\001 ", /*partial derivative*/
"\001 ", /*gradient*/
"\001 ", /*not*/
"\001 ", /*integral sign*/
"\001 ",	 /*proportional to*/
"\001 ",	 /*empty set*/
"\001 ",	 /*member of*/
"\001+",	 /*equation plus*/
"\003(r)",	 /*registered*/
"\003(c)",	 /*copyright*/
"\001|",	 /*box rule */
"\001c\b/",	 /*cent sign*/
"\001|\b=",	 /*dbl dagger*/
"\002*>",	 /*right hand*/
"\002<*",	 /*left hand*/
"\001*",	 /*math * */
"\001 ",	 /*bell system sign*/
"\001|",	 /*or (was star)*/
"\001O",	 /*circle*/
"\001|",	 /*left top (of big curly)*/
"\001|",	 /*left bottom*/
"\001|",	 /*right top*/
"\001|",	 /*right bot*/
"\001|",	 /*left center of big curly bracket*/
"\001|",	 /*right center of big curly bracket*/
"\001|",	/*bold vertical*/
"\001|",	/*left floor (left bot of big sq bract)*/
"\001|",	/*right floor (rb of ")*/
"\001|",	/*left ceiling (lt of ")*/
"\001|"};	/*right ceiling (rt of ")*/
