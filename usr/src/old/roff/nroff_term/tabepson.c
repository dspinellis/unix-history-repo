/*************************************************************************
 * This is 'tabepson.c' for an Epson FX80. Default spacing is elite.	 *
 * This represents a combination and very slight modification of two	 *
 *  nroff driving tables. Specifically :				 *
 *									 *
 *     tablpr.c  by UCB Computing Center (the approximation of greek)    *
 *									 *
 *     tabfx80p.c by Goeke@MIT-Multics.ARPA (Epson print mode switching) *
 *	replaces big curly bracket parts with the following:		 *
 *									 *
 *		PRINT MODE:		  CHARACTER SEQUENCE		 *
 *		FX80 set Double Strike 	 	\(lt			 *
 *		FX80 cancels D.S.  	 	\(lb			 *
 *		FX80 set Condensed 	 	\(rt			 *
 *		FX80 cancels Condensed 	 	\(rb			 *
 *		FX80 set Pica size 	 	\(lk			 *
 *		FX80 set Elite size	 	\(rk			 *
 *		FX80 set Proportional 	 	\(bv 			 *
 *		FX80 set Expanded Mode 	 	\(lf 			 *
 *		FX80 cancel Expanded Mode	\(rf 			 *
 *		FX80 underline on 	 	\(lc 			 *
 *		FX80 underline off 	 	\(rc 			 *
 *									 *
 *  --> J. Michael Cherry, 1/18/85  molbio@ucblapis or			 *
 *				    ucbvax!molbio!mike			 *
 *									 *
 *************************************************************************/

#define INCH 240

struct {
          int bset;               /* terminal driver set          */
          int breset;             /* terminal driver reset        */
          int Hor;                /* horizontal resolution        */
          int Vert;               /* vetical resolution           */
          int Newline;            /* length on one line feed      */
          int Char;               /* character size increment     */
          int Em;                 /* em size                      */
          int Halfline;           /* length of half line feed     */
          int Adj;                /* white size increment         */
          char *twinit;           /* initialize string to printer */
          char *twrest;           /* reset string to printer      */
          char *twnl;             /* newline string               */
          char *hlr;              /* half-line-feed-reverse       */
          char *hlf;              /* half-line-feed-forward       */
          char *flr;              /* full-line-feed-reverse       */
          char *bdon;             /* bold-on string               */
          char *bdoff;            /* bold-off string              */
          char *ploton;           /* plot on string               */
          char *plotoff;          /* plot off string              */
          char *up;               /* these strings move as        */
          char *down;             /*     indicated in plot mode   */
          char *right;            /*     in best available        */
          char *left;             /*     resolution               */
          char *codetab[256-32];  /* see the table                */
          int zzz;
          } t = {
/*bset*/	0,
/*breset*/	0,
/*Hor*/		INCH/120,	/* must be consisten with plot increments */
/*Vert*/	INCH/48,	/* ditto */
/*Newline*/	INCH/6,
/*Char*/	INCH/12,	/* the character unit for the code table */
/*Em*/		INCH/12,	/* the nominal space size */
/*Halfline*/	INCH/12,
/*Adj*/		INCH/12,	/* the white caused by a single plot move */
/*twinit*/	"\033M",	/* invoke elite */
/*twrest*/	"\033@",	/* reset printer to deflauts */
/*twnl*/	"\n",		/* newline code        */
/*hlr*/		"\033j\022",
/*hlf*/		"\033J\022",	/* forward feed */
/*flr*/		"\033j\044",
/*bdon*/	"\033G",	/* Alternate chars.  '\fB' to invoke */
/*bdoff*/	"\033H",	/* Standard chars. '\fP' to invoke */
/*ploton*/	"",
/*plotoff*/	"",
/*up*/		"",
/*down*/	"",
/*right*/	"",
/*left*/	"",

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
"\001",		/* narrow sp */
"\001-",	/*hyphen*/
"\001\033j\003.\033J\003",	/*bullet*/
"\002\[]",	/*square*/
"\001-",	/*3/4 em*/
"\001_",	/*rule*/
"\0031/4",	/*1/4*/
"\0031/2",	/*1/2*/
"\0033/4",	/*3/4*/
"\001-",	/*minus*/
"\202fi",	/*fi*/
"\202fl",	/*fl*/
"\202ff",	/*ff*/
"\203ffi",	/*ffi*/
"\203ffl",	/*ffl*/
"\001\033j\013o\033J\013",	/*degree*/
"\001|\b\033j\002-\033J\002",	/*dagger*/
"\001 ",	/* section*/
"\001'",	/*foot mark*/
"\001'",	/*acute accent*/
"\001`",	/*grave accent*/
"\001_",	/*underrule*/
"\001/",	/*slash (longer)*/
"\001\0",	/*half narrow space => 1/12 em */
"\001 ",	/*unpaddable space*/
"\201o\b(", 	/*alpha*/
"\2018\b|", 	/*beta*/
"\201>\b/", 	/*gamma*/
"\201d\b`", 	/*delta*/
"\201C\b-", 	/*epsilon*/
"\000\0", 	/*zeta*/
"\201n",	/*eta*/
"\201o\b-", 	/*theta*/
"\201i",	/*iota*/
"\201k",	/*kappa*/
"\201,\b\\", 	/*lambda*/
"\201u",	/*mu*/
"\201v",	/*nu*/
"\000\0", 	/*xi*/
"\201o",	/*omicron*/
"\202i\b~i\b~",	/*pi*/
"\201p",	/*rho*/
"\201o\b~", 	/*sigma*/
"\201i\b~", 	/*tau*/
"\201u",	/*upsilon*/
"\201o\b|", 	/*phi*/
"\201x",	/*chi*/
"\201u\b|", 	/*psi*/
"\201w", 	/*omega*/
"\201I\b~", 	/*Gamma*/
"\202/\b_\\\b_", /*Delta*/
"\201O\b-",	/*Theta*/
"\202/\\",	/*Lambda*/
"\201=\b_",	/*Xi*/
"\202TT",	/*Pi*/
"\201>\b_\b~", 	/*Sigma*/
"\000",		/**/
"\201Y",	/*Upsilon*/
"\201O\b|",	/*Phi*/
"\201U\b|",	/*Psi*/
"\201O\b_",	/*Omega*/
"\001v\b/",	/*square root*/
"\000\0",	/*terminal sigma*/
"\001~",	/*root en*/
"\001>\b_",	/*>=*/
"\001<\b_",	/*<=*/
"\001=\b_",	/*identically equal*/
"\001-",	/*equation minus*/
"\001~\b_",	/*approx =*/
"\001~",	/*approximates*/
"\001=\b/",	/*not equal*/
"\002->",	/*right arrow*/
"\002<-",	/*left arrow*/
"\001|\b^",	/*up arrow*/
"\001|\bv",	/*down arrow*/
"\001=",	/*equation equal*/
"\001x",	/*multiply*/
"\001:\b-",	/*divide*/
"\001+\b_",	/*plus-minus*/
"\002(\b~)\b~",	/*cup (union)*/
"\002(\b_)\b_",	/*cap (intersection)*/
"\002(=",	/*subset of*/
"\002=)",	/*superset of*/
"\002(=\b_",	/*improper subset*/
"\002=\b_)",	/*improper superset*/
"\002oo",	/*infinity*/
"\001o\b`",	/*partial derivative*/
"\002\\\b~/\b~", /*gradient*/
"\000\0",	/*not*/
"\000\0",	/*integral sign*/
"\002oc",	/*proportional to*/
"\001O\b/",	/*empty set*/
"\001<\b-",	/*member of*/
"\001+",	/*equation plus*/
"\003(R)",	/*registered*/
"\003(C)",	/*copyright*/
"\001|",	/*box rule */
"\001c\b/",	/*cent sign*/
"\001|\b=",	/*dbl dagger*/
"\002=>",	/*right hand*/
"\002<=",	/*left hand*/
"\001*",	/*math * */
"\000\0",	/*bell system sign*/
"\001|",	/*or (was star)*/
"\001O",	/*circle*/
"\000\033G",	/* FX80 set Double Strike */	/* \(lt */
"\000\033H",	/* FX80 cancels D.S.  */	/* \(lb */
