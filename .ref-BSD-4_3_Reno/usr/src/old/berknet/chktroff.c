static char sccsid[] = "@(#)chktroff.c	4.1	(Berkeley)	9/12/82";

/* sccs id variable */
static char *chktroff_sid = "@(#)chktroff.c	1.2";

/*
	chktroff [-l] [-num] [file]

		-l	says list the code
		-num	num is octal offset into file
		file	if specified, read from file, otherwise stdin
*/
# include  "local.h"
# ifdef ONYX
# define NOFP
# endif

# define FEET 15.0
#define DBL 0200
#define	BUFSIZ	1024
/*
C version of pti
*/

char *ap;
char ibuf[BUFSIZ];
char *ibufp = ibuf;
char *eibufp = ibuf;
int fid;
int esc;
int escd;
int verd;
int esct;
int osize = 02;
int size = 02;
int leadtot;
int railmag;
int lead;
int mcase;
int stab[] = {010,0,01,07,02,03,04,05,0211,06,0212,0213,0214,0215,0216,0217};
int rtab[] = {6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 28, 36, 18};
char *asctab[128];
char *spectab[128];
long offset;
int lflg = 1;
int xxx;
long bytetot = 0L;
int init = 0, stop = 0;

main(argc,argv)
int argc;
char **argv;
{
	register i, j;
	register char *k;
	extern ex();
	double f;

	while((--argc > 0) && ((++argv)[0][0]=='-')){
		switch(argv[0][1]){
			case 'l':
				lflg = 0;
				continue;
			default:
				ap = &argv[0][1];
				while(((j = *ap++ - '0') >= 0)
					&& (j <= 9))offset = 8*offset +j;
				continue;
		}
	}
	if(argc){
		if((fid=open(argv[0], 0)) < 0){
			perror(argv[0]);
			exit(1);
		}
	}
	if((i = getc()) != 0100){
		printf("Not typesetter format file. Sorry.\n");
		exit(1);
		}
	escd = verd = mcase = railmag = 0;
	if(!lflg)printf("Initialize\n");
	init++;
/*
	lseek(fid,offset,0);
*/
	while((i = getc()) >= 0){
		if(i & 0200){
			if(!lflg)printf("%o ",i);
			esc += (~i) & 0177;
			continue;
		}
		if(esc){
			if(escd){
				if(!lflg)printf("< %d\n",esc);
				esc = -esc;
			}else{
				if(!lflg)printf("> %d\n",esc);
			}
			esct += esc;
			esc = 0;
		}
		if(!lflg)printf("%o ",i);
		if(!i){if(!lflg)printf("\n"); continue;}
		switch(i){
			case 0100:	/*init*/
				escd = verd = mcase = railmag = 0;
				if(!lflg)printf("Initialize\n");
				init++;
				continue;
			case 0101:	/*lower rail*/
				railmag &= ~01;
				if(!lflg)printf("Lower rail\n");
				continue;
			case 0102:	/*upper rail*/
				railmag |= 01;
				if(!lflg)printf("Upper rail\n");
				continue;
			case 0103:	/*upper mag*/
				railmag |= 02;
				if(!lflg)printf("Upper mag\n");
				continue;
			case 0104:	/*lower mag*/
				railmag &= ~02;
				if(!lflg)printf("Lower mag\n");
				continue;
			case 0105:	/*lower case*/
				mcase = 0;
				if(!lflg)printf("Lower case\n");
				continue;
			case 0106:	/*upper case*/
				mcase = 0100;
				if(!lflg)printf("Upper case\n");
				continue;
			case 0107:	/*escape forward*/
				escd = 0;
				if(!lflg)printf("> mode, %d\n",esct);
				continue;
			case 0110:	/*escape backward*/
				escd = 1;
				if(!lflg)printf("< mode, %d\n",esct);
				continue;
			case 0111:	/*stop*/
				if(!lflg)printf("STOP\n");
				stop++;
				continue;
			case 0112:	/*lead forward*/
				verd = 0;
				if(!lflg)printf("Lead forward, %d\n",leadtot);
				continue;
			case 0114:	/*lead backward*/
				verd = 1;
				if(!lflg)printf("Lead backward, %d\n",leadtot);
				continue;
			case 0115:	/*undefined*/
			case 0116:
			case 0117:
			case 0113:
				if(!lflg)printf("Undefined code\n");
				continue;
		}
		if((i & 0340) == 0140){	/*leading*/
			lead = (~i) & 037;
			if(!lflg)printf("Lead %d\n",lead);
			if(verd)lead = -lead;
			leadtot += lead;
#ifndef NOFP
			f = ((float)leadtot / (float)(144 * 12));
			if(f > FEET){
				printf("Only %3.0f feet maximum per request. Sorry.\n",FEET);
				exit(1);
				}
#endif
			continue;
		}
		if((i & 0360) == 0120){	/*size change*/
			i &= 017;
			for(j = 0; i != (stab[j] & 017); j++);
			osize = size;
			size = stab[j];
			if(!lflg){
				printf("Size %d",rtab[j]);
				if(!(osize & DBL) && (size & DBL))printf(", double\n");
				else if((osize & DBL) && !(size & DBL))printf(", single\n");
				else printf("\n");
			}
			continue;
		}
		if(i & 0300)continue;
		i = (i & 077) | mcase;
		if(railmag != 03)k = asctab[i];
		else k = spectab[i];
		if(!lflg)printf("%s\n",k);
		continue;
	}
	ex();
}
ex(){
	double f1;
#ifndef NOFP
	f1 = ((leadtot * 3)/432.0)/12.0;
	printf("Total bytes %ld, lead %d, feet %4.2f\n",bytetot,leadtot,f1);
#endif
	if(stop != 1 || init != 2){
		printf("Error - wrong # init %d, # stop %d\n",init,stop);
		exit(1);
		}
	exit(0);
}
getc(){
	register i;

	if(ibufp >= eibufp){
		if((i=read(fid,ibuf,BUFSIZ)) <= 0)ex();
		eibufp = ibuf + i;
		ibufp = ibuf;
		bytetot += i;
	}
	return(*ibufp++ & 0377);
}
char *asctab[128] = {
  0,	/*blank*/
"h",	/*h*/
"t",	/*t*/
"n",	/*n*/
"m",	/*m*/
"l",	/*l*/
"i",	/*i*/
"z",	/*z*/
"s",	/*s*/
"d",	/*d*/
"b",	/*b*/
"x",	/*x*/
"f",	/*f*/
"j",	/*j*/
"u",	/*u*/
"k",	/*k*/
  0,	/*blank*/
"p",	/*p*/
"-",	/*_ 3/4 em dash*/
";",	/*;*/
  0,	/*blank*/
"a",	/*a*/
"_",	/*rule*/
"c",	/*c*/
"`",	/*` open*/
"e",	/*e*/
"'",	/*' close*/
"o",	/*o*/
  0,	/*1/4*/
"r",	/*r*/
  0,	/*1/2*/
"v",	/*v*/
"-",	/*- hyphen*/
"w",	/*w*/
"q",	/*q*/
"/",	/*/*/
".",	/*.*/
"g",	/*g*/
  0,	/*3/4*/
",",	/*,*/
"&",	/*&*/
"y",	/*y*/
  0,	/*blank*/
"%",	/*%*/
  0,	/*blank*/
"Q",	/*Q*/
"T",	/*T*/
"O",	/*O*/
"H",	/*H*/
"N",	/*N*/
"M",	/*M*/
"L",	/*L*/
"R",	/*R*/
"G",	/*G*/
"I",	/*I*/
"P",	/*P*/
"C",	/*C*/
"V",	/*V*/
"E",	/*E*/
"Z",	/*Z*/
"D",	/*D*/
"B",	/*B*/
"S",	/*S*/
"Y",	/*Y*/
  0,	/*blank*/
"F",	/*F*/
"X",	/*X*/
"A",	/*A*/
"W",	/*W*/
"J",	/*J*/
"U",	/*U*/
"K",	/*K*/
"0",	/*0*/
"1",	/*1*/
"2",	/*2*/
"3",	/*3*/
"4",	/*4*/
"5",	/*5*/
"6",	/*6*/
"7",	/*7*/
"8",	/*8*/
"9",	/*9*/
"*",	/***/
"-",	/*minus*/
   0,	/*fi*/
  0,	/*fl*/
  0,	/*ff*/
  0,	/*cent mark*/
  0,	/*ffl*/
  0,	/* ffi */
"(",	/*(*/
")",	/*)*/
"[",	/*[*/
"]",	/*]*/
  0,	/*degree*/
  0,	/*dagger*/
"=",	/*=*/
  0,	/*registered*/
":",	/*:*/
"+",	/*+*/
  0,	/*blank*/
"!",	/*!*/
  0,	/*bullet*/
"?",	/*?*/
"'",	/*foot mark*/
"|",	/*|*/
  0,	/*blank*/
  0,	/*copyright*/
  0,	/*square*/
"$" };	/*$*/

char *spectab[128] = {
  0,	/*blank*/
  0,	/*psi*/
  0,	/*theta*/
  0,	/*nu*/
  0,	/*mu*/
  0,	/*lambda*/
  0,	/*iota*/
  0,	/*zeta*/
  0,	/*sigma*/
  0,	/*delta*/
  0,	/*beta*/
  0,	/*xi*/
  0,	/*eta*/
  0,	/*phi*/
  "u",	/*upsilon*/
  0,	/*kappa*/
  0,	/*blank*/
  0,	/*pi*/
  "@",	/*at sign @*/
  0,	/*down arrow*/
  0,	/*blank*/
  0,	/*alpha*/
"|",	/*or*/
  0,	/*chi*/
"\"",	/*"*/
  0,	/*epsilon*/
  "=",	/*equals*/
  "o",	/*omicron*/
  0,	/*left arrow*/
  0,	/*rho*/
  0,	/*up arrow*/
  0,	/*tau*/
"_",	/*underrule*/
"\\",	/*\*/
  0,	/*Psi*/
  0,	/*bell system sign*/
  0,	/*infinity*/
  0,	/*gamma*/
  0,	/*improper superset*/
  0,	/*proportional to*/
  0,	/*right hand*/
  0,	/*omega*/
  0,	/*blank*/
  0,	/*gradient*/
  0,	/*blank*/
  0,	/*Phi*/
  0,	/*Theta*/
  0,	/*Omega*/
  0,	/*cup (union)*/
  0,	/*root en*/
  0,	/*terminal sigma*/
  0,	/*Lambda*/
  "-",	/*some horizontal line*/
  0,	/*Gamma*/
  0,	/*integral sign*/
  0,	/*Pi*/
  0,	/*subset of*/
  0,	/*superset of*/
  0,	/*approximates*/
  0,	/*partial derivative*/
  0,	/*Delta*/
  0,	/*square root*/
  0,	/*Sigma*/
  0,	/*approx =*/
  0,	/*blank*/
">",	/*>*/
  0,	/*Xi*/
"<",	/*<*/
"/",	/*slash (longer)*/
  0,	/*cap (intersection)*/
  "Y",	/*Upsilon*/
  0,	/*not*/
"|",	/*right ceiling (rt of ")*/
"|",	/*left top (of big curly)*/
"|",	/*bold vertical*/
"|",	/*left center of big curly bracket*/
"|",	/*left bottom*/
"|",	/*right top*/
"|",	/*right center of big curly bracket*/
"|",	/*right bot*/
"|",	/*right floor (rb of ")*/
"|",	/*left floor (left bot of big sq bract)*/
"|",	/*left ceiling (lt of ")*/
"x",	/*multiply*/
  0,	/*divide*/
  0,	/*plus-minus*/
  0,	/*<=*/
  0,	/*>=*/
  0,	/*identically equal*/
  0,	/*not equal*/
"{",	/*{*/
"}",	/*}*/
"'",	/*' acute accent*/
"`",	/*` grave accent*/
"^",	/*^*/
  "#",	/*sharp*/
  0,	/*left hand*/
  0,	/*member of*/
"~",	/*~*/
  0,	/*empty set*/
  0,	/*blank*/
  0,	/*dbl dagger*/
"|",	/*box rule*/
  "*",	/*telephone asterisk?*/
  0,	/*improper subset*/
  0,	/*circle*/
  0,	/*blank*/
  "+",	/*eqn plus sign*/
  0,	/*right arrow*/
  0 };	/*section mark*/
