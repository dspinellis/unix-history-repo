static char *sccsid = "@(#)tc.c	4.3 (Berkeley) %G%";
/*
 * Simulate typesetter on 4014
*/

#include <sys/signal.h>
#include <paths.h>
#include <stdio.h>

#define	oput(c) if (pgskip==0) putchar(c); else (c);
#define MAXY 3071
#define US 037
#define GS 035
#define ESC 033
#define FF 014
#define DBL 0200

int pl = 11*144;
int mpy = 1;
int div = 1;
char *ap;
int ch;
int nonumb;
int psize = 10;
int dfact = 1;
int esc;
int escd;
int verd;
int esct;
int osize = 02;
int size = 02;
int rx;
int xx;
int yy = MAXY+62+48;
int leadtot = -31;
int ohy = -1;
int ohx = -1;
int oxb = -1;
int oly = -1;
int olx = -1;
int tflag;
int railmag;
int lead;
int skip;
int pgskip;
int ksize = ';';
int mcase;
int stab[] = {010,0,01,07,02,03,04,05,0211,06,0212,0213,0214,0215,0216,0217};
int rtab[] = {6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 28, 36, 18};
int ktab[] = {';',';',';',';',';',';',':',':','9','9','9','9','8','8','8','9'};
int first = 1;
int alpha;
extern char *asctab[128];
extern char *spectab[128];
int erase = 1;
int	(*sigint)();
int	(*sigquit)();

main(argc,argv)
int argc;
char **argv;
{
	register i, j;
	register char *k;
	extern ex();

	while((--argc > 0) && ((++argv)[0][0]=='-')){
		switch(argv[0][1]){
			case 'p':
				ap = &argv[0][2];
				dfact = 72;
				if(i = atoi())pl = i/3;
				continue;
			case 't':
				tflag++;
				continue;
			case 's':
				ap = &argv[0][2];
				dfact = 1;
				pgskip = atoi();
				continue;
			default:
				dfact = 1;
				ap = &argv[0][1];
				if(i = atoi())mpy = i;
				if(i = atoi())div = i;
				continue;
		}
	}
	if(argc){
		if (freopen(argv[0], "r", stdin) == NULL) {
			fprintf(stderr, "tc: cannot open %s\n", argv[0]);
			exit(1);
		}
	}
	sigint = signal(SIGINT, ex);
	sigquit = signal(SIGQUIT, SIG_IGN);
	while((i = getchar()) != EOF){
		if(!i)continue;
		if(i & 0200){
			esc += (~i) & 0177;
			continue;
		}
		if(esc){
			if(escd)esc = -esc;
			esct += esc;
			xx += (esc*mpy + rx)/div;
			rx = (esc*mpy + rx)%div;
			sendpt();
			esc = 0;
		}
		switch(i){
			case 0100:	/*init*/
				escd = verd = mcase = railmag = xx = 0;
				yy = MAXY + 48;
				leadtot = -31;
				ohy = oxb = oly = ohx = olx = -1;
				oput(US);
				fflush(stdout);
				if(!first && !tflag)kwait();
				if(first){
					first = 0;
					yy += 62;
				}
				init();
				continue;
			case 0101:	/*lower rail*/
				railmag &= ~01;
				continue;
			case 0102:	/*upper rail*/
				railmag |= 01;
				continue;
			case 0103:	/*upper mag*/
				railmag |= 02;
				continue;
			case 0104:	/*lower mag*/
				railmag &= ~02;
				continue;
			case 0105:	/*lower case*/
				mcase = 0;
				continue;
			case 0106:	/*upper case*/
				mcase = 0100;
				continue;
			case 0107:	/*escape forward*/
				escd = 0;
				continue;
			case 0110:	/*escape backward*/
				escd = 1;
				continue;
			case 0111:	/*stop*/
				continue;
			case 0112:	/*lead forward*/
				verd = 0;
				continue;
			case 0113:	/*undefined*/
				continue;
			case 0114:	/*lead backward*/
				verd = 1;
				continue;
			case 0115:	/*undefined*/
			case 0116:
			case 0117:
				continue;
		}
		if((i & 0340) == 0140){	/*leading*/
			lead = (~i) & 037;
			if(verd)lead = -lead;
			if((leadtot += lead) > pl){
				leadtot = lead;
				oput(US);
				fflush(stdout);
				if(!tflag)kwait();
				yy = MAXY;
				if(pgskip)--pgskip;
				init();
				continue;
			}
			if(skip)continue;
			if((yy -= (lead<<1)) < 0){
				skip++;
				yy = 0;
			}else sendpt();
			continue;
		}
		if((i & 0360) == 0120){	/*size change*/
			i &= 017;
			for(j = 0; i != (stab[j] & 017); j++);
			osize = size;
			size = stab[j];
			psize = rtab[j];
			ksize = ktab[j];
			oput(ESC);
			oput(ksize);
			i = 0;
			if(!(osize & DBL) && (size & DBL))i = -55;
			else if((osize & DBL) && !(size & DBL))i = 55;
			if(escd)i = -i;
			esc += i;
			continue;
		}
		if(i & 0300)continue;
		i = (i & 077) | mcase;
		if(railmag != 03)k = asctab[i];
		else k = spectab[i];
		if(alpha)sendpt();
		if(*k!='\0'){
			oput(US);
			while(*k & 0377)oput(*k++);
			alpha++;
			continue;
		}else{
			if(railmag != 03){
				switch(i){
				case 0124: lig("fi"); break;
				case 0125: lig("fl"); break;
				case 0126: lig("ff"); break;
				case 0130: lig("ffl"); break;
				case 0131: lig("ffi"); break;
				default: continue;
				}
			}
			continue;
		}
	}
	ex();
}
lig(x)
char *x;
{
	register i, j;
	register char *k;

	j = 0;
	k = x;
	oput(US);
	oput(*k++);
	i = psize * 8 * mpy / (div * 6); /* 8/36 em */
	while(*k){
		xx += i;
		j += i;
		sendpt();
		oput(US);
		oput(*k++);
	}
	xx -= j;
	sendpt();
}
init(){

	fflush(stdout);
	if(erase){
		oput(ESC);
		oput(FF);
	}else erase = 1;
	oput(ESC);
	oput(ksize);
	/*delay about a second*/
/* let the system do it...
	for(i = 960; i > 0; i--)oput(GS);
*/
	skip = 0;
	sendpt();
}
ex(){
	yy = MAXY;
	xx = 0;
	sendpt();
	oput(ESC);
	oput(';');
	oput(US);
	fflush(stdout);
	exit(0);
}
kwait(){
	char buf[128]; char *bptr; char c;
	if(pgskip) return;
next:
	bptr=buf;
	while((c=readch())&&(c!='\n')) *bptr++=c;
	*bptr=0;
	if(bptr!=buf){
		bptr = buf;
		if(*bptr == '!'){callunix(&buf[1]); fputs("!\n", stderr); goto next;}
		else switch(*bptr++){
			case 'e':
				erase = 0;
				goto next;
			case 's':
				ap = &buf[1];
				dfact = 1;
				pgskip = atoi() + 1;
				goto next;
			default:
				fputs("?\n", stderr);
				goto next;
		}
	}
	else if (c==0) ex();
	else	return;
}
callunix(line)
char line[];
{
	int rc, status, unixpid;
	if( (unixpid=fork())==0 ) {
		signal(SIGINT,sigint); signal(SIGQUIT,sigquit);
		close(0); dup(2);
		execl(_PATH_BSHELL, "-sh", "-c", line, 0);
		exit(255);
	}
	else if(unixpid == -1)
		return;
	else{	signal(SIGINT, SIG_IGN); signal(SIGQUIT, SIG_IGN);
		while( (rc = wait(&status)) != unixpid && rc != -1 ) ;
		signal(SIGINT,ex); signal(SIGQUIT,sigquit);
	}
}
readch(){
	char c;
	if (read(2,&c,1)<1) c=0;
	return(c);
}
sendpt(){
	int hy,xb,ly,hx,lx;

	oput(GS);
	hy = ((yy>>7) & 037);
	xb = ((xx & 03) + ((yy<<2) & 014) & 017);
	ly = ((yy>>2) & 037);
	hx = ((xx>>7) & 037);
	lx = ((xx>>2) & 037);
	if(hy != ohy)oput(hy | 040);
	if(xb != oxb)oput(xb | 0140);
	if((ly != oly) || (hx != ohx) || (xb != oxb))
		oput(ly | 0140);
	if(hx != ohx)oput(hx | 040);
	oput(lx | 0100);
	ohy = hy;
	oxb = xb;
	oly = ly;
	ohx = hx;
	olx = lx;
	alpha = 0;
	return;
}
atoi()
{
	register i, j, acc;
	int field, digits;
	long dd;
	long tscale();

	field = digits = acc = 0;
a1:
	while(((j = (i = getch()) - '0') >= 0) && (j <= 9)){
		field++;
		digits++;
		acc = 10*acc + j;
	}
	if(i == '.'){
		field++;
		digits = 0;
		goto a1;
	}
	if(!(ch = i))ch = 'x';
	dd = tscale(acc);
	acc = dd;
	if((field != digits) && (digits > 0)){
		j = 1;
		while(digits--)j *= 10;
		acc = dd/j;
	}
	nonumb = !field;
	ch = 0;
	return(acc);
}
long tscale(n)
int n;
{
	register i, j;

	switch(i = getch()){
		case 'u':
			j = 1;
			break;
		case 'p':	/*Points*/
			j = 6;
			break;
		case 'i':	/*Inches*/
			j = 432;
			break;
		case 'c':	/*Centimeters; should be 170.0787*/
			j = 170;
			break;
		case 'P':	/*Picas*/
			j = 72;
			break;
		default:
			j = dfact;
			ch = i;
	}
	return((long)n*j);
}
getch(){
	register i;

	if(ch){
		i = ch;
		ch = 0;
		return(i);
	}
	return(*ap++);
}

char *asctab[128] = {
"\0",	/*blank*/
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
"\0",	/*blank*/
"p",	/*p*/
"-",	/*_ 3/4 em dash*/
";",	/*;*/
"\0",	/*blank*/
"a",	/*a*/
"_",	/*rule*/
"c",	/*c*/
"`",	/*` open*/
"e",	/*e*/
"\'",	/*' close*/
"o",	/*o*/
"\0",	/*1/4*/
"r",	/*r*/
"\0",	/*1/2*/
"v",	/*v*/
"-",	/*- hyphen*/
"w",	/*w*/
"q",	/*q*/
"/",	/*/*/
".",	/*.*/
"g",	/*g*/
"\0",	/*3/4*/
",",	/*,*/
"&",	/*&*/
"y",	/*y*/
"\0",	/*blank*/
"%",	/*%*/
"\0",	/*blank*/
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
"\0",	/*blank*/
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
"",	/*fi*/
"",	/*fl*/
"",	/*ff*/
"\033\016Z\bM\033\017",	/*cent sign*/
"",	/*ffl*/
"",	/*ffi*/
"(",	/*(*/
")",	/*)*/
"[",	/*[*/
"]",	/*]*/
"\033\016J\033\017",	/*degree*/
"\033\016M\b_\033\017",	/*dagger*/
"=",	/*=*/
"\033\016O\b&\033\017",	/*registered*/
":",	/*:*/
"+",	/*+*/
"\0",	/*blank*/
"!",	/*!*/
"\033\016O\b~\033\017",	/*bullet*/
"?",	/*?*/
"\'",	/*foot mark*/
"|",	/*|*/
"\0",	/*blank*/
"\033\016O\b#\033\017",	/*copyright*/
"\033\016L\033\017",	/*square*/
"$" };	/*$*/

char *spectab[128] = {
"\0",	/*blank*/
"\033\016(\bM\033\017",	/*psi*/
"\033\016o\b_\033\017",	/*theta*/
"v\b)",	/*nu*/
"\033\016V\b,\033\017",	/*mu*/
"\033\016)\b?\033\017",	/*lambda*/
"\033\016I\033\017",	/*iota*/
"S\b\033\016Z\033\017",	/*zeta*/
"o\b\'",	/*sigma*/
"o\b\033\0165\033\017",	/*delta*/
"\033\016b\033\017",	/*beta*/
"\033\016e\bc\033\017",	/*xi*/
"j\b\033\016C\033\017",	/*eta*/
"\033\016O\bM\033\017",	/*phi*/
"\033\016(\033\017",	/*upsilon*/
"\033\016k\033\017",	/*kappa*/
"\0",	/*blank*/
"T\b\033\016S\033\017",	/*pi*/
"@",	/*at-sign*/
"\033\016U\033\017",	/*down arrow*/
"\0",	/*blank*/
"\033\016A\033\017",	/*alpha*/
"|",	/*or*/
"l\b/",	/*chi*/
"\"",	/*"*/
"\033\016E\033\017",	/*epsilon*/
"=",	/*=*/
"\033\016O\033\017",	/*omicron*/
"\033\016[\033\017",	/*left arrow*/
"\033\016R\033\017",	/*rho*/
"\033\016Y\033\017",	/*up arrow*/
"\033\016N\033\017",	/*tau*/
"_",	/*underrule*/
"\\",	/*\*/
"I\b\033\016(\033\017",	/*Psi*/
"\033\016O\bJ\033\017",	/*bell system sign*/
"\033\016W\bX\033\017",	/*infinity*/
"`\b/",	/*gamma*/
"\033\016X\bF\033\017",	/*improper superset*/
"\033\016A\033\017",	/*proportional to*/
"\033\016\\\b]\033\017",	/*right hand*/
"\033\016W\033\017",	/*omega*/
"\0",	/*blank*/
"\033\016G\033\017",	/*gradient*/
"\0",	/*blank*/
"I\033\016\bO\033\017",	/*Phi*/
"O\b=",	/*Theta*/
"O\b_",	/*Omega*/
"\033\016V\033\017",	/*cup (union)*/
"\033\016@\033\017",	/*root en*/
"s",	/*terminal sigma*/
"\033\016)\bK\033\017",	/*Lambda*/
"-",	/*minus*/
"\033\016S\bK\033\017",	/*Gamma*/
"\033\016i\033\017",	/*integral sign*/
"\033\016t\b'\033\017",	/*Pi*/
"\033\016Z\033\017",	/*subset of*/
"\033\016X\033\017",	/*superset of*/
"\033\016T\033\017",	/*approximates*/
"o\b`",	/*partial derivative*/
"\033\016H\033\017",	/*Delta*/
"\033\016I\b'\033\017",	/*square root*/
">\b\033\016F\b@\033\017",	/*Sigma*/
"\033\016T\bF\033\017",	/*approx =*/
"\0",	/*blank*/
">",	/*>*/
"\033\016_\bF\b@\033\017",	/*Xi*/
"<",	/*<*/
"/",	/*slash (longer)*/
"\033\016C\033\017",	/*cap (intersection)*/
"\033\016y\033\017",	/*Upsilon*/
"\033\016|\033\017",	/*not*/
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
"\033\016=\033\017",	/*multiply*/
"\033\016+\033\017",	/*divide*/
"+\b_",	/*plus-minus*/
"\033\016$\033\017",	/*<=*/
"\033\016^\033\017",	/*>=*/
"=\b_",	/*identically equal*/
"\033\016*\033\017",	/*not equal*/
"{",	/*{*/
"}",	/*}*/
"\'",	/*' acute accent*/
"`",	/*` grave accent*/
"^",	/*^*/
"#",	/*sharp*/
"\033\016|\b[\033\017",	/*left hand*/
"\033\016c\b_\033\017",	/*member of*/
"~",	/*~*/
"\033\016O\b/\033\017",	/*empty set*/
"\0",	/*blank*/
"\033\016%\bM\033\017",	/*dbl dagger*/
"|",	/*box rule*/
"*",	/*asterisk*/
"\033\016Z\bF\033\017",	/*improper subset*/
"\033\016O\033\017",	/*circle*/
"\0",	/*blank*/
"+",	/*eqn plus*/
"\033\016]\033\017",	/*right arrow*/
"g\b\033\016C\033\017" };	/*section mark*/
