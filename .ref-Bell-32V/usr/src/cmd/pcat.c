#include <signal.h>
#define X 0
/*
C version of tcatsim
*/

#define OBSZ 512
#define MAXY 3071
#define US 037
#define GS 035
#define ESC 033
#define FF 014
#define SO 016
#define SI 017
#define DBL 0200

int pl = 11*144;
int mpy = 1;
int div = 1;
char *ap;
int ch;
int nonumb;
int psize = 10;
int dfact = 1;
int ibuf[259];
char obuf[OBSZ];
char *obufp = obuf;
int esc;
int escd;
int verd;
int esct;
int osize = 02;
int size = 02;
int rx;
int xx = 0;
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
int rtab[] = {6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 28, 26, 18};
int ktab[] = {';',';',';',';',';',';',':',':','9','9','9','9','8','8','8','9'};
int od = 1;
int first = 1;
int alpha;
int xxx;
extern int *drawtab[], *moretab[];

main(argc,argv)
int argc;
char **argv;
{
	register i, j;
	extern ex();
	extern char asctab[];
	extern char spectab[];

	openpl();
	space(0,0,4095,3071);
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
		if(fopen(argv[0], ibuf) < 0){
			prstr("Cannot open: ");
			prstr(argv[0]);
			prstr("\n");
			exit(1);
		}
	}
	signal(SIGINT, ex);
	while((i = getc(ibuf)) >= 0){
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
					if(first){
						first = 0;
						yy += 62;
					}
				leadtot = -31;
				ohy = oxb = oly = ohx = olx = -1;
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
				flusho();
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
			i = 0;
			if(!(osize & DBL) && (size & DBL))i = -55;
			else if((osize & DBL) && !(size & DBL))i = 55;
			if(escd)i = -i;
			esc += i;
			continue;
		}
		if(i & 0300)continue;
		i = (i & 077) | mcase;
		if(railmag != 03)j = asctab[i];
		else j = spectab[i];
		if(alpha)sendpt();
		if (j== 0) { /* see if we can draw this character */
			trace( railmag == 03 ? drawtab[i] : moretab[i]);
			continue;
		}else if(j){
			oput(j);
			alpha++;
		}
	}
	ex();
}

# define SCL(x) (psize*x/10)
# define PT(x,y) xx-10+SCL(x), yy-20+SCL(y)
trace (p)
	int *p;
{
if (p==0) return;
while (*p)
	{
	switch (*p)
		{
		case 'l':
/*
			line(xx-10+SCL(p[1]),yy-20+SCL(p[2]),xx-10+SCL(p[3]),yy-20+SCL(p[4]));
 */ line(PT(p[1], p[2]), PT(p[3], p[4]));
			p+=5;
			break;
		case 't':
			move (xx+p[2], yy+p[3]);
			label(p+1);
			p+=4;
			break;
		case 'c':
			circle(PT(p[1],p[2]), SCL(p[3]));
			p+=4;
			break;
		case 'a':
			arc (PT(p[1],p[2]), PT(p[3],p[4]), PT(p[5],p[6]));
			p+=7;
			break;
		}
	}
}

init(){
	register i;

	erase();
	flusho();
	skip = 0;
	sendpt();
}
ex(){
	yy = MAXY;
	xx = 0;
	sendpt();
	closepl();
	exit(0);
}
kwait(){
	;
}
oput(i)
char i;
{
	if(pgskip)return;
	label(&i);
}
flusho(){
	;
}
sendpt(){
	move(xx,yy);
	alpha = 0;
	return;
}
prstr(s)
char *s;
{
	register i;

	for(i=0;*s;i++)s++;
	write(2,s-i,i);
}
atoi()
{
	register i, j, acc;
	int field, digits, *dd, *tscale();

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
	acc = dd[0];
	if((field != digits) && (digits > 0)){
		j = 1;
		while(digits--)j *= 10;
		acc = ldiv(dd[1],dd[0],j);
	}
	nonumb = !field;
	ch = 0;
	return(acc);
}
int *tscale(n)
int n;
{
	register i, j;
	static int aa[2];

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
	aa[0] = n * j;
	aa[1] = hmul(n,j);
	return(aa);
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
/* the way this program decides what to do is:
  (1) If the character is on a standard font (0-2)
   
	(a) look in "asctab"; if an entry, print that character.
	   else
	(b) look in "moretab"; if an entry, it points to a vector
	    description, which draw.
  (2) If the character is on the special font (railmag=3)
	(a) look in "spectab"; if an entry >0,
	    print that character.
	(b) if a -1, that requests the apl font; not used.
	(c) if zero, look in "drawtab"; if an entry, it pooints
	    to a vector description, which draw.
   (3) Vector descriptions are calls to the "plot" type routines.
       the possible routines are 'c' (circle), 'l' (line), 't' (label,
       only one character used), and 'a' (arc).
 */
char asctab[128] = {
  0,	/*blank*/
'h',	/*h*/
't',	/*t*/
'n',	/*n*/
'm',	/*m*/
'l',	/*l*/
'i',	/*i*/
'z',	/*z*/
's',	/*s*/
'd',	/*d*/
'b',	/*b*/
'x',	/*x*/
'f',	/*f*/
'j',	/*j*/
'u',	/*u*/
'k',	/*k*/
  0,	/*blank*/
'p',	/*p*/
'-',	/*_ 3/4 em dash*/
';',	/*;*/
  0,	/*blank*/
'a',	/*a*/
'_',	/*rule*/
'c',	/*c*/
'`',	/*` open*/
'e',	/*e*/
'\'',	/*' close*/
'o',	/*o*/
  0,	/*1/4*/
'r',	/*r*/
  0,	/*1/2*/
'v',	/*v*/
'-',	/*- hyphen*/
'w',	/*w*/
'q',	/*q*/
'/',	/*/*/
'.',	/*.*/
'g',	/*g*/
  0,	/*3/4*/
',',	/*,*/
'&',	/*&*/
'y',	/*y*/
  0,	/*blank*/
'%',	/*%*/
  0,	/*blank*/
'Q',	/*Q*/
'T',	/*T*/
'O',	/*O*/
'H',	/*H*/
'N',	/*N*/
'M',	/*M*/
'L',	/*L*/
'R',	/*R*/
'G',	/*G*/
'I',	/*I*/
'P',	/*P*/
'C',	/*C*/
'V',	/*V*/
'E',	/*E*/
'Z',	/*Z*/
'D',	/*D*/
'B',	/*B*/
'S',	/*S*/
'Y',	/*Y*/
  0,	/*blank*/
'F',	/*F*/
'X',	/*X*/
'A',	/*A*/
'W',	/*W*/
'J',	/*J*/
'U',	/*U*/
'K',	/*K*/
'0',	/*0*/
'1',	/*1*/
'2',	/*2*/
'3',	/*3*/
'4',	/*4*/
'5',	/*5*/
'6',	/*6*/
'7',	/*7*/
'8',	/*8*/
'9',	/*9*/
'*',	/***/
'-',	/*minus*/
   0,	/*fi*/
  0,	/*fl*/
  0,	/*ff*/
  0,	/*cent mark*/
  0,	/*ffl*/
  0,	/* ffi */
'(',	/*(*/
')',	/*)*/
'[',	/*[*/
']',	/*]*/
  0,	/*degree*/
  0,	/*dagger*/
'=',	/*=*/
  0,	/*registered*/
':',	/*:*/
'+',	/*+*/
  0,	/*blank*/
'!',	/*!*/
  0,	/*bullet*/
'?',	/*?*/
'\'',	/*foot mark*/
'|',	/*|*/
  0,	/*blank*/
  0,	/*copyright*/
  0,	/*square*/
'$' };	/*$*/

char spectab[128] = {
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
  'u',	/*upsilon*/
  0,	/*kappa*/
  0,	/*blank*/
  0,	/*pi*/
  '@',	/*at sign @*/
  0,	/*down arrow*/
  0,	/*blank*/
  0,	/*alpha*/
'|',	/*or*/
  0,	/*chi*/
'"',	/*"*/
  0,	/*epsilon*/
  '=',	/*equals*/
  'o',	/*omicron*/
  0,	/*left arrow*/
  0,	/*rho*/
  0,	/*up arrow*/
  0,	/*tau*/
'_',	/*underrule*/
'\\',	/*\*/
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
  '-',	/*some horizontal line*/
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
'>',	/*>*/
  0,	/*Xi*/
'<',	/*<*/
'/',	/*slash (longer)*/
  0,	/*cap (intersection)*/
  'Y',	/*Upsilon*/
  0,	/*not*/
'|',	/*right ceiling (rt of ")*/
'|',	/*left top (of big curly)*/
'|',	/*bold vertical*/
'|',	/*left center of big curly bracket*/
'|',	/*left bottom*/
'|',	/*right top*/
'|',	/*right center of big curly bracket*/
'|',	/*right bot*/
'|',	/*right floor (rb of ")*/
'|',	/*left floor (left bot of big sq bract)*/
'|',	/*left ceiling (lt of ")*/
'x',	/*multiply*/
  0,	/*divide*/
  0,	/*plus-minus*/
  0,	/*<=*/
  0,	/*>=*/
  0,	/*identically equal*/
  0,	/*not equal*/
'{',	/*{*/
'}',	/*}*/
'\'',	/*' acute accent*/
'`',	/*` grave accent*/
'^',	/*^*/
  '#',	/*sharp*/
  0,	/*left hand*/
  0,	/*member of*/
'~',	/*~*/
  0,	/*empty set*/
  0,	/*blank*/
  0,	/*dbl dagger*/
'|',	/*box rule*/
  '*',	/*telephone asterisk?*/
  0,	/*improper subset*/
  0,	/*circle*/
  0,	/*blank*/
  '+',	/*eqn plus sign*/
  0,	/*right arrow*/
  0 };	/*section mark*/
# define STOP 0
int dnot[] = { 'l', 0, 15, 25, 15, 'l', 25, 15, 25, 5, STOP};
int dlambda[] = {'l', 0, 40, 6, 40, 'l', 6, 40, 30, 0, 'l', 6, 0, 18, 20, STOP};
int dSigma[] = {'l', 0, 0, 30, 0, 'l', 0, 40, 30, 40, 'l', 0, 0, 10, 20, 'l', 10, 20, 0, 40, STOP};
int dsquare[] = {'l', 0, 0, 30, 0, 'l', 0, 0, 0, 30, 'l', 0, 30, 30, 30, 'l', 30, 0, 30, 30, STOP};
int dDelta[] = {'l', 0,0, 30, 0, 'l', 0, 0, 15, 40, 'l', 15,40, 30, 0, STOP};
int dintsign[] = { 'a', 25,30, 30,32, 20,32, 'l', 20, 32, 10, 8, 'a',5, 10, 0, 8, 10, 8, STOP};
int dtheta[] = {'a', 25, 15, 10, 30, 10, 0, 'a', -5, 15, 10, 0, 10, 30, 'l', 5, 15, 12, 15, STOP};
int dcopyr[] = {'c', 20, 20, 20, 't', 'c', 7,3, STOP};
int dregist[] = {'c', 20, 20, 20, 't', 'R', 7,3, STOP};
int dpi[] = { 'l', 0, 25, 30, 25, 'l', 5, 0, 8, 25, 'l', 17, 0, 20, 25, STOP};
int dPi[] = { 'l', 0, 40, 30, 40, 'l', 5, 0, 5, 40, 'l', 25, 0, 25, 40, STOP};
int dsqroot[] = { 'l', 0,10,5,10, 'l', 8, 10, 15, 0, 'l', 15, 0, 30, 40, STOP};
int dgradient[] = { 'l', 0,40, 30, 40, 'l', 0, 40, 15, 0, 'l', 15, 0, 30, 40, STOP};
int dbeta[] = {'t', 'B', 0, 5, 'l', 0, 20, 0, -10, STOP};
int ddagger[] = {'l', 0, 30, 20, 30, 'l', 10, 0, 10, 40, STOP};
int dpsi[] = { 'a', 15,25, 0, 27, 30, 23, 'l', 0, 0, 30, 40, STOP};
int dmu[] = { 't',  'u', 0,5, 'l', 0,10, 0, -10, STOP};
int dnu[] = { 'l',0,0,0,30, 'l', 0,0,20,30, STOP};
int diota[] = { 'l', 0, 0, 4, 24, 'l', 6, 32, 6, 36, 'l', 0, 0, 5, 0, STOP};
int dsigma[] = { 'c', 15,15,15, 'l', 15, 30, 35, 30, STOP};
int ddelta[] = { 'c', 10, 10, 10, 'a', 16, 28, 18, 37, 14, 19, STOP};
int depsilon[] = { 'a', 15, 15, 15, 30, 15, 0, 'l', 0, 15, 15, 15, STOP};
int dchi[] = { 'l', 0, 30, 4, 30, 'l', 4, 30, 16, 0, 'l', 16, 0, 20, 0, 'l', 0, 0, 20, 30, STOP};
int dtau[] = { 'l', 0, 30, 30, 30, 'l', 10, 0, 15, 30 , 'l', 10, 0, 15, 0, STOP};
int dlesseq[] = { 'l', 0, 20, 30, 30, 'l', 0, 20, 30, 10, 'l', 0, 15, 30, 5, STOP};
int dgreateq[] = { 'l', 0, 30, 30, 20, 'l', 0, 10, 30, 20, 'l', 0, 5, 30, 15, STOP};
int dinfinity[] = { 'c', 10, 10, 10, 'c', 30 , 10, 10, STOP};
int dalpha[] = { 'c', 15, 15, 15, 'a', 52, 15, 35, 30, 35, 0, STOP};
int dphi[] = { 'c', 10, 15, 10, 'l', 5, 0, 15, 30, STOP};
int dgamma[] = { 'l', 10, -10, 30, 30, 'a', 10, 15, 20, 10, 0, 20, STOP};
int dkappa[] = { 'l', 0, 0, 6, 25, 'l', 3, 12, 20, 25, 'l', 6, 15, 20, 0, STOP};
int drho[] = {'c', 15, 15, 10, 'l', 0, -5, 5, 20, STOP};
int dGamma[] = {'l', 0, 0, 0, 35, 'l', 0, 35, 25, 35, 'l', 25, 35, 25, 25, STOP};
int ddownar[]= {'l', 10, 0, 10, 30, 'l', 0, 10, 10, 0, 'l', 10, 0, 20, 10, STOP};
int dupar[] = { 'l', 10, 0, 10, 30, 'l', 0, 20, 10, 30, 'l', 10, 30, 20, 20,STOP};
int dleftar[] = { 'l', 0, 15, 25, 15, 'l', 10, 5, 0, 15, 'l', 10, 25, 0, 15, STOP};
int drightar[] = { 'l', 0, 15, 25, 15, 'l', 15, 5, 25, 15, 'l', 15, 25, 25, 15, STOP};
int ddivide[]= { 'l', 0, 15, 25, 15, 'l', 12, 20, 14, 20, 'l', 12, 10, 14, 10, STOP};
int dcap[] = { 'a', 15, 10, 30, 10, 0, 10, 'l', 0, 0, 0, 10, 'l', 30, 0, 30, 10, STOP};
int dcup[] = {'a', 15, 15, 0, 15, 30, 15, 'l', 0, 15, 0, 25, 'l', 30, 15, 30, 25, STOP};
int dsubset[] = {'l', 0, 0, 10, 0, 'l', 0, 30, 10, 30, 'a', 10, 15, 10, 0, 10, 30,STOP};
int dsupset[] = {'a', 15, 15, 15, 30, 15, 0, 'l', 15, 30, 25, 30, 'l', 15, 0, 25, 0, STOP};
int dimpsubset[] = {'l', 0, 10, 10, 10,  'l', 0, 40, 10, 40, 'a', 10, 25, 10, 10, 10, 40, 'l', 0, 0, 30, 0, STOP};
int dimpsupset[] = {'a', 15, 25, 15, 40, 15, 10, 'l', 15, 40, 25, 40, 'l', 15, 10, 25, 10, 'l', 0, 0, 30, 0, STOP};
int dxi[] = {'l', 0, 30, 25, 30, 'a', 5, 25, 5, 30, 5, 20, 'l', 5, 20, 25, 20, 'a', 5, 15, 5, 20, 5, 10, 'l', 5, 10, 20, 10, 'a', 20, 5, 20, 0, 20, 10, STOP};
int deta[] = {'a', 5,20,10,20,0,20, 'l', 10, 25, 10, 10, 'a', 15, 20, 20, 20, 10, 20, 'l', 20, 20, 20, 0, STOP};
int dzeta[] = {'l', 0, 30, 25, 30, 'a', 20, 20, 20, 30, 20, 10, 'a', 20, 5, 20, 0, 20, 10, STOP};
int dPsi[] = {'l', 0, 0, 30, 0, 'l', 0, 35, 30, 35, 'l', 15, 0, 15, 35, 'a', 15, 25, 0, 25, 30, 25, STOP};
int dPhi[] = { 'l', 0, 0, 30, 0, 'l', 0, 40, 30, 40, 'l', 15, 0, 15, 40, 'c', 15, 20, 10, STOP};
int domega[] = { 'a', 8, 18, 8, 26, 16, 18, 'a', 24, 18, 16, 18, 24, 26, STOP};
int dtsigma[] = {'a', 10, 20, 17, 27, 10, 10, 'a', 10, 5, 10, 0, 10, 10, STOP};
int dpartial[] = { 'c', 10, 10, 10, 'a', 0, 20, 10, 0, 0, 40, STOP};
int dprop[] = {'c', 10,10,10, 'a', 30, 10, 30, 20, 30, 0, STOP};
int dTheta[]= { 'c', 15, 15, 15, 'l', 5,15,25,15, 'l', 5, 13, 5, 17, 'l', 25, 13, 25, 17, STOP};
int dXi[] = { 'l', 0, 40 , 30, 40, 'l' , 0, 0, 30, 0, 'l', 7, 20, 23, 20, STOP};
int dLambda[] = { 'l', 0, 0, 15, 40, 'l', 15, 40, 30, 0, STOP};
int drighth[]= { 'l', 0,0,20,0, 'l', 0, 10, 20, 10, 'l',0,20,20,20, 'l',0,30,35,30, 'l',0,40,35,40,
	'a',20,5,20,0,20,10, 'a', 20,15,20,10,20,20, 'a',20,25,20,20,20,30, 'a',35,35,35,30,35,30,STOP};
int dlefth[] = {'l', 20, 0, 40,0, 'l',20,10,40,10, 'l',20,20,40,20, 'l',5,30,40,30, 'l',5,40,40,40, 
	'a', 20, 5, 20,10,20,0, 'a', 20,15,20,20,20,10, 'a', 20, 25, 20,30,20,20, 'a', 5,35,5,40,5,30, STOP};
int dcircle[] = {'c',15,15,15,STOP};
int dapprox[] = {'a', 7, 0, 14, 15, 0, 15, 'a', 21, 30, 14,15,28,15, STOP};
int dappreq[] = {'a', 7, 0, 14, 15, 0, 15, 'a', 21, 30, 14,15,28,15, 'l', 0, 0, 28, 0, STOP};
int dOmega[] = {'a', 15,25,30,25,0,25, 'l',10,0,0,25, 'l',20,0,30,25, 'l',0,0,10,0, 'l',20,0,30,0, STOP};
int dplmn[] = {'l', 0, 0, 30, 0, 'l', 0, 25, 30, 25, 'l', 15,10, 15, 40, STOP};
int dmultiply[] = {'l',0,0,30,30, 'l', 0,30,30,0, STOP};
int dident[]= {'l', 0,0,30,0, 'l',0,10,30,10, 'l',0,20,30,20, STOP};
int dnoteq[] = {'l',0,10,30,10, 'l',0,25,30,25, 'l', 0,0,30,40, STOP};
int ddbldag[] = {'l',10,0,10,30, 'l',0,5,20,5, 'l',0,25,20,25, STOP};
int dbullet[] = { 'c',10,10,10, 'c',10,10,8, 'c', 10,10,6, 'c', 10, 10, 4, 'c', 10, 10, 2, STOP};
int drooten[] = {'l', 0,30,25,30, STOP};
int dempty[] = { 'c', 15, 15, 15, 'l', 0, 0, 30, 30, STOP};
int dsection [] = {'a', 7,7, 7, 0, 7, 14, 'c', 7,21, 7, 'a', 7, 35, 7, 42, 7, 28, STOP};
int dff[] = { 't', 'f', 0, 0, 't', 'f', 15, 0, STOP};
int dfi[] = { 't', 'f', 0, 0, 't', 'i', 15, 0, STOP};
int dfl[] = { 't', 'f', 0, 0, 't', 'l', 15, 0, STOP};
int dffi[] = { 't', 'f', 0, 0, 't', 'f', 15, 0, 't', 'i', 30, 0, STOP};
int dffl[] = { 't', 'f', 0, 0, 't', 'f', 15, 0, 't', 'l', 30, 0, STOP};
int dcent[] = {'a', 15, 20, 25, 17, 25, 13, 'l', 15, 0, 15, 35, STOP};
int ddegree[] = {'t', 'o', 0, 20, STOP};
int dhalf[] = {'t', '1', 0, 20, 'l', 0, 0, 30, 40, 't', '2', 15, 0, STOP};
int dquarter[] = {'t', '1', 0, 20, 'l', 0, 0, 30, 40, 't', '4', 15, 0, STOP};
int dthreequarter[] = {'t', '3', 0, 20, 'l', 0, 0, 30, 40, 't', '4', 15, 0, STOP};
int dbell[] = {'l', 0, 10, 40, 10, 'a', 0, 15, 0, 10, 5, 15, 'a', 40, 15, 35, 15, 40, 10,
'a', 20, 15, 35, 15, 15, 15, 'l', 20, 35, 20, 40, 'l', 20, 0, 20, 10, 'c', 20, 20, 22, STOP};
int *drawtab[128] = {
  0,	/*blank*/
  dpsi,	/*psi*/
  dtheta,	/*theta*/
  dnu,	/*nu*/
  dmu,	/*mu*/
  dlambda,	/*lambda*/
  diota,	/*iota*/
  dzeta,	/*zeta*/
  dsigma,	/*sigma*/
  ddelta,	/*delta*/
  dbeta,	/*beta*/
  dxi,	/*xi*/
  deta,	/*eta*/
  dphi,	/*phi*/
  0,	/*upsilon ASCII*/
  dkappa,	/*kappa*/
  0,	/*blank*/
  dpi,	/*pi*/
  0,	/* at sign (ASCII) */
  ddownar,	/*down arrow*/
  0,	/*blank*/
  dalpha,	/*alpha*/
  0,	/*or (ASCII)*/
  dchi,	/*chi*/
  0,	/*" (ASCII)*/
  depsilon,	/*epsilon*/
  0,	/*= (ASCII)*/
  0,	/*omicron (ASCII)*/
  dleftar,	/*left arrow*/
  drho,	/*rho*/
  dupar,	/*up arrow*/
  dtau,	/*tau*/
  0,	/*underrule (ASCII)*/
  0,	/*\ (ASCII)*/
  dPsi,	/*Psi*/
  dbell,	/*bell system sign*/
  dinfinity,	/*infinity*/
  dgamma,	/*gamma*/
  dimpsupset,	/*improper superset*/
  dprop,	/*proportional to*/
  drighth,	/*right hand*/
  domega,	/*omega*/
  0,	/*blank*/
  dgradient,	/*gradient*/
  0,	/*blank*/
  dPhi,	/*Phi*/
  dTheta,	/*Theta*/
  dOmega,	/*Omega */
  dcup,	/*cup (union)*/
  drooten,	/*root en*/
  dtsigma,	/*terminal sigma*/
  dLambda,	/*Lambda*/
  0,		/* some kind of horizontal line (ASCII) */
  dGamma,	/*Gamma*/
  dintsign,	/*integral sign*/
  dPi,	/*Pi*/
  dsubset,	/*subset of*/
  dsupset,	/*superset of*/
  dapprox,	/*approximates*/
  dpartial,	/*partial derivative*/
  dDelta,	/*Delta*/
  dsqroot,	/*square root*/
  dSigma,	/*Sigma*/
  dappreq,	/* approx equal */
  0,	/*blank*/
  0,	/*> (ASCII)*/
  dXi,	/*Xi*/
  0,	/*< (ASCII)*/
  0,	/*slash (longer)*/
  dcap,	/*cap (intersection)*/
  0,	/*Upsilon (ASCII Y)*/
  dnot,	/*not*/
  0,	/*right ceiling (rt of ")*/
  0,	/*left top (of big curly)*/
  0,	/*bold vertical*/
  0,	/*left center of big curly bracket*/
  0,	/*left bottom*/
  0,	/*right top*/
  0,	/*right center of big curly bracket*/
  0,	/*right bot*/
  0,	/*right floor (rb of ")*/
  0,	/*left floor (left bot of big sq bract)*/
  0,	/*left ceiling (lt of ")*/
  dmultiply,	/*multiply*/
  ddivide,	/*divide*/
  dplmn,	/*plus-minus*/
  dlesseq,	/*<=*/
  dgreateq,	/*>=*/
  dident,	/*identically equal*/
  dnoteq,	/*not equal*/
  0,	/*{ (ASCII)*/
  0,	/*} (ASCII)*/
  0,	/*' acute accent (ASCII)*/
  0,	/*` grave accent (ASCII)*/
  0,	/*^ (ASCII)*/
  0,	/* sharp (ASCII)*/
  dlefth,	/*left hand*/
  depsilon,	/*member of*/
  0,	/* ~ ASCII*/
  dempty,	/*empty set*/
  0,	/*blank*/
  ddbldag,	/*dbl dagger*/
  0,	/*box rule ASCII*/
  0,	/*asterisk (ASCII)*/
  dimpsubset,	/*improper subset*/
  dcircle,	/*circle*/
  0,	/*blank*/
  0,	/*plus (ASCII)*/
  drightar,	/*right arrow*/
  dsection };	/*section mark*/
int *moretab[128] = {
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	dquarter, /* one quarter 1/4 */
	0,
	dhalf, /* one half 1/2 */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	dthreequarter, /* 3 /4 */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	dfi,	/* fi */
	dfl, 	/* fl */
	dff, 	/* ff */
	dcent,	/* cent mark */
	dffl,	/* ffl */
	dffi,	/* ffi */
	0,
	0,
	0,
	0,
	0,
	ddagger, /* dagger */
	0,
	dregist, /* registered */
	0,
	0,
	0,
	0,
	dbullet, /* bullet */
	0,
	0,
	0,
	0,
	dcopyr, /* copyright */
	dsquare, /* square */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	0
	};
