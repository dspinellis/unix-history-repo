#ifndef lint
static char sccsid[] = "@(#)t6.c	4.2 2/23/83";
#endif lint

#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"

/*
troff6.c

width functions, sizes and fonts
*/

extern	int	inchar[LNSIZE], *pinchar;	/* XXX */
extern int eschar;
extern int widthp;
extern int ohc;
extern int xpts;
extern int xfont;
extern int code;
extern int smnt;
extern int setwdf;
extern int cs;
extern int ccs;
extern int spacesz;
extern char trtab[];
extern int xbitf;
extern int mfont;
extern int mpts;
extern int pfont;
extern int ppts;
extern int oldbits;
extern int chbits;
extern int spbits;
extern int nonumb;
extern int noscale;
extern int font;
extern int font1;
extern int pts;
extern int pts1;
extern int apts;
extern int apts1;
extern int sps;
extern int nlflg;
extern int nform;
extern int dfact;
extern int lss;
extern int lss1;
extern int vflag;
extern int ch0;
extern int lg;
char *fontfile = "/usr/lib/font/ftXX";
int ffi = 0;
extern int bd;
extern int level;
extern int ch;
extern int res;
extern int ptid;
extern char W1[],W2[],W3[],W4[];
extern int xxx;
int trflg;
char *fontab[] = {W1,W2,W3,W4};
int fontlab[] = {'R','I','B','S',0};
char pstab[] = {6,7,8,9,10,11,12,14,16,18,20,22,24,28,36,0};
char psctab[] = {010,000,001,007,002,003,004,005,0211,006,
		0212,0213,0214,0215,0216,0};
int cstab[4], ccstab[4];
int bdtab[4];
int sbold = 0;
int spsz = 0;
struct fz {
	char sign;
	char size;
	int inc;
	} fz[4];

width(c)
int c;
{
	register i,j,k;

	j = c;
	k = 0;
	if(j & MOT){
		if(j & VMOT)goto rtn;
		k = j & ~MOTV;
		if(j & NMOT)k = -k;
		goto rtn;
	}
	if((i = (j & CMASK)) == 010){
		k = -widthp;
		goto rtn;
	}
	if(i == PRESC)i = eschar;
	if((i == ohc) ||
	   (i >= 0370))goto rtn;
	if((j>>BYTE) == oldbits){
		xfont = pfont;
		xpts = ppts;
	}else xbits(j);
	if(j & ZBIT)goto rtn;
	if(!trflg)i = trtab[i] & BMASK;
	if((i -= 32) < 0)goto rtn;
	k = getcw(i);
	if(bd)k += bd - 1;
	if(cs)k = cs;
	widthp = k;
rtn:
	xbitf = trflg = 0;
	return(k);
}
getcw(i)
int i;
{
	register j,k;
	register char *p;
	int x;
	extern char codetab[];

	bd = 0;
	if((code = codetab[i])  & 0200){
		if(smnt){
			p = fontab[smnt-1];
			if(xfont == (sbold-1))bd = bdtab[smnt-1];
			goto g0;
		}
		code = 0;
		k = 36;
		goto g1;
	}
	p = fontab[xfont];
g0:
	if(!i)k = spacesz;
	else k = *(p + i) & BMASK;
	if(setwdf)v.ct |= ((k>>6) & 3);
g1:
	k = (j = (k&077)*(xpts&077))/6;
	if((j%6) >= 3)k++;
	if(cs = cstab[xfont]){
		if(ccs = ccstab[xfont])x = ccs; else x = xpts;
		cs = (j = (cs&077)*(x&077))/6;
		if((j%6) >= 3)cs++;
	}
	if(!bd)bd = bdtab[xfont];
	return(k);
}
xbits(i)
int i;
{
	register j, k;

/*
	if((j = i >> BYTE) == oldbits){
		xfont = pfont;
		xpts = ppts;
		goto rtn;
	}
*/
	j = i >> BYTE;
	xfont = (j>>1) & 03;
	if(k = (j>>3) & 017){
		xpts = pstab[--k];
		if(psctab[k] < 0)xpts |= DBL;
		oldbits = j;
		pfont = xfont;
		ppts = xpts;
		goto rtn;
	}
	switch(xbitf){
		case 0:
			xfont = font;
			xpts = pts;
			break;
		case 1:
			xfont = pfont;
			xpts = ppts;
			break;
		case 2:
			xfont = mfont;
			xpts = mpts;
	}
rtn:
	xbitf = 0;
}
setch(){
	register i,*j,k;
	extern int chtab[];

	if((i = getrq()) == 0)return(0);
	for(j=chtab;*j != i;j++)if(*(j++) == 0)return(0);
	k = *(++j) | chbits;
/*
	if((i & CMASK) == '*'){
		if(((i = find('R',fontlab)) < 0) &&
		   ((i = find('G',fontlab)) < 0))
			return(k);
		else return((k & ~(03<<(BYTE+1))) | (i<<(BYTE+1)));
	}
*/
	return(k);
}
find(i,j)
int i,j[];
{
	register k;

	if(((k = i-'0') >= 1) && (k <= 4) && (k != smnt))return(--k);
	for(k=0; j[k] != i; k++)if(j[k] == 0)return(-1);
	return(k);
}
casefz(){
	register i, j, k;
	int savinc;

	k = 0;
fz0:
	if(skip() || !(i = getrq()) ||
	  ((j = find(i,fontlab))  == -1)){
		if(k)goto fz1;
		else return;
	}
	if(j == (smnt-1)){
		k = smnt;
		goto fz0;
	}
	if(k){
		spsz = j + 1;
		j = k -1;
	}
fz1:
	if((j==font) && fz[j].inc)savinc = fz[j].inc;
	else savinc = 0;
	fz[j].inc = fz[j].sign = fz[j].size = 0;
	if(skip()){
		if(k)spsz = 0;
		goto fz2;
	}
	if(((i=((k=getch()) & CMASK)) == '+') || (i == '-'))fz[j].sign = i;
	else{
		fz[j].sign = 0;
		ch = k;
	}
	noscale++;
	fz[j].size = atoi();
	noscale = 0;
fz2:
	if(j==font)casps1(apts + savinc);
	else if(j == smnt-1)mchbits();
}
caseps(){
	register i;

	if(skip())i = apts1;
	else{
		noscale++;
		i = inumb(&apts);
		noscale = 0;
		if(nonumb)return;
	}
	casps1(i);
}
casps1(i)
int i;
{
	if(i <= 0)return;
	if(fz[font].size){
		i = getfz(font, i);
	}
	apts1 = apts;
	apts = i;
	pts1 = pts;
	pts = findps(i & 077);
	mchbits();
}
findps(i)
int i;
{
	register j, k;

	for(j=0; i > (k = pstab[j]);j++)if(!k){k=pstab[--j];break;}
	if(psctab[j] < 0)k |= DBL;
	return(k);
}
mchbits(){
	register i, j, k;

	spbits = 0;
	i = pts & 077;
	for(j=0; i > (k = pstab[j]);j++)if(!k){k=pstab[--j];break;}
	chbits = (((++j)<<2) | font) << (BYTE + 1);
	sps = width(' ' | chbits);
	if(font == (spsz-1)){
		i = findps(getfz(smnt-1, apts + fz[font].inc));
		for(j=0; i > (k = pstab[j]);j++)if(!k){k=pstab[--j];break;}
		spbits = (((++j)<<2) | font) << (BYTE + 1);
	}
}
getfz(x,y)
int x, y;
{
	register i, j, k;

	i = fz[x].size;
	j = fz[x].sign;
	if(i || j){
		if(j == '+')i += y;
		else if(j == '-')i = y - i;
	}
	fz[x].inc = y - i;
	return(i);
}
setps(){
	register i,j;

	if((((i=getch() & CMASK) == '+')  || (i == '-')) &&
	  (((j=(ch = getch() & CMASK) - '0') >= 0) && (j <= 9))){
		if(i == '-')j = -j;
		ch = 0;
		casps1(apts+j);
		return;
	}
	if((i -= '0') == 0){
		casps1(apts1);
		return;
	}
	if((i > 0) && (i <= 9)){
		if((i <= 3) &&
		  ((j=(ch = getch() & CMASK) - '0') >= 0) && (j <= 9)){
			i = 10*i +j;
			ch = 0;
		}
		casps1(i);
	}
}
caseft(){
	skip();
	setfont(1);
}
setfont(a)
int a;
{
	register i,j;

	if(a)i = getrq();
		else i = getsn();
	if(!i || (i == 'P')){
		j = font1;
		goto s0;
	}
	if(i == 'S')return;
	if((j = find(i,fontlab))  == -1)return;
s0:
	font1 = font;
	font = j;
	i = 0;
	if(fz[font1].size){
		i++;
		casps1(apts + fz[font1].inc);
	}else if(fz[font].size){
		i++;
		casps1(apts);
	}
	if(!i)mchbits();
}
setwd(){
	register i, base, wid;
	int delim, em, k;
	int savlevel, savhp, savapts, savapts1, savfont, savfont1,
		savpts, savpts1;
        int *savpinchar, *p, *q, tempinchar[LNSIZE];    /* XXX */

	base = v.st = v.sb = wid = v.ct = 0;
	if((delim = getch() & CMASK) & MOT)return;
	savhp = v.hp;
        savpinchar = pinchar;   /* XXX */
        for (p=inchar, q=tempinchar; p < pinchar; )     /* XXX */
                *q++ = *p++;    /* XXX */
        pinchar = inchar;       /* XXX */
	savlevel = level;
	v.hp = level = 0;
	savapts = apts;
	savapts1 = apts1;
	savfont = font;
	savfont1 = font1;
	savpts = pts;
	savpts1 = pts1;
	setwdf++;
	while((((i = getch()) & CMASK) != delim) && !nlflg){
		wid += width(i);
		if(!(i & MOT)){
			em = (xpts & 077)*6;
		}else if(i & VMOT){
			k = i & ~MOTV;
			if(i & NMOT)k = -k;
			base -= k;
			em = 0;
		}else continue;
		if(base < v.sb)v.sb = base;
		if((k=base + em) > v.st)v.st = k;
	}
	nform = 0;
	setn1(wid);
	v.hp = savhp;
        pinchar = savpinchar;   /* XXX */
        for (p=inchar, q=tempinchar; p < pinchar; )     /* XXX */
                *p++ = *q++;    /* XXX */
	level = savlevel;
	apts = savapts;
	apts1 = savapts1;
	font = savfont;
	font1 = savfont1;
	pts = savpts;
	pts1 = savpts1;
	mchbits();
	setwdf = 0;
}
vmot(){
	dfact = lss;
	vflag++;
	return(mot());
}
hmot(){
	dfact = 6 * (pts & 077);
	return(mot());
}
mot(){
	register i, j;

	j = HOR;
	getch(); /*eat delim*/
	if(i = atoi()){
		if(vflag)j = VERT;
		i = makem(quant(i,j));
	}
	getch();
	vflag = 0;
	dfact = 1;
	return(i);
}
sethl(k)
int k;
{
	register i;

	i = 3 * (pts & 077);
	if(k == 'u')i = -i;
	else if(k == 'r')i = -2*i;
	vflag++;
	i = makem(i);
	vflag = 0;
	return(i);
}
makem(i)
int i;
{
	register j;

	if((j = i) < 0)j = -j;
	j = (j & ~MOTV) | MOT;
	if(i < 0)j |= NMOT;
	if(vflag)j |= VMOT;
	return(j);
}
getlg(i)
int i;
{
	register j, k;

	switch((j = getch0()) & CMASK){
		case 'f':
			if(lg!=2){switch((k =getch0()) & CMASK){
					case 'i':
						j = 0214;
						break;
					case 'l':
						j = 0215;
						break;
					default:
						ch0 = k;
						j = 0213;
				}
			}else j = 0213;
			break;
		case 'l':
			j = 0212;
			break;
		case 'i':
			j = 0211;
			break;
		default:
			ch0 = j;
			j = i;
	}
	return((i & ~CMASK) | j);
}
caselg(){

	lg = 1;
	if(skip())return;
	lg = atoi();
}
casefp(){
	register i, j, k;
	int x;

	if (ffi == 0)
		while (fontfile[ffi] != 'X')
			ffi++;
	skip();
	if(((i = (getch() & CMASK) - '0' -1) < 0) || (i >3)){prstr("fp: bad font position\n"); return;}
	if(skip() || !(j = getrq())){prstr("fp: no font name\n"); return;}
	fontfile[ffi] = j & BMASK;
	fontfile[ffi+1] = j>>BYTE;
	if((k = open(fontfile,0)) < 0){
		prstr("Cannot open ");
	c0:
		prstr(fontfile);
		prstr("\n");
		done(-1);
	}
	if(lseek(k,8L * sizeof(int),0) < 0)goto c1; 
	if(read(k,fontab[i],256-32) != 256-32){
	c1:
		prstr("Cannot read ");
		goto c0;
	}
	close(k);
	if(i == (smnt-1)){smnt = 0; sbold = 0; spsz = 0;}
	if((fontlab[i] = j) == 'S')smnt = i + 1;
	bdtab[i] = cstab[i] = ccstab[i] = 0;
	fz[i].inc = fz[i].sign = fz[i].size = 0;
	if(ptid != 1){
		prstr("Mount font ");
		prstr(&fontfile[ffi]);
		prstr(" on ");
		x = PAIR((i + '1'),0);
		prstr((char *)&x);
		prstr("\n");
	}
}
casecs(){
	register i, j;

	noscale++;
	skip();
	if(!(i=getrq()) ||
	  ((i = find(i,fontlab)) < 0))goto rtn;
	skip();
	cstab[i] = atoi();
	skip();
	j = atoi();
	if(!nonumb)ccstab[i] = findps(j);
rtn:
	noscale = 0;
}
casebd(){
	register i, j, k;

	k = 0;
bd0:
	if(skip() || !(i = getrq()) ||
	  ((j = find(i,fontlab))  == -1)){
		if(k)goto bd1;
		else return;
	}
	if(j == (smnt-1)){
		k = smnt;
		goto bd0;
	}
	if(k){
		sbold = j + 1;
		j = k -1;
	}
bd1:
	skip();
	noscale++;
	bdtab[j] = atoi();
	noscale = 0;
}
casevs(){
	register i;

	skip();
	vflag++;
	dfact = 6; /*default scaling is points!*/
	res = VERT;
	i = inumb(&lss);
	if(nonumb)i = lss1;
	if(i < VERT)i = VERT;
	lss1 = lss;
	lss = i;
}
casess(){
	register i;

	noscale++;
	skip();
	if(i = atoi()){
		spacesz = i& 0177;
		sps = width(' ' | chbits);
	}
	noscale = 0;
}
xlss(){
	register i, j;

	getch();
	dfact = lss;
	i = quant(atoi(),VERT);
	dfact = 1;
	getch();
	if((j = i) < 0)j = -j;
	ch0 = ((j & 03700)<<3) | HX;
	if(i < 0)ch0 |= 040000;
	return(((j & 077)<<9) | LX);
}
