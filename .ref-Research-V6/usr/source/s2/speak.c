#
/*	Voice Synthesizer Program "Speak"			*/
/*	Copyright 1974, Bell Telephone Laboratories, Inc.	*/
/*								*/
/*	Language: C						*/
/*	Programmer: M. D. McIlroy				*/
/*								*/
/*	For description of method see "Synthetic English	*/
/*	Speech by Rule", M. D. McIlroy, Bell Telephone		*/
/*	Laboratories, Inc. 1974					*/
/*								*/
#define NT 800
#define NS 9500

char *diags[] {
	0,
	"bad option",
	"no vocabulary",
	"can't create",
	"too many words",
	"too many chars" };

char *aeiou "aeiou";
char *aeiouy "aeiouy";
char *aeiouwxy "aeiouwxy";
char *aeo "aeo";
char *aou "aou";
char *bcdfgkpt "bcdfgkpt";

struct rec {
	int word,phon;
};
int recsize 4;
struct rec table[NT];
char strings[NS];
int ttop 1;
int stop 1;
char buf[100];
int eflag;
int tflag 0;
int code[] {
	'a0',	033,	/*AH--co_ntact, ca_r*/
	'a1',	052,	/*AH1--co_nnect*/
	'a2',	067,	/*AH2--*/
	'W0',	002,	/*AW--ca_ll, law__ (,l,u2,aw)*/
	'W1',	054,	/*AW1--fau__lt*/
	'W2',	017,	/*AW2--*/
	'ae',	021,	/*AE--ca_t, sa_t*/
	'ea',	020,	/*EA1--a_ntenna*/
	'A0',	037,	/*A--na_me (,n,ai,y0,m)*/
	'A1',	071,	/*A1--na_mely*/
	'A2',	072,	/*A2--*/
	'e0',	004,	/*EH--me_t, e_nter*/
	'e1',	075,	/*EH1--*/
	'e2',	076,	/*EH2--*/
	'e3',	077,	/*EH3--*/
	'er',	005,	/*ER--weather__*/
	'E0',	023,	/*E--three__*/
	'E1',	026,	/*Y--sixty_*/
	'E2',	035,	/*Y1--y_es*/
	'y0',	003,	/*IE--ze_ro*/
	'y1',	036,	/*AY--may_*/
	'i0',	030,	/*I--si_x*/
	'i1',	064,	/*I1--i_nept, i_nside*/
	'i2',	065,	/*I2--stati_c*/
	'i3',	066,	/*I3--*/
	'o0',	031,	/*O--o_nly, no_*/
	'o1',	012,	/*O1--lostcomment*/
	'o2',	013,	/*O2--lostcomment*/
	'ou',	051,	/*OO1--lostcommen*/
	'eu',	011,	/*IU--lostcomment*/
	'oo',	050,	/*OO--lostcomment*/
	'u0',	014,	/*UH--lostcomment*/
	'u1',	015,	/*UH1--lostcommen*/
	'u2',	016,	/*UH2--lostcommen*/
	'u3',	034,	/*UH3--lostcommen*/
	'U0',	027,	/*U--lost comment*/
	'U1',	010,	/*U1--lostcomm*/
	'b',	061,
	'd',	041,
	'dt',	073,
	'f',	042,
	'g',	043,
	'h',	044,
	'k',	046,
	'l',	047,
	'm',	063,
	'n',	062,
	'p',	032,
	'r',	024,
	's',	040,
	't',	025,
	'v',	060,
	'w',	022,
	'z',	055,
	'sh',	056,
	'zh',	070,	/*ZH--pleas_ure*/
	'j',	045,	/*J--edge_ */
	'ch',	057,	/*CH--batch__*/
	'th',	006,	/*TH--th__in*/
	'dh',	007,	/*THV--th__en*/
	'ng',	053,	/*NG--long__, in_k*/
	'-0',	001,	/*PA1*/
	'-1',	074,	/*PA0--short pause*/
	0,	0
};
char work[100];
char line[100];
int vs 2;

main(argc,argv)
char **argv; 
{
	char register *t,*u;
	int i;
	int wtop;
	int f;
	int pflag,sflag,vflag,lflag;
	int xflag,yflag,wflag;
	char register *w;
	char *v;
	pflag =  eflag =sflag = vflag = 1;
	if(argc>1 && *argv[1]=='-') {
loop:	
		switch(*(argv[1]++)) {
		default: 
			goto loop;
		case 'e': 
			eflag = 0;
			goto loop;
		case 'p': 
			pflag = 0;
			goto loop;
		case 's': 
			sflag = 0;
			goto loop;
		case 'v': 
			vflag = 0;
			goto loop;
		case '\0':; 
		}
		argc--; 
		argv++; 
	}
	if(vflag) vs = argc>2?creat(argv[2],0666):open("/dev/vs",1);
	readin(argc>1 ? argv[1]:"/usr/lib/speak.m");
	for(;;)	{

		t = line;
		do	{ 
			if(!read(0,t,1)) {
				exit(); 
			}
		} 
		while(*t++!='\n');
		*(t-1)=' ';
		*t=0;
		if(line[0]=='!') switch(line[1]) {
		case 'c':
			copy();
			break;
		case 'w':
			writeout(name());
		case 'r':
			t = name();
			if(*t) readin(t);
			break;
		case 'd':
			if(phread(work,buf+1)!=buf+1) 
				decode(1,buf+1);
			else {
				tflag = 1;
				phpron(work,buf);
				tflag = 0; 
			}
			write(1,"\n",1);
			break;
		case 'p':
			decode(1,buf+1);
			write(1,"\n",1);
			break;
		case 'l':
			i = 0;
			while(++i<ttop) 
				list(1,&strings[table[i].word]);
			break;
		default:
			diag(1);
			break;
		}
		else	{
			if(!replace()&&line[1]!='\0') {
				t = line; 
				u = work; 
				lflag = 0; 
				for(;;) {
					while(*t==' '||*t=='\t') t++;
					while((*u= *t++)!=' '&&*u!='\t') {
						if(*u) {
							if('a'<=*u&&'z'>=*u
							    ||*u=='%')
								lflag++;
							u++; 
						}
						else goto next; 
					}
					*u++ = 0; 
				}
next:
				wtop = u; 
			}
			t = work;
			while(t<wtop){
				u = phread(t,buf+1);
				wflag=yflag=0;
				if(u==buf+1 && pflag) {
					for(v=t;oneof(*v,"([`\"");v++);
					for(w=v;*w;w++)
						if(*w=='-'&&w-v>=2){
							wflag = *w;
							*w = 0;
							break;
						}
					if(!wflag){
						for(;oneof(*--w,".,;:?!'\"])"););
						yflag = *++w;
					}
					if(w<=v) goto noword;
					*w = 0;
					xflag = 0;
					if(!lflag) for(u=v;*u;u++)
						if(fold(u)) xflag++;
					if((yflag||xflag||wflag)&&
					    (u=phread(v,buf+1))!=buf+1);
					else u = phpron(v,buf+1);
					if((*w=yflag)&&u!=buf+1) {
						/*pause for punct*/
						*u++ = 001;
						*u++ = 001; 
					}
				}
noword:
				if(u==buf+1&&wflag)
					*w = wflag;
				if(u==buf+1&&sflag)
					u = phspell(t,buf+1);
				*buf = 0174;	/*phoneme ,2-1*/
				*u++ = 0174;	/* temp */
				*u++ = 0;
				if(vflag) write(vs,buf,u-buf);
				while(*t++); 
			}
		}
	}
}

decode(f,s)
char *s; 
{
	int b,c;
	int register flag;
	flag = 1;
	while(c = *s++) {
		if(flag) {
			write(f,",",1);
			b ='3'- ((c&0377)>>6);
			if(c==001) {
				flag = 0;
				c = '%'; 
			}
			else {
				if(b!='2') write(1,&b,1);
				c = c&077;
				c = dencode(&code[1],&code[0],c); 
			}
		}
		while(c){
			write(f,&c,1);
			c=>>8; 
		}
	}
}

dencode(t1,t2,c)
struct {
	int x,y;
} 
t1[], t2[];
{
	int register i,d;
	for(i=0;d=t1[i].x;i++) if(c==d) break;
	return(t2[i].x); 
}

replace(){
	char register *t,*u;
	int register n;
	int b,i;
	t = line;
	u = buf;
	if(*t++!=',')return(0);
	for(;;) { 
		if(*t=='%') {
			*u++ = 001;
			while((*u = *++t) && *u!=' ') u++;
			break; 
		}
		b = 1;
		if(*t<='3') if(*t>='0')
			b = '3'-*t++;
		n = 0;
		while(*t!=',' && *t!=' ' && *t!=0) {
			i = *t++;
			if(n) i=<<8;
			n =| i; 
		}
		n = dencode(&code[0],&code[1],n);
		if(n) *u++ = n+ (b<<6);
		if(*t!=',' && *t!=' ') break;
		t++; 
	}
	*u++=0;
	phwrite(work,buf);
	return(1); 
}

list(f,s)
char *s; 
{
	char register *t;
	if(phread(s,buf)==buf) return;
	write(f," ",1);
	t = s;
	while(*t) write(f,t++,1);
	write(f,"\n",1);
	decode(f,buf); 
	write(f,"\n",1); 
}

copy(){
	char buf1[100];
	phread(work,buf1);
	phwrite(name(),buf1); 
}

name(){
	char register *u,*t;
	u = &line[2];
	while(*u==' ') u++;
	t = buf;
	while(*u && (*t = *u++)!=' ') t++;
	*t = 0;
	return(buf); 
}

readin(file){
	int register f;
	if((f = open(file,0))<0) {
		diag(2);
		return; 
	}
	read(f,&ttop,2);
	read(f,table,recsize*ttop);
	read(f,&stop,2);
	read(f,strings,stop);
	close(f); 
}

writo1(f,u,n)
int *u; 
{
	int register i,j,k;
	i = j = *u;
	*u = n;
	k = 1;
	while(strings[i++]) k++;
	write(f,&strings[j],k);
	return(k); 
}

writeout(file) {
	int register f,i;
	int n;
	if((f=creat(file,0666))<0) {
		diag(3);
		return; 
	}
	seek(f,recsize*ttop+4,0); /*get to byte 0 of string store*/
	write(f,strings,1);	/*and put it out*/
	n = 1;
	for(i=1;i<ttop;i++) {
		n =+ writo1(f,&table[i].word,n);
		n =+ writo1(f,&table[i].phon,n); 
	}
	seek(f,0,0);
	write(f,&ttop,2);
	write(f,table,recsize*ttop);
	write(f,&n,2);	/*new value of stop */
	close(f); 
}


find(in)
char *in; 
{
	int register bot,top,i;
	int z;
	bot = 0;
	top = ttop;
	z = 0;
	while((i=(bot+top)/2)>bot) {
		z = compare(in,&strings[table[i].word]);
		if(z==0) break;
		if(z<0) top = i;
		else bot = i; 
	}
	return(i); 
}

prefix(in)
char *in; 
{
	char register *u,*s;
	char *end;
	int register i;
	int pref,bot,top;
	pref = bot = 0;
	top = ttop;
	end = in+1;	/* +1 saves wasted time looking up % */
loop:	
	while((i=(bot+top)/2)>bot) {
		do {
			s = &strings[table[i].word];
			for(u=in;;u++) {
				if(*u<*s) top = i;
				else if(*u== *s++) {
					if(*u==0) return(i);
					if(*s!=0) continue;
					pref = bot = i;
					end = u+1; 
				}
				else if(u<=end) bot = i;
				else break;
				goto loop; 
			}
		} 
		while((i=(bot+i)/2)>bot);
		bot++;
		end++; 
	}
	return(pref); 
}

compare(a,b)
char *a,*b; 
{
	while(*a == *b) {
		if(*a==0) return(0);
		a++;
		b++; 
	}
	return(*a<*b ? -1 : 1); 
}

char *
phread(in,out)	/* returns address of letter after output string */
char *in,*out; 
{
	char *s;
	int i;
	i = find(in);
	if(compare(in,&strings[table[i].word])==0) {
		for(s = &strings[table[i].phon];*out = *s++;)
			out++; 
	}
	*out = 0; 
	return(out); 
}

phwrite(in,out)
char *in, *out; 
{
	int register i,j,z;
	i = find(in);
	if(0!=(z=compare(in,&strings[table[i].word]))) {
		if(*out==0) return;
		i++;
		if(ttop>=NT) {
			diag(4);
			return; 
		}
		for(j=ttop;j>i;j--) {
			table[j].word = table[j-1].word;
			table[j].phon = table[j-1].phon; 
		}
		table[i].word = stop;
		stash(in);
		ttop++; 
	}
	else if(*out==0) {
		for(j=i;j<ttop;j++) {
			table[j].word = table[j+1].word;
			table[j].phon = table[j+1].phon; 
		}
		ttop--;
		return; 
	}
	table[i].phon = stop;
	stash(out); 
}

stash(s)
char *s; 
{
	while(stop<NS)
		if((strings[stop++]= *s++)==0)
			return; 
	diag(5); 
}

diag(n) {
	char register *p;
	p = diags[n];
	while(*p) write(1,p++,1); 
	write(1,"\n",1); 
}

phspell(in,out)
char *in, *out; 
{
	char register *c,*t;
	c = "* \0";
	while(c[1] = *in++) {
		fold(&c[1]);
		t = phread(c,out);
		if(t!=out) out = t;
		else {
			*out++ = 0346;	/* ,0k */
			*out++ = 0367;	/* ,0a2 */
		}
		if(*in) *out++ = 0101; 
	}	/*phoneme 2-0*/
	*out = 0;
	return(out); 
}


/*danger--reuses "line" to conserve space*/
char *
phpron(in,out)
char *in, *out; 
{
	char register *t,*u;
	char *s;
	char register *sout;
	char sflag;
	int i;
	sout = out;
	*sout = 0;
	s = t = line+2; 
	u = in;
	while(*s++ = *u++);
	s =- 2;
	sflag = 0;
	if(fold(t)) if(sout!=(out=phread(t,out)))
		return(out);
	if(s==t||vowel(t,s+1)<t) {
		/*spell one-letter words and vowelless words*/
		goto done; 
	}
	if(eflag) {	/* handle english endings*/
		if(sflag = finals(t,&s)) 
			if(sout!=(out=phread(t,out))) {
				*out++ = sflag>1?0140:0155;
				*out++ = 0140; /*,s,s or ,z,s*/
				goto done; 
			}
		midu(t,s);
		finale(t,&s);
		mide(t,&s);
		mids(t,s);
		if(sflag) *++s = 's'; 
	}
	*--t = '#'; 
	*++s = '#'; 
	*++s = 0;
	while(*t) {
		*--t = '%';
		i = prefix(t);
		if(i==0) {
			*sout = 0; 
			return(sout); 
		}
		u = &strings[table[i].word];
		while(*u) {
			t++;
			if(tflag) write(1,u,1);
			u++; 
		}
		if(tflag) write(1," ",1);
		s = &strings[table[i].phon];
		while(*out = *s++)
			if(*out!=001) out++;
			else	{	/*do replacement*/
				u = s;
				while(*u) u++;
				while(--u>=s) *--t = *u; 
				break; 
			}
	}
done:
	*out = 0;
	return(out); 
}

char
finals(in,ls)
char *in,**ls; 
{
	char register *end;
	int *val;
	end = *ls;
	val = 0;
	if(*end=='s'&&!oneof(end[-1],"us")) {
		*end-- = 0;
		if(*end=='\'') *end-- = 0;
		val = oneof(*end,"cfkpt")+1; 
	}
	if(*end=='e'&&end[-1]=='i') {
		*end-- = 0;
		*end = 'y'; 
	}
	*ls = end;
	return(val); 
}

midu(in,end)
char *in,*end;
{
	char register *s,*t; 
	for(s=in;s<end-1;s++) if(*(t=s)=='u' && !oneof(s[-1],aeiou)) {
		if(oneof(s[1],aeiouwxy)) continue;
		if(s[2]=='r'&&oneof(s[1],bcdfgkpt)) s++;
		if(oneof(s[2]|040,aeiouy)) *t = 'U'; 
	}
	for(s=in;s<end-2;s++) if(oneof(*(t=s),aeo)) {
		if(oneof(s[1],"aeiouwxy|"))continue;
		if(th(s+1)) s++;
		if(s[2]=='r'&&s[3]=='i'&&oneof(s[1],bcdfgkpt)) s++;
		if(oneof(s[2],"ie")&&oneof(s[3]|040,aou)
		    || s[2]=='i'&&s[3]=='e'&&s[4]=='n')
			*t =^ 040; 
	}
	s = in;
	if(*s=='y') s++;
	while(!oneof(*s|040,aeiouy)&&s<end) s++;
	if(oneof(*s,"iy") && oneof(s[1],aou)) *s =^ 040; 
}

char *suff0[] {
	"la",
	"el",
	"er",
	"su",
	"y",
	0 };
char *suff1[] {
	"elba",
	"de",
	"re",
	"gni",
	"tse",
	"ne",
	"ylba",
	"yl",
	"ro",
	"yre",
	"ye",
	"ssel",
	"ssen",
	"luf",
	"tnem",
	0 };
char *suff2[] {
	"ci",
	"laci",
	0 };
char *suff3[] {
	"e",
	0 };

finale(in,ls)
char *in,**ls; 
{
	char register *t,*end,*u;
	char *s;
	char *z;
	end = *ls;
	if((*end=='e')&&vowel(in,end)<in) {
		*end = 'E';	/* monosyllable in -e */
		return; 
	}
	t = suffix(in,end,suff0);
	if(t<end) t = longe(in,t);
	if(t==end||t<in||vowel(in,t)>=in||*t=='h') {
		t = end;
		while((u=suffix(in,t,suff1))!=t) {
			insert(u+1,ls);
			t = u; 
		}
		if((u=suffix(in,t,suff2))!=t) {
			insert(u+1,ls);
			return; 
		}
		if((u=suffix(in,t,suff3))!=t) {
			if(u[2]=='e') return;
			insert(u+1,ls);
			t = u; 
		}
		if(oneof(*t,"iuy")&&vowel(in,t)<in) {
			*t =^040;	/*monosyllables in -y, */
			return; 
		}	/*perhaps suffixed*/
		if(!oneof(t[t[1]=='|'?2:1],"aeio")) return;
		t = longe(in,t);
		if(t<in || oneof(t[1],"cg")&&vowel(in,t)>=in) return;
		if(th(t+1)) {
			t[1] = 'T'; 
			t[2] = 'H'; 
		} 
	}
	if((t==in||!oneof(t[-1],aeo)) && !(*t=='e'&&t[1]=='l'))
		*t =^ 040; 
}

mide(in,ls)
char *in,**ls; 
{
	char register *u,*end;
	end = *ls;
	for(u=in+3;u<end-2;u++)
		if(*u=='e') {
			if(u>in+4
			    &&syltest(u+1,"aeiouy|",end)
			    &&u[-1]=='l'
			    &&oneof(u[-2],"bdfgkpt")
			    &&oneof(u[-3],"bcdfgmnprst"))
				goto shift;
			if(syltest(u+1,"aeinoruy|",end)
			    &&!oneof(u[-1],"aehiouwxy")
			    &&oneof(u[-2],"aiouyU")
			    &&!oneof(u[-3],"aeiu")) {
				if(u[-3]!='o') u[-2] =& ~040; 
			}
			else return;
shift:		
			insert(u+1,ls); 
		} 
}

mids(in,end)
char *in,*end; 
{
	while(++in<end) 
		if(*in=='s'&&oneof(in[-1]|040,"aeiouy")
		    &&oneof(in[1]|040,"aeimouy"))
			*in =^ 040; 
}

syltest(in,s,end)
char *in,*end,*s; 
{
	if(!oneof(*in|040,s))
		while(++in<end) {
			if(*in=='e'&&in[1]=='|') break;
			if(*in=='|') break;
			if(oneof(*in|040,aeiouy)) return(1); 
		}
	return(0); 
}

char *
insert(in,ls)
char *in,**ls; 
{
	char register *s,*end;
	end = *ls;
	if(*in=='e') if(*++in=='|') return;
	for(s=++end;s>=in;s--) s[1] = *s;
	*in = '|'; 
	*ls = end; 
}

char *
suffix(in,end,s)
char *in,*end,**s; 
{
	char register *t,*u;
	while(u = *s) {
		t = end+1;
		while(*u == *--t) u++;
		if(*u==0) {
			if(vowel(in,t+1)<in) break;
			else return(t); 
		}
		s++; 
	}
	return(end); 
}

char *
longe(in,end)
char *in, *end; 
{
	if(th(end-1)) end--;
	return(!oneof(*end|040,aeiouwxy)&&oneof(*--end,aeiouy)?end:in-1); 
}

oneof(c,l)
char c,*l;
{ 
	while(*l) if(c == *l++) return(1);
	return(0); 
}

char *
vowel(in,end)
char *in,*end; 
{
	while(--end>=in)
		if(oneof(*end|040,aeiouy)) break;
	return(end); 
}

fold(s)
char *s;
{
	if('A'<=*s && *s <='Z') {
		*s =^ 040;
		return(1); 
	}
	return(0); 
}

th(s)
char *s; 
{
	return(*s=='t'&s[1]=='h'); 
}
