int dflag 0;
int sflag 0;
int cflag 0;
int save 0;
char code[256];
char squeez[256];
char vect[256];
struct string { int last, max, rep; char *p; } string1, string2;
int inbuf[259];

main(argc,argv)
char **argv;
{
	int i, j;
	int c, d;
	char *compl;
	extern fout;

	string1.last = string2.last = 0;
	string1.max = string2.max = 0;
	string1.rep = string2.rep = 0;
	string1.p = string2.p = "";

	if(--argc>0) {
		argv++;
		if(*argv[0]=='-'&&argv[0][1]!=0) {
			while(*++argv[0])
				switch(*argv[0]) {
				case 'c':
					cflag++;
					continue;
				case 'd':
					dflag++;
					continue;
				case 's':
					sflag++;
					continue;
				}
			argc--;
			argv++;
		}
	}
	if(argc>0) string1.p = argv[0];
	if(argc>1) string2.p = argv[1];
	for(i=0; i<256; i++)
		code[i] = vect[i] = 0;
	if(cflag) {
		while(c = next(&string1))
			vect[c&0377] = 1;
		j = 0;
		for(i=1; i<256; i++)
			if(vect[i]==0) vect[j++] = i;
		vect[j] = 0;
		compl = vect;
	}
	for(i=0; i<256; i++)
		squeez[i] = 0;
	for(;;){
		if(cflag) c = *compl++;
		else c = next(&string1);
		if(c==0) break;
		d = next(&string2);
		if(d==0) d = c;
		code[c&0377] = d;
		squeez[d&0377] = 1;
	}
	while(d = next(&string2))
		squeez[d&0377] = 1;
	squeez[0] = 1;
	for(i=0;i<256;i++) {
		if(code[i]==0) code[i] = i;
		else if(dflag) code[i] = 0;
	}

	inbuf[0] = 0;
	fout = dup(1);
	close(1);
	while((c=getc(inbuf)) >=0 ) {
		if(c == 0) continue;
		if(c = code[c&0377]&0377)
			if(!sflag || c!=save || !squeez[c&0377])
				putchar(save = c);
	}
	flush();
}

next(s)
struct string *s;
{
	int a, b, c, n;
	int base;

	if(--s->rep > 0) return(s->last);
	if(s->last < s->max) return(++s->last);
	if(*s->p=='[') {
		nextc(s);
		s->last = a = nextc(s);
		s->max = 0;
		switch(nextc(s)) {
		case '-':
			b = nextc(s);
			if(b<a || *s->p++!=']')
				goto error;
			s->max = b;
			return(a);
		case '*':
			base = (*s->p=='0')?8:10;
			n = 0;
			while((c = *s->p)>='0' && c<'0'+base) {
				n = base*n + c - '0';
				s->p++;
			}
			if(*s->p++!=']') goto error;
			if(n==0) n = 1000;
			s->rep = n;
			return(a);
		default:
		error:
			write(1,"Bad string\n",11);
			exit();
		}
	}
	return(nextc(s));
}

nextc(s)
struct string *s;
{
	int c, i, n;

	c = *s->p++;
	if(c=='\\') {
		i = n = 0;
		while(i<3 && (c = *s->p)>='0' && c<='7') {
			n = n*8 + c - '0';
			i++;
			s->p++;
		}
		if(i>0) c = n;
		else c = *s->p++;
	}
	if(c==0) *--s->p = 0;
	return(c&0377);
}
