char number[3];
int eflg;
char w2006[100];
flg 0;
char realwd[100];
char *wd {&realwd[1]};
char *fptr;
char *ffptr &ffbuf;
char ffbuf[36];
int	neng;
int	npr;
int table[2];	/*keep these four cards in order*/
int tab1[26];
int tab2[730];
char tab3[19684];
int logtab[256];
float inctab[256];
char nwd[100];
int tot;
int wtot;
char *buf[3];
file[3];
ptr[3];
char *name[3];
bsp[768];

main(argc,argv) int argc; char *argv[]; {
	char let,lt;
	auto arg,t,sw,i,j,salt,er,c;
	register k,l,m;
	double junk;
	int unl();
	int ii;
	double log(), exp(), pow();

	nice(-20);
	inctab[0] = 1;
	logtab[0] = -10;
	for(ii=1; ii<256; ii++){
		inctab[ii] = exp(-ii/30.497);
		logtab[ii] = log(30.*pow(1.0333,ii+0.) - 30.) + .5;
		}
	logtab[1] = -10;

	number[2] = ' ';
	buf[0] = bsp;
	buf[1] = bsp + 0400;
	buf[2] = bsp + 01000;
	ptr[0] = 0; ptr[1] = 0;
	ptr[2] = 1;
	arg = 1;
	while(argc>1 && argv[arg][0] == '-') {
		switch(argv[arg][1]) {
		default:
			printf("Unrecognizable argument: %c\n",argv[arg][1]);
				exit();
		case 0:
		case 'n':
				neng++;
				break;
		case '1':
				npr++;
		}
		arg++;
		--argc;
	}
	if(!neng) {
		salt = open("/usr/lib/salt",0);
		er = read(salt,table,21200);
			if(er != 21200)err("read salt");
		close(salt);
		}
	if((signal(2,1) & 1) != 1)
	signal(2,unl);
	name[0] = "/usr/tmp/ttmpa1";
	name[1] = "/usr/tmp/ttmpa2";
	name[2] = "/usr/tmp/ttmpa3";
	while((file[0] = open(name[0],1)) > 0){
		close(file[0]);
		for(j=0; j < 3; j++)name[j][13]++;
		if(name[0][13] == 'z')err("creat tmp file");
	}
	file[0] = creat(name[0],0666);
	fptr = argv[arg];
	if(argc == 1) {argc = 2; arg = 0;}
	while(--argc){
		if(arg == 0){
			file[2] = 0;
		}else{
			file[2] = open(argv[arg++],0);
			if(file[2] < 0)err("open input file");
		}
		eflg = 1;
		while((j = wdval(2)) != 0){
			put(0,nwd,j);
			k = -1;
			l = 0;
			m = 1;
			if(inctab[table[0]] > (junk=rand()/32768.)) table[0]++;
			while(m <= j){
				c = 27*wd[k++] + wd[l++];
				if(inctab[tab2[c]] > junk) tab2[c]++;
				c = 27*c + wd[m++];
				if(inctab[tab3[c]] > junk) tab3[c]++;
			}
			c = 27*wd[k] + wd[l];
			if(inctab[tab2[c]] > junk) tab2[c]++;
		}
		if(file[2]) close(file[2]);
	}
	flsh(0,0);
	close(file[0]);
	sw = fork();
		if(sw == 0){execl("/usr/bin/usort","usort","-o",name[2],name[0],0);
			err("sort"); }
		if(sw == -1)err("fork");
	er = wait();
		if(er != sw)err("probs");
	file[0] = creat(name[0],0666);
		if(file[0] < 0)err("creat tmp");
	file[1] = open("/usr/lib/w2006",0);
		if(file[1] < 0)err("open w2006");
	ptr[1] = 1;
	for(k=0;((c = w2006[k] = get(1)) != '\n');k++)
		if(c == -1) break;
	file[2] = open(name[2],0);
	if(file[2] < 0)err("open tmp");
	ptr[2] = 1;

	while(ptr[2]){
		l=0;
		for(k=0;((c = wd[k] = get(2)) != '\n');k++)
			if(c == -1)goto done;
		for(i=0; i<=k;i++){
			if(wd[i] < w2006[l]){
				put(0,wd,k);
				break;
			}
			if(wd[i] > w2006[l]){
				for(l=0;((c = w2006[l] = get(1)) != '\n');l++)
				if(c == -1){
					put(0,wd,k);
					for(k=0;((c = wd[k] =get(2))!= -1);k++){
						put(0,wd,k);
						k = -1;
					}
					goto done;
				}
				i = -1;
				l=0;
				continue;
			}
			l++;
		}
	}
done:
	close(file[2]); 
	unlink(name[2]);
	flsh(0,0);
	close(file[1]);
	close(file[0]);
	ptr[1] = 1;
	file[1] = open(name[0],0);
		if(file[1] < 0)err("open tmp ");
	file[0] = creat(name[1],0666);
		if(file[0] < 0)err("create tmp");
	while((j = nwdval(1)) != 0){
		wtot = 0;
		flg = 0;
		k = -1; l = 0; m = 1;
		while(m <= j){
			tot = 0;
			c = wd[k++]*27 + wd[l++];
			tot =+ (logtab[tab2[c]]+logtab[tab2[wd[k]*27+wd[l]]]);
			tot =>> 1;
			c = c*27 + wd[m++];
			tot =- logtab[tab3[c] & 0377];
			if(tot > wtot) wtot = tot;
			}
		if(wtot < 0) wtot = 0;
		t = conf(wtot,2,number);
		put(0,number,2);
		put(0,nwd,j);
		}
	flsh(0,0);
	close(file[1]);
	close(file[0]);

	sw = fork();
		if(sw == 0){execl("/bin/sort","sort","+0nr", "+1","-o",name[1],name[1]
			,0);
			err("sort"); }
		if(sw == -1)err("fork");
	er = wait();
		if(er != sw)err("prob");

	sw = fork();
		if(sw == 0){
			if(npr) {
				execl("/bin/cat","cat",name[1],0);
			} else {
				i = 0 ;
				while((c = "Possible typo's in "[i++])!=0)
					*ffptr++ = c;
				i = 0;
				while((c = fptr[i++]) != 0)
					*ffptr++ = c;
				*ffptr = 0;
				execl("/bin/pr","pr","-3", "-h",
				ffbuf,name[1],0);
				err("pr");
		}
	}
		if(sw == -1)err("fork");
	er = wait();
		if(er != sw)err("prob");
	unl();
}

unl() {
	register j;
	j = 2;
	while(j--)unlink(name[j]);
	exit();
}


err(c) char c[];{
	register j;
	printf("cannot %s\n",c);
	unl();
}

get(ifile) int ifile;{
	static char *ibuf[10];
	if(--ptr[ifile]){
		return(*ibuf[ifile]++ & 0377);}
	if(ptr[ifile] = read(file[ifile],buf[ifile],512)){
		if(ptr[ifile] < 0)goto prob;
		ibuf[ifile] = buf[ifile];
		return(*ibuf[ifile]++ & 0377);
	}
	ptr[ifile] = 1;
	return(-1);

prob:
	ptr[ifile] = 1;
	printf("read error\n");
	return(-1);
}

put(ofile,s,optr) char s[]; {
	register i;

	while(optr-- >= 0)
		 buf[ofile][(ptr[ofile] < 512)?ptr[ofile]++:flsh(ofile,1)] = *s++;
	return;
}

flsh(ofile,i){
	register error;
	error = write(file[ofile],buf[ofile],ptr[ofile]);
	if(error < 0)goto prob;

	ptr[ofile] = i;
	return(0);
prob:
	printf("write error on t.%d\n",file[ofile]);
	unl();
}

wdval(wfile) int wfile; {
	static let,wflg;
	register j;
beg:
	j = -1;
	if(wflg == 1){wflg = 0;
		goto st; }
	while((let = get(wfile)) != '\n'){
st:
		switch(let){
		case -1:	return(0);
		case '%':	if(j != -1)break;
					goto ret;
		case '-':
				if((let = get(wfile)) == '\n'){
					while((let = get(wfile)) == '\n')if(let == -1)return(0);
					goto st; }
				else {wflg = 1;
					goto ret; }
		case '\'':
				if(eflg != 1){
					if(j < 1)goto beg;
						else break;
					}
		case '.':
				if(eflg == 1){
					while((let = get(wfile)) != '\n')if(let == -1)return(0);
					goto beg; }
				else goto ret;
		default:
				eflg = 0;
				if(let < 'A')goto ret;
				if(let <= 'Z'){ wd[++j] = let - 0100;
					nwd[j] = let + ' ';
					break; }
				if(let < 'a' || let > 'z')goto ret;
				wd[++j] = let - 0140;
				nwd[j] = let;
			}
		 eflg = 0;	}

	eflg = 1;
ret:
	if(j < 1)goto beg;
	nwd[++j] = '\n';
	wd[j] = 0;
	return(j);
}

nwdval(wfile) int wfile;{
	register j;
	register char c;
	j = -1;
	do{
		if(( c = nwd[++j] = get(wfile)) == -1)return(0);
		wd[j] = c - 0140;
	}
	while(c != '\n');
	wd[j] = '\0';
	return(j);
}
conf(n,width,cbuf) char cbuf[]; {
	register i,a;

	i = width;
	while(i--)cbuf[i] = ' ';

	cbuf[(a = n/10)?conf(a,--width,cbuf):--width] = n%10 + '0';

	return(++width);
}
rand(){
	static gorp;
	gorp = (gorp + 625) & 077777;
	return(gorp);
}
