char w2006[24];
flg 0;
char realwd[26];
char *wd {&realwd[1]};
/*
int etext;
int rathole[1000];
*/
int	neng;
int	npr;
int table[2];
int tab1[26];
int tab2[730];
char tab3[19684];
int logtab[256];
float inctab[256];
char nwd[26];
mask 0377;
int tot;
int wtot;
char *buf[3];
file[3];
ptr[3];
char *name[4];
bsp[768];
main(argc,argv) int argc; char *argv[]; {
char let,lt;
auto arg,num,t,sw,i,j,k,l,m,salt,er,c;
int unl();
int junk;
/*
monitor(&main, &etext, rathole, 1000);
*/

if(--argc < 1){printf("arg count\n");
		exit(); }

buf[0] = bsp;
buf[1] = bsp + 0400;
buf[2] = bsp + 01000;
ptr[0] = 0; ptr[1] = 0;
ptr[2] = 1;
arg = 1;
while(argv[arg][0] == '-') {

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
	if(--argc < 1) {
		printf("arg count\n");
		exit();
	}
}
if(!neng) {
	salt = open("/usr/lib/salt",0);
	er = read(salt,table,4);
		if(er != 4)err("read salt");
	er = read(salt,tab1,52);
		if(er != 52) err("read salt");
	er = read(salt,tab2,1460);
		if(er != 1460) err("read salt");
	er = read(salt,tab3,19684);
		if(er != 19684)err("read salt");
	close(salt);
	}


signal(2,unl);
name[0] = "/tmp/ttmpa1";
name[1] = "/tmp/ttmpa2";
name[2] = "/tmp/ttmpa3";
name[3] = "/tmp/ttmpa4";
topen:
file[0] = open(name[0],1);
	if(file[0] > 0){
		close(file[0]);
		j = -1;
		while(++j < 4)name[j][9]++;
		if(name[0][9] == 'z')err("creat tmp file");
		goto topen; }
j = 2;
while(j--)file[j] = creat(name[j],0666);

while(argc--){
file[2] = open(argv[arg++],0);
	if(file[2] < 0)err("open input file");

while((j = wdval(2)) != 0){

put(dfile(nwd[0]),nwd,j+1);
k = -1;
l = 0;
m = 1;
table[0] = incr(table[0]);
while(m <= j+1){
	c = 27*wd[k++] + wd[l++];
	tab2[c] = incr(tab2[c]);
	c = 27*c + wd[m++];
	tab3[c] = incr(tab3[c] & 0377);
		}
	c = 27*wd[k] + wd[l];
	tab2[c] = incr(tab2[c]);

	}

done1: close(file[2]); }

/*
junk = creat("junk\0",0666);
write(junk,table,4+52+1460+19684);
close(junk);
*/

j = 2;
while(j--){flsh(j,0);
	close(file[j]); }
j = 2;
while(j--){
sw = fork();
	if(sw == 0){execl("/bin/sort","sort","-o",name[j],name[j],0);
		err("sort"); }
	if(sw < 0)err("fork");

er = wait();
	if(er != sw)err("probs");
	}

j = -1;
while(++j < 2){
sw = fork();
	if(sw == 0){execl("/bin/uniq","uniq",name[j],name[j+2]);
		err("uniq"); }
	if(sw < 0)err("fork");
er = wait();
	if(er != sw)err("prob");
	}

file[0] = creat(name[0],0666);
	if(file[0] < 0)err("creat tmp");

file[1] = open("/usr/lib/w2006",0);
	if(file[1] < 0)err("open w2006");

ptr[1] = 1;
j = -1;
while((w2006[++j] = get(1)) != '\n');

lt = 1;
while(++lt < 4){
file[2] = open(name[lt],0);
	if(file[2] < 0)err("open tmp");
ptr[2] = 1;
getw:
	k = -1;
	while((wd[++k] = get(2)) != '\n')if(wd[k] == 0)goto done;

scan:
	i = -1;
	l = 0;
	while(++i <= k){
		if(wd[i] < w2006[l]){
			put(0,wd,k);
			goto getw; }
		if(wd[i] > w2006[l]){
			l = -1;
			while((w2006[++l] = get(1)) != '\n')
				if(w2006[l] == 0)goto fin;
			goto scan; }
		l++; }
goto getw;

fin:
	put(0,wd,k);
	k = -1;
	while((wd[++k] = get(2)) != 0){
		if(wd[k] == '\n'){
		put(0,wd,k);
		k = -1; }}
done:
close(file[2]); 
unlink(name[lt]);
}
flsh(0,0);
close(file[1]);
close(file[0]);
ptr[1] = 1;

file[1] = open(name[0],0);
	if(file[1] < 0)err("open tmp ");
file[0] = creat(name[1],0666);
	if(file[0] < 0)err("create tmp");

while((j = wdval(1)) != 0){
	wtot = 0;
	flg = 0;
	k = -1; l = 0; m = 1;
	while(m <= j+1){
		tot = 0;
		c = wd[k++]*27 + wd[l++];
		digram(c);
		digram(wd[k]*27 + wd[l]);
		tot =>> 1;
		c = c*27 + wd[m++];
		trigram(c);
		if(tot > wtot) wtot = tot;
		}
	if(wtot < 0) wtot = 0;
	t = conf(wtot,3,wd);
	wd[3] = ' ';
	put(0,wd,3);
	put(0,nwd,j+1); }
flsh(0,0);
close(file[1]);
close(file[0]);
/*
monitor(0);
goto unl;
*/
sw = fork();
	if(sw == 0){execl("/bin/sort","sort","-r","-o",name[1],name[1]
		,0);
		err("sort"); }
	if(sw < 0)err("fork");
er = wait();
	if(er != sw)err("prob");

sw = fork();
	if(sw == 0){
		if(npr) {
			execl("/bin/cat","cat",name[1],0);
		} else {
			execl("/bin/pr","pr","-3", "-h",
			"Possible typo's and spelling errors",name[1],0);
			err("pr");
		}
	}
	if(sw < 0)err("fork");
er = wait();
	if(er != sw)err("prob");
	unl();
}
unl() {
int j;
j = 2;
while(j--)unlink(name[j]);
exit();
}

trigram(c) int c; {
int t;
t = logtab[tab3[c]&0377];
tot =- t;
return;
}

digram(c) int c; {
int t;
t = logtab[tab2[c]];
tot =+ t;
return;
}
dfile(c) char c; {
if(c >= 'a' && c <= 'k')return(0);
return(1);
}

err(c) char c[];{
auto j;
printf("cannot %s\n",c);
j = 4;
while(j--)unlink(name[j]);
exit();
}
get(ifile) int ifile;{

    static char *ibuf[10];

  if(--ptr[ifile]){
    return(*ibuf[ifile]++);}

  if(ptr[ifile] = read(file[ifile],buf[ifile],512)){
      if(ptr[ifile] < 0)goto prob;

    ibuf[ifile] = buf[ifile];
    return(*ibuf[ifile]++);
    }

  ptr[ifile] = 1;
  return(0);

prob:
  ptr[ifile] = 1;
  printf("read error\n");
  return(0);

}
put(ofile,s,optr) char s[]; {auto i;


while(optr-- >= 0)

    buf[ofile][(ptr[ofile] < 512)?ptr[ofile]++:flsh(ofile,1)] = *s++;
return;
}

flsh(ofile,i){auto error;

error = write(file[ofile],buf[ofile],ptr[ofile]);
if(error < 0)goto prob;

ptr[ofile] = i;

return(0);

prob:
  printf("write error on t.%d\n",file);
exit();
}
wdval(wfile) int wfile; {
static let,wflg;
auto j;
beg:
j = -1;
if(wflg == 1){wflg = 0;
		goto st; }
	while((let = get(wfile)) != '\n'){
		if(let == 0)return(0);
	st:
		if(let == '%' && j != -1)goto cont;
		if(let == '-'){
			if((let = get(wfile)) == '\n'){
				while((let = get(wfile)) == '\n');
				goto st; }
			wflg = 1;
			goto ret; }
		if(let == 047){if(j < 1)goto ret;
			goto cont; }
		if(let < 'A')goto ret;
		if(let <= 'Z'){ wd[++j] = let - 0100;
			nwd[j] = let + ' ';
			goto cont; }
		if(let < 'a' || let > 'z')goto ret;
		wd[++j] = let - 0140;
		nwd[j] = let;
	cont: ;	}

while(((let = get(wfile)) == '.') || (let == 047)){
	while((let = get(wfile)) != '\n');
	}
wflg = 1;
ret:
if(j < 1)goto beg;
nwd[j+1] = '\n';
wd[j+1] = 0;
return(j);
}
conf(n,width,cbuf) char cbuf[]; {
auto i,a;

i = width;
while(i--)cbuf[i] = ' ';

cbuf[(a = n/10)?conf(a,--width,cbuf):--width] = n%10 + '0';

return(++width);
}
incr(gork) int gork; {
float log(), exp(), pow();
int static first;
int ii;

if(first == 0){
	inctab[0] = 1;
	logtab[0] = -10;
	for(ii=1; ii<256; ii++){
		inctab[ii] = exp(-ii/30.497);
		logtab[ii] = log(30.*pow(1.0333,ii+0.) - 30.) + .5;
		}
	logtab[1] = -10;
	first = 1;
	}

if(inctab[gork] > rand()/32768.) gork++;
if(gork > 255) gork = 255;
return(gork);
}
