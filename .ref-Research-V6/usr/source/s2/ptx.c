/* permuted title index */

char	*tfil "/tmp/p.tmp";
char	*sw[] {
	"a",
	"an",
	"and",
	"as",
	"for",
	"is",
	"of",
	"on",
	"or",
	"the",
	"to",
	"up",
	0};
char	line[200];
int	ch;
int	ptflg;
int	llen	72;

main(argc, argv)
int argc;
char *argv[];
{
	extern fin, fout;
	extern onintr();
	int f;

	if(argc>1 && *argv[1]=='-') {
		llen = 100;
		ptflg++;
		argc--;
		argv++;
	}
	if(argc<2) {
		printf("arg count\n");
		exit();
	}
	fin = open(argv[1]);
	if(fin < 0) {
		printf("%s: cannot open\n", argv[1]);
		exit();
	}
	f = creat(tfil, 0600);
	if(f < 0) {
		printf("cannot create %s\n", tfil);
		exit();
	}
	fout = f;
	if ((signal(2, 1) & 01) ==0)
		signal(2, onintr);
	pass1();
	flush();
	close(fin);
	fin = 0;
	close(fout);
	fout = 1;
	f = fork();
	if(f < 0) {
		printf("try again\n");
		exit();
	}
	if(f == 0) {
		execl("/bin/sort", "sort", "-d", "-o", tfil, tfil, 0);
		execl("/usr/bin/sort", "sort", "-d", "-o", tfil, tfil, 0);
		printf("someone moved sort\n");
		exit();
	}
	while(wait() != f);
	fin = open(tfil, 0);
	if(fin < 0) {
		printf("cannot reopen %s\n", tfil);
		exit();
	}
	if (argc>=3)
		f = creat(argv[2], 0666);
	else
		f = dup(1);
	if(f < 0) {
		printf("%s: cannot open\n", argv[2]);
		exit();
	}
	fout = f;
	pass2();
	flush();
	onintr();
}

onintr()
{
	unlink(tfil);
	exit();
}

pass1()
{
	int n, c, i, ll, j, k, cc, ccc;

loop:
	if ((c=getchar())=='\0')
		return;
	n = 0;
	i = 0;
	while(c!='\n' && c!='\0') {
		if(c == '(')
			c = 0177;
		if(c==' ' || c=='\t') {
			i++;
			c = getchar();
			continue;
		}
		if(i) {
			i = 0;
			if(n<=llen) line[n++] = ' ';
		}
		if (n<=llen) line[n++] = c;
		c = getchar();
	}
	ll = n;
	line[n++] = 0;
	i = -1;
l1:
	while((cc=line[++i])==' ');
	n = i;
	j = 0;
	while(sw[j]) {
		i = n;
		k = 0;
		while ((cc=sw[j][k++])==line[i++]);
		if(cc==0 && ((ccc=line[--i])==' '||ccc==0))
			goto l1;
		j++;
	}
	i = n;
	while (c=line[n++]) putchar(c);
	putchar('~');
	n = 0;
	while (n<i) {
		c = line[n++];
		if (c!=' ' || n!=i)
			putchar(c);
	}
	putchar('\n');
	while((c=line[i++])!=0 && c!=' ');
	--i;
	if (c) goto l1;
	goto loop;
}

pass2()
{
	int i, n, c, tilde, llen2, nbfore, nafter;


	llen2 = llen/2+6;
loop:
	if ((c=getchar())=='\0')
		return;
	n = nbfore = nafter = 0;
	tilde = -1;
	while(c!='\n' && c!='\0') {
		if(c == 0177)
			c = '(';
		if (n<=llen) line[n] = c;
		if (c=='~') tilde = n;
		if (tilde>=0) nafter++; else nbfore++;
		n++;
		c = getchar();
	}
	if (tilde<0)
		tilde = n++;
	nafter--;
	if (nbfore>llen2) {
		i = tilde;
		while (nbfore > llen2)
			while(line[--i]!=' ' && i>=0) nbfore--;
		if (i<0) goto l1;
		line[tilde] = 0200;
		nafter =+ (tilde-i+2);
		tilde = i;
	}
	if (nafter >= llen-llen2) {
		i = tilde;
		while(nafter-- >= llen-llen2)
			while(line[++i]!=' ' && i<n) nafter--;
		if (i>=n) goto l1;
		line[tilde] = 0200;
		nafter++;
		tilde = i;
	}
l1:
	if(!ptflg) {
		for(i=llen-llen2-nafter; i>=8; i =- 8)
			putchar('\t');
		while(--i>=0)
			putchar(' ');
	} else
		printf(".xx \"");
	i = tilde;
	while (++i<n) p1char(line[i]);
	if(!ptflg)
		printf("  "); else
		printf("\" \"");
	i = -1;
	while(++i<tilde) p1char(line[i]);
	if(ptflg)
		putchar('"');
	putchar('\n');
	goto loop;
}

p1char(c)
{
	if ((c&0377) == 0200) {
		putchar('.');
		putchar('.');
		c = '.';
	}
	putchar(c);
}
