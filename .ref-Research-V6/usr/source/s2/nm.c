int	fout;
int	cflg;
int	nflg;
int	uflg;
int	rflg	1;
int	gflg;
int	pflg;
struct	nl
{
	char	name[8];
	int	typ;
	int	*val;
} *nlp;
int	fi;
int	buf[8];
main(argc, argv)
char **argv;
{
	int n, i, j;
	int compare();

	if (--argc > 0 && *argv[1] == '-') {
		argv++;
		while (*++*argv) switch (**argv) {
		case 'n':
			nflg++;
			continue;

		case 'c':
			cflg++;
			continue;

		case 'g':
			gflg++;
			continue;

		case 'u':
			uflg++;
			continue;

		case 'r':
			rflg = -1;
			continue;

		case 'p':
			pflg ++;
			continue;

		default:
			continue;
		}
		argc--;
	}
	if (argc==0)
		fi = open("a.out", 0); else
		fi = open(*++argv, 0);
	if(fi < 0) {
		printf("cannot open input\n");
		exit();
	}
	read(fi, buf, 020);
	if(buf[0]!=0407 && buf[0]!=0410 && buf[0]!=0411) {
		printf("bad format\n");
		exit();
	}
	seek(fi, buf[1], 1);		/* text */
	seek(fi, buf[2], 1);		/* data */
	if(buf[7] != 1) {
		seek(fi, buf[1], 1);
		seek(fi, buf[2], 1);	/* reloc */
	}
	n = ldiv(0, buf[4], 12);
	if(n == 0) {
		printf("no name list\n");
		exit();
	}
	nlp = sbrk(12*n);
	read(fi, nlp, n*12);
	if (pflg==0)
		qsort(nlp, n, 12, compare);
	fout = dup(1);
	close(1);
	for(i=0; i<n; i++) {
		if(gflg && (nlp->typ&040)==0)
			goto out;
		if(cflg) {
			if(nlp->name[0] != '_')
				goto out;
			for(j=0; j<7; j++)
				nlp->name[j] = nlp->name[j+1];
			nlp->name[7] = '\0';
		}
		j = nlp->typ&037;
		if(j > 4)
			j = 1;
		if(j==0 && nlp->val)
			j = 5;
		if(uflg && j!=0)
			goto out;
		if(!uflg) {
			if(j==0)
				printf("      "); else
				printo(nlp->val);
			printf("%c ", (nlp->typ&040? "UATDBC":"uatdbc")[j]);
		}
		printf("%.8s\n", nlp);
	out:
		nlp++;
	}
	flush();
}

compare(p1, p2)
struct nl *p1, *p2;
{
	int a, i;

	a = 0;
	if(nflg) {
		if(p1->val > p2->val) {
			a = 1;
			goto out;
		}
		if(p1->val < p2->val) {
			a = -1;
			goto out;
		}
	}
	for(i=0; i<8; i++)
	if(p1->name[i] != p2->name[i]) {
		if(p1->name[i] > p2->name[i])
			a = 1; else
			a = -1;
		goto out;
	}
out:
	return(a*rflg);
}

printo(v)
{
	int i;

	printf("%c", v<0?'1':'0');
	for(i=0; i<5; i++) {
		printf("%c", ((v>>12)&7)+'0');
		v =<<3;
	}
}
