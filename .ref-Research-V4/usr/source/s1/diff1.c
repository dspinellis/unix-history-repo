#define BUFSIZ 518

int *xx;
struct buf { int fdes; char junk[BUFSIZ-2]; } *buf1, *buf2;
int *oldnew[2];
int ms[2] {0,0};	/*maximum serial seen en each file*/
int debug 0;
int *ixold;
int *ixnew;
int *a0;
int *b0;
int *L, *R;
int *J;	/*index of lines in file b matched to each in file a*/

main(argc,argv)
char **argv;
{
	int i;
	if(argc>1&&*argv[1]=='-') {
		argc--;
		argv++;
	}
	if(argc!=3) {
		write(2,"arg count\n",10);
		exit();
	}
	buf1 = sbrk(BUFSIZ);
	input(argv);
	a0 = oldnew[0];
	b0 = oldnew[1];
	i = 2*ms[1]+4;
	L = sbrk(i);
	R = buf2 = sbrk(i<BUFSIZ?BUFSIZ:i);
	J = sbrk(2*ms[0]+4);
	for(i=0;i<=ms[0];i++)
		J[i]=0;
	part(a0,ms[0],b0,ms[1]);
	ixold = xx;
	ixnew = ixold+ms[0]+1;
	if(check(argv))
		write(2,"jackpot\n",8);
	output(argv);
}

input(argv)
char **argv;
{
	int i;
	register int h,k;
	k = 0;
	xx = sbrk(516);
	for(i=0;i<=1;i++) {
		oldnew[i] = &xx[k];
		if(fopen(argv[i+1],buf1)<0) {
			write(2,"can't open input\n",17);
			exit(1);
		}
		while((h=readhash(buf1))!=-1) {
			if(++k%256==0) sbrk(512);
			xx[k] = h;
		}
		if(debug) vprint("xx",xx,xx,k,1);
		close(buf1->fdes);
		ms[i] = &xx[k]-oldnew[i];
	}
}


/* check does double duty:
1.  ferret out any fortuitous correspondences due
to counfounding by hashing (which result in "jackpot")
2.  collect random access indexes to the two files */

check(argv)
char **argv;
{
	register int i, j;
	int ctold, ctnew;
	int jackpot;
	char c,d;
	fopen(argv[1],buf1);
	fopen(argv[2],buf2);
	j = 1;
	ctold = ctnew = 0;
	ixold[0] = ixnew[0] = 0;
	jackpot = 0;
	for(i=1;i<=ms[0];i++) {
		if(J[i]==0) {
			while(getc(buf1)!='\n') ctold++;
			ixold[i] = ++ctold;
			continue;
		}
		while(j<J[i]) {
			while(getc(buf2)!='\n') ctnew++;
			ixnew[j] = ++ctnew;
			j++;
		}
		while((c=getc(buf1))==(d=getc(buf2))) {
			if(c=='\n') break;
			ctold++;
			ctnew++;
		}
		while(c!='\n') {
			jackpot++;
			J[i] = 0;
			c = getc(buf1);
			ctold++;
		}
		ixold[i] = ++ctold;
		while(d!='\n') {
			jackpot++;
			J[i] = 0;
			d = getc(buf2);
			ctnew++;
		}
		ixnew[j] = ++ctnew;
		j++;
	}
	for(;j<=ms[1];j++) {
		while(getc(buf2)!='\n') ctnew++;
		ixnew[j] = ++ctnew;
	}
	close(buf1->fdes);
	close(buf2->fdes);
	return(jackpot);
}

/*
part asks inner to find the x[m][j] vector
left-to-right and right-to-left, and then partitions the problem
to give a maximum string.
This scheme, due to hirshberg, allows one
to due the dynamic programming in linear space. */

part(a,m,b,n)
int a[],b[];
{
	int h,i1,i2,j1,j2,k,k0;
	int t,u;
	if(debug) {
		vprint("part a",a,a0,m,1);
		vprint("part b",b,b0,n,1);
	}
	while(m>0&&n>0&&a[1]==b[1]) {
		J[++a-a0] = ++b-b0;
		m--;
		n--;
	}
	while(m>0&&n>0&&a[m]==b[n])
		J[a-a0+m--] = b-b0+n--;
	if(m<=0||n<=0) return;
	L[0] = R[0] = 0;
	h = m/2;
	inner(a,h,b,n,L,1);
	inner(a+m+1,m-h,b+n+1,n,R,-1);
	t = 0;
	k0 = 0;
	for(k=0;k<=n;k++)
		if((u=L[k]+R[n-k])>t) {
			t = u;
			k0 = k;
		}
	strip(a,h,b,k0,L,1,&i1,&j1);
	strip(a+m+1,m-h,b+n+1,n-k0,R,-1,&i2,&j2);
	part(a,i1,b,j1);
	part(a+m-i2,i2,b+n-j2,j2);
}
/* inner generates the last row of the matrix x[i,j]
where x[i,j] shows the length of the longest common subsequence
between the first i elements of vector a and the first
j elements of vector b.
x[i,j] is computed by the recurrence
x[0,j] = x[i,0] = 0
x[i,j] = max {x[i-1,j], x[i,j-1], x[i-1,j-1]+c[i,j]
where c[i,j] = 0 iff a[i]=b[j] */


inner(a,m,b,n,x,d)
int a[],b[],x[];
int d;			/* +1 if a and b stored forward, -1 backward */
{
	int aid;	/* a[i*d] */
	int *lbjd;	/* &b[j*d] */
	int i;
	int *y;
	register int xi_j_;	/* x[i-1][j-1] */
	register int j,t;

	if(debug) {
		vprint("inn a",a,a0,m,d);
		vprint("inn b",b,b0,n,d);
	}
	for(j=0;j<=n;j++)
		x[j] = 0;
	y = x;
	for(i=1;i<=m;i++) {
		x = y;
		xi_j_ = t = 0;
		aid = a[i*d];
		lbjd = b;
		for(j=1;j<=n;j++) {
			if(t<*++x) t = *x;	/*t=x[i][j-1] *++x=x[i-1][j]*/
			if(aid== *(lbjd=+d) && xi_j_>=t) t++;
			xi_j_ = *x;
			*x = t;		/* x[i][j] = t */
		}
	}
}
/* strip finds the last mated pair of elements in the maximum
matching of vector a and vector b, given the appropriate
x vector computed by inner.
it strips all elements after this mated pair, */



strip(a,m,b,n,x,d,li,lj)
int a[],b[],x[];
int *li,*lj;
{
	register int i,j,*laid;
	int bjd;
	if(debug) {
		vprint("strip a",a,a0,m,d);
		vprint("strip b",b,b0,n,d);
		vprint("strip x",x,x,n,1);
	}
	for(j=n;j>0&&x[j]==x[j-1];j--);
	bjd = b[j*d];
	laid = &a[(m+1)*d];
	for(i=m;i>0&&*(laid=-d)!=bjd;i--);
	*li = i;
	*lj = j;
}

output(argv)
char **argv;
{
	int dir;
	int m;
	int i0,i1,j0,j1;
	extern fout;
	dir = **argv=='-';
	fout = dup(1);
	buf1->fdes = open(argv[1],0);
	buf2->fdes = open(argv[2],0);
	m = ms[0];
	J[0] = 0;
	J[m+1] = ms[1]+1;
	if(dir==0) for(i0=1;i0<=m;i0=i1+1) {
		while(i0<=m&&J[i0]==J[i0-1]+1) i0++;
		j0 = J[i0-1]+1;
		i1 = i0-1;
		while(i1<m&&J[i1+1]==0) i1++;
		j1 = J[i1+1]-1;
		J[i1] = j1;
		change(i0,i1,j0,j1,dir);
	} else for(i0=m;i0>=1;i0=i1-1) {
		while(i0>=1&&J[i0]==J[i0+1]-1&&J[i0]!=0) i0--;
		j0 = J[i0+1]-1;
		i1 = i0+1;
		while(i1>1&&J[i1-1]==0) i1--;
		j1 = J[i1-1]+1;
		J[i1] = j1;
		change(i1,i0,j1,j0,dir);
	}
	flush();
}

change(a,b,c,d,dir)
{
	if(a>b&&c>d) return;
	range(a,b);
	putchar(a>b?'a':c>d?'d':'c');
	if(dir==0) range(c,d);
	putchar('\n');
	if(dir==0) {
		fetch(ixold,a,b,buf1,"* ");
		if(a<=b&&c<=d) printf("---\n");
	}
	fetch(ixnew,c,d,buf2,dir==0?". ":"");
	if(dir!=0&&c<=d) printf(".\n");
}

range(a,b)
{
	if(a>b) printf("%d",b);
	if(a<=b) printf("%d",a);
	if(a<b) printf(",%d",b);
}

fetch(f,a,b,lb,pref)
int *f;
struct buf *lb;
char *pref;
{
	int i, j;
	for(i=a;i<=b;i++) {
		seek(lb->fdes,f[i-1],0);
		read(lb->fdes,lb->junk,f[i]-f[i-1]);
		printf(pref);
		for(j=0;j<f[i]-f[i-1];j++)
			putchar(lb->junk[j]);
	}
}

vprint(s,v,v0,n,d)
int *v,*v0;
char *s;
{
	int i;
	printf("%s\n",s);
	for(i=1;i<=n;i++)
		printf("%7o",v-v0+i*d);
	printf("\n");
	for(i=1;i<=n;i++)
		printf("%7o",v[i*d]);
	printf("\n");
}
