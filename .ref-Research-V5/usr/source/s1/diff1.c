/*	diff - differential file comparison
*
*	Uses an algorithm due to Harold Stone, which finds
*	a pair of longest identical subsequences in the two
*	files.
*
*	The major goal is to generate the match vector J.
*	J[i] is the index of the line in file1 corresponding
*	to line i file0. J[i] = 0 if there is no
*	such line in file1.
*
*	Lines are hashed so as to work in core. All potential
*	matches are located by sorting the lines of each file
*	on the hash (called value_____). In particular, this
*	collects the equivalence classes in file1 together.
*	Subroutine equiv____  replaces the value of each line in
*	file0 by the index of the first element of its 
*	matching equivalence in (the reordered) file1.
*	To save space equiv_____ squeezes file1 into a single
*	array member______ in which the equivalence classes
*	are simply concatenated, except that their first
*	members are flagged by changing sign.
*
*	Next the indices that point into member______ are unsorted_______   into
*	array class_____ according to the original order of file0.
*
*	The cleverness lies in routine stone______. This marches
*	through the lines of file0, developing a vector klist_____
*	of "k-candidates". At step i a k-candidate is a matched
*	pair of lines x,y (x in file0 y in file1) such that
*	there is a common subsequence of lenght k
*	between the first i lines of file0 and the first y 
*	lines of file1, but there is no such subsequence for
*	any smaller y. x is the earliest possible mate to y
*	that occurs in such a subsequence.
*
*	Whenever any of the members of the equivalence class of
*	lines in file1 matable to a line in file0 has serial number 
*	less than the y of some k-candidate, that k-candidate 
*	with the smallest such y is replaced. The new 
*	k-candidate is chained (via pred____) to the current
*	k-1 candidate so that the actual subsequence can
*	be recovered. When a member has serial number greater
*	that the y of all k-candidates, the klist is extended.
*	At the end, the longest subsequence is pulled out
*	and placed in the array J by unravel_______.
*
*	With J in hand, the matches there recorded are
*	check_____ed against reality to assure that no spurious
*	matches have crept in due to hashing. If they have,
*	they are broken, and "jackpot " is recorded--a harmless
*	matter except that a true match for a spuriously
*	mated line may now be unnecessarily reported as a change.
*
*	Much of the complexity of the program comes simply
*	from trying to minimize core utilization and
*	maximize the range of doable problems by dynamically
*	allocating what is needed and reusing what is not.
*	The core requirements for problems larger than somewhat
*	are (in words) 2*length(file0) + length(file1) +
*	3*(number of k-candidates installed),  typically about
*	6n words for files of length n. There is also space for buf1
*	used which could, by moving data underfoot and reallocating
*	buf1 together with buf2, be completely overlaid.
*/
struct buf {
	int fdes;
	char data[516];
} *buf1, *buf2;

struct cand {
	int x;
	int y;
	struct cand *pred;
} cand;
struct line {
	int serial;
	int value;
} *file[2], line;
int len[2];
int *class;	/*will be overlaid on file[0]*/
int *member;	/*will be overlaid on file[1]*/
struct cand **klist;	/*will be overlaid on file[0] after class*/
int *J;		/*will be overlaid on class*/
int *ixold;	/*will be overlaid on klist*/
int *ixnew;	/*will be overlaid on file[1]*/

char *area;
char *top;
alloc(n)
{
	register char *p;
	p = area;
	n = (n+1) & ~1;
	area =+ n;
	while(area > top) {
		if(sbrk(1024) == -1) {
			mesg("Out of space\n");
			exit(1);
		}
		top =+ 1024;
	}
	return(p);
}

mesg(s)
char *s;
{
	while(*s)
		write(2,s++,1);
}

sort(a,n)	/*shellsort CACM #201*/
struct line *a;
{
	struct line w;
	register int j,m;
	struct line *ai;
	register struct line *aim;
	int k;
	for(j=1;j<=n;j=* 2)
		m = 2*j - 1;
	for(m=/2;m!=0;m=/2) {
		k = n-m;
		for(j=1;j<=k;j++) {
			for(ai = &a[j]; ai > a; ai =- m) {
				aim = &ai[m];
				if(aim->value > ai[0].value ||
				   aim->value == ai[0].value &&
				   aim->serial > ai[0].serial)
					break;
				w.value = ai[0].value;
				ai[0].value = aim->value;
				aim->value = w.value;
				w.serial = ai[0].serial;
				ai[0].serial = aim->serial;
				aim->serial = w.serial;
			}
		}
	}
}

unsort(f, l, b)
struct line *f;
int *b;
{
	int *a;
	int i;
	a = alloc((l+1)*sizeof(a[0]));
	for(i=1;i<=l;i++)
		a[f[i].serial] = f[i].value;
	for(i=1;i<=l;i++)
		b[i] = a[i];
	area = a;
}

prepare(i, arg)
char *arg;
{
	register char *temp;
	temp = file[i] = area;
	alloc(sizeof(line));
	input(arg);
	len[i] = (area - temp)/sizeof(line) - 1;
	alloc(sizeof(line));
	sort(file[i], len[i]);
}

input(arg)
{
	register int h, i;
	register struct line *p;
	if(fopen(arg,buf1) == -1) {
		mesg("Cannot open ");
		mesg(arg);
		mesg("\n");
		exit(1);
	}
	for(i=0; h=readhash(buf1);) {
		p = alloc(sizeof(line));
		p->serial = ++i;
		p->value = h;
	}
	close(buf1->fdes);
}

equiv(a,n,b,m,c)
struct line *a, *b;
int *c;
{
	register int i, j;
	i = j = 1;
	while(i<=n && j<=m) {
		if(a[i].value <b[j].value)
			a[i++].value = 0;
		else if(a[i].value == b[j].value)
			a[i++].value = j;
		else
			j++;
	}
	while(i <= n)
		a[i++].value = 0;
	b[m+1].value = 0;
	j = 0;
	while(++j <= m) {
		c[j] = -b[j].serial;
		while(b[j+1].value == b[j].value) {
			j++;
			c[j] = b[j].serial;
		}
	}
	c[j] = -1;
}

main(argc, argv)
char **argv;
{
	int k;

	if(argc>1 && *argv[1]=='-') {
		argc--;
		argv++;
	}
	if(argc!=3) {
		mesg("Arg count\n");
		exit(1);
	}

	area = top = sbrk(0);
	buf1 = alloc(sizeof(*buf1));
	prepare(0, argv[1]);
	prepare(1, argv[2]);

	member = file[1];
	equiv(file[0], len[0], file[1], len[1], member);

	class = file[0];
	unsort(file[0], len[0], class);

	klist = &class[len[0]+2];
	area = &member[len[1]+2];
	k = stone(class, len[0], member, klist);
	J = class;
	unravel(klist[k]);

	ixold = klist;
	ixnew = file[1];
	area = &ixnew[len[1]+2];
	buf2 = alloc(sizeof(*buf2));
	if(check(argv))
		mesg("Jackpot\n");
	output(argv);
}

stone(a,n,b,c)
int *a;
int *b;
struct cand **c;
{
	register int i, k,y;
	int j, l;
	int skip;
	k = 0;
	c[0] = 0;
	for(i=1; i<=n; i++) {
		j = a[i];
		if(j==0)
			continue;
		skip = 0;
		do {
			y = b[j];
			if(y<0) y = -y;
			if(skip)
				continue;
			l = search(c, k, y);
			if(l > k) {
				c[k+1] = newcand(i,y,c[k]);
				skip = 1;
				k++;
			}
			else if(c[l]->y > y && c[l]->x < i)
				c[l] = newcand(i,y,c[l-1]);
		} while(b[++j] > 0);
	}
	return(k);
}

struct cand *
newcand(x,y,pred)
struct cand *pred;
{
	struct cand *p;
	p = alloc(sizeof(cand));
	p->x = x;
	p->y = y;
	p->pred = pred;
	return(p);
}

search(c, k, y)
struct cand **c;
{
	register int i, j, l;
	int t;
	i = 0;
	j = k+1;
	while((l=(i+j)/2) > i) {
		t = c[l]->y;
		if(t > y)
			j = l;
		else if(t < y)
			i = l;
		else
			return(l);
	}
	return(l+1);
}

unravel(p)
struct cand *p;
{
	int i;
	for(i=0; i<=len[0]; i++)
		J[i] = 0;
	while(p) {
		J[p->x] = p->y;
		p = p->pred;
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
	for(i=1;i<=len[0];i++) {
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
	for(;j<=len[1];j++) {
		while(getc(buf2)!='\n') ctnew++;
		ixnew[j] = ++ctnew;
	}
	close(buf1->fdes);
	close(buf2->fdes);
	return(jackpot);
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
	m = len[0];
	J[0] = 0;
	J[m+1] = len[1]+1;
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
	if(m==0)
		change(1,0,1,len[1],dir);
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
	int nc;
	for(i=a;i<=b;i++) {
		seek(lb->fdes,f[i-1],0);
		nc = read(lb->fdes,lb->data,f[i]-f[i-1]);
		printf(pref);
		for(j=0;j<nc;j++)
			putchar(lb->data[j]);
	}
}

