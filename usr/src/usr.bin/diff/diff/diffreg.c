static	char sccsid[] = "@(#)diffreg.c 4.9 %G%";

#include "diff.h"
/*
 * diff - compare two files.
 */

/*
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
 *	on the hash (called ``value''). In particular, this
 *	collects the equivalence classes in file1 together.
 *	Subroutine equiv replaces the value of each line in
 *	file0 by the index of the first element of its 
 *	matching equivalence in (the reordered) file1.
 *	To save space equiv squeezes file1 into a single
 *	array member in which the equivalence classes
 *	are simply concatenated, except that their first
 *	members are flagged by changing sign.
 *
 *	Next the indices that point into member are unsorted into
 *	array class according to the original order of file0.
 *
 *	The cleverness lies in routine stone. This marches
 *	through the lines of file0, developing a vector klist
 *	of "k-candidates". At step i a k-candidate is a matched
 *	pair of lines x,y (x in file0 y in file1) such that
 *	there is a common subsequence of length k
 *	between the first i lines of file0 and the first y 
 *	lines of file1, but there is no such subsequence for
 *	any smaller y. x is the earliest possible mate to y
 *	that occurs in such a subsequence.
 *
 *	Whenever any of the members of the equivalence class of
 *	lines in file1 matable to a line in file0 has serial number 
 *	less than the y of some k-candidate, that k-candidate 
 *	with the smallest such y is replaced. The new 
 *	k-candidate is chained (via pred) to the current
 *	k-1 candidate so that the actual subsequence can
 *	be recovered. When a member has serial number greater
 *	that the y of all k-candidates, the klist is extended.
 *	At the end, the longest subsequence is pulled out
 *	and placed in the array J by unravel
 *
 *	With J in hand, the matches there recorded are
 *	check'ed against reality to assure that no spurious
 *	matches have crept in due to hashing. If they have,
 *	they are broken, and "jackpot" is recorded--a harmless
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
 *	6n words for files of length n. 
 */

#define	prints(s)	fputs(s,stdout)

FILE	*input[2];
FILE	*fopen();

struct cand {
	int	x;
	int	y;
	int	pred;
} cand;
struct line {
	int	serial;
	int	value;
} *file[2], line;
int	len[2];
struct	line *sfile[2];	/* shortened by pruning common prefix and suffix */
int	slen[2];
int	pref, suff;	/* length of prefix and suffix */
int	*class;		/* will be overlaid on file[0] */
int	*member;	/* will be overlaid on file[1] */
int	*klist;		/* will be overlaid on file[0] after class */
struct	cand *clist;	/* merely a free storage pot for candidates */
int	clen = 0;
int	*J;		/* will be overlaid on class */
long	*ixold;		/* will be overlaid on klist */
long	*ixnew;		/* will be overlaid on file[1] */

diffreg()
{
	register int i, j;
	FILE *f1, *f2;
	char buf1[BUFSIZ], buf2[BUFSIZ];

	if (hflag) {
		diffargv[0] = "diffh";
		execv(diffh, diffargv);
		fprintf(stderr, "diff: ");
		perror(diffh);
		done();
	}
	if ((stb1.st_mode & S_IFMT) == S_IFDIR)
		file1 = splice(file1, file2);
	else if ((stb2.st_mode & S_IFMT) == S_IFDIR)
		file2 = splice(file2, file1);
	else if (!strcmp(file1, "-")) {
		if (!strcmp(file2, "-")) {
			fprintf(stderr, "diff: can't specify - -\n");
			done();
		}
		file1 = copytemp();
	} else if (!strcmp(file2, "-"))
		file2 = copytemp();
	if ((f1 = fopen(file1, "r")) == NULL) {
		fprintf(stderr, "diff: ");
		perror(file1);
		done();
	}
	if ((f2 = fopen(file2, "r")) == NULL) {
		fprintf(stderr, "diff: ");
		perror(file2);
		fclose(f1);
		done();
	}
	if (stb1.st_size != stb2.st_size)
		goto notsame;
	for (;;) {
		i = fread(buf1, 1, BUFSIZ, f1);
		j = fread(buf2, 1, BUFSIZ, f2);
		if (i < 0 || j < 0 || i != j)
			goto notsame;
		if (i == 0 && j == 0) {
			fclose(f1);
			fclose(f2);
			status = 0;		/* files don't differ */
			goto same;
		}
		for (j = 0; j < i; j++)
			if (buf1[j] != buf2[j])
				goto notsame;
	}
notsame:
	/*
	 *	Files certainly differ at this point; set status accordingly
	 */
	status = 1;
	if (!asciifile(f1) || !asciifile(f2)) {
		printf("Binary files %s and %s differ\n", file1, file2);
		fclose(f1);
		fclose(f2);
		done();
	}
	prepare(0, f1);
	prepare(1, f2);
	fclose(f1);
	fclose(f2);
	prune();
	sort(sfile[0],slen[0]);
	sort(sfile[1],slen[1]);

	member = (int *)file[1];
	equiv(sfile[0], slen[0], sfile[1], slen[1], member);
	member = (int *)ralloc((char *)member,(slen[1]+2)*sizeof(int));

	class = (int *)file[0];
	unsort(sfile[0], slen[0], class);
	class = (int *)ralloc((char *)class,(slen[0]+2)*sizeof(int));

	klist = (int *)talloc((slen[0]+2)*sizeof(int));
	clist = (struct cand *)talloc(sizeof(cand));
	i = stone(class, slen[0], member, klist);
	free((char *)member);
	free((char *)class);

	J = (int *)talloc((len[0]+2)*sizeof(int));
	unravel(klist[i]);
	free((char *)clist);
	free((char *)klist);

	ixold = (long *)talloc((len[0]+2)*sizeof(long));
	ixnew = (long *)talloc((len[1]+2)*sizeof(long));
	check();
	output();
	status = anychange;
same:
	if (opt == D_CONTEXT && anychange == 0)
		printf("No differences encountered\n");
	done();
}

char *
copytemp()
{
	char buf[BUFSIZ];
	register int i, f;

	signal(SIGHUP,done);
	signal(SIGINT,done);
	signal(SIGPIPE,done);
	signal(SIGTERM,done);
	tempfile = mktemp("/tmp/dXXXXX");
	f = creat(tempfile,0600);
	if (f < 0) {
		fprintf(stderr, "diff: ");
		perror(tempfile);
		done();
	}
	while ((i = read(0,buf,BUFSIZ)) > 0)
		if (write(f,buf,i) != i) {
			fprintf(stderr, "diff: ");
			perror(tempfile);
			done();
		}
	close(f);
	return (tempfile);
}

char *
splice(dir, file)
	char *dir, *file;
{
	char *tail;
	char buf[BUFSIZ];

	if (!strcmp(file, "-")) {
		fprintf(stderr, "diff: can't specify - with other arg directory\n");
		done();
	}
	tail = rindex(file, '/');
	if (tail == 0)
		tail = file;
	else
		tail++;
	sprintf(buf, "%s/%s", dir, tail);
	return (savestr(buf));
}

prepare(i, fd)
	int i;
	FILE *fd;
{
	register struct line *p;
	register j,h;

	fseek(fd, (long)0, 0);
	p = (struct line *)talloc(3*sizeof(line));
	for(j=0; h=readhash(fd);) {
		p = (struct line *)ralloc((char *)p,(++j+3)*sizeof(line));
		p[j].value = h;
	}
	len[i] = j;
	file[i] = p;
}

prune()
{
	register i,j;
	for(pref=0;pref<len[0]&&pref<len[1]&&
		file[0][pref+1].value==file[1][pref+1].value;
		pref++ ) ;
	for(suff=0;suff<len[0]-pref&&suff<len[1]-pref&&
		file[0][len[0]-suff].value==file[1][len[1]-suff].value;
		suff++) ;
	for(j=0;j<2;j++) {
		sfile[j] = file[j]+pref;
		slen[j] = len[j]-pref-suff;
		for(i=0;i<=slen[j];i++)
			sfile[j][i].serial = i;
	}
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

stone(a,n,b,c)
int *a;
int *b;
register int *c;
{
	register int i, k,y;
	int j, l;
	int oldc, tc;
	int oldl;
	k = 0;
	c[0] = newcand(0,0,0);
	for(i=1; i<=n; i++) {
		j = a[i];
		if(j==0)
			continue;
		y = -b[j];
		oldl = 0;
		oldc = c[0];
		do {
			if(y <= clist[oldc].y)
				continue;
			l = search(c, k, y);
			if(l!=oldl+1)
				oldc = c[l-1];
			if(l<=k) {
				if(clist[c[l]].y <= y)
					continue;
				tc = c[l];
				c[l] = newcand(i,y,oldc);
				oldc = tc;
				oldl = l;
			} else {
				c[l] = newcand(i,y,oldc);
				k++;
				break;
			}
		} while((y=b[++j]) > 0);
	}
	return(k);
}

newcand(x,y,pred)
{
	register struct cand *q;
	clist = (struct cand *)ralloc((char *)clist,++clen*sizeof(cand));
	q = clist + clen -1;
	q->x = x;
	q->y = y;
	q->pred = pred;
	return(clen-1);
}

search(c, k, y)
int *c;
{
	register int i, j, l;
	int t;
	if(clist[c[k]].y<y)	/*quick look for typical case*/
		return(k+1);
	i = 0;
	j = k+1;
	while (1) {
		l = i + j;
		if ((l >>= 1) <= i) 
			break;
		t = clist[c[l]].y;
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
{
	register int i;
	register struct cand *q;
	for(i=0; i<=len[0]; i++)
		J[i] =	i<=pref ? i:
			i>len[0]-suff ? i+len[1]-len[0]:
			0;
	for(q=clist+p;q->y!=0;q=clist+q->pred)
		J[q->x+pref] = q->y+pref;
}

/* check does double duty:
1.  ferret out any fortuitous correspondences due
to confounding by hashing (which result in "jackpot")
2.  collect random access indexes to the two files */

check()
{
	register int i, j;
	int jackpot;
	long ctold, ctnew;
	char c,d;

	if ((input[0] = fopen(file1,"r")) == NULL) {
		perror(file1);
		done();
	}
	if ((input[1] = fopen(file2,"r")) == NULL) {
		perror(file2);
		done();
	}
	j = 1;
	ixold[0] = ixnew[0] = 0;
	jackpot = 0;
	ctold = ctnew = 0;
	for(i=1;i<=len[0];i++) {
		if(J[i]==0) {
			ixold[i] = ctold += skipline(0);
			continue;
		}
		while(j<J[i]) {
			ixnew[j] = ctnew += skipline(1);
			j++;
		}
		for(;;) {
			c = getc(input[0]);
			d = getc(input[1]);
			ctold++;
			ctnew++;
			if(bflag && isspace(c) && isspace(d)) {
				do {
					if(c=='\n') break;
					ctold++;
				} while(isspace(c=getc(input[0])));
				do {
					if(d=='\n') break;
					ctnew++;
				} while(isspace(d=getc(input[1])));
			}
			if(c!=d) {
				jackpot++;
				J[i] = 0;
				if(c!='\n')
					ctold += skipline(0);
				if(d!='\n')
					ctnew += skipline(1);
				break;
			}
			if(c=='\n')
				break;
		}
		ixold[i] = ctold;
		ixnew[j] = ctnew;
		j++;
	}
	for(;j<=len[1];j++) {
		ixnew[j] = ctnew += skipline(1);
	}
	fclose(input[0]);
	fclose(input[1]);
/*
	if(jackpot)
		fprintf(stderr, "jackpot\n");
*/
}

sort(a,n)	/*shellsort CACM #201*/
struct line *a;
{
	struct line w;
	register int j,m;
	struct line *ai;
	register struct line *aim;
	int k;
	for(j=1;j<=n;j*= 2)
		m = 2*j - 1;
	for(m/=2;m!=0;m/=2) {
		k = n-m;
		for(j=1;j<=k;j++) {
			for(ai = &a[j]; ai > a; ai -= m) {
				aim = &ai[m];
				if(aim < ai)
					break;	/*wraparound*/
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
	register int *a;
	register int i;
	a = (int *)talloc((l+1)*sizeof(int));
	for(i=1;i<=l;i++)
		a[f[i].serial] = f[i].value;
	for(i=1;i<=l;i++)
		b[i] = a[i];
	free((char *)a);
}

skipline(f)
{
	register i;
	char c;

	for(i=1;(c=getc(input[f]))!='\n';i++)
		if (c < 0)
			return(i);
	return(i);
}

output()
{
	int m;
	register int i0, i1, j1;
	int j0;
	input[0] = fopen(file1,"r");
	input[1] = fopen(file2,"r");
	m = len[0];
	J[0] = 0;
	J[m+1] = len[1]+1;
	if(opt!=D_EDIT) for(i0=1;i0<=m;i0=i1+1) {
		while(i0<=m&&J[i0]==J[i0-1]+1) i0++;
		j0 = J[i0-1]+1;
		i1 = i0-1;
		while(i1<m&&J[i1+1]==0) i1++;
		j1 = J[i1+1]-1;
		J[i1] = j1;
		change(i0,i1,j0,j1);
	} else for(i0=m;i0>=1;i0=i1-1) {
		while(i0>=1&&J[i0]==J[i0+1]-1&&J[i0]!=0) i0--;
		j0 = J[i0+1]-1;
		i1 = i0+1;
		while(i1>1&&J[i1-1]==0) i1--;
		j1 = J[i1-1]+1;
		J[i1] = j1;
		change(i1,i0,j1,j0);
	}
	if(m==0)
		change(1,0,1,len[1]);
	if (opt==D_IFDEF) {
		for (;;) {
#define	c i0
			c = getc(input[0]);
			if (c < 0)
				return;
			putchar(c);
		}
#undef c
	}
}

/* indicate that there is a difference between lines a and b of the from file
   to get to lines c to d of the to file.
   If a is greater then b then there are no lines in the from file involved
   and this means that there were lines appended (beginning at b).
   If c is greater than d then there are lines missing from the to file.
*/
change(a,b,c,d)
{
	char ch;
	int lowa,upb,lowc,upd;
	struct stat stbuf;

	if (opt != D_IFDEF && a>b && c>d)
		return;
	if (anychange == 0) {
		anychange = 1;
		if(opt == D_CONTEXT) {
			printf("*** %s	", file1);
			stat(file1, &stbuf);
			printf("%s--- %s	",
			    ctime(&stbuf.st_mtime), file2);
			stat(file2, &stbuf);
			printf("%s", ctime(&stbuf.st_mtime));
		}
	}
	if (a <= b && c <= d)
		ch = 'c';
	else
		ch = (a <= b) ? 'd' : 'a';
	if(opt == D_CONTEXT) {
		lowa = max(1, a-context);
		upb  = min(len[0], b+context);
		lowc = max(1, c-context);
		upd  = min(len[1], d+context);

		/* print out from file */
		printf("***************\n*** ");
		range(lowa,upb,",");
		printf("\n");
		if (ch == 'a')
			fetch(ixold,lowa,upb,input[0],"  ");
		else {
			fetch(ixold,lowa,a-1,input[0],"  ");
			fetch(ixold,a,b,input[0],ch == 'c' ? "! " : "- ");
			fetch(ixold,b+1,upb,input[0],"  ");
		}
		putchar('\n');
		printf("--- ");
		range(lowc,upd,",");
		printf(" -----\n");
		if (ch == 'd')
			fetch(ixnew,lowc,upd,input[1],"  ");
		else {
			fetch(ixnew,lowc,c-1,input[1],"  ");
			fetch(ixnew,c,d,input[1],ch == 'c' ? "! " : "+ ");
			fetch(ixnew,d+1,upd,input[1],"  ");
		}
		return;
	}
	switch (opt) {

	case D_NORMAL:
	case D_EDIT:
		range(a,b,",");
		putchar(a>b?'a':c>d?'d':'c');
		if(opt==D_NORMAL)
			range(c,d,",");
		putchar('\n');
		break;
	case D_REVERSE:
		putchar(a>b?'a':c>d?'d':'c');
		range(a,b," ");
		putchar('\n');
		break;
	}
	if(opt == D_NORMAL || opt == D_IFDEF) {
		fetch(ixold,a,b,input[0],"< ", 1);
		if(a<=b&&c<=d && opt == D_NORMAL)
			prints("---\n");
	}
	fetch(ixnew,c,d,input[1],opt==D_NORMAL?"> ":"", 0);
	if ((opt ==D_EDIT || opt == D_REVERSE) && c<=d)
		prints(".\n");
	if (inifdef) {
		fprintf(stdout, "#endif %s\n", endifname);
		inifdef = 0;
	}
}

range(a,b,separator)
char *separator;
{
	printf("%d", a>b?b:a);
	if(a<b) {
		printf("%s%d", separator, b);
	}
}

fetch(f,a,b,lb,s,oldfile)
long *f;
FILE *lb;
char *s;
{
	register int i, j;
	register int nc;
	int oneflag = (*ifdef1!='\0') != (*ifdef2!='\0');

	/*
	 * When doing #ifdef's, copy down to current line
	 * if this is the first file, so that stuff makes it to output.
	 */
	if (opt == D_IFDEF && oldfile){
		long curpos = ftell(lb);
		/* print through if append (a>b), else to (nb: 0 vs 1 orig) */
		nc = f[a>b? b : a-1 ] - curpos;
		for (i = 0; i < nc; i++)
			putchar(getc(lb));
	}
	if (a > b)
		return;
	if (opt == D_IFDEF) {
		if (inifdef)
			fprintf(stdout, "#else %s%s\n", oneflag && oldfile==1 ? "!" : "", ifdef2);
		else {
			if (oneflag) {
				/* There was only one ifdef given */
				endifname = ifdef2;
				if (oldfile)
					fprintf(stdout, "#ifndef %s\n", endifname);
				else
					fprintf(stdout, "#ifdef %s\n", endifname);
			}
			else {
				endifname = oldfile ? ifdef1 : ifdef2;
				fprintf(stdout, "#ifdef %s\n", endifname);
			}
		}
		inifdef = 1+oldfile;
	}
	for(i=a;i<=b;i++) {
		fseek(lb,f[i-1],0);
		nc = f[i]-f[i-1];
		if (opt != D_IFDEF)
			prints(s);
		for(j=0;j<nc;j++)
			putchar(getc(lb));
	}
	if (inifdef && !wantelses) {
		fprintf(stdout, "#endif %s\n", endifname);
		inifdef = 0;
	}
}

#define HALFLONG 16
#define low(x)	(x&((1L<<HALFLONG)-1))
#define high(x)	(x>>HALFLONG)

/*
 * hashing has the effect of
 * arranging line in 7-bit bytes and then
 * summing 1-s complement in 16-bit hunks 
 */
readhash(f)
register FILE *f;
{
	long sum;
	register unsigned shift;
	register space;
	register t;
	sum = 1;
	space = 0;
	if(!bflag) for(shift=0;(t=getc(f))!='\n';shift+=7) {
		if(t==-1)
			return(0);
		sum += (long)t << (shift &= HALFLONG - 1);
	}
	else for(shift=0;;) {
		switch(t=getc(f)) {
		case -1:
			return(0);
		case '\t':
		case ' ':
			space++;
			continue;
		default:
			if(space) {
				shift += 7;
				space = 0;
			}
			sum += (long)t << (shift &= HALFLONG - 1);
			shift += 7;
			continue;
		case '\n':
			break;
		}
		break;
	}
	sum = low(sum) + high(sum);
	return((short)low(sum) + (short)high(sum));
}

#include <a.out.h>

asciifile(f)
	FILE *f;
{
	char buf[BUFSIZ];
	register int cnt;
	register char *cp;

	fseek(f, (long)0, 0);
	cnt = fread(buf, 1, BUFSIZ, f);
	if (cnt >= sizeof (struct exec)) {
		struct exec hdr;
		hdr = *(struct exec *)buf;
		if (!N_BADMAG(hdr))
			return (0);
	}
	cp = buf;
	while (--cnt >= 0)
		if (*cp++ & 0200)
			return (0);
	return (1);
}
