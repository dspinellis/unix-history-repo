# include	"../hdr/defines.h"
# include	"../hdr/had.h"

SCCSID(@(#)stree.c	4.3);
USXALLOC();

struct	tree {
	int	t_dsucc;		/* first successor (trunk) */
	struct	list	*t_osucc;	/* other successors */
	int	t_trunk;		/* != 0 is a trunk delta */
};

struct	list {
	struct	list	*l_next;
	int	l_val;
};

struct	position {
	int	p_depth;
	int	p_width;
	int	p_node;
};


struct	tree	*tree;
struct	position	*pos;
int	dval;

struct packet gpkt;
struct sid sid;
int	num_files;
char	had[26];

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	int testmore;
	extern prttree();
	extern int Fcnt;

	Fflags = FTLEXIT | FTLMSG | FTLCLN;
	for(i = 1; i < argc; i++)
		if(argv[i][0] == '-' && (c=argv[i][1])) {
			p = &argv[i][2];
			testmore = 0;
			switch (c) {

			case 'p':
				testmore++;
				break;

			default:
				fatal("unknown key letter (cm1)");
			}

			if (testmore) {
				testmore = 0;
				if (*p)
					fatal(sprintf(Error,
					  "value after %c arg (cm7)",c));
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");
	setsig();
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;
	for (i = 1; i < argc; i++)
		if (p=argv[i])
			do_file(p,prttree);
	exit(Fcnt ? 1 : 0);
}


prttree(file)
{
	register struct idel *rdp;
	register int n, i;
	char str[32];
	extern char had_dir, had_standinp;
	struct stats stats;
	extern poscomp();

	if (setjmp(Fjmp))
		return;
	sinit(&gpkt, file, 1);
	gpkt.p_verbose = -1;
	gpkt.p_stdout = stderr;
	if (gpkt.p_verbose && (num_files > 1 || had_dir || had_standinp))
		fprintf(gpkt.p_stdout,"\n%s:\n",gpkt.p_file);

	if (dodelt(&gpkt,&stats,0,0) == 0)
		fmterr(&gpkt);
	fclose(gpkt.p_iop);
	gpkt.p_iop = 0;

	tree = alloc(n = ((maxser(&gpkt) + 1) * sizeof(struct tree)));
	bzero(tree, n);
	pos = alloc(n = ((maxser(&gpkt) + 1) * sizeof(struct position)));
	bzero(pos, n);
	for (i = 1; i <= maxser(&gpkt); i++)
		pos[i].p_node = i;
	rdp = gpkt.p_idel;
	for (i = 1; i <= maxser(&gpkt); i++) {
		if (rdp[i].i_sid.s_br == 0)
			tree[i].t_trunk = 1;
		else
			pos[i].p_width = pos[rdp[i].i_pred].p_width + 1;
		for (n = i + 1; n <= maxser(&gpkt); n++)
			if (rdp[n].i_pred == i)
				addsucc(i, n, rdp[n].i_sid.s_br);
	}
	dval = 0;
	traverse(1);
	if (!HADP) {
		qsort(&pos[1], maxser(&gpkt), sizeof(pos[1]), poscomp);
		for (n = 1; n <= maxser(&gpkt); n++) {
			sid_ba(&rdp[pos[n].p_node].i_sid, str);
			printf("Node %d\tSid %s\tDepth %d\tWidth %d\n",
				pos[n].p_node, str, pos[n].p_depth, pos[n].p_width);
		}
	}
	else
		plot(rdp, maxser(&gpkt));
	xfreeall();
}


addsucc(par, child, childbr)
{
	struct tree *tp;

	tp = &tree[par];
	if (tp->t_trunk && tp->t_dsucc == 0 && childbr == 0)
		tp->t_dsucc = child;
	else
		addlist(&tp->t_osucc, child);
}


addlist(headp, val)
struct list *headp;
{
	struct list *prev, *p;

	for (p = headp; p = (prev = p)->l_next; )
		;
	prev->l_next = p = alloc(sizeof(struct list));
	p->l_next = 0;
	p->l_val = val;
}


traverse(node)
{
	register struct list *lp;

	pos[node].p_depth = dval;
	if (lp = tree[node].t_osucc) {
		traverse(lp->l_val);
		while (lp = lp->l_next) {
			++dval;
			traverse(lp->l_val);
		}
	}
	if (tree[node].t_dsucc) {
		++dval;
		traverse(tree[node].t_dsucc);
	}
}


poscomp(p1, p2)
register struct position *p1, *p2;
{
	register int diff;

	if (diff = p1->p_depth - p2->p_depth)
		return(diff);
	else
		return(p1->p_width - p2->p_width);
}


dmptree()
{
	register int n;
	register struct tree *tp;
	register struct list *lp;

	for (n = maxser(&gpkt); n; n--) {
		printf("Node %d", n);
		tp = &tree[n];
		if (tp->t_dsucc)
			printf("\t%d", tp->t_dsucc);
		for (lp = tp->t_osucc; lp; lp = lp->l_next)
			printf("\t%d", lp->l_val);
		printf("\n");
	}
}


plot(rdp, n)
register struct idel *rdp;
{
	char str[32];
	int i, j, x, y, node;
	struct tree *tp;
	struct list *lp;

	for (i = 1; i <= n; i++) {
		node = pos[i].p_node;
		x = pos[i].p_width;
		y = pos[i].p_depth;
		sid_ba(&rdp[node].i_sid, str);
		pllabel(str, x, y);
		tp = &tree[node];
		if (j = tp->t_dsucc)
			plline(x, y, pos[j].p_width, pos[j].p_depth);
		for (lp = tp->t_osucc; lp; lp = lp->l_next) {
			j = lp->l_val;
			plline(x, y, pos[j].p_width, pos[j].p_depth);
		}
	}
	pllabel("", 0, 15);
}


pllabel(s, x, y)
{
	x = scx(x) + 64;
	y = scy(y) + 64;
	putchar('m');
	putwd(x);
	putwd(y);
	printf("t%s\n", s);
}


plline(x0, y0, x1, y1)
{
	x0 = scx(x0);
	x1 = scx(x1);
	y0 = scy(y0);
	y1 = scy(y1);
	putchar('l');
	putwd(x0);
	putwd(y0);
	putwd(x1);
	putwd(y1);
}


putwd(w)
{
	register char *p;

	p = &w;
	putchar(*p++);
	putchar(*p);
}


scx(xi)
{
	return(xi * 1024 - 2047);
}


scy(yi)
{
	return(2047 - (yi * 256));
}


clean_up(n)
{
	xfreeall();
}


escdodelt()	/* dummy for dodelt() */
{
}
