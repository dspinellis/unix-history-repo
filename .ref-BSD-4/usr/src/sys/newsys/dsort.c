
#define	NULL	0
#define	B_READ	1

struct	buf {
	struct	buf *b_actf, *b_actl;
	int	b_cylin;
	struct	buf *av_forw;
	int	b_flags;
} dtab;

main(argc, argv)
	char **argv;
{
	int i; char *cp;

	argc--, argv++;
	while (argc > 0) {
		cp = *argv++;
		i = *cp++ == 'r';
		doit(atoi(cp), i);
		printem();
		--argc;
	}
}

struct	buf fum[100];
int	next;

doit(cyl, rw)
{
	struct buf *bp = &fum[next++];

	bp->b_cylin = cyl;
	bp->b_flags = rw;
	disksort(&dtab, bp);
}

printem()
{
	register struct buf *bp = dtab.b_actf;
	while (bp != &dtab && bp) {
		printf("w\0r" + 2 * bp->b_flags);
		printf("%d ", bp->b_cylin);
		bp = bp->av_forw;
	}
	printf("\n");
}

disksort(dp, bp)
register struct buf *dp, *bp;
{
	register struct buf *ap;
	struct buf *tp;

	ap = dp->b_actf;
	if(ap == NULL) {
		dp->b_actf = bp;
		dp->b_actl = bp;
		bp->av_forw = NULL;
		return;
	}
	tp = NULL;
	if (ap->b_cylin > bp->b_cylin) {
		while (ap->av_forw && ap->av_forw->b_cylin > ap->b_cylin)
			ap = ap->av_forw;
	}
	while (ap->av_forw && ap->av_forw->b_cylin <= bp->b_cylin)
		ap = ap->av_forw;
	bp->av_forw = ap->av_forw;
	ap->av_forw = bp;
	if(ap == dp->b_actl)
		dp->b_actl = bp;
}
