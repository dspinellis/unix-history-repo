#include <stdio.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/vm.h>
#include <sys/proc.h>
#include <sys/pte.h>
#include <sys/cmap.h>
#include <sys/inode.h>
#include <sys/buf.h>
#include <sys/text.h>
#include <sys/ino.h>

struct	buf *bread();
struct	buf b;
char	bc[BSIZE];
int	bflag;

main(argc, argv)
	char *argv[];
{
	struct dinode di;
	struct inode i;
	struct proc p;
	struct user u;
	struct text x;
	struct pte *pte;
	int j,k,l;

	if (argc > 1 && !strcmp(argv[1], "-b"))
		argc--, argv++, bflag++;
	if (argc != 3) {
		fprintf(stderr, "usage: copyout [ -b ] inum disk\n");
		exit(1);
	}
	close(0);
	if (open(argv[2], 0) != 0) {
		perror(argv[2]);
		exit(1);
	}
	lseek(0, itod(atoi(argv[1])) * BSIZE + itoo(atoi(argv[1])) * sizeof (di), 0);
	if (read(0, &di, sizeof (di)) != sizeof (di)) {
		fprintf(stderr, "error reading inode ");
		perror(argv[1]);
		exit(1);
	}
	i.i_dev = 0;
	i.i_number = atoi(argv[1]);
	i.i_flag = ILOCK;
	i.i_count = 1;
	i.i_un.i_lastr = 0;
	i.i_mode = di.di_mode;
	i.i_nlink = di.di_nlink;
	i.i_uid = di.di_uid;
	i.i_size = di.di_size;
	l3tol(i.i_un.i_addr, di.di_addr, NADDR);
	p.p_textp = &x;
	x.x_iptr = &i;
	b.b_un.b_addr = bc;
	pte = (struct pte *)calloc(btoc(i.i_size), sizeof (struct pte));
	if (pte == NULL) {
		fprintf(stderr, "Not enough core for block pointers\n");
		exit(1);
	}
	vinizfod(&p, pte, 0, (btoc(i.i_size)+(CLSIZE-1)) / CLSIZE);
	l = i.i_size;
	for (j = 0; j < (btoc(i.i_size)+(CLSIZE-1)) / CLSIZE; j++)
		if (bflag)
			printf("#%d: block %d\n", j, pte[j].pg_pfnum);
		else {
			k = imin(l, BSIZE);
			write(1, bread(0, pte[j].pg_pfnum)->b_un.b_words, k);
			brelse(&b);
			l -= BSIZE;
		}
	exit(0);
}

int	bused;

struct buf *
bread(dev, blk)
{

	if (bused)
		abort();
	bused = 1;
	printf("getblk %x\n", blk);
	lseek(0, blk * BSIZE, 0);
	if (read(0, b.b_un.b_addr, BSIZE) != BSIZE) {
		printf("block %d: ", blk);
		perror("bread");
	}
	return (&b);
}

brelse(bp)
struct buf *bp;
{

	if (bp != &b)
		abort();
	bused = 0;
}

struct	buf *vbmap();
/*
 * Initialize the page tables for paging from an inode,
 * by scouring up the indirect blocks in order.
 */
vinizfod(p, pte, bstart, count)
	struct proc *p;
	register struct pte *pte;
	daddr_t bstart;
	int count;
{
	register struct inode *ip = p->p_textp->x_iptr;
	register int i;
	struct buf *bp;
	int indx;
	register daddr_t *pp;

	while (count > 0) {
		if (bstart < NADDR - 3) {
			pte++->pg_pfnum = ip->i_un.i_addr[bstart];
			bstart++;
			count--;
		} else {
			bp = vbmap(ip, bstart);
			indx = (bstart - (NADDR - 3)) % NINDIR;
			i = imin(NINDIR - indx, count);
			bstart += i;
			count -= i;
			if (bp) {
				pp = &bp->b_un.b_daddr[indx];
				do
					pte++->pg_pfnum = *pp++;
				while (--i > 0);
				brelse(bp);
			} else
				pte += i;
		}
	}
}

/*
 * Vbmap returns a block full of indirect pointers for a given block offset
 * in a file.  It returns 0 if a missing address block was encountered,
 * in which case the pages can be normal zfod pages.
 */
struct buf *
vbmap(ip, bn)
register struct inode *ip;
daddr_t bn;
{
	register i;
	struct buf *bp;
	int j, sh;
	daddr_t nb;
	dev_t dev = ip->i_dev;

	if (bn < NADDR-3)
		panic("vbmap");

	/*
	 * addresses NADDR-3, NADDR-2, and NADDR-1
	 * have single, double, triple indirect blocks.
	 * the first step is to determine
	 * how many levels of indirection.
	 */
	sh = 0;
	nb = 1;
	bn -= NADDR-3;
	for (j = 3; j > 0; j--) {
		sh += NSHIFT;
		nb <<= NSHIFT;
		if(bn < nb)
			break;
		bn -= nb;
	}
	if (j == 0)
		goto noblk;

	/*
	 * fetch the address from the inode
	 */
	nb = ip->i_un.i_addr[NADDR-j];

	/*
	 * fetch through the indirect blocks
	 */
	for (;;) {
		if (nb == 0)
			return ((daddr_t)0);
		bp = bread(dev, nb);
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			goto noblk;
		}
		if (j == 3)
			break;
		j++;
		sh -= NSHIFT;
		i = (bn>>sh) & NMASK;
		nb = bp->b_un.b_daddr[i];
		brelse(bp);
		if (nb == 0)
			goto noblk;
	}
	return (bp);

noblk:
	return ((struct buf *)0);
}

imin(a,b)
{
	return (a<b?a:b);
}

panic(cp)
{
	printf(cp);
	abort();
}

char vmmap[1];
char version[1];
