/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

#define	ESIZE	128

#define	NBRA	9

#define	STAR	1

#define	CBRA	1
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11
#define	CKET	12
#define	CBRC	14
#define	CLET	15

#define	savere(a)	copy(a, &re, sizeof re)
#define	resre(a)	copy(&re, a, sizeof re)

struct savre {
	char	sexpbuf[ESIZE + 4];
	char	snbra;
	char	scircfl;
	char	*sbraslist[NBRA];
	char	*sbraelist[NBRA];
} re, scanre, subre;

char	rhsbuf[LBSIZE / 2];

char	*loc1, *loc2, *locs;
char	*linebp;

#define	circfl		re.scircfl
#define	expbuf		re.sexpbuf
#define	nbra		re.snbra
#define	braslist	re.sbraslist
#define	braelist	re.sbraelist
