#define NULL 0
#define EOS 0
#define	HSHSIZ	101
struct	nlist {
	char	*name;
	char	*def;
	int	ydef;
	struct	nlist *next;
};

struct	nlist	*hshtab[HSHSIZ];
struct nlist	*lookup();
char	*install();
char	*malloc();
char	*copy();
int	hshval;

struct nlist *lookup(str)
char *str;
{
	register char *s1, *s2;
	register struct nlist *np;
	static struct nlist nodef;

	s1 = str;
	for (hshval = 0; *s1; )
		hshval += *s1++;
	hshval %= HSHSIZ;
	for (np = hshtab[hshval]; np!=NULL; np = np->next) {
		s1 = str;
		s2 = np->name;
		while (*s1++ == *s2)
			if (*s2++ == EOS)
				return(np);
	}
	return(&nodef);
}

char *install(nam, val, tran)
char *nam, *val;
int tran;
{
	register struct nlist *np;

	if ((np = lookup(nam))->name == NULL) {
		np = (struct nlist *)malloc(sizeof(*np));
		np->name = copy(nam);
		np->def = copy(val);
		np->ydef = tran;
		np->next = hshtab[hshval];
		hshtab[hshval] = np;
		return(np->def);
	}
	free(np->def);
	np->def = copy(val);
	return(np->def);
}

char *copy(s)
register char *s;
{
	register char *p, *s1;

	p = s1 = (char *) malloc((unsigned)strlen(s)+1);
	while (*s1++ = *s++);
	return(p);
}
