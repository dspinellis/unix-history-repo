#include	<ar.h>
#include	<a.out.h>
#include	<stdio.h>
#define	MAGIC	exp.a_magic
#define	BADMAG	MAGIC!=A_MAGIC1 && MAGIC!=A_MAGIC2  \
		&& MAGIC!=A_MAGIC3 && MAGIC!=A_MAGIC4
struct	ar_hdr	arp;
struct	exec	exp;
FILE	*fi, *fo;
long	off, oldoff;
long	ftell();
#define TABSZ	700
struct tab
{	char cname[8];
	long cloc;
} tab[TABSZ];
int tnum;
int new;
char	tempnm[] = "__.SYMDEF";
char	firstname[17];
long	offdelta;

main(argc, argv)
char **argv;
{
	char buf[256];

	--argc;
	while(argc--) {
		fi = fopen(*++argv,"r");
		if (fi == NULL) {
			fprintf(stderr, "nm: cannot open %s\n", *argv);
			continue;
		}
		off = sizeof(exp.a_magic);
		fread((char *)&exp, 1, sizeof(MAGIC), fi);	/* get magic no. */
		if (MAGIC != ARMAG)
		{	fprintf(stderr, "not archive: %s\n", *argv);
			continue;
		}
		fseek(fi, 0L, 0);
		new = tnum = 0;
		if(nextel(fi) == 0)
		{	fclose(fi);
			continue;
		}
		do {
			long o;
			register n;
			struct nlist sym;

			fread((char *)&exp, 1, sizeof(struct exec), fi);
			if (BADMAG)		/* archive element not in  */
				continue;	/* proper format - skip it */
			o = (long)exp.a_text + exp.a_data;
			if ((exp.a_flag & 01) == 0)
				o *= 2;
			fseek(fi, o, 1);
			n = exp.a_syms / sizeof(struct nlist);
			if (n == 0) {
				fprintf(stderr, "nm: %s-- no name list\n", arp.ar_name);
				continue;
			}
			while (--n >= 0) {
				fread((char *)&sym, 1, sizeof(sym), fi);
				if ((sym.n_type&N_EXT)==0)
					continue;
				switch (sym.n_type&N_TYPE) {

				case N_UNDF:
					continue;

				default:
					stash(&sym);
					continue;
				}
			}
		} while(nextel(fi));
		new = fixsize();
		fclose(fi);
		fo = fopen(tempnm, "w");
		if(fo == NULL)
		{	fprintf(stderr, "can't create temporary\n");
			exit(1);
		}
		fwrite((char *)tab, tnum, sizeof(struct tab), fo);
		fclose(fo);
		if(new)
			sprintf(buf, "ar rlb %s %s %s\n", firstname, *argv, tempnm);
		else	sprintf(buf, "ar rl %s %s\n", *argv, tempnm);
		if(system(buf))
			fprintf(stderr, "can't execute %s\n", buf);
		else fixdate(*argv);
		unlink(tempnm);
	}
	exit(0);
}

nextel(af)
FILE *af;
{
	register r;

	oldoff = off;
	fseek(af, off, 0);
	r = fread((char *)&arp, 1, sizeof(struct ar_hdr), af);  /* read archive header */
	if (r <= 0)
		return(0);
	if (arp.ar_size & 1)
		++arp.ar_size;
	off = ftell(af) + arp.ar_size;	/* offset to next element */
	return(1);
}

stash(s) struct nlist *s;
{	int i;
	if(tnum >= TABSZ)
	{	fprintf(stderr, "symbol table overflow\n");
		exit(1);
	}
	for(i=0; i<8; i++)
		tab[tnum].cname[i] = s->n_name[i];
	tab[tnum].cloc = oldoff;
	tnum++;
}

fixsize()
{	int i;
	offdelta = tnum * sizeof(struct tab) + sizeof(arp);
	off = sizeof(MAGIC);
	nextel(fi);
	if(strncmp(arp.ar_name, tempnm, 14) == 0)
	{	new = 0;
		offdelta -= sizeof(arp) + arp.ar_size;
	}
	else
	{	new = 1;
		strncpy(firstname, arp.ar_name, 14);
	}
	for(i=0; i<tnum; i++)
		tab[i].cloc += offdelta;
	return(new);
}

/* patch time */
fixdate(s) char *s;
{	long timex, time();
	int fd;
	fd = open(s, 1);
	if(fd < 0)
	{	fprintf(stderr, "can't reopen %s\n", s);
		return;
	}
	timex = time(NULL) + 5; /* should be enough time */
	lseek(fd, (long)sizeof(exp.a_magic) + ((char *)&arp.ar_date-(char *)&arp), 0);
	write(fd, (char *)&timex, sizeof(timex));
	close(fd);
}
