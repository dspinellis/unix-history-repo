static char *sccsid = "@(#)symorder.c	4.2 (Berkeley) 10/2/80";
/*
 * symorder - reorder symbol table
 */
#include <stdio.h>
#include <pagsiz.h>
#include <sys/types.h>
#include <stat.h>
#include <a.out.h>

#define SPACE 100

struct	nlist order[SPACE];

char	*savestr();
struct	nlist nl1, nl2;
struct	exec exec;
FILE	*strf;
off_t	sa, ss;
struct	stat stb;
int	nsym = 0;
int	symfound = 0;
char	asym[BUFSIZ];

main(argc, argv)
	char **argv;
{
	register struct nlist *p, *q;
	register FILE *f;
	register int na, i, j;
	int maxlen;
	int n, o;

	if(argc != 3) {
		fprintf(stderr, "Usage: symorder orderlist file\n");
		exit(1);
	}
	if((f = fopen(argv[1], "r")) == NULL) {
		perror(argv[1]);
		exit(1);
	}
	maxlen = 0;
	for(p = order; fgets(asym, sizeof asym, f) != NULL; p++, nsym++) {
		for(i = 0; asym[i] && asym[i] != '\n'; i++)
			continue;
		if (asym[i] == '\n')
			asym[i] = 0;
		p->n_un.n_name = savestr(asym);
		if (maxlen < strlen(p->n_un.n_name))
			maxlen = strlen(p->n_un.n_name);
	}
	fclose(f);
	if((f = fopen(argv[2], "r")) == NULL)
		perror(argv[2]), exit(1);
	if((strf = fopen(argv[2], "r")) == NULL)
		perror(argv[2]), exit(1);
	if((o = open(argv[2], 1)) < 0)
		perror(argv[2]), exit(1);
	if((fread(&exec, sizeof exec, 1, f)) != 1 || N_BADMAG(exec)) {
		fprintf(stderr, "symorder: %s: bad format\n", argv[2]);
		exit(1);
	}
	if (exec.a_syms == 0) {
		fprintf(stderr, "symorder: %s is stripped\n", argv[2]);
		exit(1);
	}
	fstat(fileno(f), &stb);
	if (stb.st_size < N_STROFF(exec)+sizeof(off_t)) {
		fprintf(stderr, "symorder: %s is in old format or truncated\n", argv[2]);
		exit(1);
	}
	sa = N_SYMOFF(exec);
	na = sa;
	ss = sa + exec.a_syms;
	fseek(f, sa, 0);
	n = exec.a_syms;
	while(n && symfound < nsym) {
		if(fread(&nl1, sizeof nl1, 1, f) != 1) {
			fprintf(stderr, "Short file "); perror(argv[2]);
			exit(1);
		}
		na += sizeof nl1;
		n -= sizeof nl1;
		if (nl1.n_un.n_strx == 0 || nl1.n_type & N_STAB)
			continue;
		fseek(strf, ss+nl1.n_un.n_strx, 0);
		fread(asym, maxlen+1, 1, strf);
		for(j = 0; j < nsym; j++) {
			for(i = 0; asym[i]; i++)
				if(asym[i] != order[j].n_un.n_name[i])
					goto cont;
			if (order[j].n_un.n_name[i])
				goto cont;
			if (order[j].n_value)
				goto cont;
			order[j].n_value = 1;
			fseek(f, (i = (sa+(j * sizeof nl1))), 0);
			if(fread(&nl2, sizeof nl2, 1, f) != 1)
				printf("Read err on 2nd asym\n");
			lseek(o, i, 0);
			if(write(o, &nl1, sizeof nl1) == -1)
				perror("write1");
			lseek(o, na-sizeof nl1, 0);
			if(write(o, &nl2, sizeof nl2) == -1)
				perror("write2");
			fseek(f, 0, 0);
			fseek(f, na, 0);
			symfound++;
			break;
	cont:           ;

		}
	}
	if(symfound < nsym) {
		fprintf(stderr, "%d symbol(s) not found:\n", nsym - symfound);
		for (i = 0; i < nsym; i++) {
			if (order[i].n_value == 0)
				printf("%s\n", order[i].n_un.n_name);
		}
	}
}

#define	NSAVETAB	4096
char	*savetab;
int	saveleft;

char *
savestr(cp)
	register char *cp;
{
	register int len;

	len = strlen(cp) + 1;
	if (len > saveleft) {
		saveleft = NSAVETAB;
		if (len > saveleft)
			saveleft = len;
		savetab = (char *)malloc(saveleft);
		if (savetab == 0) {
			fprintf(stderr,
			    "symorder: ran out of memory (savestr)\n");
			exit(1);
		}
	}
	strncpy(savetab, cp, len);
	cp = savetab;
	savetab += len;
	saveleft -= len;
	return (cp);
}
