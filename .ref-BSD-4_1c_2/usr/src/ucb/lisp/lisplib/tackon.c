#include <stdio.h>
#include <sys/types.h>
#include <pagsiz.h>
#include <a.out.h>

FILE *map;
int aout;
#define NEWSIZ 100000
char newstrb[NEWSIZ];

main(argc, argv)
int argc;
char *argv[];
{
	char sym[50], svalue[50];
	char *strb,*newstr,*malloc();
	char *curstr;
	int value;
	int cnt;
	int strsiz;
	int strcnt;
	int size;
	struct nlist a;
	struct exec e;

	argc--, argv++;
	if (argc == 0 || argc > 2) {
usage:
		fprintf(stderr, "usage: tackon map [ a.out ]\n");
		exit(1);
	}
	map = fopen(argv[0], "r");
	if (map == NULL) {
		perror(argv[0]);
		exit(1);
	}
	aout = open(argc == 2 ? argv[1] : "a.out", 2);
	if (aout < 0) {
		printf(" No object file to tackon or text busy\n");
		exit(1);
	}
	read(aout,&e, sizeof(e));
	/* read current string table into buffer */
	lseek(aout, N_STROFF(e), 0);	/* seek to string table beginning */
	read(aout,&strsiz,4);		/* read in string table size	  */
	strb = malloc(strsiz);
	read(aout,strb,strsiz);		/* read in string table */
	lseek(aout, N_STROFF(e), 0);	/* now write at end of symbols	  */
	cnt = 0;
	strcnt = 4 + strsiz;
	curstr = newstrb;		/* point to new string buffer */
	for (;;) {
		if (fgets(sym, 50, map) == NULL)
			break;
		sym[size=strlen(sym)-1] = 0;
		if (fgets(svalue, 50, map) == NULL) {
			fprintf(stderr, "missing value\n");
			break;
		}
		strcpy(curstr,sym);
		sscanf(svalue, "%x", &a.n_value);
		a.n_un.n_strx = strcnt;
		a.n_type = N_EXT|N_TEXT;
		write(aout, &a, sizeof (a));
		curstr += size+1;
		strcnt += size+1;
		cnt++;
		if( curstr >= &newstrb[NEWSIZ])
		{
			printf(" Tackon; string buffer overflow \n");
			exit(1);
		}
	}
	write(aout, &strcnt, 4);	/* new character count */
	write(aout, strb, strsiz);	/* write out old string table */
	write(aout, newstrb, strcnt - ( 4 + strsiz));
	lseek(aout, 0, 0);
	e.a_syms += cnt*sizeof(struct nlist);
	lseek(aout, 0, 0);
	write(aout, &e, sizeof (e));
	exit(0);
}
