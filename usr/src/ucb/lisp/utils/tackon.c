#include <stdio.h>
#include "lconf.h"
#include "config.h"
#if ! os_unisoft
#include <sys/types.h>
#include <a.out.h>
/*
 * $Header: /na/franz/utils/RCS/tackon.c,v 1.4 83/08/22 19:01:17 sklower Exp $
 *
 * $Locker:  $
 *
 * This program tacks on extra symbols into the symbol table.
 * someone should write one for system 5.
 *
 */

FILE *map;
int aout;
#define NEWSIZ 100000
char newstrb[NEWSIZ];

#endif
main(argc, argv)
int argc;
char *argv[];
{
#if ! os_unisoft
	char sym[50], svalue[50];
	char *strb,*newstr,*malloc();
	char *curstr;
	int value;
	int cnt;
	int strsiz;
	int strcnt;
	int size;
	int header_location;
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
	if ((aout < 0) && (argc == 2)) {
		char Name[256];

		strcpy(Name,argv[1]);
		strcat(Name,".exe");
		aout = open(Name,2);
	}
	if (aout < 0) {
		printf(" No object file to tackon or text busy\n");
		exit(1);
	}
	header_location = 0;
	read(aout,&e, sizeof(e));
	if (N_BADMAG(e)) {
		header_location = 512;
		lseek(aout,512,0);
		read(aout,&e,sizeof(e));
		if (N_BADMAG(e)) {
			printf("tackon: bad magic number\n");
			exit(0);
		}
	}
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
	lseek(aout, header_location, 0);
	e.a_syms += cnt*sizeof(struct nlist);
	lseek(aout, header_location, 0);
	write(aout, &e, sizeof (e));
	exit(0);
#endif
}
