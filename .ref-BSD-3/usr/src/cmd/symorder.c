/* symorder orderlist symbolfile
 *  orderlist is a file containing symbols to be found in symbolfile,
 *      1 symbol per line.
 *  symbolfile is updated in place to put the requested symbols first
 *      in the symbol table, in the order specified.  This is done
 *      by swapping the old symbols in the required spots with the
 *      new ones.  If all of the order symbols are not found, an
 *      error is generated.
 *
 *  Modelled after nlist.c the nlist subroutine, which has been
 *      modified to succeed as soon as all sought symbols are found.
 *
 *  This program was specifically designed to cut down on the read
 *      overhead of systat(ss) when getting symbols from /unix.
 */

#include <stdio.h>
#include <a.out.h>
int a_magic[] = {A_MAGIC1, A_MAGIC2, A_MAGIC3, A_MAGIC4, 0};
#define SPACE 100

main(argc, argv)
char *argv[];
{
	register struct nlist *p, *q;
	register FILE *f;
	register int sa, na, i, j;
	int nsym = 0, symfound = 0, n, o;
	struct nlist nl1, nl2;
	char buf[20];
	struct nlist order[SPACE];
	struct exec exec;

	if(argc != 3) {
		fprintf(stderr, "Usage: symorder orderlist file\n");
		exit(1);
	}
	if((f = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr, "Can't open "); perror(argv[1]);
		exit(1);
	}
	for(p = order; fgets(buf, sizeof buf, f) != NULL; p++, nsym++)
		for(i = 0; i < 8 && buf[i] != '\n'; i++)
			p->n_name[i] = buf[i];
	fclose(f);
/***    for(i = 0; i < nsym; i++)                       ***/
/***            printf("\"%.8s\"\n", order[i].n_name);  ***/
/***    printf("--------\n");                           ***/
	if((f = fopen(argv[2], "r")) == NULL) {
		fprintf(stderr, "Can't open "); perror(argv[2]);
		exit(1);
	}
	if((o = open(argv[2], 1)) < 0) {
		fprintf(stderr, "Can't update "); perror(argv[2]);
		exit(1);
	}
	if((fread(&exec, sizeof exec, 1, f)) != 1) {
		fprintf(stderr, "Can't read "); perror(argv[2]);
		exit(1);
	}
	for(i=0; a_magic[i]; i++)
		if(a_magic[i] == exec.a_magic) break;
	if(a_magic[i] == 0){
		fprintf(stderr, "Bad Header on %s\n", argv[2]);
		exit(1);
	}
	sa = exec.a_text + exec.a_data;
	sa += exec.a_trsize + exec.a_drsize;
	sa += sizeof exec;
	na = sa;
	fseek(f, sa, 0);
	n = exec.a_syms;

	while(n && symfound < nsym) {
		if(fread(&nl1, sizeof nl1, 1, f) != 1) {
			fprintf(stderr, "Short file "); perror(argv[2]);
			exit(1);
		}
/***    printf("\"%.8s\"\n", nl1.n_name);       ***/
		na += sizeof nl1;
		n -= sizeof nl1;
/***    printf("Trying ");                      ***/
		for(j = 0; j < nsym; j++) {
/***    printf("%s ", order[j].n_name);         ***/
			for(i = 0; i < 8; i++)
				if(nl1.n_name[i] != order[j].n_name[i])
					goto cont;
/***    printf("Found: %.8s\n", nl1.n_name);    ***/
			if (order[j].n_value)
				goto cont;
			order[j].n_value = 1;
			fseek(f, (i = (sa+(j * sizeof nl1))), 0);
			if(fread(&nl2, sizeof nl2, 1, f) != 1)
				printf("Read err on 2nd sym\n");
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
/***    printf("\n");                           ***/
	}
	if(symfound < nsym) {
		fprintf(stderr, "%d Syms not found:\n", nsym - symfound);
		for (i = 0; i < nsym; i++) {
			if (order[i].n_value == 0)
				printf("%.8s\n", order[i].n_name);
		}
	}
}
