/*
**	print symbol tables for
**	object or archive files
**
**	nm [-goprun] [name ...]
*/



#include	<ar.h>
#include	<stdio.h>
#include	<ctype.h>
#include	<a.out.h>
#include	<pagsiz.h>

#define	MAGIC	exp.a_magic
#define	BADMAG	MAGIC!=A_MAGIC1 && MAGIC!=A_MAGIC2  \
		&& MAGIC!=A_MAGIC3 && MAGIC!=A_MAGIC4 && MAGIC != 0412 && MAGIC != 0413
#define	SELECT	arch_flg ? arp.ar_name : *argv
int	numsort_flg;
int	undef_flg;
int	revsort_flg = 1;
int	globl_flg;
int	nosort_flg;
int	arch_flg;
int	prep_flg;
struct	ar_hdr	arp;
struct	exec	exp;
FILE	*fi;
long	off;
long	ftell();
char	*malloc();
char	*realloc();

main(argc, argv)
char **argv;
{
	int narg;
	int  compare();

	if (--argc>0 && argv[1][0]=='-' && argv[1][1]!=0) {
		argv++;
		while (*++*argv) switch (**argv) {
		case 'n':		/* sort numerically */
			numsort_flg++;
			continue;

		case 'g':		/* globl symbols only */
			globl_flg++;
			continue;

		case 'u':		/* undefined symbols only */
			undef_flg++;
			continue;

		case 'r':		/* sort in reverse order */
			revsort_flg = -1;
			continue;

		case 'p':		/* don't sort -- symbol table order */
			nosort_flg++;
			continue;

		case 'o':		/* prepend a name to each line */
			prep_flg++;
			continue;

		default:		/* oops */
			fprintf(stderr, "nm: invalid argument -%c\n", *argv[0]);
			exit(1);
		}
		argc--;
	}
	if (argc == 0) {
		argc = 1;
		argv[1] = "a.out";
	}
	narg = argc;
	while(argc--) {
		fi = fopen(*++argv,"r");
		if (fi == NULL) {
			fprintf(stderr, "nm: cannot open %s\n", *argv);
			continue;
		}
		off = sizeof(exp.a_magic);
		fread((char *)&exp, 1, sizeof(MAGIC), fi);	/* get magic no. */
		if (MAGIC == ARMAG)
			arch_flg++;
		else if (BADMAG) {
			fprintf(stderr, "nm: %s-- bad format\n", *argv);
			continue;
		}
		fseek(fi, 0L, 0);
		if (arch_flg) {
			nextel(fi);
			if (narg > 1)
				printf("\n%s:\n", *argv);
		}
		do {
			long o;
			register i, n, c;
			struct nlist *symp = NULL;
			struct nlist sym;

			fread((char *)&exp, 1, sizeof(struct exec), fi);
			if (BADMAG)		/* archive element not in  */
				continue;	/* proper format - skip it */
			o = (long)exp.a_text + exp.a_data + exp.a_trsize + exp.a_drsize;
			if (MAGIC==0412 || MAGIC==0413)
				o += PAGSIZ - sizeof(struct exec);
			fseek(fi, o, 1);
			n = exp.a_syms / sizeof(struct nlist);
			if (n == 0) {
				fprintf(stderr, "nm: %s-- no name list\n", SELECT);
				continue;
			}
			i = 0;
			while (--n >= 0) {
				fread((char *)&sym, 1, sizeof(sym), fi);
				if (globl_flg && (sym.n_type&N_EXT)==0)
					continue;
				if (symp==NULL)
					symp = (struct nlist *)malloc(sizeof(struct nlist));
				else {
					symp = (struct nlist *)realloc(symp, (i+1)*sizeof(struct nlist));
				}
				if (symp == NULL) {
					fprintf(stderr, "nm: out of memory on %s\n", *argv);
					exit(2);
				}
				symp[i++] = sym;
			}
			if (nosort_flg==0)
				qsort(symp, i, sizeof(struct nlist), compare);
			if ((arch_flg || narg>1) && prep_flg==0)
				printf("\n%s:\n", SELECT);
			for (n=0; n<i; n++) {
				if (prep_flg) {
					if (arch_flg)
						printf("%s:", *argv);
					printf("%s:", SELECT);
				}
				c = symp[n].n_type;
				
				if (c & STABTYPE) {
					printf("%08x - %-8.8s %02x %02x %04x\n",
						symp[n].n_value,
						symp[n].n_name,
						symp[n].n_type & 0xff,
						symp[n].n_other & 0xff,
						symp[n].n_desc & 0xffff);
					continue;
				}
				switch (c&(N_TYPE-N_EXT)) {

				case N_UNDF:
					c = 'u';
					if (symp[n].n_value)
						c = 'c';
					break;

				case N_ABS:
					c = 'a';
					break;

				case N_TEXT:
					c = 't';
					break;

				case N_DATA:
					c = 'd';
					break;

				case N_BSS:
					c = 'b';
					break;

				case N_FN:
					c = 'f';
					break;

/*
				case N_REG:
					c = 'r';
					break;
 */
				}
				if (undef_flg && c!='u')
					continue;
				if (symp[n].n_type&N_EXT)
					c = toupper(c);
				if (!undef_flg) {
					if (c=='u' || c=='U')
						printf("        ");
					else
						printf(FORMAT, symp[n].n_value);
					printf(" %c ", c);
				}
				printf("%.8s\n", symp[n].n_name);
		l1:;	}
			if (symp)
				free((char *)symp);
		} while(arch_flg && nextel(fi));
		fclose(fi);
	}
	exit(0);
}

compare(p1, p2)
struct nlist *p1, *p2;
{
	register i;

	if (numsort_flg) {
		if (p1->n_value > p2->n_value)
			return(revsort_flg);
		if (p1->n_value < p2->n_value)
			return(-revsort_flg);
	}
	for(i=0; i<sizeof(p1->n_name); i++)
		if (p1->n_name[i] != p2->n_name[i]) {
			if (p1->n_name[i] > p2->n_name[i])
				return(revsort_flg);
			else
				return(-revsort_flg);
		}
	return(0);
}

nextel(af)
FILE *af;
{
	register r;

	fseek(af, off, 0);
	r = fread((char *)&arp, 1, sizeof(struct ar_hdr), af);  /* read archive header */
	if (r <= 0)
		return(0);
	if (arp.ar_size & 1)
		++arp.ar_size;
	off = ftell(af) + arp.ar_size;	/* offset to next element */
	return(1);
}
