static	char sccsid[] = "@(#)nm.c 4.1 10/1/80";
/*
 * nm - print name list; VAX string table version
 */
#include <sys/types.h>
#include <ar.h>
#include <stdio.h>
#include <ctype.h>
#include <a.out.h>
#include <stab.h>
#include <pagsiz.h>
#include <stat.h>

#define	SELECT	archive ? archdr.ar_name : *xargv

int	aflg, gflg, nflg, oflg, pflg, uflg; 
int	rflg = 1;
char	**xargv;
int	archive;
struct	ar_hdr	archdr;
union {
	char	mag_armag[SARMAG+1];
	struct	exec mag_exp;
} mag_un;
#define	OARMAG	0177545
FILE	*fi;
off_t	off;
off_t	ftell();
char	*malloc();
char	*realloc();
char	*strp;
char	*stab();
off_t	strsiz;
int	compare();
int	narg;
int	errs;

main(argc, argv)
char **argv;
{

	if (--argc>0 && argv[1][0]=='-' && argv[1][1]!=0) {
		argv++;
		while (*++*argv) switch (**argv) {

		case 'n':
			nflg++;
			continue;
		case 'g':
			gflg++;
			continue;
		case 'u':
			uflg++;
			continue;
		case 'r':
			rflg = -1;
			continue;
		case 'p':
			pflg++;
			continue;
		case 'o':
			oflg++;
			continue;
		case 'a':
			aflg++;
			continue;
		default:
			fprintf(stderr, "nm: invalid argument -%c\n",
			    *argv[0]);
			exit(2);
		}
		argc--;
	}
	if (argc == 0) {
		argc = 1;
		argv[1] = "a.out";
	}
	narg = argc;
	xargv = argv;
	while (argc--) {
		++xargv;
		namelist();
	}
	exit(errs);
}

namelist()
{
	register int j;

	archive = 0;
	fi = fopen(*xargv, "r");
	if (fi == NULL) {
		error(0, "cannot open");
		return;
	}
	off = SARMAG;
	fread((char *)&mag_un, 1, sizeof(mag_un), fi);
	if (mag_un.mag_exp.a_magic == OARMAG) {
		error(0, "old archive");
		return;
	}
	if (strncmp(mag_un.mag_armag, ARMAG, SARMAG)==0)
		archive++;
	else if (N_BADMAG(mag_un.mag_exp)) {
		error(0, "bad format");
		return;
	}
	fseek(fi, 0L, 0);
	if (archive) {
		nextel(fi);
		if (narg > 1)
			printf("\n%s:\n", *xargv);
	}
	do {
		off_t o;
		register i, n, c;
		struct nlist *symp = NULL;
		struct nlist sym;
		struct stat stb;

		fread((char *)&mag_un.mag_exp, 1, sizeof(struct exec), fi);
		if (N_BADMAG(mag_un.mag_exp))
			continue;
		if (archive == 0)
			fstat(fileno(fi), &stb);
		o = N_SYMOFF(mag_un.mag_exp) - sizeof (struct exec);
		fseek(fi, o, 1);
		n = mag_un.mag_exp.a_syms / sizeof(struct nlist);
		if (n == 0) {
			error(0, "no name list");
			continue;
		}
		if (N_STROFF(mag_un.mag_exp) + sizeof (off_t) >
		    (archive ? off : stb.st_size))
			error(1, "old format .o (no string table) or truncated file");
		i = 0;
		if (strp)
			free(strp), strp = 0;
		while (--n >= 0) {
			fread((char *)&sym, 1, sizeof(sym), fi);
			if (gflg && (sym.n_type&N_EXT)==0)
				continue;
			if ((sym.n_type&N_STAB) && (!aflg||gflg||uflg))
				continue;
			if (symp==NULL)
				symp = (struct nlist *)
				    malloc(sizeof(struct nlist));
			else
				symp = (struct nlist *)
				    realloc(symp,
					(i+1)*sizeof(struct nlist));
			if (symp == NULL)
				error(1, "out of memory");
			symp[i++] = sym;
		}
		if (archive && ftell(fi)+sizeof(off_t) >= off) {
			error(0, "no string table (old format .o?)");
			continue;
		}
		if (fread((char *)&strsiz,sizeof(strsiz),1,fi) != 1) {
			error(0, "no string table (old format .o?)");
			goto out;
		}
		strp = (char *)malloc(strsiz);
		if (strp == NULL)
			error(1, "ran out of memory");
		if (fread(strp+sizeof(strsiz),strsiz-sizeof(strsiz),1,fi) != 1)
			error(1, "error reading string table");
		for (j = 0; j < i; j++)
			if (symp[j].n_un.n_strx)
				symp[j].n_un.n_name =
				    symp[j].n_un.n_strx + strp;
			else
				symp[j].n_un.n_name = "";
		if (pflg==0)
			qsort(symp, i, sizeof(struct nlist), compare);
		if ((archive || narg>1) && oflg==0)
			printf("\n%s:\n", SELECT);
		psyms(symp, i);
		if (symp)
			free((char *)symp), symp = 0;
		if (strp)
			free((char *)strp), strp = 0;
	} while(archive && nextel(fi));
out:
	fclose(fi);
}

psyms(symp, nsyms)
	register struct nlist *symp;
	int nsyms;
{
	register int n, c;

	for (n=0; n<nsyms; n++) {
		c = symp[n].n_type;
		if (c & N_STAB) {
			if (oflg) {
				if (archive)
					printf("%s:", *xargv);
				printf("%s:", SELECT);
			}
			printf("%08x - %02x %04x %5.5s %s\n",
			    symp[n].n_value,
			    symp[n].n_other & 0xff, symp[n].n_desc & 0xffff,
			    stab(symp[n].n_type & 0xff),
			    symp[n].n_un.n_name);
			continue;
		}
		switch (c&N_TYPE) {

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
		}
		if (uflg && c!='u')
			continue;
		if (oflg) {
			if (archive)
				printf("%s:", *xargv);
			printf("%s:", SELECT);
		}
		if (symp[n].n_type&N_EXT)
			c = toupper(c);
		if (!uflg) {
			if (c=='u' || c=='U')
				printf("        ");
			else
				printf(N_FORMAT, symp[n].n_value);
			printf(" %c ", c);
		}
		printf("%s\n", symp[n].n_un.n_name);
l1:		;
	}
}

compare(p1, p2)
struct nlist *p1, *p2;
{
	register i;

	if (nflg) {
		if (p1->n_value > p2->n_value)
			return(rflg);
		if (p1->n_value < p2->n_value)
			return(-rflg);
	}
	return (rflg * strcmp(p1->n_un.n_name, p2->n_un.n_name));
}

nextel(af)
FILE *af;
{
	register char *cp;
	register r;
	long arsize;

	fseek(af, off, 0);
	r = fread((char *)&archdr, 1, sizeof(struct ar_hdr), af);
	if (r != sizeof(struct ar_hdr))
		return(0);
	for (cp = archdr.ar_name; cp < &archdr.ar_name[sizeof(archdr.ar_name)]; cp++)
		if (*cp == ' ')
			*cp = '\0';
	arsize = atol(archdr.ar_size);
	if (arsize & 1)
		++arsize;
	off = ftell(af) + arsize;	/* beginning of next element */
	return(1);
}

error(n, s)
char *s;
{
	fprintf(stderr, "nm: %s:", *xargv);
	if (archive) {
		fprintf(stderr, "(%s)", archdr.ar_name);
		fprintf(stderr, ": ");
	} else
		fprintf(stderr, " ");
	fprintf(stderr, "%s\n", s);
	if (n)
		exit(2);
	errs = 1;
}

struct	stabnames {
	int	st_value;
	char	*st_name;
} stabnames[] ={
	N_GSYM, "GSYM",
	N_FNAME, "FNAME",
	N_FUN, "FUN",
	N_STSYM, "STSYM",
	N_LCSYM, "LCSYM",
	N_RSYM, "RSYM",
	N_SLINE, "SLINE",
	N_SSYM, "SSYM",
	N_SO, "SO",
	N_LSYM, "LSYM",
	N_SOL, "SOL",
	N_PSYM, "PSYM",
	N_ENTRY, "ENTRY",
	N_LBRAC, "LBRAC",
	N_RBRAC, "RBRAC",
	N_BCOMM, "BCOMM",
	N_ECOMM, "ECOMM",
	N_ECOML, "ECOML",
	N_LENG, "LENG",
	N_PC, "PC",
	0, 0
};

char *
stab(val)
{
	register struct stabnames *sp;
	static char prbuf[32];

	for (sp = stabnames; sp->st_name; sp++)
		if (sp->st_value == val)
			return (sp->st_name);
	sprintf(prbuf, "%02x", val);
	return (prbuf);
}
