/*	mprof.c 2.6 9/14/90 16:01:20	*/
/*	Copyright (c) 1987, 1990, Benjamin G. Zorn */

/*	mprof.c -- code to analyse and print out mprof data
 */
	

#include	<stdio.h>
#include	<sys/file.h>
#include	<ctype.h>
#include	<a.out.h>
#include	<stab.h>
#include 	"mprof.h"

#ifdef mips
#include <ldfcn.h>
#endif

#if defined(mips) || defined(vax)
char *
strdup(s)
char	*s;
{
    char	*result = malloc(strlen(s) + 1);
    (void) strcpy(result, s);
    return 	result;
}
#endif

extern	void	mprof_graph_ops();		/* from mpgraph.c */

#define check_fscanf(x)	\
    { int result = (x); \
      if (!result) { \
	 fprintf(stderr, "fscanf -- can't read input\n"); \
	 exit(1); \
       } \
    }

struct leakpair {
    char	*func;
    int		offset;
};

struct leakentry {
    struct leakpair	path[SHORT_CALLSTACK_SIZE];
    int			all_no;
    int			all_by;
    int			fre_no;
    int			fre_by;
    struct leakentry	*next;
};


lte_str_compar(lte1, lte2)
struct	leakentry *lte1, *lte2;
{
    struct	leakpair	*path1, *path2;
    int		i;
      
    path1 = lte1->path;
    path2 = lte2->path;

    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	if (strcmp(path1[i].func, path2[i].func) != 0) {
	    return strcmp(path1[i].func, path2[i].func);
	}
    }
    return 0;
}

int
path_equal(path1, path2)
struct	leakpair	*path1, *path2;
{
    int		i;
    
    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	if (strcmp(path1[i].func, path2[i].func) != 0) {
	    return 0;
	}
    }
    return 1;
}


int
lte_size_compar(lte1, lte2)
struct	leakentry *lte1, *lte2;
{
    if ((lte1->all_by - lte1->fre_by) <
	(lte2->all_by - lte2->fre_by)) {
	return 1;
    } else if ((lte1->all_by - lte1->fre_by) >
	       (lte2->all_by - lte2->fre_by)) {
	return -1;
    } else {
	return 0;
    }
}

#define	V_TERSE		0
#define	V_NORMAL	1
#define	V_VERBOSE	2
int	verbosity;

#define LEAK_NONE	0
#define	LEAK_SHOW	1
#define	LEAK_OFFSETS	2
int	leak_level;

#define	TYPE_EQUAL		0
#define	TYPE_GREATERTHAN 	1
void	print_type_list();

extern 	char	*malloc();

void	print_bin_table();
void	print_leak_table();

char *
percent(m, n)
int	m, n;
{
    int		d;
    char	buf[80];
    char	*result;

    if (n == 0) {
	d = 0;
    } else {
	d = (100 * m) / n;
    }
    sprintf(buf, "%d", d);
    result = (char *) malloc(strlen(buf) + 1);
    strcpy(result, buf);
    return result;
}

char *
percent_string(m, n)
int	m, n;
{
    int		intpart, frac;
    int		d;
    char	buf[80];
    char	*result;

    if (n == 0) {
	goto rest;
    } else {
	intpart = (100 * m) / n;
	frac = (100 * m) % n;
	if ((intpart == 0) && (frac == 0)) {
	    return "";
	} else if (intpart == 0) {
	    return "(.)";
	} else if (intpart == 100) {
	    return "(**)";
	}
    }
 rest:
    if (n == 0) {
	d = 0;
    } else {
	d = (100 * m) / n;
    }
    sprintf(buf, "(%d)", d);
    result = (char *) malloc(strlen(buf) + 1);
    strcpy(result, buf);
    return result;
}
   

/* functions for uniquely recording recording each structure type
 */
struct sthash {
    int		kind;
    char 	*str;
    int		size;
    struct stconscell *numlist;
    struct sthash *next;
};


#define STHASH_SIZE	2047
struct sthash *sthmem[STHASH_SIZE];

#define	STNIL	NULL

struct stconscell {
    int		car;
    struct stconscell *cdr;
};

struct sthash **read_and_sort_types();

sthash_compar(ste1, ste2)
struct	sthash **ste1, **ste2;
{
    if ((*ste1)->size < (*ste2)->size) {
	return -1;
    } else if ((*ste1)-> size > (*ste2)->size) {
	return 1;
    } else {
	return strcmp((*ste1)->str, (*ste2)->str);
    }
}
    
	      
struct stconscell
*stcons(car, cdr)
int	car;
struct stconscell *cdr;
{
    struct stconscell *newcons;
    newcons = (struct stconscell *) malloc(sizeof(struct stconscell));
    newcons->car = car;
    newcons->cdr = cdr;
    return newcons;
}
		


struct stconscell
*stmember(item, stlist)
int	item;
struct stconscell *stlist;
{
    struct stconscell *s = stlist;
    
    while (s != STNIL) {
	if (s->car == item)
	    return s;
	s = s->cdr;
    }
    return STNIL;
}


bool
stmatchlist(l1, l2)
struct stconscell *l1, *l2;
{
    struct stconscell *s1, *s2;
    int		len1, len2;

    for (len1 = 0, s1 = l1; s1 != STNIL; len1++, s1 = s1->cdr) ;
    for (len2 = 0, s2 = l2; s2 != STNIL; len2++, s2 = s2->cdr) ;

    if (len1 == len2) {
	for (s1 = l1; s1 != STNIL; s1 = s1->cdr) {
	    if (stmember(s1->car, l2) == STNIL) {
		return FALSE;
	    }
	}
	return TRUE;
    }
    return FALSE;
}

	    
#define	T_STRUCT	0
#define	T_TYPEDEF	1


struct sthash
*mpf_new_stlink(kind, str, size, numlist, next)
int	kind;
char	*str;
int	size;
struct stconscell *numlist;
struct sthash *next;
{
    struct sthash *new_stl;

    new_stl = (struct sthash *) malloc(sizeof(struct sthash));
    new_stl->kind = kind;
    new_stl->str = str;
    new_stl->size = size;
    new_stl->numlist = numlist;
    new_stl->next = next;
    return new_stl;
}
    
int
mpf_sthash(s, len)
char	*s;
int	len;
{
    int		i;
    int		hash = 0;

    for (i = 0; i < len; i++) {
	hash = hash ^ (((int) *(s+i)) << (i % 6));
    }
    return ((hash >> 3) % STHASH_SIZE);
}

#define	N_EXCEPTIONS	11

char	*struct_exceptions[] = {
    "mp_function_struct",
    "mp_cons_struct",
    "mp_data_struct",
    "mp_sstk_struct",
    "mpheader",
    "_physadr",
    "_iobuf",
    "_quad",
    "flock",
    "fd_set",
    "label_t",
#ifdef mips
    "pdr",
    "fdr",
    "runtime_pdr",
#endif
    };

    
void
mpf_intern_type(s, size, structsize, tnumber)
char	*s;
int	size;
int	structsize;
int	tnumber;
{
    int			hash = mpf_sthash(s, size);
    int			i;
    struct sthash 	*ste = sthmem[hash];
    char		*newstr;

    for (i = 0; i < N_EXCEPTIONS; i++) {
	if (strncmp(s, struct_exceptions[i],
		    strlen(struct_exceptions[i])) == 0) {
	    return;
	}
    }
    while (ste != NULL) {
	if ((strncmp(s, ste->str, size) == 0) &&
	    (*((ste->str)+size) == NULL)) {
	    /* add the number to the list of numbers
	     */
	    if (stmember(tnumber, ste->numlist) == STNIL)
		ste->numlist = stcons(tnumber, ste->numlist);
	    return;
	}
	ste = ste->next;
    }
    newstr = malloc((unsigned) (size + 1));
    strncpy(newstr, s, size);
    *(char *) ((int) newstr + size) = NULL;
    ste = mpf_new_stlink(T_STRUCT,
			 newstr,
			 structsize,
			 stcons(tnumber, STNIL),
			 sthmem[hash]);
    sthmem[hash] = ste;
    return;
}

    
void
mpf_intern_typedef(s, size, tnumber)
char	*s;
int	size;
int	tnumber;
{
    int			hash = mpf_sthash(s, size);
    struct sthash 	*ste = sthmem[hash];
    char		*newstr;

    while (ste != NULL) {
	if ((strncmp(s, ste->str, size) == 0) &&
	    (*((ste->str)+size) == NULL)) {
	    
	    /* add the number to the list of numbers
	     */
	    if (stmember(tnumber, ste->numlist) == STNIL)
		ste->numlist = stcons(tnumber, ste->numlist);
	    return;
	}
	ste = ste->next;
    }
    newstr = malloc(size + 1);
    strncpy(newstr, s, size);
    *(char *) ((int) newstr + size) = NULL;
    ste = mpf_new_stlink(T_TYPEDEF,
			 newstr,
			 0,
			 stcons(tnumber, STNIL),
			 sthmem[hash]);
    sthmem[hash] = ste;
    return;
}

struct sthash **
read_and_sort_types(number)
int	*number;
{
    struct sthash 	*ste, *ste1;
    struct stconscell	*typedefs, *tl, *structs, *sl;
    int		tnum;
    int		i, pair_count;
    struct sthash 	**result;

    /* first, make a list of all typedefs and structs
     */
    typedefs = STNIL;
    structs = STNIL;
    for (i = 0; i < STHASH_SIZE; i++) {
	ste = sthmem[i];
	while (ste != NULL) {
	    if (ste->kind == T_TYPEDEF) {
		typedefs = stcons((int) ste, typedefs);
	    } else if (ste->kind == T_STRUCT) {
		structs = stcons((int) ste, structs);
	    }
	    ste = ste->next;
	}
    }

    /* for each typedef, if it points to a struct, change to a struct,
     * add the size, and remove the struct
     */
    tl = typedefs;
    while (tl != STNIL) {
	ste = (struct sthash *) tl->car;
	tnum = ste->numlist->car;
	sl = structs;
	while (sl != STNIL) {
	    ste1 = (struct sthash *) sl->car;
	    if ((stmember(tnum, ste1->numlist) != STNIL) &&
		(stmatchlist(ste->numlist, ste1->numlist))) {
		ste->kind = T_STRUCT;
		ste->size = ste1->size;
		ste1->kind = T_TYPEDEF;
		break;
	    }
	    sl = sl->cdr;
	}
	tl = tl->cdr;
    }

    /*
     * First, count how many there are
     */
    pair_count = 0;
    for (i = 0; i < STHASH_SIZE; i++) {
	ste = sthmem[i];
	while (ste != NULL) {
	    if (ste->kind == T_STRUCT) {
		pair_count += 1;
	    }
	    ste = ste->next;
	}
    }
    /*
     * Allocate a vector containing that many entries.
     */
    result = (struct sthash **) malloc(sizeof(struct sthash *) * pair_count);
    pair_count = 0;
    for (i = 0; i < STHASH_SIZE; i++) {
	ste = sthmem[i];
	while (ste != NULL) {
	    if (ste->kind == T_STRUCT) {
		result[pair_count] = ste;
		pair_count += 1;
	    }
	    ste = ste->next;
	}
    }

    qsort((char *) result, pair_count, sizeof(struct sthash *), sthash_compar);
	   
    *number = pair_count;
    return result;
}
	       


struct finfo {
    char 	*name;
    unsigned	addr;
};

#define	stab_name(x)	(stab[(x)].name)
#define	stab_addr(x)	(stab[(x)].addr)
  
#define	ST_SIZE		5000
#define	ST_NOT_FOUND	-1
typedef	int	stindex;

struct	finfo	stab[ST_SIZE];
int	stab_i;
#define	stab_incr(idx)	(((idx) < ST_SIZE) ? (idx)++ : \
			 (fprintf(stderr, "stab_incr -- stab table overflow (%d)\n", \
				 idx), exit(0)));

void		st_init();
int		stab_compare();
void		st_read();
#ifdef mips
int		st_read_structure();
#else
void		st_read_structure();
#endif
stindex		st_locate();
void		st_print();
void		st_print_one();

char	*st_strings;
bool	mprofing = FALSE;


int
stab_compare(e1, e2)
struct finfo *e1, *e2;
{
    if (e1->addr < e2->addr) {
	return -1;
    } else if (e1-> addr > e2-> addr) {
	return 1;
    } else {
	return 0;
    }
}

#ifdef mips

LDFILE *ldptr;

void
st_read(exec_name)
char	*exec_name;
{
    int i;
    PDR pdr;
    SYMR asym, asym2;
    extern char *ldgetname();	/* why isn't this in some header file? */
    extern pAUXU ldgetaux();	/* why isn't this in some header file? */
    pFDR pfd;
    pAUXU aux;
    AUXU localaux, aux2;
    char *name;
    int fdindex;
    int symindex;
    int rfd, rfi;

    ldptr = ldopen(exec_name, NULL);
    ldreadst(ldptr, ST_PSYMS | ST_PAUXS | ST_PFDS | ST_PPDS);

    /* read in pdr table */
    for (pfd = PFD(ldptr); pfd < PFD(ldptr) + SYMHEADER(ldptr).ifdMax;
        pfd++) {
      for (i = pfd->ipdFirst; i < pfd->ipdFirst + pfd->cpd; i++) {
        if (ldgetpd (ldptr, i, &pdr) != SUCCESS) {
          fprintf(stderr, "can't read pdr %d\n", i);
          exit (1);
        }
        if (pdr.isym != isymNil) {
          if (ldtbread(ldptr, pfd->csym ? pdr.isym :
		pdr.isym + SYMHEADER(ldptr).isymMax, &asym) != SUCCESS) {
            fprintf(stderr, "can't read symbol");
            exit (1);
          }
          pdr.adr = asym.value;
        }

	/* fill in finfo array */
	stab[i].addr = pdr.adr;
        if (pdr.isym == isymNil) {
          stab[i].name = "<stripped>";
        } else {
	  stab[i].name = ldgetname(ldptr, &asym);
        }

      }
    }

    stab_name(i) = "unknown";
    stab_addr(i) = stab_addr(i - 1) + 0x10000;
    stab_incr(i);
    stab_name(i) = "end_marker";
    stab_addr(i) = 0xffffffff;
    stab_incr(i);

    stab_i = i;

    /* read in structures
       this mips symbol table is extremely esoteric */

#ifdef DEBUG
#define err_print(str, index) fprintf(stderr, str, index)
#else
#define err_print(str, index)
#endif

    if (ldtbseek(ldptr) == SUCCESS) {

      for (i = 0; i < SYMHEADER(ldptr).isymMax + SYMHEADER(ldptr).iextMax - 1;
    		i++) {

	if (ldtbread(ldptr, i, &asym) != SUCCESS) {
	  err_print("can't read symbol, index = %d\n", asym.index);
	  continue;
	}

	/*
	 *  check locals and globals for possible structures and unions
	 */
	if (asym.st == stLocal || asym.st == stGlobal) {

	  if (asym.index == indexNil)
	    continue;

	  if (!(aux = ldgetaux(ldptr, asym.index))) {
	    err_print("can't read aux symbol, index = %d\n", asym.index);
	    continue;
	  }
	  localaux = *aux;	/* have to make copy before swapping */

	  fdindex = ld_ifd_iaux(ldptr, asym.index);
	  if (LDAUXSWAP(ldptr, fdindex))
	    swap_aux(&localaux, ST_AUX_TIR, gethostsex());
 
	  /* see if it's a struct or union */
	  if (localaux.ti.bt == btStruct || localaux.ti.bt == btUnion) {

	    /* if width specified, skip it */
	    if (localaux.ti.fBitfield) {
	      aux++;
	    }

	    ldgetrndx(ldptr, fdindex, aux + 1, &rfd, &aux2);
	    if (!aux2.rndx.index || aux2.rndx.index == ST_ANONINDEX)
	      continue;

	    rfi = ldgetrfd(ldptr, PFD(ldptr)[fdindex].rfdBase + rfd);
	    symindex = PFD(ldptr)[rfi].isymBase + aux2.rndx.index;

	    if (ldtbread(ldptr, symindex, &asym2) != SUCCESS) {
	      err_print("can't read symbol, index = %d\n", symindex);
	      continue;
	    }

	    name = ldgetname(ldptr, &asym2);
	    mpf_intern_type(name, strlen(name), asym2.value, asym2.index);

          }

	} else if (asym.st == stTypedef) {

	  if (asym.index == indexNil)
	    continue;

	  if (!(aux = ldgetaux(ldptr, asym.index))) {
	    err_print("can't read aux symbol, index = %d\n", asym.index);
	    continue;
	  }
	  localaux = *aux;	/* have to make copy before swapping */

	  fdindex = ld_ifd_iaux(ldptr, asym.index);
	  if (LDAUXSWAP(ldptr, fdindex))
	    swap_aux(&localaux, ST_AUX_TIR, gethostsex());
 
	  /* if width specified, skip it */
	  if (localaux.ti.fBitfield) {
	    aux++;
	  }

	  ldgetrndx(ldptr, fdindex, aux + 1, &rfd, &aux2);
	  if (!aux2.rndx.index || aux2.rndx.index == ST_ANONINDEX)
	    continue;

	  rfi = ldgetrfd(ldptr, PFD(ldptr)[fdindex].rfdBase + rfd);
	  symindex = PFD(ldptr)[rfi].isymBase + aux2.rndx.index;

	  if (ldtbread(ldptr, symindex, &asym2) != SUCCESS) {
	    err_print("can't read symbol, index = %d\n", symindex);
	    continue;
	  }

	  name = ldgetname(ldptr, &asym);
	  mpf_intern_typedef(name, strlen(name), asym2.index);

	}

      }

    }

}

#else

void
st_read(exec_name)
char	*exec_name;
{
    int		aout_file = open(exec_name, (O_RDONLY));
    struct	exec	hdr;
    struct	nlist	asym;
    extern char *index();
    extern char *malloc();
    char	*stmp;
    int		string_size;
    unsigned char type;
    char	*fname;
    int		i;
    
    read(aout_file, &hdr, sizeof(hdr));
    if (!hdr.a_syms) {
	fprintf(stdout, "st_read -- no symbols in executable\n");
	exit(1);
    }

    /* read in the string table
     */
    lseek(aout_file, N_STROFF(hdr), L_SET);
    read(aout_file, &string_size, 4);

    st_strings = malloc(string_size);
    
    lseek(aout_file, N_STROFF(hdr), L_SET);
    read(aout_file, st_strings, string_size);

    /* read in the symbols one at a time
     */
    lseek(aout_file, N_SYMOFF(hdr), L_SET);
    for (i = 0; i < hdr.a_syms / sizeof(struct nlist); i++) {
	read(aout_file, &asym, sizeof(asym));
	type = asym.n_type;
	/* check for functions compiled with -g
	 */
	if (type & N_STAB) {
	    if (asym.n_type == N_FUN) {
/*	    
		stab_name(stab_i) = (char *) (st_strings + asym.n_un.n_strx);
		stab_addr(stab_i) = asym.n_value;
		stmp = index(stab_name(stab_i), ':');
		*stmp = NULL;
		stab_incr(stab_i);
*/
	    } else if ((asym.n_type == N_LSYM) ||
		       (asym.n_type == N_GSYM)) {
		/* a local symbol that may be a structure definition
		 */
		st_read_structure((char *) (st_strings + asym.n_un.n_strx));
	    }
	} else {
	    /* here's a candidate for a function name
	     */
	    if ((type & N_TYPE) == N_TEXT) {
		fname = (char *) (st_strings + asym.n_un.n_strx);
		if ((*fname == '_') & !index(fname, '.')) {
		    /* since there is not '.' in the name, its probably a
		     * function name
		     */
		    stab_name(stab_i) = (char *) ((int) fname + 1);
		    stab_addr(stab_i) = asym.n_value;
		    stab_incr(stab_i);
		}
	    }
	}
    }
    stab_name(stab_i) = "unknown";
    stab_addr(stab_i) = stab_addr(stab_i - 1) + 0x10000;
    stab_incr(stab_i);
    stab_name(stab_i) = "end_marker";
    stab_addr(stab_i) = 0xffffffff;
    stab_incr(stab_i);
    qsort(stab, stab_i, sizeof(struct finfo), stab_compare);
}

void
st_read_structure(symp)
char	*symp;
{
    char	*eqp, *colp;
    int		ssize, tnum;
    extern int	atoi();
    extern char	*index();
    
    eqp = index(symp, '=');
    colp = index(symp, ':');

    if ((eqp == NULL) && (*(colp+1) == 't')) {
	/*
	 * Check for a Sun stabs type definition.
	 */
	if (*(colp+2) == '(') {
	    char *commap;
	    commap = index(symp, ',');
	    *commap = '0';
	    tnum = atoi((char *) index(symp, '(')+1);
	} else {
	    tnum = atoi((char *) (colp+2));
	}
	mpf_intern_typedef(symp, colp - symp, tnum);
	
    } else if ((eqp != NULL) && *(eqp+1) == 's') {
	
	/* we have a structure entry...
	 * 1. get the size, number, and name
	 * 3. enter into the structure hash table
	 */
	/* get the size (follows eqp+1)
	 */
	ssize = atoi((char *) eqp+2);
	
	/*
	 * Get the number (follows :T)
	 */
	
	/*
	 * Check for a Sun stabs type definition.
	 */
	if (*(colp+2) == '(') {
	    char *commap;
	    commap = index(symp, ',');
	    *commap = '0';
	    tnum = atoi((char *) index(symp, '(')+1);
	} else {
	    tnum = atoi((char *) colp+2);
	}
	
	/* enter the name into the structure hash table
	 */
	mpf_intern_type(symp, (index(symp, ':') - symp), ssize, tnum);
    }
}

#endif

stindex
st_locate(addr)
unsigned	addr;
{
    int		upper = stab_i - 1;
    int		lower = 0;
    int		middle = (upper + lower) / 2;

    while (!((stab_addr(middle) <= addr) && (stab_addr(middle + 1) > addr))) {
	if (middle == upper || middle == lower) {
	    return ST_NOT_FOUND;
	}
	if (stab_addr(middle) > addr) {
	    upper = middle;
	} else {
	    lower = middle;
	}
	middle = (upper + lower) / 2;
    }
    if (middle >= (stab_i - 2)) {
	return ST_NOT_FOUND;
    } else {
	return middle;
    }
}

void
st_print()
{
    int		i;

    printf("function symbol table:\n");
    for (i = 0; i < stab_i; i++) {
	st_print_one(i);
    }
}

void
st_print_one(i)
stindex		i;
{
    printf(" %d	%-15s  %10d\n", i,stab_name(i), stab_addr(i));
}

void
increment_data(dt, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
mpdata	dt;
int	d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12;
{
    dt_b_small(dt) += d1;    
    dt_b_med(dt) += d2;    
    dt_b_large(dt) += d3;    
    dt_b_xlarge(dt) += d4;    
    dt_n_small(dt) += d5;    
    dt_n_med(dt) += d6;    
    dt_n_large(dt) += d7;
    dt_n_xlarge(dt) += d8;
    dt_d_small(dt) += d9;    
    dt_d_med(dt) += d10;   
    dt_d_large(dt) += d11;
    dt_d_xlarge(dt) += d12;
}

void
st_convert(data_filename)
char	*data_filename;
{
    FILE	*f = fopen(data_filename, "r");
    unsigned	faddr, paddr;
    stindex	fx, px;
    mpsym	fsym, psym;
    mpdata	dcell;
    mpcell	ppair;
    int		d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12;
    int		i, x;

    /*
     * read the prolog containing stats
     * handle the stats data first as a special case
     */
    check_fscanf(fscanf(f, "alloc=%d free=%d depth=%d same=%d all=%d\n", &d1, &d2, &d3, &d4, &d5));
    fflush(stdout);
    check_fscanf(fscanf(f, "fmem=%d dmem=%d lmem=%d smem=%d\n", &d6, &d7, &d8, &d9));
    /*
     * print the prolog
     */
    printf("--c%2s+--v3.0+--m%d+--+--+--+--+--+--+--+--+ MPROF +--+--+--+--+--+--+--s%d+--f%d+--d%d+--l%d+\n\n\n",
	   percent(d4, d5), d3, d9, d6, d7, d8);
    
    fflush(stdout);

    print_bin_table(f, stdout);

    print_leak_table(f, stdout);

    while (fscanf(f, "%d\n", &faddr) != EOF) {
	fx = st_locate(faddr);
	fsym = pc_lookup(stab_addr(fx));
	fn_name(fsym) = stab_name(fx);
	fscanf(f, "%d %d %d %d %d %d %d %d %d %d %d %d\n",
	       &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &d9, &d10, &d11, &d12);
	increment_data(fn_lcount(fsym),
		       d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12);
	fscanf(f, "%d\n", &paddr);
	while ((int) paddr != MP_NIL) {
	    px = st_locate(paddr);
	    psym = pc_lookup(stab_addr(px));
	    ppair = mp_has_parent(fsym, psym);
	    if (mp_null(ppair)) {
		dcell = mp_new_data();
		ppair = mp_cons((int) psym, (int) dcell);
		fn_parents(fsym) = mp_cons((int) ppair,
					   fn_parents(fsym));
	    } 
	    fscanf(f, "%d %d %d %d %d %d %d %d %d %d %d %d\n",
		   &d1, &d2, &d3, &d4, &d5, &d6,
		   &d7, &d8, &d9, &d10, &d11, &d12);
	    increment_data(mp_cdr(ppair),
			   d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12);
	    fscanf(f, "%d\n", &paddr);
	}
    }
    mprof_graph_ops(stdout);
}


void
print_bin_table(infile, outfile)
FILE	*infile, *outfile;
{
    int		i;
    int		type_count, type_index = 0;
    struct sthash **type_table;
    int		alloc_bins[MP_NUM_BINS];
    int		free_bins[MP_NUM_BINS];
    int		big_alloc_count, big_alloc_bytes;
    int		big_free_count, big_free_bytes;
    int		alloc_count = 0, alloc_bytes = 0, free_count = 0, free_bytes = 0;
    int		other_alloc_count = 0, other_alloc_bytes = 0;
    int		other_free_count = 0, other_free_bytes = 0;
    int		abin, fbin;
    int		byte_difference;

    /*
     * Read in and print out the Bin table information.
     */
    type_table = read_and_sort_types(&type_count);

    /* read in the bins and save the data
     */
    alloc_count = 0;
    alloc_bytes = 0;
    for (i = 0; i < (MP_NUM_BINS - 2); i++) {
	check_fscanf(fscanf(infile, "%d\n", &alloc_bins[i]));
	alloc_count += alloc_bins[i];
	alloc_bytes += alloc_bins[i] * i;
    }
    check_fscanf(fscanf(infile, "%d\n", &big_alloc_count));
    alloc_count += big_alloc_count;
    if (alloc_count == 0) {
	alloc_count = 1;
    }
    check_fscanf(fscanf(infile, "%d\n", &big_alloc_bytes));
    alloc_bytes += big_alloc_bytes;
   
    free_count = 0;
    free_bytes = 0;
    for (i = 0; i < (MP_NUM_BINS - 2); i++) {
	check_fscanf(fscanf(infile, "%d\n", &free_bins[i]));
	free_count += free_bins[i];
	free_bytes += free_bins[i] * i;
    }
    check_fscanf(fscanf(infile, "%d\n", &big_free_count));
    free_count += big_free_count;
    if (free_count == 0) {
	free_count = 1;
    }
    check_fscanf(fscanf(infile, "%d\n", &big_free_bytes));
    free_bytes += big_free_bytes;

    byte_difference = alloc_bytes - free_bytes;

#define abin_template \
    "%11d%10d%10d %-4s%10d%10d %-4s   "
#define abin_template2 \
    "%11s%10d%10d %-4s%10d%10d %-4s   "
#define	abin_titles_template \
    "%11s%10s%10s %-4s%10s%10s %-4s    %-8s\n"
    
    fprintf(outfile, "--------- Allocation Bins with possible Types ------------\n\n");
    fprintf(outfile, abin_titles_template,
	    "size:", "allocs", "bytes", "(%)", "frees", "kept", "(%)", "types");
    fprintf(outfile, "\n");

    for (i = 0; i < (MP_NUM_BINS - 2); i++) {
	abin = alloc_bins[i];
	fbin = free_bins[i];
	/*
	 * Print things depending on the level of verbosity.
	 */
	if (((verbosity == V_VERBOSE) &&
	     ((abin > 0) || (fbin > 0)))
	    ||
	    ((verbosity == V_NORMAL) &&
	     (((abin > 0) && (i > 100)) ||
	      (((double) abin / alloc_count) > 1.0/500.0) ||
	      ((type_index != type_count) &&
	       (((double) abin / alloc_count) > 1.0/100.0) &&
	       ((type_table[type_index])->size == i)))) 
	    ||
	    ((verbosity == V_TERSE) &&
	     ((((double) abin / alloc_count) > 1.0/50.0) ||
	      ((type_index != type_count) &&
	       (((double) abin / alloc_count) > 1.0/100.0) &&
	       ((type_table[type_index])->size == i))))) {
	    fprintf(outfile, abin_template,
		    i, abin, i * abin,
		    percent_string(i * abin, alloc_bytes),
		    fbin, (i * abin) - (i * fbin),
		    percent_string((i * abin) - (i * fbin),
				   byte_difference));
	    /*
	     * Print out relevant types.
	     */
	    print_type_list(outfile, type_table, TYPE_EQUAL, i,
			    type_count, &type_index);
		   
	    fprintf(outfile, "\n");
	} else {
	    other_alloc_count += abin;
	    other_free_count += fbin;
	    other_alloc_bytes += abin * i;
	    other_free_bytes += fbin * i;
	}
    }
    /*
     * Print the things at the end of the table.
     */
    fprintf(outfile, abin_template2,
	    "> 1024", big_alloc_count, big_alloc_bytes,
	    percent_string(big_alloc_bytes, alloc_bytes),
	    big_free_count, (big_alloc_bytes - big_free_bytes),
	    percent_string((big_alloc_bytes - big_free_bytes),
			   byte_difference));
    print_type_list(outfile, type_table, TYPE_GREATERTHAN, 1024,
		    type_count, &type_index);
    fprintf(outfile, "\n\n");
    if ((other_alloc_count > 0) ||
	(other_free_count > 0)) {
	fprintf(outfile, abin_template2,
		"other bins", other_alloc_count, other_alloc_bytes,
		percent_string(other_alloc_bytes, alloc_bytes),
		other_free_count, (other_alloc_bytes - other_free_bytes),
		percent_string((other_alloc_bytes - other_free_bytes),
			       byte_difference));
	fprintf(outfile, "\n");
    }
    fprintf(outfile, abin_template2,
	    "<TOTAL>", alloc_count, alloc_bytes, "",
	    free_count, byte_difference, "");
    fprintf(outfile, "\n\f\n\n");
    fflush(outfile);
}


void
print_type_list(outfile, tlist, compar, binsize, type_count, type_index)
FILE	*outfile;
struct sthash **tlist;
int	compar, binsize, type_count;
int	*type_index;
{
    int		i, cond, pcount = 0;
    
    for (i = *type_index;
	 ((i < type_count) && (tlist[i]->size <= binsize));
	 i++) {
	if (compar == TYPE_EQUAL) {
	    cond = (tlist[i]->size == binsize);
	} else if (compar == TYPE_GREATERTHAN) {
	    cond = (tlist[i]->size > binsize);
	}
	if (cond) {
	    /*
	     * Make the spacing nice.
	     */
	    if ((pcount > 0) && ((pcount % 3) == 0)) {
		fprintf(outfile, "\n%64s", "");
	    }
	    fprintf(outfile, "%-12s ", tlist[i]->str);
	    pcount += 1;
	}
    }
    *type_index = i;
}



#define leak_titles_template1 \
    "%10s %-4s%10s%10s %-4s%10s%10s %-4s    %-8s\n"
#define	leak_titles_template2 \
    "%10s %-4s%10s%10s %-4s    %-8s\n"
#define	leak_template1 \
    "%10d %-4s%10d%10d %-4s%10d%10d %-4s   "
#define	leak_template2 \
    "%10d %-4s%10d%10d %-4s   "

void
print_leak_table(infile, outfile)
FILE	*infile, *outfile;
{
    int		total_allocs = 0, bytes_alloced = 0;
    int		total_frees= 0, bytes_freed= 0;
    int		byte_diff;
    struct leakentry	*lt_root = NULL, *lte = NULL, *lt_vec = NULL;
    int		lte_count = 0;
    int		d1, d2, d3, d4, d5, i, j, real_i;
    stindex	fx;
    mpsym	fsym;
    
    /* read in the leak table and print it back out
     */
    check_fscanf(fscanf(infile, "%d %d %d %d\n", &d1, &d2, &d3, &d4));
    while(d1 != -2) {
	/*
	 * Gather the path for a single leak table entry.
	 */
	lte = (struct leakentry *) malloc(sizeof(struct leakentry));
	lte_count += 1;
	
	for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	    check_fscanf(fscanf(infile, "%d\n", &d5));
	    if (d5 != 0) {
		fx = st_locate(d5);
		fsym = pc_lookup(stab_addr(fx));
		fn_name(fsym) = stab_name(fx);
		lte->path[SHORT_CALLSTACK_SIZE - (i + 1)].func = fn_name(fsym);
		lte->path[SHORT_CALLSTACK_SIZE - (i + 1)].offset = d5 - stab_addr(fx);
	    } else {
		lte->path[SHORT_CALLSTACK_SIZE - (i + 1)].func = "";
		lte->path[SHORT_CALLSTACK_SIZE - (i + 1)].offset = 0;
	    }
	}
        lte->all_no = d1;
	total_allocs += d1;
        lte->all_by = d2;
	bytes_alloced += d2;
        lte->fre_no = d3;
	total_frees += d3;
        lte->fre_by = d4;
	bytes_freed += d4;

	/*
	 * Add to the list of leak entries.
	 */
	lte->next = lt_root;
	lt_root = lte;
	
	check_fscanf(fscanf(infile, "%d %d %d %d", &d1, &d2, &d3, &d4));
    }
    byte_diff = bytes_alloced - bytes_freed;

    if ((lte_count == 0) || (leak_level == LEAK_NONE)) {
	return;
    }
    fprintf(outfile, "--------- Partial Dynamic Call Paths for Memory Leaks ------------\n\n");
    fprintf(outfile,  "Total bytes not freed: %d\n\n", byte_diff);
    
    if (total_frees > 0) {
	fprintf(outfile, leak_titles_template1,
		"kept bytes", "(%)", "allocs", "bytes", "(%)",
		"frees", "bytes", "(%)", "path");
    } else {
	fprintf(outfile, leak_titles_template2,
		"kept bytes", "(%)", "allocs", "bytes", "(%)", "path");
    }
    fprintf(outfile, "\n");

    /*
     * Here we put the leak table entries into a vector so we can
     * manipulate them.
     */
    lt_vec = (struct leakentry *) malloc(sizeof(struct leakentry) * lte_count);
    lte = lt_root;
    i = 0;
    while (lte != NULL) {
	struct leakentry	*tmp;
	
	lt_vec[i] = *lte;
	i++;
	tmp = lte;
	lte = lte->next;
	free(tmp);
    }
	      
    if (leak_level == LEAK_SHOW) {
	/*
	 * Sort the entries so that duplicate paths are together.
	 */
	qsort((char *) lt_vec, lte_count, sizeof(struct leakentry), lte_str_compar);
	if (lte_count > 1) {
	    real_i = 0;
	    for (i = 1; i < lte_count;) {
		if (path_equal(lt_vec[real_i].path, lt_vec[i].path)) {
		    /*
		     * Merge the data from identical paths together.
		     */
		    lt_vec[real_i].all_no += lt_vec[i].all_no;
		    lt_vec[real_i].all_by += lt_vec[i].all_by;
		    lt_vec[real_i].fre_no += lt_vec[i].fre_no;
		    lt_vec[real_i].fre_by += lt_vec[i].fre_by;
		    i += 1;
		} else {
		    real_i += 1;
		    /*
		     * First, copy the data, compressing out bubbles
		     */
		    if (real_i != i) {
			lt_vec[real_i] = lt_vec[i];
		    }
		    i += 1;
		}
	    }
	    lte_count = real_i + 1;
	}
    }

    qsort((char *) lt_vec, lte_count, sizeof(struct leakentry), lte_size_compar);

    for (i = 0; i < lte_count; i+=1) {
	struct leakentry lte;
	lte = lt_vec[i];
	if (verbosity == V_TERSE) {
	    if (((double) lte.all_by / bytes_alloced) < 0.01) {
		continue;
	    }
	} else if (verbosity == V_NORMAL) {
	    if (((double) lte.all_by / bytes_alloced) < 0.005) {
		continue;
	    }
	}
	if (total_frees > 0) {
	    fprintf(outfile, leak_template1,
		    (lte.all_by - lte.fre_by),
		    percent_string((lte.all_by - lte.fre_by), byte_diff),
		    lte.all_no, lte.all_by,
		    percent_string(lte.all_by, bytes_alloced),
		    lte.fre_no, lte.fre_by,
		    percent_string(lte.fre_by, bytes_freed));
	} else {
	    fprintf(outfile, leak_template2,
		    (lte.all_by - lte.fre_by),
		    percent_string((lte.all_by - lte.fre_by), byte_diff),
		    lte.all_no, lte.all_by,
		    percent_string(lte.all_by, bytes_alloced));
	}
	if ((strcmp(lte.path[0].func, "") == 0) ||
	    (strcmp(lte.path[0].func, "main") == 0)) {
	    fprintf(outfile, "|| ");
	} else {
	    fprintf(outfile, "...");
	}
	for (j = 0; j < SHORT_CALLSTACK_SIZE; j++) {
	    if (strcmp(lte.path[j].func, "") != 0) {
		if (leak_level == LEAK_SHOW) {
		    fprintf(outfile, "> %s ", lte.path[j].func);
		} else if (leak_level == LEAK_OFFSETS) {
		    fprintf(outfile, "> %s+%d ", lte.path[j].func, lte.path[j].offset);
		}
	    }
	}
	fprintf(outfile, "\n");
    }
    fprintf(outfile, "\n\f\n\n");
    fflush(outfile);
}
	
    


void
usage()
{
    fprintf(stderr, "usage: mprof [-leaktable | -noleaktable] \n\
		[-verbose | -normal | -terse] \n\
		[a.out-name] [data-name]\n");
    exit(1);
}

#define	str_equal(s1, s2)	(strcmp((s1), (s2)) == 0)

int
main(argc, argv)
int argc;
char *argv[];
{
    int		i;
    char	*exec_file = "a.out";
    char	*data_file = "mprof.data";

    /*
     * Default settings.
     */
    leak_level = LEAK_SHOW;
    verbosity = V_NORMAL;

    for (i = 1; i < argc; i++) {
	if (str_equal(argv[i], "-leaktable")) {
	    leak_level = LEAK_SHOW;
        } else if (str_equal(argv[i], "-noleaktable")) {
	    leak_level = LEAK_NONE;
	} else if (str_equal(argv[i], "-offsets")) {
	    leak_level = LEAK_OFFSETS;
	} else if (str_equal(argv[i], "-verbose")) {
	    verbosity = V_VERBOSE;
	} else if (str_equal(argv[i], "-normal")) {
	    verbosity = V_NORMAL;
	} else if (str_equal(argv[i], "-terse")) {
	    verbosity = V_TERSE;
	} else if (i == (argc - 1)) {
	    exec_file = argv[i];
	    break;
	} else if (i == (argc - 2)) {
	    exec_file = argv[i];
	    data_file = argv[i+1];
	    break;
	} else {
	    usage();
	}
    }
	   
    mpstruct_init();
    stab_i = 0;
    for (i = 0; i < ST_SIZE; i++) {
	stab_name(i) = NULL;
    }
    
    st_read(exec_file);
    st_convert(data_file);

    exit(0);
}    

