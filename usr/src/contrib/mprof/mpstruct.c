/*	mpstruct.c 2.2 9/14/90 11:54:22	*/
/*	Copyright (c) 1987, Benjamin G. Zorn */

#include	<stdio.h>
#include	"mprof.h"

extern	mpcell	hmem[];
extern	bool	mprofing;

mpcell	hmem[MP_HASH_SIZE];

int	mprof_fmemC;
int	mprof_dmemC;
int	mprof_lmemC;


mpcell
mp_cons(a, d)
int	a;
mpcell	d;
{
    mpcell result = (mpcell) malloc(MPCELL_SIZE);
    
    mprof_lmemC++;
    mp_car(result) = a;
    mp_cdr(result) = d;
    return result;
}

mpsym
mp_new_fn(addr)
unsigned	addr;
{
    mpsym	newfn = (mpsym) malloc(MPSYM_SIZE);

    mprof_fmemC++;
    fn_lcount(newfn) = mp_new_data();
    fn_addr(newfn) = addr;
    fn_parents(newfn) = (mpcell) MP_NIL;
    fn_name(newfn) = NULL;
    return newfn;
}

mpdata
mp_new_data()
{
    mpdata 	result = (mpdata) malloc(MPDATA_SIZE);
    
    mprof_dmemC++;

    dt_b_small(result) = 0;
    dt_b_med(result) = 0;
    dt_b_large(result) = 0;
    dt_b_xlarge(result) = 0;
    dt_n_small(result) = 0;
    dt_n_med(result) = 0;
    dt_n_large(result) = 0;
    dt_n_xlarge(result) = 0;
    dt_d_small(result) = 0;
    dt_d_med(result) = 0;
    dt_d_large(result) = 0;
    dt_d_xlarge(result) = 0;
    return result;
}

mpdata
mp_add_data(ddest, dsrc)
mpdata	ddest, dsrc;
{
    dt_b_small(ddest) += dt_b_small(dsrc);
    dt_b_med(ddest) += dt_b_med(dsrc);
    dt_b_large(ddest) += dt_b_large(dsrc);
    dt_b_xlarge(ddest) += dt_b_xlarge(dsrc);
    dt_n_small(ddest) += dt_n_small(dsrc);
    dt_n_med(ddest) += dt_n_med(dsrc);
    dt_n_large(ddest) += dt_n_large(dsrc);
    dt_n_xlarge(ddest) += dt_n_xlarge(dsrc);
    dt_d_small(ddest) += dt_d_small(dsrc);
    dt_d_med(ddest) += dt_d_med(dsrc);
    dt_d_large(ddest) += dt_d_large(dsrc);
    dt_d_xlarge(ddest) += dt_d_xlarge(dsrc);
    return ddest;
}

int
mp_sum_data(d)
mpdata	d;
{
    return (dt_b_small(d) + dt_b_med(d) + dt_b_large(d) + dt_b_xlarge(d));
}

int
mp_sum_calls(d)
mpdata	d;
{
    return (dt_n_small(d) + dt_n_med(d) + dt_n_large(d) + dt_n_xlarge(d));
}

int
mp_sum_kept(d)
mpdata	d;
{
    return (dt_d_small(d) + dt_d_med(d) + dt_d_large(d) + dt_d_xlarge(d));
}

mpcell
mp_has_parent(c, p)
mpsym	c, p;
{
    mpcell	plist = fn_parents(c);
    unsigned	pname;
    mpcell	pair;
    mpsym	f;

    pname = fn_addr(p);
    while (!mp_null(plist)) {
	pair = (mpcell) mp_car(plist);
	f = (mpsym) mp_car(pair);
	if (fn_addr(f) == pname)
	  return pair;
	plist = (mpcell) mp_cdr(plist);
    }
    return (mpcell) MP_NIL;
}

int
mp_hash(pc)
unsigned	pc;
{
    int		hash = (pc >> 24 | (pc & 0xff00)) ^
      		       (((pc & 0xff) << 8) | (pc & 0xff0000) >> 16);
    return (hash % MP_HASH_SIZE);
}

void
mp_puthash(pc, f)
unsigned	pc;
mpsym		f;
{
    int		hash = mp_hash(pc);

    hmem[hash] = mp_cons((int) f, hmem[hash]);
}

mpsym
mp_lookup(pc)
unsigned	pc;
{
    int		hash = mp_hash(pc);
    mpcell	c = hmem[hash];
    mpsym	s;
    
    while (!mp_null(c)) {
	s = (mpsym) mp_car(c);
	if (pc == fn_addr(s))
	  return s;
	c = (mpcell) mp_cdr(c);
    }
    return (mpsym) MP_NIL;
}
	

bool	mp_pprint = FALSE;

void
mp_print_fn(mprof_file, f)
int	mprof_file;
mpsym	f;
{
    mpcell	plist;

    plist = fn_parents(f);
    if (fn_name(f) != NULL)
      mp_pprint = TRUE;

    /* print name, lcount, and parents
     */

    if (mp_pprint) {
	write(mprof_file, "(\"", 2);
	write(mprof_file, fn_name(f), strlen(fn_name(f)));
	write(mprof_file, "\" ", 2);
	mp_print_data(mprof_file, fn_lcount(f));
	write(mprof_file, " ", 1);
	mp_print_parents(mprof_file, plist);
	write(mprof_file, ")\n", 2);
    } else {
	mp_print_addr(mprof_file, f);
	mp_print_data(mprof_file, fn_lcount(f));
	mp_print_parents(mprof_file, plist);
    }
}

void
mp_print_addr(mprof_file, f)
int	mprof_file;
mpsym	f;
{
    char	digits[14];

    sprintf(digits, "%d\n", fn_addr(f));
    write(mprof_file, digits, strlen(digits));
}

char *
mp_sprint_data(d)
mpdata	d;
{
    char	digits[255];
    char	*result;

    sprintf(digits, "(%d %d %d %d %d %d %d %d %d %d %d %d)",
	    dt_b_small(d),
	    dt_b_med(d),
	    dt_b_large(d),
	    dt_b_xlarge(d),
	    dt_n_small(d),
	    dt_n_med(d),
	    dt_n_large(d),
	    dt_n_xlarge(d),
	    dt_d_small(d),
	    dt_d_med(d),
	    dt_d_large(d),
	    dt_d_xlarge(d));
    result = malloc(strlen(digits) + 1);
    strcpy(result, digits);
    return result;
}

void
mp_print_data(mprof_file, d)
int	mprof_file;
mpdata	d;
{
    char	digits[255];

    if (mp_pprint) {
	sprintf(digits, "(%d %d %d %d %d %d %d %d %d %d %d %d)",
		dt_b_small(d),
		dt_b_med(d),
		dt_b_large(d),
		dt_b_xlarge(d),
		dt_n_small(d),
		dt_n_med(d),
		dt_n_large(d),
		dt_n_xlarge(d),
		dt_d_small(d),
		dt_d_med(d),
		dt_d_large(d),
		dt_d_xlarge(d));
    } else {
	sprintf(digits, "%d %d %d %d %d %d %d %d %d %d %d %d\n",
		dt_b_small(d),
		dt_b_med(d),
		dt_b_large(d),
		dt_b_xlarge(d),
		dt_n_small(d),
		dt_n_med(d),
		dt_n_large(d),
		dt_n_xlarge(d),
		dt_d_small(d),
		dt_d_med(d),
		dt_d_large(d),
		dt_d_xlarge(d));
    }
    write(mprof_file, digits, strlen(digits));
}

void
mp_print_parents(mprof_file, l)
int	mprof_file;
mpcell	l;
{
    mpcell	rest = l;
    mpcell	p;

    if (mp_pprint) {
	write(mprof_file, "(", 1);
	while (!mp_null(rest)) {
	    p = (mpcell) mp_car(rest);
	    write(mprof_file, "(\"", 2);
	    write(mprof_file,
		  fn_name((mpsym) mp_car(p)),
		  strlen(fn_name((mpsym) mp_car(p))));
	    write(mprof_file, "\" ", 2);
	    mp_print_data(mprof_file, (mpdata) mp_cdr(p));
	    write(mprof_file, ")", 1);
	    rest = mp_cdr(rest);
	    if (!mp_null(rest)) {
		write(mprof_file, " ", 1);
	    }
	}
	write(mprof_file, ")", 1);
    } else {
	while (!mp_null(rest)) {
	    p = (mpcell) mp_car(rest);
	    mp_print_addr(mprof_file, (mpsym) mp_car(p));
	    mp_print_data(mprof_file, (mpdata) mp_cdr(p));
	    rest = (mpcell) mp_cdr(rest);
	}
	write(mprof_file, "-1\n", 3);
    }
}


void
mprof_print(mprof_file)
int	mprof_file;
{
    mpsym	s;
    int		i;
    mpcell	chain;

    for (i = 0; i < MP_HASH_SIZE; i++) {
	chain = hmem[i];
	while (!mp_null(chain)) {
	    s = (mpsym) mp_car(chain);
	    mp_print_fn(mprof_file, s);
	    chain = (mpcell) mp_cdr(chain);
	}
    }
}


mpsym
pc_lookup(pc)
unsigned	pc;
{
    mpsym	s;
    
    s = mp_lookup(pc);
    if (!mp_null(s)) {
	return s;
    } else {
	/*
	 *  create a new function
	 */
	s = mp_new_fn(pc);
	mp_puthash(pc, s);
	return s;
    }
}

void
mpstruct_init()
{
    int		i;
    
    for (i = 0; i < MP_HASH_SIZE; i++) {
	hmem[i] = (mpcell) MP_NIL;
    }
    mprof_lmemC = 0;
    mprof_fmemC = 0;
    mprof_dmemC = 0;
}
