/*	leak.c 2.1 7/8/88 14:15:57	*/
/*	Copyright (c) 1987, Benjamin G. Zorn */

#include	<stdio.h>
#include	"mprof.h"

int	mprof_smemC;
mpcell	lt_hmem[MP_HASH_SIZE];

mpsstk	mp_new_sstk();
int	lt_hash();
void	lt_puthash();
mpsstk	lt_lookup();
void	mp_print_leak();

extern	bool	keeping_leaks;

bool	keeping_leaks = TRUE;

mpsstk
mp_new_sstk(nbytes, scallstack)
int		nbytes;
unsigned	scallstack[SHORT_CALLSTACK_SIZE];
{
    mpsstk	newsstk = (mpsstk) malloc(MPSSTK_SIZE);
    int		i;

    mprof_smemC++;
    sstk_allocs(newsstk) = 1;
    sstk_bytes_alloced(newsstk) = nbytes;
    sstk_frees(newsstk) = 0;
    sstk_bytes_freed(newsstk) = 0;
    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	sstk_addrs(newsstk)[i] = scallstack[i];
    }
    return (newsstk);
}


int
lt_hash(addrs)
unsigned	addrs[SHORT_CALLSTACK_SIZE];
{
    unsigned 	tmp = 0, i;
    int		hash;

    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	tmp = tmp ^ addrs[i];
    }
    hash = (tmp >> 24 | (tmp & 0xff00)) ^
           (((tmp & 0xff) << 8) | (tmp & 0xff0000) >> 16);
    return (hash % MP_HASH_SIZE);
}

void
lt_puthash(s)
mpsstk	s;
{
    int		hash = lt_hash(sstk_addrs(s));

    lt_hmem[hash] = mp_cons((int) s, lt_hmem[hash]);
}

mpsstk
lt_lookup(addrs)
unsigned	addrs[SHORT_CALLSTACK_SIZE];
{
    int		hash = lt_hash(addrs);
    mpcell	c = lt_hmem[hash];
    mpsstk	s;
    int		i;
    unsigned	*haddrs;
    
    while (!mp_null(c)) {
	s = (mpsstk) mp_car(c);
	haddrs = sstk_addrs(s);
	for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	    if (addrs[i] != haddrs[i]) {
		break;
	    } else if (i == SHORT_CALLSTACK_SIZE - 1) {
		return s;
	    }
	}
	c = (mpcell) mp_cdr(c);
    }
    return (mpsstk) MP_NIL;
}

mpsstk
mp_add_leak_table(sstk, nbytes)
unsigned	sstk[SHORT_CALLSTACK_SIZE];
int		nbytes;
{
    mpsstk s;

    if (!keeping_leaks) return NULL;

    s = lt_lookup(sstk);
    if (s != (mpsstk) MP_NIL) {
	sstk_allocs(s)++;
	sstk_bytes_alloced(s) += nbytes;
    } else {
	s = mp_new_sstk(nbytes, sstk);
	lt_puthash(s);
    }
    return s;
}

void
mp_remove_leak_table(leakdata, nbytes)
mpsstk 		leakdata;
int		nbytes;
{

    if (!keeping_leaks) return;

    if (leakdata != (mpsstk) MP_NIL) {
	sstk_frees(leakdata)++;
	sstk_bytes_freed(leakdata) += nbytes;
    } else {
	fprintf(stderr,
		"mp_remove_leak_table -- free of unallocated object\n");
	exit(1);
    }
}

void
mp_print_leak_table(file)
int	file;
{
    mpsstk 	s;
    int		i;
    mpcell	chain;
    int		abytes = 0, fbytes = 0;
    char	outbuf[256];

/*  sprintf(outbuf, "%10s %10s %10s %10s     %-10s\n",
	    "allocs", "bytes", "frees", "bytes", "path");
    write(file, outbuf, strlen(outbuf));
*/

    for (i = 0; i < MP_HASH_SIZE; i++) {
	chain = lt_hmem[i];
	while (!mp_null(chain)) {
	    s = (mpsstk) mp_car(chain);
	    
	    if (((sstk_allocs(s) != sstk_frees(s)) ||
		 (sstk_bytes_alloced(s) != sstk_bytes_freed(s)))) {
		 mp_print_leak(file, s);
	    }
	    abytes += sstk_bytes_alloced(s);
	    fbytes += sstk_bytes_freed(s);
	    
	    chain = (mpcell) mp_cdr(chain);
	}
    }

/*
    printf(" a: %d f: %d\n", abytes, fbytes);
*/
    write(file, "-2 -1 -1 -1\n", 12);
}

void
mp_print_leak(file, s)
int	file;
mpsstk	s;
{
    char	outbuf[256];
    int		i;
    
    sprintf(outbuf, "%d %d %d %d\n",
	    sstk_allocs(s), sstk_bytes_alloced(s),
	    sstk_frees(s), sstk_bytes_freed(s));
    write(file, outbuf, strlen(outbuf));
    /*
    */

    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	sprintf(outbuf, "%d\n", sstk_addrs(s)[i]);
	write(file, outbuf, strlen(outbuf));
	/*
	*/
    }
}
    
		  
void
mpleak_init()
{
    int		i;
    
    for (i = 0; i < MP_HASH_SIZE; i++) {
	lt_hmem[i] = (mpcell) MP_NIL;
    }
    mprof_smemC = 0;
}
