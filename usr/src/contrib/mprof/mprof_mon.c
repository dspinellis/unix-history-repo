/*	mprof_mon.c 1.1 9/14/90 11:59:04	*/
/*	Copyright (c) 1987, Benjamin G. Zorn */

/* mprof_mon -- code that is attached to executing programs.
 */

#include	<stdio.h>
#include	<sys/file.h>
#include	"mprof.h"

#ifdef mips
#include <filehdr.h>
#include <syms.h>
#include <ldfcn.h>

int intloc;	/* for use by assembly routines */
#endif

/* local routines */

#ifdef mips
pPDR getpdr();
#endif

void mp_note_alloc();
void mp_note_free();
mpcell mp_note_parent();
void mp_note_leaf();

mpsstk mprof();
void mprof_note_free();
void mprof_startup();
void mprof_writefile();
void mprof_cleanup();

/* local variables */
  
char	*mprof_filename = "mprof.data";
int	mprof_autosave = 0;
int	mprof_file;
bool	mprof_initialized = FALSE;
bool	mprofing = TRUE;
int    	mprof_create_mask = 0644;

extern	int	main();
unsigned	mp_root_address = CRT0_ADDRESS;

int	mprof_bound1 = 32;
int	mprof_bound2 = 256;
int	mprof_bound3 = 2048;

int	mprof_alloc_bins[MP_NUM_BINS];
int	mprof_free_bins[MP_NUM_BINS];


/* mp_zero_bins() -- initialize the bins to zero
 */

void
mp_zero_bins()
{
    int 	i;

    for (i = 0; i < MP_NUM_BINS; i++)
      mprof_alloc_bins[i] = 0;
    for (i = 0; i < MP_NUM_BINS; i++)
      mprof_free_bins[i] = 0;
}


/* mp_inc_bin(bin, size) -- increment the bin of the appropriate size.
 */
   
void
mp_inc_bin(bin, size)
int	bin[];
int	size;
{
    if (size < 0) {
	fprintf(stderr, "mp_inc_bin -- negative size\n");
    } else if (size < MP_NUM_BINS - 2) {
	bin[size]++;
    } else {
	bin[MP_NUM_BINS - 2]++;
	bin[MP_NUM_BINS - 1] += size;
    }
}

/* mp_print_bins(file) -- print the bins to a file
 */

void
mp_print_bins(file)
{
    int 	i;
    char	digits[32];

    for (i = 0; i < MP_NUM_BINS; i++) {
	sprintf(digits, "%d\n", mprof_alloc_bins[i]);
	write(file, digits, strlen(digits));
    }
    for (i = 0; i < MP_NUM_BINS; i++) {
	sprintf(digits, "%d\n", mprof_free_bins[i]);
	write(file, digits, strlen(digits));
    }
}
      
	
/* mp_note_alloc -- note allocation by bin.  There are currently
 * four bins with boundaries that are potentially user setable.
 */

void
mp_note_alloc(d, nbytes)
mpdata		d;
int		nbytes;
{
    if (nbytes <= mprof_bound1) {
	dt_b_small(d)+=nbytes;
	dt_d_small(d)+=nbytes;
	dt_n_small(d)+=1;
    } else if (nbytes <= mprof_bound2) {
	dt_b_med(d)+=nbytes;
	dt_d_med(d)+=nbytes;
	dt_n_med(d)+=1;
    } else if (nbytes <= mprof_bound3) {
	dt_b_large(d)+=nbytes;
	dt_d_large(d)+=nbytes;
	dt_n_large(d)+=1;
    } else {
	dt_b_xlarge(d)+=nbytes;
	dt_d_xlarge(d)+=nbytes;
	dt_n_xlarge(d)+=1;
    }
}

/* mp_note_free -- note when a memory block is released.
 */

void
mp_note_free(d, nbytes)
mpdata		d;
int		nbytes;
{
    if (nbytes <= mprof_bound1) {
	dt_d_small(d)-=nbytes;
    } else if (nbytes <= mprof_bound2) {
	dt_d_med(d)-=nbytes;
    } else if (nbytes <= mprof_bound3) {
	dt_d_large(d)-=nbytes;
    } else {
	dt_d_xlarge(d)-=nbytes;
    }
}

/* mp_note_parent -- record a caller/callee relationship.
 * Allocate a data cell and put the parent on the parents list if
 * necessary.
 */
   
mpcell
mp_note_parent(p, c, nbytes)
mpsym		p, c;
int		nbytes;
{
    mpcell ppair;
    mpdata dcell;

    /* if yes -- is parent already listed?
     */
    ppair = mp_has_parent(c, p);
	
    if (!mp_null(ppair)) {

	/* if yes -- increment count of calls from parent
	 */
	mp_note_alloc((mpdata) mp_cdr(ppair), nbytes);
	    
    } else {

	/* if no -- add this parent to the list of parents
	 */
	dcell = mp_new_data();
	mp_note_alloc(dcell, nbytes);
	ppair = mp_cons((int) p, (mpcell) dcell);
	fn_parents(c) = mp_cons((int) ppair, fn_parents(c));
    }
    
    return ppair;
}

/* mp_note_leaf -- note allocation directly in the function.
 */
   
void
mp_note_leaf(l, nbytes)
mpsym		l;
int		nbytes;
{
    mp_note_alloc(fn_lcount(l), nbytes);
}



#define	MAXCALLS	5000

/* variables to record information about the monitoring
 */
   
int	mprof_cs_maxdepth;
int	mprof_cs_sameC;
int	mprof_cs_allC;

int	mprof_allocC;
int	mprof_freeC;

int	mprof_debug = 0;
int	no_call_graph = 0;


unsigned	pcs1[MAXCALLS];
unsigned	pcs2[MAXCALLS];
unsigned	*fpcs;
mpsym	 	fsyms[MAXCALLS];
mpcell		fpcells[MAXCALLS];
int		fstk_i;
unsigned	*last_fpcs;
int		last_fstk_i;

unsigned	short_callstack[SHORT_CALLSTACK_SIZE];


mpsstk
mprof(nbytes)
int		nbytes;
{
    unsigned	first_local;		/* WARNING -- This MUST be the first
					 * local variable in this function.
					 */
    unsigned	fp;
    unsigned	ret_addr;
    mpsym	child, parent;
    int		i, lasti, j;
    int		lookupcount, samecount;
    unsigned	*pcstmp;
    mpsstk	leakdata;

#ifdef mips
    pPDR pdr;
#endif

    if (!mprof_initialized) {
	mprof_startup();
	mprof_initialized = TRUE;
    }

    mprof_allocC++;
    mp_inc_bin(mprof_alloc_bins, nbytes);
    
    if (mprof_autosave &&
	(mprof_allocC % mprof_autosave) == 0) {
	mprof_writefile();
    }

    fstk_i = 0;

    /* gather return addresses from the callstack
     */
#ifndef mips
    fp = get_current_fp(first_local);
    ret_addr = ret_addr_from_fp(fp);
    
    /* Step back 1 frame (to the caller of malloc)
     */
    fp = prev_fp_from_fp(fp);
    ret_addr = ret_addr_from_fp(fp);
    
    while (ret_addr > mp_root_address) {
	if (no_call_graph && (fstk_i > SHORT_CALLSTACK_SIZE))
	  break;
	
	fpcs[fstk_i] = ret_addr;
	fstk_i++;
	fp = prev_fp_from_fp(fp);
	if (fp == 0) break;
	ret_addr = ret_addr_from_fp(fp);
    }
#else
    get31();
    pdr = getpdr(intloc);
    getsp();
    fp = intloc;
    ret_addr = getretaddr(&fp, pdr);	/* fp is changed */

    /* Step back 1 frame (to the caller of malloc) */
    pdr = getpdr(ret_addr);
    ret_addr = getretaddr(&fp, pdr);	/* fp is changed */

    while (ret_addr > mp_root_address) {
	if (no_call_graph && (fstk_i > SHORT_CALLSTACK_SIZE))
	  break;
	
	fpcs[fstk_i] = ret_addr;
	fstk_i++;
	pdr = getpdr(ret_addr);
	ret_addr = getretaddr(&fp, pdr);	/* fp is updated */
    }
#endif

    /* note last N addresses (short_callstack) for the leak table
     */
    for (i = 0; i < SHORT_CALLSTACK_SIZE; i++) {
	short_callstack[i] = 0;
    }
    for (i = 0; ((i < SHORT_CALLSTACK_SIZE) && (i < fstk_i)); i++) {
	short_callstack[i] = fpcs[i];
    }
    leakdata = mp_add_leak_table(short_callstack, nbytes);
    if (no_call_graph) {
	/* note the direct allocation
	 */
	mp_note_leaf(pc_lookup(fpcs[0]), nbytes);
	return leakdata;
    }

    /* note maximum stack depth
     */
    if (fstk_i > mprof_cs_maxdepth)
      mprof_cs_maxdepth = fstk_i;

    /* determine the overlap with the last callstack
     */
    i = fstk_i - 1;
    lasti = last_fstk_i - 1;
    while ((lasti > 0) &&
	   (i > 0) &&
	   (fpcs[i] == last_fpcs[lasti])) {
	i--;
	lasti--;
    }

    /* i is the index of the first difference of pc's in the stack
     * i+1 is the number of pc's that need to be looked up
     */
    lookupcount = i + 1;
    
    /* put the new calls in the stack of functions
     */
    if (lookupcount != 0) {
	for (j = fstk_i - lookupcount; j < fstk_i; j++) {
	    fsyms[j] = pc_lookup(fpcs[fstk_i - j - 1]);
	}
    }

    samecount = fstk_i - lookupcount;

    mprof_cs_sameC += samecount;
    mprof_cs_allC += fstk_i;

    /* record the parent/child relations
     */

    i = 0;

    for (i = 0; i < (fstk_i - 1); i++) {
	parent = fsyms[i];
	child = fsyms[i+1];
	if (mprof_debug)
	  printf("%d -> ", fn_addr(parent));
	if (i < (samecount - 1)) {
	    mp_note_alloc((mpdata) mp_cdr(fpcells[i+1]), nbytes);
	} else {
	    fpcells[i+1] = mp_note_parent(parent, child, nbytes);
	}
    }
    if (mprof_debug)
      printf("%d\n", fn_addr(child));
    mp_note_leaf(fsyms[(fstk_i - 1)], nbytes);

    /* swap the last pc stack with the current one
     */
    pcstmp = fpcs;
    fpcs = last_fpcs;
    last_fpcs = pcstmp;
    last_fstk_i = fstk_i;
    return leakdata;
}

void
mprof_note_free(leakdata, nbytes)
mpsstk		leakdata;
int		nbytes;
{
    mpsym	f;
    unsigned	addr;
    
    addr = leakdata->sstack[0];

    mprof_freeC++;
    mp_inc_bin(mprof_free_bins, nbytes);
    
    f = pc_lookup(addr);
    mp_note_free(fn_lcount(f), nbytes);

    mp_remove_leak_table(leakdata, nbytes);
}

void
mprof_startup()
{
#ifdef sun
    on_exit(mprof_exit, NULL);
#endif    
    if (strcmp(mprof_filename, "") == 0) {
	mprof_file = 1;
    } else {
	mprof_file = open(mprof_filename,
			  (O_WRONLY | O_CREAT | O_TRUNC),
			  mprof_create_mask);
    }
    mpstruct_init();
    mp_zero_bins();
    mpleak_init();

    mprof_cs_maxdepth = 0;
    mprof_cs_sameC = 0;
    mprof_cs_allC = 0;

    mprof_allocC = 0;
    mprof_freeC = 0;

    last_fstk_i = 0;
    fpcs = pcs1;
    last_fpcs = pcs2;

#ifdef mips
    pdrinit();
#endif

}

void
mprof_writefile()
{
    char	stats[256];
    extern	int mprof_fmemC, mprof_dmemC, mprof_lmemC, mprof_smemC;

    ftruncate(mprof_file, 0);
    lseek(mprof_file, 0L, 0);
    
    sprintf(stats, "alloc=%d free=%d depth=%d same=%d all=%d\n",
	    mprof_allocC,
	    mprof_freeC,
	    mprof_cs_maxdepth,
	    mprof_cs_sameC,
	    mprof_cs_allC);
    write(mprof_file, stats, strlen(stats));

    sprintf(stats, "fmem=%d dmem=%d lmem=%d smem=%d\n",
	    (mprof_fmemC * MPSYM_SIZE) / 1024,
	    (mprof_dmemC * MPDATA_SIZE) / 1024,
	    (mprof_lmemC * MPCELL_SIZE) / 1024,
	    (mprof_smemC * MPSSTK_SIZE) / 1024);
    
    write(mprof_file, stats, strlen(stats));

    mp_print_bins(mprof_file);

    mp_print_leak_table(mprof_file);
    
    mprof_print(mprof_file);
}

void
mprof_cleanup()
{
    if (mprof_initialized) {
    	mprof_writefile();
    	close(mprof_file);
    }
}


/* external interface --

   void
   set_mprof_autosave(count)	-- set the autosave count of profile data
   int count;			   count = 0 (default)  implies no autosave

   void
   mprof_stop()			-- stop the memory profile in progress

   void
   mprof_restart(datafile)	-- restart memory profiling

*/
   
void
set_mprof_autosave(count)
int	count;
{
    mprof_autosave = count;
}
      

void
mprof_restart(datafile)
char	*datafile;
{
    if (mprofing)
	fprintf(stderr,
	"mprof_restart -- restart ingnored; memory profiling in progress\n");
    else {
	mprof_initialized = FALSE;
	mprofing = TRUE;
	mprof_filename = datafile;
    }
}

void
mprof_stop()
{
    if (!mprofing) 
	fprintf(stderr,
	"mprof_stop -- stop ingnored; memory profiling not in progress\n");
    else {
	mprof_cleanup();
	mprofing = FALSE;
    }
}

#ifdef mips

pPDR pdrarray;
LDFILE *ldptr;

pdrinit()
{
	int i;
	SYMR asym;
	extern char **__Argv;	/* hack */
	pFDR pfd;

        ldptr = ldopen(__Argv[0], NULL);	/* hack */
        pdrarray = (pPDR) malloc (sizeof(PDR) * SYMHEADER(ldptr).ipdMax);

#ifdef notdef	/* doesn't work for libraries compiled -O */
        /* read in pdr table */
        for (i = 0; i < SYMHEADER(ldptr).ipdMax - 1; i++) {
          if (ldgetpd(ldptr, i, &pdrarray[i]) != SUCCESS) {
	    printf("bad pdr %d\n", i);
	    i--;
	    continue;
	  }
        }
#endif

	/* indirectly read in pdr table through the file descriptor table */
	for (pfd = PFD(ldptr); pfd < PFD(ldptr) + SYMHEADER(ldptr).ifdMax;
	    pfd++) {
	  for (i = pfd->ipdFirst; i < pfd->ipdFirst + pfd->cpd; i++) {
	    if (ldgetpd (ldptr, i, &pdrarray[i]) != SUCCESS) {
	      fprintf(stderr, "can't read pdr %d\n", i);
	      exit (1);
	    }
	    if (pdrarray[i].isym != isymNil) {
	      if (ldtbread(ldptr, pfd->csym ? pdrarray[i].isym :
		  pdrarray[i].isym + SYMHEADER(ldptr).isymMax, &asym)
		  != SUCCESS) {
	        fprintf(stderr, "can't read symbol");
	        exit (1);
	      }
	      pdrarray[i].adr = asym.value;
	    }
	  }
	}

	/* This is guaranteed to be between __start and main. */
	mp_root_address = pdrarray[1].adr - 1;
}

pPDR
getpdr(loc)
int loc;
{
	int low = 0, high = SYMHEADER(ldptr).ipdMax - 1, mid;

	/* do binary search on address */
	while (low <= high) {
	  mid = (low + high) / 2;
	  if (loc < pdrarray[mid].adr) {
	    high = mid - 1;
	  } else if (loc > pdrarray[mid].adr) {
	    low = mid + 1;
	  } else {
	    return (&pdrarray[mid]);
	  }
	}

	return (&pdrarray[low - 1]);

}

getretaddr(pfp, pdr)
int *pfp;
pPDR pdr;
{
	int fp = *pfp;
	int retaddr;
	int saved31loc;

	/* return return address and update fp
	   1.  I am told what my current fp and pdr is
	   2.  see what the return register is
	   3.  see if return reg is on stack
	   4.  add the framesize to framereg to get the virtual fp
	   5.  add the frameoffset to fp to get to the save register area
	   6.  read the stack to get the return address
	*/

	if (pdr->pcreg < 0) {
	  punt("return addreses not in a saved register");
	}

	if (!(pdr->regmask & (1 << pdr->pcreg))) {
	  /* in a register and register is not saved */
	  punt("don't know how to get register");
	}

	if (pdr->framereg != 29) punt("framereg != 29");
	fp += pdr->frameoffset;

	saved31loc = fp + pdr->regoffset;

	/* assume pcreg is 31, else have to figure out where it is in saved
		area */
	if (pdr->pcreg != 31) punt("return reg not 31");

	retaddr = *(int *) saved31loc;

	*pfp = fp;
	return (retaddr);

}

punt(str)
char *str;
{
	fprintf(stderr, "%s\n", str);
	exit(1);
}
#endif
