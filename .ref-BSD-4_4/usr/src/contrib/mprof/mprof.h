/*	mprof.h 2.5 9/14/90 16:01:19	*/
/*	Copyright (c) 1987, Benjamin G. Zorn */

typedef	int	bool;
#define	TRUE	1
#define	FALSE	0

#define	MP_NUM_BINS	1025
#define	MP_HASH_SIZE		8009
#define	SHORT_CALLSTACK_SIZE	5



typedef struct	mp_data_struct {
    int		d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12;
} mpdata_str, *mpdata;

#define	MPDATA_SIZE	(sizeof(mpdata_str))

#define dt_b_small(x)	((x)->d1)
#define dt_b_med(x)	((x)->d2)
#define dt_b_large(x)	((x)->d3)
#define dt_b_xlarge(x)	((x)->d4)
#define dt_n_small(x)	((x)->d5)
#define dt_n_med(x)	((x)->d6)
#define dt_n_large(x)	((x)->d7)
#define dt_n_xlarge(x)	((x)->d8)
#define dt_d_small(x)	((x)->d9)
#define dt_d_med(x)	((x)->d10)
#define dt_d_large(x)	((x)->d11)
#define dt_d_xlarge(x)	((x)->d12)


  
typedef struct	mp_cons_struct {
    int				mpcar;
    struct mp_cons_struct	*mpcdr;
} mpcell_str, *mpcell;

#define	MPCELL_SIZE	(sizeof(mpcell_str))

#define	MP_NIL		-1
#define	mp_car(i)	((i)->mpcar)
#define	mp_cdr(i)	((i)->mpcdr)
#define	mp_null(i)	(((int) i) == MP_NIL)


  
typedef struct mp_function_struct {
    mpdata	lcount;
    unsigned	addr;
    char	*name;
    mpcell	parents;
} mpsym_str, *mpsym;

#define	MPSYM_SIZE	(sizeof(mpsym_str))

#define	fn_addr(fn)	((fn)->addr)
#define	fn_parents(fn)	((fn)->parents)
#define	fn_lcount(fn)	((fn)->lcount)
#define fn_name(fn)	((fn)->name)



typedef struct mp_sstk_struct {
    unsigned	sstack[SHORT_CALLSTACK_SIZE];
    int		allocs, bytes_alloced;
    int		frees, bytes_freed;
} mpsstk_str, *mpsstk;

#define	MPSSTK_SIZE	(sizeof(mpsstk_str))

#define	sstk_addrs(fn)		((fn)->sstack)
#define	sstk_allocs(fn)		((fn)->allocs)
#define	sstk_bytes_alloced(fn)	((fn)->bytes_alloced)
#define	sstk_frees(fn)		((fn)->frees)
#define	sstk_bytes_freed(fn)	((fn)->bytes_freed)

extern	void	mprof_exit();

extern	char	*malloc();
extern	mpcell	mp_cons();
extern	mpsym	mp_new_fn();
extern	mpdata	mp_new_data();
extern	mpdata	mp_add_data();
extern	int	mp_sum_data();
extern	int	mp_sum_calls();
extern	int	mp_sum_kept();
extern	int	mp_hash();
extern	void	mp_puthash();
extern	mpsym	mp_lookup();
extern	void	mp_note_alloc();
extern	mpcell	mp_note_parent();
extern	void	mp_note_leaf();
extern	void	mp_print_addr();
extern	void	mp_print_data();
extern	char	*mp_sprint_data();
extern	void	mp_print_fn();
extern	void	mp_print_parents();
extern	mpcell	mp_has_parent();
extern	void	mprof_print();
extern	void	mpstruct_init();
extern	mpsym	pc_lookup();

extern mpsstk mp_add_leak_table();
extern void mp_remove_leak_table();
extern void mp_print_leak_table();
extern void mpleak_init();

extern	char	*strdup();

#if (defined(vax) || (defined(sun) && !defined(sun4)))
#define get_current_fp(first_local) ((unsigned)&(first_local) + 4)
#endif

#if (defined(vax) || defined(sun))
#include <sys/types.h>
#include <frame.h>
#define prev_fp_from_fp(fp)	(unsigned)(((struct frame *)(fp))->fr_savfp)
#define ret_addr_from_fp(fp)	(unsigned)(((struct frame *)(fp))->fr_savpc)
#endif

  
/* for ultrix 0x38, 4.3 bsd 0x3d, other?
*/
  
#ifdef tahoe
#define	CRT0_ADDRESS		0x3d
#endif
  
#ifdef vax
#define	CRT0_ADDRESS		0x3d
#endif
  
#ifdef sun
#define	CRT0_ADDRESS		0x204c
#endif

#ifdef mips
#define CRT0_ADDRESS		0x0  /* to be filled in later */
#endif
