# 1 "genattr.c"
 




















# 1 "/usr/include/stdio.h" 1
 









































# 1 "/usr/include/sys/types.h" 1
 





































 
# 1 "/usr/include/machine/endian.h" 1
 


































 






 









# 1 "/usr/include/sys/cdefs.h" 1
 













































 











# 76 "/usr/include/sys/cdefs.h"


 
















# 53 "/usr/include/machine/endian.h" 2


 
unsigned long	htonl (unsigned long)		;
unsigned short	htons (unsigned short)		;
unsigned long	ntohl (unsigned long)		;
unsigned short	ntohs (unsigned short)		;
 

 





















# 40 "/usr/include/sys/types.h" 2


typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		 

typedef	unsigned long long u_quad_t;
typedef	long long quad_t;
typedef	quad_t * qaddr_t;
typedef	char *	caddr_t;		 
typedef	long	daddr_t;		 
typedef	u_long	dev_t;			 
typedef	u_long	ino_t;			 
typedef	quad_t	off_t;			 
typedef	u_short	nlink_t;		 
typedef	long	swblk_t;		 
typedef	long	segsz_t;		 
typedef	u_long	uid_t;			 
typedef	u_long	gid_t;			 
typedef	short	pid_t;			 
typedef	u_short	mode_t;			 
typedef u_long	fixpt_t;		 







# 1 "/usr/include/machine/ansi.h" 1
 





































 



















# 71 "/usr/include/sys/types.h" 2


# 1 "/usr/include/machine/types.h" 1
 





































typedef struct _physadr {
	short r[1];
} *physadr;

typedef struct label_t {			 
	int val[15];
} label_t;

typedef	u_long	vm_offset_t;
typedef	u_long	vm_size_t;


typedef	char			s1byte_t;	 
typedef	unsigned char		u1byte_t;
typedef	short			s2byte_t;
typedef	unsigned short		u2byte_t;
typedef	long			s4byte_t;
typedef	unsigned long		u4byte_t;
typedef	long long		s8byte_t;
typedef	unsigned long long	u8byte_t;
typedef	float			f4byte_t;
typedef	double			f8byte_t;



# 73 "/usr/include/sys/types.h" 2




typedef	unsigned long			clock_t;




typedef	unsigned int			size_t;




typedef	int				ssize_t;




typedef	long				time_t;






 









typedef long	fd_mask;






typedef	struct fd_set {
	fd_mask	fds_bits[(((256 )+(( (sizeof(fd_mask) * 8		)	)-1))/( (sizeof(fd_mask) * 8		)	)) ];
} fd_set;







# 140 "/usr/include/sys/types.h"




# 43 "/usr/include/stdio.h" 2



# 1 "/usr/include/sys/cdefs.h" 1
 


































# 94 "/usr/include/sys/cdefs.h"

# 46 "/usr/include/stdio.h" 2


# 1 "/usr/include/machine/ansi.h" 1
 


































# 58 "/usr/include/machine/ansi.h"

# 48 "/usr/include/stdio.h" 2










 





typedef off_t fpos_t;








 





 
struct __sbuf {
	unsigned char *_base;
	int	_size;
};

 























typedef	struct __sFILE {
	unsigned char *_p;	 
	int	_r;		 
	int	_w;		 
	short	_flags;		 
	short	_file;		 
	struct	__sbuf _bf;	 
	int	_lbfsize;	 

	 
	void	*_cookie;	 
	int	(*_close) (void *)		;
	int	(*_read)  (void *, char *, int)		;
	fpos_t	(*_seek)  (void *, fpos_t, int)		;
	int	(*_write) (void *, const char *, int)		;

	 
	struct	__sbuf _ub;	 
	unsigned char *_up;	 
	int	_ur;		 

	 
	unsigned char _ubuf[3];	 
	unsigned char _nbuf[1];	 

	 
	struct	__sbuf _lb;	 

	 
	int	_blksize;	 
	int	_offset;	 
} FILE;

 
extern FILE __sF[];
 





	 











 















 







 




















 


 
void	 clearerr (FILE *)		;
int	 fclose (FILE *)		;
int	 feof (FILE *)		;
int	 ferror (FILE *)		;
int	 fflush (FILE *)		;
int	 fgetc (FILE *)		;
int	 fgetpos (FILE *, fpos_t *)		;
char	*fgets (char *, size_t, FILE *)		;
FILE	*fopen (const char *, const char *)		;
int	 fprintf (FILE *, const char *, ...)		;
int	 fputc (int, FILE *)		;
int	 fputs (const char *, FILE *)		;
size_t	 fread (void *, size_t, size_t, FILE *)		;
FILE	*freopen (const char *, const char *, FILE *)		;
int	 fscanf (FILE *, const char *, ...)		;
int	 fseek (FILE *, long, int)		;
int	 fsetpos (FILE *, const fpos_t *)		;
long	 ftell (const FILE *)		;
size_t	 fwrite (const void *, size_t, size_t, FILE *)		;
int	 getc (FILE *)		;
int	 getchar (void)		;
char	*gets (char *)		;

extern int sys_nerr;			 
extern char *sys_errlist[];

void	 perror (const char *)		;
int	 printf (const char *, ...)		;
int	 putc (int, FILE *)		;
int	 putchar (int)		;
int	 puts (const char *)		;
int	 remove (const char *)		;
int	 rename  (const char *, const char *)		;
void	 rewind (FILE *)		;
int	 scanf (const char *, ...)		;
void	 setbuf (FILE *, char *)		;
int	 setvbuf (FILE *, char *, int, size_t)		;
int	 sprintf (char *, const char *, ...)		;
int	 sscanf (const char *, const char *, ...)		;
FILE	*tmpfile (void)		;
char	*tmpnam (char *)		;
int	 ungetc (int, FILE *)		;
int	 vfprintf (FILE *, const char *, char *			)		;
int	 vprintf (const char *, char *			)		;
int	 vsprintf (char *, const char *, char *			)		;
 

 






 
char	*ctermid (char *)		;
FILE	*fdopen (int, const char *)		;
int	 fileno (FILE *)		;
 


 



 
char	*fgetline (FILE *, size_t *)		;
int	 fpurge (FILE *)		;
int	 getw (FILE *)		;
int	 pclose (FILE *)		;
FILE	*popen (const char *, const char *)		;
int	 putw (int, FILE *)		;
void	 setbuffer (FILE *, char *, int)		;
int	 setlinebuf (FILE *)		;
char	*tempnam (const char *, const char *)		;
int	 snprintf (char *, size_t, const char *, ...)		;
int	 vsnprintf (char *, size_t, const char *, char *			)		;
int	 vscanf (const char *, char *			)		;
int	 vsscanf (const char *, const char *, char *			)		;
FILE	*zopen (const char *, const char *, int)		;
 

 






 


 
FILE	*funopen (const void *,
		int (*)(void *, char *, int),
		int (*)(void *, const char *, int),
		fpos_t (*)(void *, fpos_t, int),
		int (*)(void *))		;
 




 


 
int	__srget (FILE *)		;
int	__svfscanf (FILE *, const char *, char *			)		;
int	__swbuf (int, FILE *)		;
 

 





static __inline int __sputc(int _c, FILE *_p) {
	if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
		return (*_p->_p++ = _c);
	else
		return (__swbuf(_c, _p));
}
# 347 "/usr/include/stdio.h"























# 22 "genattr.c" 2

# 1 "hconfig.h" 1
 



















 



 








 

# 1 "tm.h" 1


 



 




 



 



 



 



 



 




 



# 36 "hconfig.h" 2


 



 

 



# 23 "genattr.c" 2

# 1 "rtl.h" 1
 



















# 1 "machmode.h" 1
 





















 












 













 







 





 



enum machine_mode {
# 1 "machmode.def" 1
 




















 










 



























 

 VOIDmode, 

 QImode, 		 
 HImode, 
 


 PSImode, 
 SImode, 
 PDImode, 
 DImode, 
 TImode, 

 SFmode, 
 DFmode, 
 XFmode,     
 TFmode, 

 SCmode, 
 DCmode, 
 XCmode, 
 TCmode, 

 

 BLKmode, 

 





 CCmode, 

 


 





# 69 "machmode.h" 2





MAX_MACHINE_MODE };









 

extern char *mode_name[];


enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
		  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT, MAX_MODE_CLASS};

 


extern enum mode_class mode_class[];


 

extern int mode_size[];


 

extern int mode_unit_size[];


 





 



 






 

extern enum machine_mode mode_wider_mode[];


 



extern enum machine_mode mode_for_size ();

 

extern enum machine_mode get_best_mode ();

 





 

extern enum machine_mode class_narrowest_mode[];


 


extern enum machine_mode byte_mode;
extern enum machine_mode word_mode;


# 21 "rtl.h" 2





 


enum rtx_code  {


# 1 "rtl.def" 1
 





















 




































 




 
  UNKNOWN , 

 

  NIL , 

 



 
  EXPR_LIST , 

 

  INSN_LIST , 

 




 































  MATCH_OPERAND , 

 




  MATCH_SCRATCH , 

 


  MATCH_DUP , 

 





  MATCH_OPERATOR , 

 






  MATCH_PARALLEL , 

 


  MATCH_OP_DUP , 

 


  MATCH_PAR_DUP , 

 
















  DEFINE_INSN , 

 





  DEFINE_PEEPHOLE , 

 







  DEFINE_SPLIT , 

 

  DEFINE_COMBINE , 

 









  DEFINE_EXPAND , 
   
 













  DEFINE_DELAY , 

 






































  DEFINE_FUNCTION_UNIT , 

 
  DEFINE_ASM_ATTRIBUTES , 

 



  SEQUENCE , 

 



  ADDRESS , 

 




 



  DEFINE_ATTR , 

 
  ATTR , 

 





  SET_ATTR , 

 












  SET_ATTR_ALTERNATIVE , 

 


  EQ_ATTR , 

 









 
  INSN , 

 

  JUMP_INSN , 

 



  CALL_INSN , 

 
  BARRIER , 

 





  CODE_LABEL , 
     
 







  NOTE , 

 



  INLINE_HEADER , 

 


   
 
  PARALLEL , 

 





  ASM_INPUT , 

 











  ASM_OPERANDS , 

 








  UNSPEC , 

 
  UNSPEC_VOLATILE , 

 
 
  ADDR_VEC , 

 



  ADDR_DIFF_VEC , 

 



 





  SET , 

 


  USE , 

 


  CLOBBER , 

 



  CALL , 

 

  RETURN , 

 



  TRAP_IF , 

 



 
  CONST_INT , 

 






  CONST_DOUBLE , 

 
  CONST_STRING , 

 



  CONST , 

 

  PC , 

 


  REG , 

 




  SCRATCH , 

 







  SUBREG , 

 










  STRICT_LOW_PART , 

 

  MEM , 

 



  LABEL_REF , 

 



  SYMBOL_REF , 

 




  CC0 , 

 















  QUEUED , 

 



 





  IF_THEN_ELSE , 

 






  COND , 

 
  COMPARE , 

 
  PLUS , 

 
  MINUS , 

 
  NEG , 

  MULT , 

 
  DIV , 
 
  MOD , 

 
  UDIV , 
  UMOD , 

 
  AND , 

  IOR , 

  XOR , 

  NOT , 

 




  LSHIFT , 
  ASHIFT , 
  ROTATE , 

 


  ASHIFTRT , 
  LSHIFTRT , 
  ROTATERT , 

 



  SMIN , 
  SMAX , 
  UMIN , 
  UMAX , 

 






  PRE_DEC , 
  PRE_INC , 
  POST_DEC , 
  POST_INC , 

 

  NE , 
  EQ , 
  GE , 
  GT , 
  LE , 
  LT , 
  GEU , 
  GTU , 
  LEU , 
  LTU , 

 


  SIGN_EXTEND , 

 
  ZERO_EXTEND , 

 
  TRUNCATE , 

 
  FLOAT_EXTEND , 
  FLOAT_TRUNCATE , 

 
  FLOAT , 

 





  FIX , 

 
  UNSIGNED_FLOAT , 

 


  UNSIGNED_FIX , 

 
  ABS , 

 
  SQRT , 

 


  FFS , 

 








  SIGN_EXTRACT , 

 
  ZERO_EXTRACT , 

 

 
  HIGH , 

 

  LO_SUM , 

 





# 32 "rtl.h" 2



  LAST_AND_UNUSED_RTX_CODE};	 




				 

extern int rtx_length[];


extern char *rtx_name[];


extern char *rtx_format[];


extern char rtx_class[];


 

typedef union rtunion_def
{
  int  rtwint;
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
} rtunion;

 

typedef struct rtx_def
{







   
  enum rtx_code code : 16;

   



  enum machine_mode mode : 8;

   

  unsigned int jump : 1;
   
  unsigned int call : 1;
   








  unsigned int unchanging : 1;
   






  unsigned int volatil : 1;
   














  unsigned int in_struct : 1;
   





  unsigned int used : 1;
   


  unsigned integrated : 1;
   


  rtunion fld[1];
} *rtx;

 










 

















 















 



typedef struct rtvec_def{
  unsigned num_elem;		 
  rtunion elem[1];
} *rtvec;








 



 






 









 

 



 



 


 



 






 


 


 



 





 





















































 
enum reg_note { REG_DEAD = 1, REG_INC = 2, REG_EQUIV = 3, REG_WAS_0 = 4,
		REG_EQUAL = 5, REG_RETVAL = 6, REG_LIBCALL = 7,
		REG_NONNEG = 8, REG_NO_CONFLICT = 9, REG_UNUSED = 10,
		REG_CC_SETTER = 11, REG_CC_USER = 12, REG_LABEL = 13,
		REG_DEP_ANTI = 14, REG_DEP_OUTPUT = 15 };

 



 

extern char *reg_note_name[];


 






 








 



 


 



 






 





 

 

 

 

 

 

 










 

extern char *note_insn_name[];


 



 



 




 





 





 




 



 




 


 



 





 










 













 



 


 



 
 


 


 


 



 









 




 


 


 


 


 




































 



 











 


 






 


 




 

 













extern rtx plus_constant_wide		 (rtx, int ) ;
extern rtx plus_constant_for_output_wide (rtx, int ) ;



# 631 "rtl.h"

extern rtx gen_rtx ();
extern rtvec gen_rtvec ();



extern rtx read_rtx			(FILE *) ;










extern char *xrealloc ();


extern char *xmalloc			(unsigned) ;
extern char *oballoc			(int) ;
extern char *permalloc			(int) ;
extern void free			(void *) ;
extern rtx rtx_alloc			(enum rtx_code ) ;
extern rtvec rtvec_alloc		(int) ;
extern rtx find_reg_note		(rtx, enum reg_note, rtx) ;
extern rtx find_regno_note		(rtx, enum reg_note, int) ;
extern int  get_integer_term	(rtx) ;
extern rtx get_related_value		(rtx) ;
extern rtx single_set			(rtx) ;
extern rtx find_last_value		(rtx, rtx *, rtx) ;
extern rtx copy_rtx			(rtx) ;
extern rtx copy_rtx_if_shared		(rtx) ;
extern rtx copy_most_rtx		(rtx, rtx) ;
extern rtx replace_rtx			(rtx, rtx, rtx) ;
extern rtvec gen_rtvec_v		(int, rtx *) ;
extern rtx gen_reg_rtx			(enum machine_mode) ;
extern rtx gen_label_rtx		(void) ;
extern rtx gen_inline_header_rtx	(rtx, rtx, int, int, int, int, int, int, rtx, int, int, rtvec, rtx) ;
extern rtx gen_lowpart_common		(enum machine_mode, rtx) ;
extern rtx gen_lowpart			(enum machine_mode, rtx) ;
extern rtx gen_lowpart_if_possible	(enum machine_mode, rtx) ;
extern rtx gen_highpart			(enum machine_mode, rtx) ;
extern rtx gen_realpart			(enum machine_mode, rtx) ;
extern rtx gen_imagpart			(enum machine_mode, rtx) ;
extern rtx operand_subword		(rtx, int, int, enum machine_mode) ;
extern rtx operand_subword_force	(rtx, int, enum machine_mode) ;
extern int subreg_lowpart_p		(rtx) ;
extern rtx make_safe_from		(rtx, rtx) ;
extern rtx memory_address		(enum machine_mode, rtx) ;
extern rtx get_insns			(void) ;
extern rtx get_last_insn		(void) ;
extern rtx get_last_insn_anywhere	(void) ;
extern void start_sequence		(void) ;
extern void push_to_sequence		(rtx) ;
extern void end_sequence		(void) ;
extern rtx gen_sequence			(void) ;
extern rtx immed_double_const		(int , int , enum machine_mode) ;
extern rtx force_const_mem		(enum machine_mode, rtx) ;
extern rtx force_reg			(enum machine_mode, rtx) ;
extern rtx get_pool_constant		(rtx) ;
extern enum machine_mode get_pool_mode	(rtx) ;
extern int get_pool_offset		(rtx) ;
extern rtx simplify_subtraction		(rtx) ;
extern rtx assign_stack_local		(enum machine_mode, int, int) ;
extern rtx assign_stack_temp		(enum machine_mode, int, int) ;
extern rtx protect_from_queue		(rtx, int) ;
extern void emit_queue			(void) ;
extern rtx emit_move_insn		(rtx, rtx) ;
extern rtx emit_insn_before		(rtx, rtx) ;
extern rtx emit_jump_insn_before	(rtx, rtx) ;
extern rtx emit_call_insn_before	(rtx, rtx) ;
extern rtx emit_barrier_before		(rtx) ;
extern rtx emit_note_before		(int, rtx) ;
extern rtx emit_insn_after		(rtx, rtx) ;
extern rtx emit_jump_insn_after		(rtx, rtx) ;
extern rtx emit_barrier_after		(rtx) ;
extern rtx emit_label_after		(rtx, rtx) ;
extern rtx emit_note_after		(int, rtx) ;
extern rtx emit_line_note_after		(char *, int, rtx) ;
extern rtx emit_insn			(rtx) ;
extern rtx emit_insns			(rtx) ;
extern rtx emit_insns_before		(rtx, rtx) ;
extern rtx emit_jump_insn		(rtx) ;
extern rtx emit_call_insn		(rtx) ;
extern rtx emit_label			(rtx) ;
extern rtx emit_barrier			(void) ;
extern rtx emit_line_note		(char *, int) ;
extern rtx emit_note			(char *, int) ;
extern rtx emit_line_note_force		(char *, int) ;
extern rtx make_insn_raw		(rtx) ;
extern rtx previous_insn		(rtx) ;
extern rtx next_insn			(rtx) ;
extern rtx prev_nonnote_insn		(rtx) ;
extern rtx next_nonnote_insn		(rtx) ;
extern rtx prev_real_insn		(rtx) ;
extern rtx next_real_insn		(rtx) ;
extern rtx prev_active_insn		(rtx) ;
extern rtx next_active_insn		(rtx) ;
extern rtx prev_label			(rtx) ;
extern rtx next_label			(rtx) ;
extern rtx next_cc0_user		(rtx) ;
extern rtx prev_cc0_setter		(rtx) ;
extern rtx reg_set_last			(rtx, rtx) ;
extern rtx next_nondeleted_insn		(rtx) ;
extern enum rtx_code reverse_condition	(enum rtx_code) ;
extern enum rtx_code swap_condition	(enum rtx_code) ;
extern enum rtx_code unsigned_condition	(enum rtx_code) ;
extern enum rtx_code signed_condition	(enum rtx_code) ;
extern rtx find_equiv_reg		(rtx, rtx, enum reg_class, int, short *, int, enum machine_mode) ;
extern rtx squeeze_notes		(rtx, rtx) ;
extern rtx delete_insn			(rtx) ;
extern void delete_jump			(rtx) ;
extern rtx get_label_before		(rtx) ;
extern rtx get_label_after		(rtx) ;
extern rtx follow_jumps			(rtx) ;
extern rtx adj_offsettable_operand	(rtx, int) ;
extern rtx try_split			(rtx, rtx, int) ;
extern rtx split_insns			(rtx, rtx) ;
extern rtx simplify_unary_operation	(enum rtx_code, enum machine_mode, rtx, enum machine_mode) ;
extern rtx simplify_binary_operation	(enum rtx_code, enum machine_mode, rtx, rtx) ;
extern rtx simplify_ternary_operation	(enum rtx_code, enum machine_mode, enum machine_mode, rtx, rtx, rtx) ;
extern rtx simplify_relational_operation (enum rtx_code, enum machine_mode, rtx, rtx) ;
extern rtx nonlocal_label_rtx_list	(void) ;
extern rtx gen_move_insn		(rtx, rtx) ;
extern rtx gen_jump			(rtx) ;
extern rtx gen_beq			(rtx) ;
extern rtx gen_bge			(rtx) ;
extern rtx gen_ble			(rtx) ;
extern rtx eliminate_constant_term	(rtx, rtx *) ;
extern rtx expand_complex_abs		(enum machine_mode, rtx, rtx, int) ;

 



extern int max_parallel;

extern int asm_noperands		(rtx) ;
extern char *decode_asm_operands	(rtx, rtx *, rtx **, char **, enum machine_mode *) ;

extern enum reg_class reg_preferred_class (int) ;
extern enum reg_class reg_alternate_class (int) ;

extern rtx get_first_nonparm_insn	(void) ;

 
extern rtx pc_rtx;
extern rtx cc0_rtx;
extern rtx const0_rtx;
extern rtx const1_rtx;
extern rtx const2_rtx;
extern rtx constm1_rtx;
extern rtx const_true_rtx;

extern rtx const_tiny_rtx[3][(int) MAX_MACHINE_MODE];

 




 




 


extern rtx stack_pointer_rtx;
extern rtx frame_pointer_rtx;
extern rtx arg_pointer_rtx;
extern rtx pic_offset_table_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;

 







 



extern rtx virtual_incoming_args_rtx;



 



extern rtx virtual_stack_vars_rtx;



 



extern rtx virtual_stack_dynamic_rtx;



 



extern rtx virtual_outgoing_args_rtx;





extern rtx find_next_ref		(rtx, rtx) ;
extern rtx *find_single_use		(rtx, rtx, rtx *) ;

 


extern rtx expand_expr ();
extern rtx immed_real_const_1();









extern rtx output_constant_def ();
extern rtx immed_real_const ();
extern rtx immed_real_const_1 ();


 





 


extern int reload_completed;

 


extern int reload_in_progress;

 





extern int cse_not_expected;

 

extern rtx *regno_reg_rtx;
# 24 "genattr.c" 2

# 1 "obstack.h" 1
 
















 


















































































 




 











 




# 133 "obstack.h"

# 1 "/usr/include/stddef.h" 1
 





































# 1 "/usr/include/machine/ansi.h" 1
 


































# 58 "/usr/include/machine/ansi.h"

# 39 "/usr/include/stddef.h" 2


typedef	int				ptrdiff_t;







typedef	unsigned short			wchar_t;










# 134 "obstack.h" 2










struct _obstack_chunk		 
{
  char  *limit;			 
  struct _obstack_chunk *prev;	 
  char	contents[4];		 
};

struct obstack		 
{
  long	chunk_size;		 
  struct _obstack_chunk* chunk;	 
  char	*object_base;		 
  char	*next_free;		 
  char	*chunk_limit;		 
  ptrdiff_t  temp;		 
  int   alignment_mask;		 
  struct _obstack_chunk *(*chunkfun) ();  
  void (*freefun) ();		 
  char *extra_arg;		 
  unsigned use_extra_arg:1;	 
  unsigned maybe_empty_object:1; 



};

 


extern void _obstack_newchunk (struct obstack *, int);
extern void _obstack_free (struct obstack *, void *);
extern void _obstack_begin (struct obstack *, int, int,
			    void *(*) (), void (*) ());
extern void _obstack_begin_1 (struct obstack *, int, int,
			      void *(*) (), void (*) (), void *);









 


void obstack_init (struct obstack *obstack);

void * obstack_alloc (struct obstack *obstack, int size);

void * obstack_copy (struct obstack *obstack, void *address, int size);
void * obstack_copy0 (struct obstack *obstack, void *address, int size);

void obstack_free (struct obstack *obstack, void *block);

void obstack_blank (struct obstack *obstack, int size);

void obstack_grow (struct obstack *obstack, void *data, int size);
void obstack_grow0 (struct obstack *obstack, void *data, int size);

void obstack_1grow (struct obstack *obstack, int data_char);
void obstack_ptr_grow (struct obstack *obstack, void *data);
void obstack_int_grow (struct obstack *obstack, int data);

void * obstack_finish (struct obstack *obstack);

int obstack_object_size (struct obstack *obstack);

int obstack_room (struct obstack *obstack);
void obstack_1grow_fast (struct obstack *obstack, int data_char);
void obstack_ptr_grow_fast (struct obstack *obstack, void *data);
void obstack_int_grow_fast (struct obstack *obstack, int data);
void obstack_blank_fast (struct obstack *obstack, int size);

void * obstack_base (struct obstack *obstack);
void * obstack_next_free (struct obstack *obstack);
int obstack_alignment_mask (struct obstack *obstack);
int obstack_chunk_size (struct obstack *obstack);



 


 





 



 



 




























 














 





# 301 "obstack.h"


# 312 "obstack.h"









 


   














































 


# 387 "obstack.h"









# 482 "obstack.h"



# 25 "genattr.c" 2


static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;




extern void free ();
extern int atoi ();
extern rtx read_rtx ();

char *xmalloc ();
static void fatal ();
void fancy_abort ();

 

struct range
{
  int min;
  int max;
};

 


struct function_unit
{
  char *name;			 
  struct function_unit *next;	 
  int multiplicity;		 
  int simultaneity;		 

  struct range ready_cost;	 
  struct range issue_delay;	 
};

static void
extend_range (range, min, max)
     struct range *range;
     int min;
     int max;
{
  if (range->min > min) range->min = min;
  if (range->max < max) range->max = max;
}

static void
init_range (range)
     struct range *range;
{
  range->min = 100000;
  range->max = -1;
}

static void
write_upcase (str)
    char *str;
{
  for (; *str; str++)
    if (*str >= 'a' && *str <= 'z')
      printf ("%c", *str - 'a' + 'A');
    else
      printf ("%c", *str);
}

static void
gen_attr (attr)
     rtx attr;
{
  char *p;

  printf ("#define HAVE_ATTR_%s\n", ((attr)->fld[ 0].rtstr) );

   
  if (*((attr)->fld[ 1].rtstr)  == '\0')
    printf ("extern int get_attr_%s ();\n", ((attr)->fld[ 0].rtstr) );
  else
    {
      printf ("enum attr_%s {", ((attr)->fld[ 0].rtstr) );
      write_upcase (((attr)->fld[ 0].rtstr) );
      printf ("_");

      for (p = ((attr)->fld[ 1].rtstr) ; *p != '\0'; p++)
	{
	  if (*p == ',')
	    {
	      printf (", ");
	      write_upcase (((attr)->fld[ 0].rtstr) );
	      printf ("_");
	    }
	  else if (*p >= 'a' && *p <= 'z')
	    printf ("%c", *p - 'a' + 'A');
	  else
	    printf ("%c", *p);
	}

      printf ("};\n");
      printf ("extern enum attr_%s get_attr_%s ();\n\n",
	      ((attr)->fld[ 0].rtstr) , ((attr)->fld[ 0].rtstr) );
    }

   

  if (! strcmp (((attr)->fld[ 0].rtstr) , "length"))
    {
      printf ("extern void init_lengths ();\n");
      printf ("extern void shorten_branches ();\n");
      printf ("extern int insn_default_length ();\n");
      printf ("extern int insn_variable_length_p ();\n");
      printf ("extern int insn_current_length ();\n\n");
      printf ("extern int *insn_addresses;\n");
      printf ("extern int insn_current_address;\n\n");
    }
}

static void
write_units (num_units, multiplicity, simultaneity,
	     ready_cost, issue_delay, blockage)
     int num_units;
     struct range *multiplicity;
     struct range *simultaneity;
     struct range *ready_cost;
     struct range *issue_delay;
     struct range *blockage;
{
  int i, q_size;

  printf ("#define INSN_SCHEDULING\n\n");
  printf ("extern int result_ready_cost ();\n");
  printf ("extern int function_units_used ();\n\n");
  printf ("extern struct function_unit_desc\n");
  printf ("{\n");
  printf ("  char *name;\n");
  printf ("  int bitmask;\n");
  printf ("  int multiplicity;\n");
  printf ("  int simultaneity;\n");
  printf ("  int default_cost;\n");
  printf ("  int max_issue_delay;\n");
  printf ("  int (*ready_cost_function) ();\n");
  printf ("  int (*conflict_cost_function) ();\n");
  printf ("  int max_blockage;\n");
  printf ("  unsigned int (*blockage_range_function) ();\n");
  printf ("  int (*blockage_function) ();\n");
  printf ("} function_units[];\n\n");
  printf ("#define FUNCTION_UNITS_SIZE %d\n", num_units);
  printf ("#define MIN_MULTIPLICITY %d\n", multiplicity->min);
  printf ("#define MAX_MULTIPLICITY %d\n", multiplicity->max);
  printf ("#define MIN_SIMULTANEITY %d\n", simultaneity->min);
  printf ("#define MAX_SIMULTANEITY %d\n", simultaneity->max);
  printf ("#define MIN_READY_COST %d\n", ready_cost->min);
  printf ("#define MAX_READY_COST %d\n", ready_cost->max);
  printf ("#define MIN_ISSUE_DELAY %d\n", issue_delay->min);
  printf ("#define MAX_ISSUE_DELAY %d\n", issue_delay->max);
  printf ("#define MIN_BLOCKAGE %d\n", blockage->min);
  printf ("#define MAX_BLOCKAGE %d\n", blockage->max);
  for (i = 0; (1 << i) < blockage->max; i++)
    ;
  printf ("#define BLOCKAGE_BITS %d\n", i + 1);

   

  i = ((blockage->max) > ( ready_cost->max) ? (blockage->max) : ( ready_cost->max)) ;
  for (q_size = 1; q_size <= i; q_size <<= 1)
    ;
  printf ("#define INSN_QUEUE_SIZE %d\n", q_size);
}

char *
xmalloc (size)
     unsigned size;
{
  register char *val = (char *) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");
  return val;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  char * result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

static void
fatal (s, a1, a2)
     char *s;
{
  fprintf ((&__sF[2]) , "genattr: ");
  fprintf ((&__sF[2]) , s, a1, a2);
  fprintf ((&__sF[2]) , "\n");
  exit (33 );
}

 


void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  register int c;
  int have_delay = 0;
  int have_annul_true = 0;
  int have_annul_false = 0;
  int num_units = 0;
  struct range all_simultaneity, all_multiplicity;
  struct range all_ready_cost, all_issue_delay, all_blockage;
  struct function_unit *units = 0, *unit;
  int i;

  init_range (&all_multiplicity);
  init_range (&all_simultaneity);
  init_range (&all_ready_cost);
  init_range (&all_issue_delay);
  init_range (&all_blockage);

  _obstack_begin ((rtl_obstack), 0, 0, (void *(*) ()) xmalloc , (void (*) ()) free ) ;

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (33 );
    }

  init_rtl ();

  printf ("/* Generated automatically by the program `genattr'\nfrom the machine description file `md'.  */\n\n");


   


  printf ("#define HAVE_ATTR_alternative\n");
  printf ("#define get_attr_alternative(insn) which_alternative\n");
     
   

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == (-1) )
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (	((desc)->code)  == DEFINE_ATTR)
	gen_attr (desc);

      else if (	((desc)->code)  == DEFINE_DELAY)
        {
	  if (! have_delay)
	    {
	      printf ("#define DELAY_SLOTS\n");
	      printf ("extern int num_delay_slots ();\n");
	      printf ("extern int eligible_for_delay ();\n\n");
	      printf ("extern int const_num_delay_slots ();\n\n");
	      have_delay = 1;
	    }

	  for (i = 0; i < ((desc)->fld[ 1].rtvec->num_elem) ; i += 3)
	    {
	      if (((desc)->fld[ 1].rtvec->elem[ i + 1].rtx)  && ! have_annul_true)
		{
		  printf ("#define ANNUL_IFTRUE_SLOTS\n");
		  printf ("extern int eligible_for_annul_true ();\n");
		  have_annul_true = 1;
		}

	      if (((desc)->fld[ 1].rtvec->elem[ i + 2].rtx)  && ! have_annul_false)
		{
		  printf ("#define ANNUL_IFFALSE_SLOTS\n");
		  printf ("extern int eligible_for_annul_false ();\n");
		  have_annul_false = 1;
		}
	    }
        }

      else if (	((desc)->code)  == DEFINE_FUNCTION_UNIT)
	{
	  char *name = ((desc)->fld[ 0].rtstr) ;
	  int multiplicity = ((desc)->fld[ 1].rtint) ;
	  int simultaneity = ((desc)->fld[ 2].rtint) ;
	  int ready_cost = ((((desc)->fld[ 4].rtint) ) > ( 1) ? (((desc)->fld[ 4].rtint) ) : ( 1)) ;
	  int issue_delay = ((((desc)->fld[ 5].rtint) ) > ( 1) ? (((desc)->fld[ 5].rtint) ) : ( 1)) ;
	  int issueexp_p = (((desc)->fld[ 6].rtvec)  != 0);

	  for (unit = units; unit; unit = unit->next)
	    if (strcmp (unit->name, name) == 0)
	      break;

	  if (unit == 0)
	    {
	      int len = strlen (name) + 1;
	      unit = (struct function_unit *)
		__builtin_alloca(sizeof (struct function_unit)) ;
	      unit->name = (char *) __builtin_alloca(len) ;
	      bcopy (name, unit->name, len);
	      unit->multiplicity = multiplicity;
	      unit->simultaneity = simultaneity;
	      unit->ready_cost.min = unit->ready_cost.max = ready_cost;
	      unit->issue_delay.min = unit->issue_delay.max = issue_delay;
	      unit->next = units;
	      units = unit;
	      num_units++;

	      extend_range (&all_multiplicity, multiplicity, multiplicity);
	      extend_range (&all_simultaneity, simultaneity, simultaneity);
	    }
	  else if (unit->multiplicity != multiplicity
		   || unit->simultaneity != simultaneity)
	    fatal ("Differing specifications given for `%s' function unit.",
		   unit->name);

	  extend_range (&unit->ready_cost, ready_cost, ready_cost);
	  extend_range (&unit->issue_delay,
			issueexp_p ? 1 : issue_delay, issue_delay);
	  extend_range (&all_ready_cost,
			unit->ready_cost.min, unit->ready_cost.max);
	  extend_range (&all_issue_delay,
			unit->issue_delay.min, unit->issue_delay.max);
	}
    }

  if (num_units > 0)
    {
       











      for (unit = units; unit; unit = unit->next)
	{
	  struct range blockage;
	  int max_issue_time = ((unit->issue_delay.max) > ( 1) ? (unit->issue_delay.max) : ( 1)) ;

	  blockage = unit->issue_delay;
	  blockage.max = ((unit->ready_cost.max
			      - (unit->ready_cost.min - 1)) > (
			      blockage.max) ? (unit->ready_cost.max
			      - (unit->ready_cost.min - 1)) : (
			      blockage.max)) ;
# 392 "genattr.c"
	  blockage.min = ((1) > ( blockage.min) ? (1) : ( blockage.min)) ;

	  if (unit->simultaneity != 0)
	    {
	      int fill_time = ((unit->simultaneity - 1)
			       * unit->issue_delay.min);
	      blockage.min = ((unit->ready_cost.min - fill_time) > (
				  blockage.min) ? (unit->ready_cost.min - fill_time) : (
				  blockage.min)) ;
# 400 "genattr.c"
	      blockage.max = ((unit->ready_cost.max - fill_time) > (
				  blockage.max) ? (unit->ready_cost.max - fill_time) : (
				  blockage.max)) ;
# 402 "genattr.c"
	    }
	  extend_range (&all_blockage, blockage.min, blockage.max);
	}

      write_units (num_units, &all_multiplicity, &all_simultaneity,
		   &all_ready_cost, &all_issue_delay, &all_blockage);
    }

  fflush ((&__sF[1]) );
  exit (((((&__sF[1]) )->_flags & 0x0040		) != 0)   != 0 ? 33  : 0 );
   
  return 0;
}
