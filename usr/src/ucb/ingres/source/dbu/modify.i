




extern	struct	_iobuf {
	int	_cnt;
	char	*_ptr;
	char	*_base;
	int	_bufsiz;
	short	_flag;
	char	_file;
} _iob[20];


























struct _iobuf	*fopen();
struct _iobuf	*fdopen();
struct _iobuf	*freopen();
long	ftell();
char	*fgets();






























typedef char	bool;		
















 












































































































extern short	*tT;













































struct relation
{
		char	relid[	12		];	
		char	relowner[2];	
		char	relspec;	
				
				
				
				
				
				
		char	relindxd;	
		short	relstat2;	
		short	relstat;	
		long	relsave;	
		long	reltups;	
		short	relatts;	
		short	relwid;		
		long	relprim;	
		long	relfree;	
		long	relstamp;	
};

















struct attribute
{
		char 	attrelid[	12		];	
		char	attowner[2];	
		short	attid;		
		char	attname[	12		];	
		short	attoff;		
		char	attfrmt;	
		char	attfrml;	
		char	attxtra;	
};









struct tup_id
{

		char	line_id, pg2, pg1, pg0;

};

typedef struct tup_id	TID;

typedef union
{
	long	ltid;
	TID	s_tupid;
} tid_type;


























struct descriptor
{
	struct relation	reldum;
		




	char	relvname[	12		];	
		short	relfp;		
		short	relopn;		
	tid_type reltid;	


		long	reladds;	
		short	reloff[	50		];	
		char	relfrmt[	50		]; 

		char	relfrml[	50		]; 


		char	relxtra[	50		]; 

		char	relgiven[	50		]; 


};

typedef struct descriptor	DESC;


typedef struct
{
	DESC	*rngvdesc;	
	bool	rngvmark;	
}  RANGEV;
















union anytype
{
	char		i1type;
	short		i2type;
	long		i4type;
	float		f4type;
	double		f8type;
	char		c0type[1];	


	char		*cptype;
	char		**cpptype;
};

typedef union anytype	ANYTYPE;
























typedef struct
{
	short	pv_type;	
	short	pv_len;		
	union
	{
		short			pv_int;		
		struct querytree	*pv_qtree;	
		char			*pv_str;	
		char			*pv_tuple;	
	} pv_val;
}  PARM;




























struct accessparam
{
	short	mode;		
	short	sec_index;	
	char	keydno[	50		 + 1];
};










struct desxx
{
	char	*cach_relname;	
	DESC	*cach_desc;	
	DESC	*cach_alias;	
};











































































extern char	*Usercode;
extern char	*Pathname;

















struct out_arg
{
	int	c0width;	
	int	i1width;	
	int	i2width;	
	int	i4width;	
	int	f4width;	
	int	f8width;	
	int	f4prec;		
	int	f8prec;		
	char	f4style;	
	char	f8style;	
	int	linesperpage;	
	char	coldelim;	
};










































































struct accbuf
{
	
	long		mainpg;		
	long		ovflopg;	
	short		nxtlino;	
	char		firstup[	1024		 - 12];	
	short		linetab[1];	
					



	
	long		rel_tupid;	
	long		thispage;	
	int		filedesc;	
	struct accbuf	*modf;		
	struct accbuf	*modb;		
	int		bufstatus;	
};







struct
{
	char	acc_buf[3		];
};


extern struct accbuf	*Acc_head;	
extern struct accbuf	*Acc_tail;	
extern struct accbuf	Acc_buf[3		];	












struct adminhdr
{
	char	adowner[2];	
	short	adflags;	
	short	adlength;	
	short	adversion;	
	short	adreldsz;	
	short	adattdsz;	
};

struct admin
{
	struct adminhdr		adhdr;
	struct descriptor	adreld;
	struct descriptor	adattd;
};














extern struct admin		Admin;





struct pgtuple
{
	struct tup_id	childtid;		
	char		childtup[	1010		];
};






extern long	Accuread, Accuwrite;





extern char	*Acctuple;		
extern		Accerror;		
extern char	Accanon[	1010		];	
















struct batchbuf
{
	char	file_id[	6	];	
	char	bbuf[506	];	
};


struct si_doms
{
	short	rel_off;	
	short	tupo_off;	
	short	dom_size;	
				
};
struct batchhd
{
	char	db_name[15];	
	char	rel_name[13];	
	char	userid[2];	
	long	num_updts;	
	short	mode_up;	
	short	tido_size;	
	short	tupo_size;	
	short	tupn_size;	
	short	tidn_size;	
	short	si_dcount;	
	struct si_doms	si[	50		+1];	
};



short	Batch_fp;	
short	Batch_cnt;	
short	Batch_dirty;	
short	Batch_lread;	
short	Batch_recovery;	

extern char	*Fileset;	
struct batchbuf	Batchbuf;
struct batchhd	Batchhd;




























struct lockreq
{
	char	lract;			





	char	lrtype;			





	char	lrmod;			



					
	char	dbnode[4];		
	char	lrel[4];		
	char	lpage[4];		
};

extern char	Acclock;		
extern int	Alockdes;		
extern int	Lockrel;		




































































































struct ttychars {
	char	tc_erase;	
	char	tc_kill;	
	char	tc_intrc;	
	char	tc_quitc;	
	char	tc_startc;	
	char	tc_stopc;	
	char	tc_eofc;	
	char	tc_brkc;	
	char	tc_suspc;	
	char	tc_dsuspc;	
	char	tc_rprntc;	
	char	tc_flushc;	
	char	tc_werasc;	
	char	tc_lnextc;	
};





































































struct sgttyb {
	char	sg_ispeed;		
	char	sg_ospeed;		
	char	sg_erase;		
	char	sg_kill;		
	short	sg_flags;		
};

struct tchars {
	char	t_intrc;	
	char	t_quitc;	
	char	t_startc;	
	char	t_stopc;	
	char	t_eofc;		
	char	t_brkc;		
};
struct ltchars {
	char	t_suspc;	
	char	t_dsuspc;	
	char	t_rprntc;	
	char	t_flushc;	
	char	t_werasc;	
	char	t_lnextc;	
};





































































































































































































































































































































int	(*signal())();















































































typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;


typedef	struct	_physadr { int r[1]; } *physadr;
typedef	struct	label_t	{
	int	val[14];
} label_t;


typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef	u_long	ino_t;
typedef	long	swblk_t;
typedef	int	size_t;
typedef	int	time_t;
typedef	short	dev_t;
typedef	int	off_t;

typedef	struct	fd_set { int fds_bits[1]; } fd_set;






































































struct	stat
{
	dev_t	st_dev;
	ino_t	st_ino;
	unsigned short st_mode;
	short	st_nlink;
	short	st_uid;
	short	st_gid;
	dev_t	st_rdev;
	off_t	st_size;
	time_t	