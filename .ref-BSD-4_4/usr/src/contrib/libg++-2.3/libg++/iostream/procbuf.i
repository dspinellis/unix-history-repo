# 1 "procbuf.C"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


# 1 "ioprivate.h" 1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

# 1 "/usr/include/stddef.h" 1 3
 





































# 1 "/usr/include/machine/ansi.h" 1 3
 





































 



















# 39 "/usr/include/stddef.h" 2 3


typedef	int				ptrdiff_t;


typedef	unsigned int			size_t;




typedef	unsigned short			wchar_t;










# 18 "ioprivate.h" 2

# 1 "/usr/include/stdlib.h" 1 3
 
















































typedef struct {
	int quot;		 
	int rem;		 
} div_t;

typedef struct {
	long quot;		 
	long rem;		 
} ldiv_t;








# 1 "/usr/include/sys/cdefs.h" 1 3
 













































 











# 76 "/usr/include/sys/cdefs.h" 3


 
















# 67 "/usr/include/stdlib.h" 2 3


extern "C" { 
__volatile  void
	 abort (void)		;
__const  int
	 abs (int)		;
int	 atexit (void (*)(void))		;
double	 atof (const char *)		;
int	 atoi (const char *)		;
long	 atol (const char *)		;
void	*bsearch (const void *, const void *, size_t,
	    size_t, int (*)(const void *, const void *))		;
void	*calloc (size_t, size_t)		;
__const  div_t
	 div (int, int)		;
__volatile  void
	 exit (int)		;
void	 free (void *)		;
char	*getenv (const char *)		;
__const  long
	 labs (long)		;
__const  ldiv_t
	 ldiv (long, long)		;
void	*malloc (size_t)		;
void	 qsort (void *, size_t, size_t,
	    int (*)(const void *, const void *))		;
int	 rand (void)		;
void	*realloc (void *, size_t)		;
void	 srand (unsigned)		;
double	 strtod (const char *, char **)		;
long	 strtol (const char *, char **, int)		;
unsigned long
	 strtoul (const char *, char **, int)		;
int	 system (const char *)		;

 
int	 mblen (const char *, size_t)		;
size_t	 mbstowcs (wchar_t *, const char *, size_t)		;
int	 wctomb (char *, wchar_t)		;
int	 mbtowc (wchar_t *, const char *, size_t)		;
size_t	 wcstombs (char *, const wchar_t *, size_t)		;


int	 putenv (const char *)		;
int	 setenv (const char *, const char *, int)		;


# 155 "/usr/include/stdlib.h" 3

}; 


# 19 "ioprivate.h" 2

# 1 "/usr/include/string.h" 1 3
 

















































extern "C" { 
void	*memchr (const void *, int, size_t)		;
int	 memcmp (const void *, const void *, size_t)		;
void	*memcpy (void *, const void *, size_t)		;
void	*memmove (void *, const void *, size_t)		;
void	*memset (void *, int, size_t)		;
char	*strcat (char *, const char *)		;
char	*strchr (const char *, int)		;
int	 strcmp (const char *, const char *)		;
int	 strcoll (const char *, const char *)		;
char	*strcpy (char *, const char *)		;
size_t	 strcspn (const char *, const char *)		;
char	*strerror (int)		;
size_t	 strlen (const char *)		;
char	*strncat (char *, const char *, size_t)		;
int	 strncmp (const char *, const char *, size_t)		;
char	*strncpy (char *, const char *, size_t)		;
char	*strpbrk (const char *, const char *)		;
char	*strrchr (const char *, int)		;
size_t	 strspn (const char *, const char *)		;
char	*strstr (const char *, const char *)		;
char	*strtok (char *, const char *)		;
size_t	 strxfrm (char *, const char *, size_t)		;

 

int	 bcmp (const void *, const void *, size_t)		;
void	 bcopy (const void *, void *, size_t)		;
void	 bzero (void *, size_t)		;
int	 ffs (int)		;
char	*index (const char *, int)		;
void	*memccpy (void *, const void *, int, size_t)		;
char	*rindex (const char *, int)		;
int	 strcasecmp (const char *, const char *)		;
char	*strdup (const char *)		;
void	 strmode (int, char *)		;
int	 strncasecmp (const char *, const char *, size_t)		;
char	*strsep (char **, const char *)		;
void	 swab (const void *, void *, size_t)		;

}; 


# 20 "ioprivate.h" 2

# 1 "/usr/include/unistd.h" 1 3
 






































# 1 "/usr/include/sys/types.h" 1 3
 





































 
# 1 "/usr/include/machine/endian.h" 1 3
 


































 





# 83 "/usr/include/machine/endian.h" 3

# 40 "/usr/include/sys/types.h" 2 3










typedef	unsigned long long u_quad_t;	 
typedef	long long	quad_t;
typedef	quad_t *	qaddr_t;

typedef	char *		caddr_t;	 
typedef	long		daddr_t;	 
typedef	unsigned long	dev_t;		 
typedef unsigned long	fixpt_t;	 
typedef	unsigned long	gid_t;		 
typedef	unsigned long	ino_t;		 
typedef	unsigned short	mode_t;		 
typedef	unsigned short	nlink_t;	 
typedef	quad_t		off_t;		 
typedef	short		pid_t;		 
typedef	long		segsz_t;	 
typedef	long		swblk_t;	 
typedef	unsigned long	uid_t;		 













typedef	unsigned long			clock_t;









typedef	int				ssize_t;




typedef	long				time_t;



# 145 "/usr/include/sys/types.h" 3


# 40 "/usr/include/unistd.h" 2 3

# 1 "/usr/include/sys/unistd.h" 1 3
 





































 






 


				 


 





 











 










 










# 41 "/usr/include/unistd.h" 2 3










extern "C" { 
__volatile  void
	 _exit (int)		;
int	 access (const char *, int)		;
u_int	 alarm (u_int)		;
int	 chdir (const char *)		;
int	 chown (const char *, uid_t, gid_t)		;
int	 close (int)		;
int	 dup (int)		;
int	 dup2 (int, int)		;
int	 execl (const char *, const char *, ...)		;
int	 execle (const char *, const char *, ...)		;
int	 execlp (const char *, const char *, ...)		;
int	 execv (const char *, char * const *)		;
int	 execve (const char *, char * const *, char * const *)		;
int	 execvp (const char *, char * const *)		;
pid_t	 fork (void)		;
long	 fpathconf (int, int)		;		 
char	*getcwd (char *, size_t)		;
gid_t	 getegid (void)		;
uid_t	 geteuid (void)		;
gid_t	 getgid (void)		;
int	 getgroups (int, int *)		;		 
char	*getlogin (void)		;
pid_t	 getpgrp (void)		;
pid_t	 getpid (void)		;
pid_t	 getppid (void)		;
uid_t	 getuid (void)		;
int	 isatty (int)		;
int	 link (const char *, const char *)		;

off_t	 __lseek  (int, off_t, int)		;
long	 pathconf (const char *, int)		;	 
int	 pause (void)		;
int	 pipe (int *)		;
ssize_t	 read (int, void *, size_t)		;
int	 rmdir (const char *)		;
int	 setgid (gid_t)		;
int	 setpgid (pid_t, pid_t)		;
pid_t	 setsid (void)		;
int	 setuid (uid_t)		;
u_int	 sleep (u_int)		;
long	 sysconf (int)		;			 
pid_t	 tcgetpgrp (int)		;
int	 tcsetpgrp (int, pid_t)		;
char	*ttyname (int)		;
int	 unlink (const char *)		;
ssize_t	 write (int, const void *, size_t)		;

# 178 "/usr/include/unistd.h" 3

}; 


# 21 "ioprivate.h" 2

# 1 "streambuf.h" 1
 
 
 
 
 
 
 
 
 
 
 
 
 
 





#pragma interface


   
















class ostream; class streambuf; class backupbuf;

 







extern "C" int __underflow(streambuf*);
extern "C" int __overflow(streambuf*, int);

typedef _G_off_t streamoff;
typedef _G_off_t streampos;  

typedef unsigned long __fmtflags;
typedef unsigned char __iostate;

struct _ios_fields {  
    streambuf *_strbuf;
    ostream* _tie;
    int _width;
    __fmtflags _flags;
    _G_wchar_t _fill;
    __iostate _state;
    __iostate _exceptions;
    int _precision;
};















# 95 "streambuf.h"


class ios : public _ios_fields {
  public:
    typedef __fmtflags fmtflags;
    typedef int iostate;
    typedef int openmode;
    enum io_state {
	goodbit = 0 ,
	eofbit = 1 ,
	failbit = 2 ,
	badbit = 4  };
    enum open_mode {
	in = 1 ,
	out = 2 ,
	ate = 4 ,
	app = 8 ,
	trunc = 16 ,
	nocreate = 32 ,
	noreplace = 64 ,
	bin = 128  };
    enum seek_dir { beg, cur, end};
     
    enum { skipws=01, left=02, right=04, internal=010,
	   dec=020, oct=040, hex=0100,
	   showbase=0200, showpoint=0400, uppercase=01000, showpos=02000,
	   scientific=04000, fixed=010000, unitbuf=020000, stdio=040000,
	   dont_close=0x80000000  
	   };
    enum {  
	basefield=dec+oct+hex,
	floatfield = scientific+fixed,
	adjustfield = left+right+internal
    };

# 138 "streambuf.h"


    ostream* tie() const { return _tie; }
    ostream* tie(ostream* val) { ostream* save=_tie; _tie=val; return save; }

     
    _G_wchar_t fill() const { return (_G_wchar_t)_fill; }
    _G_wchar_t fill(_G_wchar_t newf)
	{_G_wchar_t oldf = (_G_wchar_t)_fill; _fill = (char)newf; return oldf;}
    fmtflags flags() const { return _flags; }
    fmtflags flags(fmtflags new_val) {
	fmtflags old_val = _flags; _flags = new_val; return old_val; }
    int precision() const { return _precision; }
    int precision(int newp) {
	unsigned short oldp = _precision; _precision = (unsigned short)newp;
	return oldp; }
    fmtflags setf(fmtflags val) {
	fmtflags oldbits = _flags;
	_flags |= val; return oldbits; }
    fmtflags setf(fmtflags val, fmtflags mask) {
	fmtflags oldbits = _flags;
	_flags = (_flags & ~mask) | (val & mask); return oldbits; }
    fmtflags unsetf(fmtflags mask) {
	fmtflags oldbits = _flags & mask;
	_flags &= ~mask; return oldbits; }
    int width() const { return _width; }
    int width(int val) { int save = _width; _width = val; return save; }




    void _throw_failure() { }


    streambuf* rdbuf() const { return _strbuf; }
    void clear(iostate state = 0) {
	_state = _strbuf ? state : state|badbit;
	if (_state & _exceptions) _throw_failure(); }
    void set(iostate flag) { _state |= flag;
	if (_state & _exceptions) _throw_failure(); }
    int good() const { return _state == 0; }
    int eof() const { return _state & ios::eofbit; }
    int fail() const { return _state & (ios::badbit|ios::failbit); }
    int bad() const { return _state & ios::badbit; }
    iostate rdstate() const { return _state; }
    operator void*() const { return fail() ? (void*)0 : (void*)(-1); }
    int operator!() const { return fail(); }
    iostate exception(iostate enable) {
	iostate old = _exceptions; _exceptions = enable;
	if (_state & _exceptions) _throw_failure();
	return old; }

    static int sync_with_stdio(int on);
    static void sync_with_stdio() { sync_with_stdio(1); }









  protected:
    ios(streambuf* sb = 0, ostream* tie = 0);
    virtual ~ios();
    void init(streambuf* sb) { _state=0; _strbuf=sb; }
};




typedef ios::seek_dir _seek_dir;


 
 
 
 
 




















 
 
class streammarker {
    friend class streambuf;



    friend int __underflow(streambuf*);

    struct streammarker *_next;   
    streambuf *_sbuf;  
    streampos _spos;  
    void set_streampos(streampos sp) { _spos = sp; }
    void set_offset(int offset) { _pos = offset; _spos = (streampos)(-2); }
     
     
    int _pos;
  public:
    streammarker(streambuf *sb);
    ~streammarker();
    int saving() { return  _spos == -2; }
    int delta(streammarker&);
    int delta();
};

struct __streambuf {
     
    int _flags;		 
    char* _gptr;	 
    char* _egptr;	 
    char* _eback;	 
    char* _pbase;	 
    char* _pptr;	 
    char* _epptr;	 
    char* _base;	 
    char* _ebuf;	 
    struct streambuf *_chain;

     
    friend class streammarker;
    char *_other_gbase;  
    char *_aux_limit;   
    char *_other_egptr;  
    streammarker *_markers;


     
    unsigned short _cur_column;
    char _unused;
    char _shortbuf[1];
};

extern unsigned __adjust_column(unsigned start, const char *line, int count);

struct streambuf : private __streambuf {
    friend class ios;
    friend class istream;
    friend class ostream;
    friend class streammarker;



    friend int __underflow(streambuf*);

  protected:
    static streambuf* _list_all;  
    streambuf*& xchain() { return _chain; }
    void _un_link();
    void _link_in();
    char* gptr() const { return _gptr; }
    char* pptr() const { return _pptr; }
    char* egptr() const { return _egptr; }
    char* epptr() const { return _epptr; }
    char* pbase() const { return _pbase; }
    char* eback() const { return _eback; }
    char* base() const { return _base; }
    char* ebuf() const { return _ebuf; }
    int blen() const { return _ebuf - _base; }
    void xput_char(char c) { *_pptr++ = c; }
    int xflags() { return _flags; }
    int xflags(int f) { int fl = _flags; _flags = f; return fl; }
    void xsetflags(int f) { _flags |= f; }
    void xsetflags(int f, int mask) { _flags = (_flags & ~mask) | (f & mask); }
    void gbump(int n) { _gptr += n; }
    void pbump(int n) { _pptr += n; }
    void setb(char* b, char* eb, int a=0);
    void setp(char* p, char* ep) { _pbase=_pptr=p; _epptr=ep; }
    void setg(char* eb, char* g, char *eg) { _eback=eb; _gptr=g; _egptr=eg; }
    char *shortbuf() { return _shortbuf; }

    int in_backup() { return _flags & 0x100 ; }
     
    char *Gbase() { return in_backup() ? _other_gbase : _eback; }
     
    char *eGptr() { return in_backup() ? _other_egptr : _egptr; }
     
    char *Bbase() { return in_backup() ? _eback : _other_gbase; }
    char *Bptr() { return _aux_limit; }
     
    char *eBptr() { return in_backup() ? _egptr : _other_egptr; }
    char *Nbase() { return _other_gbase; }
    char *eNptr() { return _other_egptr; }
    int have_backup() { return _other_gbase != 0 ; }
    int have_markers() { return _markers != 0 ; }
    int _least_marker();
    void switch_to_main_get_area();
    void switch_to_backup_area();
    void free_backup_area();
    void unsave_markers();  
    int put_mode() { return _flags & 0x800 ; }
    int switch_to_get_mode();
    
    streambuf(int flags=0);
  public:
    static int flush_all();
    static void flush_all_linebuffered();  
    virtual int underflow() = 0;  
    virtual int overflow(int c = (-1) ) = 0;  
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);
    int seekmark(streammarker& mark, int delta = 0);
    int sputbackc(char c);
    int sungetc();
    virtual ~streambuf();
    int unbuffered() { return _flags & 2  ? 1 : 0; }
    int linebuffered() { return _flags & 0x200  ? 1 : 0; }
    void unbuffered(int i)
	{ if (i) _flags |= 2 ; else _flags &= ~2 ; }
    void linebuffered(int i)
	{ if (i) _flags |= 0x200 ; else _flags &= ~0x200 ; }
    int allocate() {  
	if (base() || unbuffered()) return 0;
	else return doallocate(); }
     
    void allocbuf() { if (base() == 0 ) doallocbuf(); }
    void doallocbuf();
    virtual int sync();
    virtual int pbackfail(int c);
    virtual streambuf* setbuf(char* p, int len);
    int in_avail() { return _egptr - _gptr; }
    int out_waiting() { return _pptr - _pbase; }
    virtual int xsputn(const char* s, int n);
    int sputn(const char* s, int n) { return xsputn(s, n); }
    int padn(char pad, int n);  
    virtual int xsgetn(char* s, int n);
    int sgetn(char* s, int n) { return xsgetn(s, n); }
    int ignore(int);
    virtual int get_column();
    virtual int set_column(int);
    long sgetline(char* buf, _G_size_t n, char delim, int putback_delim);
    int sbumpc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr++; }
    int sgetc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	else return *(unsigned char*)_gptr; }
    int snextc() {
	if (_gptr >= _egptr && __underflow(this) == (-1) ) return (-1) ;
	return _gptr++, sgetc(); }
    int sputc(int c) {
	if (_pptr >= _epptr) return __overflow(this, (unsigned char)c);
	else return *_pptr++ = c, (unsigned char)c; }
    void stossc() { if (_gptr < _egptr) _gptr++; }
    int vscan(char const *fmt0, _G_va_list ap, ios* stream = 0 );
    int scan(char const *fmt0 ...);
    int vform(char const *fmt0, _G_va_list ap);
    int form(char const *fmt0 ...);




};

 
 

 
 
 
 

class backupbuf : public streambuf {
    friend class streammarker;
  protected:
    backupbuf(int flags=0) : streambuf(flags|0x4000 ) { }
  public:
    virtual int pbackfail(int c);
    virtual int underflow();
    virtual int overflow(int c = (-1) );
};

struct __file_fields {
    short _fileno;
    int _blksize;
    _G_fpos_t  _offset;
 
};

class filebuf : public backupbuf {
  protected:
    struct __file_fields _fb;
    void init();
  public:
    static const int openprot;  
    filebuf();
    filebuf(int fd);
    filebuf(int fd, char* p, int len);
    ~filebuf();
    filebuf* attach(int fd);
    filebuf* open(const char *filename, const char *mode);
    filebuf* open(const char *filename, ios::openmode mode, int prot = 0664);
    virtual int underflow();
    virtual int overflow(int c = (-1) );
    int is_open() const { return _fb._fileno >= 0; }
    int fd() const { return is_open() ? _fb._fileno : (-1) ; }
    filebuf* close();
    virtual int doallocate();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streambuf* setbuf(char* p, int len);
    int xsputn(const char* s, int n);
    int xsgetn(char* s, int n);
    virtual int sync();
  protected:  
 
    int is_reading() { return eback() != egptr(); }
    char* cur_ptr() { return is_reading() ?  gptr() : pptr(); }
     
    char* file_ptr() { return eGptr(); }
    int do_write(const char *data, int to_do);
    int do_flush() { return do_write(_pbase, _pptr-_pbase); }
     
    virtual _G_ssize_t sys_read(char* buf, _G_size_t size);
    virtual _G_fpos_t  sys_seek(_G_fpos_t , _seek_dir);
    virtual _G_ssize_t sys_write(const void*, long);
    virtual int sys_stat(void*);  
    virtual int sys_close();
};

inline ios::ios(streambuf* sb  , ostream* tie  ) {
		_state = sb ? ios::goodbit : ios::badbit; _exceptions=0;
		_strbuf=sb; _tie = tie; _width=0; _fill=' ';
		_flags=ios::skipws|ios::dec; _precision=6; }
inline ios::~ios() {
    if (!(_flags & (unsigned int)ios::dont_close)) delete _strbuf; }


# 22 "ioprivate.h" 2

# 1 "/usr/include/stdarg.h" 1 3
 





































typedef char *va_list;



















# 23 "ioprivate.h" 2






extern int __cvt_double(double number, register int prec, int flags,
			int *signp, int fmtch, char *startp, char *endp);

 











 
 
 
 
 
 
 
 
 
 


extern "C" double _Xstrtod(const char *s00, char **se);

extern "C" char *dtoa(double d, int mode, int ndigits,
                        int *decpt, int *sign, char **rve);
extern int __outfloat(double value, streambuf *sb, char mode,
	       int width, int precision, __fmtflags flags,
	       char sign_mode, char fill);


# 19 "procbuf.C" 2

# 1 "procbuf.h" 1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 



class procbuf : public filebuf {
    _G_pid_t _pid;
  public:
    procbuf() : filebuf() { }
    procbuf(const char *command, int mode);
    procbuf* open(const char *command, int mode);
    procbuf *close() { return (procbuf*)filebuf::close(); }
    virtual int sys_close();
    ~procbuf();
};
# 20 "procbuf.C" 2

# 1 "/usr/include/signal.h" 1 3
 







































# 1 "/usr/include/sys/signal.h" 1 3
 








































# 1 "/usr/include/machine/signal.h" 1 3
 


































 



typedef int sig_atomic_t;





 






struct	sigcontext {
	int	sc_onstack;	 
	int	sc_mask;	 
	int	sc_sp;		 
	int	sc_fp;		 
	int	sc_ap;		 
	int	sc_pc;		 
	int	sc_ps;		 
};
# 42 "/usr/include/sys/signal.h" 2 3




































# 86 "/usr/include/sys/signal.h" 3









typedef unsigned int sigset_t;

 


struct	sigaction {
	void	(*sa_handler)();	 
	sigset_t sa_mask;		 
	int	sa_flags;		 
};










 






# 169 "/usr/include/sys/signal.h" 3



 



extern "C" { 
void	(*signal (int, void (*) (int)		)		) (int)		;
}; 

# 41 "/usr/include/signal.h" 2 3







extern "C" { 
int	raise (int)		;

int	kill (pid_t, int)		;
int	sigaction (int, const struct sigaction *, struct sigaction *)		;
int	sigaddset (sigset_t *, int)		;
int	sigdelset (sigset_t *, int)		;
int	sigemptyset (sigset_t *)		;
int	sigfillset (sigset_t *)		;
int	sigismember (const sigset_t *, int)		;
int	sigpending (sigset_t *)		;
int	sigprocmask (int, const sigset_t *, sigset_t *)		;
int	sigsuspend (const sigset_t *)		;
# 71 "/usr/include/signal.h" 3


}; 

 







# 21 "procbuf.C" 2


# 1 "/usr/include/sys/wait.h" 1 3
 


































 




 

























 











# 142 "/usr/include/sys/wait.h" 3






extern "C" { 
struct rusage;	 

pid_t	wait (int *)		;
pid_t	waitpid (pid_t, int *, int)		;




}; 

# 23 "procbuf.C" 2







procbuf::procbuf(const char *command, int mode) : filebuf()
{
    open(command, mode);
}

procbuf *procbuf::open(const char *command, int mode)
{
    int read_or_write;
    if (is_open())
	return 0 ;
    int pipe_fds[2];
    int parent_end, child_end;
    if (::pipe(pipe_fds) < 0)
	return 0 ;
    if (mode == ios::in) {
	parent_end = pipe_fds[0];
	child_end = pipe_fds[1];
	read_or_write = 8 ;
    }
    else {
	parent_end = pipe_fds[1];
	child_end = pipe_fds[0];
	read_or_write = 4 ;
    }
    _pid = vfork ();
    if (_pid == 0) {
	::close(parent_end);
	int child_std_end = mode == ios::in ? 1 : 0;
	if (child_end != child_std_end) {
	    ::dup2(child_end, child_std_end);
	    ::close(child_end);
	}
	::execl("/bin/sh", "sh", "-c", command, 0 );
	::_exit(127);
    }
    ::close(child_end);
    if (_pid < 0) {
	::close(parent_end);
	return 0 ;
    }
    _fb._fileno = parent_end;
    xsetflags(read_or_write, 4 |8 );
    return this;
}

 

int procbuf::sys_close()
{
    _G_pid_t wait_pid;
    int status = filebuf::sys_close();
    if (status < 0)
	return status;
    int wstatus;

    sigset_t set, oset;
    (*(&set) = 0) ;
    (*(&set) |= 1 << (( 2	) - 1), 0) ;
    (*(&set) |= 1 << (( 3	) - 1), 0) ;
    (*(&set) |= 1 << (( 1	) - 1), 0) ;
    sigprocmask (1	, &set, &oset);
# 100 "procbuf.C"

    while ((wait_pid = wait(&wstatus)) != _pid && wait_pid != -1) { }

    sigprocmask (3	, &oset, (sigset_t *)0 );
# 112 "procbuf.C"

    if (wait_pid == -1)
	return -1;
    return 0;
}

procbuf::~procbuf()
{
    close();
}
