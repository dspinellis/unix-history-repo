/*
** Various portability definitions.
**
**	@(#)port.h              e07@nikhef.nl (Eric Wassenaar) 930915
*/

#if defined(SYSV)
#define SYSV_MEMSET
#define SYSV_STRCHR
#define SYSV_SETVBUF
#endif

#if defined(__hpux) || defined(hpux)
#define SYSV_SETVBUF
#endif

#if defined(sparc) && !defined(__svr4__)
#define SWAPFILE_HACK
#endif
#if defined(apollo) || defined(ultrix) || defined(sgi) || defined(_AIX)
#define SWAPFILE_HACK
#endif

#if defined(RES_PRF_STATS)
#define BIND_49
#else
#define BIND_48
#endif

/*
** The following should depend on existing definitions.
*/

#if defined(BIND_49)
typedef struct __res_state	res_state_t;
#else
typedef struct state		res_state_t;
#endif

#if defined(BIND_49)
typedef char		rrec_data_t;
#else
typedef struct rrec	rrec_data_t;
#endif

#if defined(__alpha) || defined(BIND_49)
typedef u_int	ipaddr_t;
#else
typedef u_long	ipaddr_t;
#endif

#if defined(apollo) || defined(_BSD_SIGNALS)
typedef int	sigtype_t;
#else
typedef void	sigtype_t;
#endif

typedef char	ptr_t;		/* generic pointer type */
typedef u_int	siz_t;		/* general size type */

#ifdef SYSV_MEMSET
#define bzero(a,n)	(void) memset(a,'\0',n)
#define bcopy(a,b,n)	(void) memcpy(b,a,n)
#endif

#ifdef SYSV_STRCHR
#define index		strchr
#define rindex		strrchr
#endif

#ifdef SYSV_SETVBUF
#define linebufmode(a)	(void) setvbuf(a, (char *)NULL, _IOLBF, BUFSIZ);
#else
#define linebufmode(a)	(void) setlinebuf(a);
#endif

#ifdef ULTRIX_RESOLV
#define nslist(i)	_res.ns_list[i].addr
#else
#define nslist(i)	_res.nsaddr_list[i]
#endif

#if defined(sparc) && defined(NO_YP_LOOKUP)
#define gethostbyname	(struct hostent *)__switch_gethostbyname
#define gethostbyaddr	(struct hostent *)__switch_gethostbyaddr
#endif

#ifdef SWAPFILE_HACK
#if defined(apollo)
#define swapfile(a,b)	\
	{ int fd; fd = fileno(a); fileno(a) = fileno(b); fileno(b) = fd; }
#else
#define swapfile(a,b)	\
	{ FILE f; f = *(a); *(a) = *(b); *(b) = f; }
#endif
#endif

#if !defined(HOST_RES_SEND) && !defined(BIND_RES_SEND)
#if defined(BIND_49)
#define BIND_RES_SEND
#else
#define HOST_RES_SEND
#endif
#endif

#define PROTO(TYPES)	()
