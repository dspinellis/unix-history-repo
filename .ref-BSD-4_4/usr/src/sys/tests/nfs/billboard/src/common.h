typedef unsigned char	uchar;

/*
** We use the name bool_t to maintain consistency with the RPC headers.
#ifdef  bool_t
#undef  bool_t
#endif
typedef enum    { FALSE = 0, TRUE = 1 } bool_t; 
*/


/*
** These are the return code defenitions for the rpc interface calls.
*/
#define	BB_SUCCESS		0
#define	BB_FAILURE		(-1)
#define BB_BAD_CLIENT		(-2)
#define BB_BAD_SERVER		(-3)
#define BB_ALREADY_SET		(-4)
#define BB_ALREADY_UNSET	(-5)
#define BB_BAD_PASSWD		(-6)
#define BB_BAD_PHASE		(-7)

#define	NUL		'\0'	/* The null character.		*/

#define BB_END_OF_FILE	(-2)

#define BB_MAX_LINE_LEN	200

#define BB_SYSLOG_UDP_CREATE	"Unable to create UDP transport."
#define BB_SYSLOG_SVC_REGISTER	"Unable to register the rpc service."
#define BB_SYSLOG_SVC_RUN	"The service run routine failed."

