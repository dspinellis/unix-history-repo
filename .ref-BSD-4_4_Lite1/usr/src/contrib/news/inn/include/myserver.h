/*  $Revision: 1.3 $
**
**  Replacement "server.h" file for remote rn.
*/


#if	!defined(NNTP_CLASS_OK)
/*
**  These are defined in INN <nntp.h>.  Rather then require this file
**  to include that file, we duplicate the relevent lines here.
*/
#define NNTP_NOSUCHGROUP_VAL		411
#define NNTP_CLASS_OK			'2'
#define NNTP_CLASS_ERROR		'4'
#define NNTP_CLASS_FATAL		'5'

#define NNTP_SYNTAX_VAL			501
#define NNTP_STRLEN			512
#endif	/* !defined(NNTP_CLASS_OK) */


#define CHAR_OK			NNTP_CLASS_OK
#define CHAR_ERR		NNTP_CLASS_ERROR
#define CHAR_FATAL		NNTP_CLASS_FATAL
#define ERR_NOGROUP		NNTP_NOSUCHGROUP_VAL
#define ERR_CMDSYN		NNTP_SYNTAX_VAL


extern char	*getserverbyfile();
extern int	get_server();
extern int	server_init();
extern void	close_server();
extern void	put_server();

extern char	ser_line[NNTP_STRLEN];
