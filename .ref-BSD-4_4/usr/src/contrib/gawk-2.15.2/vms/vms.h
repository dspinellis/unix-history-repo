/*
 * vms.h - miscellaneous definitions for use with VMS system services.
 *							Pat Rankin, Nov'89
 */

#if 0
#include <iodef.h>
#else
#define IO$_WRITEVBLK	48	/* write virtual block */
#define IO$V_CANCTRLO	6	/* cancel <ctrl/O> (ie, resume tty output) */
#define IO$M_CANCTRLO	(1 << IO$V_CANCTRLO)
#endif

#if 0
#include <clidef.h>
#include <cliverbdef.h>
#include <fscndef.h>
#else
#define CLI$K_GETCMD	1
#define CLI$K_VERB_MCR	33
#define CLI$K_VERB_RUN	36
#define FSCN$_FILESPEC	1
#endif

#if 0
#include <climsgdef.h>
#else
#define CLI$_RUNUSED	0x00030000	/* value returned by $CLI for "RUN" */
#define CLI$_SYNTAX	0x000310FC	/* error signalled by CLI$DCL_PARSE */
#define CLI$_INSFPRM	0x00038048	/* insufficient parameters */
#define CLI$_VALREQ	0x00038150	/* missing required value  */
#define CLI$_CONFLICT	0x00038258	/* conflicting qualifiers  */
#define CLI$_NOOPTPRS	0x00038840	/* no option present	   */
#endif

#if 0
#include <psldef.h>
#else
#define PSL$C_USER	3	/* user mode */
#endif

#if !defined(_TYPES_) || !defined(__GNUC__)
typedef unsigned long	u_long;
typedef unsigned short	u_short;
#endif
typedef struct _dsc { int len; char *adr; } Dsc; /* limited string descriptor */
 /* standard VMS itemlist-3 structure */
typedef struct _itm { u_short len, code; void *buffer; u_short *retlen; } Itm;

#define vmswork(sts) ((sts)&1)
#define vmsfail(sts) (!vmswork(sts))
#define CondVal(sts) ((sts)&0x0FFFFFF8)     /* strip severity & msg inhibit */
#define Descrip(strdsc,strbuf) Dsc strdsc = {sizeof strbuf - 1, strbuf}

extern int    shell$is_shell P((void));
extern u_long LIB$FIND_FILE P((const Dsc *, Dsc *, void *, ...));
extern u_long LIB$FIND_FILE_END P((void *));
#ifndef NO_TTY_FWRITE
extern u_long LIB$GET_EF P((long *));
extern u_long SYS$ASSIGN P((const Dsc *, short *, long, const Dsc *));
extern u_long SYS$DASSGN P((short));
extern u_long SYS$QIO P((u_long, u_long, u_long, void *, void (*)(), u_long,
			 const char *, int, int, u_long, int, int));
extern u_long SYS$SYNCH P((long, void *));
#endif	/*!NO_TTY_FWRITE*/
 /* system services for logical name manipulation */
extern u_long SYS$TRNLNM P((const u_long *,const Dsc *,const Dsc *,
			    const unsigned char *,Itm *));
extern u_long SYS$CRELNM P((const u_long *,const Dsc *,const Dsc *,
			    const unsigned char *,const Itm *));
extern u_long SYS$CRELOG P((int,const Dsc *,const Dsc *,unsigned char));
extern u_long SYS$DELLNM P((const Dsc *,const Dsc *,const unsigned char *));

extern void   v_add_arg P((int, const char *));
extern void   vms_exit P((int));
extern char  *vms_strerror P((int));
extern char  *vms_strdup P((const char *));
extern int    vms_devopen P((const char *,int));
extern int    vms_execute P((const char *, const char *, const char *));
extern int    vms_gawk P((void));
extern u_long Cli_Present P((const char *));
extern u_long Cli_Get_Value P((const char *, char *, int));
extern u_long Cli_Parse_Command P((const void *, const char *));

