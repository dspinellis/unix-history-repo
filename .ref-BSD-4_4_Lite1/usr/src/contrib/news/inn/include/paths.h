/*  $Revision: 1.22 $
**
**  Here be #define's for filenames, socket names, environment variables,
**  and so on.  The numbers refer to sections in the config.dist file.
*/


/*
**  7.  PATHS TO COMMON PROGRAMS
*/
    /* =()<#define _PATH_INND	"@<_PATH_INND>@">()= */
#define _PATH_INND	"/usr/contrib/news/innd"
    /* =()<#define _PATH_INNDSTART	"@<_PATH_INNDSTART>@">()= */
#define _PATH_INNDSTART	"/usr/contrib/news/inndstart"
    /* =()<#define _PATH_SENDMAIL	"@<_PATH_SENDMAIL>@">()= */
#define _PATH_SENDMAIL	"/usr/sbin/sendmail -t"
    /* =()<#define _PATH_SH		"@<_PATH_SH>@">()= */
#define _PATH_SH		"/bin/sh"
    /* =()<#define _PATH_NNRPD		"@<_PATH_NNRPD>@">()= */
#define _PATH_NNRPD		"/usr/contrib/news/nnrpd"
    /* =()<#define _PATH_NNTPD          "@<_PATH_NNTPD>@">()= */
#define _PATH_NNTPD          "/usr/contrib/news/nnrpd"
    /* =()<#define _PATH_NNQRD		"@<_PATH_NNQRD>@">()= */
#define _PATH_NNQRD		"/usr/contrib/news/nnrpd"
    /* =()<#define _PATH_COMPRESS	"@<_PATH_COMPRESS>@">()= */
#define _PATH_COMPRESS	"/usr/bin/compress"
    /* =()<#define _PATH_RNEWS	"@<_PATH_RNEWS>@">()= */
#define _PATH_RNEWS	"/usr/contrib/news/rnews"
    /* =()<#define _PATH_NEWSBIN	"@<_PATH_NEWSBIN>@">()= */
#define _PATH_NEWSBIN	"/usr/contrib/news"
    /* =()<#define _PATH_TMP	"@<_PATH_TMP>@">()= */
#define _PATH_TMP	"/tmp"


/*
**  8.  PATHS RELATED TO THE SPOOL DIRECTORY
*/
    /* =()<#define _PATH_SPOOL		"@<_PATH_SPOOL>@">()= */
#define _PATH_SPOOL		"/var/spool/news/spool"
    /* =()<#define _PATH_OVERVIEWDIR		"@<_PATH_OVERVIEWDIR>@">()= */
#define _PATH_OVERVIEWDIR		"/var/spool/news/spool"
    /* =()<#define _PATH_OVERVIEW		"@<_PATH_OVERVIEW>@">()= */
#define _PATH_OVERVIEW		".overview"
    /* =()<#define _PATH_SPOOLNEWS	"@<_PATH_SPOOLNEWS>@">()= */
#define _PATH_SPOOLNEWS	"/var/spool/news/spool/in.coming"
    /* =()<#define _PATH_SPOOLTEMP	"@<_PATH_SPOOLTEMP>@">()= */
#define _PATH_SPOOLTEMP	"/var/spool/news/spool/in.coming/tmp"
    /* =()<#define _PATH_BADNEWS	"@<_PATH_BADNEWS>@">()= */
#define _PATH_BADNEWS	"/var/spool/news/spool/in.coming/bad"
    /* =()<#define _PATH_RELBAD		"@<_PATH_RELBAD>@">()= */
#define _PATH_RELBAD		"bad"


/*
**  9.  EXECUTION PATHS FOR INND AND RNEWS
*/
    /* =()<#define _PATH_RNEWS_DUP_LOG	"@<_PATH_RNEWS_DUP_LOG>@">()= */
#define _PATH_RNEWS_DUP_LOG	"/dev/null"
    /* =()<#define _PATH_RNEWSPROGS	"@<_PATH_RNEWSPROGS>@">()= */
#define _PATH_RNEWSPROGS	"/usr/contrib/rnews"
    /* =()<#define _PATH_CONTROLPROGS	"@<_PATH_CONTROLPROGS>@">()= */
#define _PATH_CONTROLPROGS	"/var/spool/news/data/ctlbin"
    /* =()<#define _PATH_BADCONTROLPROG	"@<_PATH_BADCONTROLPROG>@">()= */
#define _PATH_BADCONTROLPROG	"default"


/*
**  10.  SOCKETS CREATED BY INND OR CLIENTS
*/
    /* =()<#define _PATH_INNDDIR	"@<_PATH_INNDDIR>@">()= */
#define _PATH_INNDDIR	"/var/spool/news/data/innd"
    /* =()<#define _PATH_NNTPCONNECT	"@<_PATH_NNTPCONNECT>@">()= */
#define _PATH_NNTPCONNECT	"/var/spool/news/data/innd/nntpin"
    /* =()<#define _PATH_NEWSCONTROL	"@<_PATH_NEWSCONTROL>@">()= */
#define _PATH_NEWSCONTROL	"/var/spool/news/data/innd/control"
    /* =()<#define _PATH_TEMPSOCK	"@<_PATH_TEMPSOCK>@">()= */
#define _PATH_TEMPSOCK	"/var/spool/news/data/innd/ctlinndXXXXXX"


/*
**  11.  LOG AND CONFIG FILES
*/
    /* =()<#define _PATH_NEWSLIB	"@<_PATH_NEWSLIB>@">()= */
#define _PATH_NEWSLIB	"/var/spool/news/data"
    /* =()<#define _PATH_LOGFILE		"@<_PATH_LOGFILE>@">()= */
#define _PATH_LOGFILE		"/var/spool/news/data/news"
    /* =()<#define _PATH_ERRLOG	"@<_PATH_ERRLOG>@">()= */
#define _PATH_ERRLOG	"/var/spool/news/data/errlog"
    /* =()<#define _PATH_SERVERPID	"@<_PATH_SERVERPID>@">()= */
#define _PATH_SERVERPID	"/var/spool/news/data/innd/innd.pid"
    /* =()<#define _PATH_NEWSFEEDS	"@<_PATH_NEWSFEEDS>@">()= */
#define _PATH_NEWSFEEDS	"/var/spool/news/data/newsfeeds"
    /* =()<#define _PATH_HISTORY	"@<_PATH_HISTORY>@">()= */
#define _PATH_HISTORY	"/var/spool/news/data/history"
    /* =()<#define _PATH_INNDHOSTS	"@<_PATH_INNDHOSTS>@">()= */
#define _PATH_INNDHOSTS	"/var/spool/news/data/hosts.nntp"
    /* =()<#define _PATH_ACTIVE	"@<_PATH_ACTIVE>@">()= */
#define _PATH_ACTIVE	"/var/spool/news/data/active"
    /* =()<#define _PATH_NEWACTIVE	"@<_PATH_NEWACTIVE>@">()= */
#define _PATH_NEWACTIVE	"/var/spool/news/data/active.tmp"
    /* =()<#define _PATH_OLDACTIVE	"@<_PATH_OLDACTIVE>@">()= */
#define _PATH_OLDACTIVE	"/var/spool/news/data/active.old"
    /* =()<#define _PATH_ACTIVETIMES	"@<_PATH_ACTIVETIMES>@">()= */
#define _PATH_ACTIVETIMES	"/var/spool/news/data/active.times"
    /* =()<#define _PATH_BATCHDIR	"@<_PATH_BATCHDIR>@">()= */
#define _PATH_BATCHDIR	"/var/spool/news/out.going"
    /* =()<#define _PATH_ARCHIVEDIR	"@<_PATH_ARCHIVEDIR>@">()= */
#define _PATH_ARCHIVEDIR	"/var/spool/news/news.archive"
    /* =()<#define _PATH_DISTPATS	"@<_PATH_DISTPATS>@">()= */
#define _PATH_DISTPATS	"/var/spool/news/data/distrib.pats"
    /* =()<#define _PATH_NNRPDIST	"@<_PATH_NNRPDIST>@">()= */
#define _PATH_NNRPDIST	"/var/spool/news/data/distributions"
    /* =()<#define _PATH_NEWSGROUPS	"@<_PATH_NEWSGROUPS>@">()= */
#define _PATH_NEWSGROUPS	"/var/spool/news/data/newsgroups"
    /* =()<#define _PATH_CONFIG	"@<_PATH_CONFIG>@">()= */
#define _PATH_CONFIG	"/var/spool/news/data/inn.conf"
    /* =()<#define _PATH_CLIENTACTIVE	"@<_PATH_CLIENTACTIVE>@">()= */
#define _PATH_CLIENTACTIVE	"/var/spool/news/data/active"
    /* =()<#define _PATH_TEMPACTIVE	"@<_PATH_TEMPACTIVE>@">()= */
#define _PATH_TEMPACTIVE	"/tmp/activeXXXXXX"
    /* =()<#define _PATH_MODERATORS	"@<_PATH_MODERATORS>@">()= */
#define _PATH_MODERATORS	"/var/spool/news/data/moderators"
    /* =()<#define _PATH_SERVER	"@<_PATH_SERVER>@">()= */
#define _PATH_SERVER	"/var/spool/news/data/server"
    /* =()<#define _PATH_NNTPPASS	"@<_PATH_NNTPPASS>@">()= */
#define _PATH_NNTPPASS	"/var/spool/news/data/passwd.nntp"
    /* =()<#define _PATH_NNRPACCESS	"@<_PATH_NNRPACCESS>@">()= */
#define _PATH_NNRPACCESS	"/var/spool/news/data/nnrp.access"
    /* =()<#define _PATH_EXPIRECTL	"@<_PATH_EXPIRECTL>@">()= */
#define _PATH_EXPIRECTL	"/var/spool/news/data/expire.ctl"
    /* =()<#define _PATH_SCHEMA	"@<_PATH_SCHEMA>@">()= */
#define _PATH_SCHEMA	"/var/spool/news/data/overview.fmt"


/*
**  ENVIRONMENT VARIABLES
*/
    /* The host name of the NNTP server, for client posting. */
#define _ENV_NNTPSERVER		"NNTPSERVER"
    /* The Organization header line, for client posting. */
#define _ENV_ORGANIZATION	"ORGANIZATION"
    /* What to put in the From line, for client posting. */
#define _ENV_FROMHOST		"FROMHOST"
    /* =()<#define _ENV_UUCPHOST	"@<_ENV_UUCPHOST>@">()= */
#define _ENV_UUCPHOST	"UU_MACHINE"


/*
**  PARAMETERS IN THE _PATH_CONFIG FILE.
*/
    /* Host for the From line; default is FQDN. */
#define _CONF_FROMHOST		"fromhost"
    /* NNTP server to post to, if getenv(_ENV_NNTPSERVER) is NULL. */
#define _CONF_SERVER		"server"
    /* Host for the Path line; default is FQDN. */
#define _CONF_PATHHOST		"pathhost"
    /* Data for the Organization line if getenv(_ENV_ORGANIZATION) is NULL. */
#define _CONF_ORGANIZATION	"organization"
    /* Default host to mail moderated articles to. */
#define _CONF_MODMAILER		"moderatormailer"
    /* Default domain of local host. */
#define _CONF_DOMAIN		"domain"
    /* Default mime version. */
#define _CONF_MIMEVERSION	"mime-version"
    /* Default Content-Type */
#define _CONF_CONTENTTYPE	"mime-contenttype"
    /* Default encoding */
#define _CONF_ENCODING		"mime-encoding"
