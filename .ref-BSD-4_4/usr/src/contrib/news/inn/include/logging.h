/*  $Revision: 1.8 $
**
**  The logging levels in syslog(3) are confusing.  We use only three,
**  and give them more meaningful names.  See section 2 in config.dist.
*/
#include <syslog.h>


/*
**  2.  LOGGING LEVELS
*/
    /* Facility innd should log under. */
    /* =()<#define LOG_INN_SERVER		@<LOG_INN_SERVER>@>()= */
#define LOG_INN_SERVER		LOG_NEWS
    /* Facility all other programs should log under. */
    /* =()<#define LOG_INN_PROG		@<LOG_INN_PROG>@>()= */
#define LOG_INN_PROG		LOG_NEWS
    /* Flags to use in opening the logs; some programs add LOG_PID. */
    /* =()<#define L_OPENLOG_FLAGS		@<L_OPENLOG_FLAGS>@>()= */
#define L_OPENLOG_FLAGS		(LOG_CONS | LOG_NDELAY)
    /* Fatal error, program is about to exit. */
    /* =()<#define L_FATAL		@<L_FATAL>@>()= */
#define L_FATAL		LOG_CRIT
    /* Log an error that might mean one or more articles get lost. */
    /* =()<#define L_ERROR		@<L_ERROR>@>()= */
#define L_ERROR		LOG_ERR
    /* Informational notice, usually not worth caring about. */
    /* =()<#define L_NOTICE		@<L_NOTICE>@>()= */
#define L_NOTICE		LOG_WARNING
    /* A protocol trace. */
    /* =()<#define L_TRACE		@<L_TRACE>@>()= */
#define L_TRACE		LOG_DEBUG
    /* All incoming control commands (ctlinnd, etc). */
    /* =()<#define L_CC_CMD		@<L_CC_CMD>@>()= */
#define L_CC_CMD		LOG_INFO
