/* tws.h - header file for libtws date/time library */


/* Definition of the tws data structure. */

struct tws {
    int     tw_sec;
    int     tw_min;
    int     tw_hour;

    int     tw_mday;
    int     tw_mon;
    int     tw_year;

    int     tw_wday;
    int     tw_yday;

    int     tw_zone;

    long    tw_clock;

    int     tw_flags;
#define TW_NULL 0x0000
#define TW_SDAY 0x0007		/* how day-of-week was determined */
#define   TW_SNIL 0x0000	/*   not given */
#define   TW_SEXP 0x0001	/*   explicitly given */
#define   TW_SIMP 0x0002	/*   implicitly given */
#define TW_DST  0x0010		/* daylight savings time */
#define TW_ZONE 0x0020		/* use numeric timezones only */
#define TW_JUNK 0x0040		/* date string contained junk */
};


/* Declarations of routines. */

void twscopy( );
	/* twscopy( &totws, &fromtws ) copies a tws */
int twsort( );
	/* twsort( &tws1, &tws2 ) compares two tws's: 1 means tws1 is
	   later; -1 means tws1 is earlier; 0 means they are equal */
long twclock( );
	/* twclock( &tws ) turns a tws into a time(3)-style clock value */
long twjuliandate( );
	/* twjuliandate( &tws ) returns the Julian day number of a tws */
long twsubtract( );
	/* twsubtract( &tws1, &tws2 ) returns seconds of difference */

/* These routines are functionally similar to the ctime(3) routines
   in the standard Unix library. */
char *dctime( );
	/* dctime( &tws ) returns a string for the date/time passed in */
struct tws *dlocaltime( );
	/* dlocaltime( &clock ) turns a time(3) clock value into a tws */
struct tws *dgmtime( );
	/* dgmtime( &clock ) turns a time(3) clock value into a tws */
char *dasctime( );
	/* dasctime( &tws, flags ) turns a tws into a string */
char *dtimezone( );
	/* dtimezone( offset, flags ) returns the name of the time zone */

char *dtimenow( );
	/* dtimenow( ) returns a string for the current date/time */

struct tws *dparsetime( );
	/* dparsetime( &str ) turns a string into a tws */

struct tws *dtwstime( );
	/* dtwstime( ) returns a tws for the current date/time */

#ifdef ATZ
#define dtime(cl) dasctime( dlocaltime( cl ), TW_NULL )
#else ATZ
#define dtime(cl) dasctime( dlocaltime( cl ), TW_ZONE )
#endif ATZ

#define dtwszone(tw) dtimezone( tw -> tw_zone, tw -> tw_flags )


extern char   *tw_dotw[], *tw_ldotw[], *tw_moty[];
