/*
** Various new resource record type and class values.
**
**	They belong in <arpa/nameser.h>
**
**	@(#)type.h              e07@nikhef.nl (Eric Wassenaar) 930919
*/

/* missing in some old versions */

#ifndef T_TXT
#define T_TXT		16
#endif

/* defined per RFC 1183 */

#ifndef T_RP
#define T_RP		17
#endif

#ifndef T_AFSDB
#define T_AFSDB		18
#endif

#ifndef T_X25
#define T_X25		19
#endif

#ifndef T_ISDN
#define T_ISDN		20
#endif

#ifndef T_RT
#define T_RT		21
#endif

/* defined per RFC 1348 */

#ifndef T_NSAP
#define T_NSAP		22
#endif

#ifndef T_NSAPPTR
#define T_NSAPPTR	23
#endif

/* never used in practice */

#ifndef C_CSNET
#define C_CSNET		2
#endif

/* missing in some old versions */

#ifndef C_HS
#define C_HS		4
#endif
