/* listen.h - listen demo definitions */

/* 
 * $Header: /f/osi/others/listen/RCS/listen.h,v 7.1 91/02/22 09:27:27 mrose Interim $
 *
 *
 * $Log:	listen.h,v $
 * Revision 7.1  91/02/22  09:27:27  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:19  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "psap2.h"
#include "ssap.h"
#include "tsap.h"
#include "logger.h"

#define	RMASK \
    "\020\01HALFDUPLEX\02DUPLEX\03EXPEDITED\04MINORSYNC\05MAJORSYNC\06RESYNC\
\07ACTIVITY\010NEGOTIATED\011CAPABILITY\012EXCEPTIONS\013TYPEDATA"


typedef struct sblk {
    int	    sb_sd;		/* session-descriptor */

    struct SSAPref sb_connect;	/* session connection reference */

    int	    sb_requirements;	/* session requirements */
    int	    sb_settings;	/* initial settings */
    int	    sb_owned;		/* session tokens we own */

    long    sb_ssn;		/* session serial number */
    long    sb_isn;		/* initial serial number */
}		*SB;


void	adios (), advise ();
