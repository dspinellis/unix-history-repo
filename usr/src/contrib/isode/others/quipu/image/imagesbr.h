/* imagesbr.h - include file for image subroutines */

/* 
 * $Header: /f/osi/others/quipu/image/RCS/imagesbr.h,v 7.1 91/02/22 09:33:22 mrose Interim $
 *
 *
 * $Log:	imagesbr.h,v $
 * Revision 7.1  91/02/22  09:33:22  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:00:00  mrose
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


#include "psap.h"
#include "isoaddrs.h"
#include "logger.h"

/*  */

extern int   debug;
extern int   errsw;

/* GENERAL */

extern int   recording;
extern LLog *pgm_log;


/* AKA */

void	init_aka ();

/* DIRECTORY */


/* IMAGE */

struct type_IMAGE_Image {
    int     width;

    int     height;

    struct qbuf *data;
};

struct type_IMAGE_Image *fetch_image ();


/* ERRORS */

void	adios (), advise ();


/* MISC */

char   *strdup ();
