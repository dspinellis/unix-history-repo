/* ftamfdf.c - FDF support */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamfdf.c,v 7.1 91/02/22 09:22:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamfdf.c,v 7.1 91/02/22 09:22:53 mrose Interim $
 *
 *
 * $Log:	ftamfdf.c,v $
 * Revision 7.1  91/02/22  09:22:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:37  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include "fpkt.h"

/*  */

int	fdf_p2names (fd, bits, names, fti)
int	fd;
PE	bits;
int    *names;
struct FTAMindication *fti;
{
    register struct ftamblk *fsb;

    if ((fsb = findfsblk (fd)) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
			 "invalid ftam descriptor");

    return fpm2bits (fsb, fname_pairs, bits, names, fti);
}

/*  */

int	fdf_names2p (fd, names, bits, fti)
int	fd;
int     names;
PE     *bits;
struct FTAMindication *fti;
{
    register struct ftamblk *fsb;

    if ((fsb = findfsblk (fd)) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
			 "invalid ftam descriptor");

    if ((*bits) = bits2fpm (fsb, fname_pairs, names, fti))
	return OK;
    return NOTOK;
}

/*  */

int	fdf_attrs2d (fd, fa, attrs, fti)
int	fd;
struct FTAMattributes *fa;
struct type_FTAM_Read__Attributes **attrs;
struct FTAMindication *fti;
{
    register struct ftamblk *fsb;

    if ((fsb = findfsblk (fd)) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
			 "invalid ftam descriptor");

    if ((*attrs) = attr2fpm (fsb, fa, fti))
	return OK;
    return NOTOK;
}

/*  */

int	fdf_d2attrs (fd, attrs, fa, fti)
int	fd;
struct type_FTAM_Read__Attributes *attrs;
struct FTAMattributes *fa;
struct FTAMindication *fti;
{
    register struct ftamblk *fsb;

    if ((fsb = findfsblk (fd)) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
			 "invalid ftam descriptor");

    return fpm2attr (fsb, attrs, fa, fti);
}
