/* chrcnv.c - character conversion table */

#ifndef lint
static char *rcsid = "$Header: /f/osi/compat/RCS/chrcnv.c,v 7.1 91/02/22 09:15:04 mrose Interim $";
#endif

/*
 * $Header: /f/osi/compat/RCS/chrcnv.c,v 7.1 91/02/22 09:15:04 mrose Interim $
 *
 *
 * $Log:	chrcnv.c,v $
 * Revision 7.1  91/02/22  09:15:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:22:58  mrose
 * Release 6.0
 * 
 */


/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include "general.h"

/*  */

char                                   /* character conversion table   */
	chrcnv[] =                     /*   lower to upper case letters */
{
    '\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7',
    '\10', '\t', '\n', '\13', '\14', '\r', '\16', '\17',
    '\20', '\21', '\22', '\23', '\24', '\25', '\26', '\27',
    '\30', '\31', '\32', '\33', '\34', '\35', '\36', '\37',
    ' ', '!', '"', '#', '$', '%', '&', '\47',
    '(', ')', '*', '+', ',', '-', '.', '/',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', ':', ';', '<', '=', '>', '?',
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
    '`', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '{', '|', '}', '~', '\177',
    '\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7',
    '\10', '\t', '\n', '\13', '\14', '\r', '\16', '\17',
    '\20', '\21', '\22', '\23', '\24', '\25', '\26', '\27',
    '\30', '\31', '\32', '\33', '\34', '\35', '\36', '\37',
    ' ', '!', '"', '#', '$', '%', '&', '\47',
    '(', ')', '*', '+', ',', '-', '.', '/',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', ':', ';', '<', '=', '>', '?',
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
    '`', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '{', '|', '}', '~', '\177'
};

