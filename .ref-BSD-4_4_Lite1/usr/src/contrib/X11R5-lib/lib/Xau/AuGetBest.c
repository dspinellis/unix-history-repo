/*
 * Xau - X Authorization Database Library
 *
 * $XConsortium: AuGetBest.c,v 1.4 91/01/08 15:09:20 gildea Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include <X11/Xauth.h>
#include <X11/Xos.h>

static
binaryEqual (a, b, len)
register char	*a, *b;
register int	len;
{
    while (len--)
	if (*a++ != *b++)
	    return 0;
    return 1;
}

#if NeedFunctionPrototypes
Xauth *
XauGetBestAuthByAddr (
#if NeedWidePrototypes
    unsigned int	family,
    unsigned int	address_length,
#else
    unsigned short	family,
    unsigned short	address_length,
#endif
    _Xconst char*	address,
#if NeedWidePrototypes
    unsigned int	number_length,
#else
    unsigned short	number_length,
#endif
    _Xconst char*	number,
    int			types_length,
    char**		types,
    _Xconst int*	type_lengths)
#else
Xauth *
XauGetBestAuthByAddr (family, address_length, address,
			  number_length, number,
			  types_length, types, type_lengths)
    unsigned short	family;
    unsigned short	address_length;
    char		*address;
    unsigned short	number_length;
    char		*number;
    int			types_length;
    char		**types;
    int			*type_lengths;
#endif
{
    FILE    *auth_file;
    char    *auth_name;
    Xauth   *entry;
    Xauth   *best;
    int	    best_type;
    int	    type;

    auth_name = XauFileName ();
    if (!auth_name)
	return 0;
    if (access (auth_name, R_OK) != 0)		/* checks REAL id */
	return 0;
    auth_file = fopen (auth_name, "r");
    if (!auth_file)
	return 0;
    best = 0;
    best_type = types_length;
    for (;;) {
	entry = XauReadAuth (auth_file);
	if (!entry)
	    break;
	/*
	 * Match when:
	 *   either family or entry->family are FamilyWild or
	 *    family and entry->family are the same
	 *  and
	 *   either address or entry->address are empty or
	 *    address and entry->address are the same
	 *  and
	 *   either number or entry->number are empty or
	 *    number and entry->number are the same
	 *  and
	 *   name matches one of the specified names, or no names
	 *    were specified
	 */

	if ((family == FamilyWild || entry->family == FamilyWild ||
	     (entry->family == family &&
	      address_length == entry->address_length &&
	      binaryEqual (entry->address, address, (int)address_length))) &&
	    (number_length == 0 || entry->number_length == 0 ||
	     (number_length == entry->number_length &&
	      binaryEqual (entry->number, number, (int)number_length))))
	{
	    if (best_type == 0)
	    {
		best = entry;
		break;
	    }
	    for (type = 0; type < best_type; type++)
		if (type_lengths[type] == entry->name_length &&
		    !(strncmp (types[type], entry->name, entry->name_length)))
		{
		    break;
		}
	    if (type < best_type)
	    {
		if (best)
		    XauDisposeAuth (best);
		best = entry;
		best_type = type;
		if (type == 0)
		    break;
		continue;
	    }
	}
	XauDisposeAuth (entry);
    }
    (void) fclose (auth_file);
    return best;
}
