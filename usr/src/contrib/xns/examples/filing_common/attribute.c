#ifndef lint
static char *rcsid = "$Header: attribute.c,v 1.2 87/03/31 14:32:17 ed Exp $";
#endif lint

/* $Log:	attribute.c,v $
 * Revision 1.2  87/03/31  14:32:17  ed
 * Added Filing version 5 support.
 * 
 * Revision 1.1  87/01/14  11:25:53  ed
 * Initial revision
 * 
 */

#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#include "clearinghouseV2.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#include "clearinghouseV2.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#include "clearinghouseV3.h"
#endif FILING6
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#include "clearinghouseV3.h"
#endif FILINGSUBSET1

StringToAttr(str, attr)
	char *str;
	FILING_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_String(&str, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

char *
AttrToString(attr)
	FILING_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;
	char *strval;

	externalize_Sequence_of_Unspecified(&(attr->value), buf);
	bp = buf;
	bp += internalize_Cardinal(&len, bp);
	bp += internalize_String(&strval, bp);	
	return(strval);
}

UserToAttr(id, attr)
	CLEARINGHOUSE_Name id;
	FILING_Attribute *attr;
{
	Unspecified buf[2049], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = CLEARINGHOUSE_externalize_Name(&id, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

LongCardinalToAttr(val, attr)
	LongCardinal val;
	FILING_Attribute *attr;
{
	Unspecified buf[3], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_LongCardinal(&val, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

LongCardinal
AttrToLongCardinal(attr)
	FILING_Attribute *attr;
{
	Unspecified buf[2];
	LongCardinal result;

	(void) externalize_Unspecified(attr->value.sequence, buf);
	(void) externalize_Unspecified((attr->value.sequence)+1, buf+1);
	(void) internalize_LongCardinal(&result, buf);
	return(result);
}

BooleanToAttr(val, attr)
	int val;
	FILING_Attribute *attr;
{
	Boolean boolval;
	Unspecified buf[3], *bp;
	Cardinal len;

	boolval = (Boolean) val;
	bp = buf + sizeof_Cardinal(len);
	len = externalize_Boolean(&boolval, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

int
AttrToBoolean(attr)
	FILING_Attribute *attr;
{
	Unspecified buf[1];
	Boolean result;

	(void) externalize_Unspecified(attr->value.sequence, buf);
	(void) internalize_Boolean(&result, buf);
	return(result);
}

CardinalToAttr(val, attr)
	Cardinal val;
	FILING_Attribute *attr;
{
	Unspecified buf[3], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = externalize_Cardinal(&val, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

Cardinal
AttrToCardinal(attr)
	FILING_Attribute *attr;
{
    	Unspecified buf[2];
	Cardinal result;
	(void) externalize_Unspecified(attr->value.sequence, buf);
	(void) internalize_Cardinal(&result, buf);
	return(result);
}

FileIDToAttr(value, attr)
	Cardinal value[];
	FILING_Attribute *attr;
{
	Unspecified buf[6], *bp;
	Cardinal len;

	bp = buf + sizeof_Cardinal(len);
	len = FILING_externalize_FileID(value, bp);
	(void) externalize_Cardinal(&len, buf);
	internalize_Sequence_of_Unspecified(&(attr->value), buf);
	return;
}

Unspecified *
AttrToFileID(attr)
	FILING_Attribute *attr;
{
	Unspecified *bp;
	Unspecified buf[6];

	bp= Allocate(FILING_sizeof_FileID(0));

	(void) FILING_externalize_FileID(attr->value.sequence,buf);
	(void) FILING_internalize_FileID(bp,buf);

	return(bp);
}

