#ifndef lint
static char *rcsid = "$Header: VPsupport.c,v 1.3 87/04/16 15:24:53 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	VPsupport.c,v $
 * Revision 1.3  87/04/16  15:24:53  ed
 * Some machines prefer large arrays to be static.
 * 
 * Revision 1.2  87/04/01  10:08:39  ed
 * Added GetSizeAttribute.
 * 
 * Revision 1.1  87/03/18  08:42:28  ed
 * Initial revision
 * 
 */

/*
 * support routines for dealing with Viewpoint related files on Unix
 * files are formatted as
 *	header ("viewpoint-files/xerox")
 *	int attribute_length (byte length of attribute sequence to follow)
 *	Sequence of attributes
 *	file content
 *		will be serialized data if isDirectory value is TRUE
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#endif FILING6
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#endif FILINGSUBSET1
#include <xnscourier/filetypes.h>

Boolean isValidAttr(t)
FILING_AttributeType t;
{
	/* 
	 * Following are service specific and therefore irrelevant
	 *	(what are 24 and 25 ??)
	 */
	if ( (t == FILING_fileID) || (t == FILING_numberOfChildren) ||
			(t == FILING_parentID) || (t == FILING_storedSize) ||
			(t == FILING_subtreeSize) || (t == 24) || (t == 25) ) 
			return(FALSE);

	/*
	 * Following are retained locally and may be in conflict
	 * (i.e., name/pathname/position/version) so they are in effect replaced
	 */
	if ( (t == FILING_createdOn) || (t == FILING_dataSize) ||
			(t == FILING_isDirectory) ||
			(t == FILING_modifiedOn) || (t == FILING_name) ||
			(t == FILING_pathname) || (t == FILING_position) ||
			(t == FILING_type) || (t == FILING_version) )
		return(FALSE);

	return(TRUE);
}

SaveExtendedAttributes(file, attr)
FILE *file;
FILING_AttributeSequence attr;

{
	int i, count, pos;
	int attr_size;
	Cardinal sequence_count;
       	FILING_AttributeType t;
	static Unspecified buffer[MAX_SEQUENCE_LENGTH];

	attr_size= sequence_count= 0;

	fprintf(file, VPHDR);
	attr_size= FILING_sizeof_AttributeSequence(&attr) * sizeof(Unspecified);
	fwrite(&attr_size, sizeof(attr_size), 1, file);
	fwrite(&attr.length, sizeof(Cardinal), 1, file);

	for ( i= 0; i < attr.length; i++ ) {
		t= attr.sequence[i].type;

		count= FILING_externalize_Attribute(&attr.sequence[i], buffer);
		fwrite(buffer, sizeof(Unspecified), count, file);
	}

	return(attr_size);
}

GetExtendedAttributes(file,attrptr)
FILE *file;
char **attrptr;
{
	int size, count;
	char *buffer, *malloc();
	char hdr[50];

	fread(hdr, sizeof(char), strlen(VPHDR), file);
	fread(&size, sizeof(size), 1, file);

	if ( (buffer= malloc(size)) == 0 ) {
		return(0);
	}

	if ( (count= fread(buffer, sizeof(char), size, file)) == 0 ) {
		return(0);
	}

	if ( count != size ) {
		return(0);
	}
	*attrptr= buffer;
	return(count);
}

FreeExtendedAttributes(attrptr)
char *attrptr;
{
	if ( attrptr != 0 )
		free(attrptr);
}

AddAllExtendedAttributes(file, attrseq)
FILE *file;
FILING_AttributeSequence *attrseq;
{
	int attr_size, size, index, i;
	Unspecified *buffer, *unptr;
	Cardinal sequence_count= 0;
	FILING_AttributeType t;

	if ( ftell(file) != 0 )
		rewind(file);			/* to be sure */
	
	if ( (attr_size= GetExtendedAttributes(file, &buffer)) == 0 ) {
		return(0);
	}

	unptr= (Unspecified *) buffer;
	sequence_count= *unptr;
	unptr ++;

	index= attrseq->length;

	for ( i= 0; i < sequence_count ; i++ ) {
		size= FILING_internalize_Attribute(&(attrseq->sequence[index]), unptr);
		t= attrseq->sequence[index].type;
		if ( isValidAttr(t) )
			index++;

		unptr += size;
	}

	attrseq->length= index;

	FreeExtendedAttributes(buffer);
	return(-1);
}

AddExtendedStoreAttributes(file, attrseq)
FILE *file;
FILING_AttributeSequence *attrseq;

{
        int attr_size, size, index, i;
        Unspecified *buffer, *unptr;
	Cardinal sequence_count= 0;
	FILING_AttributeType t;

	if ( ftell(file) != 0 )
	        rewind(file);                           /* to be sure */

        if ( (attr_size= GetExtendedAttributes(file,&buffer)) == 0 )
                return(0);

        unptr= (Unspecified *) buffer;
	sequence_count= *unptr;
	unptr++;

        index= attrseq->length;

	for ( i= 0; i < sequence_count; i++ ) {
                size= FILING_internalize_Attribute(&(attrseq->sequence[index]), unptr);
		t= attrseq->sequence[index].type;

		if ( (isValidAttr(t)) && (t != FILING_modifiedBy) &&
				(t != FILING_readBy) && (t != FILING_readOn) &&
				(t != FILING_type) ) {
			index++;
                }
                unptr+= size;
        }

        attrseq->length= index;

        FreeExtendedAttributes(buffer);
        return(-1);
}

AddExtendedDeserializeAttributes(file, attrseq)
FILE *file;
FILING_AttributeSequence *attrseq;

{
        int attr_size, size, index, i;
        Unspecified *buffer, *unptr;
	Cardinal sequence_count= 0;
	FILING_AttributeType t;

	if ( ftell(file) != 0 )
	        rewind(file);                           /* to be sure */

        if ( (attr_size= GetExtendedAttributes(file,&buffer)) == 0 )
                return(0);

        unptr= buffer;
	sequence_count= *unptr;
	unptr++;

        index= attrseq->length;

	for ( i= 0; i < sequence_count; i++ ) {
                size= FILING_internalize_Attribute(&(attrseq->sequence[index]), unptr);
		t= attrseq->sequence[index].type;
        /*
         * omit attributes which are illegal on Deserialize
         */

                if ( (isValidAttr(t)) && (t != FILING_checksum) &&
                                (t != FILING_childrenUniquelyNamed) &&
                                (t != FILING_createdBy) &&
                                (t != FILING_modifiedBy) &&
                                (t != FILING_ordering) &&
				(t != FILING_readBy) &&
                                (t != FILING_readOn) ) {
                        index++;
                }
                unptr+= size;
        }

        attrseq->length= index;

        FreeExtendedAttributes(buffer);
        return(-1);
}

Boolean GetDirectoryAttribute(file)
FILE *file;

{
	int i;
	Unspecified *buffer, *unptr;
	int attr_size;
	Boolean dirval;
	Cardinal sequence_count= 0;
	FILING_Attribute attribute;

	dirval= FALSE;

	if ( ftell(file) != 0 )
		rewind(file);			/* back to beginning */

	if ( (attr_size= GetExtendedAttributes(file,&buffer)) == 0 ) {
		return(FALSE);
	}

	unptr= buffer;
	sequence_count= *unptr;
	unptr++;

	for ( i= 0; i < sequence_count; i++ ){
		unptr+= FILING_internalize_Attribute(&attribute, unptr);
		if ( attribute.type == FILING_isDirectory ) {
			dirval= AttrToBoolean(&attribute);
			break;
		}
	}

	FreeExtendedAttributes(buffer);
	return(dirval);
}

LongCardinal GetTypeAttribute(file)
FILE *file;

{
	int i;
	Unspecified *buffer, *unptr;
	LongCardinal filetype, attr_size;
	FILING_Attribute attribute;
	Cardinal sequence_count= 0;

	filetype= TYPE_I;

	if ( ftell(file) != 0 )
		rewind(file);			/* back to beginning */

	if ( (attr_size= GetExtendedAttributes(file, &buffer)) == 0 ) {
		return(TYPE_I);
	}

	unptr= buffer;
	sequence_count= *unptr;
	unptr++;

	for ( i= 0; i < sequence_count; i++ ) {
		unptr+= FILING_internalize_Attribute(&attribute, unptr);
		if ( attribute.type == FILING_type ) {
			filetype= AttrToLongCardinal(&attribute);
			break;
		}
	}

	FreeExtendedAttributes(buffer);
	return(filetype);
}

LongCardinal GetSizeAttribute(file)
FILE *file;

{
	int i;
	Unspecified *buffer, *unptr;
	LongCardinal filesize, attr_size;
	FILING_Attribute attribute;
	Cardinal sequence_count= 0;

	filesize= 0;

	if ( ftell(file) != 0 )
		rewind(file);			/* back to beginning */

	if ( (attr_size= GetExtendedAttributes(file, &buffer)) == 0 ) {
		return(filesize);
	}

	unptr= buffer;
	sequence_count= *unptr;
	unptr++;

	for ( i= 0; i < sequence_count; i++ ) {
		unptr+= FILING_internalize_Attribute(&attribute, unptr);
		if ( attribute.type == FILING_dataSize ) {
			filesize= AttrToLongCardinal(&attribute);
			break;
		}
	}

	FreeExtendedAttributes(buffer);
	return(filesize);
}

PositionAfterExtendedAttributes(file)
FILE *file;
{
	int size;

	fseek(file, strlen(VPHDR), 0);
	fread(&size, sizeof(size), 1, file);
	return (fseek(file, (long) size, 1));

}
