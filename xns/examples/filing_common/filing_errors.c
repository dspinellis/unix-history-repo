#ifndef lint
static char *rcsid = "$Header: filing_errors.c,v 1.2 87/03/31 14:34:45 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	filing_errors.c,v $
 * Revision 1.2  87/03/31  14:34:45  ed
 * Added Filing version 5 support.
 * Corrected inconsistent use of prob in many routines.
 * 
 * Revision 1.1  87/01/14  11:25:56  ed
 * Initial revision
 * 
 *
 */

#include <sys/types.h>
#include <netns/ns.h>
#include <netns/sp.h>
#ifdef FILING4
#include "filingV4.h"
#include "authenticationV2.h"
#endif FILING4
#ifdef FILING5
#include "filingV5.h"
#include "authenticationV2.h"
#endif FILING5
#ifdef FILING6
#include "filingV6.h"
#endif FILING6
#ifdef FILINGSUBSET1
#include "filingsubsetV1.h"
#endif FILINGSUBSET1


extern CourierConnection *_serverConnection;
extern BDTabort_expected;

ReturnAccessError(prob)
FILING_AccessProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_AccessError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}

ReturnAttributeTypeError(prob,type)
FILING_ArgumentProblem prob;
LongCardinal type;
{
	LongCardinal error;
	FILING_AttributeTypeErrorArgs attribute_type_error;

	error= FILING_AttributeTypeError;
	attribute_type_error.problem= prob;
	attribute_type_error.type= type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&attribute_type_error);
	/* NOT REACHED */
}

ReturnAttributeValueError(prob,type)
FILING_ArgumentProblem prob;
LongCardinal type;
{
	LongCardinal error;
	static FILING_AttributeValueErrorArgs attribute_value_error;

	error= FILING_AttributeValueError;
	attribute_value_error.problem= prob;
	attribute_value_error.type= type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&attribute_value_error);
	/* NOT REACHED */
}

#if FILING4 | FILING5
ReturnAuthenticationError(prob)
AUTHENTICATION_Problem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_AuthenticationError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}

#else FILING4 | FILING5 	(the following is for FILING6 or FILINGSUBSET1)

ReturnAuthenticationError(prob)
FILING_AuthenticationProblem prob;
{
	LongCardinal error;
	static FILING_AuthenticationErrorArgs authentication_error;
	static FILING_SecondaryItemType required_types[2];
	static FILING_SecondaryType required_secondaries= { 2, required_types };

	required_types[0]= FILING_userName;
	required_types[1]= FILING_userPassword;
	error= FILING_AuthenticationError;
	authentication_error.problem= prob;
	authentication_error.type= required_secondaries;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&authentication_error);
	/* NOT REACHED */
}
#endif FILING4 | FILING5

ReturnControlTypeError(prob,type)
FILING_ArgumentProblem prob;
Cardinal type;
{
	static FILING_ControlTypeErrorArgs control_type_error;
	LongCardinal error;

	error= FILING_ControlTypeError;
	control_type_error.problem= prob;
	control_type_error.type= (FILING_ControlType)type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&control_type_error);
	/* NOT REACHED */
}

ReturnControlValueError(prob,type)
FILING_ArgumentProblem prob;
Cardinal type;
{
	static FILING_ControlValueErrorArgs control_value_error;
	LongCardinal error;

	error= FILING_ControlValueError;
	control_value_error.problem= prob;
	control_value_error.type= (FILING_ControlType)type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&control_value_error);
	/* NOT REACHED */
}

ReturnHandleError(prob)
FILING_HandleProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_HandleError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
} 

ReturnInsertionError(prob)
FILING_InsertionProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_InsertionError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
} 

ReturnScopeTypeError(prob,type)
FILING_ArgumentProblem prob;
Cardinal type;
{
	static FILING_ScopeTypeErrorArgs scoptype_error;
	LongCardinal error;

	error= FILING_ScopeTypeError;
	scoptype_error.problem= prob;
	scoptype_error.type= (FILING_ScopeType)type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&scoptype_error);
	/* NOT REACHED */
}

ReturnScopeValueError(prob,type)
FILING_ArgumentProblem prob;
Cardinal type;
{
	static FILING_ScopeValueErrorArgs scope_value_error;
	LongCardinal error;

	error= FILING_ScopeValueError;
	scope_value_error.problem= prob;
	scope_value_error.type= (FILING_ScopeType)type;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&scope_value_error);
	/* NOT REACHED */
}

ReturnServiceError(prob)
FILING_ServiceProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_ServiceError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}

ReturnSessionError(prob)
FILING_SessionProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_SessionError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}

ReturnSpaceError(prob)
FILING_SpaceProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_SpaceError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
} 

ReturnTransferError(prob)
FILING_TransferProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_TransferError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}

ReturnUndefinedError(prob)
FILING_UndefinedProblem prob;
{
	LongCardinal error;
	Cardinal problem;

	error= FILING_UndefinedError;
	problem= (Cardinal) prob;
	if ( BDTabort_expected )
		BDTabort(_serverConnection);
	raise(error,&problem);
	/* NOT REACHED */
}



