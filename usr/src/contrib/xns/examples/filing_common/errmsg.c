/* $Header: errmsg.c,v 2.3 87/05/12 11:46:32 ed Exp $ */

/* $Log:	errmsg.c,v $
 * Revision 2.3  87/05/12  11:46:32  ed
 * Change to use FilingSubset1_defs.h, new AuthenticationError problems.
 * 
 * Revision 2.2  87/03/07  14:44:42  jqj
 * set problem correctly.  Cardinal != Enum on most UNIX systems
 * 
 * Revision 2.1  86/06/02  07:10:30  jqj
 * print more information on unspecifiedError
 * 
 * Revision 2.0  85/11/21  07:22:44  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  14:19:04  jqj
 * Initial revision
 * 
 */
#include "FilingSubset1_defs.h"

FilingErrMsg(Code, Message)
	int Code;
	char *Message;
{
	static char *errmsgs[] = {
		"AttributeTypeError",
		"AttributeValueError",
		"ControlTypeError",
		"ControlValueError",
		"ScopeTypeError",
		"ScopeValueError",
		"AccessError",
		"AuthenticationError",
		"ConnectionError",
		"HandleError",
		"InsertionError",
		"ServiceError",
		"SessionError",
		"SpaceError",
		"TransferError",
		"UndefinedError",
		"RangeError" };
	static char *argproblems[] = {
	"illegal",
		"disallowed",
		"unreasonable",
		"unimplemented",
		"duplicated",
		"missing" };
	static char *accessproblems[] = {
		"accessRightsInsufficient",
		"accessRightsIndeterminate",
		"fileChanged",
		"fileDamaged",
		"fileInUse",
		"fileNotFound",
		"fileOpen" };
	static char *connectionproblems[] = {
		"noRoute",
		"noResponse",
		"transmissionHardware",
		"transportTimeout",
		"tooManyLocalConnections",
		"tooManyRemoteConnections",
		"missingCourier",
		"missingProgram",
		"missingProcedure",
		"protocolMismatch",
		"parameterInconsistency",
		"invalidMessage",
		"returnTimedOut",
		"otherCallProblem" };
	static char* handleproblems[] = {
		"invalid",
		"nullDisallowed",
		"directoryRequired" };
	static char *insertionproblems[] = {
		"positionUnavailable",
		"fileNotUnique",
		"loopInHierarchy" };
	static char *serviceproblems[] = {
		"cannotAuthenticate",
		"serviceFull",
		"serviceUnavailable",
		"sessionInUse" };
	static char *sessionproblems[] = {
		"tokenInvalid",
		"serviceAlreadySet" };
	static char *spaceproblems[] = {
		"allocationExceeded",
		"attributeAreadFull",
		"mediumFull" };
	static char *transferproblems[] = {
		"aborted",
		"checksumIncorrect",
		"formatIncorrect",
		"noRendevous",
		"wrongDirection" };
	static char *authenticationproblems[] = {
		"primaryCredentialsInvalid",
		"verifierInvalid",
		"verifierExpired",
		"verifierReused",
		"primaryCredentialsExpired",
		"inappropriatePrimaryCredentials",
		"secondaryCredentialsRequired",
		"secondaryCredentialsTypeInvalid",
		"secondaryCredentialsValueInvalid" };
	static char *rejectproblem[] = {
		"noSuchProgramNumber",
		"noSuchVersionNumber",
		"noSuchProcedureValue",
		"invalidArgument" };
	char *msg, *problemstr;
	int problem;
	char tempbuf[40];

	if (Code < 1000) {
		if (Message != (char *) 0)
		  printf("ERROR: %s\n", Message);
		return;
	}

	msg = "";
	problem = 0;
	if (Code-ERROR_OFFSET >= 0 && Code-ERROR_OFFSET <= 16) {
		msg = errmsgs[Code-ERROR_OFFSET];
	}
	switch (Code) {
	case AttributeTypeError:
	case AttributeValueError:
	case ControlTypeError:
	case ControlValueError:
	case ScopeTypeError:
	case ScopeValueError:
/* the following fails because "type" is defined as "Filing4_type".  Argh!!
/*		problem = (int) (((ScopeTypeErrorArgs *) Message)->problem);
/*		problemstr = sprintf(tempbuf,"problem: %s; type: %d",
/*				argproblems[problem],
/*				((ScopeTypeErrorArgs *) Message)->type);
/*		break;
 */
	case RangeError:
		problem = (int) (((RangeErrorArgs *) Message)->problem);
		problemstr = argproblems[problem];
		break;
	case AccessError:
		problem = (int) (((AccessErrorArgs *) Message)->problem);
		problemstr = accessproblems[problem];
		  break;
	case AuthenticationError:
		problem = (int) (((AuthenticationErrorArgs *) Message)->problem);
		problemstr = authenticationproblems[problem];
		break;
	case ConnectionError:
		problem = (int) (((ConnectionErrorArgs *) Message)->problem);
		problemstr = connectionproblems[problem];
		break;
	case HandleError:
		problem = (int) (((HandleErrorArgs *) Message)->problem);
		problemstr = handleproblems[problem];
		break;
	case InsertionError:
		problem = (int) (((InsertionErrorArgs *) Message)->problem);
		problemstr = insertionproblems[problem];
		break;
	case ServiceError:
		problem = (int) (((ServiceErrorArgs *) Message)->problem);
		problemstr = serviceproblems[problem];
		break;
	case SessionError:
		problem = (int) (((SessionErrorArgs *) Message)->problem);
		problemstr = sessionproblems[problem];
		break;
	case SpaceError:
		problem = (int) (((SpaceErrorArgs *) Message)->problem);
		problemstr = spaceproblems[problem];
		break;
	case TransferError:
		problem = (int) (((TransferErrorArgs *) Message)->problem);
		problemstr = transferproblems[problem];
		break;
	case UndefinedError:
		problem = (int) (((UndefinedErrorArgs *) Message)->problem);
		problemstr = tempbuf;
		sprintf(problemstr,"number %d",problem);
		break;
	case REJECT_ERROR:
		msg = "Courier REJECT";
		problem = (int) (((rejectionDetails *) Message)->designator);
		if (problem <= 3)
			problemstr = rejectproblem[problem];
		else {
			problemstr = tempbuf;
			sprintf(problemstr,"unspecifiedError (%d)", problem);
		}
		break;
	case PROTOCOL_VIOLATION:
		problemstr = "Courier protocol violation";
		break;
	default:
		problemstr = tempbuf;
		sprintf(problemstr,"unexpected error number %d", Code);
		break;
	}
	printf("ERROR: %s, %s\n", msg, problemstr);
}
