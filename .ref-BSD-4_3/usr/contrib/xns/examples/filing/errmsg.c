/* $Header: errmsg.c,v 2.0 85/11/21 07:22:44 jqj Exp $ */

/* $Log:	errmsg.c,v $
 * Revision 2.0  85/11/21  07:22:44  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  14:19:04  jqj
 * Initial revision
 * 
 */
#include "Filing4_defs.h"

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
		"credentialsInvalid",
		"verifierInvalid",
		"verifierExpiered",
		"verifierReused",
		"credentialsExpired",
		"inappropriateCredentials" };
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

	if (Code-ERROR_OFFSET >= 0 && Code-ERROR_OFFSET <= 16) {
		msg = errmsgs[Code-ERROR_OFFSET];
		problem = (((UndefinedErrorArgs *) Message)->problem);
	} else {
		msg = "";
		problem = 0;
	}
	switch (Code) {
	case AttributeTypeError:
	case AttributeValueError:
	case ControlTypeError:
	case ControlValueError:
	case ScopeTypeError:
	case ScopeValueError:
	case RangeError:
		problemstr = argproblems[problem];
		break;
	case AccessError:
		problemstr = accessproblems[problem];
		  break;
	case AuthenticationError:
		problemstr = authenticationproblems[problem];
		break;
	case ConnectionError:
		problemstr = connectionproblems[problem];
		break;
	case HandleError:
		problemstr = handleproblems[problem];
		break;
	case InsertionError:
		problemstr = insertionproblems[problem];
		break;
	case ServiceError:
		problemstr = serviceproblems[problem];
		break;
	case SessionError:
		problemstr = sessionproblems[problem];
		break;
	case SpaceError:
		problemstr = spaceproblems[problem];
		break;
	case TransferError:
		problemstr = transferproblems[problem];
		break;
	case UndefinedError:
		problemstr = tempbuf;
		sprintf(problemstr,"number %d",problem);
		break;
	case REJECT_ERROR:
		msg = "Courier REJECT";
		problem = (int) (((rejectionDetails *) Message)->designator);
		if (problem <= 3)
			problemstr = rejectproblem[problem];
		else problemstr = "unspecifiedError";
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
