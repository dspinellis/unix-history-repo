/*
 * Server for PasswordLookup.
 */
#include "PasswordLookup_support.c"
#include <except.h>

extern CourierConnection *_serverConnection;

extern LookupUidResults LookupUid();

server_LookupUid(_buf)
	register Unspecified *_buf;
{
	register Unspecified *_bp = _buf;
	register LongCardinal _n;
	Cardinal uid;
	LookupUidResults _Results;

	_bp += internalize_Cardinal(&uid, _bp);
	_Results = LookupUid(_serverConnection, 0, uid);
	_n = sizeof_LookupUidResults(&_Results);
	_bp = Allocate(_n);
	externalize_LookupUidResults(&_Results, _bp);
	SendReturnMessage(_n, _bp);
	Deallocate(_bp);
}

extern LookupUserResults LookupUser();

server_LookupUser(_buf)
	register Unspecified *_buf;
{
	register Unspecified *_bp = _buf;
	register LongCardinal _n;
	String user;
	LookupUserResults _Results;

	_bp += internalize_String(&user, _bp);
	_Results = LookupUser(_serverConnection, 0, user);
	_n = sizeof_LookupUserResults(&_Results);
	_bp = Allocate(_n);
	externalize_LookupUserResults(&_Results, _bp);
	SendReturnMessage(_n, _bp);
	Deallocate(_bp);
}

Server()
{
	Cardinal _procedure;
	register Unspecified *_buf;
	Cardinal _n;

	for (;;) {
		if (LookAheadCallMessage(PasswordLookup_NUMBER, PasswordLookup_VERSION))
			break;
		_buf = ReceiveCallMessage(&_procedure);
		DURING switch (_procedure) {
		case 1:
			server_LookupUser(_buf);
			break;
		case 0:
			server_LookupUid(_buf);
			break;
		default:
			NoSuchProcedureValue("PasswordLookup", _procedure);
			break;
		} HANDLER {
		    Deallocate(_buf);
		    switch (&Exception.Code) {
		    case OtherError:
			_n = sizeof_T_cn754_2((T_cn754_2 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn754_2((T_cn754_2*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case NoSuchUser:
			_buf = Allocate(0);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, 0, _buf);
			break;
		    default:
			_buf = Allocate(0);
			SendRejectMessage(unspecifiedError, 0, _buf);
			break;
		    }
		} END_HANDLER;
		Deallocate(_buf);
	}
}
