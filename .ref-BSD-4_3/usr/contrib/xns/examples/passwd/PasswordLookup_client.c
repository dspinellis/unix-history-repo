/*
 * Client stubs for PasswordLookup.
 */
#include "PasswordLookup_support.c"

LookupUidResults
LookupUid(_Connection, _BDTprocptr, uid)
	CourierConnection *_Connection;
	int (*_BDTprocptr)();
	Cardinal uid;
{
	LookupUidResults _Results;
	register Unspecified *_buf, *_bp;
	Boolean _errorflag;
	Cardinal _errtype;

	_buf = Allocate(1);
	_bp = _buf;
	_bp += externalize_Cardinal(&uid, _bp);
	SendCallMessage(_Connection, PasswordLookup_NUMBER, PasswordLookup_VERSION, 0, 1, _buf);
	Deallocate(_buf);
	MaybeCallBDTHandler(_Connection, _BDTprocptr);
	_bp = ReceiveReturnMessage(_Connection, &_errorflag);
	_buf = _bp;
	if (_errorflag) {
		_bp += internalize_Cardinal(&_errtype, _bp);
		switch (ERROR_OFFSET+_errtype) {
		case NoSuchUser:
			raise(ERROR_OFFSET+_errtype, 0);
			/*NOTREACHED*/
		default:
			/* don't know how to unpack this */
			raise(ERROR_OFFSET+_errtype, 0);
			/*NOTREACHED*/
		}
	} else
		_bp += internalize_LookupUidResults(&_Results, _bp);
	Deallocate(_buf);
	return (_Results);
}

LookupUserResults
LookupUser(_Connection, _BDTprocptr, user)
	CourierConnection *_Connection;
	int (*_BDTprocptr)();
	String user;
{
	LookupUserResults _Results;
	register Unspecified *_buf, *_bp;
	Boolean _errorflag;
	Cardinal _errtype;
	register LongCardinal _n = 0;

	_n += sizeof_String(&user);
	_buf = Allocate(_n);
	_bp = _buf;
	_bp += externalize_String(&user, _bp);
	SendCallMessage(_Connection, PasswordLookup_NUMBER, PasswordLookup_VERSION, 1, _n, _buf);
	Deallocate(_buf);
	MaybeCallBDTHandler(_Connection, _BDTprocptr);
	_bp = ReceiveReturnMessage(_Connection, &_errorflag);
	_buf = _bp;
	if (_errorflag) {
		_bp += internalize_Cardinal(&_errtype, _bp);
		switch (ERROR_OFFSET+_errtype) {
		case NoSuchUser:
			raise(ERROR_OFFSET+_errtype, 0);
			/*NOTREACHED*/
		case OtherError: {
			static T_cn754_2 _result;
			_bp += internalize_T_cn754_2(&_result, _bp);
			raise(ERROR_OFFSET+_errtype, (char *) &_result);
			/*NOTREACHED*/
			}
		default:
			/* don't know how to unpack this */
			raise(ERROR_OFFSET+_errtype, 0);
			/*NOTREACHED*/
		}
	} else
		_bp += internalize_LookupUserResults(&_Results, _bp);
	Deallocate(_buf);
	return (_Results);
}
