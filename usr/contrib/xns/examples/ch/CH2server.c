/*
 * Server for Clearinghouse.
 */
#include "Clearinghouse2.h"
#include <xnscourier/except.h>

extern CourierConnection *_serverConnection;

extern Clearinghouse2_RetrieveAddressesResults Clearinghouse2_RetrieveAddresses();

server_Clearinghouse2_RetrieveAddresses(_buf)
	register Unspecified *_buf;
{
	register Unspecified *_bp = _buf;
	register LongCardinal _n;
	Clearinghouse2_RetrieveAddressesResults _Results;

	_Results = Clearinghouse2_RetrieveAddresses(_serverConnection, 0);
	_n = sizeof_Clearinghouse2_RetrieveAddressesResults(&_Results);
	_bp = Allocate(_n);
	externalize_Clearinghouse2_RetrieveAddressesResults(&_Results, _bp);
	SendReturnMessage(_n, _bp);
	Deallocate(_bp);
}

Server(skipcount,skippedwords)
	int skipcount;
	Unspecified skippedwords[];
{
	Cardinal _procedure;
	register Unspecified *_buf;
	LongCardinal programnum;
	Cardinal versionnum;
	Cardinal _n;

	for (;;) {
		_buf = ReceiveCallMessage(&_procedure, skipcount, skippedwords);
		DURING switch (_procedure) {
		case 0:
			server_Clearinghouse2_RetrieveAddresses(_buf);
			break;
		default:
			NoSuchProcedureValue("Clearinghouse", _procedure);
			break;
		} HANDLER {
		    Deallocate(_buf);
		    switch (&Exception.Code) {
		    case Clearinghouse2_WrongServer:
			_n = sizeof_T_cn2_48((T_cn2_48 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_48((T_cn2_48*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case Clearinghouse2_UpdateError:
			_n = sizeof_T_cn2_47((T_cn2_47 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_47((T_cn2_47*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case Clearinghouse2_PropertyError:
			_n = sizeof_T_cn2_46((T_cn2_46 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_46((T_cn2_46*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case Clearinghouse2_CallError:
			_n = sizeof_T_cn2_45((T_cn2_45 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_45((T_cn2_45*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case Clearinghouse2_AuthenticationError:
			_n = sizeof_T_cn2_44((T_cn2_44 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_44((T_cn2_44*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    case Clearinghouse2_ArgumentError:
			_n = sizeof_T_cn2_43((T_cn2_43 *)Exception.Message);
			_buf = Allocate(_n);
			(void) externalize_T_cn2_43((T_cn2_43*)Exception.Message, _buf);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, _n, _buf);
			break;
		    default:
			_buf = Allocate(0);
			SendRejectMessage(unspecifiedError, 0, _buf);
			break;
		    }
		} END_HANDLER;
		Deallocate(_buf);
		for (;;) {
			skipcount = LookAheadCallMsg(&programnum, &versionnum,
						     skippedwords);
			if (skipcount < 0) return(0);	/* timed out */
			if (programnum != 2 || versionnum != 2)
				ExecCourierProgram(programnum, versionnum,
						   skipcount, skippedwords);
		}  /* can't exec that program */
	}
}
