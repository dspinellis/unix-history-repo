/*
 *  solIo.c
 *
 *	remade by A.Fujita, DEC-16-1992
 */

#include "sol.h"
#include "solKbd.h"

int lastEventTime = 0;

static void (* solIoHandler)(); 

void
solSetIoHandler( ioHandler )
void (* ioHandler)(); 
{
#ifdef	DEBUG
	fprintf(stderr, "solSetIoHandler\t[solIo.c]\tStart\n");
#endif

	solIoHandler = ioHandler;

#ifdef	DEBUG
	fprintf(stderr, "solSetIoHandler\t[solIo.c]\tEnd\n");
#endif
}

void
solWakeupProc(blockData, result, pReadmask)
pointer blockData;
unsigned long   result;
pointer pReadmask;
{
	long devicesReadable[mskcnt];

	if(result <= 0) return;

	MASKANDSETBITS(devicesReadable, LastSelectMask, EnabledDevices);

	if(ANYSET(devicesReadable)) {
		(* solIoHandler)();
	}
}

void
ProcessInputEvents()
{
	mieqProcessInputEvents();
	miPointerUpdate();
}

solDevEvt	evtbuf[MAXEVENTS];

void
solEnqueueEvents()
{
	DevicePtr     	pPtr;
	DevicePtr     	pKbd;
	struct msdata 	*ptrEvents;	
	unsigned char 	*KbdEvents;
	register int	numEvt;
	register solDevEvtPtr evp;
	Bool	 	ptrRetry, kbdRetry;			
	
	pPtr = LookupPointerDevice();
	pKbd = LookupKeyboardDevice();

	evp = evtbuf;

	numEvt = solDevGetEvents(evp);

	while (numEvt-- > 0) {
		if (evp->type == EvtKey) {
			solKbdEnqueueEvent(pKbd, evp);
		} else {
			solMouseEnqueueEvent(pPtr, evp);
		}

		evp++;
	}

/*
	if (!pPtr->on || !pKbd->on)
		return;

	kbdRetry = TRUE;
	while( kbdRetry ) {
		KbdEvents = solKbdGetEvents(pKbd, &nk, &kbdRetry); 	
		while(nk--) {
			solKbdEnqueueEvent(pKbd, KbdEvents++);
		}
	}

	ptrRetry = TRUE;
	while( ptrRetry ) {
		ptrEvents = solMouseGetEvents(pPtr, &np, &ptrRetry); 	
		while(np--) {
			solMouseEnqueueEvent(pPtr, ptrEvents++);
		}
	}
 */
}
