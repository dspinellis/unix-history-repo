#include <xnscourier/Clearinghouse2.h>

Clearinghouse2_RetrieveAddressesResults
Clearinghouse2_RetrieveAddresses()
{
	static Clearinghouse2_NetworkAddress ouraddr;
	static Clearinghouse2_RetrieveAddressesResults result =
		{1, &ouraddr};

	ouraddr.network[0] = 0;
	ouraddr.network[1] = 2272;
	ouraddr.host[0] = 0x702;
	ouraddr.host[1] = 0x1;
	ouraddr.host[2] = 0x4501;
	ouraddr.socket = 0;
	return(result);
}
