/*
 * CourierConnection as seen by everyone else except the runtime library,
 * effectively hiding the internal structure
 */

#ifndef CourierConnectionHeader
#define CourierConnectionHeader

typedef int CourierConnection;
extern CourierConnection *CourierOpen();

#endif CourierConnectionHeader
