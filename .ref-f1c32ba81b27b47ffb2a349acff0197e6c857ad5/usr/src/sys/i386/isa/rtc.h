/*
 * @(#)rtc.h	1.1 (Berkeley) %G%
 * RTC Register locations
 */

#define RTC_SEC		0x00	/* seconds */
#define RTC_SECALRM	0x01	/* seconds alarm */
#define RTC_MIN		0x02	/* minutes */
#define RTC_MINALRM	0x03	/* minutes alarm */
#define RTC_HRS		0x04	/* hours */
#define RTC_HRSALRM	0x05	/* hours alarm */
#define RTC_WDAY	0x06	/* week day */
#define RTC_DAY		0x07	/* day of month */
#define RTC_MONTH	0x08	/* month of year */
#define RTC_YEAR	0x09	/* month of year */
#define RTC_STATUSA	0x0a	/* status register A */

#define RTC_BASELO	0x15	/* low byte of basemem size */
#define RTC_BASEHI	0x16	/* high byte of basemem size */
#define RTC_EXTLO	0x17	/* low byte of extended mem size */
#define RTC_EXTHI	0x18	/* low byte of extended mem size */
