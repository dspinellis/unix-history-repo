/* Number of bits in a `char'.  */
#define CHAR_BIT 8

/* No multibyte characters supported yet.  */
#define MB_LEN_MAX 1

/* Minimum and maximum values a `signed char' can hold.  */
#define SCHAR_MIN (-128)
#define SCHAR_MAX 127

/* Maximum value an `unsigned char' can hold.  (Minimum is 0).  */
#define UCHAR_MAX 255U

/* Minimum and maximum values a `char' can hold.  */
#ifdef __CHAR_UNSIGNED__
#define CHAR_MIN 0
#define CHAR_MAX 255U
#else
#define CHAR_MIN (-128)
#define CHAR_MAX 127
#endif

/* Minimum and maximum values a `signed short int' can hold.  */
#define SHRT_MIN (-32768)
#define SHRT_MAX 32767

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0).  */
#define USHRT_MAX 65535U

/* Minimum and maximum values a `signed int' can hold.  */
#define INT_MIN (-INT_MAX-1)
#define INT_MAX 2147483647

/* Maximum value an `unsigned int' can hold.  (Minimum is 0).  */
#define UINT_MAX 4294967295U

/* Minimum and maximum values a `signed long int' can hold.
   (Same as `int').  */
#define LONG_MIN (-LONG_MAX-1)
#define LONG_MAX 2147483647

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0).  */
#define ULONG_MAX 4294967295U
