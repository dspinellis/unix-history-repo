/* machine dependent stuff for core files */
#define TXTRNDSIZ 1024L
#define stacktop(siz) (0x80000000L-6*0x200)
#define stackbas(siz) (0x80000000L-6*0x200-siz)
