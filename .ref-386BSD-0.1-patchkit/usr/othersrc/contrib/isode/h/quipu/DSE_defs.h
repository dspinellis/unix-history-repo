#ifndef	PEPYPATH
#include <isode/pepsy/DSE_pre_defs.h>
#else
#include "DSE_pre_defs.h"
#endif



#ifndef	lint
#define encode_DSE_PSAPaddr(pe, top, len, buffer, parm) \
    enc_f(_ZPSAPaddrDSE, &_ZDSE_mod, pe, top, len, buffer, (char *) parm)

#define decode_DSE_PSAPaddr(pe, top, len, buffer, parm) \
    dec_f(_ZPSAPaddrDSE, &_ZDSE_mod, pe, top, len, buffer, (char **) parm)

#define print_DSE_PSAPaddr(pe, top, len, buffer, parm) \
    prnt_f(_ZPSAPaddrDSE, &_ZDSE_mod, pe, top, len, buffer)
#define print_DSE_PSAPaddr_P    _ZPSAPaddrDSE, &_ZDSE_mod


#endif   /* lint */
