/* popauth.h - POP authorization DB definitions */
/* @(#)$Id: popauth.h,v 1.1 1992/02/11 17:41:39 jromine Exp $ */


struct authinfo {
    char    auth_secret[16];
    int	    auth_secretlen;
};
