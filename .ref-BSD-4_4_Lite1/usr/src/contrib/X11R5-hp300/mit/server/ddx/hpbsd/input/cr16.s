; $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/cr16.s,v 1.1 1992/09/30 03:14:10 root Exp $
        .SPACE  $TEXT$
        .SUBSPA $CODE$
        .export cr16
        .PROC
        .CALLINFO
        .ENTRY
cr16
        bv      (%rp)
	mfctl	16,%ret0
        .PROCEND
