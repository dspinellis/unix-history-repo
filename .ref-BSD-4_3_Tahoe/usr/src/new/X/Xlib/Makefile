# 	Copyright 1985, Massachusetts Institute of Technology
#
#	Xlib Makefile - C interface library makefile.
#
#

DESTDIR=

INCLUDES= -I../include
C2= /lib/c2
CFLAGS= -O ${INCLUDES} ${NETOPTIONS}

.SUFFIXES: .o .h .c

.c.o:
	${CC} -pg ${CFLAGS} -c $*.c
	mv $*.o profiled/$*.o
	${CC} ${CFLAGS} -c $*.c

OLIST=	XAddHost.o \
	XAddNode.o \
	XAppendBuffer.o \
	XAppendVertex.o \
	XAutoRptOff.o \
	XAutoRptOn.o \
	XBitmapBitPut.o \
	XCharBitmap.o \
	XCharWidths.o \
	XChngBackgrd.o \
	XChngBorder.o \
	XChngWindow.o \
	XCircWinUp.o \
	XCircWinDn.o \
	XChkMaskEvent.o \
	XChkWinEvent.o \
	XClear.o \
	XClipClippd.o \
	XClipDrawThru.o \
	XCloseDisplay.o \
	XCloseFont.o \
	XClrIconWind.o \
	XCompEvents.o \
	XConfWindow.o \
	XCopyArea.o \
	XCreate.o \
	XCreatTranss.o \
	XCreateAssoc.o \
	XCreateCursor.o \
	XCreateTerm.o \
	XCreateTrans.o \
	XCreateWinBat.o \
	XCreateWindow.o \
	XCreateWinds.o \
	XDefineCursor.o \
	XDeleteAssoc.o \
	XDestAssoc.o \
	XDestSubWind.o \
	XDestWindow.o \
	XDisplayName.o \
	XDraw.o \
	XDrawDashed.o \
	XDrawFilled.o \
	XDrawPatternd.o \
	XDrawTiled.o \
	XErrDescrip.o \
	XErrHndlr.o \
	XExpandEvents.o \
	XFeep.o \
	XFeepControl.o \
	XFetchBuffer.o \
	XFetchBytes.o \
	XFetchName.o \
	XFlush.o \
	XFocusKbd.o \
	XFontWidths.o \
	XFreeBitmap.o \
	XFreeColors.o \
	XFreeCursor.o \
	XFreeFont.o \
	XFreePixmap.o \
	XGeom.o \
	XGetColCells.o \
	XGetColor.o \
	XGetFont.o \
	XGetDefault.o \
	XGetHardColor.o \
	XGetHosts.o \
	XGetNodes.o \
	XGetResHint.o \
	XGrabButton.o \
	XGrabMouse.o \
	XGrabServer.o \
	XInterpLoc.o \
	XIOErrHndlr.o \
	XKeyBind.o \
	XKeyClickCon.o \
	XLine.o \
	XLockToggle.o \
	XLockUpDown.o \
	XLookUpAssoc.o \
	XLowerWindow.o \
	XMakeAssoc.o \
	XMakePixmap.o \
	XMakeTile.o \
	XMakeTiles.o \
	XMapSubWind.o \
	XMapWindow.o \
	XMaskEvent.o \
	XMouseControl.o \
	XMoveArea.o \
	XMoveWindow.o \
	XNextEvent.o \
	XOpenDisplay.o \
	XOpenFont.o \
	XParseColor.o \
	XParseGeom.o \
	XPeekEvent.o \
	XPending.o \
	XPixBitsPutXY.o \
	XPixBitsPutZ.o \
	XPixFill.o \
	XPixSet.o \
	XPixmapGetXY.o \
	XPixmapGetZ.o \
	XPixmapPut.o \
	XPixmapSave.o \
	XPutBackEvent.o \
	XQueryBrush.o \
	XQueryButtons.o \
	XQueryColor.o \
	XQueryColors.o \
	XQueryCursor.o \
	XQueryFont.o \
	XQueryInput.o \
	XQueryMouse.o \
	XQueryTile.o \
	XQueryTree.o \
	XQueryWidth.o \
	XQueryWindow.o \
	XRaiseWindow.o \
	XReadBitmapF.o \
	XRemoveHost.o \
	XRemoveNode.o \
	XRotBuffers.o \
	XScreenSaver.o \
	XSelectInput.o \
	XSetDisplay.o \
	XSetIconWind.o \
	XSetResHint.o \
	XStippleFill.o \
	XStoreBitmap.o \
	XStoreBuffer.o \
	XStoreBytes.o \
	XStoreColor.o \
	XStoreColors.o \
	XStoreCursor.o \
	XStoreName.o \
	XStorePixXY.o \
	XStorePixmapZ.o \
	XStringWidth.o \
	XSync.o \
	Xtextlib.o\
	XText.o \
	XTextMask.o \
	XTextMaskPad.o \
	XTextPad.o \
	XTileFill.o \
	XTileAbsolute.o \
	XTileRelative.o \
	XTileSet.o \
	Xttylib.o \
	XUndefCursor.o \
	XUngrabButton.o \
	XUngrabMouse.o \
	XUngrabServer.o \
	XUnmapSubs.o \
	XUnmapTrans.o \
	XUnmapWindow.o \
	XUpdateMouse.o \
	XWarpCond.o \
	XWarpMouse.o \
	XWindowEvent.o \
	XlibInternal.o \
	DisplaySize.o \
	keymap.o

all: libX.a libX_p.a

$(OLIST): Xlib.h

Xtextlib: Xtext.h

XKeyBind.o: Xkeyboard.h Xkeymap.h

libX.a: $(OLIST)
	rm -f libX_p.a
	ar cr libX.a $(OLIST)
	@ranlib libX.a

libX_p.a: $(OLIST)
	rm -f libX_p.a
	cd profiled; ar cr ../libX_p.a $(OLIST)
	@ranlib libX_p.a

lint:
	lint -CX ${INCLUDES} *.c

install: all
	cp libX.a ${DESTDIR}/usr/lib
	@ranlib ${DESTDIR}/usr/lib/libX.a
	cp libX_p.a ${DESTDIR}/usr/lib
	@ranlib ${DESTDIR}/usr/lib/libX_p.a

include: Xlib.h Xkeyboard.h Xtext.h
	install -c -m 0644 Xlib.h      ${DESTDIR}/usr/include/X
	install -c -m 0644 Xkeyboard.h ${DESTDIR}/usr/include/X
	install -c -m 0644 Xtext.h     ${DESTDIR}/usr/include/X
	install -c -m 0644 Xtty.h      ${DESTDIR}/usr/include/X

Xttylib.o: Xtty.h
Xtextlib.o: Xtext.h

clean:
	rm -f *.*.* \#* *~ *.bak *.BAK *.CKP *.s *.o libX.a profiled/*.o libX_p.a errs

