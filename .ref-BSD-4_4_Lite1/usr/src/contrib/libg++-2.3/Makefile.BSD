CC= cc
DIRS= libiberty libg++ libg++/src libg++/iostream
BUILD_DIR= ${.CURDIR}
TARGET_LIB= libg++.a	
INSTALL_DIR= /usr/lib
INCLUDE_INSTALL= /usr/include/g++
.CURDIR != pwd
IOLIB = $(.CURDIR)/libg++/iostream/libio.a
SRCLIB = $(.CURDIR)/libg++/src/stamp
LIBIBERTY = $(.CURDIR)/libiberty/libiberty.a
LIBIBERTY_OBJECTS_TO_GET = `cat needed-list` strerror.o strsignal.o
IOSTREAM_OBS= streambuf.o stdstrbufs.o iostream.o stdstreams.o strstream.o \
  indstream.o PlotFile.o SFile.o fstream.o parsestream.o stream.o makebuf.o \
  editbuf.o filebuf.o sgetline.o igetline.o igetsb.o procbuf.o \
  sbufvform.o sbufvscan.o stdiostream.o floatconv.o outfloat.o iomanip.o

all $(TARGET_LIB): $(IOLIB) $(SRCLIB) $(LIBIBERTY)
	cd ${.CURDIR}/libg++/src; ar qc ${BUILD_DIR}/${TARGET_LIB} *.o
	cd ${.CURDIR}/libg++/iostream; ar qc ${BUILD_DIR}/${TARGET_LIB} \
	$(IOSTREAM_OBS)
	cd ${.CURDIR}/libiberty; ar qc ${BUILD_DIR}/${TARGET_LIB} \
	$(LIBIBERTY_OBJECTS_TO_GET)
	ranlib ${BUILD_DIR}/${TARGET_LIB}

$(SRCLIB):
	cd ${.CURDIR}/libg++/src; make "CC = $(CC)" "CXX = $(CC)"

$(IOLIB):
	cd ${.CURDIR}/libg++/iostream; make "CC=$(CC)" libio.a

$(LIBIBERTY):
	cd ${.CURDIR}/libiberty; make "CC=$(CC)"

install: $(TARGET_LIB)
	if [ -d ${INSTALL_DIR} ]; then true; \
	else mkdir ${INSTALL_DIR}; \
	fi
	install -c -m 444 ${.CURDIR}/${TARGET_LIB} ${INSTALL_DIR}
	install -c -m 444 ${LIBIBERTY} ${INSTALL_DIR}
	if [ -d ${INCLUDE_INSTALL} ]; then true; \
	else mkdir ${INCLUDE_INSTALL}; \
	fi
	install -c -m 444 ${.CURDIR}/libg++/g++-include/*.h ${INCLUDE_INSTALL}
	if [ -d ${INCLUDE_INSTALL}/sys ] ; then true; \
	else mkdir ${INCLUDE_INSTALL}/sys; \
	fi
	install -c -m 444 ${.CURDIR}/libg++/g++-include/sys/*.h \
		${INCLUDE_INSTALL}/sys
	install -c -m 444 ${.CURDIR}/libg++/src/*.h ${INCLUDE_INSTALL}
	install -c -m 444 ${.CURDIR}/libg++/iostream/*.h ${INCLUDE_INSTALL}
	install -c -m 444 ${.CURDIR}/libg++/g++-include/*.h ${INCLUDE_INSTALL}
	install -c -m 444 ${.CURDIR}/libg++/_G_config.h ${INCLUDE_INSTALL}

clean:
	for i in $(DIRS); do \
		cd ${.CURDIR}/$$i ; \
		rm -f *.o *.a *~ core stamp*; \
	done
	rm -f $(BUILD_DIR)/$(TARGET_LIB)






























































