#
# Makefile for emacs-canna.
#

EMACS_FLAGS	= -batch -q -no-site-file -l CANNA-MK

# directory where emacs source resides
#SRC_TOP		= /usr/src/local/GNU/emacs/emacs-27.2
# directory where emacs binaries built
#EMACS_BLD_DIR	= /usr/src/local/GNU/emacs/emacs-27.2
# version of emacs
BLD_VERSION	= 27.2

INC_DIRS	= -I.
CC		= cc

CANNA_DEFINES	= -DAS_EMACS_MODULES

# directory where `canna' includes files reside
CANNA_HEADERDIR = /usr/local/include
CANNA_CFLAGS	= $(CANNA_DEFINES) -I$(CANNA_HEADERDIR)
# directory where libcanna.so reside
CANNA_LDFLAGS	= -L/usr/local/lib -lcanna

# directory to install emacs support binaries
#	typically `/usr/local/libexec/emacs/<BLD_VERSION>/<OS Name>'
DL_INSTALLDIR	= /usr/local/libexec/emacs/$(BLD_VERSION)/amd64-freebsd12

# directory to install emacs site lisp codes
#	typically `/usr/local/share/emacs/<BLD_VERSION>/site-lisp
LISPDIR		= /usr/local/share/emacs/$(BLD_VERSION)/site-lisp

CFLAGS		= -fPIC -g3 -O2 -c -nostdlib -Demacs \
		  -D_THREAD_SAFE -MMD -MP \
		  -Wno-switch \
		  -Wno-tautological-constant-out-of-range-compare \
		  -Wno-pointer-sign \
		  -Wno-string-plus-int

LINKER		= $(CC) -shared -o

VERSION		= 1.2

all:

canna.elc:	canna.el
	emacs $(EMACS_FLAGS) -f compile-canna

canna.o:	canna.c canna-module.h
	$(CC) $(CFLAGS) $(INC_DIRS) $(CANNA_CFLAGS) canna.c

canna-module.h:	canna.c
	@if [ -e $(EMACS_BLD_DIR)/../lib-src/make-docfile ]; then \
	  $(EMACS_BLD_DIR)/../lib-src/make-docfile -g canna.c | \
	  sed -n -e 's/^#define \([^ ]*\) .*/static emacs_value \1;/p' | \
	  fgrep -v Qnil > canna-module.h; \
	  $(EMACS_BLD_DIR)/../lib-src/make-docfile canna.c | \
	  ./mk-docstr >> canna-module.h;\
	elif [ -f canna-module.h-save ]; then \
	  cp canna-module.h-save canna-module.h; \
	  echo '** canna-module.h may not be correct **'; \
	fi

canna.so:	canna.o
	$(LINKER) canna.so canna.o $(CANNA_LDFLAGS)
	
install.elc:	canna.elc
	install -c canna.el $(LISPDIR)
	gzip $(LISPDIR)/canna.el
	install -c canna.elc $(LISPDIR)
	install -c canna-leim.el $(LISPDIR)

install.so:	canna.so
	install -c canna.so $(DL_INSTALLDIR)/canna.so.$(VERSION)
	(cd $(DL_INSTALLDIR); ln -sf canna.so.$(VERSION) canna.so)

clean:
	-[ -f canna-module.h ] && mv -f canna-module.h canna-module.h-old
	-rm -f *~ *.o *.so *.elc a.out canna.d
