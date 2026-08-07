IDRIS2 ?= idris2
CC ?= cc
PACKAGE_VERSION := 1.0.1
PACKAGE_LIBDIR := $(shell $(IDRIS2) --libdir)/iotaTime-$(PACKAGE_VERSION)/lib

ifeq ($(OS),Windows_NT)
SHARED_EXTENSION := dll
WINDOWS_LIBS := -ladvapi32
else
SHARED_EXTENSION := so
WINDOWS_LIBS :=
endif

WINDOWS_LIBRARY := support/libiotatime_windows.$(SHARED_EXTENSION)
UNIX_LIBRARY := support/libiotatime_unix.$(SHARED_EXTENSION)
SUPPORT_LIBRARIES := $(WINDOWS_LIBRARY) $(UNIX_LIBRARY)

.PHONY: support install-support clean-support

support: $(SUPPORT_LIBRARIES)

$(WINDOWS_LIBRARY): support/iotatime_windows.c
	$(CC) -std=c11 -O2 -Wall -Wextra -Werror -shared -o $@ $< $(WINDOWS_LIBS)

$(UNIX_LIBRARY): support/iotatime_unix.c
	$(CC) -std=c11 -O2 -Wall -Wextra -Werror -shared -o $@ $<

install-support: support
	mkdir -p "$(PACKAGE_LIBDIR)"
	cp $(SUPPORT_LIBRARIES) "$(PACKAGE_LIBDIR)/"

clean-support:
	rm -f support/libiotatime_windows.so support/libiotatime_windows.dll \
		support/libiotatime_unix.so support/libiotatime_unix.dll
