IDRIS2 ?= idris2
CC ?= cc
PACKAGE_VERSION := 0.1.0
PACKAGE_LIBDIR := $(shell $(IDRIS2) --libdir)/iotaTime-$(PACKAGE_VERSION)/lib
SUPPORT_SOURCE := support/iotatime_windows.c

ifeq ($(OS),Windows_NT)
SUPPORT_LIBRARY := support/libiotatime_windows.dll
SUPPORT_LIBS := -ladvapi32
else
SUPPORT_LIBRARY := support/libiotatime_windows.so
SUPPORT_LIBS :=
endif

.PHONY: support install-support clean-support

support: $(SUPPORT_LIBRARY)

$(SUPPORT_LIBRARY): $(SUPPORT_SOURCE)
	$(CC) -std=c11 -O2 -Wall -Wextra -Werror -shared -o $@ $< $(SUPPORT_LIBS)

install-support: support
	mkdir -p "$(PACKAGE_LIBDIR)"
	cp "$(SUPPORT_LIBRARY)" "$(PACKAGE_LIBDIR)/"

clean-support:
	rm -f support/libiotatime_windows.so support/libiotatime_windows.dll
