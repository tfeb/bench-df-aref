# Just build the shared library
#

CFLAGS	= -O2

.PHONY: build clean

build: dp.dylib

%.dylib: %.c
	$(CC) -shared $(CFLAGS) $(OPTIMIZE) $< -o $@

clean:
	rm -f *.dylib
