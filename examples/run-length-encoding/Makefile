include ../Makefile.common

.PHONY: all
all:	rle apples

.PHONY: rle
rle:
	${TOP} ${INCLUDES} ${LIB} rle.ml

apples:
	${TOP} ${INCLUDES} ${LIB} apples.ml
	$(CC) -W -Wall -o apples apples.c && ./apples

clean::
	rm -f apples

