sources := $(wildcard *.c)
testsources := $(wildcard test*.c)
testsources += cli.c demo.c
libsources := $(filter-out $(testsources),$(sources))
objects = $(libsources:.c=.o)
tinyschemedir := $(HOME)/src/tinyscheme-1.41
CFLAGS += -Werror -Wfatal-errors
CFLAGS += -std=c99
CFLAGS += -I $(tinyschemedir) -DUSE_DL=1
apps := demo

ifeq ($(OPTIMIZE), true)
	CFLAGS += -O3
else
	CFLAGS += -Wall -g
endif

all: $(apps)

sdl2.so: sdl2.c Makefile
	$(CC) -I $(tinyschemedir) -shared -Wall -fPIC $(CFLAGS) -DUSE_DL=1 -o sdl2.so sdl2.c -lSDL2 -lSDL2_image -Wl,-rpath=/usr/local/lib
	strip sdl2.so

demo: demo.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -lSDL2 -lSDL2_image -o $@ -Wl,-rpath=/usr/local/lib

test_v3: test_v3.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -o $@

clean:
	rm -f *.o $(apps) sdl2.so
