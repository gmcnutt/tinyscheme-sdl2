sources := log.c sdl2.c scm.c
objects = $(sources:.c=.o)
tinyschemedir := $(HOME)/src/tinyscheme-1.41
CFLAGS += -Werror -Wfatal-errors
CFLAGS += -std=c99
CFLAGS += -I $(tinyschemedir) -DUSE_DL=1
CFLAGS += -fPIC

ifeq ($(OPTIMIZE), true)
	CFLAGS += -O3
else
	CFLAGS += -Wall -g
endif

all: sdl2.so

sdl2.so: $(objects)
	$(CC) -I $(tinyschemedir) -shared $(CFLAGS) -DUSE_DL=1 -o $@ $(objects) -lSDL2 -lSDL2_image -Wl,-rpath=/usr/local/lib
	strip sdl2.so

demo: demo.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -lSDL2 -lSDL2_image -o $@ -Wl,-rpath=/usr/local/lib

test_v3: test_v3.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -o $@

test: sdl2.so test.scm
	tinyscheme test.scm

clean:
	rm -f *.o $(apps) sdl2.so
