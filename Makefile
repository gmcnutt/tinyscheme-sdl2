sources := log.c sdl2.c scm.c
objects = $(sources:.c=.o)
tinyschemedir := $(HOME)/src/tinyscheme-1.41
CFLAGS += -Werror -Wfatal-errors
CFLAGS += -std=c99
CFLAGS += -I $(tinyschemedir) -DUSE_DL=1
CFLAGS += -fPIC
CFLAGS += -Wall -g
lib := sdl2.so

all: $(lib)

$(lib): $(objects) Makefile
	$(CC) -I $(tinyschemedir) -shared $(CFLAGS) -DUSE_DL=1 -o $@ $(objects) -lSDL2 -lSDL2_image -Wl,-rpath=/usr/local/lib
	#strip $(lib)

demo: demo.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -lSDL2 -lSDL2_image -o $@ -Wl,-rpath=/usr/local/lib

test: $(lib) test.scm
	tinyscheme test.scm

clean:
	rm -f *.o $(lib)
