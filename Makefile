sources := log.c ts_sdl2.c scm.c iso.c ts_iso.c
objects = $(sources:.c=.o)
tinyschemedir := $(HOME)/src/tinyscheme-1.41
CFLAGS += -Werror -Wfatal-errors
CFLAGS += -std=c99
CFLAGS += -I $(tinyschemedir) -DUSE_DL=1
CFLAGS += -fPIC
CFLAGS += -Wall -g
sdl2lib := ts_sdl2.so
isolib := ts_iso.so

all: $(sdl2lib) $(isolib)

$(sdl2lib): $(objects) Makefile
	$(CC) -I $(tinyschemedir) -shared $(CFLAGS) -DUSE_DL=1 -o $@ $(objects) -lSDL2 -lSDL2_image -Wl,-rpath=/usr/local/lib
	#strip $(sdl2lib)

$(isolib): $(objects) Makefile
	$(CC) -I $(tinyschemedir) -shared $(CFLAGS) -DUSE_DL=1 -o $@ $(objects) -lSDL2 -lSDL2_image -Wl,-rpath=/usr/local/lib

demo: demo.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -lSDL2 -lSDL2_image -o $@ -Wl,-rpath=/usr/local/lib

test: $(sdl2lib) $(isolib) test.scm
	TINYSCHEMEINIT=$(tinyschemedir)/init.scm $(tinyschemedir)/scheme test.scm

clean:
	rm -f *.o $(sdl2lib) $(isolib) demo
