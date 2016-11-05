sources := $(wildcard *.c)
testsources := $(wildcard test*.c)
testsources += cli.c demo.c
libsources := $(filter-out $(testsources),$(sources))
objects = $(libsources:.c=.o)
CFLAGS += -Werror -Wfatal-errors
CFLAGS += -std=c99
apps := demo

ifeq ($(OPTIMIZE), true)
	CFLAGS += -O3
else
	CFLAGS += -Wall -g
endif

all: $(apps)

demo: demo.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -lSDL2 -lSDL2_image -o $@ -Wl,-rpath=/usr/local/lib

test_v3: test_v3.o $(objects)
	$(CC) $(CFLAGS) $< $(objects) $(LDFLAGS) -o $@

clean:
	rm -f *.o $(apps)
