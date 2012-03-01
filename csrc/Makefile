#IDIR =../include
INC=/usr/local/include/glib-2.0 /usr/local/lib/glib-2.0/include
IDIR=$(foreach d, $(INC), -I$d)
CC=gcc
CFLAGS=$(IDIR) -g

ODIR=obj
LDIR =/usr/local/lib

LIBS=-lglib-2.0

#_DEPS = tape.h
#DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
DEPS = tape.h

_OBJ = brainmess.o tape.o execute.o program.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))


$(ODIR)/%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

brainmess: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o *~ core brainmess
