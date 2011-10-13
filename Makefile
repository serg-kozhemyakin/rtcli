objects=rtcli.o bencode.o rtorrent-connection.o rtorrent-commands.o rtcli-commands.o version.o
HGVERSION=$(shell hg parents --template 'hgid: {node|short}')
HGDATE=$(shell hg parents --template '{date|date}')

.PHONY: clean version.scm

%.o: %.scm
	csc -c -O2 $<

rtcli: $(objects)
	csc $(objects) -o rtcli

version.scm:
	@echo '(declare (unit version)) (define version:date "'$(HGDATE)'") (define version:id "'$(HGVERSION)'")' > version.scm

clean:
	rm rtcli version.scm *.o

