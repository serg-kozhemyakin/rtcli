objects=rtcli.o bencode.o rtorrent-connection.o rtorrent-commands.o

%.o: %.scm
	csc -c -O2 $<

rtcli: $(objects)
	csc $(objects) -o rtcli

clean:
	rm rtcli *.o

