bin_PROGRAMS = rtcli

rtcli_SOURCES = rtcli.scm \
	rtorrent-connection.scm \
	rtorrent-commands.scm \
	rtcli-commands.scm \
	bencode.scm \
	version.scm

rtcli_LDADD = -lm

.scm.o:

OBJEXT=o

LINK=csc -o $@

%.o: %.scm
	csc -c $< -o $@ $(CHICKENFLAGS)
