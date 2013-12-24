all:
	./build

install:
	./build true

clean:
	rm -f lib/v1.cmt lib/v1.cmti lib/v1.cmi
