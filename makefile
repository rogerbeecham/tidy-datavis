# makefile
# adapted from Tom Swan (www.tomswan.com)
# 2018-04-09
# Type 'make' to build, 'make clean' to delete generated files

targets=index.html data-vis.html r-studio.html code1.html code2.html code3.html

all: ${targets}

%.html : %.adoc menu-include.adoc
	asciidoctor $<

clean:
	rm -v -f ${targets}
	# rm -v -f *.*~
