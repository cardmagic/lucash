.SUFFIXES: .tex .dvi .ps .pdf $(.SUFFIXES)

TEX=	front.tex intro.tex procnotation.tex syscalls.tex network.tex \
	strings.tex awk.tex miscprocs.tex running.tex

TEX2PAGE=tex2page

man.dvi: $(TEX)
man.pdf: $(TEX)

.dvi.ps:
	dvips -j0 -o $@ $<

.tex.dvi:
	latex $< && latex $<
	makeindex $(<:.tex=.idx)
	rm $*.log

.tex.pdf:
	pdflatex $< && thumbpdf $@ && pdflatex $<
	makeindex $(<:.tex=.idx)
	rm $*.log

clean:
	-rm -f *.log *.png man.out man.dvi man.ps man.pdf thumb*.png
	rm -rf html

INSTALL_DATA= install -c -m 644

tar:
	tar cf - *.tex sty | gzip > man.tar.gz

html: $(TEX)
	$(TEX2PAGE) man && $(TEX2PAGE) man

install: man.ps
	@echo WARNING:
	@echo WARNING: this depends on /u/su/scsh/scsh 
	@echo WARNING: pointing to the current release
	@echo WARNING:
	$(INSTALL_DATA) cheat.txt /u/su/scsh/scsh/doc/
	$(INSTALL_DATA) man.ps /u/su/scsh/scsh/doc/scsh-manual.ps
	$(INSTALL_DATA) $(TEX) /u/su/scsh/scsh/doc/scsh-manual/
	$(INSTALL_DATA) sty/* /u/su/scsh/scsh/doc/scsh-manual/sty/
