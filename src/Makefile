all: region.pdf

words.exe: words.sml
	mlkit -no_gc -prof -Pcee -o words.exe words.sml

region.ps: words.exe
	./words.exe -microsec 300 ../texts/dickens-christmas_carol.txt
	~/gits/mlkit/src/Tools/Rp2ps/rp2ps -region -name words -sampleMax 1000

region.pdf: region.ps
	pstopdf region.ps region.pdf

clean:
	rm -rf region.ps region.pdf *.rp *~ run words.exe MLB
