LABS = matrix_algebra/lab1.Rmd getting_started/lab1.Rmd getting_started/lab2.Rmd getting_started/lab3.Rmd getting_started/lab4.Rmd

SUMMARIES = journals/week?/week?.pdf

build: index.html summaries
	./Rmd.sh $(LABS)
	cp ~/develop/pubstatmeth/assignments/matrix_algebra/lab?.html ./ph525.2/
	cp ~/develop/pubstatmeth/assignments/getting_started/lab?.html ./ph525.1/
index.html:
	pandoc -t revealjs -s index.md -o index.html
summaries:
	cp ~/develop/pubstatmeth/${SUMMARIES}
	echo "moved summaries"

clean:
	rm ./ph525.2/*.nb.html
	rm ./ph525.1/*.nb.html

