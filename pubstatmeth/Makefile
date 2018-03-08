MATRIX_LABS = matrix_algebra/lab1.Rmd matrix_algebra/lab2.rmd 
STATS_LABS = getting_started/lab1.Rmd getting_started/lab2.Rmd getting_started/lab3.Rmd getting_started/lab4.Rmd

SUMMARIES = journals/week?/week?.pdf

build: index summaries matrix stats
	echo "done building"
stats:
	./Rmd.sh $(STATS_LABS)
	cp ~/develop/pubstatmeth/assignments/getting_started/lab?.html ./ph525.1/
matrix:
	./Rmd.sh $(MATRIX_LABS)
	cp ~/develop/pubstatmeth/assignments/matrix_algebra/lab?.html ./ph525.2/

index:
	pandoc -t revealjs -s index.md -o index.html
summaries:
	cp ~/develop/pubstatmeth/${SUMMARIES}

clean:
	rm ./ph525.2/*.nb.html
	rm ./ph525.1/*.nb.html

