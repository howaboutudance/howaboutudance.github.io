LABS = matrix_algebra/lab1.Rmd getting_started/lab1.Rmd getting_started/lab2.Rmd getting_started/lab3.Rmd getting_started/lab4.Rmd

build:
	./Rmd.sh $(LABS)
	cp ~/develop/pubstatmeth/assignments/matrix_algebra/*.html ./ph525.2/
	cp ~/develop/pubstatmeth/assignments/getting_started/*.html ./ph525.1/
	pandoc -t revealjs -s index.md -o index.html

