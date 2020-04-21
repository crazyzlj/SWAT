	subroutine 	copyfile (inputfn, outputfn)

	character*150 line
	character*4 inputfn
	character*7 outputfn


	
	open(100,file=inputfn)
	open(200, file=outputfn)
	
3	read(100,1,end=40) line
	write(200,1) line
	go to 3
40	continue
1	format(a150)	
	close(100)
	close(200)
	return
	end 
