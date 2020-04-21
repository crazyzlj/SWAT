      subroutine sort3(n,m,rb,raq,rbf,im,igoc)

c
c  SORTING SUBROUTINE ADAPTED FROM "NUMERicalm RECIPES"
c  BY W.H. PRESS ET AL., pp. 233-234
c
c  LIST OF VARIABLES
c     raq(.) = array to be sorted
c     rb(.,.) = arrays ordered corresponding to rearrangement of ra(.)
c     wk(.,.), iwk(.), in() = local varibles
c
      real*8  raq(n),rb(n,100),wk(n,100)
	real*8  rbf(n,igoc)
	integer im(n), in(n),n,igoc,m,iwk(n)

      call indexx(n, raq, iwk)

      do 11 i = 1, n
      wk(i,1) = raq(i)
	in(i)=im(i)
   11 continue
      do 12 i = 1, n
      raq(i) = wk(iwk(i),1)
	im(i)=in(iwk(i))
   12 continue
      do 14 j = 1, m
      do 13 i = 1, n
      wk(i,j) = rb(i,j)
   13 continue
   14 continue
      do 16 j = 1, m
      do 15 i = 1, n
      rb(i,j) = wk(iwk(i),j)
   15 continue
   16 continue
      do 18 j = 1, igoc
      do 17 i = 1, n
      wk(i,j) = rbf(i,j)
   17 continue
   18 continue
      do 20 j = 1, igoc
      do 19 i = 1, n
      rbf(i,j) = wk(iwk(i),j)
   19 continue
   20 continue
c  END OF SUBROUTINE SORT
      return
      end
