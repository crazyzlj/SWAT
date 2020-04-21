      subroutine analyse(nintval,nopt,iobj,isens, parname)
      integer nintval,nopt,isamp,iobj,isens, ira(nopt), irank(nopt)
      real*8 xx(nopt), lhx(nintval,nopt), lhy(nintval)
      real*8 yobj(nintval,max(iobj,isens)),help(nopt)      
      real*8 oatobj(nintval,nopt,max(iobj,isens)),
     *      oatmax(nopt,max(iobj,isens)), help1(nopt)
      real*8 yy(max(iobj,isens)),yhelp(max(iobj,isens)),rank(nopt)
      character*10 parname(nopt)
      common /iopar/ in,iprii

      


c      AOT SENSITIVITY ANALYSING  ON OBJECTIVE FUNTCTIONS

      if (iobj.gt.0) then
      write (iprii,*)'AOT sensitivity analyses on objective functions '
      rewind(18019)
      rewind(18012)
      do iloop=1,nintval
            read(18019,*)
            read(18019,1) itel, ipar, (xx(kk), kk=1,nopt)
            read(18012,2) itel,(yy(kk),kk=1,iobj)

            do kk=1,nopt
            lhx(iloop,kk)=xx(kk)
            end do

            do kk=1,iobj
            yobj(iloop,kk)=yy(kk)
            end do
            do jj=1,nopt
            read(18019,1) itel, ipar, (xx(kk), kk=1,nopt)
                        yhelp=yy                  
               read(18012,2) itel,(yy(kk),kk=1,iobj)

                  do kk=1,iobj
            t=yy(kk)-yhelp(kk)
                        oatobj(iloop,ipar,kk)=0.
                  if (abs(t).gt.0) oatobj(iloop,ipar,kk)=abs(t)*50./
     *      (yhelp(kk)+yy(kk))
                  end do
                  end do
      end do
      oatmax=maxval(oatobj,dim=1)

      do ik=1,iobj
      write (iprii,*) 'OBJECTIVE FUNCTION ', ik
      write (iprii,*) 'results per loop '
            help=0.
            help1=0.
            do jk=1,nintval
            write (iprii,3) jk,(oatobj(jk,kk,ik),kk=1,nopt)
            do kk=1,nopt
            help(kk)=help(kk)+oatobj(jk,kk,ik)
            end do
            end do
            do kk=1,nopt
            help(kk)=help(kk)/nintval
            end do
            
            do jk=1,nintval
            do kk=1,nopt
            help1(kk)=help1(kk)+(help(kk)-oatobj(jk,kk,ik))**2
            end do
            end do      
            
            do kk=1,nopt
            help1(kk)=help1(kk)/(nintval-1)
            end do

      write (iprii,*) '-----------------------------------------------'      
      write (iprii,*) ' maximum '
      write (iprii,4) (oatmax(kk,ik), kk=1,nopt)
      write (iprii,*) ' variance '
      write (iprii,4) (help1(kk), kk=1,nopt)
      write (iprii,*) '-----------------------------------------------'      
      write (iprii,*) ' mean '
      write (iprii,4) (help(kk), kk=1,nopt)

      call sorteer3(help,rank,nopt,irank,ira)
      do ii=1,nopt
      if (rank(ii).eq.0.) ira(ii)=nopt+1
      if (help(ii).eq.0.)  irank(ii)=nopt+1
      end do
      write (iprii,*) ' ranking '
      write(18015,55) ik, (irank(kk), kk=1,nopt)
      write (iprii,5) (irank(kk), kk=1,nopt)
      write (iprii,*) '------------------------------------------------'

      write (iprii,*) '------------------------------------------------'
      write (iprii,*) ' parname   rank      mean  '
      do ii=1,nopt
      write (iprii,8) parname(ii), irank(ii),help(ii)
8      format(a10,i5,e12.3)
      end do

      end do


c      AOT SENSTIVITY ANALYSING ON OUTPUT VALUES
      end if

      if (isens.gt.0) then
      write (iprii,*)'AOT sensitivity analyses on objective functions '
      rewind(18019)
      rewind(18014)
      do iloop=1,nintval
            read(18019,*)
            read(18019,1) itel, ipar, (xx(kk), kk=1,nopt)
            read(18014,2) itel,(yy(kk),kk=1,isens)
            do kk=1,nopt
            lhx(iloop,kk)=xx(kk)
            end do
            do kk=1,isens
            yobj(iloop,kk)=yy(kk)
            end do
      
                  do jj=1,nopt
                  read(18019,1) itel, ipar, (xx(kk), kk=1,nopt)
                  yhelp=yy                        
                     read(18014,2) itel,(yy(kk),kk=1,isens)
                  do kk=1,isens
            t=yy(kk)-yhelp(kk)
                        oatobj(iloop,ipar,kk)=0.
                  if (abs(t).gt.0) oatobj(iloop,ipar,kk)=abs(t)*50./
     *      (yhelp(kk)+yy(kk))
                  end do
                  end do
      end do
      oatmax=maxval(oatobj,dim=1)

      do ik=1,isens
      write (iprii,*) 'OUTPUT VALUE ', ik
      write (iprii,*) 'results per loop '
            help=0.
            help1=0.
            do jk=1,nintval
            write (iprii,3) jk,(oatobj(jk,kk,ik),kk=1,nopt)
            do kk=1,nopt
            help(kk)=help(kk)+oatobj(jk,kk,ik)
            end do
            end do

            do kk=1,nopt
            help(kk)=help(kk)/nintval
            end do
            
            do jk=1,nintval
            do kk=1,nopt
            help1(kk)=help1(kk)+(help(kk)-oatobj(jk,kk,ik))**2
            end do
            end do      
            
            do kk=1,nopt
            help1(kk)=help1(kk)/(nintval-1)
            end do

      write (iprii,*) '-----------------------------------------------'      
      write (iprii,*) ' maximum '
      write (iprii,4) (oatmax(kk,ik), kk=1,nopt)
      write (iprii,*) ' variance '
      write (iprii,4) (help1(kk), kk=1,nopt)
      write (iprii,*) '-----------------------------------------------'      
      write (iprii,*) ' mean '
      write (iprii,4) (help(kk), kk=1,nopt)

      call sorteer3(help,rank,nopt,irank,ira)
      do ii=1,nopt
      if (rank(ii).eq.0.) ira(ii)=nopt+1
      if (help(ii).eq.0.)  irank(ii)=nopt+1
      end do
      write(18015,56) ik, (irank(kk), kk=1,nopt)
      write (iprii,*) ' ranking '
      write (iprii,5) (irank(kk), kk=1,nopt)
      write (iprii,*) '------------------------------------------------'

      write (iprii,*) '------------------------------------------------'
      write (iprii,*) '      par #     mean  '
      do ii=1,nopt
      write (iprii,8) parname(ii), irank(ii),help(ii)
      end do

      end do
      end if

c      AOT SENSTIVITY ANALYSING ON OUTPUT VALUES
55      format('of ',i3,'|',100i10)
56      format('out',i3,'|',100i10)
5      format (200i12)
4      format(4x,200e12.5)                              
3      format(i4,200e12.5)
1      format(2i4,200f12.5)
2      format(i5,200e12.5)
6      format(20e13.3)
      return
      end
