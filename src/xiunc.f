      subroutine xiunc(omin,nobs,nopt,igoc,iprob,icalml,objflim,objfmin)
c      this subroutine calculates the threshold on the objective function based on 
c      XI-Squared statisitics       
c      written by Ann van Griensven - University of California Riverside, april 2003

      real*8 vobj(igoc), vpar(nopt), omin(igoc), varobj(igoc)
        real*8 xisquare
      integer nobs(igoc), iprob, nopt
      real*8 objfmin, objflim, bestpar(nopt)
      rewind(18014)
      rewind(18012)
      rewind(18013)
      rewind(18020) 
      rewind(18018)
      read(18012,*)
      ii=0
      objfmin=1.e10

      do mm=1,igoc
        varobj(mm)=omin(mm)/nobs(mm)
      end do
      do i=1,icalml
                  read(18012,5) ik,(vobj(kk), kk=1,igoc)
                  read(18013,5) ik,(vpar(kk),kk=1,nopt)

            objf=0.
            do mm=1,igoc
            objf=objf+vobj(mm)/(varobj(mm)*2)
            end do
          if (objf.lt.objfmin) then
          objfmin=objf
          bestpar=vpar
          isim=i
          end if
          write(18018,18) i, ii,vpar, objf

18     format(2i5,100e12.5)
5     format(1i5,100e12.5)
      end do
      write(18020,5) isim,(bestpar(kk),kk=1,nopt)
      nobstot=0
      do mm=1,igoc
      nobstot=nobs(mm)+nobstot
      end do

      objflim=objfmin*(1+xisquare(iprob,nopt)/(nobstot-nopt))
      return
      end
