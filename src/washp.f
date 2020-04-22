      subroutine washp

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of pesticide washed off the plant
!!    foliage and onto the soil

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    ihru          |none          |HRU number
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    pst_wof(:)    |none          |fraction of pesticide on foliage which
!!                                 |is washed-off by a rainfall event
!!    sol_pst(:,:,1)|kg/ha         |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha         |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    xx          |kg/ha         |amount of pesticide washed off foliage
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k, kk
      real*8 :: xx

      j = 0
      j = ihru

      if (hrupest(j) == 0) return

      do k = 1, npmx
        kk = 0
        kk = npno(k)  
        if (plt_pst(k,j) >= 0.0001) then
          if (kk > 0) then
            xx = 0.
            xx = pst_wof(kk) * plt_pst(k,j)
            if (xx > plt_pst(k,j)) xx = plt_pst(k,j)

            sol_pst(k,j,1) = sol_pst(k,j,1) + xx
            plt_pst(k,j) = plt_pst(k,j) - xx
          end if
        end if
      end do

      return
      end