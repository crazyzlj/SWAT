      subroutine caps(file_name)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the input and output names given in file.cio
!!    and converts all capital letters to lowercase letters.

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    file_name   |NA        |dummy argument, file name character string
!!    ii          |none      |counter in loop
!!    j           |none      |counter in loop
!!    low_case    |NA        |string containing all the lowercase letters
!!    temp_name   |NA        |string with all capitals switched to lowercase
!!    up_case     |NA        |string containing all the uppercase letters
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Len, Index, AdjustL

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      intrinsic Len, Index, AdjustL

      character (len=*) ::  file_name
      character (len=Len(file_name)) ::  temp_name
      character (len=26) :: low_case = "abcdefghijklmnopqrstuvwxyz",  
     &                       up_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      integer :: ii, j

      temp_name = ""
      j = 0

      temp_name = file_name

      do ii = 1, Len(file_name)
       j = Index (up_case,file_name(ii:ii))
       if (j /= 0) temp_name(ii:ii) = low_case(j:j)
      end do

      temp_name = AdjustL (temp_name)     !moves leading blanks to right end
      
      file_name = temp_name

      return
      end