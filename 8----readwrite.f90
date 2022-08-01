PROGRAM my_readwrite
  IMPLICIT NONE
  INTEGER           :: i,n 
  REAL              :: A(5)
  CHARACTER(len=5)  :: inputn_1
  CHARACTER(len=2)  :: inputn_2
  CHARACTER(len=4)  :: inputn_3
  CHARACTER(len=12) :: inputname
  CHARACTER(len=1)  :: cnum
  CHARACTER(len=6)  :: outputn_1
  CHARACTER(len=2)  :: outputn_2
  CHARACTER(len=4)  :: outputn_3
  CHARACTER(len=13) :: outputname

  inputn_1='input'
  inputn_2='00'
  inputn_3='.txt'
  outputn_1='output'
  outputn_2='00'
  outputn_3='.txt'

     DO n=1,3
                  write(cnum,'(i1)') n
                  inputname=  inputn_1// inputn_2 // cnum // inputn_3
                  outputname=  outputn_1// outputn_2 // cnum // outputn_3
              
                  open(unit=1 ,file=inputname)
                  open(2,file=outputname)
        DO i=1,5
              read(1,*) a(i)
              write(2,*)a(i)
        END DO
    END DO
END
!**************************************************




