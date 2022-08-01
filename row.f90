PROGRAM my_row
   IMPLICIT NONE
   INTEGER nrow
   call filerows(nrow)
   print*,nrow

END PROGRAM
!**********************
subroutine filerows(nrow)
integer nrow 
    integer:: count,ios, whatever
    count=0
    open (10,file='g12003u.txt')
     Do
           read (10,*,iostat=ios) whatever
           if (ios/=0) exit     
         count=count+1
      End Do
    close(10)
	  nrow=count
  return
end
!******************************
