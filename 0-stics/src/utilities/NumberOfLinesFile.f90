integer function NumberOfLinesFile(file)

implicit none
  character(len=30), intent(IN) ::file
!  locals
integer :: Nb_line
integer :: ierr ! successful (if == 0) ou failed (si /= 0)

open(1,file=file, form="formatted", iostat=ierr,status="old")

Nb_line = 0
do while (ierr==0)
read(1,*,iostat=ierr)
if (ierr==0) Nb_line = Nb_line + 1
enddo

close(1)
NumberOfLinesFile = NB_line

end function NumberOfLinesFile

