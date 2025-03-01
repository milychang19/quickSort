module intIO
    implicit none

contains
    subroutine readUnsorted(array_int, n)
        integer, allocatable, intent(out) :: array_int(:)
        integer, intent(out) :: n           ! # of integers in the array
        integer :: num, i                   ! temporary variables
        integer :: ierr, unit               ! I/O status and file unit
        character(len=100) :: inputFile     ! input file name

        ierr = 1
        unit = 3

        do while(ierr /= 0)
            print *, "Please enter the file name: "
            read *, inputFile
        
            ! opening the file
            open(unit=unit, file=inputFile, status='old', action='read', iostat=ierr)
            if (ierr /= 0) then
                print *, "Error: Unable to open the file: ", inputFile
                print *, "Please try again."
            end if
        end do

        ! count how many integers are in the file
        n = 0
        do
            read(unit, *, iostat=ierr) num
            if (ierr /= 0) exit  ! Exit the loop when end of file is reached
            n = n + 1
        end do

        allocate(array_int(n))
        rewind(unit)
        ! store the integers to our array
        i = 1 ! fortran index starts from 1
        do
            read(unit, *, iostat=ierr) array_int(i)
            if (ierr /= 0) exit  ! Exit the loop when end of file is reached
            i = i + 1
        end do
        close(unit)

    end subroutine readUnsorted

    subroutine writeSorted(array_int, n)
        integer, intent(in) :: array_int(:)
        integer, intent(in) :: n
        integer :: i, ioerr, unit

        unit = 2
        open(unit, file="sortedNUM.txt", status='unknown', action='write', iostat=ioerr)

        do i = 1, n
            write(unit, '(I0)') array_int(i)
        end do

        close(unit)
        print *, "File written successfully to sortedNUM.txt"
    end subroutine writeSorted

end module intIO