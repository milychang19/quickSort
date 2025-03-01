program iqsort
    ! iterative quick sort algorithm
    ! gfortran -Wall stackADT.f03 intIO.f03 iqsort.f03

    use intIO
    implicit none
    integer, allocatable :: array_int(:)
    integer :: n
    real :: start_time, end_time
    call readUnsorted(array_int, n)     ! prompt the user to provide the input file
    call cpu_time(start_time)           ! get cpu time before execution
    call iterativeQsort(array_int, n)   ! run the iterative quick sort algorithm
    call cpu_time(end_time)             ! get cpu time after execution
    print *, "Iterative Quick Sort cpu time elapsed: ", end_time - start_time
    call writeSorted(array_int, n)      ! output the sorted result to file sortedNUM.txt

contains
    subroutine iterativeQsort(array, n)
        use stackADT
        integer, intent(inout) :: array(:)
        integer, intent(in) :: n
        integer :: l, r, i, j, pivot, temp

        call push(1, n) ! push the original array so we can pop it in the loop
        do while (.not. isEmpty())
            call pop(l, r)
            do while (l < r)
                i = l
                j = r
                pivot = array((l + r) / 2) ! pivot is the value of the middle index

                do
                    do while (array(i) < pivot)
                        i = i + 1
                    end do
                    do while (array(j) > pivot)
                        j = j - 1
                    end do
                    if (i <= j) then
                        temp = array(i)
                        array(i) = array(j)
                        array(j) = temp
                        i = i + 1
                        j = j - 1
                    else
                        exit
                    end if
                end do

                if (j-1 < r-1) then
                    if (i < r) call push(i, r)
                    r = j
                else 
                    if (l < j) call push(l, j)
                    l = i
                end if
            end do
        end do
        call clear() ! clean up the allocated space for stack
    end subroutine iterativeQsort

end program iqsort