program rqsort
    ! recursive quick sort algorithm
    ! gfortran -Wall intIO.f03 rqsort.f03

    use intIO
    implicit none
    integer, allocatable :: array_int(:)
    integer :: n
    real :: start_time, end_time
    call readUnsorted(array_int, n)         ! prompt the user to provide the input file
    call cpu_time(start_time)               ! get cpu time before execution
    call recursiveQsort(array_int, 1, n)    ! run the iterative quick sort algorithm
    call cpu_time(end_time)                 ! get cpu time after execution
    print *, "Recursive Quick Sort cpu time elapsed: ", end_time - start_time
    call writeSorted(array_int, n)          ! output the sorted result to file sortedNUM.txt

contains
    recursive subroutine recursiveQsort(array, l, r)
        integer, intent(inout) :: array(:)
        integer, intent(in) :: l, r
        integer :: i, j, pivot, temp

        if (l >= r) return ! base case
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

        call recursiveQsort(array, i, r)
        call recursiveQsort(array, l, j)

    end subroutine recursiveQsort
end program rqsort