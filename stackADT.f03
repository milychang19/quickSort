module stackADT
    implicit none
    integer, parameter :: m =  24 ! maximum size of the stack is log(100,000) = 16.6 
    integer :: stack_ptr = 0  ! stack pointer
    integer, allocatable :: stack(:,:)  ! each stack elements contains 2 columns l and r

contains
    subroutine push(l, r)
        integer, intent(in) :: l, r
        if (stack_ptr == m) then
            print *, "Stack Overflow: Unable to push. Stack is full."
        else
            stack_ptr = stack_ptr + 1
            if (.not. allocated(stack)) then
                allocate(stack(m, 2)) 
            end if
            stack(stack_ptr, 1) = l
            stack(stack_ptr, 2) = r
        end if
    end subroutine push

    subroutine pop(l, r)
        integer, intent(out) :: l, r
        if (stack_ptr == 0) then
            print *, "Stack Underflow: Unable to pop. Stack is empty."
            l = -1
            r = -1
        else
            l = stack(stack_ptr, 1)
            r = stack(stack_ptr, 2)
            stack_ptr = stack_ptr - 1
        end if
    end subroutine pop

    subroutine clear()
        if (allocated(stack)) then
            deallocate(stack)
        end if
        stack_ptr = 0
    end subroutine clear

    function isEmpty() result(empty)
        logical :: empty

        if (stack_ptr == 0) then
            empty = .true.
        else
            empty = .false.
        end if
    end function isEmpty

end module stackADT
