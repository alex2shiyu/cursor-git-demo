program quicksort_demo
    implicit none
    integer, parameter :: N = 10
    integer :: arr(N)
    integer :: i

    ! Initialize array with some values
    arr = (/64, 34, 25, 12, 22, 11, 90, 88, 45, 2/)
    
    write(*,*) "Original array:"
    write(*,*) arr
    
    call quick_sort(arr, 1, N)
    
    write(*,*) "Sorted array:"
    write(*,*) arr

contains
    recursive subroutine quick_sort(a, left, right)
        integer, intent(inout) :: a(:)
        integer, intent(in) :: sort_left, sort_right
        integer :: sort_pivot, sort_i, sort_j, sort_temp
        
        if (left < right) then
            sort_pivot = a(right)
            sort_i = left - 1
            
            do j = left, right - 1
                if (a(j) <= pivot) then
                    sort_i = sort_i + 1
                    sort_temp = a(sort_i)
                    a(sort_i) = a(j)
                    a(j) = sort_temp
                end if
            end do
            
            sort_temp = a(sort_i + 1)
            a(sort_i + 1) = a(right)
            a(right) = sort_temp
            
            call quick_sort(a, left, sort_i)
            call quick_sort(a, sort_i + 2, right)
        end if
    end subroutine quick_sort
end program quicksort_demo

