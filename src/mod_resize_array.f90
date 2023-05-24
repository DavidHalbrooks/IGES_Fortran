module mod_resize_array

  implicit none

contains

  subroutine resize_array(array, n_elements, index_to_add)
    integer, allocatable, intent(inout) :: array(:)
    integer, allocatable                :: temp(:)
    integer, intent(inout)              :: n_elements
    integer, intent(in)                 :: index_to_add

    if (allocated(array)) then
      allocate(temp(size(array)+1))
      temp = 0
      temp(1:size(array)) = array
      call move_alloc(temp,array)
      array(size(array)) = index_to_add
!      write(*,*) array
    else
      allocate(array(1))
      array = 0
      array = index_to_add
    endif
    n_elements = size(array)
  end subroutine resize_array


end module mod_resize_array
