module mod_T143_object

  use mod_T128
  use mod_T141_object

implicit none

  type T143_object
    integer                       :: Type
    type(T128)                    :: T128_bspline_surf
    integer                       :: N = 0
    type(T141_object),allocatable :: T141_boundaries(:)
    integer                       :: n_elements_T143_array = 0
  contains
    procedure :: alloc_object => alloc_T143_object
    !procedure :: read_data    => read_T143_data
  end type T143_object


contains

  subroutine alloc_T143_object(this)
    class(T143_object) :: this
    integer :: i
    do i = 1, this%n_elements_T143_array
      if (allocated(this%T141_boundaries)) then
        deallocate(this%T141_boundaries)
      end if
      !allocate(this%T141_boundaries(this%PD(this%T143_indices_array(i))%T143_Data%N))
      print *
    end do
  end subroutine alloc_T143_object

  subroutine read_T143_data(fileunit, n_PD_records, num_record_start, this)
    class(T143_object) :: this
    integer,intent(in) :: fileunit
    integer,intent(in) :: n_PD_records
    integer,intent(in) :: num_record_start
    integer               :: num_PD_entries
    integer               :: Type_ID
    character(len=4000)   :: Raw_Parameter_Data
  end subroutine read_T143_data

end module mod_T143_object
