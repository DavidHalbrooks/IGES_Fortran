module Type143

   use read_ascii, only: read_ascii_PD
   implicit none

   Private
   Public :: T143, all_subs_T143

   type T143
      integer              :: Type_BS = 0
      integer              :: SPTR = 0
      integer              :: N = 0
      integer, allocatable :: BDPT(:)
      integer, allocatable :: Parameter_Data(:)
      integer              :: n_PD_entries = 0
      integer              :: Type_ID = 0
      character(len=4000)  :: ascii_Parameter_Data = ''
   contains
      procedure  :: read_ascii => read_ascii_data_T143
      procedure  :: alloc => allocate_init_T143
      procedure  :: read_data => read_BDPT_data_T143
      !procedure  :: deallocate    => deallocate_T143
      !procedure  :: eval_props    => evaluate_param_properties_T143
      procedure  :: call_all_subs => all_subs_T143
   end type T143

contains

   subroutine all_subs_T143(this, Type_ID, num_PD_entries, ascii_Parameter_Data)
      class(T143), intent(inout) :: this
      integer, intent(in)       :: Type_ID
      character(len=4000), intent(in) :: ascii_Parameter_Data
      integer, intent(in)  :: num_PD_entries
      this%n_PD_entries = num_PD_entries
      this%ascii_Parameter_Data = ascii_Parameter_Data
      this%Type_ID = Type_ID
      call read_ascii_data_T143(this)
      call allocate_init_T143(this)
      call read_BDPT_data_T143(this)
      !call deallocate_T143(this)
      !call evaluate_param_properties_T143(this)
   end subroutine all_subs_T143

   subroutine read_ascii_data_T143(this)
      class(T143)         :: this
      allocate (this%Parameter_Data(this%n_PD_entries - 1))
      this%Parameter_Data = 0

      read (this%ascii_Parameter_Data, fmt=*) this%Type_ID, this%Parameter_Data
      this%TYPE_BS = this%Parameter_Data(1)
      this%SPTR = this%Parameter_Data(2)
      this%N = this%Parameter_Data(3)
   end subroutine read_ascii_data_T143

   subroutine allocate_init_T143(this)
      class(T143) :: this
      allocate (this%BDPT(this%N))
      this%BDPT = 0
   end subroutine allocate_init_T143

   subroutine read_BDPT_data_T143(this)
      class(T143) :: this
      integer :: i
      this%BDPT(1) = this%Parameter_Data(4)
      if (this%N > 1) then
         do i = 2, this%N
            this%BDPT(i) = this%Parameter_Data(3 + this%N)
         end do
      end if
   end subroutine read_BDPT_data_T143

   subroutine deallocate_T143(this)
      class(T143) :: this
      deallocate (this%Parameter_Data)
   end subroutine deallocate_T143

!  subroutine evaluate_param_properties_T143(this)
!    class(T143) :: this
!  end subroutine evaluate_param_properties_T143

end module Type143
