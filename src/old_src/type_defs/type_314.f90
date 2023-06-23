module Type314

   !use mod_read_raw_parametric
   use iso_fortran_env, only: int32, int64, real32, real64

   implicit none

   private
   public :: T314, all_subs_T314

   Type T314
      real(real64)        :: CC1 = 0.0
      real(real64)        :: CC2 = 0.0
      real(real64)        :: CC3 = 0.0
      character(len=80)   :: CNAME = 'NO_NAME'
      integer             :: Type_ID = 0
      character(len=4000) :: Raw_Parameter_Data = ''
      integer             :: num_PD_entries = 0
   contains
      procedure  :: test_type => test_type_T314
      procedure  :: read_T314 => read_data_T314
      procedure  :: eval_props => evaluate_param_properties_T314
      procedure  :: import_subs_T314 => all_subs_T314
   end type T314

contains

   subroutine all_subs_T314(this, Type_ID, num_PD_entries, Raw_Parameter_Data)
      class(T314), intent(inout) :: this
      character(len=4000), intent(in) :: Raw_Parameter_Data
      integer, intent(in) :: Type_ID
      integer, intent(in)  :: num_PD_entries
      this%num_PD_entries = num_PD_entries
      this%Raw_Parameter_Data = Raw_Parameter_Data
      this%Type_ID = Type_ID
      call test_type_T314(this)
      call read_data_T314(this)
      call evaluate_param_properties_T314(this)
   end subroutine all_subs_T314

   subroutine test_type_T314(this)
      class(T314)       :: this
      character(len=20) :: read_format
      read_format = '(i3)'
      read (this%Raw_Parameter_Data, fmt=read_format) this%Type_ID
      if (this%Type_ID == 314) then
         write (*, *) 'Parameter type to read in is 314, continuing...'
      else
         STOP 'ERROR: Parameter Type is not 314, see mod_T314.f90...'
      end if
   end subroutine test_type_T314

   subroutine read_data_T314(this)
      class(T314) :: this
      !character(len=20) :: read_format
      !read_format = '((i3), 3(f16.13), (a))'
      read (this%Raw_Parameter_Data, fmt=*) this%Type_ID, &
         this%CC1, &
         this%CC2, &
         this%CC3, &
         this%CNAME
      write (*, *) 'T314 Data read in ...'
      !write(*,*) this%Type_ID, this%CC1, this%CC2, this%CC3, this%CNAME
   end subroutine read_data_T314

   subroutine evaluate_param_properties_T314(this)
      class(T314) :: this
   end subroutine evaluate_param_properties_T314

end module Type314
