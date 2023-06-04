module Type_Metadata2
   use iso_fortran_env, only: int32, int64, real32, real64
   use Type_Dir_info, only: Dir_info
   implicit none

   private
   public :: Metadata2

   type Metadata2
      !integer(int32)     :: fileunit
      integer(int32)     :: num_records = 0
      integer(int32)     :: num_S_records = 0
      integer(int32)     :: num_G_records = 0
      integer(int32)     :: num_D_records = 0
      integer(int32)     :: num_P_records = 0
      integer(int32)     :: num_T_records = 0
      integer(int32)     :: G_record_start = 0
      integer(int32)     :: D_record_start = 0
      integer(int32)     :: P_record_start = 0
      integer(int32)     :: T_record_start = 0
      type(Dir_info)     :: Directory_info
   contains
      !procedure :: calc_record_start_pos => calc
      procedure :: scan_directory_data => scan_directory
   end type Metadata2

contains

   subroutine scan_directory(this)
      class(Metadata2), intent(inout) :: this
      call get_dir_indices(this%fileunit, &
                           this%D_record_start, &
                           this%num_D_Records)
   end subroutine scan_directory

end module Type_Metadata2
