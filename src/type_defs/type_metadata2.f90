module type_metadata2
   use iso_fortran_env, only: int32, int64, real32, real64
   use type_dir_meta, only: t_dir_meta
   implicit none

   private

   type, public ::  t_metadata2
      integer(int32)     :: fileunit
      integer(int32)     :: num_records
      integer(int32)     :: num_S_records
      integer(int32)     :: num_G_records
      integer(int32)     :: num_D_records
      integer(int32)     :: num_P_records
      integer(int32)     :: num_T_records
      integer(int32)     :: G_record_start
      integer(int32)     :: P_record_start
      integer(int32)     :: T_record_start
      type(t_dir_meta)   :: directory_metadata
   contains
      procedure :: scan_directory_data => get_directory_metadata
      procedure :: print_metadata2_info => print_metadata2
   end type t_metadata2

contains

   subroutine get_directory_metadata(this)
      class(t_metadata2), intent(inout) :: this
      call this%directory_metadata%get_metadata
   end subroutine get_directory_metadata

   subroutine print_metadata2(this)
      class(t_metadata2), intent(inout) :: this
      call this%directory_metadata%print_dir_metadata
   end subroutine print_metadata2
end module type_metadata2
