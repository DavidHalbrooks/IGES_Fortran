module type_141
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries
   implicit none

   private

   type, public :: t_141_dir
      integer(int32) :: entity_type_num1 = 0
      integer(int32) :: parameter_data = 0
      integer(int32) :: structure = 0
      integer(int32) :: line_font = 0
      integer(int32) :: level = 0
      integer(int32) :: view = 0
      integer(int32) :: transform_matrix = 0
      integer(int32) :: label_disp_assoc = 0
      integer(int32) :: status_number = 0
      integer(int32) :: sequence_num1 = 0
      integer(int32) :: entity_type_num2 = 0
      integer(int32) :: line_weight = 0
      integer(int32) :: color_number = 0
      integer(int32) :: param_line_count = 0
      integer(int32) :: form_number = 0
      integer(int32) :: reserved_16 = 0
      integer(int32) :: reserved_17 = 0
      integer(int32) :: entity_label = 0
      integer(int32) :: entity_subscript = 0
      integer(int32) :: sequence_num2 = 0
   end type t_141_dir

   type, private :: t_141_sharedata
      integer(int32) :: fileunit
      integer(int32) :: D_section_start_index
      integer(int32) :: P_section_start_index
   end type t_141_sharedata

   type, public :: t_141
      type(t_141_dir)        :: t141dir
      !type(t_141_pd)         :: t141param
      !type(t_141_metadata)   :: t141metadata
      type(t_141_sharedata)  :: t141sharedata
   contains
      procedure :: exchange_sharedata_t141
      ! procedure :: read_t141_dir_entries => read_entries
      ! procedure :: read_ascii_t141_pd => read_ascii_pd
      ! procedure :: allocate_t141_pd_initial => allocate_pd_initial
      ! procedure :: read_t141pd_integers => read_pd_integers
      ! procedure :: t141_index_calculations => index_calcs
      ! procedure :: allocate_t141_pd_vectors => allocate_pd_vectors
      ! procedure :: initialize_t141_pd_vectors => initialize_pd_vectors
      ! procedure :: read_in_t141_pd_data => read_in_pd_data
      ! procedure :: print_t141_data => print_data
   end type t_141

contains

   subroutine exchange_sharedata_t141(this, fileunit, D_start_index, P_start_index)
      class(t_141), intent(inout) :: this
      integer(int32), intent(in) :: fileunit
      integer(int32), intent(in) :: D_start_index
      integer(int32), intent(in) :: P_start_index

      print *, this%t141dir%level
      !this%t141sharedata%D_section_start_index = D_start_index
      !this%t141sharedata%P_section_start_index = P_start_index

      ! Send the fileunit to share_data for use by children types
      ! this%t141sharedata%fileunit = fileunit
      ! this%t141sharedata%D_section_start_index = D_start_index
      ! this%t141sharedata%P_section_start_index = P_start_index
   end subroutine exchange_sharedata_t141

end module type_141
