module type_128
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries
   implicit none

   private

   type, public :: t_128_dir
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
   end type t_128_dir

   type, public :: t_128_pd
      integer(int32)                   :: K1 = 0
      integer(int32)                   :: K2 = 0
      integer(int32)                   :: M1 = 0
      integer(int32)                   :: M2 = 0
      integer(int32)                   :: prop1 = 0
      integer(int32)                   :: prop2 = 0
      integer(int32)                   :: prop3 = 0
      integer(int32)                   :: prop4 = 0
      integer(int32)                   :: prop5 = 0
      real(real64), allocatable :: S(:)
      real(real64), allocatable :: T(:)
      real(real64), allocatable :: W(:)
      real(real64), allocatable :: P(:)
      real(real64), allocatable :: U(:)
      real(real64), allocatable :: V(:)
   end type t_128_pd

   type, public :: t_128_metadata
      integer(int32), allocatable :: Parameter_ints(:)
      real(real64), allocatable   :: Parameter_reals(:)
      integer(int32)              :: num_PD_entries = 0
      integer(int32)              :: num_PD_ints = 0
      integer(int32)              :: num_PD_reals = 0
      integer(int32)              :: Type_ID = 0
      character(len=4000)         :: ascii_t128_pd_data = ''
      integer(int32)              :: N1 = 0
      integer(int32)              :: N2 = 0
      integer(int32)              :: A = 0
      integer(int32)              :: B = 0
      integer(int32)              :: C = 0
      integer(int32)              :: i_first_knot_S = 0
      integer(int32)              :: i_last_knot_S = 0
      integer(int32)              :: i_first_knot_T = 0
      integer(int32)              :: i_last_knot_T = 0
      integer(int32)              :: i_first_weight_W = 0
      integer(int32)              :: i_last_weight_W = 0
      integer(int32)              :: i_first_point_P = 0
      integer(int32)              :: i_last_point_P = 0
      integer(int32)              :: i_first_param_U = 0
      integer(int32)              :: i_last_param_U = 0
      integer(int32)              :: i_first_param_V = 0
      integer(int32)              :: i_last_param_V = 0
      integer(int32)              :: n_knots_S = 0
      integer(int32)              :: n_knots_T = 0
      integer(int32)              :: n_weights_W = 0
      integer(int32)              :: n_control_points_P = 0
      integer(int32)              :: n_params_U = 0
      integer(int32)              :: n_params_V = 0
      integer(int32)              :: n_bezier_patches = 0
   end type t_128_metadata

   type, private :: t_128_sharedata
      integer(int32) :: fileunit
      integer(int32) :: D_section_start_index
      integer(int32) :: P_section_start_index
   end type t_128_sharedata

   type, public :: t_128
      type(t_128_dir)        :: t128dir
      type(t_128_pd)         :: t128param
      type(t_128_metadata)   :: t128metadata
      type(t_128_sharedata)  :: t128sharedata
   contains
      !procedure  :: test_type => test_type_T128
      procedure :: exchange_sharedata_t128 => exchange_share
      procedure :: read_t128_dir_entries => read_entries
      procedure :: read_ascii_t128_pd => read_ascii_pd
      procedure :: allocate_t128_pd_initial => allocate_pd_initial
      procedure :: read_t128pd_integers => read_pd_integers
      procedure :: t128_index_calculations => index_calcs
      procedure :: allocate_t128_pd_vectors => allocate_pd_vectors
      procedure :: initialize_t128_pd_vectors => initialize_pd_vectors
      procedure :: read_in_t128_pd_data => read_in_pd_data
      procedure :: print_t128_data => print_data
   end type t_128

contains

   subroutine exchange_share(this, fileunit, D_start_index, P_start_index)
      class(t_128), intent(inout) :: this
      integer(int32), intent(in) :: fileunit
      integer(int32), intent(in) :: D_start_index
      integer(int32), intent(in) :: P_start_index

      ! Send the fileunit to share_data for use by children types
      this%t128sharedata%fileunit = fileunit
      this%t128sharedata%D_section_start_index = D_start_index
      this%t128sharedata%P_section_start_index = P_start_index

   end subroutine exchange_share

   subroutine read_entries(this, D_record_index)
      class(t_128), intent(inout)    :: this
      integer(int32), intent(in)     :: D_record_index
      integer(int32)                 :: D_record_num
      integer, parameter             :: record_span = 2
      integer, parameter             :: num_dir_entries = 20
      character(len=80), allocatable :: buffer(:)
      character(len=160)             :: buffer_ascii
      character(len=1)               :: type_letter1
      character(len=1)               :: type_letter2
      integer                        :: i

      D_record_num = (D_record_index + this%t128sharedata%D_section_start_index - 1)

      allocate (buffer(record_span))
      do i = 1, record_span
         read (this%t128sharedata%fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         buffer_ascii = buffer(i - 1)//buffer(i)
         D_record_num = D_record_num + 1
      end do

      read (buffer_ascii, fmt='(9i8,a1,i7,9i8,a1,i7)') &
         this%t128dir%entity_type_num1, &
         this%t128dir%parameter_data, &
         this%t128dir%structure, &
         this%t128dir%line_font, &
         this%t128dir%level, &
         this%t128dir%view, &
         this%t128dir%transform_matrix, &
         this%t128dir%label_disp_assoc, &
         this%t128dir%status_number, &
         type_letter1, &
         this%t128dir%sequence_num1, &
         this%t128dir%entity_type_num2, &
         this%t128dir%line_weight, &
         this%t128dir%color_number, &
         this%t128dir%param_line_count, &
         this%t128dir%form_number, &
         this%t128dir%reserved_16, &
         this%t128dir%reserved_17, &
         this%t128dir%entity_label, &
         this%t128dir%entity_subscript, &
         type_letter2, &
         this%t128dir%sequence_num2

   end subroutine read_entries

   ! subroutine test_type_T128(this)
   !    class(t_128) :: this
   !    ! if (this%Type_ID == 128) then
   !    !    write (*, *) 'Parameter type to read in is 128, continuing...'
   !    ! else
   !    !    STOP 'ERROR: Parameter Type is not 128, see mod_T128.f90...'
   !    ! end if
   ! end subroutine test_type_T128

   subroutine read_ascii_pd(this)
      class(t_128) :: this
      integer(int32)                 :: record_start_index
      integer(int32)                 :: num_records
      integer(int32)                 :: type_id
      character(len=4000)            :: ascii_pd_buffer

      record_start_index = (this%t128sharedata%P_section_start_index + &
                            this%t128dir%parameter_data - 1)
      call read_pd(this%t128sharedata%fileunit, &
                   record_start_index, &
                   this%t128dir%param_line_count, &
                   type_id, &
                   ascii_pd_buffer)
      this%t128metadata%num_pd_entries = count_pd_entries(ascii_pd_buffer)
      this%t128metadata%ascii_t128_pd_data = trim(ascii_pd_buffer)
   end subroutine read_ascii_pd

   subroutine allocate_pd_initial(this)
      class(t_128), intent(inout) :: this
      this%t128metadata%num_PD_ints = (9)
      this%t128metadata%num_PD_reals = (this%t128metadata%num_PD_entries - 1)   ! DNH Note: Pad first 9 entries for indexes
      allocate (this%t128metadata%Parameter_ints(this%t128metadata%num_PD_ints))
      allocate (this%t128metadata%Parameter_reals(this%t128metadata%num_PD_reals))
      this%t128metadata%Parameter_ints = 0
      this%t128metadata%Parameter_reals = 0.0
   end subroutine allocate_pd_initial

   subroutine read_pd_integers(this)
      class(t_128)         :: this

      this%t128metadata%ascii_t128_pd_data = &
         trim(this%t128metadata%ascii_t128_pd_data)
      read (this%t128metadata%ascii_t128_pd_data, fmt=*) &
         this%t128metadata%Type_ID, &
         this%t128metadata%Parameter_ints, &
         this%t128metadata%Parameter_reals(10:size(this%t128metadata%Parameter_reals))

      this%t128param%K1 = this%t128metadata%Parameter_ints(1)
      this%t128param%K2 = this%t128metadata%Parameter_ints(2)
      this%t128param%M1 = this%t128metadata%Parameter_ints(3)
      this%t128param%M2 = this%t128metadata%Parameter_ints(4)
      this%t128param%prop1 = this%t128metadata%Parameter_ints(5)
      this%t128param%prop2 = this%t128metadata%Parameter_ints(6)
      this%t128param%prop3 = this%t128metadata%Parameter_ints(7)
      this%t128param%prop4 = this%t128metadata%Parameter_ints(8)
      this%t128param%prop5 = this%t128metadata%Parameter_ints(9)

   end subroutine read_pd_integers

   subroutine index_calcs(this)
      class(t_128) :: this
      this%t128metadata%N1 = (1 + this%t128param%K1 - this%t128param%M1)
      this%t128metadata%N2 = (1 + this%t128param%K2 - this%t128param%M2)
      this%t128metadata%A = (this%t128metadata%N1 + (2*this%t128param%M1))
      this%t128metadata%B = (this%t128metadata%N2 + (2*this%t128param%M2))
      this%t128metadata%C = ((1 + this%t128param%K1)*(1 + this%t128param%K2))
      this%t128metadata%i_first_knot_S = 10
      this%t128metadata%i_last_knot_S = (10 + this%t128metadata%A)
      this%t128metadata%i_first_knot_T = (11 + this%t128metadata%A)
      this%t128metadata%i_last_knot_T = (11 + this%t128metadata%A + this%t128metadata%B)
      this%t128metadata%i_first_weight_W = (12 + this%t128metadata%A + this%t128metadata%B)
      this%t128metadata%i_last_weight_W = (11 + this%t128metadata%A + this%t128metadata%B + this%t128metadata%C)
      this%t128metadata%i_first_point_P = (12 + this%t128metadata%A + this%t128metadata%B + this%t128metadata%C)
      this%t128metadata%i_last_point_P = (11 + this%t128metadata%A + this%t128metadata%B + (4*this%t128metadata%C))
      this%t128metadata%i_first_param_U = (12 + this%t128metadata%A + this%t128metadata%B + (4*this%t128metadata%C))
      this%t128metadata%i_last_param_U = (13 + this%t128metadata%A + this%t128metadata%B + (4*this%t128metadata%C))
      this%t128metadata%i_first_param_V = (14 + this%t128metadata%A + this%t128metadata%B + (4*this%t128metadata%C))
      this%t128metadata%i_last_param_V = (15 + this%t128metadata%A + this%t128metadata%B + (4*this%t128metadata%C))
      this%t128metadata%n_knots_S = (this%t128metadata%i_last_knot_S - this%t128metadata%i_first_knot_S + 1)
      this%t128metadata%n_knots_T = (this%t128metadata%i_last_knot_T - this%t128metadata%i_first_knot_T + 1)
      this%t128metadata%n_weights_W = (this%t128metadata%i_last_weight_W - this%t128metadata%i_first_weight_W + 1)
      this%t128metadata%n_control_points_P = (this%t128metadata%i_last_point_P - this%t128metadata%i_first_point_P + 1)
      this%t128metadata%n_params_U = (this%t128metadata%i_last_param_U - this%t128metadata%i_first_param_U + 1)
      this%t128metadata%n_params_V = (this%t128metadata%i_last_param_V - this%t128metadata%i_first_param_V + 1)
      !this%t128metadata%n_bezier_patches = this%t128metadata%n_control_points - this%t128metadata%K + 1 !Update for patches DNH: 01-13-23
   end subroutine index_calcs

   subroutine allocate_pd_vectors(this)
      class(t_128) :: this
      allocate (this%t128param%S(this%t128metadata%n_knots_S))
      allocate (this%t128param%T(this%t128metadata%n_knots_T))
      allocate (this%t128param%W(this%t128metadata%n_weights_W))
      allocate (this%t128param%P(this%t128metadata%n_control_points_P))
      allocate (this%t128param%U(this%t128metadata%n_params_U))
      allocate (this%t128param%V(this%t128metadata%n_params_V))
   end subroutine allocate_pd_vectors

   subroutine initialize_pd_vectors(this)
      class(t_128) :: this
      this%t128param%S = 0.0
      this%t128param%T = 0.0
      this%t128param%W = 0.0
      this%t128param%P = 0.0
      this%t128param%U = 0.0
      this%t128param%V = 0.0
   end subroutine initialize_pd_vectors

   subroutine read_in_pd_data(this)
      class(t_128) :: this
      this%t128param%S = this%t128metadata%Parameter_reals(this%t128metadata%i_first_knot_S:this%t128metadata%i_last_knot_S)
      this%t128param%T = this%t128metadata%Parameter_reals(this%t128metadata%i_first_knot_T:this%t128metadata%i_last_knot_T)
      this%t128param%W = this%t128metadata%Parameter_reals(this%t128metadata%i_first_weight_W:this%t128metadata%i_last_weight_W)
      this%t128param%P = this%t128metadata%Parameter_reals(this%t128metadata%i_first_point_P:this%t128metadata%i_last_point_P)
      this%t128param%U = this%t128metadata%Parameter_reals(this%t128metadata%i_first_param_U:this%t128metadata%i_last_param_U)
      this%t128param%V = this%t128metadata%Parameter_reals(this%t128metadata%i_first_param_V:this%t128metadata%i_last_param_V)
   end subroutine read_in_pd_data

   subroutine print_data(this)
      class(t_128), intent(inout) :: this
      print *, 'T128 Directory data:'
      print *, this%t128dir
      print *
      print *, 'T128 S data: '
      write (*, *) this%t128param%S
      print *
      print *, 'T128 T data: '
      write (*, *) this%t128param%T
      print *
      print *, 'T128 W data: '
      write (*, *) this%t128param%W
      print *
      print *, 'T128 P data: '
      write (*, *) this%t128param%P
      print *
      print *, 'T128 U data: '
      write (*, *) this%t128param%U
      print *
      print *, 'T128 V data: '
      write (*, *) this%t128param%V
   end subroutine print_data

end module type_128
