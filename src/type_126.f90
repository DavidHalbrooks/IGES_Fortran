module type_126
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries

   implicit none

   Private

   type, public :: t_126_dir
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
   end type t_126_dir

   type, public :: t_126_pd
      integer(int32)               :: K = 0
      integer(int32)               :: M = 0
      integer(int32)               :: prop1 = 0
      integer(int32)               :: prop2 = 0
      integer(int32)               :: prop3 = 0
      integer(int32)               :: prop4 = 0
      real(real64), allocatable    :: T(:)
      real(real64), allocatable    :: W(:)
      real(real64), allocatable    :: Px(:)
      real(real64), allocatable    :: Py(:)
      real(real64), allocatable    :: Pz(:)
      real(real64), allocatable    :: V(:)
      real(real64)                 :: Xnorm = 0.0
      real(real64)                 :: Ynorm = 0.0
      real(real64)                 :: Znorm = 0.0

   end type t_126_pd

   type, public :: t_126_metadata
      integer(int32)               :: Type_ID = 0
      integer(int32)               :: N = 0
      integer(int32)               :: A = 0
      integer(int32), allocatable  :: Parameter_ints(:)
      real(real64), allocatable    :: Parameter_reals(:)
      integer(int32)               :: num_PD_ints = 0
      integer(int32)               :: num_PD_reals = 0
      character(len=160)           :: ascii_t126_dir_data = ''
      character(len=4000)          :: ascii_t126_pd_data = ''
      integer(int32)               :: num_PD_entries = 0
      integer(int32)               :: num_control_points = 0
      integer(int32)               :: num_knots = 0
      integer(int32)               :: num_weights = 0
      integer(int32)               :: num_bezier_curves = 0
   end type t_126_metadata

   type, private :: t_126_sharedata
      integer(int32) :: fileunit = 0
      integer(int32) :: D_section_start_index = 0
      integer(int32) :: P_section_start_index = 0
   end type t_126_sharedata

   type, public :: t_126
      type(t_126_dir)        :: t126dir
      type(t_126_pd)         :: t126param
      type(t_126_metadata)   :: t126metadata
      type(t_126_sharedata)  :: t126sharedata
   contains
      procedure :: exchange_sharedata_t126 => exchange_share
      procedure :: read_t126_dir_entries => read_entries
      procedure :: read_ascii_t126_pd => read_ascii_pd
      procedure :: allocate_t126_pd_initial => allocate_pd_initial
      procedure :: read_t126pd_integers => read_pd_integers
      procedure :: t126_index_calculations => index_calcs
      procedure :: allocate_t126_pd_vectors => allocate_pd_vectors
      procedure :: initialize_t126_pd_vectors => initialize_pd_vectors
      procedure :: read_in_t126_pd_data => read_in_pd_data
      procedure :: print_t126_dir_data => print_dir_data
      procedure :: print_t126_pd_data => print_pd_data
      !procedure  :: test_type => test_type_T126
   end type t_126

contains

   subroutine exchange_share(this, fileunit, D_start_index, P_start_index)
      class(t_126), intent(inout) :: this
      integer(int32), intent(in) :: fileunit
      integer(int32), intent(in) :: D_start_index
      integer(int32), intent(in) :: P_start_index

      ! Send the fileunit to share_data for use by children types
      this%t126sharedata%fileunit = fileunit
      this%t126sharedata%D_section_start_index = D_start_index
      this%t126sharedata%P_section_start_index = P_start_index

   end subroutine exchange_share

   subroutine read_entries(this, D_record_index)
      class(t_126), intent(inout)    :: this
      integer(int32), intent(in)     :: D_record_index
      integer(int32)                 :: D_record_num
      integer, parameter             :: record_span = 2
      integer, parameter             :: num_dir_entries = 20
      character(len=80), allocatable :: buffer(:)
      character(len=160)             :: buffer_ascii
      character(len=1)               :: type_letter1
      character(len=1)               :: type_letter2
      integer                        :: i

      D_record_num = (D_record_index + this%t126sharedata%D_section_start_index - 1)

      allocate (buffer(record_span))
      do i = 1, record_span
         read (this%t126sharedata%fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         D_record_num = D_record_num + 1
         if (i < 2) then
            buffer(i) = trim(buffer(i))
         else
            buffer_ascii = trim(buffer(i - 1))//trim(buffer(i))
         end if
      end do
      this%t126metadata%ascii_t126_dir_data = buffer_ascii

      read (buffer_ascii, fmt='(9i8,a1,i7,9i8,a1,i7)') &
         this%t126dir%entity_type_num1, &
         this%t126dir%parameter_data, &
         this%t126dir%structure, &
         this%t126dir%line_font, &
         this%t126dir%level, &
         this%t126dir%view, &
         this%t126dir%transform_matrix, &
         this%t126dir%label_disp_assoc, &
         this%t126dir%status_number, &
         type_letter1, &
         this%t126dir%sequence_num1, &
         this%t126dir%entity_type_num2, &
         this%t126dir%line_weight, &
         this%t126dir%color_number, &
         this%t126dir%param_line_count, &
         this%t126dir%form_number, &
         this%t126dir%reserved_16, &
         this%t126dir%reserved_17, &
         this%t126dir%entity_label, &
         this%t126dir%entity_subscript, &
         type_letter2, &
         this%t126dir%sequence_num2

   end subroutine read_entries

   subroutine read_ascii_pd(this)
      class(t_126) :: this
      integer(int32)                 :: record_start_index
      integer(int32)                 :: num_records
      integer(int32)                 :: type_id
      character(len=4000)            :: ascii_pd_buffer

      record_start_index = (this%t126sharedata%P_section_start_index + &
                            this%t126dir%parameter_data - 1)
      call read_pd(this%t126sharedata%fileunit, &
                   record_start_index, &
                   this%t126dir%param_line_count, &
                   type_id, &
                   ascii_pd_buffer)
      this%t126metadata%num_pd_entries = count_pd_entries(ascii_pd_buffer)
      this%t126metadata%ascii_t126_pd_data = trim(ascii_pd_buffer)
   end subroutine read_ascii_pd

   subroutine allocate_pd_initial(this)
      class(t_126), intent(inout) :: this
      this%t126metadata%num_PD_ints = 6
      this%t126metadata%num_PD_reals = (this%t126metadata%num_PD_entries - 1)   ! DNH Note: Pad first 9 entries for indexes
      allocate (this%t126metadata%Parameter_ints(this%t126metadata%num_PD_ints))
      allocate (this%t126metadata%Parameter_reals(this%t126metadata%num_PD_reals))
      this%t126metadata%Parameter_ints = 0
      this%t126metadata%Parameter_reals = 0.0
   end subroutine allocate_pd_initial

   subroutine read_pd_integers(this)
      class(t_126)         :: this

      this%t126metadata%ascii_t126_pd_data = &
         trim(this%t126metadata%ascii_t126_pd_data)
      read (this%t126metadata%ascii_t126_pd_data, fmt=*) &
         this%t126metadata%Type_ID, &
         this%t126metadata%Parameter_ints, &
         this%t126metadata%Parameter_reals(7:size(this%t126metadata%Parameter_reals)) ! Note: pad first 6 reals with zeros

      this%t126param%K = this%t126metadata%Parameter_ints(1)
      this%t126param%M = this%t126metadata%Parameter_ints(2)
      this%t126param%prop1 = this%t126metadata%Parameter_ints(3)
      this%t126param%prop2 = this%t126metadata%Parameter_ints(4)
      this%t126param%prop3 = this%t126metadata%Parameter_ints(5)
      this%t126param%prop4 = this%t126metadata%Parameter_ints(6)
   end subroutine read_pd_integers

   subroutine index_calcs(this)
      class(t_126) :: this
      this%t126metadata%N = (1 + this%t126param%K - this%t126param%M)
      this%t126metadata%A = (this%t126metadata%N + 2*this%t126param%M)
   end subroutine index_calcs

   subroutine allocate_pd_vectors(this)
      class(t_126) :: this
      allocate (this%t126param%T(-this%t126param%M:this%t126param%M + this%t126metadata%N))
      allocate (this%t126param%W(0:this%t126param%K))
      allocate (this%t126param%Px(0:this%t126param%K))
      allocate (this%t126param%Py(0:this%t126param%K))
      allocate (this%t126param%Pz(0:this%t126param%K))
      allocate (this%t126param%V(0:1))
   end subroutine allocate_pd_vectors

   subroutine initialize_pd_vectors(this)
      class(t_126) :: this
      this%t126param%T = 0.0
      this%t126param%W = 0.0
      this%t126param%Px = 0.0
      this%t126param%Py = 0.0
      this%t126param%Pz = 0.0
      this%t126param%V = 0.0
   end subroutine initialize_pd_vectors

   subroutine read_in_pd_data(this)
      class(t_126) :: this
      integer(int32) :: i, index, stride

      this%t126param%T = this%t126metadata%Parameter_reals(7:(7 + this%t126metadata%A))
      this%t126param%W = this%t126metadata%Parameter_reals(8 + this%t126metadata%A:8 + this%t126metadata%A + this%t126param%K)

      index = (9 + this%t126metadata%A + this%t126param%K)
      stride = 3
      do i = 0, this%t126param%K
         this%t126param%Px(i) = this%t126metadata%Parameter_reals(index)
         this%t126param%Py(i) = this%t126metadata%Parameter_reals(index + 1)
         this%t126param%Pz(i) = this%t126metadata%Parameter_reals(index + 2)
         index = index + stride
      end do

      this%t126param%V(0) = this%t126metadata%Parameter_reals(12 + this%t126metadata%A + 4*this%t126param%K)
      this%t126param%V(1) = this%t126metadata%Parameter_reals(13 + this%t126metadata%A + 4*this%t126param%K)
      this%t126param%Xnorm = this%t126metadata%Parameter_reals(14 + this%t126metadata%A + 4*this%t126param%K)
      this%t126param%Ynorm = this%t126metadata%Parameter_reals(15 + this%t126metadata%A + 4*this%t126param%K)
      this%t126param%Znorm = this%t126metadata%Parameter_reals(16 + this%t126metadata%A + 4*this%t126param%K)

   end subroutine read_in_pd_data

   subroutine print_dir_data(this)
      class(t_126) :: this
      print *
      print *, 'T126 Directory Data'
      print *, this%t126metadata%ascii_t126_dir_data(1:80)
      print *, this%t126metadata%ascii_t126_dir_data(81:160)
      !print *, this%t126dir
   end subroutine print_dir_data

   subroutine print_pd_data(this)
      class(t_126) :: this
      integer(int32) :: i
      print *
      print *, "T126 Parameter Data"
      print *, 'T vector :', this%t126param%T(-this%t126param%M:this%t126metadata%N + this%t126param%M)
      print *, 'W vector :', this%t126param%W(0:size(this%t126param%W) - 1)
      print *, 'P Control Points :'
      do i = 0, this%t126param%K
         write (*, '(a3,i3)') 'P :', i
         write (*, '(f10.4)') this%t126param%Px(i)
         write (*, '(f10.4)') this%t126param%Py(i)
         write (*, '(f10.4)') this%t126param%Pz(i)
      end do
      print *, 'V0       :', this%t126param%V(0)
      print *, 'V1       :', this%t126param%V(1)
      print *, 'X norm   :', this%t126param%Xnorm
      print *, 'Y norm   :', this%t126param%Ynorm
      print *, 'Z norm   :', this%t126param%Znorm

   end subroutine print_pd_data

   ! subroutine test_type_T126(this)
   !    class(T126) :: this
   !    if (this%Type_ID == 126) then
   !       write (*, *) 'Parameter type to read in is 126, continuing...'
   !    else
   !       STOP 'ERROR: Parameter Type is not 126, see mod_T126.f90...'
   !    end if
   ! end subroutine test_type_T126

end module type_126
