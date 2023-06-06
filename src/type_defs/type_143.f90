module type_143
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
   use count_pd, only: count_pd_entries
   implicit none

   private

   type, public :: t_143_dir
      integer :: entity_type_num1 = 0
      integer :: parameter_data = 0
      integer :: structure = 0
      integer :: line_font = 0
      integer :: level = 0
      integer :: view = 0
      integer :: transform_matrix = 0
      integer :: label_disp_assoc = 0
      integer :: status_number = 0
      integer :: sequence_num1 = 0
      integer :: entity_type_num2 = 0
      integer :: line_weight = 0
      integer :: color_number = 0
      integer :: param_line_count = 0
      integer :: form_number = 0
      integer :: reserved_16 = 0
      integer :: reserved_17 = 0
      integer :: entity_label = 0
      integer :: entity_subscript = 0
      integer :: sequence_num2 = 0
   end type t_143_dir

   type, public :: t_143_pd
      integer              :: Type_BS = 0
      integer              :: SPTR = 0
      integer              :: N = 0
      integer, allocatable :: BDPT(:)
      integer              :: n_PD_entries = 0
      integer              :: Type_ID = 0
   end type t_143_pd

   type, public :: t_143
      type(t_143_dir)   :: t143dir
      type(t_143_pd)    :: t143param
   contains
      procedure :: read_t143_dir_entries => read_entries
      procedure :: print_t143_dir_entries => print_entries
      procedure :: read_t143_pd_entries => read_pd_entries
   end type t_143

contains

   subroutine read_entries(this, fileunit, record_start_index)
      class(t_143), intent(inout)    :: this
      integer(int32), intent(in)     :: fileunit
      integer(int32), intent(in)     :: record_start_index
      integer(int32)                 :: D_record_num
      integer, parameter             :: record_span = 2
      integer, parameter             :: num_dir_entries = 20
      character(len=80), allocatable :: buffer(:)
      character(len=160)             :: buffer_ascii
      character(len=1)               :: type_letter1
      character(len=1)               :: type_letter2
      integer                        :: i

      D_record_num = record_start_index

      allocate (buffer(record_span))
      do i = 1, record_span
         read (fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         buffer_ascii = buffer(i - 1)//buffer(i)
         D_record_num = D_record_num + 1
      end do

      read (buffer_ascii, fmt='(9i8,a1,i7,9i8,a1,i7)') &
         this%t143dir%entity_type_num1, &
         this%t143dir%parameter_data, &
         this%t143dir%structure, &
         this%t143dir%line_font, &
         this%t143dir%level, &
         this%t143dir%view, &
         this%t143dir%transform_matrix, &
         this%t143dir%label_disp_assoc, &
         this%t143dir%status_number, &
         type_letter1, &
         this%t143dir%sequence_num1, &
         this%t143dir%entity_type_num2, &
         this%t143dir%line_weight, &
         this%t143dir%color_number, &
         this%t143dir%param_line_count, &
         this%t143dir%form_number, &
         this%t143dir%reserved_16, &
         this%t143dir%reserved_17, &
         this%t143dir%entity_label, &
         this%t143dir%entity_subscript, &
         type_letter2, &
         this%t143dir%sequence_num2

   end subroutine read_entries

   subroutine print_entries(this)
      class(t_143), intent(inout) :: this
      print *
      print *, this%t143dir
   end subroutine print_entries

   subroutine read_pd_entries(this, fileunit, P_record_start_index)
      class(t_143), intent(inout)    :: this
      integer(int32), intent(in)     :: fileunit
      integer(int32), intent(in)     :: P_record_start_index
      integer(int32)                 :: record_start_index
      integer(int32)                 :: num_records
      integer(int32)                 :: num_pd_entries
      integer(int32)                 :: type_id
      integer(int32)                 :: temp
      character(len=4000)            :: ascii_pd_buffer

      ascii_pd_buffer = ''
      num_pd_entries = 0
      temp = 0

      record_start_index = (P_record_start_index + &
                            this%t143dir%parameter_data - 1)
      call read_pd(fileunit, &
                   record_start_index, &
                   this%t143dir%param_line_count, &
                   type_id, &
                   ascii_pd_buffer)
      ! NOTE: Future work: Include the num_pd_entries in the pd metadata
      num_pd_entries = count_pd_entries(ascii_pd_buffer)

      read (ascii_pd_buffer, fmt=*) temp, temp, temp, this%t143param%N
      allocate (this%t143param%BDPT(this%t143param%N))

      print *
      print *, trim(ascii_pd_buffer)
      ascii_pd_buffer = trim(ascii_pd_buffer)
      read (ascii_pd_buffer, fmt=*) &
         this%t143param%Type_ID, &
         this%t143param%Type_BS, &
         this%t143param%SPTR, &
         this%t143param%N, &
         this%t143param%BDPT

      print *, this%t143param%BDPT

   end subroutine read_pd_entries

end module type_143
