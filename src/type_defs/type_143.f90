module type_143
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_ascii_pd, only: read_pd
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
      integer, allocatable :: Parameter_Data(:)
      integer              :: n_PD_entries = 0
      integer              :: Type_ID = 0
      character(len=4000)  :: ascii_Parameter_Data = ''
   end type t_143_pd

   type, public :: t_143
      type(t_143_dir)   :: t143dir
      type(t_143_pd)    :: t143param
   contains
      procedure :: read_dir_entries => read_entries
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
      integer(int32), allocatable    :: temp_array_a(:)
      integer(int32), allocatable    :: temp_array_b(:)
      character(len=1)               :: type_letter
      integer                        :: i

      D_record_num = record_start_index

      allocate (buffer(record_span))
      do i = 1, record_span
         read (fileunit, rec=D_record_num, fmt='(a80)') buffer(i)
         D_record_num = D_record_num + 1
      end do

      allocate (temp_array_a(10))
      allocate (temp_array_b(10))

      read (buffer(1), fmt='(9i8,a1,i7)') temp_array_a(1:9), type_letter, temp_array_a(10)
      read (buffer(2), fmt='(9i8,a1,i7)') temp_array_b(1:9), type_letter, temp_array_b(10)
      print *
      print *, temp_array_a
      print *, temp_array_b

      do i = 1, size(temp_array_a)
      end do

   end subroutine read_entries

end module type_143
