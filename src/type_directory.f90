module type_directory
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private

   type, public :: t_directory
      integer :: entity_type_num = 0
      integer :: parameter_data = 0
      integer :: structure = 0
      integer :: line_font = 0
      integer :: level = 0
      integer :: view = 0
      integer :: transform_matrix = 0
      integer :: label_disp_assoc = 0
      integer :: status_number = 0
      integer :: sequence_num1 = 0
      integer :: entity_type_num_2 = 0
      integer :: line_weight_num = 0
      integer :: color_number = 0
      integer :: param_line_count = 0
      integer :: form_number = 0
      integer :: reserved_16 = 0
      integer :: reserved_17 = 0
      integer :: entity_label = 0
      integer :: entity_subscript = 0
      integer :: sequence_num2 = 0
   contains
      procedure :: read_entries => read
   end type t_directory

contains

   subroutine read (this, fileunit, &
                    num_record_start)
      class(t_directory)    :: this
      integer, intent(in)  :: fileunit
      integer, intent(in)  :: num_record_start
      integer              :: DE_record_num
      character(len=200)   :: buffer1
      character(len=200)   :: buffer2
      character(len=10)    :: string_format
      character(len=180)   :: buffer3
      character(len=400)   :: DE_format
      integer              :: DE_ints1(9)
      integer              :: DE_ints3
      integer              :: DE_ints4(9)
      integer              :: DE_ints6
      character            :: DE_chars2
      character            :: DE_chars5
      integer              :: DE_Data(20)

      !print *
      !write(*,*) '***************************************'
      !write(*,*) 'Starting: read_Entity(this)'
      !print *

      DE_record_num = num_record_start
      DE_format = '(9(i8), (a), (i7), 9(i8), (a), (i7))'
      string_format = '(a80)'
      buffer1 = ''
      buffer2 = ''
      buffer3 = ''
      read (fileunit, rec=DE_record_num, fmt=string_format) buffer1
      DE_record_num = DE_record_num + 1
      read (fileunit, rec=DE_record_num, fmt=string_format) buffer2
      DE_record_num = DE_record_num + 1
      buffer3 = trim(buffer1)//trim(buffer2)
      read (buffer3, fmt=DE_format) DE_ints1, DE_chars2, DE_ints3, &
         DE_ints4, DE_chars5, DE_ints6
      DE_Data(1:9) = DE_Ints1
      DE_Data(10) = DE_Ints3
      DE_Data(11:19) = DE_ints4
      DE_Data(20) = DE_ints6
      this%entity_type_num = DE_Data(1)
      this%parameter_data = DE_Data(2)
      this%structure = DE_Data(3)
      this%line_font = DE_Data(4)
      this%level = DE_Data(5)
      this%view = DE_Data(6)
      this%transform_matrix = DE_Data(7)
      this%label_disp_assoc = DE_Data(8)
      this%status_number = DE_Data(9)
      this%sequence_num1 = DE_Data(10)
      this%entity_type_num_2 = DE_Data(11)
      this%line_weight_num = DE_Data(12)
      this%color_number = DE_Data(13)
      this%param_line_count = DE_Data(14)
      this%form_number = DE_Data(15)
      this%reserved_16 = DE_Data(16)
      this%reserved_17 = DE_Data(17)
      this%entity_label = DE_Data(18)
      this%entity_subscript = DE_Data(19)
      this%sequence_num2 = DE_Data(20)
      !print *
      !write(*,*) 'Completed: read_Entity(this)'
      !write(*,*) '***************************************'

   end subroutine read

end module type_directory
