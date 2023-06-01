module Type_Global
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: Global

   type Global
      character(len=10) :: parameter_delim = ''
      character(len=80) :: record_delim = ''
      character(len=80) :: product_id = ''
      character(len=80) :: filename = ''
      character(len=80) :: system_id = ''
      character(len=80) :: preproc_version = ''
      integer(int32)    :: int_binary_bits = 0
      integer(int32)    :: max_power_single_float = 0
      integer(int32)    :: num_sig_digits = 0
      integer(int32)    :: max_power_double = 0
      integer(int32)    :: sig_digits_double = 0
      character(len=80) :: product_id_recieving = ''
      real(real64)      :: model_space_scale = 0.0
      integer(int32)    :: units_flag = 0
      character(len=80) :: units_name = ''
      integer(int32)    :: max_num_line_weights = 0
      real(real64)      :: max_line_weight = 0.0
      character(len=80) :: date_time_generated = ''
      real(real64)      :: min_resolution = 0.0
      real(real64)      :: max_coord_value = 0.0
      character(len=80) :: author = ''
      character(len=80) :: organization = ''
      integer(int32)    :: version_flag = 0
      integer(int32)    :: drafting_flag = 0
      character(len=80) :: date_time_modified = ''
      character(len=80) :: mil_spec = ''
      integer(int32)    :: record_start = 0
   contains
      procedure :: read_G_data => read
   end type Global

contains

   subroutine read (this, fileunit, &
                    n_records_G, num_record_start)
      class(Global), intent(inout) :: this
      integer(int32), intent(in)      :: fileunit
      integer(int32), intent(in)      :: n_records_G
      integer(int32), intent(in)      :: num_record_start
      integer(int32)                  :: i = 0
      character(len=20)               :: string_format = ''
      integer(int32)                  :: G_record_num = 0
      character(len=400), allocatable :: buffer(:)
      character(len=400)              :: buffer_string
      integer(int32)                  :: n_global_entries = 26
      character(len=512), dimension(26) :: test_reader
      character, allocatable             :: char_array(:)

      allocate (buffer(n_records_G))
      G_record_num = num_record_start
      string_format = '(a72)'
      do i = 1, n_records_G
         read (fileunit, rec=G_record_num, fmt=string_format) buffer(i)
         if (i < 2) then
            buffer(i) = trim(buffer(i))
         else
            buffer(i) = trim(buffer(i - 1))//trim(buffer(i))
         end if
         G_record_num = G_record_num + 1
         buffer_string = buffer(i)
      end do
      !read(buffer_string, delim=",") test
      write (*, *) buffer_string
      read (buffer_string, *) test_reader
      print *, 'length :', len(buffer_string)
      !allocate (char_array(len(buffer_string))
      do i = 1, size(test_reader)
         print *, i
         test_reader(i) = trim(test_reader(i))
         print *, test_reader(i)
      end do
      !print *, test_reader
   end subroutine read

end module Type_Global
