module Type_Global
   use iso_fortran_env, only: int32, int64, real32, real64
   use read_global_ascii, only: extract_ascii_vector
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
      !integer(int32)    :: record_start = 0
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
      integer(int32)                  :: j = 0
      character(len=20)               :: string_format = ''
      integer(int32)                  :: G_record_num = 0
      character(len=800), allocatable :: buffer(:)
      character(len=800)              :: buffer_string
      integer(int32)                  :: n_global_entries = 26
      character(len=320), allocatable :: ascii_vector(:)
      integer(int32), allocatable     :: ascii_to_ints(:)
      integer(int32), allocatable     :: ascii_to_ints_indices(:)
      integer(int32), allocatable     :: ascii_to_reals_indices(:)
      real(real64), allocatable       :: ascii_to_reals(:)

      buffer_string = ''
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

      ascii_vector = extract_ascii_vector(buffer_string)
      do i = 1, size(ascii_vector)
      end do

      allocate (ascii_to_ints(26))
      allocate (ascii_to_ints_indices(9))
      ascii_to_ints = 0
      ascii_to_ints_indices = [7, 8, 9, 10, 11, 14, 16, 23, 24]
      do i = 1, size(ascii_to_ints_indices)
         j = ascii_to_ints_indices(i)
         read (ascii_vector(j), *) ascii_to_ints(j)
      end do

      allocate (ascii_to_reals(26))
      allocate (ascii_to_reals_indices(4))
      ascii_to_reals = 0.0
      ascii_to_reals_indices = [13, 17, 19, 20]
      do i = 1, size(ascii_to_reals_indices)
         j = ascii_to_reals_indices(i)
         read (ascii_vector(j), *) ascii_to_reals(j)
      end do

      this%parameter_delim = ascii_vector(1)
      this%record_delim = ascii_vector(2)
      this%product_id = ascii_vector(3)
      this%filename = ascii_vector(4)
      this%system_id = ascii_vector(5)
      this%preproc_version = ascii_vector(6)
      this%int_binary_bits = ascii_to_ints(7)
      this%max_power_single_float = ascii_to_ints(8)
      this%num_sig_digits = ascii_to_ints(9)
      this%max_power_double = ascii_to_ints(10)
      this%sig_digits_double = ascii_to_ints(11)
      this%product_id_recieving = ascii_vector(12)
      this%model_space_scale = ascii_to_reals(13)
      this%units_flag = ascii_to_ints(14)
      this%units_name = ascii_vector(15)
      this%max_num_line_weights = ascii_to_ints(16)
      this%max_line_weight = ascii_to_reals(17)
      this%date_time_generated = ascii_vector(18)
      this%min_resolution = ascii_to_reals(19)
      this%max_coord_value = ascii_to_reals(20)
      this%author = ascii_vector(21)
      this%organization = ascii_vector(22)
      this%version_flag = ascii_to_ints(23)
      this%drafting_flag = ascii_to_ints(24)
      this%date_time_modified = ascii_vector(25)
      this%mil_spec = ascii_vector(26)

   end subroutine read

end module Type_Global
