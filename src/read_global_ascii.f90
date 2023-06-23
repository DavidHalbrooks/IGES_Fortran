module read_global_ascii
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: extract_ascii_vector

contains

   function extract_ascii_vector(buffer_string) result(ascii_vector)
      character(len=800), intent(in) :: buffer_string
      character(len=800)             :: buffer_chars
      character(len=1), allocatable :: char_vector(:)
      character(len=320), allocatable :: ascii_vector(:)
      integer(int32), allocatable :: del_index_vector(:)
      integer(int32), allocatable :: start_slice(:)
      integer(int32), allocatable :: end_slice(:)
      integer(int32)   :: buffer_length = 0
      integer(int32)   :: size_indices_vector = 0
      integer(int32)   :: index_pos = 0
      character(len=1) :: delim_char = ''
      character(len=80) :: read_fmt = ''
      character(len=1) :: char_fmt = ''
      character(len=8) :: char_fmt_num = ''
      integer(int32)   :: num_global_entries = 0
      integer(int32)   :: i, j

      delim_char = ','
      buffer_chars = buffer_string
      buffer_length = len(trim(buffer_chars))

      allocate (char_vector(buffer_length), source='')

      buffer_chars = trim(buffer_chars)
      do i = 1, len(trim(buffer_chars))
         write (char_vector(i), '(a)') buffer_chars(i:i)
      end do

      ! Dumb count of delimiters in the char_vector
      do i = 1, size(char_vector)
         if (char_vector(i) .eq. delim_char) then
            size_indices_vector = (size_indices_vector + 1)
         end if
      end do

      allocate (del_index_vector(26), source=0)
      ! Position of the delimiter indices
      j = 1
      do i = 1, size(char_vector)
         if (char_vector(i) .eq. delim_char) then
            del_index_vector(j) = i
            j = (j + 1)
         end if
      end do

      num_global_entries = 26
      allocate (start_slice(num_global_entries))
      allocate (end_slice(num_global_entries))
      start_slice = 0
      end_slice = 0

      ! Calculate field slice information
      do i = 2, (num_global_entries)
         if (i .eq. 2) then
            start_slice(i - 1) = 1
            end_slice(i - 1) = (del_index_vector(i) - 1)
            ! ^^^^^ Start case to obtain Holerith paramter delimeter
         else if (i .eq. 3) then
            start_slice(i - 1) = (del_index_vector(i - 1) + 1)
            end_slice(i - 1) = (del_index_vector(i) - 1)
            ! ^^^^^ Special case to obtain Holerith record delimeter
         else if (i .eq. 4) then
            start_slice(i - 1) = del_index_vector(i)
            end_slice(i - 1) = del_index_vector(i) - 1
            ! ^^^^^ Special case after the two mandated Holerith constants
         else if (del_index_vector(i) .eq. del_index_vector(i - 1)) then
            start_slice(i - 1) = (del_index_vector(i - 1))
            end_slice(i - 1) = (del_index_vector(i))
            ! ^^^^^ Special Case of two delimiters with no entry
         else if (i .eq. size(del_index_vector)) then
            start_slice(i - 1) = (del_index_vector(i - 1) + 1)
            end_slice(i - 1) = (size(char_vector) - 1)
            ! ^^^^^ End case
         else if (i .gt. 1) then
            start_slice(i - 1) = (del_index_vector(i - 1) + 1)
            end_slice(i - 1) = (del_index_vector(i) - 1)
            ! ^^^^^ Normal operating case
         end if
      end do

      ! Capture the special case where no entry 26 exists
      if (del_index_vector(26) .eq. 0) then
         start_slice(i - 1) = (size(char_vector))
         end_slice(i - 1) = (size(char_vector) - 1)
      end if

      allocate (ascii_vector(size(del_index_vector)))
      ascii_vector = ''

      do i = 1, num_global_entries
         !print *, i, char_vector(start_slice(i):end_slice(i))
         write (ascii_vector(i), *) char_vector(start_slice(i):end_slice(i))
      end do

   end function extract_ascii_vector

end module read_global_ascii
