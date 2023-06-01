module record_pos_calcs
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: position_start_calcs

contains

   function position_start_calcs(num_vector) result(record_pos_vector)
      integer(int32), dimension(4) :: num_vector
      integer(int32), dimension(4) :: record_pos_vector
      integer(int32)     :: prev_sum
      integer(int32)     :: i

      record_pos_vector = 0
      do i = 1, size(num_vector)
         record_pos_vector(i) = sum(num_vector(1:i))
      end do
      record_pos_vector = record_pos_vector + 1
   end function position_start_calcs

end module record_pos_calcs
