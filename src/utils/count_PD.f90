module count_PD

   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: count_PD_entries

contains

   function count_PD_entries(buffer) result(n_PD_entries)
      character(len=4000), intent(in) :: buffer
      integer(int32)     :: n_PD_entries
      integer(int32)     :: entry_loc
      character          :: entry = ''
      integer(int32)     :: i

      n_PD_entries = 0
      do i = 1, len(trim(buffer))
         entry = buffer(i:i)
         if (entry == ',') then
            entry_loc = i
            n_PD_entries = n_PD_entries + 1
         else if (entry == ';') then
            !write(*,*) 'End of record reached'
         end if
      end do
      n_PD_entries = n_PD_entries + 1
   end function count_PD_entries

end module count_PD
