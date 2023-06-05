module count_pd
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: count_pd_entries

contains

   function count_pd_entries(buffer) result(n_PD_entries)
      character(len=4000), intent(in) :: buffer
      integer(int32)     :: n_PD_entries
      integer(int32)     :: entry_loc
      character          :: entry = ''
      integer(int32)     :: i

      n_PD_entries = 0
      num_entries_loop: do i = 1, len(trim(buffer))
         entry = buffer(i:i)
         if (entry == ',') then
            entry_loc = i
            n_PD_entries = n_PD_entries + 1
         else if (entry == ';') then
            !write(*,*) 'End of record reached'
            exit num_entries_loop
         end if
      end do num_entries_loop
      n_PD_entries = n_PD_entries + 1
   end function count_pd_entries

end module count_pd
