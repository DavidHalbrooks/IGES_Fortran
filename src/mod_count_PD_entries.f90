module mod_count_PD_entries

  implicit none

contains

  function count_PD_entries(buffer) result(n_PD_entries)
    character(len=4000), intent(in) :: buffer
    integer            :: n_PD_entries
    integer            :: entry_loc
    character          :: entry = ''
    integer            :: i

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

end module mod_count_PD_entries
