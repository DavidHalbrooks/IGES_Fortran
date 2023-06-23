program main_count_PD_entries_test

  use mod_count_PD_entries
  implicit none

  character(len=800) :: buffer
  integer            :: n_PD_entries = 0

  buffer = '1, 2, 3, 4, 5, 6, 7, 8.0, 9.0, 10.0, 11.0, 12.0'

  n_PD_entries = count_PD_Entries(buffer)

  print *
  write(*,*) 'n_PD_entries = ', n_PD_entries


end program main_count_PD_entries_test
