program main_relational_data

  use mod_relational_data
  implicit none

  !type(Iges)         :: this
  type(S)         :: this_S
  type(G)         :: this_G
  type(DE)         :: this_DE
  character(len=80)  :: filename
!  integer            :: fileunit


  ! Set Variables
  filename        = 'hollowvase.igs'  ! Iges filename to open

  print *
  print*, 'Program start ...'
  print *

  !df%filename = filename
  !call this%get_filename()
  print *

!  call this%read_sect_info

  call this_S%sub_calls
  call this_G%sub_calls
  call this_DE%sub_calls
!  call this_S%init
!  call this_S%read

!  call this_G%alloc
!  call this_G%init
!  call this_G%read

end program main_relational_data
