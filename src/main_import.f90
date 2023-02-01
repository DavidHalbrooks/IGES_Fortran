program main_import

  !use iso_fortran_env, only: int32, real32
  !use mod_iges_classes
  use mod_DE_import

  implicit none

  !class(Iges), allocatable      :: obj
  !type(Iges)      :: f1
  !type(Records)   :: r1
  type(Entity)    :: E

  call E%sub_calls()

  !obj = Iges(filename, fileunit)
  !call f1 % init()
!  call f1 % print_debug()
  !call f1 % close_file()

  !call r1 % init()
  !call r1 % get_records_num()
!  call r1 % print_debug()

end program main_import
