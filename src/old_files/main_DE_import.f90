program main_DE_import

  !use iso_fortran_env, only: int32, real32
  !use mod_iges_classes
  use mod_DE_import

  implicit none

  !class(Iges), allocatable      :: obj
  !type(Iges)      :: f1
  !type(Records)   :: r1
  type(Entity)    :: E

  print *
  write(*,*) 'Program DE_import starting ************** '
  print *
  call E%sub_calls()


end program main_DE_import
