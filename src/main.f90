program main

   !use iso_fortran_env, only: int32, real32
   !use mod_iges_classes
   use geometry

   implicit none

   !class(Iges), allocatable      :: obj
   !type(Iges)      :: f1
   !type(Records)   :: r1
   type(Model)    :: E
   type(Entity)    :: E2

   print *
   write (*, *) '********** Program main_Model starting ************** '
   print *
   !call E%sub_calls()
   call E2%sub_calls()

end program main
