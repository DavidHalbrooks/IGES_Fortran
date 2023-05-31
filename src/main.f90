program main
   use iso_fortran_env, only: int32, int64, real32, real64
   use Iges_master, only: Iges_Model
   implicit none

   type(Iges_Model) :: Iges

   ! Call encapsulated data procedures
   print *
   call Iges%init_data
   call Iges%get_Iges_filename
   call Iges%open_file
   call Iges%get_total_num_records
   call Iges%read_tail_data
   call Iges%close_file

   print *
   Print *, 'Diagnostics'
   print *, trim(Iges%filename)
   print *, Iges%num_records
   print *, Iges%fileunit
   write (*, *) Iges%Tail_data%num_S
   write (*, *) Iges%Tail_data%num_G
   write (*, *) Iges%Tail_data%num_D
   write (*, *) Iges%Tail_data%num_P
   write (*, *) Iges%Tail_data%num_T

   !print *, Iges
end program main
