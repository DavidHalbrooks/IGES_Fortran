module Iges_master
   use iso_fortran_env, only: int32, int64, real32, real64

   use filename, only: set_filename
   use open_close, only: open_file, close_file
   use num_records, only: get_num_records
   use Type_Tail, only: Tail
   implicit none

   private
   public :: Iges_Model

   type, public :: Iges_Model
      character(len=512)       :: filename
      integer(int32)           :: fileunit
      integer(int32)           :: num_records
      integer(int32)           :: num_S_records
      integer(int32)           :: num_G_records
      integer(int32)           :: num_P_records
      integer(int32)           :: num_T_records
      integer(int32)           :: num_T126_Entities
      integer(int32)           :: num_T128_Entities
      integer(int32)           :: num_T141_Entities
      integer(int32)           :: num_T314_Entities
      integer(int32)           :: num_unidentified_Entities
      type(Tail)               :: Tail_data

   contains
      procedure, public  :: init_data => init
      procedure, public  :: get_Iges_filename => get_filename
      procedure, public  :: open_file => open_iges_file
      procedure, public  :: get_total_num_records => get_total
      procedure, public  :: read_tail_data => read_tail
      procedure, public  :: close_file => close_iges_file
   end type Iges_Model

contains

   subroutine init(this)
      class(Iges_Model), intent(inout) :: this
      this%filename = 'test_pass'
      this%num_records = 0
      this%num_S_records = 0
      this%num_G_records = 0
      this%num_P_records = 0
      this%num_T_records = 0
      this%num_T126_Entities = 0
      this%num_T128_Entities = 0
      this%num_T141_Entities = 0
      this%num_T314_Entities = 0
      this%num_unidentified_Entities = 0
      write (*, '(a)') '*** init completed'
   end subroutine init

   subroutine get_filename(this)
      class(Iges_Model), intent(inout) :: this
      this%filename = set_filename()
      write (*, '(a)') '*** get_filename completed'
   end subroutine get_filename

   subroutine open_iges_file(this)
      class(Iges_Model), intent(inout) :: this
      call open_file(this%filename, this%fileunit)
      write (*, '(a)') '*** open_iges_file completed'
   end subroutine open_iges_file

   subroutine get_total(this)
      class(Iges_Model), intent(inout) :: this
      call get_num_records(this%filename, this%num_records)
      write (*, '(a)') '*** get_num_records completed'
   end subroutine get_total

   subroutine read_tail(this)
      class(Iges_Model), intent(inout) :: this
      call this%Tail_data%initialize_T
      call this%Tail_data%read_T_data(this%fileunit, this%num_records)
      write (*, '(a)') '*** read_tail completed'
   end subroutine read_tail

   subroutine close_iges_file(this)
      class(Iges_Model), intent(inout) :: this
      call close_file(this%fileunit)
      write (*, '(a)') '*** close_iges_file completed'
   end subroutine close_iges_file

end module Iges_master
