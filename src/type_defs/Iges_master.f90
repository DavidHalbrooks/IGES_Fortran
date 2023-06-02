module Iges_master
   use iso_fortran_env, only: int32, int64, real32, real64

   use filename, only: set_filename
   use open_close, only: open_file, close_file
   use num_records, only: get_num_records
   use Type_Tail, only: Tail
   use Type_Start, only: Start
   use record_pos_calcs, only: position_start_calcs
   use Type_Global, only: Global
   use Type_Metadata2, only: Metadata2
   use Type_Directory, only: Directory
   implicit none

   private
   public :: Iges_Model

   type, public :: Iges_Model
      character(len=512)           :: filename
      integer(int32)               :: fileunit
      type(Tail)                   :: Tail_data
      type(Start)                  :: Start_data
      type(Global)                 :: Metadata_CAD
      type(Metadata2)              :: Metadata_Model
      type(Directory), allocatable :: Dir_Entries
      ! type(Pametric)           :: Parametric_data

   contains
      procedure, public  :: init_metadata => init
      procedure, public  :: get_Iges_filename => get_filename
      procedure, public  :: open_file => open_iges_file
      procedure, public  :: get_total_num_records => get_total
      procedure, public  :: read_tail_data => read_tail
      procedure, public  :: read_header_data => read_header
      procedure, public  :: calc_pos => calc_read_pos
      procedure, public  :: read_global_data => read_global
      !procedure, public  :: alloc_directory_data => alloc_directory
      ! procedure, public  :: init_directory_data => init_directory
      procedure, public  :: read_directory_data => read_directory
      procedure, public  :: close_file => close_iges_file
   end type Iges_Model

contains

   subroutine init(this)
      class(Iges_Model), intent(inout) :: this
      this%filename = 'test_pass'
      this%Metadata_Model%num_records = 0
      this%Metadata_Model%num_S_records = 0
      this%Metadata_Model%num_G_records = 0
      this%Metadata_Model%num_P_records = 0
      this%Metadata_Model%num_T_records = 0
      this%Metadata_Model%num_T126_Entities = 0
      this%Metadata_Model%num_T128_Entities = 0
      this%Metadata_Model%num_T141_Entities = 0
      this%Metadata_Model%num_T314_Entities = 0
      this%Metadata_Model%num_unidentified_Entities = 0
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
      call get_num_records(this%filename, this%Metadata_Model%num_records)
      write (*, '(a)') '*** get_num_records completed'
   end subroutine get_total

   subroutine read_tail(this)
      class(Iges_Model), intent(inout) :: this
      call this%Tail_data%initialize_T
      call this%Tail_data%read_T_data(this%fileunit, this%Metadata_Model%num_records)
      this%Metadata_Model%num_S_records = this%Tail_data%num_S
      this%Metadata_Model%num_G_records = this%Tail_data%num_G
      this%Metadata_Model%num_D_records = this%Tail_data%num_D
      this%Metadata_Model%num_P_records = this%Tail_data%num_P
      this%Metadata_Model%num_T_records = this%Tail_data%num_T
      write (*, '(a)') '*** read_tail completed'
   end subroutine read_tail

   subroutine read_header(this)
      class(Iges_Model), intent(inout) :: this
      call this%Start_data%read_S_data(this%fileunit, this%Tail_data%num_S)
      write (*, '(a)') '*** read_start completed'
   end subroutine read_header

   subroutine calc_read_pos(this)
      class(Iges_Model), intent(inout) :: this
      integer(int32), dimension(4) :: num_vector
      integer(int32), dimension(4) :: record_pos_vector
      num_vector(1) = this%Tail_data%num_S
      num_vector(2) = this%Tail_data%num_G
      num_vector(3) = this%Tail_data%num_D
      num_vector(4) = this%Tail_data%num_P
      ! Call the position_start_calcs function
      record_pos_vector = position_start_calcs(num_vector)
      this%Metadata_Model%G_record_start = record_pos_vector(1)
      this%Metadata_Model%D_record_start = record_pos_vector(2)
      this%Metadata_Model%P_record_start = record_pos_vector(3)
      this%Metadata_Model%T_record_start = record_pos_vector(4)
      write (*, '(a)') '*** calc_read_pos completed'
   end subroutine calc_read_pos

   subroutine read_global(this)
      class(Iges_Model), intent(inout) :: this
      call this%Metadata_CAD%read_G_data(this%fileunit, &
                                         this%Metadata_Model%num_G_records, &
                                         this%Metadata_Model%G_record_start)
      write (*, '(a)') '*** read_global completed'
      print *, this%Metadata_Model
   end subroutine read_global

   subroutine read_directory(this)
      class(Iges_Model), intent(inout) :: this
      ! call this%Dir_Entries%read_entries(this%fileunit, &
      !                                    this%Metadata_Model%num_D_records, &
      !                                    this%Metadata_Model%D_record_start)
      call this%Dir_Entries%read_entries(this%fileunit, &
                                         this%Metadata_Model%D_record_start)
      write (*, '(a)') '*** read_directory completed'
      print *, this%Dir_Entries
   end subroutine read_directory

   subroutine close_iges_file(this)
      class(Iges_Model), intent(inout) :: this
      call close_file(this%fileunit)
      write (*, '(a)') '*** close_iges_file completed'
   end subroutine close_iges_file

end module Iges_master
