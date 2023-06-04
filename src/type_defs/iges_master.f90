module iges_master
   use iso_fortran_env, only: int32, int64, real32, real64

   ! Type-bound procedures
   use type_tail, only: t_tail
   use type_start, only: t_start
   use type_global, only: t_global
   use type_metadata2, only: t_metadata2
   use type_directory, only: t_directory
   !use Type_Dir_Meta, only: Dir_meta
   !use Type_Parametric, only: Parametric

   ! External functions and subroutines (update to type-bound-procedures)
   use open_close, only: open_file, close_file
   use filename, only: set_filename
   use num_records, only: get_num_records
   use record_pos_calcs, only: position_start_calcs
   implicit none

   private
   public :: Iges_Model

   type, public :: Iges_Model
      character(len=512)              :: filename
      integer(int32)                  :: fileunit
      type(t_tail)                    :: tail_data
      type(t_start)                   :: start_data
      type(t_Global)                  :: metadata_CAD
      type(t_metadata2)               :: metadata_Model
      type(t_directory), allocatable  :: dir_entries(:)
      !type(Parametric), allocatable :: Parametric_data(:)

   contains
      procedure, public  :: init_metadata => init
      procedure, public  :: get_iges_filename => get_filename
      procedure, public  :: open_file => open_iges_file
      procedure, public  :: get_total_records_num => get_total_records
      procedure, public  :: read_tail_data => read_tail
      procedure, public  :: read_header_data => read_header
      procedure, public  :: calc_record_index_pos => calc_indices
      procedure, public  :: read_global_data => read_global
      procedure, public  :: scan_directory_data => scan_directory
      procedure, public  :: alloc_directory_data => alloc_directory
      procedure, public  :: read_directory_data => read_directory
      !procedure, public  :: print_metadata2_info => print_metadata2
      !procedure, public  :: alloc_parametric_data => alloc_parametric
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

   subroutine get_total_records(this)
      class(Iges_Model), intent(inout) :: this
      call get_num_records(this%filename, this%Metadata_Model%num_records)
      write (*, '(a)') '*** get_num_records completed'
   end subroutine get_total_records

   subroutine read_tail(this)
      class(Iges_Model), intent(inout) :: this
      call this%Tail_data%init_tail_data
      call this%Tail_data%read_tail_data(this%fileunit, this%Metadata_Model%num_records)
      this%Metadata_Model%num_S_records = this%Tail_data%num_S
      this%Metadata_Model%num_G_records = this%Tail_data%num_G
      this%Metadata_Model%num_D_records = this%Tail_data%num_D
      this%Metadata_Model%num_P_records = this%Tail_data%num_P
      this%Metadata_Model%num_T_records = this%Tail_data%num_T
      ! NOTE: THE LINE BELOW NEEDS TO BE MOVED
      this%Metadata_Model%Directory_Metadata%num_D_records = this%Tail_data%num_D
      write (*, '(a)') '*** read_tail completed'
   end subroutine read_tail

   subroutine read_header(this)
      class(Iges_Model), intent(inout) :: this
      call this%Start_data%read_S_data(this%fileunit, this%Tail_data%num_S)
      write (*, '(a)') '*** read_start completed'
   end subroutine read_header

   subroutine calc_indices(this)
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
      this%Metadata_Model%Directory_metadata%D_record_start_index = record_pos_vector(2)
      this%Metadata_Model%P_record_start = record_pos_vector(3)
      this%Metadata_Model%T_record_start = record_pos_vector(4)
      write (*, '(a)') '*** calc_read_pos completed'
   end subroutine calc_indices

   subroutine read_global(this)
      class(Iges_Model), intent(inout) :: this
      call this%Metadata_CAD%read_G_data(this%fileunit, &
                                         this%Metadata_Model%num_G_records, &
                                         this%Metadata_Model%G_record_start)
      write (*, '(a)') '*** read_global completed'
   end subroutine read_global

   subroutine scan_directory(this)
      class(Iges_Model), intent(inout) :: this
      this%Metadata_Model%fileunit = this%fileunit
      this%Metadata_Model%Directory_metadata%fileunit = this%fileunit
      call this%Metadata_Model%scan_directory_data
      write (*, '(a)') '*** scan_directory completed'
   end subroutine scan_directory

   subroutine print_metadata2(this)
      class(Iges_Model), intent(inout) :: this
      !print *, this%Metadata_Model
      !print *, this%Metadata_Model%T126_index_vector
      write (*, '(a)') '*** print_metadata2 completed'
   end subroutine print_metadata2

   subroutine alloc_directory(this)
      class(Iges_Model), intent(inout) :: this
!      allocate (this%Dir_Entries(this%Metadata_Model%Directory_Metadata%num_D_records))
!      print *, 'size Dir_Entries:', size(this%Dir_Entries)
      write (*, '(a)') '*** alloc_directory completed'
   end subroutine alloc_directory

   subroutine read_directory(this)
      class(Iges_Model), intent(inout) :: this
      ! call this%Dir_Entries%read_entries(this%fileunit, &
      !                                    this%Metadata_Model%num_D_records, &
      !                                    this%Metadata_Model%D_record_start)
      ! call this%Dir_Entries%read_entries(this%fileunit, &
      !                                    this%Metadata_Model%D_record_start)
      write (*, '(a)') '*** read_directory completed'
      print *, this%Dir_Entries
   end subroutine read_directory

   ! subroutine alloc_paramatric(this)
   !    class(Iges_Model), intent(inout) :: this
   !    allocate (this%Parametric_Data(this%Metadata_Model%num_P_records))
   !    print *, 'size Parametric_Data:', size(this%Parametric_Data)
   !    write (*, '(a)') '*** alloc_parameter_data completed'
   ! end subroutine alloc_parametric

   subroutine close_iges_file(this)
      class(Iges_Model), intent(inout) :: this
      call close_file(this%fileunit)
      write (*, '(a)') '*** close_iges_file completed'
   end subroutine close_iges_file

end module iges_master
