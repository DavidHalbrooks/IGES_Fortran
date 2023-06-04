module type_dir_meta
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private

   type, public :: t_dir_meta
      integer(int32)     :: fileunit
      integer(int32)     :: D_record_start_index
      integer(int32)     :: num_D_records
      integer(int32)     :: num_T126 = 0
      integer(int32)     :: num_T128 = 0
      integer(int32)     :: num_T141 = 0
      integer(int32)     :: num_T143 = 0
      integer(int32)     :: num_T314 = 0
      integer(int32)     :: num_unidentified_Entities = 0
      integer(int32), allocatable, dimension(:) :: T314_index_vector
      integer(int32), allocatable, dimension(:) :: T143_index_vector
      integer(int32), allocatable, dimension(:) :: T141_index_vector
      integer(int32), allocatable, dimension(:) :: T128_index_vector
      integer(int32), allocatable, dimension(:) :: T126_index_vector
      integer(int32), allocatable, dimension(:) :: Undefined_index_vector
   contains
      procedure :: get_metadata => get_meta
      procedure :: print_dir_metadata => print_meta
   end type t_dir_meta

contains

   subroutine get_meta(this)
      class(t_dir_meta), intent(inout) :: this
      integer(int32) :: D_record_num
      integer(int32) :: stride
      integer(int32) :: type_int
      integer(int32) :: i
      integer(int32) :: j_1, j_2, j_3, j_4, j_5, j_6
      character(len=6) :: read_format

      read_format = '(i8)'
      type_int = 0
      stride = 2
      D_record_num = this%D_record_start_index

      ! Scan directory data to obtain allocation information
      do i = 1, (this%num_D_records), stride
         read (this%fileunit, rec=D_record_num, fmt=read_format) type_int
         select case (type_int)
         case (314)
            this%num_T314 = this%num_T314 + 1
         case (141)
            this%num_T141 = this%num_T141 + 1
         case (143)
            this%num_T143 = this%num_T143 + 1
         case (126)
            this%num_T126 = this%num_T126 + 1
         case (128)
            this%num_T128 = this%num_T128 + 1
         case default
            this%num_unidentified_entities = this%num_unidentified_entities + 1
            write (*, '(a)') '*** Unidentified Directory Entries Scanned ***'
         end select
         D_record_num = D_record_num + stride
      end do

      ! Allocate the directory index vectors
      allocate (this%T314_index_vector(this%num_T314))
      allocate (this%T141_index_vector(this%num_T141))
      allocate (this%T143_index_vector(this%num_T143))
      allocate (this%T128_index_vector(this%num_T128))
      allocate (this%T126_index_vector(this%num_T126))
      allocate (this%Undefined_index_vector(this%num_unidentified_entities))

      ! Initialize the directory index vectors
      this%T314_index_vector = 0
      this%T141_index_vector = 0
      this%T143_index_vector = 0
      this%T126_index_vector = 0
      this%T128_index_vector = 0
      this%Undefined_index_vector = 0

      ! Initialize index counters
      j_1 = 1; j_2 = 1; j_3 = 1; j_4 = 1; j_5 = 1; j_6 = 1

      ! Reset Directory start index for second scan
      D_record_num = this%D_record_start_index

      ! Read in the directory index vectors
      do i = 1, (this%num_D_records), stride
         read (this%fileunit, rec=D_record_num, fmt=read_format) type_int
         select case (type_int)
         case (314)
            this%T314_index_vector(j_1) = i + (this%D_record_start_index - 1)
            j_1 = j_1 + 1
         case (143)
            this%T143_index_vector(j_2) = i + (this%D_record_start_index - 1)
            j_2 = j_2 + 1
         case (141)
            this%T141_index_vector(j_3) = i + (this%D_record_start_index - 1)
            j_3 = j_3 + 1
         case (128)
            this%T128_index_vector(j_4) = i + (this%D_record_start_index - 1)
            j_4 = j_4 + 1
         case (126)
            this%T126_index_vector(j_5) = i + (this%D_record_start_index - 1)
            j_5 = j_5 + 1
         case default
            this%Undefined_index_vector(j_6) = i + (this%D_record_start_index - 1)
            j_6 = j_6 + 1
         end select
         D_record_num = D_record_num + stride
      end do

      print *, this%T314_index_vector
      print *, this%T141_index_vector
      print *, this%T143_index_vector
      print *, this%T126_index_vector
      print *, this%T128_index_vector
      print *, this%Undefined_index_vector

   end subroutine get_meta

   subroutine print_meta(this)
      class(t_dir_meta), intent(inout) :: this
      print *, 'num_D_records:', this%num_D_records
      print *, 'D_records_start_index:', this%D_record_start_index
      print *, 'fileunit:', this%fileunit
   end subroutine print_meta

end module type_dir_meta
