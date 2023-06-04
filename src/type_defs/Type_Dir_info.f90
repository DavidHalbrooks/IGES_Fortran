module Type_Dir_info
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: Dir_info

   type, public :: Dir_info
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
      procedure :: get_dir_indices => get_indices
   end type Dir_info

contains

   subroutine get_dir_indices(this, fileunit, num_DE_records, DE_record_start_index)
      class(Dir_info), intent(inout) :: this
      integer(int32), intent(in) :: fileunit
      integer(int32), intent(in) :: num_DE_records
      integer(int32), intent(in) :: DE_record_start_index
      integer(int32)  :: DE_record_num
      integer(int32), allocatable, dimension(:) :: initial_scan_vector
      integer(int32), allocatable, dimension(:) :: T
      integer(int32) :: stride
      integer(int32) :: type_int
      integer(int32) :: i
      integer(int32) :: j_1, j_2, j_3, j_4, j_5, j_6
      character(len=6) :: read_format

      ! Allocation is only needed for 1/2 of the directory
      allocate (initial_scan_vector(num_DE_records/2))

      ! These are the types that will be scanned in the directory
      allocate (T(5))
      T = [314, 141, 143, 126, 128]

      ! Note: This is the number of cases that will scanned for return
      allocate (num_types_vector(size(T) + 1))
      num_types_vector = 0

      read_format = '(i8)'
      type_int = 0
      stride = 2
      DE_record_num = DE_record_start_index

      ! Scan directory data to obtain allocation information
      do i = 1, (num_DE_records/2), stride
         read (fileunit, rec=DE_record_num, fmt=read_format) type_int
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
         DE_record_num = DE_record_num + stride
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
      DE_record_num = DE_record_start_index

      ! Read in the directory index vectors
      do i = 1, (num_DE_records/2), stride
         read (fileunit, rec=DE_record_num, fmt=read_format) type_int
         select case (type_int)
         case (314)
            this%T314_index_vector(j_1) = i + (DE_record_start_index - 1)
            j_1 = j_1 + 1
         case (143)
            this%T143_index_vector(j_2) = i + (DE_record_start_index - 1)
            j_2 = j_2 + 1
         case (141)
            this%T141_index_vector(j_3) = i + (DE_record_start_index - 1)
            j_3 = j_3 + 1
         case (128)
            this%T128_index_vector(j_4) = i + (DE_record_start_index - 1)
            j_4 = j_4 + 1
         case (126)
            this%T126_index_vector(j_5) = i + (DE_record_start_index - 1)
            j_5 = j_5 + 1
         case default
            this%Undefined_index_vector(j_6) = i + (DE_record_start_index - 1)
            j_6 = j_6 + 1
         end select
         DE_record_num = DE_record_num + stride
      end do

   end subroutine get_dir_indices

end module Type_Dir_info
