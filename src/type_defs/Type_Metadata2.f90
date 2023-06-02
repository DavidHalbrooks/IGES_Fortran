module Type_Metadata2
   use iso_fortran_env, only: int32, int64, real32, real64
   implicit none

   private
   public :: Metadata2

   type Metadata2
      integer(int32)     :: num_records
      integer(int32)     :: num_S_records
      integer(int32)     :: num_G_records
      integer(int32)     :: num_D_records
      integer(int32)     :: num_P_records
      integer(int32)     :: num_T_records
      integer(int32)     :: num_T126_Entities
      integer(int32)     :: num_T128_Entities
      integer(int32)     :: num_T141_Entities
      integer(int32)     :: num_T314_Entities
      integer(int32)     :: num_unidentified_Entities
      integer(int32)     :: G_record_start = 0
      integer(int32)     :: D_record_start = 0
      integer(int32)     :: P_record_start = 0
      integer(int32)     :: T_record_start = 0
      integer(int32)     :: num_T314 = 0
      integer(int32)     :: num_143 = 0
      integer(int32)     :: num_T126 = 0
      integer(int32)     :: num_T28 = 0
   contains
      !procedure :: calc_record_start_pos => calc
   end type Metadata2

contains

end module Type_Metadata2
