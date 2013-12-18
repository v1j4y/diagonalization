
module irp_stack_mod
  integer, parameter            :: STACKMAX=1000
  character*(128),allocatable   :: irp_stack(:,:)
  double precision,allocatable  :: irp_cpu(:,:)
  integer,allocatable           :: stack_index(:)
  logical                       :: alloc = .False.
  character*(128)               :: white = ''
end module

subroutine irp_enter(irp_where)
 use irp_stack_mod
 integer       :: ithread
 integer       :: nthread
 character*(*) :: irp_where

!$OMP CRITICAL
 ithread = 0
 nthread = 1

!$OMP END CRITICAL


end subroutine

subroutine irp_leave (irp_where)
 use irp_stack_mod
  character*(*) :: irp_where
  integer       :: ithread
  double precision :: cpu

!$OMP CRITICAL
  ithread = 0


!$OMP END CRITICAL
end subroutine

