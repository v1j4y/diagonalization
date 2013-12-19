! -*- F90 -*-
!
!-----------------------------------------------!
! This file was generated with the irpf90 tool. !
!                                               !
!           DO NOT MODIFY IT BY HAND            !
!-----------------------------------------------!

program irp_program                                                  ! nesbet:   0
 call nesbet                                                         ! nesbet.irp.f:   0
 call irp_finalize_1637695895()                                      ! nesbet.irp.f:   0
end program                                                          ! nesbet.irp.f:   0
subroutine nesbet                                                    ! nesbet.irp.f:   1
    implicit none                                                    ! nesbet.irp.f:   2
  character*(6) :: irp_here = 'nesbet'                               ! nesbet.irp.f:   1
    integer :: i,j,k,m,rank,niter                                    ! nesbet.irp.f:   4
    real(8) :: CA,C,F,E,Cmax,DeltaC,D,DeltaD,DeltaE,sigma,dmat,Cmax2 ! nesbet.irp.f:   5
    real(8),allocatable :: H(:,:),V(:)                               ! nesbet.irp.f:   6
    rank=30                                                          ! nesbet.irp.f:   8
    allocate(H(rank,rank))                                           ! nesbet.irp.f:  10
    allocate(V(rank))                                                ! nesbet.irp.f:  11
    open(unit=33,file='FIL33')                                       ! nesbet.irp.f:  13
    open(unit=44,file='FIL44')                                       ! nesbet.irp.f:  14
    open(unit=1,file='converg.dat')                                  ! nesbet.irp.f:  15
    C=7.6d-2                                                         ! nesbet.irp.f:  17
    CA=1.0d0                                                         ! nesbet.irp.f:  18
    F=0.5d0                                                          ! nesbet.irp.f:  19
    V=0.0d0                                                          ! nesbet.irp.f:  21
    H=0.0d0                                                          ! nesbet.irp.f:  22
    Cmax=10000.0d0                                                   ! nesbet.irp.f:  23
    Cmax2=C                                                          ! nesbet.irp.f:  24
    V(11)=1.0d0                                                      ! nesbet.irp.f:  30
    E=+0.4d0                                                         ! nesbet.irp.f:  32
    niter=1                                                          ! nesbet.irp.f:  33
    D=0.0d0                                                          ! nesbet.irp.f:  34
    sigma=0.0d0                                                      ! nesbet.irp.f:  35
    m=0                                                              ! nesbet.irp.f:  36
    write(6,*)'NESBET:'                                              ! nesbet.irp.f:  38
    write(6,*)'starting with:'                                       ! nesbet.irp.f:  39
    write(6,*)'E=',E,'m=7'                                           ! nesbet.irp.f:  40
    do k=1,30                                                        ! nesbet.irp.f:  42
        read(33,*)dmat,i,j                                           ! nesbet.irp.f:  43
        H(i,j)=dmat                                                  ! nesbet.irp.f:  44
    enddo                                                            ! nesbet.irp.f:  45
    do k=1,84                                                        ! nesbet.irp.f:  47
        read(44,*)i,j,dmat                                           ! nesbet.irp.f:  48
        H(i,j)=dmat                                                  ! nesbet.irp.f:  49
    enddo                                                            ! nesbet.irp.f:  50
    do i=10+1,30                                                     ! nesbet.irp.f:  52
        H(i,i-10)=-1                                                 ! nesbet.irp.f:  53
        H(i-10,i)=-1                                                 ! nesbet.irp.f:  54
    enddo                                                            ! nesbet.irp.f:  55
    write(6,*)'read H:'                                              ! nesbet.irp.f:  57
    do i=1,rank                                                      ! nesbet.irp.f:  59
        do j=1,rank                                                  ! nesbet.irp.f:  60
            write(6,12)H(i,j)                                        ! nesbet.irp.f:  61
        enddo                                                        ! nesbet.irp.f:  62
        write(6,*)                                                   ! nesbet.irp.f:  63
    enddo                                                            ! nesbet.irp.f:  64
    do while (abs(Cmax).gt.abs(C))                                   ! nesbet.irp.f:  66
        do while (m.lt.rank  .or.  abs(DeltaC).gt.abs(CA))           ! nesbet.irp.f:  68
        m=m+(1)                                                      ! nesbet.irp.f:  70
        do i=1,rank                                                  ! nesbet.irp.f:  75
            D=D+((V(i)*V(i)))                                        ! nesbet.irp.f:  76
        enddo                                                        ! nesbet.irp.f:  77
        do i=1,rank                                                  ! nesbet.irp.f:  82
            sigma=sigma+(H(m,i)*V(i))                                ! nesbet.irp.f:  83
        enddo                                                        ! nesbet.irp.f:  84
        sigma=sigma-(E*V(m))                                         ! nesbet.irp.f:  85
        DeltaC = sigma/(E-H(m,m))                                    ! nesbet.irp.f:  89
        DeltaD = (2.0d0*V(m)+DeltaC)*DeltaC                          ! nesbet.irp.f:  93
        if((D+DeltaD).ne.0.0d0)then                                  ! nesbet.irp.f:  97
        DeltaE = sigma*DeltaC/(D+DeltaD)                             ! nesbet.irp.f:  98
        endif                                                        ! nesbet.irp.f:  99
        if(abs(Cmax2).lt.abs(DeltaC))then                            ! nesbet.irp.f: 101
            Cmax2=DeltaC                                             ! nesbet.irp.f: 102
        endif                                                        ! nesbet.irp.f: 103
        E=E+(DeltaE)                                                 ! nesbet.irp.f: 107
        V(m)=V(m)+(DeltaC)                                           ! nesbet.irp.f: 108
        D=0.0d0                                                      ! nesbet.irp.f: 114
        sigma=0.0d0                                                  ! nesbet.irp.f: 115
        DeltaC=0.0d0                                                 ! nesbet.irp.f: 116
        DeltaD=0.0d0                                                 ! nesbet.irp.f: 117
        DeltaE=0.0d0                                                 ! nesbet.irp.f: 118
        enddo                                                        ! nesbet.irp.f: 120
        m=0                                                          ! nesbet.irp.f: 122
        Cmax=Cmax2                                                   ! nesbet.irp.f: 123
        CA=F*Cmax                                                    ! nesbet.irp.f: 124
        Cmax2=C                                                      ! nesbet.irp.f: 125
        write(6,*)'iter:',niter,'Cmax:',Cmax                         ! nesbet.irp.f: 126
        write(1,*)niter,Cmax                                         ! nesbet.irp.f: 127
        niter=niter+(1)                                              ! nesbet.irp.f: 128
    enddo                                                            ! nesbet.irp.f: 130
    sigma=0.0d0                                                      ! nesbet.irp.f: 132
    write(6,*)'converged in :',niter-1,'iterations'                  ! nesbet.irp.f: 133
    write(6,*)'Eigenvalue:',E                                        ! nesbet.irp.f: 134
    write(6,*)'Eigenvectors:'                                        ! nesbet.irp.f: 135
    do i=1,rank                                                      ! nesbet.irp.f: 136
        sigma=sigma+((V(i)*V(i)))                                    ! nesbet.irp.f: 137
    enddo                                                            ! nesbet.irp.f: 138
    do i=1,rank                                                      ! nesbet.irp.f: 140
        V(i)=V(i)/sqrt(sigma)                                        ! nesbet.irp.f: 141
        write(6,*)V(i)                                               ! nesbet.irp.f: 142
    enddo                                                            ! nesbet.irp.f: 143
    sigma=0.0d0                                                      ! nesbet.irp.f: 144
    do i=1,rank                                                      ! nesbet.irp.f: 145
        sigma=sigma+((V(i)*V(i)))                                    ! nesbet.irp.f: 146
    enddo                                                            ! nesbet.irp.f: 147
    write(6,*)'normalized?',sigma                                    ! nesbet.irp.f: 149
   12   format((F8.2,'  '),$)                                        ! nesbet.irp.f: 152
end                                                                  ! nesbet.irp.f: 153
