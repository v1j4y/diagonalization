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
    C=1.0d-9                                                         ! nesbet.irp.f:  16
    CA=1.0d0                                                         ! nesbet.irp.f:  17
    F=0.5d0                                                          ! nesbet.irp.f:  18
    V=0.0d0                                                          ! nesbet.irp.f:  20
    H=0.0d0                                                          ! nesbet.irp.f:  21
    Cmax=10000.0d0                                                   ! nesbet.irp.f:  22
    Cmax2=C                                                          ! nesbet.irp.f:  23
    V(7)=2.0d0                                                       ! nesbet.irp.f:  25
    E=+0.0d0                                                         ! nesbet.irp.f:  26
    niter=1                                                          ! nesbet.irp.f:  27
    D=0.0d0                                                          ! nesbet.irp.f:  28
    sigma=0.0d0                                                      ! nesbet.irp.f:  29
    m=0                                                              ! nesbet.irp.f:  30
    write(6,*)'NESBET:'                                              ! nesbet.irp.f:  32
    do k=1,30                                                        ! nesbet.irp.f:  34
        read(33,*)dmat,i,j                                           ! nesbet.irp.f:  35
        H(i,j)=dmat                                                  ! nesbet.irp.f:  36
    enddo                                                            ! nesbet.irp.f:  37
    do k=1,84                                                        ! nesbet.irp.f:  39
        read(44,*)i,j,dmat                                           ! nesbet.irp.f:  40
        H(i,j)=dmat                                                  ! nesbet.irp.f:  41
    enddo                                                            ! nesbet.irp.f:  42
    do i=10+1,30                                                     ! nesbet.irp.f:  44
        H(i,i-10)=-1                                                 ! nesbet.irp.f:  45
        H(i-10,i)=-1                                                 ! nesbet.irp.f:  46
    enddo                                                            ! nesbet.irp.f:  47
    write(6,*)'read H:'                                              ! nesbet.irp.f:  49
    do i=1,rank                                                      ! nesbet.irp.f:  51
        do j=1,rank                                                  ! nesbet.irp.f:  52
            write(6,12)H(i,j)                                        ! nesbet.irp.f:  53
        enddo                                                        ! nesbet.irp.f:  54
        write(6,*)                                                   ! nesbet.irp.f:  55
    enddo                                                            ! nesbet.irp.f:  56
    do while (abs(Cmax).gt.abs(C))                                   ! nesbet.irp.f:  58
        do while (m.lt.rank  .or.  abs(DeltaC).gt.abs(CA))           ! nesbet.irp.f:  60
        m=m+(1)                                                      ! nesbet.irp.f:  62
        write(6,*)'m=',m                                             ! nesbet.irp.f:  64
        do i=1,rank                                                  ! nesbet.irp.f:  69
            D=D+((V(i)*V(i)))                                        ! nesbet.irp.f:  70
        enddo                                                        ! nesbet.irp.f:  71
        write(6,*)'D=',D                                             ! nesbet.irp.f:  73
        do i=1,rank                                                  ! nesbet.irp.f:  77
            sigma=sigma+(H(m,i)*V(i))                                ! nesbet.irp.f:  78
        enddo                                                        ! nesbet.irp.f:  79
        sigma=sigma-(E*V(m))                                         ! nesbet.irp.f:  80
        write(6,*)'sigma=',sigma,(E-H(m,m))                          ! nesbet.irp.f:  82
        DeltaC = sigma/(E-H(m,m))                                    ! nesbet.irp.f:  85
        write(6,*)'DeltaC=',DeltaC                                   ! nesbet.irp.f:  87
        DeltaD = (2*V(m)+DeltaC)*DeltaC                              ! nesbet.irp.f:  90
        write(6,*)'DeltaD=',DeltaD                                   ! nesbet.irp.f:  92
        if((D+DeltaD).ne.0)then                                      ! nesbet.irp.f:  95
        DeltaE = sigma*DeltaC/(D+DeltaD)                             ! nesbet.irp.f:  96
        endif                                                        ! nesbet.irp.f:  97
        write(6,*)'DeltaE=',DeltaE                                   ! nesbet.irp.f:  99
        if(abs(Cmax2).lt.abs(DeltaC))then                            ! nesbet.irp.f: 100
            Cmax2=DeltaC                                             ! nesbet.irp.f: 101
        endif                                                        ! nesbet.irp.f: 102
        write(6,*)'Cmax2=',Cmax2                                     ! nesbet.irp.f: 103
        E=E+(DeltaE)                                                 ! nesbet.irp.f: 108
        V(m)=V(m)+(DeltaC)                                           ! nesbet.irp.f: 109
        D=0.0d0                                                      ! nesbet.irp.f: 111
        sigma=0.0d0                                                  ! nesbet.irp.f: 112
        DeltaC=0.0d0                                                 ! nesbet.irp.f: 113
        DeltaD=0.0d0                                                 ! nesbet.irp.f: 114
        DeltaE=0.0d0                                                 ! nesbet.irp.f: 115
        enddo                                                        ! nesbet.irp.f: 117
        m=0                                                          ! nesbet.irp.f: 119
        Cmax=Cmax2                                                   ! nesbet.irp.f: 120
        CA=F*Cmax                                                    ! nesbet.irp.f: 121
        Cmax2=C                                                      ! nesbet.irp.f: 122
        write(6,*)'iter',niter,Cmax                                  ! nesbet.irp.f: 123
        niter=niter+(1)                                              ! nesbet.irp.f: 124
    enddo                                                            ! nesbet.irp.f: 126
    write(6,*)'Eigenvalue',E+DeltaE                                  ! nesbet.irp.f: 128
    write(6,*)'Eigenvectors:'                                        ! nesbet.irp.f: 129
    do i=1,rank                                                      ! nesbet.irp.f: 130
        write(6,*)V(i)                                               ! nesbet.irp.f: 131
    enddo                                                            ! nesbet.irp.f: 132
   12   format((F8.2,'  '),$)                                        ! nesbet.irp.f: 134
end                                                                  ! nesbet.irp.f: 135
