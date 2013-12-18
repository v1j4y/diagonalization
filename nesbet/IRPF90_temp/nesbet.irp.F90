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
    V(7)=1.0d0                                                       ! nesbet.irp.f:  25
    E=+0.0d0                                                         ! nesbet.irp.f:  26
    niter=1                                                          ! nesbet.irp.f:  27
    D=0.0d0                                                          ! nesbet.irp.f:  28
    sigma=0.0d0                                                      ! nesbet.irp.f:  29
    m=0                                                              ! nesbet.irp.f:  30
    write(6,*)'NESBET:'                                              ! nesbet.irp.f:  32
    write(6,*)'starting with:'                                       ! nesbet.irp.f:  33
    write(6,*)'E=',E,'m=',m                                          ! nesbet.irp.f:  34
    do k=1,30                                                        ! nesbet.irp.f:  36
        read(33,*)dmat,i,j                                           ! nesbet.irp.f:  37
        H(i,j)=dmat                                                  ! nesbet.irp.f:  38
    enddo                                                            ! nesbet.irp.f:  39
    do k=1,84                                                        ! nesbet.irp.f:  41
        read(44,*)i,j,dmat                                           ! nesbet.irp.f:  42
        H(i,j)=dmat                                                  ! nesbet.irp.f:  43
    enddo                                                            ! nesbet.irp.f:  44
    do i=10+1,30                                                     ! nesbet.irp.f:  46
        H(i,i-10)=-1                                                 ! nesbet.irp.f:  47
        H(i-10,i)=-1                                                 ! nesbet.irp.f:  48
    enddo                                                            ! nesbet.irp.f:  49
    write(6,*)'read H:'                                              ! nesbet.irp.f:  51
    do i=1,rank                                                      ! nesbet.irp.f:  53
        do j=1,rank                                                  ! nesbet.irp.f:  54
            write(6,12)H(i,j)                                        ! nesbet.irp.f:  55
        enddo                                                        ! nesbet.irp.f:  56
        write(6,*)                                                   ! nesbet.irp.f:  57
    enddo                                                            ! nesbet.irp.f:  58
    do while (abs(Cmax).gt.abs(C))                                   ! nesbet.irp.f:  60
        do while (m.lt.rank  .or.  abs(DeltaC).gt.abs(CA))           ! nesbet.irp.f:  62
        m=m+(1)                                                      ! nesbet.irp.f:  64
        do i=1,rank                                                  ! nesbet.irp.f:  70
            D=D+((V(i)*V(i)))                                        ! nesbet.irp.f:  71
        enddo                                                        ! nesbet.irp.f:  72
        do i=1,rank                                                  ! nesbet.irp.f:  77
            sigma=sigma+(H(m,i)*V(i))                                ! nesbet.irp.f:  78
        enddo                                                        ! nesbet.irp.f:  79
        sigma=sigma-(E*V(m))                                         ! nesbet.irp.f:  80
        DeltaC = sigma/(E-H(m,m))                                    ! nesbet.irp.f:  84
        DeltaD = (2.0d0*V(m)+DeltaC)*DeltaC                          ! nesbet.irp.f:  88
        if((D+DeltaD).ne.0.0d0)then                                  ! nesbet.irp.f:  92
        DeltaE = sigma*DeltaC/(D+DeltaD)                             ! nesbet.irp.f:  93
        endif                                                        ! nesbet.irp.f:  94
        if(abs(Cmax2).lt.abs(DeltaC))then                            ! nesbet.irp.f:  96
            Cmax2=DeltaC                                             ! nesbet.irp.f:  97
        endif                                                        ! nesbet.irp.f:  98
        E=E+(DeltaE)                                                 ! nesbet.irp.f: 103
        V(m)=V(m)+(DeltaC)                                           ! nesbet.irp.f: 104
        D=0.0d0                                                      ! nesbet.irp.f: 110
        sigma=0.0d0                                                  ! nesbet.irp.f: 111
        DeltaC=0.0d0                                                 ! nesbet.irp.f: 112
        DeltaD=0.0d0                                                 ! nesbet.irp.f: 113
        DeltaE=0.0d0                                                 ! nesbet.irp.f: 114
        enddo                                                        ! nesbet.irp.f: 116
        m=0                                                          ! nesbet.irp.f: 118
        Cmax=Cmax2                                                   ! nesbet.irp.f: 119
        CA=F*Cmax                                                    ! nesbet.irp.f: 120
        Cmax2=C                                                      ! nesbet.irp.f: 121
        write(6,*)'iter:',niter,'Cmax:',Cmax                         ! nesbet.irp.f: 122
        niter=niter+(1)                                              ! nesbet.irp.f: 123
    enddo                                                            ! nesbet.irp.f: 125
    sigma=0.0d0                                                      ! nesbet.irp.f: 127
    write(6,*)'converged in :',niter,'iterations'                    ! nesbet.irp.f: 128
    write(6,*)'Eigenvalue:',E                                        ! nesbet.irp.f: 129
    write(6,*)'Eigenvectors:'                                        ! nesbet.irp.f: 130
    do i=1,rank                                                      ! nesbet.irp.f: 131
        sigma=sigma+((V(i)*V(i)))                                    ! nesbet.irp.f: 132
    enddo                                                            ! nesbet.irp.f: 133
    do i=1,rank                                                      ! nesbet.irp.f: 135
        V(i)=V(i)/sqrt(sigma)                                        ! nesbet.irp.f: 136
        write(6,*)V(i)                                               ! nesbet.irp.f: 137
    enddo                                                            ! nesbet.irp.f: 138
    sigma=0.0d0                                                      ! nesbet.irp.f: 139
    do i=1,rank                                                      ! nesbet.irp.f: 140
        sigma=sigma+((V(i)*V(i)))                                    ! nesbet.irp.f: 141
    enddo                                                            ! nesbet.irp.f: 142
    write(6,*)'normalized?',sigma                                    ! nesbet.irp.f: 144
   12   format((F8.2,'  '),$)                                        ! nesbet.irp.f: 147
end                                                                  ! nesbet.irp.f: 148
