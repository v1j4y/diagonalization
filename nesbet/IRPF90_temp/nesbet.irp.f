program nesbet
    implicit none
    
    integer :: i,j,k,m,rank,niter
    real(8) :: CA,C,F,E,Cmax,DeltaC,D,DeltaD,DeltaE,sigma,dmat,Cmax2
    real(8),allocatable :: H(:,:),V(:)
    
    rank=30

    allocate(H(rank,rank))
    allocate(V(rank))

    open(unit=33,file='FIL33')
    open(unit=44,file='FIL44')

    C=1.0d-9
    CA=1.0d0
    F=0.5d0

    V=0.0d0
    H=0.0d0
    Cmax=10000.0d0
    Cmax2=C

    V(7)=2.0d0
    E=+0.0d0
    niter=1
    D=0.0d0
    sigma=0.0d0
    m=0
    
    write(6,*)'NESBET:'

    do k=1,30
        read(33,*)dmat,i,j
        H(i,j)=dmat
    enddo

    do k=1,84
        read(44,*)i,j,dmat
        H(i,j)=dmat
    enddo

    do i=10+1,30
        H(i,i-10)=-1
        H(i-10,i)=-1
    enddo

    write(6,*)'read H:'

    do i=1,rank
        do j=1,rank
            write(6,12)H(i,j)
        enddo
        write(6,*)
    enddo

    do while (abs(Cmax).gt.abs(C))
        
        do while (m.lt.rank  .or.  abs(DeltaC).gt.abs(CA))

        m+=1

        write(6,*)'m=',m
!       do while (DeltaC.lt.CA)

        ! calculating D=sum(C_i)

        do i=1,rank
            D+=(V(i)*V(i))
        enddo
        
        write(6,*)'D=',D

        ! calculating sigma_m=sum(H_m_i*C_i)+EC_m

        do i=1,rank
            sigma+=H(m,i)*V(i)
        enddo
        sigma-=E*V(m)

        write(6,*)'sigma=',sigma,(E-H(m,m))
        ! calculating DeltaC=sigma_m/(E-H_m_m)

        DeltaC = sigma/(E-H(m,m))

        write(6,*)'DeltaC=',DeltaC
        ! calculating DeltaD

        DeltaD = (2*V(m)+DeltaC)*DeltaC

        write(6,*)'DeltaD=',DeltaD
        ! calculating DeltaE=sigma_m*DeltaC/(D+DeltaD)
        
        if((D+DeltaD).ne.0)then
        DeltaE = sigma*DeltaC/(D+DeltaD)
        endif

        write(6,*)'DeltaE=',DeltaE
        if(abs(Cmax2).lt.abs(DeltaC))then
            Cmax2=DeltaC
        endif
        write(6,*)'Cmax2=',Cmax2

            
!       enddo

        E+=DeltaE
        V(m)+=DeltaC

        D=0.0d0
        sigma=0.0d0
        DeltaC=0.0d0
        DeltaD=0.0d0
        DeltaE=0.0d0

        enddo
        
        m=0
        Cmax=Cmax2
        CA=F*Cmax
        Cmax2=C
        write(6,*)'iter',niter,Cmax
        niter+=1

    enddo !! main loop !!

    write(6,*)'Eigenvalue',E+DeltaE
    write(6,*)'Eigenvectors:'
    do i=1,rank
        write(6,*)V(i)
    enddo
        
   12   format((F8.2,'  '),$)
end program nesbet
