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
    open(unit=1,file='converg.dat')

    C=7.6d-2
    CA=1.0d0
    F=0.5d0

    V=0.0d0
    H=0.0d0
    Cmax=10000.0d0
    Cmax2=C

!   V(1)=1.0d0
!   V(2)=1.0d0
!   V(3)=1.0d0
!   V(4)=1.0d0
    V(11)=1.0d0
!   V(11)=1.0d0
    E=+0.4d0
    niter=1
    D=0.0d0
    sigma=0.0d0
    m=0
    
    write(6,*)'NESBET:'
    write(6,*)'starting with:'
    write(6,*)'E=',E,'m=7'

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

    do while (abs(Cmax).gt.abs(C))                                              !! main loop begin !!
        
        do while (m.lt.rank  .or.  abs(DeltaC).gt.abs(CA))

        m+=1


        ! calculating D=sum(C_i)

        do i=1,rank
            D+=(V(i)*V(i))
        enddo
        

        ! calculating sigma_m=sum(H_m_i*C_i)+EC_m

        do i=1,rank
            sigma+=H(m,i)*V(i)
        enddo
        sigma-=E*V(m)

        ! calculating DeltaC=sigma_m/(E-H_m_m)

        DeltaC = sigma/(E-H(m,m))

        ! calculating DeltaD

        DeltaD = (2.0d0*V(m)+DeltaC)*DeltaC

        ! calculating DeltaE=sigma_m*DeltaC/(D+DeltaD)
        
        if((D+DeltaD).ne.0.0d0)then
        DeltaE = sigma*DeltaC/(D+DeltaD)
        endif

        if(abs(Cmax2).lt.abs(DeltaC))then
            Cmax2=DeltaC
        endif

            

        E+=DeltaE
        V(m)+=DeltaC

!       do i=1,rank
!           V(i)=V(i)/D
!       enddo

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
        write(6,*)'iter:',niter,'Cmax:',Cmax
        write(1,*)niter,Cmax
        niter+=1

    enddo                                                                   !! main loop end !!
    
    sigma=0.0d0
    write(6,*)'converged in :',niter-1,'iterations'
    write(6,*)'Eigenvalue:',E
    write(6,*)'Eigenvectors:'
    do i=1,rank
        sigma+=(V(i)*V(i))
    enddo

    do i=1,rank
        V(i)=V(i)/sqrt(sigma)
        write(6,*)V(i)
    enddo
    sigma=0.0d0
    do i=1,rank
        sigma+=(V(i)*V(i))
    enddo

    write(6,*)'normalized?',sigma
    
        
   12   format((F8.2,'  '),$)
end program nesbet
