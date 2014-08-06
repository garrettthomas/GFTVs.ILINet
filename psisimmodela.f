	subroutine psisimmodela(dt,nstep,ndays,param,nparam,x) 

C In this version the parameters are: Beta0, Beta1, t1, duration, pC, seed, omega
C we also pack here Tg, Neff and  Time Shift
	
	implicit real*8(a-h,o-z)

	real*8 dt, param(nparam)
! Param holds all the parameters
! sh holds the specific humidity
	integer ndays,nstep

! Y has in it the incidence data, x is generated by the SIR model
	real*8 x0(4),x(ndays)
	real*8 dsdt(ndays*nstep)

	beta0 = param(1)
	pC = param(2)
        Tg = param(3)
        seed = param(4)
	fNeff = param(5)

! Initial values for S(0), I(0), R(0) and -dS(0)/dt

	x0(2) = 1.0
	x0(1) = fNeff  - x0(2)
	x0(3) = 0.0d0
	x0(4) = 0.0d0


! Here we must call the RK4 routine and calculate the model prediction for incidence rate
	call RK4(dt,ndays,nstep,x0,Param,nparam,dsdt)

! Now convert tor one day bins and multiply by pC and add the baseline

	call daily(ndays,nstep,dsdt,x,pC,seed)

	return
	end subroutine psisimmodela

!----------------------------------------------------------------------

      SUBROUTINE RK4(dt, ndays,nstep,Y,Param,nparam,dsdt)

      implicit real*8(a-h,o-z)

      integer ndays, nstep
      REAL*8 dt, Y(4), Param(nparam),tmpY(4)
      real*8 param3(3)
      REAL*8 dPop1(4), dPop2(4), dPop3(4), dPop4(4)
      Real*8 SIR(ndays,4),dsdt(nstep*ndays)

C
C     Integrates the equations one step, using Runge-Kutta 4 
C     Note: we work with arrays rather than variables to make the
C     coding easier
C

	beta0 = param(1)
	pC = param(2)
        Tg = param(3)
        seed = param(4)
	fNeff = param(5)
	timeshift = param(6)

	param3(1) = beta0
	param3(2) = Tg
	param3(3) = fNeff

      do iday = 1, ndays
! Update information

         do j=1,4
            SIR(iday,j)=Y(j)
         enddo

         do istep = 1, nstep

            call diff(Param3,Y,dpop1)

            do k=1,4
               tmpY(k)=Y(k)+dt*dPop1(k)/2.0
            ENDDO 
         
            call diff(Param3,tmpY,dpop2)

            do k=1,4
               tmpY(k)=Y(k)+dt*dPop2(k)/2.0
            ENDDO

            call diff(Param3,tmpY,dpop3)

            do k=1,4
               tmpY(k)=Y(k)+dt*dPop3(k)
            ENDDO

            call diff(Param3,tmpY,dpop4)

            do k=1,4
               tmpY(k)=Y(k)+dt*(dPop1(k)/6.0 + dPop2(k)/3.0 +
     $              dPop3(k)/3.0 + dPop4(k)/6.0)
               Y(k) = tmpY(k)
            ENDDO

!     Update -dS/dt
            dsdt(istep+(iday-1)*nstep) = y(4)
	 enddo

      enddo
      RETURN
      END

!------------------------------------
C The Main Differential Equations.

      SUBROUTINE Diff(Param, Pop, dPop)

      implicit real*8(a-h,o-z)

      REAL*8 Param(3), Pop(4), dPop(4)

C
C     The differential equations-including source/sink term
C

C     dS/dt =
      dPop(1) = - Param(1)*Pop(1)*Pop(2)/Param(3)
C     dI/dt =
      dPop(2) =   Param(1)*Pop(1)*Pop(2)/Param(3)
     $          - Pop(2)/Param(2)
C     dR/dt =
      dPop(3) =   Pop(2)/Param(2) 
c cummulative dsdt
      dPop(4) = Param(1)*Pop(1)*Pop(2)/Param(3)
      RETURN
      END subroutine diff

!--------------------------------------------------------------------------------
           
        subroutine daily(iday,nstep,dsdt,x,pC,seed)
        
        implicit real*8(a-h,o-z)
        integer iday,nstep
        real*8 dsdt(iday*nstep),x(iday)

        do i=1,iday
           x(i) = dsdt(i*nstep) - dsdt(1+(i-1)*nstep)
        enddo
	x= x* pC +seed
        return
        end subroutine daily

