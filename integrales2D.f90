program integrales2D
implicit none

real(8),allocatable,dimension(:,:):: x,y, phi
real(8) xmin, xmax,ymin, ymax, dx, dy, pi, integral, exacta, Lx, Ly 
integer i,j, n, nx, ny

nx = 40
ny = 40
xmin = -2.0d0
xmax = 2.0d0

ymin = -2.0d0
ymax = 2.0d0

Lx = xmax - xmin
Ly = ymax - ymin

pi = acos(-1.0d0)
allocate(x(0:nx, 0:ny), y(0:nx, 0:ny), phi(0:nx, 0:ny))
	dx = (xmax - xmin)/ dble(nx)
	dy = (ymax - ymin)/ dble(ny)
	
	do i=0, nx
		x(i, :) = xmin + dble(i)*dx
	end do
	
	do j=0, ny
		y(:,j) = ymin + dble(j)* dy
	end do
	
	!EJEMPLO
	
	do i=0, nx
		do j=0, ny
			if(((x(i,i)**2) + (y(i,j)**2) .le. 1.0d0)) then
				phi(i,j) = 1.0d0
			else
				phi(i,j) = 0.0d0
			end if
		end do
	end do
	
	!exacta = pi
	!integral = 0.0d0
	
	!FUNCIONES 
	
	!phi = exp(-(x**2 + y**2))
	!exacta = pi

	
	!phi=1.0d0
	!exacta= (xmax-xmin)*(ymax-ymin) 
	
	integral = 0.0d0
	do i=1, nx
		do j=1, ny
			integral = integral +  (0.25d0*(phi(i-1,j-1) + phi(i,j-1) + phi(i-1,j) + phi(i,j))* (dx*dy))
		end do
	end do
	
	print *, "exacta=", exacta
	print *, "numerica=", integral
	print *, "error=", exacta - integral
	
	open(1, file="datosInteg2.txt")
		do i=0, nx
			do j=0, ny
				write(1,*) x(i,j), y(i,j), phi(i,j)
			end do
			write(1,*) !SALTO DE LINEA
		end do
	close(1)
		
	
end program
