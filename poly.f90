program poly


	implicit none	
	
	integer :: totalProblems
	
	read *, totalProblems
	do while (totalProblems /= 0)
		call outputPolygonal(totalProblems)
		print *
		read *, totalProblems
	end do
	
	
	
	contains

	
	
		!finds and prints the first 5 polygonal numbers for the given input
		subroutine outputPolygonal(totalProblems)
			integer, intent(in) :: totalProblems
			integer :: n, m, i, startPlace											 !n,m,i used for looping, startPlace holds the value we start at as well as is currently being checked
			integer :: counter = 0															 !counts how many poly-polygonal numbers have been found
			integer :: pre = 0																	 !stores the first polygonal number found, used if another is found
			double precision :: current, rank										 !holds S and r values, respectfully, found in equations from https://mathworld.wolfram.com/PolygonalNumber.html
			logical :: first = .true.														 !keeps track of if the first polygonal number has been found for a problem number
			logical :: counted = .false.
			integer, dimension(1:totalProblems) :: polygons 		 !hold which polygonal numbers are in the current problem
			
			
			!reads all polygonal numbers for the problem into an array as well as the start place
			read(*,*)  (polygons(i), i=1, totalProblems)
			read *, startPlace
			
			!finds and prints first 5 poly-polygonal numbers
			do while (counter /= 5)
				do n = 1, totalProblems, 1
					current = (8*(polygons(n) - 2)*startPlace + ((polygons(n) - 4)**2))
					if (current > 0) then
						current = SQRT(current)
						!current must be a whole number
						if (int(current) == current) then 
							do m = 1, totalProblems, 1
								rank = (current + polygons(m) - 4) / ( 2*(polygons(m) - 2) )
								
								!rank must be a whole number
								if (int(rank) == rank) then
									if (first) then
										pre = polygons(m)
										first = .false.
									else if ((.not. first) .and. (pre /= 0)) then
										write (*, fmt="(i0, a1, i0, a1, i0)", advance="no") startPlace, ":", pre, " ", polygons(m)
										pre = 0
										counted = .true.
									else if ((.not. first) .and. (pre == 0)) then
										write (*, fmt="(i0,1x)", advance="no") polygons(m)
									end if
								end if
							end do
							first = .true.
							pre = 0
							if (counted) then
								counter = counter + 1
								print *
								counted = .false.
							end if
						end if
					end if
				end do
				startplace = startPlace + 1
				first = .true.
			end do
		end subroutine outputPolygonal



end program poly
