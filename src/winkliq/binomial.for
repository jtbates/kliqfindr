      subroutine binomial_cdf ( x, a, b, cdf )
        integer a
        real b
        integer cnk
        real cdf
        integer j
        real pr
        integer x
        if ( x .lt. 0 ) then

          cdf = 0.0E+00

        else if ( x .ge. a ) then

          cdf = 1.0E+00      

        else if ( b .eq. 0.0E+00 ) then

          cdf = 1.0E+00

        else if ( b .eq. 1.0E+00 ) then

          cdf = 0.0E+00

        else

          cdf = 0.0E+00

          do j = 0, x
          
          mn = min ( j, a-j )

        if ( mn .lt. 0 ) then

          cnk = 0

        else if ( mn .eq. 0 ) then

          cnk = 1

        else

          mx = max ( j, a-j )
          cnk = mx + 1

          do i = 2, mn
            cnk = ( cnk * ( mx + i ) ) / i
          end do

        end if

            pr = real ( cnk ) * b**j * ( 1.0E+00 - b )**( a - j )

            cdf = cdf + pr

          end do

        end if

        return
      end
      
